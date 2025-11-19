# Chapter 8: Literate Programming - Message Encryption Flow

## A Complete Walkthrough of Encrypting and Decrypting Messages in an Established Session

---

### Table of Contents

1. [Introduction](#introduction)
2. [Established Session State](#1-established-session-state)
3. [Encrypting a Message (Alice → Bob)](#2-encrypting-a-message-alice-sends-to-bob)
4. [Decrypting a Message (Bob Receives)](#3-decrypting-a-message-bob-receives)
5. [DH Ratchet Step](#4-dh-ratchet-step)
6. [Out-of-Order Messages](#5-out-of-order-messages)
7. [SPQR Integration](#6-spqr-integration)
8. [Security Properties](#7-security-properties)

---

## Introduction

This chapter provides a detailed, line-by-line walkthrough of the message encryption and decryption flow in libsignal's implementation of the Double Ratchet algorithm with post-quantum (SPQR) extensions. We'll follow actual code paths through the implementation, examining:

- **Key derivation formulas** used in the ratchet
- **State management** and updates
- **Cryptographic operations** (encryption, MAC computation, key derivation)
- **Post-quantum ratchet advancement**
- **Out-of-order message handling**

This is a **literate programming** walkthrough — code and explanation interwoven to illuminate how the system works at the deepest level.

### Prerequisites

Before diving into this chapter, you should understand:
- **X3DH/PQXDH**: Session establishment (covered in previous chapters)
- **Double Ratchet**: Conceptual understanding of root keys, chain keys, and message keys
- **SPQR**: Post-quantum ratchet basics
- **HKDF and HMAC**: Cryptographic key derivation functions

### Source Files Referenced

The primary source files we'll examine:

- **`rust/protocol/src/session_cipher.rs`**: Main encryption/decryption logic
- **`rust/protocol/src/ratchet.rs`**: Session initialization and key derivation
- **`rust/protocol/src/ratchet/keys.rs`**: ChainKey, RootKey, and MessageKeys implementation
- **`rust/protocol/src/state/session.rs`**: SessionState management
- **`rust/protocol/src/protocol.rs`**: SignalMessage structure and MAC computation
- **`rust/protocol/src/crypto.rs`**: Low-level cryptographic primitives
- **`rust/protocol/src/consts.rs`**: Protocol constants

---

## 1. Established Session State

Before we can encrypt or decrypt messages, we need an established session. After PQXDH handshake completion (covered in previous chapters), both Alice and Bob have matching session state.

### 1.1 Session State Structure

The `SessionState` holds all cryptographic state for an active session:

**File: `rust/protocol/src/state/session.rs:131-165`**

```rust
#[derive(Clone, Debug)]
pub(crate) struct SessionState {
    session: SessionStructure,
}

impl SessionState {
    pub(crate) fn new(
        version: u8,
        our_identity: &IdentityKey,
        their_identity: &IdentityKey,
        root_key: &RootKey,
        alice_base_key: &PublicKey,
        pq_ratchet_state: spqr::SerializedState,
    ) -> Self {
        Self {
            session: SessionStructure {
                session_version: version as u32,
                local_identity_public: our_identity.public_key().serialize().into_vec(),
                remote_identity_public: their_identity.serialize().into_vec(),
                root_key: root_key.key().to_vec(),         // [1]
                previous_counter: 0,
                sender_chain: None,                         // [2]
                receiver_chains: vec![],                    // [3]
                pending_pre_key: None,
                pending_kyber_pre_key: None,
                remote_registration_id: 0,
                local_registration_id: 0,
                alice_base_key: alice_base_key.serialize().into_vec(),
                pq_ratchet_state,                           // [4]
            },
        }
    }
}
```

**Key Components:**

- **[1] Root Key**: 32-byte symmetric key used to derive new chain keys during DH ratchet steps
- **[2] Sender Chain**: Contains our current sending ratchet key (public/private) and sending chain key
- **[3] Receiver Chains**: List of receiver chains (one per remote ratchet key we've seen), each containing a chain key and cached message keys
- **[4] PQ Ratchet State**: SPQR state for post-quantum forward secrecy

### 1.2 Root Key Structure

**File: `rust/protocol/src/ratchet/keys.rs:178-217`**

```rust
#[derive(Clone, Debug)]
pub(crate) struct RootKey {
    key: [u8; 32],
}

impl RootKey {
    pub(crate) fn new(key: [u8; 32]) -> Self {
        Self { key }
    }

    pub(crate) fn create_chain(
        self,
        their_ratchet_key: &PublicKey,
        our_ratchet_key: &PrivateKey,
    ) -> Result<(RootKey, ChainKey)> {
        // Perform Diffie-Hellman
        let shared_secret = our_ratchet_key.calculate_agreement(their_ratchet_key)?;

        #[derive(Default, KnownLayout, IntoBytes, FromBytes)]
        #[repr(C, packed)]
        struct DerivedSecretBytes([u8; 32], [u8; 32]);
        let mut derived_secret_bytes = DerivedSecretBytes::default();

        // HKDF with current root key as salt, DH output as input
        hkdf::Hkdf::<sha2::Sha256>::new(Some(&self.key), &shared_secret)
            .expand(b"WhisperRatchet", derived_secret_bytes.as_mut_bytes())
            .expect("valid output length");

        let DerivedSecretBytes(root_key, chain_key) = derived_secret_bytes;

        Ok((
            RootKey { key: root_key },
            ChainKey {
                key: chain_key,
                index: 0,
            },
        ))
    }
}
```

**Root Key Derivation Formula:**

```
DH_output = ECDH(our_ratchet_private, their_ratchet_public)
(new_root_key, new_chain_key) = HKDF-SHA256(
    salt = current_root_key,
    input_key_material = DH_output,
    info = "WhisperRatchet",
    output_length = 64 bytes
)
```

This is the core of the **Double Ratchet's DH ratchet step**.

### 1.3 Chain Key Structure

**File: `rust/protocol/src/ratchet/keys.rs:135-176`**

```rust
#[derive(Clone, Debug)]
pub(crate) struct ChainKey {
    key: [u8; 32],
    index: u32,
}

impl ChainKey {
    const MESSAGE_KEY_SEED: [u8; 1] = [0x01u8];
    const CHAIN_KEY_SEED: [u8; 1] = [0x02u8];

    pub(crate) fn new(key: [u8; 32], index: u32) -> Self {
        Self { key, index }
    }

    pub(crate) fn next_chain_key(&self) -> Self {
        Self {
            key: self.calculate_base_material(Self::CHAIN_KEY_SEED),
            index: self.index + 1,
        }
    }

    pub(crate) fn message_keys(&self) -> MessageKeyGenerator {
        MessageKeyGenerator::new_from_seed(
            &self.calculate_base_material(Self::MESSAGE_KEY_SEED),
            self.index,
        )
    }

    fn calculate_base_material(&self, seed: [u8; 1]) -> [u8; 32] {
        crypto::hmac_sha256(&self.key, &seed)
    }
}
```

**Chain Key Ratcheting Formula:**

```
next_chain_key = HMAC-SHA256(key = current_chain_key, data = 0x02)
message_key_seed = HMAC-SHA256(key = current_chain_key, data = 0x01)
```

The chain key advances with **each message sent or received** on that chain, providing **forward secrecy**.

### 1.4 Message Keys Derivation

**File: `rust/protocol/src/ratchet/keys.rs:89-112`**

```rust
impl MessageKeys {
    pub(crate) fn derive_keys(
        input_key_material: &[u8],
        optional_salt: Option<&[u8]>,  // PQ ratchet key if present
        counter: u32,
    ) -> Self {
        #[derive(Default, KnownLayout, IntoBytes, FromBytes)]
        #[repr(C, packed)]
        struct DerivedSecretBytes([u8; 32], [u8; 32], [u8; 16]);
        let mut okm = DerivedSecretBytes::default();

        hkdf::Hkdf::<sha2::Sha256>::new(optional_salt, input_key_material)
            .expand(b"WhisperMessageKeys", okm.as_mut_bytes())
            .expect("valid output length");

        let DerivedSecretBytes(cipher_key, mac_key, iv) = okm;

        MessageKeys {
            cipher_key,  // 32 bytes for AES-256
            mac_key,     // 32 bytes for HMAC-SHA256
            iv,          // 16 bytes for AES-CBC
            counter,
        }
    }
}
```

**Message Keys Derivation Formula:**

```
(cipher_key, mac_key, iv) = HKDF-SHA256(
    salt = pq_message_key (if SPQR enabled, else None),
    input_key_material = message_key_seed,
    info = "WhisperMessageKeys",
    output_length = 80 bytes  // 32 + 32 + 16
)
```

**Without SPQR:**
- Salt is None
- Only classical chain key provides entropy

**With SPQR:**
- Salt is the post-quantum message key (32 bytes)
- **Combined security** from both classical and post-quantum sources

---

## 2. Encrypting a Message (Alice Sends to Bob)

Let's walk through the complete encryption process when Alice sends a message to Bob.

### 2.1 Entry Point: `message_encrypt`

**File: `rust/protocol/src/session_cipher.rs:19-159`**

```rust
pub async fn message_encrypt<R: Rng + CryptoRng>(
    ptext: &[u8],                                    // [1]
    remote_address: &ProtocolAddress,
    session_store: &mut dyn SessionStore,
    identity_store: &mut dyn IdentityKeyStore,
    now: SystemTime,
    csprng: &mut R,
) -> Result<CiphertextMessage> {
    // Load session from storage
    let mut session_record = session_store
        .load_session(remote_address)
        .await?
        .ok_or_else(|| SignalProtocolError::SessionNotFound(remote_address.clone()))?;

    let session_state = session_record
        .session_state_mut()
        .ok_or_else(|| SignalProtocolError::SessionNotFound(remote_address.clone()))?;

    // Get current sending chain key
    let chain_key = session_state.get_sender_chain_key()?;  // [2]
```

**[1]** The plaintext to encrypt (arbitrary bytes - could be text, image data, etc.)

**[2]** Retrieve the current sender chain key from session state.

**File: `rust/protocol/src/state/session.rs:384-400`**

```rust
pub(crate) fn get_sender_chain_key(&self) -> Result<ChainKey, InvalidSessionError> {
    let sender_chain = self
        .session
        .sender_chain
        .as_ref()
        .ok_or(InvalidSessionError("missing sender chain"))?;

    let chain_key = sender_chain
        .chain_key
        .as_ref()
        .ok_or(InvalidSessionError("missing sender chain key"))?;

    let chain_key_bytes = chain_key.key[..]
        .try_into()
        .map_err(|_| InvalidSessionError("invalid sender chain key"))?;

    Ok(ChainKey::new(chain_key_bytes, chain_key.index))
}
```

### 2.2 Post-Quantum Ratchet Send

**File: `rust/protocol/src/session_cipher.rs:37-44`**

```rust
    // Advance PQ ratchet and get PQ message key
    let (pqr_msg, pqr_key) = session_state.pq_ratchet_send(csprng).map_err(|e| {
        SignalProtocolError::InvalidState(
            "message_encrypt",
            format!("post-quantum ratchet send error: {e}"),
        )
    })?;
    let message_keys = chain_key.message_keys().generate_keys(pqr_key);  // [3]
```

**[3]** Generate message keys by combining:
- **Classical chain key** → message_key_seed via HMAC
- **PQ ratchet key** → used as HKDF salt
- Result: cipher_key, mac_key, iv

**File: `rust/protocol/src/state/session.rs:610-617`**

```rust
pub(crate) fn pq_ratchet_send<R: Rng + CryptoRng>(
    &mut self,
    csprng: &mut R,
) -> Result<(spqr::SerializedMessage, spqr::MessageKey), spqr::Error> {
    let spqr::Send { state, key, msg } = spqr::send(&self.session.pq_ratchet_state, csprng)?;
    self.session.pq_ratchet_state = state;  // Update PQ state
    Ok((msg, key))
}
```

The SPQR library advances its internal ratchet and returns:
- **`msg`**: Serialized PQ ratchet update (included in SignalMessage)
- **`key`**: 32-byte PQ message key (used in HKDF)
- **`state`**: Updated PQ ratchet state (persisted)

### 2.3 Encrypt the Plaintext

**File: `rust/protocol/src/session_cipher.rs:46-66`**

```rust
    let sender_ephemeral = session_state.sender_ratchet_key()?;
    let previous_counter = session_state.previous_counter();
    let session_version = session_state
        .session_version()?
        .try_into()
        .map_err(|_| SignalProtocolError::InvalidSessionStructure("version does not fit in u8"))?;

    let local_identity_key = session_state.local_identity_key()?;
    let their_identity_key = session_state.remote_identity_key()?.ok_or_else(|| {
        SignalProtocolError::InvalidState(
            "message_encrypt",
            format!("no remote identity key for {remote_address}"),
        )
    })?;

    // AES-256-CBC encryption
    let ctext =
        signal_crypto::aes_256_cbc_encrypt(ptext, message_keys.cipher_key(), message_keys.iv())
            .map_err(|_| {
                log::error!("session state corrupt for {remote_address}");
                SignalProtocolError::InvalidSessionStructure("invalid sender chain message keys")
            })?;
```

**AES-256-CBC Encryption:**
- **Algorithm**: AES-256 in CBC mode
- **Key**: 32-byte cipher_key from message keys
- **IV**: 16-byte initialization vector from message keys
- **Padding**: PKCS#7 padding applied automatically

The `signal_crypto` module wraps the standard AES implementation. Note that libsignal uses **CBC mode** (not GCM) because:
1. MAC is computed separately over the entire message
2. Simpler implementation
3. Better studied in the academic literature on the Double Ratchet

### 2.4 Create SignalMessage

**File: `rust/protocol/src/session_cipher.rs:92-102`**

```rust
        let message = SignalMessage::new(
            session_version,
            message_keys.mac_key(),
            sender_ephemeral,
            chain_key.index(),        // Message counter
            previous_counter,
            &ctext,
            &local_identity_key,
            &their_identity_key,
            &pqr_msg,                 // PQ ratchet update
        )?;
```

**File: `rust/protocol/src/protocol.rs:76-121`**

```rust
pub fn new(
    message_version: u8,
    mac_key: &[u8],
    sender_ratchet_key: PublicKey,
    counter: u32,
    previous_counter: u32,
    ciphertext: &[u8],
    sender_identity_key: &IdentityKey,
    receiver_identity_key: &IdentityKey,
    pq_ratchet: &[u8],
) -> Result<Self> {
    // Create protobuf structure
    let message = proto::wire::SignalMessage {
        ratchet_key: Some(sender_ratchet_key.serialize().into_vec()),
        counter: Some(counter),
        previous_counter: Some(previous_counter),
        ciphertext: Some(Vec::<u8>::from(ciphertext)),
        pq_ratchet: if pq_ratchet.is_empty() {
            None
        } else {
            Some(pq_ratchet.to_vec())
        },
    };

    // Serialize with version byte
    let mut serialized = Vec::with_capacity(1 + message.encoded_len() + Self::MAC_LENGTH);
    serialized.push(((message_version & 0xF) << 4) | CIPHERTEXT_MESSAGE_CURRENT_VERSION);
    message
        .encode(&mut serialized)
        .expect("can always append to a buffer");

    // Compute and append MAC
    let mac = Self::compute_mac(
        sender_identity_key,
        receiver_identity_key,
        mac_key,
        &serialized,
    )?;
    serialized.extend_from_slice(&mac);  // Last 8 bytes
    let serialized = serialized.into_boxed_slice();

    Ok(Self {
        message_version,
        sender_ratchet_key,
        counter,
        previous_counter,
        ciphertext: ciphertext.into(),
        pq_ratchet: pq_ratchet.to_vec(),
        serialized,
    })
}
```

**SignalMessage Structure:**

```
┌────────────────────────────────────────────────────────┐
│ Version Byte (1 byte)                                  │
│   High nibble: protocol version (4)                    │
│   Low nibble: message version (4)                      │
├────────────────────────────────────────────────────────┤
│ Protobuf Encoded Message:                              │
│   - ratchet_key: sender's current public ratchet key   │
│   - counter: chain key index (message number)          │
│   - previous_counter: from last DH ratchet step        │
│   - ciphertext: AES-256-CBC encrypted plaintext        │
│   - pq_ratchet: SPQR update (if enabled)               │
├────────────────────────────────────────────────────────┤
│ MAC (8 bytes) = truncated HMAC-SHA256                  │
└────────────────────────────────────────────────────────┘
```

### 2.5 MAC Computation

**File: `rust/protocol/src/protocol.rs:178-196`**

```rust
fn compute_mac(
    sender_identity_key: &IdentityKey,
    receiver_identity_key: &IdentityKey,
    mac_key: &[u8],
    message: &[u8],
) -> Result<[u8; Self::MAC_LENGTH]> {
    if mac_key.len() != 32 {
        return Err(SignalProtocolError::InvalidMacKeyLength(mac_key.len()));
    }

    let mut mac = Hmac::<Sha256>::new_from_slice(mac_key)
        .expect("HMAC-SHA256 should accept any size key");

    mac.update(sender_identity_key.public_key().serialize().as_ref());
    mac.update(receiver_identity_key.public_key().serialize().as_ref());
    mac.update(message);

    let mut result = [0u8; Self::MAC_LENGTH];
    result.copy_from_slice(&mac.finalize().into_bytes()[..Self::MAC_LENGTH]);
    Ok(result)
}
```

**MAC Calculation:**

```
mac_input = sender_identity_public || receiver_identity_public || serialized_message
full_mac = HMAC-SHA256(key = mac_key, data = mac_input)
truncated_mac = first_8_bytes(full_mac)
```

**Why include identity keys in MAC?**
- **Prevents identity key substitution attacks**
- Even if an attacker compromises session keys, they can't forge messages between different identity key pairs
- Binds the message to specific identities

**Why truncate to 8 bytes?**
- 64 bits of MAC security is sufficient (2^64 forgery attempts needed)
- Saves bandwidth (24 bytes saved per message)
- Standard practice in authenticated encryption schemes

### 2.6 Advance Chain Key

**File: `rust/protocol/src/session_cipher.rs:133`**

```rust
    session_state.set_sender_chain_key(&chain_key.next_chain_key());
```

**Critical for forward secrecy:**

```
old_chain_key (index: N) ──────> new_chain_key (index: N+1)
       │                                │
       ├─> message_key_seed ───> message_keys (used for this message)
       └─> HMAC-SHA256(data=0x02) ────> new chain key
```

After sending, the old chain key is **deleted from memory**. Even if the device is compromised later, past message keys cannot be recovered.

### 2.7 Store Updated Session

**File: `rust/protocol/src/session_cipher.rs:136-158`**

```rust
    // Verify trusted identity (defense in depth)
    if !identity_store
        .is_trusted_identity(remote_address, &their_identity_key, Direction::Sending)
        .await?
    {
        log::warn!(
            "Identity key {} is not trusted for remote address {}",
            hex::encode(their_identity_key.public_key().public_key_bytes()),
            remote_address,
        );
        return Err(SignalProtocolError::UntrustedIdentity(
            remote_address.clone(),
        ));
    }

    identity_store
        .save_identity(remote_address, &their_identity_key)
        .await?;

    session_store
        .store_session(remote_address, &session_record)
        .await?;

    Ok(message)
}
```

The updated session state (with advanced chain key and PQ ratchet) is persisted to storage.

---

## 3. Decrypting a Message (Bob Receives)

Now let's follow the decryption path when Bob receives Alice's message.

### 3.1 Entry Point: `message_decrypt_signal`

**File: `rust/protocol/src/session_cipher.rs:280-331`**

```rust
pub async fn message_decrypt_signal<R: Rng + CryptoRng>(
    ciphertext: &SignalMessage,
    remote_address: &ProtocolAddress,
    session_store: &mut dyn SessionStore,
    identity_store: &mut dyn IdentityKeyStore,
    csprng: &mut R,
) -> Result<Vec<u8>> {
    let mut session_record = session_store
        .load_session(remote_address)
        .await?
        .ok_or_else(|| SignalProtocolError::SessionNotFound(remote_address.clone()))?;

    let ptext = decrypt_message_with_record(
        remote_address,
        &mut session_record,
        ciphertext,
        CiphertextMessageType::Whisper,
        csprng,
    )?;

    // ... identity verification ...

    session_store
        .store_session(remote_address, &session_record)
        .await?;

    Ok(ptext)
}
```

### 3.2 Decrypt with Session Record

Bob might have **multiple session states** (current + previous sessions). We try them in order:

**File: `rust/protocol/src/session_cipher.rs:424-582`**

```rust
fn decrypt_message_with_record<R: Rng + CryptoRng>(
    remote_address: &ProtocolAddress,
    record: &mut SessionRecord,
    ciphertext: &SignalMessage,
    original_message_type: CiphertextMessageType,
    csprng: &mut R,
) -> Result<Vec<u8>> {
    let mut errs = vec![];

    // Try current session first
    if let Some(current_state) = record.session_state() {
        let mut current_state = current_state.clone();
        let result = decrypt_message_with_state(
            CurrentOrPrevious::Current,
            &mut current_state,
            ciphertext,
            original_message_type,
            remote_address,
            csprng,
        );

        match result {
            Ok(ptext) => {
                log::info!(
                    "decrypted {:?} message from {} with current session state",
                    original_message_type,
                    remote_address,
                );
                record.set_session_state(current_state);  // Update state
                return Ok(ptext);
            }
            Err(SignalProtocolError::DuplicatedMessage(_, _)) => {
                return result;  // Don't try other sessions for duplicates
            }
            Err(e) => {
                errs.push(e);
                // Fall through to try previous sessions
            }
        }
    }

    // Try previous sessions
    for (idx, previous) in record.previous_session_states().enumerate() {
        let mut previous = previous?;

        let result = decrypt_message_with_state(
            CurrentOrPrevious::Previous,
            &mut previous,
            ciphertext,
            original_message_type,
            remote_address,
            csprng,
        );

        match result {
            Ok(ptext) => {
                log::info!(
                    "decrypted message from {} with PREVIOUS session state",
                    remote_address,
                );
                record.promote_old_session(idx, previous);  // Promote to current
                return Ok(ptext);
            }
            Err(e) => {
                errs.push(e);
            }
        }
    }

    // All sessions failed
    log::error!("No valid session for recipient: {}", remote_address);
    Err(SignalProtocolError::InvalidMessage(
        original_message_type,
        "decryption failed",
    ))
}
```

**Why multiple sessions?**
- **Session conflicts**: Both parties might initiate sessions simultaneously
- **Out-of-order delivery**: Older session messages might arrive late
- **Robustness**: Graceful handling of network issues

### 3.3 Decrypt with Single Session State

**File: `rust/protocol/src/session_cipher.rs:599-694`**

```rust
fn decrypt_message_with_state<R: Rng + CryptoRng>(
    current_or_previous: CurrentOrPrevious,
    state: &mut SessionState,
    ciphertext: &SignalMessage,
    original_message_type: CiphertextMessageType,
    remote_address: &ProtocolAddress,
    csprng: &mut R,
) -> Result<Vec<u8>> {
    // Validate session exists
    let _ = state.root_key().map_err(|_| {
        SignalProtocolError::InvalidMessage(
            original_message_type,
            "No session available to decrypt",
        )
    })?;

    // Check version matches
    let ciphertext_version = ciphertext.message_version() as u32;
    if ciphertext_version != state.session_version()? {
        return Err(SignalProtocolError::UnrecognizedMessageVersion(
            ciphertext_version,
        ));
    }

    let their_ephemeral = ciphertext.sender_ratchet_key();
    let counter = ciphertext.counter();

    // Get or create receiver chain for this ratchet key
    let chain_key = get_or_create_chain_key(state, their_ephemeral, remote_address, csprng)?;

    // Get or create message key for this counter
    let message_key_gen = get_or_create_message_key(
        state,
        their_ephemeral,
        remote_address,
        original_message_type,
        &chain_key,
        counter,
    )?;
```

### 3.4 Get or Create Chain Key

**File: `rust/protocol/src/session_cipher.rs:696-730`**

```rust
fn get_or_create_chain_key<R: Rng + CryptoRng>(
    state: &mut SessionState,
    their_ephemeral: &PublicKey,
    remote_address: &ProtocolAddress,
    csprng: &mut R,
) -> Result<ChainKey> {
    // Check if we already have a receiver chain for this ratchet key
    if let Some(chain) = state.get_receiver_chain_key(their_ephemeral)? {
        log::debug!("{remote_address} has existing receiver chain.");
        return Ok(chain);
    }

    // New ratchet key from sender! Need to perform DH ratchet step.
    log::info!("{remote_address} creating new chains.");

    let root_key = state.root_key()?;
    let our_ephemeral = state.sender_ratchet_private_key()?;

    // Create new receiver chain
    let receiver_chain = root_key.create_chain(their_ephemeral, &our_ephemeral)?;

    // Generate new ephemeral key pair for our sending chain
    let our_new_ephemeral = KeyPair::generate(csprng);

    // Create new sender chain
    let sender_chain = receiver_chain
        .0  // new root key
        .create_chain(their_ephemeral, &our_new_ephemeral.private_key)?;

    // Update state with new root key and chains
    state.set_root_key(&sender_chain.0);
    state.add_receiver_chain(their_ephemeral, &receiver_chain.1);

    let current_index = state.get_sender_chain_key()?.index();
    let previous_index = if current_index > 0 {
        current_index - 1
    } else {
        0
    };
    state.set_previous_counter(previous_index);
    state.set_sender_chain(&our_new_ephemeral, &sender_chain.1);

    Ok(receiver_chain.1)
}
```

**This is the DH Ratchet Step on the receiving side!**

When Bob sees a new ratchet key from Alice:
1. **Receive DH Ratchet**: Use Alice's new public key + Bob's old private key → new root key & receiver chain key
2. **Send DH Ratchet**: Generate Bob's new key pair, use it with Alice's new public key → newer root key & sender chain key
3. **Update state**: Store new root key, new receiver chain, new sender chain

### 3.5 Get or Create Message Key

**File: `rust/protocol/src/session_cipher.rs:732-782`**

```rust
fn get_or_create_message_key(
    state: &mut SessionState,
    their_ephemeral: &PublicKey,
    remote_address: &ProtocolAddress,
    original_message_type: CiphertextMessageType,
    chain_key: &ChainKey,
    counter: u32,
) -> Result<MessageKeyGenerator> {
    let chain_index = chain_key.index();

    // Message from the past? Check if we cached the key
    if chain_index > counter {
        return match state.get_message_keys(their_ephemeral, counter)? {
            Some(keys) => Ok(keys),
            None => {
                log::info!("{remote_address} Duplicate message for counter: {counter}");
                Err(SignalProtocolError::DuplicatedMessage(chain_index, counter))
            }
        };
    }

    assert!(chain_index <= counter);

    let jump = (counter - chain_index) as usize;

    // Future message limit (prevent DoS via excessive key derivation)
    if jump > MAX_FORWARD_JUMPS {
        if state.session_with_self()? {
            log::info!(
                "{remote_address} Jumping ahead {jump} messages (self-session)"
            );
        } else {
            log::error!(
                "{remote_address} Exceeded future message limit: {MAX_FORWARD_JUMPS}"
            );
            return Err(SignalProtocolError::InvalidMessage(
                original_message_type,
                "message from too far into the future",
            ));
        }
    }

    // Derive intermediate message keys and cache them
    let mut chain_key = chain_key.clone();

    while chain_key.index() < counter {
        let message_keys = chain_key.message_keys();
        state.set_message_keys(their_ephemeral, message_keys)?;  // Cache for later
        chain_key = chain_key.next_chain_key();
    }

    // Advance chain key and return message key for this message
    state.set_receiver_chain_key(their_ephemeral, &chain_key.next_chain_key())?;
    Ok(chain_key.message_keys())
}
```

**Message Key Caching Logic:**

```
Chain at index 5, message arrives with counter 8:

Chain key [5] → message_keys → CACHE (counter 5)
      ↓
Chain key [6] → message_keys → CACHE (counter 6)
      ↓
Chain key [7] → message_keys → CACHE (counter 7)
      ↓
Chain key [8] → message_keys → USE FOR DECRYPTION
      ↓
Chain key [9] → STORE (ready for next message)
```

**Constants** (`rust/protocol/src/consts.rs`):
- **`MAX_FORWARD_JUMPS = 25,000`**: Maximum gap in message sequence numbers
- **`MAX_MESSAGE_KEYS = 2,000`**: Maximum cached message keys per chain

### 3.6 Post-Quantum Ratchet Receive

**File: `rust/protocol/src/session_cipher.rs:633-648`**

```rust
    let pqr_key = state
        .pq_ratchet_recv(ciphertext.pq_ratchet())
        .map_err(|e| match e {
            spqr::Error::StateDecode => SignalProtocolError::InvalidState(
                "decrypt_message_with_state",
                format!("post-quantum ratchet error: {e}"),
            ),
            _ => {
                log::info!("post-quantum ratchet error in decrypt_message_with_state: {e}");
                SignalProtocolError::InvalidMessage(
                    original_message_type,
                    "post-quantum ratchet error",
                )
            }
        })?;
    let message_keys = message_key_gen.generate_keys(pqr_key);
```

**File: `rust/protocol/src/state/session.rs:601-608`**

```rust
pub(crate) fn pq_ratchet_recv(
    &mut self,
    msg: &spqr::SerializedMessage,
) -> Result<spqr::MessageKey, spqr::Error> {
    let spqr::Recv { state, key } = spqr::recv(&self.session.pq_ratchet_state, msg)?;
    self.session.pq_ratchet_state = state;  // Update PQ state
    Ok(key)
}
```

The SPQR library:
1. Processes the PQ ratchet update from the message
2. Advances its internal ratchet state
3. Returns the PQ message key (used in HKDF salt)

### 3.7 Verify MAC

**File: `rust/protocol/src/session_cipher.rs:650-668`**

```rust
    let their_identity_key =
        state
            .remote_identity_key()?
            .ok_or(SignalProtocolError::InvalidSessionStructure(
                "cannot decrypt without remote identity key",
            ))?;

    let mac_valid = ciphertext.verify_mac(
        &their_identity_key,
        &state.local_identity_key()?,
        message_keys.mac_key(),
    )?;

    if !mac_valid {
        return Err(SignalProtocolError::InvalidMessage(
            original_message_type,
            "MAC verification failed",
        ));
    }
```

**File: `rust/protocol/src/protocol.rs:153-176`**

```rust
pub fn verify_mac(
    &self,
    sender_identity_key: &IdentityKey,
    receiver_identity_key: &IdentityKey,
    mac_key: &[u8],
) -> Result<bool> {
    let our_mac = &Self::compute_mac(
        sender_identity_key,
        receiver_identity_key,
        mac_key,
        &self.serialized[..self.serialized.len() - Self::MAC_LENGTH],
    )?;
    let their_mac = &self.serialized[self.serialized.len() - Self::MAC_LENGTH..];

    let result: bool = our_mac.ct_eq(their_mac).into();  // Constant-time comparison

    if !result {
        log::warn!(
            "Bad Mac! Their Mac: {} Our Mac: {}",
            hex::encode(their_mac),
            hex::encode(our_mac)
        );
    }
    Ok(result)
}
```

**Constant-time comparison** prevents timing attacks where an attacker learns information about the MAC by measuring verification time.

### 3.8 Decrypt Ciphertext

**File: `rust/protocol/src/session_cipher.rs:670-689`**

```rust
    let ptext = match signal_crypto::aes_256_cbc_decrypt(
        ciphertext.body(),
        message_keys.cipher_key(),
        message_keys.iv(),
    ) {
        Ok(ptext) => ptext,
        Err(signal_crypto::DecryptionError::BadKeyOrIv) => {
            log::warn!("{current_or_previous} session state corrupt for {remote_address}",);
            return Err(SignalProtocolError::InvalidSessionStructure(
                "invalid receiver chain message keys",
            ));
        }
        Err(signal_crypto::DecryptionError::BadCiphertext(msg)) => {
            log::warn!("failed to decrypt 1:1 message: {msg}");
            return Err(SignalProtocolError::InvalidMessage(
                original_message_type,
                "failed to decrypt",
            ));
        }
    };

    state.clear_unacknowledged_pre_key_message();

    Ok(ptext)
}
```

**AES-256-CBC Decryption:**
- Reverses the encryption with the same key and IV
- PKCS#7 padding is removed automatically
- Returns the original plaintext bytes

---

## 4. DH Ratchet Step

The **Double Ratchet** gets its name from two interleaved ratchets:
1. **Symmetric Ratchet**: Chain key advancing with each message (HMAC-based)
2. **DH Ratchet**: New Diffie-Hellman exchange when sender's ratchet key changes

### 4.1 When to Perform DH Ratchet

**Sending Side:**
- Alice always uses her current sender ratchet key
- DH ratchet occurs when **receiving a message with a new ratchet key from Bob**

**Receiving Side:**
- When Bob receives a message with a ratchet key he hasn't seen before
- Triggers creation of new receiver chain + new sender chain

### 4.2 DH Ratchet Mathematics

Let's trace a DH ratchet step in detail:

**Initial State (Alice's perspective):**
```
Root Key: RK_0
Alice's ratchet key pair: (A_priv_0, A_pub_0)
Bob's ratchet public key: B_pub_0
Sender chain key: SCK_0 (for sending to Bob)
Receiver chain key: RCK_0 (for Bob's messages)
```

**Bob sends message with new ratchet key `B_pub_1`:**

Alice receives this and sees a new ratchet key. She performs:

```
Step 1: Create new receiver chain
    DH_recv = ECDH(A_priv_0, B_pub_1)
    (RK_1, RCK_1) = HKDF(salt=RK_0, ikm=DH_recv, info="WhisperRatchet")

Step 2: Generate new sender ratchet key pair
    (A_priv_1, A_pub_1) = generate_keypair()

Step 3: Create new sender chain
    DH_send = ECDH(A_priv_1, B_pub_1)
    (RK_2, SCK_1) = HKDF(salt=RK_1, ikm=DH_send, info="WhisperRatchet")

New State:
    Root Key: RK_2
    Alice's ratchet key pair: (A_priv_1, A_pub_1)  [NEW]
    Bob's ratchet public key: B_pub_1  [NEW]
    Sender chain key: SCK_1  [NEW]
    Receiver chain key: RCK_1  [NEW - for Bob's messages with B_pub_1]
    Old receiver chain: RCK_0  [KEPT - for delayed messages from Bob]
```

### 4.3 Code Implementation

**File: `rust/protocol/src/session_cipher.rs:696-730`**

This code was shown earlier in section 3.4, but let's highlight the DH ratchet sequence:

```rust
    // Step 1: Receive DH ratchet
    let root_key = state.root_key()?;                              // RK_0
    let our_ephemeral = state.sender_ratchet_private_key()?;       // A_priv_0
    let receiver_chain = root_key.create_chain(
        their_ephemeral,      // B_pub_1 (new)
        &our_ephemeral        // A_priv_0 (old)
    )?;
    // receiver_chain = (RK_1, RCK_1)

    // Step 2: Generate new ephemeral key
    let our_new_ephemeral = KeyPair::generate(csprng);             // (A_priv_1, A_pub_1)

    // Step 3: Send DH ratchet
    let sender_chain = receiver_chain
        .0  // RK_1
        .create_chain(
            their_ephemeral,                    // B_pub_1 (new)
            &our_new_ephemeral.private_key      // A_priv_1 (new)
        )?;
    // sender_chain = (RK_2, SCK_1)

    // Update state
    state.set_root_key(&sender_chain.0);                           // RK_2
    state.add_receiver_chain(their_ephemeral, &receiver_chain.1);  // RCK_1
    state.set_sender_chain(&our_new_ephemeral, &sender_chain.1);   // SCK_1 with A_pub_1
```

### 4.4 Security Properties

**Forward Secrecy:**
- Old ratchet private keys are deleted after use
- Compromising `A_priv_1` doesn't reveal `A_priv_0`
- Past message keys cannot be recovered

**Future Secrecy (Break-in Recovery):**
- If attacker compromises state at time T
- First message exchange after T involves new DH exchange
- New root key provides fresh entropy
- Session security restored

**Transcript Consistency:**
- `previous_counter` field in messages tracks last counter before DH ratchet
- Enables detection of missing messages across ratchet boundaries

---

## 5. Out-of-Order Messages

Real-world networks deliver messages out of order. The Double Ratchet handles this gracefully through **message key caching**.

### 5.1 Scenario: Messages Arrive Out of Order

**Sent order:** Message 5, 6, 7, 8, 9
**Received order:** Message 5, 6, **9**, 7, 8

When message 9 arrives before 7 and 8:

```
Chain key at index 6 (after receiving message 6)

Chain key [6] → next → Chain key [7] → message_keys [7] → CACHE
                           ↓
                     Chain key [8] → message_keys [8] → CACHE
                           ↓
                     Chain key [9] → message_keys [9] → DECRYPT message 9
                           ↓
                     Chain key [10] → STORE
```

**Cached message keys:**
- Message 7: stored in session state
- Message 8: stored in session state

When message 7 arrives:
```
Chain key at index 10

Check cache for counter 7 → FOUND → Use cached key → Decrypt
Delete from cache after use
```

### 5.2 Message Key Storage

**File: `rust/protocol/src/state/session.rs:431-475`**

```rust
pub(crate) fn get_message_keys(
    &mut self,
    sender: &PublicKey,
    counter: u32,
) -> Result<Option<MessageKeyGenerator>, InvalidSessionError> {
    if let Some(mut chain_and_index) = self.get_receiver_chain(sender)? {
        let message_key_idx = chain_and_index
            .0
            .message_keys
            .iter()
            .position(|m| m.index == counter);

        if let Some(position) = message_key_idx {
            let message_key = chain_and_index.0.message_keys.remove(position);
            let keys =
                MessageKeyGenerator::from_pb(message_key).map_err(InvalidSessionError)?;

            // Update chain with message key removed
            self.session.receiver_chains[chain_and_index.1] = chain_and_index.0;
            return Ok(Some(keys));
        }
    }

    Ok(None)
}

pub(crate) fn set_message_keys(
    &mut self,
    sender: &PublicKey,
    message_keys: MessageKeyGenerator,
) -> Result<(), InvalidSessionError> {
    let chain_and_index = self
        .get_receiver_chain(sender)?
        .expect("called set_message_keys for a non-existent chain");
    let mut updated_chain = chain_and_index.0;

    updated_chain.message_keys.insert(0, message_keys.into_pb());

    // Enforce limit to prevent DoS
    if updated_chain.message_keys.len() > consts::MAX_MESSAGE_KEYS {
        updated_chain.message_keys.pop();  // Drop oldest
    }

    self.session.receiver_chains[chain_and_index.1] = updated_chain;

    Ok(())
}
```

### 5.3 Limits and DoS Prevention

**`MAX_MESSAGE_KEYS = 2,000`** (from `rust/protocol/src/consts.rs`)

If more than 2,000 message keys are cached, oldest keys are dropped. This prevents:
- **Memory exhaustion attacks**: Attacker sends message with very high counter
- **Storage bloat**: Unbounded state growth

**Trade-off:**
- Legitimate delayed messages beyond 2,000 gaps will fail to decrypt
- In practice, this limit is generous for normal network conditions

### 5.4 Duplicate Message Detection

**File: `rust/protocol/src/session_cipher.rs:742-750`**

```rust
    if chain_index > counter {
        return match state.get_message_keys(their_ephemeral, counter)? {
            Some(keys) => Ok(keys),
            None => {
                log::info!("{remote_address} Duplicate message for counter: {counter}");
                Err(SignalProtocolError::DuplicatedMessage(chain_index, counter))
            }
        };
    }
```

**Duplicate Detection Logic:**

1. Message arrives with counter = 5
2. Chain is currently at index = 8 (we've processed messages 6, 7, 8)
3. Check cache for counter 5
   - **Found**: Use cached key (out-of-order delivery)
   - **Not found**: This is a duplicate! Key was already used and deleted

**Security implication:**
- Prevents replay attacks
- Each message key can only be used once
- After use, key is deleted from cache

---

## 6. SPQR Integration

**SPQR (Signal Post-Quantum Ratchet)** adds post-quantum forward secrecy on top of the classical Double Ratchet.

### 6.1 SPQR Design Goals

1. **Quantum-resistant forward secrecy**: Even quantum computers can't recover past message keys
2. **Independent ratcheting**: PQ ratchet advances with every message
3. **Combined security**: Message keys derived from BOTH classical AND PQ sources
4. **Backwards compatibility**: Graceful fallback if peer doesn't support SPQR

### 6.2 SPQR State Structure

The SPQR library maintains its own ratchet state, separate from the classical Double Ratchet:

```
Classical State:              PQ State (SPQR):
- Root key                    - SPQR state (opaque blob)
- Sender chain key              - Internal PQ ratchet
- Receiver chains               - PQ message keys
- Message key cache             - PQ state cache
```

**File: `rust/protocol/src/state/session.rs:147`**

```rust
    pq_ratchet_state: spqr::SerializedState,
```

This is an opaque byte blob maintained by the SPQR library.

### 6.3 SPQR Initialization

**File: `rust/protocol/src/ratchet.rs:19-39`**

During session establishment, we derive an initial PQ ratchet key:

```rust
fn derive_keys(secret_input: &[u8]) -> (RootKey, ChainKey, InitialPQRKey) {
    derive_keys_with_label(
        b"WhisperText_X25519_SHA-256_CRYSTALS-KYBER-1024",
        secret_input,
    )
}

fn derive_keys_with_label(label: &[u8], secret_input: &[u8]) -> (RootKey, ChainKey, InitialPQRKey) {
    let mut secrets = [0; 96];
    hkdf::Hkdf::<sha2::Sha256>::new(None, secret_input)
        .expand(label, &mut secrets)
        .expect("valid length");
    let (root_key_bytes, chain_key_bytes, pqr_bytes) =
        (&secrets[0..32], &secrets[32..64], &secrets[64..96]);

    let root_key = RootKey::new(root_key_bytes.try_into().expect("correct length"));
    let chain_key = ChainKey::new(chain_key_bytes.try_into().expect("correct length"), 0);
    let pqr_key: InitialPQRKey = pqr_bytes.try_into().expect("correct length");

    (root_key, chain_key, pqr_key)
}
```

The PQXDH handshake provides 224 bytes of shared secret:
- **32 bytes** → Classical root key
- **32 bytes** → Classical chain key
- **32 bytes** → **PQ ratchet authentication key**

**File: `rust/protocol/src/ratchet.rs:101-118`**

```rust
    let pqr_state = spqr::initial_state(spqr::Params {
        auth_key: &pqr_key,              // 32 bytes from PQXDH
        version: spqr::Version::V1,
        direction: spqr::Direction::A2B,  // Alice to Bob
        min_version: spqr::Version::V0,   // Allow fallback to no PQR (for old clients)
        chain_params: spqr_chain_params(self_session),
    })
    .map_err(|e| {
        SignalProtocolError::InvalidArgument(format!(
            "post-quantum ratchet: error creating initial A2B state: {e}"
        ))
    })?;
```

**Direction matters:**
- **Alice (initiator)**: `Direction::A2B`
- **Bob (responder)**: `Direction::B2A`

The direction ensures both parties derive the same PQ ratchet keys in the correct order.

### 6.4 SPQR Send

**File: `rust/protocol/src/state/session.rs:610-617`**

```rust
pub(crate) fn pq_ratchet_send<R: Rng + CryptoRng>(
    &mut self,
    csprng: &mut R,
) -> Result<(spqr::SerializedMessage, spqr::MessageKey), spqr::Error> {
    let spqr::Send { state, key, msg } = spqr::send(&self.session.pq_ratchet_state, csprng)?;
    self.session.pq_ratchet_state = state;  // Update state
    Ok((msg, key))
}
```

**SPQR send operation:**
1. **Input**: Current PQ ratchet state, randomness
2. **Output**:
   - `state`: New PQ ratchet state (replaces old)
   - `key`: 32-byte PQ message key
   - `msg`: Serialized PQ ratchet update (sent in SignalMessage)

**Internal SPQR operations** (conceptual, actual implementation in `spqr` crate):
```
PQ_ratchet_state_N:
    - counter: N
    - shared_secret: secret_N

On send:
    1. pq_message_key = HKDF(secret_N, "send" || N)
    2. secret_N+1 = HKDF(secret_N, "ratchet" || N)
    3. counter = N + 1
    4. msg = serialize(N, proof_of_knowledge)

PQ_ratchet_state_N+1:
    - counter: N + 1
    - shared_secret: secret_N+1
```

### 6.5 SPQR Receive

**File: `rust/protocol/src/state/session.rs:601-608`**

```rust
pub(crate) fn pq_ratchet_recv(
    &mut self,
    msg: &spqr::SerializedMessage,
) -> Result<spqr::MessageKey, spqr::Error> {
    let spqr::Recv { state, key } = spqr::recv(&self.session.pq_ratchet_state, msg)?;
    self.session.pq_ratchet_state = state;
    Ok(key)
}
```

**SPQR receive operation:**
1. **Input**: Current PQ ratchet state, received PQ message
2. **Output**:
   - `state`: New PQ ratchet state
   - `key`: 32-byte PQ message key (matches sender's key)

The receiver processes the PQ ratchet update and derives the same message key as the sender.

### 6.6 Combined Key Derivation

**File: `rust/protocol/src/ratchet/keys.rs:22-34`**

```rust
impl MessageKeyGenerator {
    pub(crate) fn generate_keys(self, pqr_key: spqr::MessageKey) -> MessageKeys {
        match self {
            Self::Seed((seed, counter)) => {
                MessageKeys::derive_keys(&seed, pqr_key.as_deref(), counter)
            }
            Self::Keys(k) => {
                // PQR keys should only be set for newer sessions
                assert!(pqr_key.is_none());
                k
            }
        }
    }
}
```

**File: `rust/protocol/src/ratchet/keys.rs:89-112`**

```rust
pub(crate) fn derive_keys(
    input_key_material: &[u8],      // Classical message key seed
    optional_salt: Option<&[u8]>,   // PQ message key (if SPQR enabled)
    counter: u32,
) -> Self {
    // ... (struct definition) ...

    hkdf::Hkdf::<sha2::Sha256>::new(optional_salt, input_key_material)
        .expand(b"WhisperMessageKeys", okm.as_mut_bytes())
        .expect("valid output length");

    let DerivedSecretBytes(cipher_key, mac_key, iv) = okm;

    MessageKeys {
        cipher_key,
        mac_key,
        iv,
        counter,
    }
}
```

**Combined Key Derivation Formula:**

```
Classical:  chain_key → message_key_seed (32 bytes)
PQ:         spqr_state → pq_message_key (32 bytes)

Combined:   (cipher_key, mac_key, iv) = HKDF-SHA256(
                salt = pq_message_key,
                ikm = message_key_seed,
                info = "WhisperMessageKeys",
                output = 80 bytes
            )
```

**Security properties:**
- **Post-quantum forward secrecy**: PQ ratchet advances independently
- **Defense in depth**: Attacker must break BOTH classical AND post-quantum ratchets
- **Graceful degradation**: If `pq_message_key` is None (old client), falls back to classical security

### 6.7 SPQR Chain Parameters

**File: `rust/protocol/src/ratchet.rs:41-52`**

```rust
fn spqr_chain_params(self_connection: bool) -> spqr::ChainParams {
    spqr::ChainParams {
        max_jump: if self_connection {
            u32::MAX
        } else {
            consts::MAX_FORWARD_JUMPS.try_into().expect("should be <4B")
        },
        max_ooo_keys: consts::MAX_MESSAGE_KEYS.try_into().expect("should be <4B"),
        ..Default::default()
    }
}
```

**SPQR parameters match classical ratchet:**
- **`max_jump`**: Maximum forward jump in message sequence (25,000)
- **`max_ooo_keys`**: Maximum out-of-order keys cached (2,000)
- **Special case**: Self-connections (sending to yourself) allow unlimited jumps

---

## 7. Security Properties

Let's summarize the security properties achieved by this message encryption flow.

### 7.1 Confidentiality

**AES-256-CBC Encryption:**
- **Key size**: 256 bits (128-bit security against quantum attacks via Grover's algorithm)
- **IV**: Unique per message (derived from message key seed)
- **Mode**: CBC with PKCS#7 padding

**Key derivation:**
- Fresh message key for every message
- Derived from both classical and post-quantum ratchets

### 7.2 Authenticity

**HMAC-SHA256 MAC:**
- **Truncated to 8 bytes**: 64-bit security (2^64 forgery attempts)
- **Covers**: Version, all protobuf fields, PQ ratchet update
- **Binds**: Sender identity, receiver identity, message content

**Purpose:**
- Prevents forgery
- Prevents tampering
- Binds message to specific identity key pair

### 7.3 Forward Secrecy

**Classical Double Ratchet:**
- Chain keys deleted after use
- Old message keys irrecoverable even with current state compromise

**Post-Quantum (SPQR):**
- PQ ratchet advances with every message
- Past PQ keys deleted
- **Quantum-resistant forward secrecy**

**Combined:**
- Attacker must compromise device BEFORE message and break BOTH classical and PQ ratchets
- Extremely strong forward secrecy guarantees

### 7.4 Future Secrecy (Break-in Recovery)

**DH Ratchet:**
- New ephemeral key pair generated on sender ratchet step
- Fresh DH exchange provides new entropy
- Root key updated with new shared secret

**Recovery:**
1. Attacker compromises state at time T
2. First message exchange after T involves DH ratchet
3. New ephemeral keys provide fresh entropy unknown to attacker
4. Session security restored

### 7.5 Deniability

**Cryptographic deniability:**
- MAC keys are symmetric (both parties can compute them)
- Either party could have forged a message (cryptographically)
- No digital signatures that prove sender identity to third party

**Note:** Metadata (who sent when) may not be deniable depending on transport layer.

### 7.6 Replay Protection

**Counter-based:**
- Each message has a counter (chain key index)
- Message keys used once and deleted
- Duplicate counters detected and rejected

**Scope:**
- Per receiver chain (per sender ratchet key)
- Replays across different chains possible (different ephemeral keys)

### 7.7 Out-of-Order Delivery

**Message key caching:**
- Up to 2,000 message keys cached per chain
- Delayed messages within window can be decrypted
- Beyond window: decryption fails (graceful degradation)

### 7.8 Denial of Service Resistance

**Limits enforced:**
- **`MAX_FORWARD_JUMPS = 25,000`**: Prevents excessive key derivation
- **`MAX_MESSAGE_KEYS = 2,000`**: Prevents memory exhaustion
- **`MAX_RECEIVER_CHAINS = 5`**: Prevents chain proliferation

**Trade-offs:**
- Legitimate edge cases beyond limits will fail
- Limits are generous for normal operation
- Protects against malicious or buggy peers

### 7.9 Post-Quantum Security

**SPQR integration:**
- Every message benefits from PQ ratchet
- Message keys require breaking BOTH classical and PQ
- Quantum computer would need to break:
  1. ECDH (Shor's algorithm) AND
  2. SPQR (no known quantum attack)

**Current quantum threat:**
- Large-scale quantum computers don't exist yet (2025)
- SPQR provides insurance against future threats
- "Harvest now, decrypt later" attacks mitigated

---

## Conclusion

This chapter has provided a comprehensive, line-by-line walkthrough of message encryption and decryption in libsignal. We've seen:

1. **Session state structure**: How keys are organized and stored
2. **Encryption flow**: From plaintext to SignalMessage with MAC
3. **Decryption flow**: MAC verification, key derivation, AES decryption
4. **DH Ratchet**: How fresh entropy is introduced via ephemeral key exchanges
5. **Out-of-order handling**: Message key caching for network realities
6. **SPQR integration**: Post-quantum forward secrecy on every message

The implementation demonstrates **defense in depth**:
- **Multiple layers**: Classical ratchet + PQ ratchet
- **Multiple checks**: MAC verification, version checks, identity verification
- **Graceful degradation**: Handles old sessions, out-of-order messages
- **DoS protection**: Limits on jumps, cached keys, receiver chains

The code is production-grade, handling real-world edge cases while maintaining strong cryptographic guarantees. This is the beating heart of Signal's end-to-end encryption — billions of messages encrypted and decrypted through this exact code path.

**Next chapter**: We'll explore group messaging with Sender Keys, which builds on these primitives to enable efficient multi-party encryption.

---

## References

**Source Files:**
- `/home/user/libsignal/rust/protocol/src/session_cipher.rs`
- `/home/user/libsignal/rust/protocol/src/ratchet.rs`
- `/home/user/libsignal/rust/protocol/src/ratchet/keys.rs`
- `/home/user/libsignal/rust/protocol/src/state/session.rs`
- `/home/user/libsignal/rust/protocol/src/protocol.rs`
- `/home/user/libsignal/rust/protocol/src/crypto.rs`
- `/home/user/libsignal/rust/protocol/src/consts.rs`

**Protocol Specifications:**
- The Double Ratchet Algorithm: https://signal.org/docs/specifications/doubleratchet/
- The X3DH Key Agreement Protocol: https://signal.org/docs/specifications/x3dh/
- More Privacy, Less Harvesting with PQXDH and SPQR (Signal Blog, Sept 2023)

**Academic Papers:**
- Cohn-Gordon, Cremers, et al. "A Formal Security Analysis of the Signal Messaging Protocol" (2017)
- Alwen, Coretti, Dodis. "The Double Ratchet: Security Notions, Proofs, and Modularization for the Signal Protocol" (2019)

**Test Files:**
- `/home/user/libsignal/rust/protocol/tests/session.rs`
- `/home/user/libsignal/rust/protocol/tests/ratchet.rs`

---

*This chapter is part of the libsignal Encyclopedia — A comprehensive guide to Signal's cryptographic protocol library.*

*Version: Based on libsignal v0.86.5 (November 2025)*
