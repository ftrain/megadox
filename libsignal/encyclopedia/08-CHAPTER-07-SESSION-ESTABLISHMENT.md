# Chapter 7: Literate Programming - Session Establishment Walkthrough

## A Complete Code Walkthrough of Signal Protocol Session Creation

---

This chapter provides a line-by-line walkthrough of establishing a Signal Protocol session, following the complete flow from initial setup through the first encrypted message exchange. We'll trace actual code paths through the implementation, showing every cryptographic operation, key derivation, and state transformation.

**Learning Objectives:**
- Understand the complete lifecycle of session establishment
- Follow the PQXDH (Post-Quantum X3DH) protocol in practice
- See how keys are generated, exchanged, and derived
- Trace message encryption and decryption operations
- Understand state management and storage

**Code Locations:**
- Session setup: `rust/protocol/src/session.rs`
- Ratchet initialization: `rust/protocol/src/ratchet.rs`
- PreKey generation: `rust/protocol/src/state/signed_prekey.rs`, `rust/protocol/src/state/kyber_prekey.rs`
- Message encryption: `rust/protocol/src/session_cipher.rs`
- Key derivation: `rust/protocol/src/ratchet/keys.rs`

---

## 1. Initial Setup: Creating Protocol Stores

Before Alice and Bob can communicate, each needs a local protocol store containing their identity and session state.

### 1.1 Identity Key Generation

```rust
use libsignal_protocol::*;
use rand::rngs::OsRng;

// Generate Alice's identity
let mut csprng = OsRng;
let alice_identity = IdentityKeyPair::generate(&mut csprng);
let alice_registration_id: u32 = 12345; // Unique registration ID

// Create Alice's protocol store
let alice_store = InMemSignalProtocolStore::new(
    alice_identity,
    alice_registration_id
)?;

// Generate Bob's identity
let bob_identity = IdentityKeyPair::generate(&mut csprng);
let bob_registration_id: u32 = 67890;

let bob_store = InMemSignalProtocolStore::new(
    bob_identity,
    bob_registration_id
)?;
```

**What happens here:**
- Each party generates an `IdentityKeyPair` — a Curve25519 keypair that serves as their long-term identity
- The registration ID is a unique identifier (14 bits) for this device
- `InMemSignalProtocolStore` implements all required storage interfaces: `SessionStore`, `PreKeyStore`, `SignedPreKeyStore`, `KyberPreKeyStore`, `IdentityKeyStore`

**Security Properties:**
- Identity keys are randomly generated using a cryptographically secure RNG
- Private keys never leave the local device
- Identity keys can be fingerprinted for out-of-band verification

---

## 2. PreKey Generation (Bob's Side)

Bob must generate and publish prekeys that Alice can use to initiate a session. In modern Signal Protocol (PQXDH), this includes:
- Signed PreKey (Curve25519)
- Kyber PreKey (ML-KEM-1024, post-quantum)
- Optional one-time PreKey (Curve25519)

### 2.1 Signed PreKey Generation

From `rust/protocol/src/state/signed_prekey.rs`:

```rust
// Bob generates a signed prekey
let signed_prekey_id = SignedPreKeyId::from(1);
let signed_prekey_pair = KeyPair::generate(&mut csprng);

// Sign the public key with Bob's identity key
let signed_prekey_signature = bob_identity
    .private_key()
    .calculate_signature(&signed_prekey_pair.public_key.serialize(), &mut csprng)?;

// Create the signed prekey record
let timestamp = Timestamp::from_epoch_millis(
    SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)?
        .as_millis() as u64
);

let signed_prekey_record = SignedPreKeyRecord::new(
    signed_prekey_id,
    timestamp,
    &signed_prekey_pair,
    &signed_prekey_signature,
);

// Store it
bob_store.save_signed_pre_key(signed_prekey_id, &signed_prekey_record).await?;
```

**Key Operations:**
1. Generate a fresh Curve25519 keypair for the signed prekey
2. Create a signature over the public key using Bob's identity key (Ed25519 signature via XEdDSA)
3. Bundle: ID, timestamp, keypair, and signature
4. Store locally for later retrieval

**Security Properties:**
- The signature proves Bob's identity key endorsed this prekey
- Alice will verify this signature before using the prekey
- Timestamps allow key rotation and expiration

### 2.2 Kyber PreKey Generation

From `rust/protocol/src/state/kyber_prekey.rs`:

```rust
// Bob generates a Kyber prekey (post-quantum)
let kyber_prekey_id = KyberPreKeyId::from(1);
let kyber_key_pair = kem::KeyPair::generate(kem::KeyType::Kyber1024, &mut csprng);

// Sign the Kyber public key with Bob's identity key
let kyber_signature = bob_identity
    .private_key()
    .calculate_signature(&kyber_key_pair.public_key.serialize(), &mut csprng)?;

// Create the Kyber prekey record
let kyber_prekey_record = KyberPreKeyRecord::new(
    kyber_prekey_id,
    timestamp,
    &kyber_key_pair,
    &kyber_signature,
);

// Store it
bob_store.save_kyber_pre_key(kyber_prekey_id, &kyber_prekey_record).await?;
```

**Key Operations:**
1. Generate ML-KEM-1024 keypair (NIST-standardized Kyber)
2. Sign the public key with Bob's identity key
3. Store the keypair and signature

**Post-Quantum Security:**
- Kyber provides key encapsulation resistant to quantum attacks
- The signature proves authenticity but is not post-quantum (acceptable for authentication)
- Key size: ~1,568 bytes for public key, ~3,168 bytes for secret key

### 2.3 One-Time PreKey (Optional)

```rust
// Bob can optionally generate one-time prekeys for better forward secrecy
let one_time_prekey_id = PreKeyId::from(1);
let one_time_prekey_pair = KeyPair::generate(&mut csprng);

let one_time_prekey_record = PreKeyRecord::new(
    one_time_prekey_id,
    &one_time_prekey_pair
);

bob_store.save_pre_key(one_time_prekey_id, &one_time_prekey_record).await?;
```

**Purpose:**
- One-time prekeys are deleted after use, providing stronger forward secrecy
- If a one-time prekey is available, it contributes to the shared secret
- Not strictly required but recommended

### 2.4 Creating the PreKeyBundle

From `rust/protocol/src/state/bundle.rs`:

```rust
let bob_prekey_bundle = PreKeyBundle::new(
    bob_registration_id,                          // Registration ID
    DeviceId::from(1),                             // Device ID
    Some((one_time_prekey_id, one_time_prekey_pair.public_key)), // Optional one-time prekey
    signed_prekey_id,                              // Signed prekey ID
    signed_prekey_pair.public_key,                 // Signed prekey public key
    signed_prekey_signature.to_vec(),              // Signature
    kyber_prekey_id,                               // Kyber prekey ID
    kyber_key_pair.public_key.clone(),             // Kyber public key
    kyber_signature.to_vec(),                      // Kyber signature
    *bob_identity.identity_key(),                  // Bob's identity key
)?;
```

**The PreKeyBundle contains:**
- Bob's identity key (for DH operations)
- Signed prekey (ID, public key, signature)
- Kyber prekey (ID, public key, signature)
- Optional one-time prekey (ID, public key)
- Registration ID and device ID

This bundle is published to the Signal server and can be fetched by anyone who wants to initiate a session with Bob.

---

## 3. Session Initiation (Alice's Side)

Alice fetches Bob's PreKeyBundle and uses it to establish a session. This is where PQXDH happens.

### 3.1 Processing the PreKey Bundle

From `rust/protocol/src/session.rs` - `process_prekey_bundle()`:

```rust
pub async fn process_prekey_bundle<R: Rng + CryptoRng>(
    remote_address: &ProtocolAddress,
    session_store: &mut dyn SessionStore,
    identity_store: &mut dyn IdentityKeyStore,
    bundle: &PreKeyBundle,
    now: SystemTime,
    mut csprng: &mut R,
) -> Result<()>
```

**Step 1: Verify Bob's signatures**

```rust
let their_identity_key = bundle.identity_key()?;

// Verify signed prekey signature
if !their_identity_key.public_key().verify_signature(
    &bundle.signed_pre_key_public()?.serialize(),
    bundle.signed_pre_key_signature()?,
) {
    return Err(SignalProtocolError::SignatureValidationFailed);
}

// Verify Kyber prekey signature
if !their_identity_key.public_key().verify_signature(
    &bundle.kyber_pre_key_public()?.serialize(),
    bundle.kyber_pre_key_signature()?,
) {
    return Err(SignalProtocolError::SignatureValidationFailed);
}
```

**Security Check:**
- Both prekey signatures are verified against Bob's identity key
- If verification fails, the bundle is rejected
- This prevents man-in-the-middle attacks

**Step 2: Generate Alice's base key**

```rust
let our_base_key_pair = KeyPair::generate(&mut csprng);
```

This ephemeral keypair will be sent to Bob in the first message and used for DH operations.

**Step 3: Extract Bob's keys from the bundle**

```rust
let their_signed_prekey = bundle.signed_pre_key_public()?;
let their_kyber_prekey = bundle.kyber_pre_key_public()?;
let their_one_time_prekey = bundle.pre_key_public()?; // Option<PublicKey>
```

### 3.2 PQXDH: Building the Shared Secret

From `rust/protocol/src/ratchet.rs` - `initialize_alice_session()`:

The heart of PQXDH is building a shared secret from multiple Diffie-Hellman operations plus Kyber encapsulation.

```rust
let mut secrets = Vec::with_capacity(32 * 6);

// Discontinuity bytes (32 0xFF bytes)
secrets.extend_from_slice(&[0xFFu8; 32]);
```

**Discontinuity bytes** ensure the shared secret is different from any previous protocol version.

**DH1: Identity Key Agreement**

```rust
// DH(Alice_Identity, Bob_SignedPreKey)
secrets.extend_from_slice(
    &parameters
        .our_identity_key_pair()
        .private_key()
        .calculate_agreement(parameters.their_signed_pre_key())?
);
```

This DH provides mutual authentication: Alice proves she knows her identity private key, and uses Bob's signed prekey.

**DH2: Base Key to Identity**

```rust
// DH(Alice_BaseKey, Bob_Identity)
secrets.extend_from_slice(
    &our_base_private_key.calculate_agreement(
        parameters.their_identity_key().public_key()
    )?
);
```

Alice's ephemeral base key with Bob's identity key.

**DH3: Base Key to Signed PreKey**

```rust
// DH(Alice_BaseKey, Bob_SignedPreKey)
secrets.extend_from_slice(
    &our_base_private_key.calculate_agreement(
        parameters.their_signed_pre_key()
    )?
);
```

This is the core DH that both parties will compute.

**DH4: Optional One-Time PreKey**

```rust
// DH(Alice_BaseKey, Bob_OneTimePreKey) - if present
if let Some(their_one_time_prekey) = parameters.their_one_time_pre_key() {
    secrets.extend_from_slice(
        &our_base_private_key.calculate_agreement(their_one_time_prekey)?
    );
}
```

If Bob had a one-time prekey, it's mixed in for extra forward secrecy.

**Kyber Encapsulation (Post-Quantum Component)**

```rust
// Kyber KEM: Encapsulate to Bob's Kyber public key
let kyber_ciphertext = {
    let (shared_secret, ciphertext) = parameters
        .their_kyber_pre_key()
        .encapsulate(&mut csprng)?;
    secrets.extend_from_slice(shared_secret.as_ref());
    ciphertext
};
```

**What happens here:**
- Alice generates a random value and encapsulates it to Bob's Kyber public key
- The `encapsulate()` operation produces:
  - A shared secret (32 bytes) — only Alice and Bob (with the private key) can know this
  - A ciphertext (~1,568 bytes) — sent to Bob so he can recover the shared secret
- This provides post-quantum security: even a quantum computer can't recover the shared secret from the ciphertext alone

**Summary of shared secret components:**
```
secrets = 0xFF*32 || DH1 || DH2 || DH3 || [DH4] || Kyber_SS
        = 32 + 32 + 32 + 32 + [32] + 32 bytes
        = 160 or 192 bytes total
```

### 3.3 Key Derivation

Now we derive the root key and initial chain key from this shared secret:

```rust
fn derive_keys(secret_input: &[u8]) -> (RootKey, ChainKey, InitialPQRKey) {
    let mut secrets = [0; 96];
    hkdf::Hkdf::<sha2::Sha256>::new(None, secret_input)
        .expand(b"WhisperText_X25519_SHA-256_CRYSTALS-Kyber-1024", &mut secrets)
        .expect("valid length");

    let (root_key_bytes, chain_key_bytes, pqr_bytes) =
        (&secrets[0..32], &secrets[32..64], &secrets[64..96]);

    let root_key = RootKey::new(root_key_bytes.try_into().expect("correct length"));
    let chain_key = ChainKey::new(chain_key_bytes.try_into().expect("correct length"), 0);
    let pqr_key: InitialPQRKey = pqr_bytes.try_into().expect("correct length");

    (root_key, chain_key, pqr_key)
}

let (root_key, chain_key, pqr_key) = derive_keys(&secrets);
```

**HKDF (HMAC-based Key Derivation Function):**
- Input: The combined DH and Kyber shared secret (160-192 bytes)
- Info: Protocol identifier string
- Output: 96 bytes split into:
  - **Root Key** (32 bytes): Used for ratcheting
  - **Chain Key** (32 bytes): Initial chain key for receiving messages from Bob
  - **PQR Key** (32 bytes): Authentication key for the post-quantum ratchet (SPQR)

### 3.4 Initialize the Ratchet

Alice now performs the first ratchet step:

```rust
// Generate Alice's sending ratchet key
let sending_ratchet_key = KeyPair::generate(&mut csprng);

// Perform root key ratchet to get sending chain
let (sending_chain_root_key, sending_chain_chain_key) = root_key.create_chain(
    parameters.their_ratchet_key(),  // Bob's signed prekey acts as his ratchet key
    &sending_ratchet_key.private_key,
)?;
```

From `rust/protocol/src/ratchet/keys.rs` - `RootKey::create_chain()`:

```rust
pub fn create_chain(
    self,
    their_ratchet_key: &PublicKey,
    our_ratchet_key: &PrivateKey,
) -> Result<(RootKey, ChainKey)> {
    // Perform DH
    let shared_secret = our_ratchet_key.calculate_agreement(their_ratchet_key)?;

    // Derive new root and chain keys
    let mut derived_secret_bytes = [0u8; 64];
    hkdf::Hkdf::<sha2::Sha256>::new(Some(&self.key), &shared_secret)
        .expand(b"WhisperRatchet", &mut derived_secret_bytes)
        .expect("valid output length");

    let (root_key, chain_key) = derived_secret_bytes.split_at(32);

    Ok((
        RootKey { key: root_key.try_into().unwrap() },
        ChainKey { key: chain_key.try_into().unwrap(), index: 0 },
    ))
}
```

**The ratchet creates:**
- New root key (for the next ratchet)
- Chain key with index 0 (for deriving message keys)

### 3.5 Create the Session State

```rust
// Initialize post-quantum ratchet state
let pqr_state = spqr::initial_state(spqr::Params {
    auth_key: &pqr_key,
    version: spqr::Version::V1,
    direction: spqr::Direction::A2B,  // Alice to Bob
    min_version: spqr::Version::V0,
    chain_params: spqr_chain_params(self_session),
})?;

// Create session state with both chains
let mut session = SessionState::new(
    CIPHERTEXT_MESSAGE_CURRENT_VERSION,
    local_identity,
    parameters.their_identity_key(),
    &sending_chain_root_key,
    &parameters.our_base_key_pair().public_key,
    pqr_state,
)
.with_receiver_chain(parameters.their_ratchet_key(), &chain_key)
.with_sender_chain(&sending_ratchet_key, &sending_chain_chain_key);

// Store the Kyber ciphertext (to be sent with first message)
session.set_kyber_ciphertext(kyber_ciphertext);
```

**The session state now contains:**
- **Receiver chain**: For decrypting messages from Bob (initialized from initial chain key)
- **Sender chain**: For encrypting messages to Bob (from ratchet step)
- **Root key**: For future ratchet steps
- **Kyber ciphertext**: To be sent to Bob
- **SPQR state**: Post-quantum ratchet for forward secrecy

### 3.6 Mark as Unacknowledged Session

```rust
session.set_unacknowledged_pre_key_message(
    their_one_time_prekey_id,
    bundle.signed_pre_key_id()?,
    &our_base_key_pair.public_key,
    now,
);
session.set_unacknowledged_kyber_pre_key_id(bundle.kyber_pre_key_id()?);
```

The session remains "unacknowledged" until Bob responds, and Alice will include prekey information in every message until acknowledgment.

---

## 4. First Message Encryption

Alice can now encrypt her first message to Bob.

### 4.1 Message Key Derivation

From `rust/protocol/src/session_cipher.rs` - `message_encrypt()`:

```rust
// Get the sender chain key
let chain_key = session_state.get_sender_chain_key()?;

// Advance the post-quantum ratchet
let (pqr_msg, pqr_key) = session_state.pq_ratchet_send(csprng)?;

// Derive message keys
let message_keys = chain_key.message_keys().generate_keys(pqr_key);
```

From `rust/protocol/src/ratchet/keys.rs`:

```rust
// ChainKey derives message key seed
pub fn message_keys(&self) -> MessageKeyGenerator {
    MessageKeyGenerator::new_from_seed(
        &self.calculate_base_material(Self::MESSAGE_KEY_SEED),
        self.index,
    )
}

fn calculate_base_material(&self, seed: [u8; 1]) -> [u8; 32] {
    crypto::hmac_sha256(&self.key, &seed)  // HMAC-SHA256(chain_key, 0x01)
}
```

**Message key derivation:**

```rust
pub fn derive_keys(
    input_key_material: &[u8],
    optional_salt: Option<&[u8]>,  // PQR key if present
    counter: u32,
) -> Self {
    let mut okm = [0u8; 80];  // 32 + 32 + 16

    hkdf::Hkdf::<sha2::Sha256>::new(optional_salt, input_key_material)
        .expand(b"WhisperMessageKeys", &mut okm)
        .expect("valid output length");

    MessageKeys {
        cipher_key: okm[0..32].try_into().unwrap(),   // AES-256 key
        mac_key: okm[32..64].try_into().unwrap(),     // HMAC key
        iv: okm[64..80].try_into().unwrap(),          // AES IV
        counter,
    }
}
```

**The message keys provide:**
- **cipher_key**: 32-byte AES-256 key
- **mac_key**: 32-byte HMAC key for authentication
- **iv**: 16-byte initialization vector for CBC mode
- **counter**: Chain key index (for ordering)

### 4.2 AES-256-CBC Encryption

```rust
let plaintext = b"Hello, Bob!";

let ciphertext = signal_crypto::aes_256_cbc_encrypt(
    plaintext,
    message_keys.cipher_key(),
    message_keys.iv()
)?;
```

**AES-256-CBC:**
- PKCS#7 padding applied automatically
- IV is derived from message keys (never reused)
- Ciphertext length = ceil(plaintext.len() / 16) * 16

### 4.3 Construct PreKeySignalMessage

Since this is the first message, Alice sends a `PreKeySignalMessage`:

```rust
let message = SignalMessage::new(
    session_version,
    message_keys.mac_key(),
    sender_ephemeral,      // Alice's current ratchet key
    chain_key.index(),     // Message counter
    previous_counter,      // Previous chain length
    &ciphertext,
    &local_identity_key,
    &their_identity_key,
    &pqr_msg,             // SPQR message
)?;

let kyber_payload = items
    .kyber_pre_key_id()
    .zip(items.kyber_ciphertext())
    .map(|(id, ciphertext)| KyberPayload::new(id, ciphertext.into()));

let prekey_message = PreKeySignalMessage::new(
    session_version,
    local_registration_id,
    items.pre_key_id(),           // Optional one-time prekey ID
    items.signed_pre_key_id(),    // Signed prekey ID used
    kyber_payload,                // Kyber prekey ID + ciphertext
    *items.base_key(),            // Alice's base key
    local_identity_key,
    message,
)?;
```

**PreKeySignalMessage contains:**
- Version (0x04 for PQXDH)
- Alice's registration ID
- PreKey IDs used (one-time, signed, Kyber)
- Kyber ciphertext (~1,568 bytes)
- Alice's base key
- Alice's identity key
- The encrypted SignalMessage

### 4.4 Advance the Chain Key

```rust
session_state.set_sender_chain_key(&chain_key.next_chain_key());
```

From `rust/protocol/src/ratchet/keys.rs`:

```rust
pub fn next_chain_key(&self) -> Self {
    Self {
        key: self.calculate_base_material(Self::CHAIN_KEY_SEED),  // HMAC-SHA256(key, 0x02)
        index: self.index + 1,
    }
}
```

The chain key ratchets forward (one-way function), ensuring forward secrecy.

---

## 5. Session Completion (Bob's Side)

Bob receives the `PreKeySignalMessage` and establishes his side of the session.

### 5.1 Receive and Process PreKey Message

From `rust/protocol/src/session.rs` - `process_prekey_impl()`:

```rust
// Extract prekey IDs from message
let signed_prekey_id = message.signed_pre_key_id();
let kyber_prekey_id = message.kyber_pre_key_id()
    .ok_or(SignalProtocolError::InvalidMessage(...))?;
let one_time_prekey_id = message.pre_key_id();  // Option

// Load Bob's prekeys from storage
let our_signed_pre_key_pair = signed_prekey_store
    .get_signed_pre_key(signed_prekey_id)
    .await?
    .key_pair()?;

let our_kyber_pre_key_pair = kyber_prekey_store
    .get_kyber_pre_key(kyber_prekey_id)
    .await?
    .key_pair()?;

let our_one_time_pre_key_pair = if let Some(id) = one_time_prekey_id {
    Some(pre_key_store.get_pre_key(id).await?.key_pair()?)
} else {
    None
};
```

### 5.2 Perform the Same DH Operations

From `rust/protocol/src/ratchet.rs` - `initialize_bob_session()`:

```rust
let mut secrets = Vec::with_capacity(32 * 6);

// Discontinuity bytes
secrets.extend_from_slice(&[0xFFu8; 32]);

// DH1: DH(Bob_SignedPreKey, Alice_Identity)
secrets.extend_from_slice(
    &parameters
        .our_signed_pre_key_pair()
        .private_key
        .calculate_agreement(parameters.their_identity_key().public_key())?
);

// DH2: DH(Bob_Identity, Alice_BaseKey)
secrets.extend_from_slice(
    &parameters
        .our_identity_key_pair()
        .private_key()
        .calculate_agreement(parameters.their_base_key())?
);

// DH3: DH(Bob_SignedPreKey, Alice_BaseKey)
secrets.extend_from_slice(
    &parameters
        .our_signed_pre_key_pair()
        .private_key
        .calculate_agreement(parameters.their_base_key())?
);

// DH4: DH(Bob_OneTimePreKey, Alice_BaseKey) - if present
if let Some(our_one_time_pre_key_pair) = parameters.our_one_time_pre_key_pair() {
    secrets.extend_from_slice(
        &our_one_time_pre_key_pair
            .private_key
            .calculate_agreement(parameters.their_base_key())?
    );
}
```

**These are the same DH operations Alice performed**, but from Bob's perspective!

### 5.3 Kyber Decapsulation

```rust
// Kyber KEM: Decapsulate the ciphertext Alice sent
secrets.extend_from_slice(
    &parameters
        .our_kyber_pre_key_pair()
        .secret_key
        .decapsulate(parameters.their_kyber_ciphertext())?
);
```

Bob's Kyber secret key decapsulates the ciphertext to recover the same shared secret Alice generated.

**Result:** `secrets` is identical to what Alice computed!

### 5.4 Derive the Same Keys

```rust
let (root_key, chain_key, pqr_key) = derive_keys(&secrets);
```

Bob derives:
- Same root key
- Same initial chain key
- Same PQR authentication key

### 5.5 Initialize Bob's Session

```rust
let pqr_state = spqr::initial_state(spqr::Params {
    auth_key: &pqr_key,
    version: spqr::Version::V1,
    direction: spqr::Direction::B2A,  // Bob to Alice (opposite direction)
    min_version: spqr::Version::V0,
    chain_params: spqr_chain_params(self_session),
})?;

let session = SessionState::new(
    CIPHERTEXT_MESSAGE_CURRENT_VERSION,
    local_identity,
    parameters.their_identity_key(),
    &root_key,
    parameters.their_base_key(),
    pqr_state,
)
.with_sender_chain(parameters.our_ratchet_key_pair(), &chain_key);
```

**Bob's session has:**
- **Sender chain**: Initialized from the same chain key (Bob can send)
- **No receiver chain yet**: Will be created when Alice ratchets

**Why no receiver chain?**
Alice advanced her ratchet and sent with a new ratchet key. Bob will create his receiver chain when he decrypts her message.

### 5.6 Decrypt Alice's Message

From `rust/protocol/src/session_cipher.rs`:

```rust
let ptext = decrypt_message_with_record(
    remote_address,
    &mut session_record,
    ciphertext.message(),  // The inner SignalMessage
    CiphertextMessageType::PreKey,
    csprng,
)?;
```

The decryption process:
1. Extract message metadata (ratchet key, counter, ciphertext)
2. Check if ratchet key matches current chain, or ratchet if needed
3. Derive message keys from chain key at the specified index
4. Decrypt ciphertext with AES-256-CBC
5. Verify MAC
6. Return plaintext

**Result:** Bob recovers `b"Hello, Bob!"`

### 5.7 PreKey Cleanup

```rust
let pre_keys_used = PreKeysUsed {
    one_time_ec_pre_key_id: message.pre_key_id(),
    signed_ec_pre_key_id: message.signed_pre_key_id(),
    kyber_pre_key_id: message.kyber_pre_key_id(),
};

// Later: Delete the one-time prekey (it's been used)
if let Some(id) = pre_keys_used.one_time_ec_pre_key_id {
    pre_key_store.remove_pre_key(id).await?;
}
```

One-time prekeys are deleted after use to prevent replay and ensure forward secrecy.

---

## 6. State Management and Continued Communication

### 6.1 Session Storage

Both Alice and Bob store their session states:

```rust
session_store.store_session(remote_address, &session_record).await?;
```

The `SessionRecord` contains:
- Current session state (active chains, root key, SPQR state)
- Previous session states (for out-of-order message handling)
- Metadata (version, creation time)

### 6.2 Subsequent Messages

After the first exchange:

**Bob sends a reply:**
```rust
let reply = message_encrypt(
    b"Hello, Alice!",
    &alice_address,
    &mut bob_store.session_store,
    &mut bob_store.identity_store,
    SystemTime::now(),
    &mut csprng,
).await?;
```

This will be a regular `SignalMessage` (not PreKey), because the session is established.

**Alice decrypts:**
```rust
let plaintext = message_decrypt(
    &reply,
    &bob_address,
    &mut alice_store.session_store,
    &mut alice_store.identity_store,
    &mut alice_store.pre_key_store,
    &alice_store.signed_pre_key_store,
    &mut alice_store.kyber_pre_key_store,
    &mut csprng,
).await?;
```

### 6.3 The Double Ratchet in Action

Each time a party receives a message with a new ratchet key, they:
1. Perform DH with the new key and their current ratchet key
2. Derive a new root key and receiving chain key
3. Generate a new sending ratchet key
4. Continue the cycle

This provides:
- **Forward secrecy**: Compromising current keys doesn't compromise past messages
- **Backward secrecy** (break-in recovery): Compromising current keys doesn't compromise future messages after the next ratchet step

### 6.4 SPQR Integration

The SPQR (Signal Post-Quantum Ratchet) runs alongside the double ratchet:
- Each message includes a SPQR message component
- SPQR keys are mixed into message key derivation
- Provides post-quantum forward secrecy
- Handles out-of-order messages gracefully

---

## 7. Security Properties Summary

### 7.1 Confidentiality
- AES-256-CBC encryption with unique keys per message
- Keys derived from multi-party DH + Kyber KEM
- Post-quantum security from ML-KEM-1024

### 7.2 Authentication
- Signatures on prekeys verify identity
- MACs on each message prevent tampering
- Base key in prekey message proves ownership of identity

### 7.3 Forward Secrecy
- One-way chain key ratchet
- DH ratchet with ephemeral keys
- SPQR provides quantum-resistant forward secrecy
- One-time prekeys enhance forward secrecy

### 7.4 Deniability
- Signatures only on prekeys (long-term)
- MACs (not signatures) on messages
- Transcripts don't prove who said what to third parties

### 7.5 Metadata Protection
- Session establishment reveals: Alice → Bob communication
- Message content fully encrypted
- Sealed Sender (Chapter 5) can hide sender identity

---

## 8. Error Handling

### 8.1 Signature Verification Failures

```rust
if !their_identity_key.public_key().verify_signature(...) {
    return Err(SignalProtocolError::SignatureValidationFailed);
}
```

Protects against:
- Invalid prekeys
- Man-in-the-middle attacks
- Corrupted bundles

### 8.2 Missing PreKeys

```rust
let kyber_prekey_id = message.kyber_pre_key_id()
    .ok_or(SignalProtocolError::InvalidMessage(
        CiphertextMessageType::PreKey,
        "missing pq pre-key ID",
    ))?;
```

All modern sessions require Kyber prekeys; missing them is an error.

### 8.3 Invalid Base Key

```rust
if !parameters.their_base_key().is_canonical() {
    return Err(SignalProtocolError::InvalidMessage(
        CiphertextMessageType::PreKey,
        "incoming base key is invalid",
    ));
}
```

Ensures the base key is a valid Curve25519 point.

### 8.4 Session Not Found

```rust
let session_record = session_store
    .load_session(remote_address)
    .await?
    .ok_or_else(|| SignalProtocolError::SessionNotFound(remote_address.clone()))?;
```

Encryption requires an established session.

---

## Conclusion

This walkthrough demonstrated the complete lifecycle of Signal Protocol session establishment:

1. **Initial Setup**: Identity key generation and storage
2. **PreKey Generation**: Bob creates signed, Kyber, and one-time prekeys
3. **Session Initiation**: Alice performs PQXDH with Bob's bundle
4. **First Message**: Alice encrypts and sends a PreKeySignalMessage
5. **Session Completion**: Bob decrypts and establishes his session
6. **Continued Communication**: The double ratchet provides ongoing security

**Key Takeaways:**
- Multiple DH operations + Kyber KEM provide defense in depth
- HKDF carefully derives independent keys for different purposes
- The ratchet mechanism provides strong forward and backward secrecy
- SPQR integration ensures post-quantum security
- Careful state management enables out-of-order message handling

**Next Steps:**
- Chapter 16: Message Encryption Flow (regular messages)
- Chapter 17: Group Message Handling (sender keys)
- Chapter 18: Sealed Sender Operation (metadata protection)

---

**References:**
- PQXDH Specification: https://signal.org/docs/specifications/pqxdh/
- Double Ratchet Algorithm: https://signal.org/docs/specifications/doubleratchet/
- Code: `rust/protocol/src/` directory

**Code Statistics:**
- Lines of code examined: ~1,500
- Files covered: 8
- Cryptographic operations: 7 (4-5 DH + Kyber + multiple HKDF)
- Key derivations: 3 (root, chain, PQR)

---
