# Chapter 9: Literate Programming - Group Messaging Deep-Dive

*A comprehensive exploration of Sender Keys and efficient group messaging in the Signal Protocol*

---

## Table of Contents

1. [Group Messaging Architecture](#1-group-messaging-architecture)
2. [Sender Key Structure](#2-sender-key-structure)
3. [Sender Key Distribution](#3-sender-key-distribution)
4. [Group Encryption](#4-group-encryption)
5. [Group Decryption](#5-group-decryption)
6. [Key Rotation](#6-key-rotation)
7. [Multi-Recipient Messages](#7-multi-recipient-messages)

---

## 1. Group Messaging Architecture

### The Problem: Pairwise Sessions at Scale

When Alice wants to send a message to a group of N members, the naive approach would be to encrypt the message N times using pairwise Signal Protocol sessions—once for each recipient. For a group of 100 members, this means:

- 100 Double Ratchet operations
- 100 separate ciphertexts
- Significant computational overhead
- Large bandwidth consumption

This approach doesn't scale well. Consider a 500-person group where each member sends 10 messages per hour. That's 5,000 messages × 500 encryptions = 2,500,000 encryption operations per hour.

### The Solution: Sender Keys

The Signal Protocol implements an elegant solution called **Sender Keys**, which transforms the O(N) problem into an O(1) operation for the sender. Here's how it works:

**Key Insight**: Instead of encrypting the same message N times with different keys, encrypt it once with a shared symmetric key that only the sender advances.

```
Pairwise Sessions (Naive):
Alice → [Encrypt for Bob]    → Bob's ciphertext
Alice → [Encrypt for Carol]  → Carol's ciphertext
Alice → [Encrypt for Dave]   → Dave's ciphertext
... (N operations)

Sender Key (Efficient):
Alice → [Encrypt once] → Shared ciphertext → {Bob, Carol, Dave, ...}
                         (1 operation)
```

### Efficiency Comparison

| Operation | Pairwise Sessions | Sender Keys |
|-----------|------------------|-------------|
| Encryption ops | O(N) | O(1) |
| Ciphertext size | N × message_size | 1 × message_size |
| CPU time (100 recipients) | ~100ms | ~1ms |
| Bandwidth (1KB message, 100 recipients) | 100KB | 1KB |

### Security Trade-offs

Sender Keys make a deliberate security trade-off:

**What We Keep:**
- Forward secrecy: Each message uses a different key
- Authenticity: Messages are signed with the sender's private key
- Confidentiality: Only group members can decrypt

**What We Sacrifice:**
- Post-compromise security: If a sender key is compromised, past messages encrypted with old keys from the same chain remain secure, but the attacker can derive future keys until the sender key is rotated
- Deniability: Signature verification makes messages non-repudiable

**Why This Trade-off Makes Sense:**
For group messaging, the efficiency gains are worth the reduced post-compromise security. Groups typically have dozens to hundreds of members, and the probability that one member's device is compromised is relatively high. The protocol mitigates this through:
1. Regular key rotation when membership changes
2. Separate sender keys per distribution (group)
3. Forward secrecy within each sender key chain

---

## 2. Sender Key Structure

The sender key system consists of three primary data structures: `SenderMessageKey`, `SenderChainKey`, and `SenderKeyState`. Let's examine each in detail.

### SenderMessageKey: The Ephemeral Encryption Key

Each message encrypted with sender keys gets its own unique encryption key and IV, derived from a seed:

```rust
#[derive(Debug, Clone)]
pub(crate) struct SenderMessageKey {
    iteration: u32,
    iv: Vec<u8>,
    cipher_key: Vec<u8>,
    seed: Vec<u8>,
}

impl SenderMessageKey {
    pub(crate) fn new(iteration: u32, seed: Vec<u8>) -> Self {
        // Derive 48 bytes: 16 for IV, 32 for cipher key
        let mut derived = [0; 48];
        hkdf::Hkdf::<sha2::Sha256>::new(None, &seed)
            .expand(b"WhisperGroup", &mut derived)
            .expect("valid output length");

        Self {
            iteration,
            seed,
            iv: derived[0..16].to_vec(),        // AES-256-CBC IV
            cipher_key: derived[16..48].to_vec(), // AES-256-CBC key
        }
    }

    pub(crate) fn iteration(&self) -> u32 {
        self.iteration
    }

    pub(crate) fn iv(&self) -> &[u8] {
        &self.iv
    }

    pub(crate) fn cipher_key(&self) -> &[u8] {
        &self.cipher_key
    }
}
```

**Key Properties:**
- Each message key is bound to a specific iteration number
- The seed is used to deterministically derive both the IV and cipher key
- HKDF ensures the derived keys are cryptographically independent
- The info parameter `"WhisperGroup"` domain-separates this from other key derivations

### SenderChainKey: The Ratcheting Mechanism

The `SenderChainKey` is the heart of forward secrecy in sender keys. It ratchets forward with each message:

```rust
#[derive(Debug, Clone)]
pub(crate) struct SenderChainKey {
    iteration: u32,
    chain_key: Vec<u8>,
}

impl SenderChainKey {
    const MESSAGE_KEY_SEED: u8 = 0x01;
    const CHAIN_KEY_SEED: u8 = 0x02;

    pub(crate) fn new(iteration: u32, chain_key: Vec<u8>) -> Self {
        Self {
            iteration,
            chain_key,
        }
    }

    pub(crate) fn iteration(&self) -> u32 {
        self.iteration
    }

    pub(crate) fn seed(&self) -> &[u8] {
        &self.chain_key
    }

    // Advance the chain key to the next iteration
    pub(crate) fn next(&self) -> Result<SenderChainKey, SignalProtocolError> {
        let new_iteration = self.iteration.checked_add(1).ok_or_else(|| {
            SignalProtocolError::InvalidState(
                "sender_chain_key_next",
                "Sender chain is too long".into(),
            )
        })?;

        Ok(SenderChainKey::new(
            new_iteration,
            self.get_derivative(Self::CHAIN_KEY_SEED),
        ))
    }

    // Derive the message key for the current iteration
    pub(crate) fn sender_message_key(&self) -> SenderMessageKey {
        SenderMessageKey::new(
            self.iteration,
            self.get_derivative(Self::MESSAGE_KEY_SEED)
        )
    }

    fn get_derivative(&self, label: u8) -> Vec<u8> {
        let label = [label];
        hmac_sha256(&self.chain_key, &label).to_vec()
    }
}
```

**The Ratchet Process:**

```
Chain Key₀ ─┬─[HMAC(0x01)]→ Message Key Seed₀ ─[HKDF]→ (IV₀, CipherKey₀)
            │
            └─[HMAC(0x02)]→ Chain Key₁ ─┬─[HMAC(0x01)]→ Message Key Seed₁
                                         │
                                         └─[HMAC(0x02)]→ Chain Key₂ ...
```

**Security Properties:**
- **One-way function**: Given Chain Key_n, you cannot compute Chain Key_(n-1)
- **Forward secrecy**: Compromising Chain Key_n doesn't reveal previous message keys
- **Deterministic**: The same chain key always produces the same message key
- **Domain separation**: Labels 0x01 and 0x02 ensure message keys and chain keys never collide

### SenderKeyState: The Complete State

The `SenderKeyState` bundles everything needed to encrypt/decrypt messages:

```rust
#[derive(Debug, Clone)]
pub(crate) struct SenderKeyState {
    state: storage_proto::SenderKeyStateStructure,
}

impl SenderKeyState {
    pub(crate) fn new(
        message_version: u8,
        chain_id: u32,
        iteration: u32,
        chain_key: &[u8],
        signature_key: PublicKey,
        signature_private_key: Option<PrivateKey>,
    ) -> SenderKeyState {
        let state = storage_proto::SenderKeyStateStructure {
            message_version: message_version as u32,
            chain_id,
            sender_chain_key: Some(
                SenderChainKey::new(iteration, chain_key.to_vec()).as_protobuf(),
            ),
            sender_signing_key: Some(
                storage_proto::sender_key_state_structure::SenderSigningKey {
                    public: signature_key.serialize().to_vec(),
                    private: match signature_private_key {
                        None => vec![],  // Receivers don't store the private key
                        Some(k) => k.serialize().to_vec(),
                    },
                },
            ),
            sender_message_keys: vec![],  // For out-of-order messages
        };

        Self { state }
    }

    pub(crate) fn message_version(&self) -> u32 {
        match self.state.message_version {
            0 => 3, // the first SenderKey version
            v => v,
        }
    }

    pub(crate) fn chain_id(&self) -> u32 {
        self.state.chain_id
    }

    pub(crate) fn sender_chain_key(&self) -> Option<SenderChainKey> {
        let sender_chain = self.state.sender_chain_key.as_ref()?;
        Some(SenderChainKey::new(
            sender_chain.iteration,
            sender_chain.seed.clone(),
        ))
    }

    pub(crate) fn set_sender_chain_key(&mut self, chain_key: SenderChainKey) {
        self.state.sender_chain_key = Some(chain_key.as_protobuf());
    }

    // Store message keys for out-of-order delivery
    pub(crate) fn add_sender_message_key(&mut self, sender_message_key: &SenderMessageKey) {
        self.state
            .sender_message_keys
            .push(sender_message_key.as_protobuf());

        // Limit storage to prevent DoS attacks
        while self.state.sender_message_keys.len() > consts::MAX_MESSAGE_KEYS {
            self.state.sender_message_keys.remove(0);
        }
    }

    pub(crate) fn remove_sender_message_key(&mut self, iteration: u32) -> Option<SenderMessageKey> {
        if let Some(index) = self
            .state
            .sender_message_keys
            .iter()
            .position(|x| x.iteration == iteration)
        {
            let smk = self.state.sender_message_keys.remove(index);
            Some(SenderMessageKey::from_protobuf(smk))
        } else {
            None
        }
    }
}
```

**Chain ID**: Each sender key rotation gets a new random chain ID. This allows receivers to maintain multiple sender key states for the same sender, handling race conditions during key rotation.

**Message Key Storage**: The `sender_message_keys` vector caches message keys for out-of-order delivery. When a message arrives early, we ratchet forward, derive and cache intermediate message keys, then use them when older messages arrive.

---

## 3. Sender Key Distribution

Before Alice can send sender key encrypted messages, she must distribute her sender key to all group members. This is accomplished through a `SenderKeyDistributionMessage`.

### SenderKeyDistributionMessage Structure

```rust
#[derive(Debug, Clone)]
pub struct SenderKeyDistributionMessage {
    message_version: u8,
    distribution_id: Uuid,
    chain_id: u32,
    iteration: u32,
    chain_key: Vec<u8>,
    signing_key: PublicKey,
    serialized: Box<[u8]>,
}
```

**Components:**
- `distribution_id`: A UUID identifying this sender key distribution (typically the group ID)
- `chain_id`: A random 31-bit integer identifying this particular chain
- `iteration`: The current iteration number (usually 0 for new distributions)
- `chain_key`: The current chain key material
- `signing_key`: The public key used to verify message signatures

### Creating a Distribution Message

When Alice first joins a group or rotates her key, she creates a distribution message:

```rust
pub async fn create_sender_key_distribution_message<R: Rng + CryptoRng>(
    sender: &ProtocolAddress,
    distribution_id: Uuid,
    sender_key_store: &mut dyn SenderKeyStore,
    csprng: &mut R,
) -> Result<SenderKeyDistributionMessage> {
    let sender_key_record = sender_key_store
        .load_sender_key(sender, distribution_id)
        .await?;

    let sender_key_record = match sender_key_record {
        Some(record) => record,
        None => {
            // Create a new sender key from scratch
            // Use 31-bit chain IDs for Java compatibility
            let chain_id = (csprng.random::<u32>()) >> 1;
            log::info!(
                "Creating SenderKey for distribution {distribution_id} with chain ID {chain_id}"
            );

            let iteration = 0;
            let sender_key: [u8; 32] = csprng.random();  // Random chain key
            let signing_key = KeyPair::generate(csprng); // Random signing key

            let mut record = SenderKeyRecord::new_empty();
            record.add_sender_key_state(
                SENDERKEY_MESSAGE_CURRENT_VERSION,
                chain_id,
                iteration,
                &sender_key,
                signing_key.public_key,
                Some(signing_key.private_key),  // Sender stores private key
            );

            sender_key_store
                .store_sender_key(sender, distribution_id, &record)
                .await?;
            record
        }
    };

    let state = sender_key_record
        .sender_key_state()
        .map_err(|_| SignalProtocolError::InvalidSenderKeySession { distribution_id })?;

    let sender_chain_key = state
        .sender_chain_key()
        .ok_or(SignalProtocolError::InvalidSenderKeySession { distribution_id })?;

    let message_version = state
        .message_version()
        .try_into()
        .map_err(|_| SignalProtocolError::InvalidSenderKeySession { distribution_id })?;

    SenderKeyDistributionMessage::new(
        message_version,
        distribution_id,
        state.chain_id(),
        sender_chain_key.iteration(),
        sender_chain_key.seed().to_vec(),
        state
            .signing_key_public()
            .map_err(|_| SignalProtocolError::InvalidSenderKeySession { distribution_id })?,
    )
}
```

**Distribution Flow:**

```
1. Alice creates SKDM:
   - Generate random chain_key
   - Generate random signing key pair
   - Create SKDM with chain_id, iteration=0, chain_key, signing_public_key

2. Alice sends SKDM to Bob (encrypted with their pairwise session):
   Alice ─[Signal Protocol]→ SKDM_encrypted → Bob

3. Bob processes SKDM:
   - Decrypt using pairwise session
   - Store sender key state for Alice
   - Ready to receive sender key messages
```

### Processing a Distribution Message

When Bob receives Alice's distribution message, he processes it:

```rust
pub async fn process_sender_key_distribution_message(
    sender: &ProtocolAddress,
    skdm: &SenderKeyDistributionMessage,
    sender_key_store: &mut dyn SenderKeyStore,
) -> Result<()> {
    let distribution_id = skdm.distribution_id()?;
    log::info!(
        "{} Processing SenderKey distribution {} with chain ID {}",
        sender,
        distribution_id,
        skdm.chain_id()?
    );

    let mut sender_key_record = sender_key_store
        .load_sender_key(sender, distribution_id)
        .await?
        .unwrap_or_else(SenderKeyRecord::new_empty);

    // Add the new sender key state (or update existing)
    sender_key_record.add_sender_key_state(
        skdm.message_version(),
        skdm.chain_id()?,
        skdm.iteration()?,
        skdm.chain_key()?,
        *skdm.signing_key()?,
        None,  // Receivers don't get the private signing key
    );

    sender_key_store
        .store_sender_key(sender, distribution_id, &sender_key_record)
        .await?;

    Ok(())
}
```

**Storage Strategy**: The `SenderKeyRecord` can store up to `MAX_SENDER_KEY_STATES` (5) different states for the same sender and distribution. This handles key rotation race conditions—if Alice rotates her key but Bob receives messages encrypted with both old and new keys out of order, he can decrypt both.

---

## 4. Group Encryption

With the sender key distributed, Alice can now efficiently encrypt messages for the entire group.

### Encryption Process

```rust
pub async fn group_encrypt<R: Rng + CryptoRng>(
    sender_key_store: &mut dyn SenderKeyStore,
    sender: &ProtocolAddress,
    distribution_id: Uuid,
    plaintext: &[u8],
    csprng: &mut R,
) -> Result<SenderKeyMessage> {
    // 1. Load the sender key record
    let mut record = sender_key_store
        .load_sender_key(sender, distribution_id)
        .await?
        .ok_or(SignalProtocolError::NoSenderKeyState { distribution_id })?;

    let sender_key_state = record
        .sender_key_state_mut()
        .map_err(|_| SignalProtocolError::InvalidSenderKeySession { distribution_id })?;

    // 2. Get the current chain key
    let sender_chain_key = sender_key_state
        .sender_chain_key()
        .ok_or(SignalProtocolError::InvalidSenderKeySession { distribution_id })?;

    // 3. Derive the message key for this iteration
    let message_keys = sender_chain_key.sender_message_key();

    // 4. Encrypt the plaintext with AES-256-CBC
    let ciphertext = signal_crypto::aes_256_cbc_encrypt(
        plaintext,
        message_keys.cipher_key(),
        message_keys.iv()
    )
    .map_err(|_| {
        log::error!(
            "outgoing sender key state corrupt for distribution ID {distribution_id}",
        );
        SignalProtocolError::InvalidSenderKeySession { distribution_id }
    })?;

    // 5. Get the signing key to sign the message
    let signing_key = sender_key_state
        .signing_key_private()
        .map_err(|_| SignalProtocolError::InvalidSenderKeySession { distribution_id })?;

    // 6. Create the SenderKeyMessage with signature
    let message_version = sender_key_state
        .message_version()
        .try_into()
        .map_err(|_| SignalProtocolError::InvalidSenderKeySession { distribution_id })?;

    let skm = SenderKeyMessage::new(
        message_version,
        distribution_id,
        sender_key_state.chain_id(),
        message_keys.iteration(),
        ciphertext.into_boxed_slice(),
        csprng,
        &signing_key,
    )?;

    // 7. Ratchet the chain key forward
    sender_key_state.set_sender_chain_key(sender_chain_key.next()?);

    // 8. Save the updated state
    sender_key_store
        .store_sender_key(sender, distribution_id, &record)
        .await?;

    Ok(skm)
}
```

### Message Format

A `SenderKeyMessage` contains:
- **Version**: Protocol version (currently 3)
- **Distribution ID**: Which group this message is for
- **Chain ID**: Which sender key chain
- **Iteration**: Which message number in the chain
- **Ciphertext**: AES-256-CBC encrypted payload
- **Signature**: Ed25519 signature over the message

### Example from Tests

```rust
#[test]
fn group_basic_encrypt_decrypt() -> Result<(), SignalProtocolError> {
    async {
        let mut csprng = OsRng.unwrap_err();
        let sender_address = ProtocolAddress::new(
            "+14159999111".to_owned(),
            DeviceId::new(1).unwrap()
        );
        let distribution_id = Uuid::from_u128(0xd1d1d1d1_7000_11eb_b32a_33b8a8a487a6);

        let mut alice_store = test_in_memory_protocol_store()?;
        let mut bob_store = test_in_memory_protocol_store()?;

        // Alice creates and distributes her sender key
        let sent_distribution_message = create_sender_key_distribution_message(
            &sender_address,
            distribution_id,
            &mut alice_store,
            &mut csprng,
        )
        .await?;

        // Bob receives the distribution message
        let recv_distribution_message =
            SenderKeyDistributionMessage::try_from(sent_distribution_message.serialized())?;

        // Alice encrypts a message
        let alice_ciphertext = group_encrypt(
            &mut alice_store,
            &sender_address,
            distribution_id,
            "space camp?".as_bytes(),
            &mut csprng,
        )
        .await?;

        // Bob processes the distribution and decrypts
        process_sender_key_distribution_message(
            &sender_address,
            &recv_distribution_message,
            &mut bob_store,
        )
        .await?;

        let bob_plaintext = group_decrypt(
            alice_ciphertext.serialized(),
            &mut bob_store,
            &sender_address,
        )
        .await?;

        assert_eq!(
            String::from_utf8(bob_plaintext).expect("valid utf8"),
            "space camp?"
        );

        Ok(())
    }
    .now_or_never()
    .expect("sync")
}
```

---

## 5. Group Decryption

Decryption handles several complex scenarios: in-order messages, out-of-order messages, and duplicate messages.

### Decryption Process

```rust
pub async fn group_decrypt(
    skm_bytes: &[u8],
    sender_key_store: &mut dyn SenderKeyStore,
    sender: &ProtocolAddress,
) -> Result<Vec<u8>> {
    // 1. Parse the SenderKeyMessage
    let skm = SenderKeyMessage::try_from(skm_bytes)?;

    let distribution_id = skm.distribution_id();
    let chain_id = skm.chain_id();

    // 2. Load the sender key record
    let mut record = sender_key_store
        .load_sender_key(sender, skm.distribution_id())
        .await?
        .ok_or(SignalProtocolError::NoSenderKeyState { distribution_id })?;

    // 3. Find the state for this chain ID
    let sender_key_state = match record.sender_key_state_for_chain_id(chain_id) {
        Some(state) => state,
        None => {
            log::error!(
                "SenderKey distribution {} could not find chain ID {} (known chain IDs: {:?})",
                distribution_id,
                chain_id,
                record.chain_ids_for_logging().collect::<Vec<_>>(),
            );
            return Err(SignalProtocolError::NoSenderKeyState { distribution_id });
        }
    };

    // 4. Verify message version
    let message_version = skm.message_version() as u32;
    if message_version != sender_key_state.message_version() {
        return Err(SignalProtocolError::UnrecognizedMessageVersion(
            message_version,
        ));
    }

    // 5. Verify the signature
    let signing_key = sender_key_state
        .signing_key_public()
        .map_err(|_| SignalProtocolError::InvalidSenderKeySession { distribution_id })?;

    if !skm.verify_signature(&signing_key)? {
        return Err(SignalProtocolError::SignatureValidationFailed);
    }

    // 6. Get the message key (handles out-of-order delivery)
    let sender_key = get_sender_key(sender_key_state, skm.iteration(), distribution_id)?;

    // 7. Decrypt the ciphertext
    let plaintext = match signal_crypto::aes_256_cbc_decrypt(
        skm.ciphertext(),
        sender_key.cipher_key(),
        sender_key.iv(),
    ) {
        Ok(plaintext) => plaintext,
        Err(signal_crypto::DecryptionError::BadKeyOrIv) => {
            log::error!(
                "incoming sender key state corrupt for {sender}, distribution ID {distribution_id}",
            );
            return Err(SignalProtocolError::InvalidSenderKeySession { distribution_id });
        }
        Err(signal_crypto::DecryptionError::BadCiphertext(msg)) => {
            log::error!("sender key decryption failed: {msg}");
            return Err(SignalProtocolError::InvalidMessage(
                CiphertextMessageType::SenderKey,
                "decryption failed",
            ));
        }
    };

    // 8. Save the updated state (with cached message keys)
    sender_key_store
        .store_sender_key(sender, distribution_id, &record)
        .await?;

    Ok(plaintext)
}
```

### Handling Out-of-Order Messages

The `get_sender_key` function is where the magic happens for out-of-order delivery:

```rust
fn get_sender_key(
    state: &mut SenderKeyState,
    iteration: u32,
    distribution_id: Uuid,
) -> Result<SenderMessageKey> {
    let sender_chain_key = state
        .sender_chain_key()
        .ok_or(SignalProtocolError::InvalidSenderKeySession { distribution_id })?;
    let current_iteration = sender_chain_key.iteration();

    // Case 1: Message from the past
    if current_iteration > iteration {
        // Try to retrieve from cache
        if let Some(smk) = state.remove_sender_message_key(iteration) {
            return Ok(smk);
        } else {
            // Duplicate message
            log::info!(
                "SenderKey distribution {distribution_id} Duplicate message for iteration: {iteration}"
            );
            return Err(SignalProtocolError::DuplicatedMessage(
                current_iteration,
                iteration,
            ));
        }
    }

    // Case 2: Message too far in the future
    let jump = (iteration - current_iteration) as usize;
    if jump > consts::MAX_FORWARD_JUMPS {
        log::error!(
            "SenderKey distribution {} Exceeded future message limit: {}, current iteration: {})",
            distribution_id,
            consts::MAX_FORWARD_JUMPS,
            current_iteration
        );
        return Err(SignalProtocolError::InvalidMessage(
            CiphertextMessageType::SenderKey,
            "message from too far into the future",
        ));
    }

    // Case 3: Message from the future (but within limits)
    let mut sender_chain_key = sender_chain_key;

    // Ratchet forward, caching intermediate message keys
    while sender_chain_key.iteration() < iteration {
        state.add_sender_message_key(&sender_chain_key.sender_message_key());
        sender_chain_key = sender_chain_key.next()?;
    }

    // Ratchet one more time and save the new chain key
    state.set_sender_chain_key(sender_chain_key.next()?);

    // Return the message key for the requested iteration
    Ok(sender_chain_key.sender_message_key())
}
```

**Out-of-Order Scenario:**

```
Alice sends:     Msg₀  Msg₁  Msg₂  Msg₃  Msg₄
Bob receives:    Msg₀  Msg₃  Msg₁  Msg₂  Msg₄

On Msg₀: Bob's chain key is at iteration 0
  - Decrypt with key₀
  - Ratchet to iteration 1

On Msg₃: Bob's chain key is at iteration 1, but message is iteration 3
  - Ratchet from 1 → 2, cache key₁
  - Ratchet from 2 → 3, cache key₂
  - Use key₃ to decrypt
  - Ratchet to iteration 4

On Msg₁: Bob's chain key is at iteration 4, but message is iteration 1
  - Retrieve key₁ from cache
  - Decrypt successfully

On Msg₂:
  - Retrieve key₂ from cache
  - Decrypt successfully

On Msg₄:
  - Current iteration is 4, message is 4
  - Use current chain key
```

### Example: Out-of-Order Decryption Test

```rust
#[test]
fn group_out_of_order() -> Result<(), SignalProtocolError> {
    async {
        let mut csprng = OsRng.unwrap_err();
        let sender_address = ProtocolAddress::new(
            "+14159999111".to_owned(),
            DeviceId::new(1).unwrap()
        );
        let distribution_id = Uuid::from_u128(0xd1d1d1d1_7000_11eb_b32a_33b8a8a487a6);

        let mut alice_store = test_in_memory_protocol_store()?;
        let mut bob_store = test_in_memory_protocol_store()?;

        // Setup
        let sent_distribution_message = create_sender_key_distribution_message(
            &sender_address,
            distribution_id,
            &mut alice_store,
            &mut csprng,
        )
        .await?;

        let recv_distribution_message =
            SenderKeyDistributionMessage::try_from(sent_distribution_message.serialized())?;

        process_sender_key_distribution_message(
            &sender_address,
            &recv_distribution_message,
            &mut bob_store,
        )
        .await?;

        // Alice encrypts 100 messages
        let mut ciphertexts = Vec::with_capacity(100);
        for i in 0..ciphertexts.capacity() {
            ciphertexts.push(
                group_encrypt(
                    &mut alice_store,
                    &sender_address,
                    distribution_id,
                    format!("nefarious plotting {i:02}/100").as_bytes(),
                    &mut csprng,
                )
                .await?,
            );
        }

        // Shuffle the ciphertexts (out-of-order delivery)
        ciphertexts.shuffle(&mut csprng);

        // Bob decrypts all messages despite disorder
        let mut plaintexts = Vec::with_capacity(ciphertexts.len());
        for ciphertext in ciphertexts {
            plaintexts.push(
                group_decrypt(ciphertext.serialized(), &mut bob_store, &sender_address).await?,
            );
        }

        // Verify all messages decrypted correctly
        plaintexts.sort();
        for (i, plaintext) in plaintexts.iter().enumerate() {
            assert_eq!(
                String::from_utf8(plaintext.to_vec()).expect("valid utf8"),
                format!("nefarious plotting {i:02}/100")
            );
        }

        Ok(())
    }
    .now_or_never()
    .expect("sync")
}
```

---

## 6. Key Rotation

Sender keys must be rotated when group membership changes to maintain forward secrecy and prevent removed members from reading new messages.

### When to Rotate

**Mandatory Rotation:**
- A member leaves the group
- A member is removed
- A member's device is compromised (if detected)

**Optional Rotation:**
- Periodically (e.g., every N messages)
- After a time period
- On security policy changes

### How Rotation Works

Rotation is simply creating a new sender key distribution message:

```rust
// Alice rotates her sender key
let new_distribution_message = create_sender_key_distribution_message(
    &alice_address,
    distribution_id,  // Same distribution (group)
    &mut alice_store,
    &mut csprng,
)
.await?;

// This creates a NEW chain with:
// - New random chain_id
// - New random chain_key
// - New random signing key pair
// - iteration = 0
```

### Rotation Scenario

```
Initial State:
  Alice has chain_id=100, iteration=50
  Bob has Alice's chain_id=100 at iteration=50

Charlie leaves the group:

1. Alice creates new sender key:
   - chain_id=200 (new random)
   - iteration=0
   - new chain_key
   - new signing_key

2. Alice distributes to Bob (but not Charlie)

3. Alice sends new messages with chain_id=200

4. Bob receives:
   - Sees chain_id=200 (not 100)
   - Looks for state with chain_id=200
   - Finds it, decrypts successfully

5. Charlie receives message with chain_id=200:
   - Looks for state with chain_id=200
   - Not found → Cannot decrypt
```

### Handling Rotation Race Conditions

The `SenderKeyRecord` stores multiple states to handle race conditions:

```rust
#[derive(Debug, Clone)]
pub struct SenderKeyRecord {
    states: VecDeque<SenderKeyState>,  // Up to MAX_SENDER_KEY_STATES (5)
}
```

**Race Condition Example:**

```
Timeline:
  t₀: Alice sends Msg_A with chain_id=100
  t₁: Alice rotates → chain_id=200
  t₂: Alice sends Msg_B with chain_id=200
  t₃: Bob receives Msg_B (chain_id=200)
  t₄: Bob receives Msg_A (chain_id=100) ← Out of order!

If Bob only stored one chain:
  - At t₃, Bob would replace chain_id=100 with chain_id=200
  - At t₄, Bob couldn't decrypt Msg_A

With multiple chain storage:
  - At t₃, Bob stores chain_id=200 in addition to chain_id=100
  - At t₄, Bob finds chain_id=100 state and decrypts successfully
```

---

## 7. Multi-Recipient Messages

The ultimate efficiency optimization: combine sender keys with sealed sender to send one message to many recipients with different devices.

### The Problem: Server-Side Fanout

Even with sender keys, the sender must:
1. Encrypt the message once with sender key
2. Encrypt that ciphertext N times with sealed sender (once per recipient)
3. Send N separate packages to the server

For a 100-person group, that's still 100 sealed sender operations and 100 separate transmissions.

### The Solution: Multi-Recipient Sealed Sender (Sealed Sender v2)

Sealed Sender v2 allows encrypting a message once and including per-recipient headers in a single transmission:

```
Traditional Sealed Sender:
  Alice → [Encrypt for Bob]   → 1KB ciphertext → Server
  Alice → [Encrypt for Carol] → 1KB ciphertext → Server
  Alice → [Encrypt for Dave]  → 1KB ciphertext → Server

  Total: 3KB upload, 3 encryption operations

Multi-Recipient Sealed Sender:
  Alice → [Encrypt once] → {
            Shared: 1KB ciphertext (encrypted symmetrically)
            Bob:   48 bytes (header)
            Carol: 48 bytes (header)
            Dave:  48 bytes (header)
          } → Server

  Total: 1.14KB upload, 1 symmetric encryption + 3 key agreements
```

### Algorithmic Overview

```rust
pub async fn sealed_sender_multi_recipient_encrypt<R: Rng + CryptoRng>(
    destinations: &[&ProtocolAddress],
    destination_sessions: &[&SessionRecord],
    excluded_recipients: impl IntoIterator<Item = ServiceId>,
    usmc: &UnidentifiedSenderMessageContent,  // Contains the SenderKeyMessage
    identity_store: &dyn IdentityKeyStore,
    rng: &mut R,
) -> Result<Vec<u8>>
```

**High-level steps:**

```
1. Generate random M (32 bytes)

2. Derive ephemeral key pair E from M:
   r = KDF("r", M)
   E = DeriveKeyPair(r)

3. Derive symmetric key K from M:
   K = KDF("K", M)

4. For each recipient R_i:
   a. Compute shared secret: DH(E, R_i)
   b. Encrypt M: C_i = KDF(DH(E, R_i)) ⊕ M
   c. Compute auth tag: AT_i = KDF(DH(Sender, R_i) || E.public || C_i)

5. Symmetrically encrypt the payload (ONLY ONCE):
   ciphertext = AES-GCM-SIV(K, usmc.serialize())

6. Output:
   {
     E.public,              // 32 bytes
     [(C_i, AT_i, devices), ...],  // 48+ bytes per recipient
     ciphertext             // Payload size + 16 bytes (auth tag)
   }
```

### Wire Format

```
SentMessage {
    version_byte: u8 = 0x22,
    recipient_count: varint,

    // Per-recipient data
    recipients: [
        {
            service_id: [u8; 17],     // Fixed-width ServiceID
            devices: [
                {
                    device_id: u8,
                    registration_id: u14,
                    has_more: bool,
                },
                ...
            ],
            c: [u8; 32],              // Encrypted M
            at: [u8; 16],             // Auth tag
        },
        ...
    ],

    e_pub: [u8; 32],                  // Ephemeral public key
    ciphertext: [u8],                 // AES-GCM-SIV(K, message)
}
```

### Example: Group Message with Sealed Sender v2

From the test suite:

```rust
#[test]
fn group_sealed_sender() -> Result<(), SignalProtocolError> {
    async {
        let mut csprng = OsRng.unwrap_err();

        // Setup: Alice, Bob, and Carol
        let alice_uuid_address = ProtocolAddress::new(alice_uuid.clone(), alice_device_id);
        let bob_uuid_address = ProtocolAddress::new(bob_uuid.clone(), bob_device_id);
        let carol_uuid_address = ProtocolAddress::new(carol_uuid.clone(), carol_device_id);

        let distribution_id = Uuid::from_u128(0xd1d1d1d1_7000_11eb_b32a_33b8a8a487a6);

        // Alice establishes sessions with Bob and Carol
        process_prekey_bundle(&bob_uuid_address, ...).await?;
        process_prekey_bundle(&carol_uuid_address, ...).await?;

        // Alice distributes her sender key
        let sent_distribution_message = create_sender_key_distribution_message(
            &alice_uuid_address,
            distribution_id,
            &mut alice_store,
            &mut csprng,
        )
        .await?;

        // Bob and Carol process the distribution
        process_sender_key_distribution_message(
            &alice_uuid_address,
            &recv_distribution_message,
            &mut bob_store,
        ).await?;
        process_sender_key_distribution_message(
            &alice_uuid_address,
            &recv_distribution_message,
            &mut carol_store,
        ).await?;

        // Alice encrypts with sender key
        let alice_message = group_encrypt(
            &mut alice_store,
            &alice_uuid_address,
            distribution_id,
            "space camp?".as_bytes(),
            &mut csprng,
        )
        .await?;

        // Alice wraps in UnidentifiedSenderMessageContent
        let alice_usmc = UnidentifiedSenderMessageContent::new(
            CiphertextMessageType::SenderKey,
            sender_cert.clone(),
            alice_message.serialized().to_vec(),
            ContentHint::Implicit,
            Some([42].to_vec()),  // Group ID
        )?;

        // Alice creates multi-recipient sealed sender message
        let recipients = [&bob_uuid_address, &carol_uuid_address];
        let alice_ctext = sealed_sender_multi_recipient_encrypt(
            &recipients,
            &alice_store.session_store.load_existing_sessions(&recipients)?,
            [],  // No excluded recipients
            &alice_usmc,
            &alice_store.identity_store,
            &mut csprng,
        )
        .await?;

        // Parse to verify structure
        let alice_ctext_parsed = SealedSenderV2SentMessage::parse(&alice_ctext)?;
        assert_eq!(alice_ctext_parsed.recipients.len(), 2);

        // Extract Bob's portion
        let bob_ctext = alice_ctext_parsed
            .received_message_parts_for_recipient(&alice_ctext_parsed.recipients[0])
            .as_ref()
            .concat();

        // Bob decrypts the sealed sender layer
        let bob_usmc = sealed_sender_decrypt_to_usmc(
            &bob_ctext,
            &bob_store.identity_store
        ).await?;

        // Bob extracts and decrypts the sender key message
        let bob_plaintext = group_decrypt(
            bob_usmc.contents()?,
            &mut bob_store,
            &alice_uuid_address
        ).await?;

        assert_eq!(
            String::from_utf8(bob_plaintext).expect("valid utf8"),
            "space camp?"
        );

        // Carol does the same
        let carol_ctext = alice_ctext_parsed
            .received_message_parts_for_recipient(&alice_ctext_parsed.recipients[1])
            .as_ref()
            .concat();

        let carol_usmc = sealed_sender_decrypt_to_usmc(
            &carol_ctext,
            &carol_store.identity_store
        ).await?;

        let carol_plaintext = group_decrypt(
            carol_usmc.contents()?,
            &mut carol_store,
            &alice_uuid_address,
        )
        .await?;

        assert_eq!(
            String::from_utf8(carol_plaintext).expect("valid utf8"),
            "space camp?"
        );

        Ok(())
    }
    .now_or_never()
    .expect("sync")
}
```

### Performance Characteristics

For a group of N members with M total devices:

| Metric | Traditional | Sender Key | Multi-Recipient SS |
|--------|-------------|------------|-------------------|
| Sender encryptions | M × (Double Ratchet) | 1 × (Symmetric) | 1 × (Symmetric) + N × (Key Agreement) |
| Bandwidth | M × payload_size | 1 × payload_size | 1 × payload_size + M × 48 bytes |
| CPU time (100 members) | ~1000ms | ~1ms | ~10ms |
| Upload (1KB message, 100 members) | 100KB | 1KB × 100 transmissions | ~6KB (single transmission) |

**Example Calculation** (1KB message, 100 members, 150 devices):
- Traditional: 150KB upload, 150 transmissions
- Sender Key only: 150KB upload, 150 transmissions (each is 1KB)
- Multi-recipient SS: 1KB + (150 × 48 bytes) = ~8.2KB upload, 1 transmission

**Savings: 94% bandwidth reduction, 99% fewer transmissions**

---

## Summary

Signal's group messaging implementation demonstrates elegant engineering:

1. **Sender Keys** transform O(N) encryption into O(1), enabling efficient group messaging
2. **Chain key ratcheting** provides forward secrecy within each sender key
3. **Out-of-order handling** gracefully manages network realities
4. **Key rotation** maintains security when membership changes
5. **Multi-recipient sealed sender** optimizes the final mile with 90%+ bandwidth savings

The trade-offs are well-considered: reduced post-compromise security in exchange for practicality at scale, with mitigations like key rotation and chain separation.

This is literate programming at its finest—code that tells a story of security, efficiency, and real-world pragmatism.

---

*Chapter 9: Group Messaging - End*
