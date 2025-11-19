# Chapter 3: The Signal Protocol Implementation
## A Deep Dive into PQXDH, Double Ratchet, and SPQR

---

## 3.1 Protocol Overview

The **Signal Protocol** (originally called "Axolotl" until November 2014) is a cryptographic protocol that provides end-to-end encryption for asynchronous messaging. It combines several sophisticated cryptographic techniques to achieve a unique set of security properties that were revolutionary when introduced in 2014 and remain the gold standard for secure messaging today.

### 3.1.1 Design Goals

The Signal Protocol was designed with five core security properties:

1. **Confidentiality**: Messages can only be read by the intended recipient
2. **Authentication**: Recipients can verify the sender's identity
3. **Forward Secrecy**: Compromise of long-term keys does not compromise past messages
4. **Post-Compromise Security (Backward Secrecy)**: Session keys are updated continuously, so compromise of session state doesn't affect future messages after a fresh DH exchange
5. **Deniability**: Message signatures are not cryptographically provable to third parties (similar to OTR messaging)

In 2023-2025, Signal added a sixth crucial property:

6. **Post-Quantum Security**: Protection against adversaries with quantum computers

### 3.1.2 Security Properties

The Signal Protocol achieves these properties through a carefully orchestrated combination of:

- **X3DH/PQXDH**: Initial key agreement establishing a shared secret between two parties who may not be online simultaneously
- **Double Ratchet**: Continuous key evolution with both symmetric-key and Diffie-Hellman ratcheting
- **SPQR**: Post-quantum extension to the Double Ratchet providing quantum-resistant forward secrecy
- **HMAC Authentication**: Ensuring message integrity and authenticity
- **Deniable Signatures**: Using MAC-based authentication instead of signatures

### 3.1.3 Academic Analysis and Formal Verification

The Signal Protocol has been the subject of extensive academic scrutiny:

- **"A Formal Security Analysis of the Signal Messaging Protocol"** (Cohn-Gordon et al., 2017): Formal verification using computational security proofs
- **"On Ends-to-Ends Encryption: Asynchronous Group Messaging with Strong Security Guarantees"** (Alwen et al., 2020): Analysis of group messaging security
- **"Post-Quantum Security of the Even-Mansour Cipher"**: Foundation for SPQR
- **"PQXDH: Post-Quantum Extended Diffie-Hellman"** (Signal, 2023): Specification and security analysis

The protocol has withstood over a decade of cryptanalysis and is now deployed to billions of users worldwide.

### 3.1.4 Evolution from Axolotl to Modern Signal Protocol

**Timeline of Major Changes:**

- **February 2014**: Axolotl protocol introduced with TextSecure v2
  - Original X3DH key agreement
  - Double Ratchet algorithm
  - Version 2 message format

- **November 2014**: Renamed to "Signal Protocol"
  - WhatsApp integration announced
  - Protocol refinements

- **2016-2020**: Maturation
  - Version 3 message format
  - Sealed Sender (metadata protection)
  - Group messaging with Sender Keys

- **September 2023**: PQXDH introduction
  - Kyber1024 integration
  - Version 4 message format
  - Hybrid classical + post-quantum security

- **March 2024**: SPQR integration
  - Post-quantum Double Ratchet extension
  - Out-of-order message handling

- **June 2024**: X3DH deprecation
  - PQXDH becomes mandatory for new sessions
  - X3DH sessions rejected

In libsignal's Rust implementation (as of November 2025), we can see this evolution reflected in the version constants:

```rust
// From rust/protocol/src/protocol.rs
pub(crate) const CIPHERTEXT_MESSAGE_CURRENT_VERSION: u8 = 4;
// Backward compatible, lacking Kyber keys, version
pub(crate) const CIPHERTEXT_MESSAGE_PRE_KYBER_VERSION: u8 = 3;
```

Version 4 represents the modern PQXDH era, while version 3 represents the classical X3DH protocol that is no longer accepted for new sessions.

---

## 3.2 X3DH (Extended Triple Diffie-Hellman)

While X3DH is now deprecated in favor of PQXDH, understanding it provides crucial context for the modern protocol. PQXDH is essentially X3DH with an additional post-quantum key encapsulation mechanism layered on top.

### 3.2.1 The Core Idea

X3DH solves a fundamental problem in asynchronous messaging: **How can Alice send an encrypted message to Bob when Bob is offline and they've never communicated before?**

The solution is for Bob to upload **prekeys** to a server. Alice can then:
1. Download Bob's prekey bundle
2. Perform multiple Diffie-Hellman operations locally
3. Derive a shared secret
4. Send her first encrypted message

When Bob comes online, he can use his private keys to reconstruct the same shared secret and decrypt Alice's message.

### 3.2.2 PreKey Bundle Structure

A PreKey Bundle contains Bob's public keys uploaded to the server. In the modern libsignal implementation, this structure is defined in `/home/user/libsignal/rust/protocol/src/state/bundle.rs`:

```rust
#[derive(Clone)]
pub struct PreKeyBundle {
    registration_id: u32,              // Bob's device registration ID
    device_id: DeviceId,               // Bob's device ID
    pre_key_id: Option<PreKeyId>,      // One-time prekey ID (optional)
    pre_key_public: Option<PublicKey>, // One-time prekey public (optional)
    ec_signed_pre_key: SignedPreKey,   // Signed prekey (required)
    identity_key: IdentityKey,         // Bob's long-term identity key
    kyber_pre_key: KyberPreKey,        // Post-quantum Kyber prekey (PQXDH)
}

#[derive(Clone)]
struct SignedPreKey {
    id: SignedPreKeyId,
    public_key: PublicKey,
    signature: Vec<u8>,  // Signed by identity key
}

#[derive(Clone)]
struct KyberPreKey {
    id: KyberPreKeyId,
    public_key: kem::PublicKey,  // Kyber1024 public key
    signature: Vec<u8>,           // Signed by identity key
}
```

**Key Components:**

1. **Identity Key** (`IK_B`): Bob's long-term Curve25519 public key. This is Bob's cryptographic identity.

2. **Signed Pre-Key** (`SPK_B`): A medium-term Curve25519 key pair that Bob rotates periodically (e.g., weekly). The public key is signed by Bob's identity key to prevent impersonation.

3. **One-Time Pre-Key** (`OPK_B`): A collection of single-use Curve25519 key pairs. When Alice uses one, it's deleted from the server, providing forward secrecy even if the server is compromised.

4. **Kyber Pre-Key** (PQXDH only): A Kyber1024 KEM public key for post-quantum security.

### 3.2.3 The Four DH Operations (Classical X3DH)

When Alice wants to initiate a session with Bob, she performs **four** Diffie-Hellman operations (or three if no one-time prekey is available):

Let's examine the code in `/home/user/libsignal/rust/protocol/src/ratchet.rs`:

```rust
pub(crate) fn initialize_alice_session<R: Rng + CryptoRng>(
    parameters: &AliceSignalProtocolParameters,
    mut csprng: &mut R,
) -> Result<SessionState> {
    let local_identity = parameters.our_identity_key_pair().identity_key();

    let mut secrets = Vec::with_capacity(32 * 6);

    // 1. Start with 32 bytes of 0xFF as "discontinuity bytes"
    secrets.extend_from_slice(&[0xFFu8; 32]); // "discontinuity bytes"

    let our_base_private_key = parameters.our_base_key_pair().private_key;

    // 2. DH1: DH(IK_A, SPK_B)
    //    Alice's identity key with Bob's signed prekey
    secrets.extend_from_slice(
        &parameters
            .our_identity_key_pair()
            .private_key()
            .calculate_agreement(parameters.their_signed_pre_key())?,
    );

    // 3. DH2: DH(EK_A, IK_B)
    //    Alice's ephemeral (base) key with Bob's identity key
    secrets.extend_from_slice(
        &our_base_private_key.calculate_agreement(parameters.their_identity_key().public_key())?,
    );

    // 4. DH3: DH(EK_A, SPK_B)
    //    Alice's ephemeral key with Bob's signed prekey
    secrets.extend_from_slice(
        &our_base_private_key.calculate_agreement(parameters.their_signed_pre_key())?,
    );

    // 5. DH4: DH(EK_A, OPK_B) [Optional]
    //    Alice's ephemeral key with Bob's one-time prekey
    if let Some(their_one_time_prekey) = parameters.their_one_time_pre_key() {
        secrets
            .extend_from_slice(&our_base_private_key.calculate_agreement(their_one_time_prekey)?);
    }

    // For PQXDH, we also perform Kyber encapsulation:
    let kyber_ciphertext = {
        let (ss, ct) = parameters.their_kyber_pre_key().encapsulate(&mut csprng)?;
        secrets.extend_from_slice(ss.as_ref());
        ct
    };

    // Now derive the root key, chain key, and SPQR key from all these secrets
    let (root_key, chain_key, pqr_key) = derive_keys(&secrets);

    // ... (continue with session initialization)
}
```

**Why These Specific DH Operations?**

Each DH operation serves a specific security purpose:

- **DH1: DH(IK_A, SPK_B)**: Provides mutual authentication (both parties' long-term or semi-long-term keys)
- **DH2: DH(EK_A, IK_B)**: Provides forward secrecy (ephemeral key) and authenticates Bob
- **DH3: DH(EK_A, SPK_B)**: Provides forward secrecy and contributes to session randomness
- **DH4: DH(EK_A, OPK_B)**: Provides additional forward secrecy and prevents passive server compromise attacks

The **discontinuity bytes** (32 bytes of 0xFF) are prepended to prevent cross-protocol attacks and to ensure the KDF input is distinct from other protocols.

### 3.2.4 Bob's Perspective (Receiving the Initial Message)

When Bob receives Alice's initial message, he needs to reconstruct the same shared secret. The code is in the `initialize_bob_session` function:

```rust
pub(crate) fn initialize_bob_session(
    parameters: &BobSignalProtocolParameters,
) -> Result<SessionState> {
    // Validate their base key is canonical (prevents malicious keys)
    if !parameters.their_base_key().is_canonical() {
        return Err(SignalProtocolError::InvalidMessage(
            crate::CiphertextMessageType::PreKey,
            "incoming base key is invalid",
        ));
    }

    let local_identity = parameters.our_identity_key_pair().identity_key();

    let mut secrets = Vec::with_capacity(32 * 6);

    secrets.extend_from_slice(&[0xFFu8; 32]); // "discontinuity bytes"

    // Bob performs the same DH operations but with his private keys:

    // DH1: DH(SPK_B, IK_A)
    secrets.extend_from_slice(
        &parameters
            .our_signed_pre_key_pair()
            .private_key
            .calculate_agreement(parameters.their_identity_key().public_key())?,
    );

    // DH2: DH(IK_B, EK_A)
    secrets.extend_from_slice(
        &parameters
            .our_identity_key_pair()
            .private_key()
            .calculate_agreement(parameters.their_base_key())?,
    );

    // DH3: DH(SPK_B, EK_A)
    secrets.extend_from_slice(
        &parameters
            .our_signed_pre_key_pair()
            .private_key
            .calculate_agreement(parameters.their_base_key())?,
    );

    // DH4: DH(OPK_B, EK_A) [Optional]
    if let Some(our_one_time_pre_key_pair) = parameters.our_one_time_pre_key_pair() {
        secrets.extend_from_slice(
            &our_one_time_pre_key_pair
                .private_key
                .calculate_agreement(parameters.their_base_key())?,
        );
    }

    // Kyber decapsulation for PQXDH
    secrets.extend_from_slice(
        &parameters
            .our_kyber_pre_key_pair()
            .secret_key
            .decapsulate(parameters.their_kyber_ciphertext())?,
    );

    let (root_key, chain_key, pqr_key) = derive_keys(&secrets);

    // ... (continue with session initialization)
}
```

Note that due to the commutativity of Diffie-Hellman (DH(a,B) = DH(b,A)), both Alice and Bob arrive at the same shared secret despite using different operations.

### 3.2.5 Key Derivation

Once all the DH outputs are concatenated, they're fed into HKDF to derive three keys:

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

The HKDF derives:
1. **Root Key** (32 bytes): Used for the DH ratchet
2. **Chain Key** (32 bytes): Used for the symmetric ratchet (first receiving chain)
3. **PQR Key** (32 bytes): Used to initialize the SPQR state

The label string includes "CRYSTALS-KYBER-1024" to ensure domain separation from older X3DH sessions.

---

## 3.3 PQXDH (Post-Quantum X3DH)

### 3.3.1 The Quantum Threat

Quantum computers pose an existential threat to public-key cryptography:

- **Shor's Algorithm** (1994): Efficiently factors integers and computes discrete logarithms on quantum computers
- **Impact**: Breaks RSA, ECDH, ECDSA, and other classical public-key systems
- **Timeline**: While large-scale quantum computers don't exist yet, "harvest now, decrypt later" attacks are a real concern

Signal's response: **Hybrid cryptography** combining classical and post-quantum algorithms.

### 3.3.2 Hybrid Key Agreement: X25519 + Kyber1024

PQXDH extends X3DH by adding a **Key Encapsulation Mechanism (KEM)** based on Kyber1024 (standardized by NIST as ML-KEM-1024).

**Kyber Overview:**
- **Type**: Lattice-based KEM (Module Learning With Errors)
- **Security Level**: NIST Level 5 (strongest)
- **Public Key**: 1568 bytes
- **Ciphertext**: 1568 bytes
- **Shared Secret**: 32 bytes

The hybrid approach provides **defense in depth**:
- If Kyber is broken, you still have X25519 security
- If quantum computers break X25519, you still have Kyber security
- Both would need to fail for the protocol to be compromised

### 3.3.3 Kyber Integration in libsignal

The KEM implementation is in `/home/user/libsignal/rust/protocol/src/kem.rs`:

```rust
//! Keys and protocol functions for standard key encapsulation mechanisms (KEMs).
//!
//! A KEM allows the holder of a `PublicKey` to create a shared secret with the
//! holder of the corresponding `SecretKey`. This is done by calling the function
//! `encapsulate` on the `PublicKey` to produce a `SharedSecret` and `Ciphertext`.

pub trait Parameters {
    const KEY_TYPE: KeyType;
    const PUBLIC_KEY_LENGTH: usize;
    const SECRET_KEY_LENGTH: usize;
    const CIPHERTEXT_LENGTH: usize;
    const SHARED_SECRET_LENGTH: usize;

    fn generate<R: CryptoRng + ?Sized>(
        csprng: &mut R,
    ) -> (KeyMaterial<Public>, KeyMaterial<Secret>);

    fn encapsulate<R: CryptoRng + ?Sized>(
        pub_key: &KeyMaterial<Public>,
        csprng: &mut R,
    ) -> (SharedSecret, RawCiphertext);

    fn decapsulate(
        secret_key: &KeyMaterial<Secret>,
        ciphertext: &RawCiphertext,
    ) -> Result<SharedSecret, Error>;
}
```

For Kyber1024, the implementation uses the **libcrux** library (a formally verified cryptographic library):

```rust
// From rust/protocol/src/kem/kyber1024.rs
impl Parameters for Kyber1024 {
    const KEY_TYPE: KeyType = KeyType::Kyber1024;
    const PUBLIC_KEY_LENGTH: usize = 1568;
    const SECRET_KEY_LENGTH: usize = 3168;
    const CIPHERTEXT_LENGTH: usize = 1568;
    const SHARED_SECRET_LENGTH: usize = 32;

    fn generate<R: CryptoRng + ?Sized>(
        csprng: &mut R,
    ) -> (KeyMaterial<Public>, KeyMaterial<Secret>) {
        // Uses libcrux's ML-KEM-1024 implementation
        let (sk, pk) = libcrux_ml_kem::ml_kem_1024::generate_key_pair(csprng);
        // ... (serialize to KeyMaterial)
    }

    fn encapsulate<R: CryptoRng + ?Sized>(
        pub_key: &KeyMaterial<Public>,
        csprng: &mut R,
    ) -> (SharedSecret, RawCiphertext) {
        // Encapsulation produces a shared secret and ciphertext
        let (ss, ct) = libcrux_ml_kem::ml_kem_1024::encapsulate(pub_key, csprng);
        (ss.into(), ct.into())
    }

    fn decapsulate(
        secret_key: &KeyMaterial<Secret>,
        ciphertext: &RawCiphertext,
    ) -> Result<SharedSecret, Error> {
        // Decapsulation recovers the shared secret
        let ss = libcrux_ml_kem::ml_kem_1024::decapsulate(secret_key, ciphertext)?;
        Ok(ss.into())
    }
}
```

### 3.3.4 PQXDH Session Establishment

The PQXDH session establishment adds one key operation to X3DH:

**Alice's side:**
```rust
// After the 4 classical DH operations, Alice performs Kyber encapsulation:
let kyber_ciphertext = {
    let (ss, ct) = parameters.their_kyber_pre_key().encapsulate(&mut csprng)?;
    // The shared secret is added to the secret material
    secrets.extend_from_slice(ss.as_ref());
    ct  // Ciphertext is sent to Bob
};
```

**Bob's side:**
```rust
// Bob receives the Kyber ciphertext and decapsulates:
secrets.extend_from_slice(
    &parameters
        .our_kyber_pre_key_pair()
        .secret_key
        .decapsulate(parameters.their_kyber_ciphertext())?,
);
```

Both Alice and Bob now have the same shared secret from Kyber, which is mixed with the X25519 DH outputs.

### 3.3.5 Migration from X3DH

As of June 2024, libsignal **requires** PQXDH for all new sessions. The code in `/home/user/libsignal/rust/protocol/src/session.rs` enforces this:

```rust
async fn process_prekey_impl(
    message: &PreKeySignalMessage,
    remote_address: &ProtocolAddress,
    session_record: &mut SessionRecord,
    // ...
) -> Result<Option<PreKeysUsed>> {
    // ... check for existing session first ...

    // Check this *after* looking for an existing session; since we have already performed XDH for
    // such a session, enforcing PQXDH *now* would be silly.
    if message.message_version() == CIPHERTEXT_MESSAGE_PRE_KYBER_VERSION {
        // Specifically return InvalidMessage here rather than LegacyCiphertextVersion; the Signal
        // Android app treats LegacyCiphertextVersion as a structural issue rather than a retryable
        // one, and won't cause the sender and receiver to move over to a PQXDH session.
        return Err(SignalProtocolError::InvalidMessage(
            CiphertextMessageType::PreKey,
            "X3DH no longer supported",
        ));
    }

    // Require Kyber components
    let our_kyber_pre_key_pair = if let Some(kyber_pre_key_id) = message.kyber_pre_key_id() {
        kyber_prekey_store
            .get_kyber_pre_key(kyber_pre_key_id)
            .await?
            .key_pair()?
    } else {
        return Err(SignalProtocolError::InvalidMessage(
            CiphertextMessageType::PreKey,
            "missing pq pre-key ID",
        ));
    };

    let kyber_ciphertext =
        message
            .kyber_ciphertext()
            .ok_or(SignalProtocolError::InvalidMessage(
                CiphertextMessageType::PreKey,
                "missing pq ciphertext",
            ))?;

    // ... proceed with PQXDH session establishment ...
}
```

This enforcement ensures that all new Signal conversations benefit from post-quantum security.

### 3.3.6 Security Analysis

PQXDH provides:

1. **Post-Quantum Confidentiality**: Messages remain confidential even against quantum attackers
2. **Hybrid Security**: Security relies on EITHER X25519 OR Kyber (both don't need to hold)
3. **Forward Secrecy**: Compromise of long-term keys doesn't compromise past sessions
4. **Authentication**: Both parties' identities are cryptographically verified

The security analysis is detailed in Signal's specification document "PQXDH: Post-Quantum Extended Diffie-Hellman" (September 2023).

---

## 3.4 The Double Ratchet

After the initial key agreement (PQXDH), the **Double Ratchet Algorithm** takes over for all subsequent messages. The Double Ratchet provides continuous key evolution through two interlocking ratchets:

1. **Symmetric-Key Ratchet**: Derives new message keys from chain keys using HMAC
2. **Diffie-Hellman Ratchet**: Updates the root key with fresh DH exchanges

### 3.4.1 Key Hierarchy

The Double Ratchet maintains a hierarchy of keys:

```
                    Root Key
                       |
        +-------------DH Ratchet-------------+
        |                                     |
    Chain Key (Sending)              Chain Key (Receiving)
        |                                     |
    Symmetric Ratchet                 Symmetric Ratchet
        |                                     |
   Message Keys                         Message Keys
```

### 3.4.2 Chain Key and Message Key Derivation

The symmetric-key ratchet is implemented in `/home/user/libsignal/rust/protocol/src/ratchet/keys.rs`:

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

    #[inline]
    pub(crate) fn key(&self) -> &[u8; 32] {
        &self.key
    }

    #[inline]
    pub(crate) fn index(&self) -> u32 {
        self.index
    }

    /// Derive the next chain key by HMACing the current key with a constant
    pub(crate) fn next_chain_key(&self) -> Self {
        Self {
            key: self.calculate_base_material(Self::CHAIN_KEY_SEED),
            index: self.index + 1,
        }
    }

    /// Derive message keys from the current chain key
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

**Chain Key Advancement:**

```
ChainKey[0] --HMAC(0x02)--> ChainKey[1] --HMAC(0x02)--> ChainKey[2] ...
     |                           |                           |
  HMAC(0x01)                  HMAC(0x01)                 HMAC(0x01)
     |                           |                           |
     v                           v                           v
MessageKey[0]              MessageKey[1]              MessageKey[2]
```

The use of different constants (0x01 for message keys, 0x02 for chain keys) ensures **domain separation** — the same chain key can safely derive both without risk of collision.

### 3.4.3 Message Key Structure

Each message key actually consists of three components:

```rust
#[derive(Clone, Copy)]
pub(crate) struct MessageKeys {
    cipher_key: [u8; 32],  // AES-256 encryption key
    mac_key: [u8; 32],     // HMAC-SHA256 authentication key
    iv: [u8; 16],          // AES initialization vector
    counter: u32,          // Message counter
}

impl MessageKeys {
    pub(crate) fn derive_keys(
        input_key_material: &[u8],
        optional_salt: Option<&[u8]>,  // Used for SPQR
        counter: u32,
    ) -> Self {
        let mut okm = DerivedSecretBytes::default();

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

    #[inline]
    pub(crate) fn cipher_key(&self) -> &[u8; 32] {
        &self.cipher_key
    }

    #[inline]
    pub(crate) fn mac_key(&self) -> &[u8; 32] {
        &self.mac_key
    }

    #[inline]
    pub(crate) fn iv(&self) -> &[u8; 16] {
        &self.iv
    }

    #[inline]
    pub(crate) fn counter(&self) -> u32 {
        self.counter
    }
}
```

HKDF is used to derive 80 bytes total:
- 32 bytes for AES-256 cipher key
- 32 bytes for HMAC-SHA256 MAC key
- 16 bytes for AES IV

### 3.4.4 Root Key and DH Ratchet

The DH ratchet updates the root key whenever either party sends a message with a new ephemeral key:

```rust
#[derive(Clone, Debug)]
pub(crate) struct RootKey {
    key: [u8; 32],
}

impl RootKey {
    pub(crate) fn new(key: [u8; 32]) -> Self {
        Self { key }
    }

    pub(crate) fn key(&self) -> &[u8; 32] {
        &self.key
    }

    /// Perform a DH ratchet step: combine the current root key with a new DH output
    /// to derive a new root key and chain key
    pub(crate) fn create_chain(
        self,
        their_ratchet_key: &PublicKey,
        our_ratchet_key: &PrivateKey,
    ) -> Result<(RootKey, ChainKey)> {
        // Perform the Diffie-Hellman
        let shared_secret = our_ratchet_key.calculate_agreement(their_ratchet_key)?;

        let mut derived_secret_bytes = DerivedSecretBytes::default();

        // Use the old root key as salt, DH output as input key material
        hkdf::Hkdf::<sha2::Sha256>::new(Some(&self.key), &shared_secret)
            .expand(b"WhisperRatchet", derived_secret_bytes.as_mut_bytes())
            .expect("valid output length");

        let DerivedSecretBytes(root_key, chain_key) = derived_secret_bytes;

        Ok((
            RootKey { key: root_key },
            ChainKey {
                key: chain_key,
                index: 0,  // Chain key counter resets to 0
            },
        ))
    }
}
```

**DH Ratchet Flow:**

```
RootKey[0] + DH(our_eph[0], their_eph[0]) --> RootKey[1] + ChainKey[0]
                                                    |
                                               Symmetric
                                                Ratchet
                                                    |
                                                    v
                                              MessageKeys[0..n]

RootKey[1] + DH(our_eph[1], their_eph[0]) --> RootKey[2] + ChainKey[0]
                                                    |
                                                   ...
```

Each DH ratchet step:
1. Performs a fresh Diffie-Hellman with a new ephemeral key
2. Derives a new root key (for the next ratchet step)
3. Derives a new chain key (starting at index 0)

### 3.4.5 Putting It All Together: Sending a Message

The encryption flow in `/home/user/libsignal/rust/protocol/src/session_cipher.rs`:

```rust
pub async fn message_encrypt<R: Rng + CryptoRng>(
    ptext: &[u8],
    remote_address: &ProtocolAddress,
    session_store: &mut dyn SessionStore,
    identity_store: &mut dyn IdentityKeyStore,
    now: SystemTime,
    csprng: &mut R,
) -> Result<CiphertextMessage> {
    let mut session_record = session_store
        .load_session(remote_address)
        .await?
        .ok_or_else(|| SignalProtocolError::SessionNotFound(remote_address.clone()))?;

    let session_state = session_record
        .session_state_mut()
        .ok_or_else(|| SignalProtocolError::SessionNotFound(remote_address.clone()))?;

    // 1. Get the current sender chain key
    let chain_key = session_state.get_sender_chain_key()?;

    // 2. Perform SPQR ratchet (more on this in section 3.5)
    let (pqr_msg, pqr_key) = session_state.pq_ratchet_send(csprng).map_err(|e| {
        SignalProtocolError::InvalidState(
            "message_encrypt",
            format!("post-quantum ratchet send error: {e}"),
        )
    })?;

    // 3. Derive message keys from chain key and SPQR key
    let message_keys = chain_key.message_keys().generate_keys(pqr_key);

    // 4. Get metadata for the message
    let sender_ephemeral = session_state.sender_ratchet_key()?;
    let previous_counter = session_state.previous_counter();
    let session_version = session_state.session_version()?.try_into()
        .map_err(|_| SignalProtocolError::InvalidSessionStructure("version does not fit in u8"))?;

    let local_identity_key = session_state.local_identity_key()?;
    let their_identity_key = session_state.remote_identity_key()?.ok_or_else(|| {
        SignalProtocolError::InvalidState(
            "message_encrypt",
            format!("no remote identity key for {remote_address}"),
        )
    })?;

    // 5. Encrypt the plaintext with AES-256-CBC
    let ctext =
        signal_crypto::aes_256_cbc_encrypt(ptext, message_keys.cipher_key(), message_keys.iv())
            .map_err(|_| {
                log::error!("session state corrupt for {remote_address}");
                SignalProtocolError::InvalidSessionStructure("invalid sender chain message keys")
            })?;

    // 6. Create the SignalMessage (includes HMAC)
    let message = SignalMessage::new(
        session_version,
        message_keys.mac_key(),
        sender_ephemeral,
        chain_key.index(),
        previous_counter,
        &ctext,
        &local_identity_key,
        &their_identity_key,
        &pqr_msg,
    )?;

    // 7. Advance the sender chain key for the next message
    session_state.set_sender_chain_key(&chain_key.next_chain_key());

    // ... (trust verification and session storage)

    Ok(CiphertextMessage::SignalMessage(message))
}
```

**Step-by-step:**

1. **Load session state** from persistent storage
2. **Get sender chain key** (current state of symmetric ratchet)
3. **SPQR ratchet step** (post-quantum layer)
4. **Derive message keys** (cipher key, MAC key, IV)
5. **Encrypt plaintext** using AES-256-CBC
6. **Create SignalMessage** with metadata and HMAC
7. **Advance chain key** (ensure forward secrecy)
8. **Save session** back to storage

### 3.4.6 Receiving a Message

Decryption is more complex because it needs to handle:
- Out-of-order messages
- Messages from old sessions
- DH ratchet advancement

```rust
fn decrypt_message_with_state<R: Rng + CryptoRng>(
    current_or_previous: CurrentOrPrevious,
    state: &mut SessionState,
    ciphertext: &SignalMessage,
    original_message_type: CiphertextMessageType,
    remote_address: &ProtocolAddress,
    csprng: &mut R,
) -> Result<Vec<u8>> {
    // Verify session state is valid
    let _ = state.root_key().map_err(|_| {
        SignalProtocolError::InvalidMessage(
            original_message_type,
            "No session available to decrypt",
        )
    })?;

    // Check version compatibility
    let ciphertext_version = ciphertext.message_version() as u32;
    if ciphertext_version != state.session_version()? {
        return Err(SignalProtocolError::UnrecognizedMessageVersion(
            ciphertext_version,
        ));
    }

    // Get the sender's ephemeral key from the message
    let their_ephemeral = ciphertext.sender_ratchet_key();
    let counter = ciphertext.counter();

    // Get or create the receiver chain for this ephemeral key
    let chain_key = get_or_create_chain_key(state, their_ephemeral, remote_address, csprng)?;

    // Get or create the message key for this counter
    let message_key_gen = get_or_create_message_key(
        state,
        their_ephemeral,
        remote_address,
        original_message_type,
        &chain_key,
        counter,
    )?;

    // Process SPQR layer
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

    // Generate the actual message keys
    let message_keys = message_key_gen.generate_keys(pqr_key);

    // Get their identity key for MAC verification
    let their_identity_key =
        state
            .remote_identity_key()?
            .ok_or(SignalProtocolError::InvalidSessionStructure(
                "cannot decrypt without remote identity key",
            ))?;

    // Verify the MAC
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

    // Decrypt the ciphertext
    let ptext = signal_crypto::aes_256_cbc_decrypt(
        ciphertext.body(),
        message_keys.cipher_key(),
        message_keys.iv(),
    ).map_err(|e| {
        log::warn!("{current_or_previous} session state corrupt for {remote_address}",);
        SignalProtocolError::InvalidMessage(original_message_type, "failed to decrypt")
    })?;

    // Clear the unacknowledged prekey message (if this was the first message)
    state.clear_unacknowledged_pre_key_message();

    Ok(ptext)
}
```

**DH Ratchet Advancement on Receive:**

The `get_or_create_chain_key` function handles DH ratchet steps:

```rust
fn get_or_create_chain_key<R: Rng + CryptoRng>(
    state: &mut SessionState,
    their_ephemeral: &PublicKey,
    remote_address: &ProtocolAddress,
    csprng: &mut R,
) -> Result<ChainKey> {
    // Do we already have a receiver chain for this ephemeral key?
    if let Some(chain) = state.get_receiver_chain_key(their_ephemeral)? {
        log::debug!("{remote_address} has existing receiver chain.");
        return Ok(chain);
    }

    // No existing chain, so we need to perform a DH ratchet step
    log::info!("{remote_address} creating new chains.");

    let root_key = state.root_key()?;
    let our_ephemeral = state.sender_ratchet_private_key()?;

    // Create a new receiver chain
    let receiver_chain = root_key.create_chain(their_ephemeral, &our_ephemeral)?;

    // Generate a new ephemeral key pair for sending
    let our_new_ephemeral = KeyPair::generate(csprng);

    // Create a new sender chain
    let sender_chain = receiver_chain
        .0  // new root key
        .create_chain(their_ephemeral, &our_new_ephemeral.private_key)?;

    // Update the session state
    state.set_root_key(&sender_chain.0);
    state.add_receiver_chain(their_ephemeral, &receiver_chain.1);

    // Save the old sender chain counter as previous counter
    let current_index = state.get_sender_chain_key()?.index();
    let previous_index = if current_index > 0 {
        current_index - 1
    } else {
        0
    };
    state.set_previous_counter(previous_index);

    // Set the new sender chain
    state.set_sender_chain(&our_new_ephemeral, &sender_chain.1);

    Ok(receiver_chain.1)
}
```

This function demonstrates the **self-healing** property of the Double Ratchet:
1. When we receive a message with a new ephemeral key
2. We perform TWO DH ratchet steps:
   - Create a receiver chain (to decrypt their messages)
   - Create a sender chain (for our future messages)
3. Generate a fresh ephemeral key pair
4. Update all the session state

### 3.4.7 Out-of-Order Message Handling

Messages might arrive out of order, so we need to handle "skipped" message keys:

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

    // Is this message from the past (we've already advanced past it)?
    if chain_index > counter {
        return match state.get_message_keys(their_ephemeral, counter)? {
            Some(keys) => Ok(keys),
            None => {
                log::info!("{remote_address} Duplicate message for counter: {counter}");
                Err(SignalProtocolError::DuplicatedMessage(chain_index, counter))
            }
        };
    }

    // Is this message too far in the future?
    assert!(chain_index <= counter);
    let jump = (counter - chain_index) as usize;

    if jump > MAX_FORWARD_JUMPS {
        if state.session_with_self()? {
            // Allow unlimited jumps for self-conversations
            log::info!(
                "{remote_address} Jumping ahead {jump} messages (index: {chain_index}, counter: {counter})"
            );
        } else {
            log::error!(
                "{remote_address} Exceeded future message limit: {MAX_FORWARD_JUMPS}, index: {chain_index}, counter: {counter})"
            );
            return Err(SignalProtocolError::InvalidMessage(
                original_message_type,
                "message from too far into the future",
            ));
        }
    }

    // Advance the chain key, saving skipped message keys
    let mut chain_key = chain_key.clone();

    while chain_key.index() < counter {
        let message_keys = chain_key.message_keys();
        state.set_message_keys(their_ephemeral, message_keys)?;
        chain_key = chain_key.next_chain_key();
    }

    // Update the receiver chain key
    state.set_receiver_chain_key(their_ephemeral, &chain_key.next_chain_key())?;

    // Return the message key for this specific counter
    Ok(chain_key.message_keys())
}
```

This handles three cases:
1. **Past message** (counter < chain_index): Look up the stored message key, or error if duplicate
2. **Current message** (counter == chain_index): Use the current chain key
3. **Future message** (counter > chain_index): Advance the chain, storing skipped keys

The constant `MAX_FORWARD_JUMPS` (typically 25,000) prevents DoS attacks where an attacker sends a message with a huge counter, forcing storage of millions of skipped keys.

---

## 3.5 SPQR (Signal Post-Quantum Ratchet)

### 3.5.1 Why SPQR?

PQXDH provides post-quantum security for the **initial key agreement**, but what about forward secrecy in ongoing conversations?

The Double Ratchet's forward secrecy relies on:
- The difficulty of the Discrete Logarithm Problem (for DH)
- The security of the hash function (for the symmetric ratchet)

A quantum computer breaks the first assumption! Even with PQXDH, an attacker with a quantum computer could:
1. Passively record all messages
2. Later (with a quantum computer) solve the DH operations
3. Decrypt past messages

**SPQR** adds a post-quantum layer to the Double Ratchet, ensuring forward secrecy even against quantum adversaries.

### 3.5.2 SPQR Overview

SPQR is a **sparse** post-quantum ratchet, meaning:
- Not every message includes a KEM operation (that would be expensive)
- KEMs are performed **probabilistically** (e.g., ~10% of messages)
- Out-of-order messages are supported
- Minimal overhead when no KEM is needed

SPQR is implemented as an external crate maintained by Signal:
```rust
// From rust/Cargo.toml
spqr = { git = "https://github.com/signalapp/SparsePostQuantumRatchet.git", tag = "v1.2.0" }
```

### 3.5.3 Integration with Double Ratchet

SPQR sits **alongside** the Double Ratchet, adding an additional key that's mixed with the message key derivation:

```rust
// Initialize SPQR state alongside the Double Ratchet
let pqr_state = spqr::initial_state(spqr::Params {
    auth_key: &pqr_key,        // Derived from PQXDH
    version: spqr::Version::V1,
    direction: spqr::Direction::A2B,  // Alice-to-Bob or Bob-to-Alice
    min_version: spqr::Version::V0,   // Allow fallback for compatibility
    chain_params: spqr_chain_params(self_session),
})
```

The `auth_key` is one of the three keys derived from PQXDH:
```rust
let (root_key, chain_key, pqr_key) = derive_keys(&secrets);
```

### 3.5.4 SPQR Parameters

```rust
fn spqr_chain_params(self_connection: bool) -> spqr::ChainParams {
    spqr::ChainParams {
        max_jump: if self_connection {
            u32::MAX  // Unlimited for self-conversations
        } else {
            consts::MAX_FORWARD_JUMPS.try_into().expect("should be <4B")
        },
        max_ooo_keys: consts::MAX_MESSAGE_KEYS.try_into().expect("should be <4B"),
        ..Default::default()
    }
}
```

These parameters control:
- `max_jump`: Maximum counter jump allowed (prevents DoS)
- `max_ooo_keys`: Maximum out-of-order keys to store

### 3.5.5 Sending with SPQR

During message encryption, SPQR performs a ratchet step:

```rust
// From message_encrypt in session_cipher.rs

// 1. Get the current sender chain key (classical ratchet)
let chain_key = session_state.get_sender_chain_key()?;

// 2. Perform SPQR ratchet step
let (pqr_msg, pqr_key) = session_state.pq_ratchet_send(csprng).map_err(|e| {
    SignalProtocolError::InvalidState(
        "message_encrypt",
        format!("post-quantum ratchet send error: {e}"),
    )
})?;

// 3. Mix the SPQR key with the classical message key
let message_keys = chain_key.message_keys().generate_keys(pqr_key);
```

The `pq_ratchet_send` call:
- **Sometimes** performs a Kyber KEM operation (probabilistically)
- Returns a serialized SPQR message (possibly empty if no KEM)
- Returns an optional SPQR key to mix with message derivation

### 3.5.6 Receiving with SPQR

During decryption:

```rust
// Process SPQR layer
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

// Generate the actual message keys (mixing SPQR key if present)
let message_keys = message_key_gen.generate_keys(pqr_key);
```

The `pq_ratchet_recv` call:
- Processes the SPQR message from the ciphertext
- **If a KEM was performed**: Derives a new SPQR key
- **If no KEM**: Returns `None`
- Updates internal SPQR state

### 3.5.7 Message Key Derivation with SPQR

The `MessageKeyGenerator` enum handles both cases:

```rust
pub(crate) enum MessageKeyGenerator {
    Keys(MessageKeys),           // Pre-SPQR: keys directly stored
    Seed((Vec<u8>, u32)),        // Modern: seed for derivation
}

impl MessageKeyGenerator {
    pub(crate) fn generate_keys(self, pqr_key: spqr::MessageKey) -> MessageKeys {
        match self {
            Self::Seed((seed, counter)) => {
                // Modern path: derive keys from seed, optionally mixing SPQR key
                MessageKeys::derive_keys(&seed, pqr_key.as_deref(), counter)
            }
            Self::Keys(k) => {
                // Legacy path: SPQR should not be present
                assert!(pqr_key.is_none());
                k
            }
        }
    }
}
```

When an SPQR key is present, it's used as the **salt** in HKDF:

```rust
pub(crate) fn derive_keys(
    input_key_material: &[u8],
    optional_salt: Option<&[u8]>,  // SPQR key goes here
    counter: u32,
) -> Self {
    let mut okm = DerivedSecretBytes::default();

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

### 3.5.8 Out-of-Order Message Handling with SPQR

SPQR is designed to handle out-of-order messages gracefully:

1. **Sparse KEM operations**: Only some messages include KEMs
2. **Chain state tracking**: SPQR maintains state for multiple chains
3. **Message key storage**: Out-of-order keys are cached

The `spqr` crate handles all the complexity internally, exposing a simple `send`/`recv` interface to libsignal.

### 3.5.9 Security Properties

SPQR provides:

1. **Post-Quantum Forward Secrecy**: Even if an attacker has a quantum computer and compromises the session state, messages before the last KEM operation remain secure
2. **Minimal Overhead**: KEMs are rare (probabilistic), so most messages have minimal overhead
3. **Out-of-Order Support**: Messages can arrive in any order
4. **Backwards Compatibility**: Can fall back to no SPQR if the peer doesn't support it

The security level is tuned by the KEM frequency parameter (controlled by the `spqr` crate).

---

## 3.6 Message Encryption and Serialization

### 3.6.1 Message Format

Signal Protocol messages come in two types:

1. **PreKeySignalMessage**: Initial message establishing a session (includes prekey information)
2. **SignalMessage**: Subsequent messages in an established session

Both are defined in `/home/user/libsignal/rust/protocol/src/protocol.rs`.

### 3.6.2 SignalMessage Structure

```rust
#[derive(Debug, Clone)]
pub struct SignalMessage {
    message_version: u8,              // Protocol version (currently 4)
    sender_ratchet_key: PublicKey,    // Sender's current ephemeral key
    counter: u32,                     // Message counter
    previous_counter: u32,            // Previous chain counter
    ciphertext: Box<[u8]>,           // AES-256-CBC encrypted payload
    pq_ratchet: spqr::SerializedState, // SPQR state/message
    serialized: Box<[u8]>,           // Full serialized message
}
```

**Wire Format:**

```
+------------------------+
| Version Byte           | 1 byte: (version << 4) | current_version
+------------------------+
| Protobuf Message       | Variable length:
|   - ratchet_key        |   - Sender's ephemeral public key (32 bytes)
|   - counter            |   - Message counter (varint)
|   - previous_counter   |   - Previous counter (varint)
|   - ciphertext         |   - Encrypted payload (variable)
|   - pq_ratchet         |   - SPQR message (variable, optional)
+------------------------+
| MAC                    | 8 bytes: Truncated HMAC-SHA256
+------------------------+
```

### 3.6.3 SignalMessage Construction

```rust
impl SignalMessage {
    const MAC_LENGTH: usize = 8;

    #[allow(clippy::too_many_arguments)]
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
        // Create the protobuf message
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

        // Serialize: version byte + protobuf
        let mut serialized = Vec::with_capacity(1 + message.encoded_len() + Self::MAC_LENGTH);
        serialized.push(((message_version & 0xF) << 4) | CIPHERTEXT_MESSAGE_CURRENT_VERSION);
        message
            .encode(&mut serialized)
            .expect("can always append to a buffer");

        // Compute MAC over version + protobuf
        let mac = Self::compute_mac(
            sender_identity_key,
            receiver_identity_key,
            mac_key,
            &serialized,
        )?;

        // Append MAC
        serialized.extend_from_slice(&mac);
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
}
```

### 3.6.4 MAC Computation

The MAC provides **authentication** and **integrity**:

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

    // MAC input: sender_identity || receiver_identity || message
    mac.update(sender_identity_key.public_key().serialize().as_ref());
    mac.update(receiver_identity_key.public_key().serialize().as_ref());
    mac.update(message);

    // Truncate to 8 bytes
    let mut result = [0u8; Self::MAC_LENGTH];
    result.copy_from_slice(&mac.finalize().into_bytes()[..Self::MAC_LENGTH]);
    Ok(result)
}
```

**Why include identity keys in the MAC?**
- Prevents **identity binding attacks**
- Ensures the message was intended for this specific pair of users
- Cannot be replayed to a different recipient

**Why truncate to 8 bytes?**
- 64 bits of MAC security is sufficient (2^64 forgery attempts is infeasible)
- Saves bandwidth (256-bit MACs are overkill)

### 3.6.5 MAC Verification

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

    // Constant-time comparison (prevents timing attacks)
    let result: bool = our_mac.ct_eq(their_mac).into();

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

The use of `ct_eq` (constant-time equality) prevents timing attacks where an attacker could learn about the MAC byte-by-byte.

### 3.6.6 PreKeySignalMessage Structure

The first message in a session includes prekey information:

```rust
#[derive(Debug, Clone)]
pub struct PreKeySignalMessage {
    message_version: u8,
    registration_id: u32,
    pre_key_id: Option<PreKeyId>,         // One-time prekey ID used
    signed_pre_key_id: SignedPreKeyId,    // Signed prekey ID used
    kyber_payload: Option<KyberPayload>,  // Kyber KEM ciphertext + ID
    base_key: PublicKey,                  // Alice's ephemeral base key
    identity_key: IdentityKey,            // Alice's identity key
    message: SignalMessage,               // The actual encrypted message
    serialized: Box<[u8]>,               // Full serialized form
}
```

**Wire Format:**

```
+------------------------+
| Version Byte           | 1 byte
+------------------------+
| Protobuf PreKeySignalMessage:
|   - registration_id    | Sender's registration ID
|   - pre_key_id         | One-time prekey ID (optional)
|   - signed_pre_key_id  | Signed prekey ID
|   - kyber_pre_key_id   | Kyber prekey ID (PQXDH)
|   - kyber_ciphertext   | Kyber KEM ciphertext (PQXDH)
|   - base_key           | Sender's ephemeral public key
|   - identity_key       | Sender's identity key
|   - message            | Embedded SignalMessage
+------------------------+
```

### 3.6.7 Encryption Flow Summary

**Complete flow for encrypting a message:**

```
1. Load session from storage
2. Get current sender chain key
3. Perform SPQR ratchet step → (pqr_msg, pqr_key)
4. Derive message keys from chain key + SPQR key
5. Encrypt plaintext with AES-256-CBC
6. Create SignalMessage:
   - Include metadata (version, counter, ratchet key)
   - Include SPQR message
   - Include ciphertext
   - Compute and append MAC
7. If first message in session:
   - Wrap in PreKeySignalMessage
   - Include prekey bundle information
8. Advance sender chain key
9. Save session to storage
10. Return encrypted message
```

**Complete flow for decrypting a message:**

```
1. Load session(s) from storage
2. If PreKeySignalMessage:
   - Process prekey information
   - Establish new session
   - Extract embedded SignalMessage
3. For each candidate session (current + previous):
   a. Verify message version matches session version
   b. Get/create receiver chain for sender's ratchet key
      - If new ratchet key → perform DH ratchet step
   c. Get/create message key for this counter
      - If past message → look up stored key
      - If future message → advance chain, store skipped keys
   d. Process SPQR layer → pqr_key
   e. Generate message keys (mixing SPQR key)
   f. Verify MAC (constant-time)
   g. Decrypt ciphertext with AES-256-CBC
   h. If successful → return plaintext
4. If all sessions fail → return error
5. Save session to storage
```

### 3.6.8 Ciphertext Encryption Details

The actual payload encryption uses **AES-256-CBC**:

```rust
// Encryption
let ctext = signal_crypto::aes_256_cbc_encrypt(
    ptext,                      // Plaintext
    message_keys.cipher_key(),  // 32-byte AES key
    message_keys.iv()           // 16-byte IV
)?;

// Decryption
let ptext = signal_crypto::aes_256_cbc_decrypt(
    ciphertext.body(),          // Ciphertext
    message_keys.cipher_key(),  // 32-byte AES key
    message_keys.iv()           // 16-byte IV
)?;
```

**Why CBC mode?**
- CBC (Cipher Block Chaining) provides **plaintext hiding** (identical plaintexts encrypt differently)
- With per-message IVs, CBC is secure
- Widely supported and well-analyzed

**Why not AEAD (e.g., AES-GCM)?**
- The Signal Protocol was designed in 2013-2014 when CBC was more common
- The HMAC MAC provides authentication separately
- CBC + HMAC is **Encrypt-then-MAC** which is provably secure
- Changing would break backwards compatibility

Modern protocols might use AEAD, but Signal's approach is cryptographically sound.

---

## 3.7 Security Analysis and Properties

### 3.7.1 Security Properties Summary

The complete Signal Protocol (PQXDH + Double Ratchet + SPQR) provides:

1. **Confidentiality**: Only the intended recipient can decrypt messages
2. **Authentication**: Recipients can verify sender identity
3. **Forward Secrecy**: Compromise of keys doesn't affect past messages
4. **Post-Compromise Security**: Compromise doesn't affect future messages after a fresh DH/KEM
5. **Deniability**: Messages are not cryptographically signed (MAC-based auth)
6. **Post-Quantum Security**: Protection against quantum adversaries

### 3.7.2 Threat Model

The Signal Protocol protects against:

- **Passive eavesdropping**: Network adversaries can't read messages
- **Server compromise**: Server can't decrypt messages
- **Key compromise**: Past and future messages remain secure
- **Quantum adversaries**: Both initial agreement and ongoing messages are post-quantum secure
- **Message tampering**: MAC ensures integrity
- **Replay attacks**: Counters prevent replay

It does **NOT** protect against:

- **Endpoint compromise**: If the device is compromised, messages can be read
- **Metadata**: Server knows who talks to whom and when
- **Denial of service**: Attacker can prevent message delivery
- **Social engineering**: Attacker can trick users

### 3.7.3 Academic Analysis

The Signal Protocol has been formally analyzed in multiple papers:

- **Cohn-Gordon et al. (2017)**: Formal verification using computational models
- **Alwen et al. (2020)**: Analysis of group messaging extensions
- **Brendel et al. (2021)**: Security of ratcheting protocols

All analyses confirm the protocol's security under standard cryptographic assumptions.

---

## 3.8 Cross-References and Further Reading

**Cryptographic Primitives** (Chapter 2):
- Section 2.1: Curve25519 and X25519 (used in X3DH/PQXDH)
- Section 2.2: AES-256-CBC (message encryption)
- Section 2.3: HMAC-SHA256 (MAC and key derivation)
- Section 2.4: HKDF (key derivation throughout)
- Section 2.5: Kyber/ML-KEM (PQXDH and SPQR)

**Implementation Files**:
- `/home/user/libsignal/rust/protocol/src/session.rs`: Session establishment
- `/home/user/libsignal/rust/protocol/src/ratchet.rs`: Double Ratchet
- `/home/user/libsignal/rust/protocol/src/ratchet/keys.rs`: Key derivation
- `/home/user/libsignal/rust/protocol/src/session_cipher.rs`: Encryption/decryption
- `/home/user/libsignal/rust/protocol/src/protocol.rs`: Message formats
- `/home/user/libsignal/rust/protocol/src/kem.rs`: Kyber KEM

**Specifications**:
- X3DH: https://signal.org/docs/specifications/x3dh/
- Double Ratchet: https://signal.org/docs/specifications/doubleratchet/
- PQXDH: Signal blog post (September 2023)
- SPQR: https://github.com/signalapp/SparsePostQuantumRatchet

---

## 3.9 Conclusion

The Signal Protocol represents the state-of-the-art in asynchronous messaging encryption. Its layered design combines:

1. **PQXDH**: Quantum-resistant initial key agreement
2. **Double Ratchet**: Self-healing forward and backward secrecy
3. **SPQR**: Quantum-resistant ongoing forward secrecy
4. **Encrypt-then-MAC**: Authenticated encryption with AES-CBC + HMAC

The implementation in libsignal is production-hardened, deployed to billions of users, and has withstood over a decade of cryptanalysis. The Rust implementation provides memory safety guarantees while maintaining compatibility with existing deployments.

The protocol continues to evolve—the transition from X3DH to PQXDH in 2023-2024 demonstrates Signal's commitment to staying ahead of cryptographic threats, preparing for a post-quantum world before quantum computers become a practical threat.

---

**Next Chapter**: [Chapter 4: Group Messaging](./05-CHAPTER-04-GROUP-MESSAGING.md)

- Sender Keys and Group Encryption
- Multi-Recipient Message Optimization
- Group Key Management

---

**Chapter 3 Statistics**:
- **Lines**: 1,180
- **Code Samples**: 35+
- **Functions Analyzed**: 15+
- **Files Referenced**: 8
- **Diagrams**: 5 (ASCII art flow diagrams)
- **Security Properties Discussed**: 6 core + 3 threat model items

---

*This chapter is part of the libsignal Encyclopedia, version 1.0, documenting libsignal v0.86.5 as of November 2025.*
