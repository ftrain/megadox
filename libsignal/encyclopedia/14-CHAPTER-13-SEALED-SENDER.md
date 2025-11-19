# Chapter 13: Literate Programming - Sealed Sender Deep-Dive

## Metadata Protection Through Anonymous Envelope Encryption

---

### Table of Contents

1. [Introduction](#introduction)
2. [The Metadata Protection Problem](#1-the-metadata-protection-problem)
3. [Server Certificates: Establishing Trust](#2-server-certificates-establishing-trust)
4. [Sender Certificates: Identity Binding](#3-sender-certificates-identity-binding)
5. [Sealed Sender v1: The Original Design](#4-sealed-sender-v1-the-original-design)
6. [Sealed Sender v2: ChaCha20-Poly1305 Migration](#5-sealed-sender-v2-chacha20-poly1305-migration)
7. [Multi-Layer Encryption Architecture](#6-multi-layer-encryption-architecture)
8. [Multi-Recipient Sealed Sender: Efficiency at Scale](#7-multi-recipient-sealed-sender-efficiency-at-scale)
9. [Decryption Flow: Unwrapping the Layers](#8-decryption-flow-unwrapping-the-layers)
10. [Security Analysis and Migration Path](#9-security-analysis-and-migration-path)

---

## Introduction

While the Signal Protocol's Double Ratchet provides excellent message content encryption, traditional messaging systems leak significant metadata to the server: **who is talking to whom**. Even with end-to-end encrypted content, a server that sees `From: Alice, To: Bob` reveals the social graph and communication patterns.

**Sealed Sender** solves this by encrypting the sender's identity in a way that:
1. The server cannot determine who sent a message (only who receives it)
2. The recipient can verify the sender's identity and certificate validity
3. The system scales efficiently for group messaging scenarios

This chapter provides a complete literate programming walkthrough of libsignal's Sealed Sender implementation, covering both the original v1 design using AES-GCM-SIV and the modern v2 design optimized for multi-recipient scenarios.

### Prerequisites

- **Double Ratchet**: Understanding of session-based encryption (Chapter 8)
- **X3DH/PQXDH**: Session establishment mechanisms (Chapter 7)
- **Key Derivation**: HKDF and key agreement fundamentals (Chapter 2)
- **Certificate Chains**: Basic public key infrastructure concepts

### Source Files Referenced

Primary implementation files:
- **`rust/protocol/src/sealed_sender.rs`**: Complete Sealed Sender implementation (2000+ lines)
- **`rust/protocol/src/proto/sealed_sender.proto`**: Protobuf message definitions
- **`rust/protocol/tests/sealed_sender.rs`**: Comprehensive test suite

---

## 1. The Metadata Protection Problem

### 1.1 What Metadata Reveals

Traditional encrypted messaging reveals to the server:

```
Envelope {
    from: "alice@example.org",     // Sender identity visible
    to: "bob@example.org",         // Recipient identity visible
    timestamp: 1699564800,         // Communication timing visible
    content: [encrypted bytes]     // Only this is protected
}
```

**What the server learns:**
- **Social graph**: Who communicates with whom
- **Communication patterns**: Frequency, timing, burst patterns
- **Group membership**: Who belongs to which groups
- **Activity correlation**: Link anonymous and known identities

### 1.2 Why Sealed Sender is Needed

**Privacy threats from metadata:**

1. **Government surveillance**: Build social graphs without warrant
2. **Traffic analysis**: Identify key figures in organizations
3. **Correlation attacks**: Link pseudonymous identities
4. **Workplace monitoring**: Track employee communications

**Example attack:** Even with encrypted content, observing that "Anonymous User X" messages "Journalist Y" every day at 9 AM reveals likely identity through timing correlation.

### 1.3 Privacy Properties Achieved

Sealed Sender provides:

| Property | Description |
|----------|-------------|
| **Sender anonymity to server** | Server cannot determine who sent a message |
| **Recipient authenticity** | Recipient can verify sender's identity |
| **Certificate-based trust** | Sender must have valid server-issued certificate |
| **Forward secrecy** | Ephemeral keys protect even if long-term keys compromised |
| **Deniability** | No proof of who sent a message (like Signal Protocol) |

**What's still visible to server:**
- Message recipient (necessary for routing)
- Message size and timing
- That Sealed Sender is being used (version byte)

---

## 2. Server Certificates: Establishing Trust

Server certificates form the root of trust in the Sealed Sender system. The server proves its authority to issue sender certificates by signing them with a private key corresponding to a public key baked into client applications.

### 2.1 Server Certificate Structure

**File: `rust/protocol/src/proto/sealed_sender.proto:10-18`**

```protobuf
message ServerCertificate {
    message Certificate {
        optional uint32 id  = 1;    // Unique certificate ID
        optional bytes  key = 2;    // Server's public key
    }

    optional bytes certificate = 1;  // Serialized Certificate
    optional bytes signature   = 2;  // Signed by trust root
}
```

**File: `rust/protocol/src/sealed_sender.rs:29-36`**

```rust
#[derive(Debug, Clone)]
pub struct ServerCertificate {
    serialized: Vec<u8>,      // Complete serialized form
    key_id: u32,              // Certificate ID (for revocation)
    key: PublicKey,           // Server's signing key
    certificate: Vec<u8>,     // Inner certificate data
    signature: Vec<u8>,       // Trust root's signature
}
```

**Key insight:** The certificate and signature are stored separately for efficient validation without re-parsing.

### 2.2 Trust Model and Known Certificates

libsignal embeds known production server certificates to save bandwidth:

**File: `rust/protocol/src/sealed_sender.rs:57-86`**

```rust
/// A set of server certificates that can be omitted from sender certificates
/// for space savings, keyed by ID.
const KNOWN_SERVER_CERTIFICATES: &[(u32, [u8; 33], &[u8])] = &[
    (
        2,
        // Staging trust root (XEd25519 public key without type byte)
        data_encoding_macro::base64!("BYhU6tPjqP46KGZEzRs1OL4U39V5dlPJ/X09ha4rErkm"),
        &const_str::hex!(
            "0a25080212210539450d63ebd0752c0fd4038b9d07a916f5e174b756d409b5ca79f4c97400631e..."
        ),
    ),
    (
        3,
        // Production trust root
        data_encoding_macro::base64!("BUkY0I+9+oPgDCn4+Ac6Iu813yvqkDr/ga8DzLxFxuk6"),
        &const_str::hex!(
            "0a250803122105bc9d1d290be964810dfa7e94856480a3f7060d004c9762c24c575a1522353a5a..."
        ),
    ),
    (
        0x7357C357,  // Test certificate
        // Public key for all-zeros private key (never used in production)
        data_encoding_macro::base64!("BS/lfaNHzWJDFSjarF+7KQcw//aEr8TPwu2QmV9Yyzt0"),
        &const_str::hex!("0a2908d786df9a07..."),
    ),
];
```

**Bandwidth optimization:** By referencing certificate ID instead of embedding full certificate, sender certificates save ~100 bytes per message.

### 2.3 Certificate Validation Logic

**File: `rust/protocol/src/sealed_sender.rs:158-167`**

```rust
pub fn validate(&self, trust_root: &PublicKey) -> Result<bool> {
    // Check revocation list
    if REVOKED_SERVER_CERTIFICATE_KEY_IDS.contains(&self.key_id()?) {
        log::error!(
            "received server certificate with revoked ID {:x}",
            self.key_id()?
        );
        return Ok(false);
    }

    // Verify signature using trust root
    Ok(trust_root.verify_signature(&self.certificate, &self.signature))
}
```

**Revocation mechanism:**

```rust
const REVOKED_SERVER_CERTIFICATE_KEY_IDS: &[u32] = &[0xDEADC357];
```

**Security property:** Constant-time comparison prevents timing attacks that could reveal which certificate IDs are revoked.

### 2.4 Creating Server Certificates

**File: `rust/protocol/src/sealed_sender.rs:128-156`**

```rust
pub fn new<R: Rng + CryptoRng>(
    key_id: u32,
    key: PublicKey,
    trust_root: &PrivateKey,  // Offline root key
    rng: &mut R,
) -> Result<Self> {
    // Build inner certificate
    let certificate_pb = proto::sealed_sender::server_certificate::Certificate {
        id: Some(key_id),
        key: Some(key.serialize().to_vec()),
    };

    let certificate = certificate_pb.encode_to_vec();

    // Sign with trust root (XEd25519 signature)
    let signature = trust_root.calculate_signature(&certificate, rng)?.to_vec();

    // Build outer envelope
    let serialized = proto::sealed_sender::ServerCertificate {
        certificate: Some(certificate.clone()),
        signature: Some(signature.clone()),
    }
    .encode_to_vec();

    Ok(Self {
        serialized,
        certificate,
        signature,
        key,
        key_id,
    })
}
```

**Trust model flow:**
1. **Trust root** (kept offline) signs server certificate
2. **Server certificate** is embedded in clients
3. **Server** uses its private key to sign sender certificates
4. **Clients** validate chain: `trust_root → server_cert → sender_cert`

---

## 3. Sender Certificates: Identity Binding

Sender certificates bind a user's identity to their identity key, proving they're authorized to send sealed sender messages.

### 3.1 Sender Certificate Structure

**File: `rust/protocol/src/proto/sealed_sender.proto:20-38`**

```protobuf
message SenderCertificate {
    message Certificate {
        optional string            senderE164    = 1;  // Phone number (optional)
        oneof senderUuid {
            string                 uuidString    = 6;  // Service ID (string form)
            bytes                  uuidBytes     = 7;  // Service ID (binary form)
        }
        optional uint32            senderDevice  = 2;  // Device ID
        optional fixed64           expires       = 3;  // Expiration timestamp (ms)
        optional bytes             identityKey   = 4;  // User's identity key
        oneof signer {
            bytes /*ServerCertificate*/ certificate = 5;  // Embedded server cert
            uint32                      id          = 8;  // Or reference to known cert
        }
    }

    optional bytes certificate = 1;  // Serialized Certificate
    optional bytes signature   = 2;  // Signed by server certificate
}
```

**File: `rust/protocol/src/sealed_sender.rs:191-207`**

```rust
#[derive(Debug, Clone)]
enum SenderCertificateSigner {
    Embedded(ServerCertificate),  // Full cert included
    Reference(u32),                // Reference to KNOWN_SERVER_CERTIFICATES
}

#[derive(Debug, Clone)]
pub struct SenderCertificate {
    signer: SenderCertificateSigner,
    key: PublicKey,                // Sender's identity key
    sender_device_id: DeviceId,
    sender_uuid: String,           // Service ID
    sender_e164: Option<String>,   // Phone number (optional)
    expiration: Timestamp,         // Certificate expiration
    serialized: Vec<u8>,
    certificate: Vec<u8>,
    signature: Vec<u8>,
}
```

### 3.2 Certificate Creation

**File: `rust/protocol/src/sealed_sender.rs:277-325`**

```rust
pub fn new<R: Rng + CryptoRng>(
    sender_uuid: String,
    sender_e164: Option<String>,
    key: PublicKey,                   // User's identity key
    sender_device_id: DeviceId,
    expiration: Timestamp,
    signer: ServerCertificate,        // Server's certificate
    signer_key: &PrivateKey,          // Server's private key
    rng: &mut R,
) -> Result<Self> {
    // Build inner certificate with sender identity
    let certificate_pb = proto::sealed_sender::sender_certificate::Certificate {
        sender_uuid: Some(
            proto::sealed_sender::sender_certificate::certificate::SenderUuid::UuidString(
                sender_uuid.clone(),
            ),
        ),
        sender_e164: sender_e164.clone(),
        sender_device: Some(sender_device_id.into()),
        expires: Some(expiration.epoch_millis()),
        identity_key: Some(key.serialize().to_vec()),
        signer: Some(
            proto::sealed_sender::sender_certificate::certificate::Signer::Certificate(
                signer.serialized()?.to_vec(),
            ),
        ),
    };

    let certificate = certificate_pb.encode_to_vec();

    // Server signs the certificate
    let signature = signer_key.calculate_signature(&certificate, rng)?.to_vec();

    let serialized = proto::sealed_sender::SenderCertificate {
        certificate: Some(certificate.clone()),
        signature: Some(signature.clone()),
    }
    .encode_to_vec();

    Ok(Self {
        signer: SenderCertificateSigner::Embedded(signer),
        key,
        sender_device_id,
        sender_uuid,
        sender_e164,
        expiration,
        serialized,
        certificate,
        signature,
    })
}
```

### 3.3 Validation with Multiple Trust Roots

**File: `rust/protocol/src/sealed_sender.rs:331-369`**

```rust
pub fn validate_with_trust_roots(
    &self,
    trust_roots: &[&PublicKey],
    validation_time: Timestamp,
) -> Result<bool> {
    let signer = self.signer()?;

    // Check signature against ALL trust roots (constant-time)
    let mut any_valid = Choice::from(0u8);
    for root in trust_roots {
        let ok = signer.validate(root)?;
        any_valid |= Choice::from(u8::from(ok));  // Bitwise OR in constant time
    }
    if !bool::from(any_valid) {
        log::error!(
            "sender certificate contained server certificate that \
             wasn't signed by any trust root"
        );
        return Ok(false);
    }

    // Verify sender certificate signature
    if !signer
        .public_key()?
        .verify_signature(&self.certificate, &self.signature)
    {
        log::error!("sender certificate not signed by server");
        return Ok(false);
    }

    // Check expiration
    if validation_time > self.expiration {
        log::error!(
            "sender certificate is expired (expiration: {}, validation_time: {})",
            self.expiration.epoch_millis(),
            validation_time.epoch_millis()
        );
        return Ok(false);
    }

    Ok(true)
}
```

**Security properties:**
- **Constant-time validation**: Prevents timing attacks revealing which trust root matched
- **Expiration checking**: Certificates have limited lifetime (typically days to weeks)
- **Revocation support**: Via revoked server certificate IDs

### 3.4 Lazy Loading of Known Certificates

**File: `rust/protocol/src/sealed_sender.rs:371-394`**

```rust
pub fn signer(&self) -> Result<&ServerCertificate> {
    // Lazy static initialization of known certificates
    static CERT_MAP: LazyLock<HashMap<u32, (PublicKey, ServerCertificate)>> =
        LazyLock::new(|| {
            HashMap::from_iter(KNOWN_SERVER_CERTIFICATES.iter().map(
                |(id, trust_root, cert)| {
                    (
                        *id,
                        (
                            PublicKey::deserialize(trust_root).expect("valid"),
                            ServerCertificate::deserialize(cert).expect("valid"),
                        ),
                    )
                },
            ))
        });

    match &self.signer {
        SenderCertificateSigner::Embedded(cert) => Ok(cert),
        SenderCertificateSigner::Reference(id) => CERT_MAP
            .get(id)
            .map(|(_trust_root, cert)| cert)
            .ok_or_else(|| SignalProtocolError::UnknownSealedSenderServerCertificateId(*id)),
    }
}
```

**Optimization:** Known certificates are deserialized once and cached for lifetime of process.

---

## 4. Sealed Sender v1: The Original Design

Sealed Sender v1 implements a **two-layer encryption** scheme where:
1. **Ephemeral layer**: Encrypts sender's identity key (server can decrypt to route)
2. **Static layer**: Encrypts actual message content (only recipient can decrypt)

### 4.1 Cryptographic Primitives (v1)

**File: `rust/protocol/src/sealed_sender.rs:700-813`**

```rust
mod sealed_sender_v1 {
    use super::*;

    const SALT_PREFIX: &[u8] = b"UnidentifiedDelivery";

    /// Ephemeral keys derived from ephemeral keypair + recipient identity
    pub(super) struct EphemeralKeys {
        pub(super) chain_key: [u8; 32],   // For deriving static keys
        pub(super) cipher_key: [u8; 32],  // For encrypting sender identity
        pub(super) mac_key: [u8; 32],     // For MAC over encrypted identity
    }

    impl EphemeralKeys {
        /// Derive keys from DH(ephemeral, recipient_identity)
        pub(super) fn calculate(
            our_keys: &KeyPair,           // Ephemeral keypair
            their_public: &PublicKey,     // Recipient's identity key
            direction: Direction,
        ) -> Result<Self> {
            let our_pub_key = our_keys.public_key.serialize();
            let their_pub_key = their_public.serialize();

            // Salt varies by direction to prevent reflection attacks
            let ephemeral_salt = match direction {
                Direction::Sending => [SALT_PREFIX, &their_pub_key, &our_pub_key],
                Direction::Receiving => [SALT_PREFIX, &our_pub_key, &their_pub_key],
            }
            .concat();

            // DH agreement
            let shared_secret = our_keys.private_key.calculate_agreement(their_public)?;

            // Derive 96 bytes: chain_key || cipher_key || mac_key
            #[derive(Default, KnownLayout, IntoBytes, FromBytes)]
            #[repr(C, packed)]
            struct DerivedValues([u8; 32], [u8; 32], [u8; 32]);
            let mut derived_values = DerivedValues::default();

            hkdf::Hkdf::<sha2::Sha256>::new(Some(&ephemeral_salt), &shared_secret)
                .expand(&[], derived_values.as_mut_bytes())
                .expect("valid output length");

            let DerivedValues(chain_key, cipher_key, mac_key) = derived_values;

            Ok(Self {
                chain_key,
                cipher_key,
                mac_key,
            })
        }
    }

    /// Static keys derived from sender identity + recipient identity + chain key
    pub(super) struct StaticKeys {
        pub(super) cipher_key: [u8; 32],  // For encrypting message
        pub(super) mac_key: [u8; 32],     // For MAC over encrypted message
    }

    impl StaticKeys {
        pub(super) fn calculate(
            our_keys: &IdentityKeyPair,   // Sender's long-term identity
            their_key: &PublicKey,         // Recipient's identity key
            chain_key: &[u8; 32],          // From ephemeral keys
            ctext: &[u8],                  // Encrypted sender identity
        ) -> Result<Self> {
            // Salt includes encrypted sender identity for binding
            let salt = [chain_key, ctext].concat();

            let shared_secret = our_keys.private_key().calculate_agreement(their_key)?;

            // Derive 96 bytes but discard first 32 (mirrors ephemeral derivation)
            #[derive(Default, KnownLayout, IntoBytes, FromBytes)]
            #[repr(C, packed)]
            struct DerivedValues(#[allow(unused)] [u8; 32], [u8; 32], [u8; 32]);
            let mut derived_values = DerivedValues::default();

            hkdf::Hkdf::<sha2::Sha256>::new(Some(&salt), &shared_secret)
                .expand(&[], derived_values.as_mut_bytes())
                .expect("valid output length");

            let DerivedValues(_, cipher_key, mac_key) = derived_values;

            Ok(Self {
                cipher_key,
                mac_key,
            })
        }
    }
}
```

**Key insight:** The chain key from ephemeral layer is fed into static layer derivation, cryptographically linking the two layers.

### 4.2 Encryption Flow (v1)

**File: `rust/protocol/src/sealed_sender.rs:967-1018`**

```rust
/// Sealed Sender v1: Single-recipient encryption
pub async fn sealed_sender_encrypt_from_usmc<R: Rng + CryptoRng>(
    destination: &ProtocolAddress,
    usmc: &UnidentifiedSenderMessageContent,
    identity_store: &dyn IdentityKeyStore,
    rng: &mut R,
) -> Result<Vec<u8>> {
    let our_identity = identity_store.get_identity_key_pair().await?;
    let their_identity = identity_store
        .get_identity(destination)
        .await?
        .ok_or_else(|| SignalProtocolError::SessionNotFound(destination.clone()))?;

    // Step 1: Generate ephemeral keypair
    let ephemeral = KeyPair::generate(rng);

    // Step 2: Derive ephemeral keys from DH(ephemeral, their_identity)
    let eph_keys = sealed_sender_v1::EphemeralKeys::calculate(
        &ephemeral,
        their_identity.public_key(),
        Direction::Sending,
    )?;

    // Step 3: Encrypt our identity key with ephemeral keys (AES-256-CTR + HMAC-SHA256)
    let static_key_ctext = crypto::aes256_ctr_hmacsha256_encrypt(
        &our_identity.public_key().serialize(),  // 33 bytes
        &eph_keys.cipher_key,
        &eph_keys.mac_key,
    )
    .expect("just generated these keys, they should be correct");

    // Step 4: Derive static keys from DH(our_identity, their_identity)
    let static_keys = sealed_sender_v1::StaticKeys::calculate(
        &our_identity,
        their_identity.public_key(),
        &eph_keys.chain_key,
        &static_key_ctext,
    )?;

    // Step 5: Encrypt message content with static keys (AES-256-CTR + HMAC-SHA256)
    let message_data = crypto::aes256_ctr_hmacsha256_encrypt(
        usmc.serialized()?,  // Contains sender cert + encrypted message
        &static_keys.cipher_key,
        &static_keys.mac_key,
    )
    .expect("just generated these keys, they should be correct");

    // Step 6: Serialize as protobuf with version byte
    let mut serialized = vec![SEALED_SENDER_V1_FULL_VERSION];  // 0x11
    let pb = proto::sealed_sender::UnidentifiedSenderMessage {
        ephemeral_public: Some(ephemeral.public_key.serialize().to_vec()),
        encrypted_static: Some(static_key_ctext),
        encrypted_message: Some(message_data),
    };
    pb.encode(&mut serialized)
        .expect("can always append to Vec");

    Ok(serialized)
}
```

### 4.3 Two-Layer Encryption Visual

```
┌─────────────────────────────────────────────────────────────┐
│                     Sealed Sender v1                        │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌─────────────────────────────────────────────────────┐  │
│  │  Version: 0x11                                      │  │
│  └─────────────────────────────────────────────────────┘  │
│                                                             │
│  ┌─────────────────────────────────────────────────────┐  │
│  │  Ephemeral Public Key (33 bytes)                    │  │
│  │  E_pub = generate_random()                          │  │
│  └─────────────────────────────────────────────────────┘  │
│                                                             │
│  ┌─────────────────────────────────────────────────────┐  │
│  │  LAYER 1: Encrypted Sender Identity                 │  │
│  │  ┌───────────────────────────────────────────────┐  │  │
│  │  │ Keys: HKDF(DH(E_priv, R_identity))           │  │  │
│  │  │ Ciphertext: AES-CTR(Sender_Identity_Key)     │  │  │
│  │  │ MAC: HMAC-SHA256(ciphertext)                 │  │  │
│  │  └───────────────────────────────────────────────┘  │  │
│  └─────────────────────────────────────────────────────┘  │
│                                                             │
│  ┌─────────────────────────────────────────────────────┐  │
│  │  LAYER 2: Encrypted Message                         │  │
│  │  ┌───────────────────────────────────────────────┐  │  │
│  │  │ Keys: HKDF(DH(S_identity, R_identity),        │  │  │
│  │  │             chain_key, layer1_ctext)          │  │  │
│  │  │ Plaintext: SenderCertificate || Message       │  │  │
│  │  │ Ciphertext: AES-CTR(plaintext)                │  │  │
│  │  │ MAC: HMAC-SHA256(ciphertext)                  │  │  │
│  │  └───────────────────────────────────────────────┘  │  │
│  └─────────────────────────────────────────────────────┘  │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

**Security properties:**
- **Forward secrecy**: Ephemeral key protects even if long-term keys compromised
- **Sender authentication**: Only holder of sender's private key can create valid message
- **Binding**: Chain key cryptographically links both layers

---

## 5. Sealed Sender v2: ChaCha20-Poly1305 Migration

Sealed Sender v2 was designed for **multi-recipient efficiency**, using a single random seed to encrypt messages for multiple recipients without recomputation.

### 5.1 Cryptographic Primitives (v2)

**File: `rust/protocol/src/sealed_sender.rs:1020-1144`**

```rust
mod sealed_sender_v2 {
    use super::*;

    // Domain separation labels
    const LABEL_R: &[u8] = b"Sealed Sender v2: r (2023-08)";
    const LABEL_K: &[u8] = b"Sealed Sender v2: K";
    const LABEL_DH: &[u8] = b"Sealed Sender v2: DH";
    const LABEL_DH_S: &[u8] = b"Sealed Sender v2: DH-sender";

    pub const MESSAGE_KEY_LEN: usize = 32;
    pub const CIPHER_KEY_LEN: usize = 32;  // AES-256-GCM-SIV
    pub const AUTH_TAG_LEN: usize = 16;
    pub const PUBLIC_KEY_LEN: usize = 32;  // Curve25519

    /// Keys derived from random message seed M
    pub(super) struct DerivedKeys {
        kdf: hkdf::Hkdf<sha2::Sha256>,
    }

    impl DerivedKeys {
        /// Initialize from random bytes M
        pub(super) fn new(m: &[u8]) -> DerivedKeys {
            Self {
                kdf: hkdf::Hkdf::<sha2::Sha256>::new(None, m),
            }
        }

        /// Derive ephemeral keypair: E = DeriveKeyPair(r), where r = KDF(M, "r")
        pub(super) fn derive_e(&self) -> KeyPair {
            let mut r = [0; 32];
            self.kdf
                .expand(LABEL_R, &mut r)
                .expect("valid output length");
            let e = PrivateKey::try_from(&r[..]).expect("valid PrivateKey");
            KeyPair::try_from(e).expect("can derive public key")
        }

        /// Derive symmetric key: K = KDF(M, "K")
        pub(super) fn derive_k(&self) -> [u8; CIPHER_KEY_LEN] {
            let mut k = [0; CIPHER_KEY_LEN];
            self.kdf
                .expand(LABEL_K, &mut k)
                .expect("valid output length");
            k
        }
    }

    /// Encrypt/decrypt M using XOR with derived key
    ///
    /// C_i = KDF(DH(E, R_i) || E.pub || R_i.pub) XOR M
    pub(super) fn apply_agreement_xor(
        our_keys: &KeyPair,
        their_key: &PublicKey,
        direction: Direction,
        input: &[u8; MESSAGE_KEY_LEN],
    ) -> Result<[u8; MESSAGE_KEY_LEN]> {
        let agreement = our_keys.calculate_agreement(their_key)?;

        // Concatenate DH output with public keys (order matters for direction)
        let agreement_key_input = match direction {
            Direction::Sending => [
                agreement,
                our_keys.public_key.serialize(),
                their_key.serialize(),
            ],
            Direction::Receiving => [
                agreement,
                their_key.serialize(),
                our_keys.public_key.serialize(),
            ],
        }
        .concat();

        // Derive XOR key
        let mut result = [0; MESSAGE_KEY_LEN];
        hkdf::Hkdf::<sha2::Sha256>::new(None, &agreement_key_input)
            .expand(LABEL_DH, &mut result)
            .expect("valid output length");

        // XOR with input
        result
            .iter_mut()
            .zip(input)
            .for_each(|(result_byte, input_byte)| *result_byte ^= input_byte);

        Ok(result)
    }

    /// Compute authentication tag
    ///
    /// AT_i = KDF(DH(S, R_i) || E.pub || C_i || S.pub || R_i.pub)
    pub(super) fn compute_authentication_tag(
        our_keys: &IdentityKeyPair,     // Sender's long-term identity
        their_key: &IdentityKey,         // Recipient's identity
        direction: Direction,
        ephemeral_pub_key: &PublicKey,   // E.pub
        encrypted_message_key: &[u8; MESSAGE_KEY_LEN],  // C_i
    ) -> Result<[u8; AUTH_TAG_LEN]> {
        let agreement = our_keys
            .private_key()
            .calculate_agreement(their_key.public_key())?;

        let mut agreement_key_input = agreement.into_vec();
        agreement_key_input.extend_from_slice(&ephemeral_pub_key.serialize());
        agreement_key_input.extend_from_slice(encrypted_message_key);

        // Append identity keys in direction-dependent order
        match direction {
            Direction::Sending => {
                agreement_key_input.extend_from_slice(&our_keys.public_key().serialize());
                agreement_key_input.extend_from_slice(&their_key.serialize());
            }
            Direction::Receiving => {
                agreement_key_input.extend_from_slice(&their_key.serialize());
                agreement_key_input.extend_from_slice(&our_keys.public_key().serialize());
            }
        }

        let mut result = [0; AUTH_TAG_LEN];
        hkdf::Hkdf::<sha2::Sha256>::new(None, &agreement_key_input)
            .expand(LABEL_DH_S, &mut result)
            .expect("valid output length");
        Ok(result)
    }
}
```

### 5.2 Key Differences from v1

| Aspect | v1 | v2 |
|--------|----|----|
| **Encryption** | AES-256-CTR + HMAC | AES-256-GCM-SIV (AEAD) |
| **Layer 1** | Encrypts sender identity | Encrypts random seed M |
| **Multi-recipient** | Re-encrypt for each | Shared ciphertext, per-recipient headers |
| **Wire format** | Protobuf | Flat binary (more efficient) |
| **Derivation** | Two HKDF calls | Single seed M → multiple keys |

### 5.3 Version Byte Encoding

**File: `rust/protocol/src/sealed_sender.rs:629-633`**

```rust
const SEALED_SENDER_V1_MAJOR_VERSION: u8 = 1;
const SEALED_SENDER_V1_FULL_VERSION: u8 = 0x11;  // (1 << 4) | 1
const SEALED_SENDER_V2_MAJOR_VERSION: u8 = 2;
const SEALED_SENDER_V2_UUID_FULL_VERSION: u8 = 0x22;  // (2 << 4) | 2
const SEALED_SENDER_V2_SERVICE_ID_FULL_VERSION: u8 = 0x23;  // (2 << 4) | 3
```

**Format:** `(required_version << 4) | current_version`

- `0x11`: v1 message (required v1, is v1)
- `0x22`: v2 message with UUID (required v2, is v2)
- `0x23`: v2 message with ServiceId (required v2, is v3 encoding)
- Hypothetical `0x34`: v4 message decodable by v3 clients

---

## 6. Multi-Layer Encryption Architecture

### 6.1 Ephemeral Layer (Server Can Decrypt)

In v2, the ephemeral layer no longer encrypts the sender's identity. Instead, it encrypts a **random seed M** that only the recipient can recover.

**Why encrypt M instead of sender identity?**
- Server never learns sender (better privacy)
- M can be reused across recipients (efficiency)
- Same ciphertext for all recipients (bandwidth savings)

### 6.2 Static Layer (Only Recipient)

The authentication tag binds sender's identity without revealing it to the server:

```
AT = HKDF(DH(sender_identity, recipient_identity), E.pub, C, S.pub, R.pub)
```

**Properties:**
- Server cannot compute (lacks sender's private key)
- Recipient can verify sender (has both public keys)
- Unique per recipient (includes R.pub)

### 6.3 Complete Code Flow

**File: `rust/protocol/src/sealed_sender.rs:1400-1421`**

```rust
async fn sealed_sender_multi_recipient_encrypt_impl<R: Rng + CryptoRng>(
    destinations: &[&ProtocolAddress],
    destination_sessions: &[&SessionRecord],
    excluded_recipients: impl IntoIterator<Item = ServiceId>,
    usmc: &UnidentifiedSenderMessageContent,
    identity_store: &dyn IdentityKeyStore,
    rng: &mut R,
) -> Result<Vec<u8>> {
    let our_identity = identity_store.get_identity_key_pair().await?;

    // Step 1: Generate random seed M
    let m: [u8; sealed_sender_v2::MESSAGE_KEY_LEN] = rng.random();

    // Step 2: Derive keys from M
    let keys = sealed_sender_v2::DerivedKeys::new(&m);
    let e = keys.derive_e();  // Ephemeral keypair
    let e_pub = &e.public_key;

    // Step 3: Encrypt shared ciphertext with AES-GCM-SIV
    let ciphertext = {
        let mut ciphertext = usmc.serialized()?.to_vec();
        let symmetric_authentication_tag = Aes256GcmSiv::new(&keys.derive_k().into())
            .encrypt_in_place_detached(
                &aes_gcm_siv::Nonce::default(),  // No nonce (key is one-use)
                &[],  // No associated data
                &mut ciphertext,
            )
            .expect("AES-GCM-SIV encryption should not fail");
        ciphertext.extend_from_slice(&symmetric_authentication_tag);
        ciphertext
    };

    // ... (per-recipient encryption continues below)
}
```

**Key insight:** The symmetric ciphertext is encrypted ONCE, regardless of recipient count. Only the headers (C_i, AT_i) are computed per-recipient.

---

## 7. Multi-Recipient Sealed Sender: Efficiency at Scale

### 7.1 Shared Payload Optimization

**Problem:** Sending same message to N recipients naively requires N full encryptions.

**Solution:** Use **randomness reuse** from [Barbosa's paper](https://haslab.uminho.pt/mbb/files/reuse.pdf):

1. Generate random seed M once
2. Encrypt message once with K = KDF(M)
3. For each recipient i:
   - Compute C_i = Encrypt_ephemeral(M)
   - Compute AT_i = AuthTag_static(C_i)

### 7.2 Per-Recipient Header Computation

**File: `rust/protocol/src/sealed_sender.rs:1423-1495`** (continued)

```rust
    // Group destinations by service ID for efficiency
    let identity_keys_and_ranges: Vec<(IdentityKey, Range<usize>)> = {
        let mut identity_keys_and_ranges = vec![];
        for (_, mut next_group) in &destinations
            .iter()
            .enumerate()
            .chunk_by(|(_i, next)| next.name())  // Group by service ID
        {
            let (i, &destination) = next_group.next().expect("at least one");
            let count = 1 + next_group.count();

            let their_identity = identity_store
                .get_identity(destination)
                .await?
                .ok_or_else(|| SignalProtocolError::SessionNotFound(destination.clone()))?;

            identity_keys_and_ranges.push((their_identity, i..i + count));
        }
        identity_keys_and_ranges
    };

    // Compute per-recipient C_i and AT_i
    let per_recipient_data: Vec<_> = identity_keys_and_ranges
        .iter()
        .map(|(their_identity, range)| {
            // C_i = XOR(M, KDF(DH(E, R_i)))
            let c_i = sealed_sender_v2::apply_agreement_xor(
                &e,
                their_identity.public_key(),
                Direction::Sending,
                &m,
            )?;

            // AT_i = KDF(DH(S, R_i), E.pub, C_i)
            let at_i = sealed_sender_v2::compute_authentication_tag(
                &our_identity,
                their_identity,
                Direction::Sending,
                e_pub,
                &c_i,
            )?;

            Ok((range.clone(), c_i, at_i))
        })
        .collect::<Result<_>>()?;
```

### 7.3 Wire Format (Sent Message)

**File: `rust/protocol/src/sealed_sender.rs:1313-1350`** (from docs)

```
SentMessage {
    version_byte: u8,           // 0x22 or 0x23
    count: varint,              // Number of recipients
    recipients: [PerRecipientData | ExcludedRecipient; count],
    e_pub: [u8; 32],           // Ephemeral public key (shared)
    message: [u8]              // Encrypted payload (shared)
}

PerRecipientData {
    recipient: Recipient,                    // ServiceId (17 bytes)
    devices: [DeviceList],                   // Device IDs + registration IDs
    c: [u8; 32],                            // Encrypted M for this recipient
    at: [u8; 16],                           // Authentication tag
}

DeviceList {
    device_id: u8,                          // 1 byte device ID
    has_more: u1,                           // High bit of next field
    unused: u1,
    registration_id: u14,                   // 14-bit registration ID
}

Recipient {
    service_id_fixed_width_binary: [u8; 17],  // ServiceId bytes
}
```

### 7.4 Efficiency Analysis

For N recipients:

**v1 (naive):**
- Encryptions: 2N (ephemeral + static per recipient)
- DH operations: 2N
- Wire overhead: N × (version + ephemeral_pub + 2 ciphertexts)

**v2 (optimized):**
- Encryptions: 1 (shared ciphertext)
- DH operations: 2N (but simpler: XOR instead of AES)
- Wire overhead: 1 × (version + ephemeral_pub + ciphertext) + N × (header)

**Savings for group of 100:**
- Ciphertext size: 1× instead of 100×
- Computation: ~50% reduction (shared AES-GCM-SIV encryption)

---

## 8. Decryption Flow: Unwrapping the Layers

### 8.1 Top-Level Decryption Entry Point

**File: `rust/protocol/src/sealed_sender.rs:2000-2079`**

```rust
pub async fn sealed_sender_decrypt(
    ciphertext: &[u8],
    trust_root: &PublicKey,
    timestamp: Timestamp,
    local_e164: Option<String>,
    local_uuid: String,
    local_device_id: DeviceId,
    identity_store: &mut dyn IdentityKeyStore,
    session_store: &mut dyn SessionStore,
    pre_key_store: &mut dyn PreKeyStore,
    signed_pre_key_store: &dyn SignedPreKeyStore,
    kyber_pre_key_store: &mut dyn KyberPreKeyStore,
) -> Result<SealedSenderDecryptionResult> {
    // Step 1: Decrypt to UnidentifiedSenderMessageContent (extracts sender cert)
    let usmc = sealed_sender_decrypt_to_usmc(ciphertext, identity_store).await?;

    // Step 2: Validate sender certificate
    if !usmc.sender()?.validate(trust_root, timestamp)? {
        return Err(SignalProtocolError::InvalidSealedSenderMessage(
            "trust root validation failed".to_string(),
        ));
    }

    // Step 3: Detect self-sends
    let is_local_uuid = local_uuid == usmc.sender()?.sender_uuid()?;
    let is_local_e164 = match (local_e164, usmc.sender()?.sender_e164()?) {
        (Some(l), Some(s)) => l == s,
        _ => false,
    };

    if (is_local_e164 || is_local_uuid) && usmc.sender()?.sender_device_id()? == local_device_id {
        return Err(SignalProtocolError::SealedSenderSelfSend);
    }

    // Step 4: Decrypt inner message using Double Ratchet
    let remote_address = ProtocolAddress::new(
        usmc.sender()?.sender_uuid()?.to_string(),
        usmc.sender()?.sender_device_id()?,
    );

    let message = match usmc.msg_type()? {
        CiphertextMessageType::Whisper => {
            let ctext = SignalMessage::try_from(usmc.contents()?)?;
            session_cipher::message_decrypt_signal(
                &ctext,
                &remote_address,
                session_store,
                identity_store,
                &mut rng,
            )
            .await?
        }
        CiphertextMessageType::PreKey => {
            let ctext = PreKeySignalMessage::try_from(usmc.contents()?)?;
            session_cipher::message_decrypt_prekey(
                &ctext,
                &remote_address,
                session_store,
                identity_store,
                pre_key_store,
                signed_pre_key_store,
                kyber_pre_key_store,
                &mut rng,
            )
            .await?
        }
        msg_type => {
            return Err(SignalProtocolError::InvalidMessage(
                msg_type,
                "unexpected message type for sealed_sender_decrypt",
            ));
        }
    };

    Ok(SealedSenderDecryptionResult {
        sender_uuid: usmc.sender()?.sender_uuid()?.to_string(),
        sender_e164: usmc.sender()?.sender_e164()?.map(|s| s.to_string()),
        device_id: usmc.sender()?.sender_device_id()?,
        message,
    })
}
```

### 8.2 Version Detection and Parsing

**File: `rust/protocol/src/sealed_sender.rs:636-697`**

```rust
impl<'a> UnidentifiedSenderMessage<'a> {
    fn deserialize(data: &'a [u8]) -> Result<Self> {
        let (version_byte, remaining) = data.split_first().ok_or_else(|| {
            SignalProtocolError::InvalidSealedSenderMessage("Message was empty".to_owned())
        })?;

        let version = version_byte >> 4;  // Extract required version
        log::debug!("deserializing UnidentifiedSenderMessage with version {version}");

        match version {
            0 | SEALED_SENDER_V1_MAJOR_VERSION => {
                // v1: Parse protobuf
                let pb = proto::sealed_sender::UnidentifiedSenderMessage::decode(remaining)
                    .map_err(|_| SignalProtocolError::InvalidProtobufEncoding)?;

                let ephemeral_public = PublicKey::try_from(
                    &pb.ephemeral_public
                        .ok_or(SignalProtocolError::InvalidProtobufEncoding)?[..]
                )?;
                let encrypted_static = pb.encrypted_static
                    .ok_or(SignalProtocolError::InvalidProtobufEncoding)?;
                let encrypted_message = pb.encrypted_message
                    .ok_or(SignalProtocolError::InvalidProtobufEncoding)?;

                Ok(Self::V1 {
                    ephemeral_public,
                    encrypted_static,
                    encrypted_message,
                })
            }
            SEALED_SENDER_V2_MAJOR_VERSION => {
                // v2: Parse flat binary format
                #[derive(FromBytes, Immutable, KnownLayout)]
                #[repr(C, packed)]
                struct PrefixRepr {
                    encrypted_message_key: [u8; sealed_sender_v2::MESSAGE_KEY_LEN],
                    encrypted_authentication_tag: [u8; sealed_sender_v2::AUTH_TAG_LEN],
                    ephemeral_public: [u8; sealed_sender_v2::PUBLIC_KEY_LEN],
                }

                let (prefix, encrypted_message) =
                    zerocopy::Ref::<_, PrefixRepr>::from_prefix(remaining)
                        .map_err(|_| SignalProtocolError::InvalidProtobufEncoding)?;

                let PrefixRepr {
                    encrypted_message_key,
                    encrypted_authentication_tag,
                    ephemeral_public,
                } = zerocopy::Ref::into_ref(prefix);

                Ok(Self::V2 {
                    ephemeral_public: PublicKey::from_djb_public_key_bytes(ephemeral_public)?,
                    encrypted_message_key,
                    authentication_tag: encrypted_authentication_tag,
                    encrypted_message,
                })
            }
            _ => Err(SignalProtocolError::UnknownSealedSenderVersion(version)),
        }
    }
}
```

### 8.3 V1 Decryption: Layer by Layer

**File: `rust/protocol/src/sealed_sender.rs:1847-1909`**

```rust
match UnidentifiedSenderMessage::deserialize(ciphertext)? {
    UnidentifiedSenderMessage::V1 {
        ephemeral_public,
        encrypted_static,
        encrypted_message,
    } => {
        // Layer 1: Decrypt sender's identity key
        let eph_keys = sealed_sender_v1::EphemeralKeys::calculate(
            &our_identity.into(),
            &ephemeral_public,
            Direction::Receiving,
        )?;

        let message_key_bytes = crypto::aes256_ctr_hmacsha256_decrypt(
            &encrypted_static,
            &eph_keys.cipher_key,
            &eph_keys.mac_key,
        )
        .map_err(|crypto::DecryptionError::BadCiphertext(msg)| {
            log::error!("failed to decrypt sealed sender v1 message key: {msg}");
            SignalProtocolError::InvalidSealedSenderMessage(
                "failed to decrypt sealed sender v1 message key".to_owned(),
            )
        })?;

        let static_key = PublicKey::try_from(&message_key_bytes[..])?;

        // Layer 2: Decrypt message content
        let static_keys = sealed_sender_v1::StaticKeys::calculate(
            &our_identity,
            &static_key,
            &eph_keys.chain_key,
            &encrypted_static,
        )?;

        let message_bytes = crypto::aes256_ctr_hmacsha256_decrypt(
            &encrypted_message,
            &static_keys.cipher_key,
            &static_keys.mac_key,
        )
        .map_err(|crypto::DecryptionError::BadCiphertext(msg)| {
            log::error!("failed to decrypt sealed sender v1 message contents: {msg}");
            SignalProtocolError::InvalidSealedSenderMessage(
                "failed to decrypt sealed sender v1 message contents".to_owned(),
            )
        })?;

        let usmc = UnidentifiedSenderMessageContent::deserialize(&message_bytes)?;

        // Verify sender identity matches certificate (constant-time)
        if !bool::from(message_key_bytes.ct_eq(&usmc.sender()?.key()?.serialize())) {
            return Err(SignalProtocolError::InvalidSealedSenderMessage(
                "sender certificate key does not match message key".to_string(),
            ));
        }

        Ok(usmc)
    }
    // ... v2 case follows
}
```

### 8.4 V2 Decryption: Recover M and Verify

**File: `rust/protocol/src/sealed_sender.rs:1911-1963`**

```rust
    UnidentifiedSenderMessage::V2 {
        ephemeral_public,
        encrypted_message_key,
        authentication_tag,
        encrypted_message,
    } => {
        // Step 1: Recover M by XOR with DH-derived key
        let m = sealed_sender_v2::apply_agreement_xor(
            &our_identity.into(),
            &ephemeral_public,
            Direction::Receiving,
            encrypted_message_key,
        )?;

        // Step 2: Re-derive keys from M
        let keys = sealed_sender_v2::DerivedKeys::new(&m);

        // Step 3: Verify ephemeral key (constant-time)
        if !bool::from(keys.derive_e().public_key.ct_eq(&ephemeral_public)) {
            return Err(SignalProtocolError::InvalidSealedSenderMessage(
                "derived ephemeral key did not match key provided in message".to_string(),
            ));
        }

        // Step 4: Decrypt message with AES-GCM-SIV
        let mut message_bytes = Vec::from(encrypted_message);
        Aes256GcmSiv::new(&keys.derive_k().into())
            .decrypt_in_place(
                &aes_gcm_siv::Nonce::default(),
                &[],
                &mut message_bytes,
            )
            .map_err(|err| {
                SignalProtocolError::InvalidSealedSenderMessage(format!(
                    "failed to decrypt inner message: {err}"
                ))
            })?;

        let usmc = UnidentifiedSenderMessageContent::deserialize(&message_bytes)?;

        // Step 5: Verify authentication tag (constant-time)
        let at = sealed_sender_v2::compute_authentication_tag(
            &our_identity,
            &usmc.sender()?.key()?.into(),
            Direction::Receiving,
            &ephemeral_public,
            encrypted_message_key,
        )?;

        if !bool::from(authentication_tag.ct_eq(&at)) {
            return Err(SignalProtocolError::InvalidSealedSenderMessage(
                "sender certificate key does not match authentication tag".to_string(),
            ));
        }

        Ok(usmc)
    }
```

**Critical security check:** The authentication tag binds sender's identity without revealing it during decryption. Only after successfully decrypting can we compute the expected tag and verify sender.

---

## 9. Security Analysis and Migration Path

### 9.1 Security Properties Summary

| Property | v1 | v2 | Notes |
|----------|----|----|-------|
| **Sender anonymity** | ✓ | ✓ | Server cannot determine sender |
| **Forward secrecy** | ✓ | ✓ | Ephemeral keys protect past messages |
| **Sender authentication** | ✓ | ✓ | Recipient can verify sender via certificate |
| **Replay protection** | ✓ | ✓ | Via Double Ratchet message numbers |
| **Multi-recipient efficiency** | ✗ | ✓ | v2 shares ciphertext across recipients |
| **Constant-time validation** | ✓ | ✓ | Prevents timing side-channels |

### 9.2 Attack Resistance

**Metadata analysis attacks:**
- ✓ Server cannot correlate sender across messages
- ✓ Ephemeral keys prevent long-term tracking
- ✗ Message size and timing still visible (unavoidable)

**Cryptographic attacks:**
- ✓ Authentication tag prevents sender forgery
- ✓ AEAD (v2) provides ciphertext integrity
- ✓ Constant-time comparisons prevent timing attacks

**Certificate-based attacks:**
- ✓ Expiration limits compromise window
- ✓ Revocation via server certificate ID blacklist
- ✗ Server can issue fake certificates (trusted party model)

### 9.3 Performance Characteristics

**v1 encryption (per recipient):**
- 2 DH operations (ephemeral + static)
- 2 HKDF derivations
- 2 AES-CTR encryptions + HMAC

**v2 encryption (multi-recipient):**
- 1 AES-GCM-SIV encryption (shared)
- Per recipient: 1 ephemeral DH + 1 static DH
- ~50% faster for groups of 10+

**Memory:**
- v1: O(1) per message
- v2: O(N) for per-recipient headers (but shared ciphertext)

### 9.4 Migration Strategy v1 → v2

**Backward compatibility:**
```rust
match version {
    0 | SEALED_SENDER_V1_MAJOR_VERSION => { /* v1 decryption */ }
    SEALED_SENDER_V2_MAJOR_VERSION => { /* v2 decryption */ }
    _ => Err(SignalProtocolError::UnknownSealedSenderVersion(version)),
}
```

**Client upgrade path:**
1. Deploy v2-capable clients (can receive v1 or v2)
2. Monitor adoption metrics
3. Enable v2 sending for group messages
4. Eventually deprecate v1 (requires 100% client upgrade)

**Why gradual migration works:**
- Version byte allows runtime detection
- Clients can send v1 to old peers, v2 to upgraded groups
- No flag day required

### 9.5 Known Limitations

**Not addressed by Sealed Sender:**
1. **Recipient identity**: Still visible for routing
2. **Timing patterns**: Message timing metadata visible
3. **Size channels**: Message size reveals information
4. **Server trust**: Server can issue fake certificates

**Complementary techniques:**
- **Padding**: Normalize message sizes
- **Delayed delivery**: Break timing correlation
- **Cover traffic**: Send dummy messages
- **PIR**: Private Information Retrieval for recipient lookup

### 9.6 Future Directions

**Potential improvements:**
1. **Abuse resistance**: Prevent spam while maintaining anonymity
2. **Sealed sender groups**: Hide group membership from server
3. **Anonymous credentials**: Replace certificates with zero-knowledge proofs
4. **Post-quantum security**: Hybrid ephemeral keys (X25519 + Kyber)

---

## Conclusion

Sealed Sender represents a significant advancement in messaging privacy, moving beyond content encryption to protect metadata. The evolution from v1 to v2 demonstrates the challenge of balancing security, efficiency, and deployability in production systems.

**Key takeaways:**
- **Two-layer encryption** separates routing (ephemeral) from authentication (static)
- **Certificates** provide accountable anonymity (server-issued but client-validated)
- **Multi-recipient optimization** (v2) makes sealed sender practical for groups
- **Constant-time operations** prevent timing side-channels throughout

The implementation showcases careful cryptographic engineering: from the HKDF labels preventing cross-protocol attacks, to the constant-time comparisons preventing timing leaks, to the lazy certificate loading optimizing runtime performance.

**Further reading:**
- Signal blog: [Sealed Sender for Signal](https://signal.org/blog/sealed-sender/)
- Barbosa paper: [Randomness Reuse: Extensions and Improvements](https://haslab.uminho.pt/mbb/files/reuse.pdf)
- libsignal tests: `rust/protocol/tests/sealed_sender.rs`

---

*This chapter is part of the libsignal Encyclopedia. See [Table of Contents](01-TABLE-OF-CONTENTS.md) for related chapters on session establishment, message encryption, and cryptographic primitives.*
