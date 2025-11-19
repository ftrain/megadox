# Comprehensive Glossary
## The libsignal Encyclopedia

---

## A

**AEAD (Authenticated Encryption with Associated Data)**
: A cryptographic scheme that provides both confidentiality and authenticity. Examples include AES-GCM and AES-GCM-SIV. Allows additional data to be authenticated without encryption.
: *See*: AES-GCM, AES-GCM-SIV, ChaCha20-Poly1305

**AES (Advanced Encryption Standard)**
: A symmetric block cipher standardized by NIST in 2001. libsignal uses AES-256 (256-bit keys) in various modes: CBC, CTR, GCM, and GCM-SIV.
: *Implementation*: `rust/crypto/src/aes_*.rs`

**AES-CBC (AES Cipher Block Chaining)**
: Block cipher mode requiring an initialization vector (IV). Used in older Signal Protocol message encryption. Requires padding (PKCS7 in libsignal).
: *Security Note*: Vulnerable to padding oracle attacks if not implemented carefully.

**AES-CTR (AES Counter Mode)**
: Streaming cipher mode that converts AES into a stream cipher. Used in some libsignal components combined with HMAC-SHA256 for authentication.
: *Property*: Allows parallel encryption/decryption.

**AES-GCM (AES Galois/Counter Mode)**
: AEAD mode combining CTR mode encryption with GMAC authentication. Standard choice for modern authenticated encryption.
: *Implementation*: `rust/crypto/src/aes_gcm.rs`
: *Test Vectors*: `rust/crypto/tests/data/aes_gcm_test.json` (256 test cases)

**AES-GCM-SIV (Synthetic IV)**
: Nonce-misuse resistant AEAD. Even if the same nonce is reused, security degrades gracefully. Used in Sealed Sender v2.
: *Advantage*: Critical for systems where nonce generation might fail.

**Alternate Identity**
: A secondary identity key (e.g., for Phone Number Identity vs ACI - Account Identifier). Allows users to have multiple identity keys with domain separation.
: *Implementation*: `rust/protocol/src/identity_key.rs:verify_alternate_identity()`

**ARMv7 / ARMv8**
: ARM processor architectures. ARMv7 is 32-bit (common in 2013-2015 phones), ARMv8 is 64-bit with crypto extensions. Crypto extensions dramatically improve AES and SHA performance.

**Asynchronous Protocol**
: A messaging protocol that doesn't require both parties to be online simultaneously. Signal Protocol is asynchronous via PreKeys.

**Attestation**
: Cryptographic proof that code is running in a trusted environment (e.g., Intel SGX enclave). Used in CDSI and SVR to prove server software integrity.
: *Implementation*: `rust/attest/`

**Axolotl**
: Original name of the Signal Protocol (2014). Later renamed to avoid confusion. Named after the axolotl salamander, which can regenerate (like protocol keys regenerate).
: *Superseded by*: Signal Protocol (same protocol, renamed)

---

## B

**Backup Key**
: 32-byte key derived from user's PIN or passphrase, used to encrypt message backups. Never sent to servers.
: *Derivation*: Argon2 key derivation from PIN
: *Related*: Message Backup, SVR

**Base64**
: Encoding scheme to represent binary data in ASCII. Used for fingerprint display and some serialization.

**BoringSSL**
: Google's fork of OpenSSL, used by libsignal for crypto operations (via `boring` Rust crate). Chosen for performance and platform support.
: *Version*: Custom fork maintained by Signal (signal-v4.18.0)

**Bridge**
: The FFI/JNI/Neon layer that exposes Rust code to Java, Swift, or Node.js. Uses procedural macros for code generation.
: *Architecture*: `rust/bridge/`
: *Macros*: `#[bridge_fn]`, `#[bridge_io]`

---

## C

**cbindgen**
: Tool that generates C/C++ headers from Rust code. Used to create headers for Swift FFI bridge.
: *Config*: `rust/bridge/ffi/cbindgen.toml`
: *Output*: `swift/Sources/SignalFfi/signal_ffi.h`

**CDSI (Contact Discovery Service Interface)**
: Privacy-preserving contact discovery using SGX enclaves. Allows finding which contacts use Signal without revealing your contact list to the server.
: *Predecessor*: CDS2
: *Implementation*: `rust/net/src/cdsi.rs`

**Chain Key**
: Key in the symmetric-key ratchet that's advanced for each message. Derives message keys via KDF and then advances to next chain key.
: *Formula*: `ChainKey(n+1) = HMAC-SHA256(ChainKey(n), 0x02)`
: *Implementation*: `rust/protocol/src/ratchet/keys.rs`

**ChaCha20-Poly1305**
: AEAD cipher using ChaCha20 stream cipher and Poly1305 MAC. Used in Sealed Sender v2. Alternative to AES-GCM with better software performance on devices without AES-NI.

**ciphertext**
: Encrypted data. In Signal Protocol, refers to the encrypted message body.
: *Counterpart*: plaintext

**CRYSTALS-Kyber**
: Post-quantum key encapsulation mechanism, finalist in NIST PQC competition. Standardized as ML-KEM. Used in PQXDH.
: *Key Sizes*: Kyber768 (smaller), Kyber1024 (higher security, used by Signal)
: *Implementation*: Via libcrux-ml-kem

**Curve25519**
: Elliptic curve designed by Daniel J. Bernstein for Diffie-Hellman key exchange (X25519) and signatures (Ed25519). Chosen for performance and security margin.
: *Field*: 2^255 - 19
: *Security*: ~128-bit security level
: *Implementation*: `rust/core/src/curve/curve25519.rs`

---

## D

**DCAP (Data Center Attestation Primitives)**
: Intel's attestation framework for SGX enclaves in data centers. Used to verify CDSI and SVR enclaves.
: *Implementation*: `rust/attest/src/dcap.rs`

**Deniability**
: Property where participants can deny having sent a message (no unforgeable signatures). Signal Protocol provides cryptographic deniability.

**DH (Diffie-Hellman)**
: Key agreement protocol allowing two parties to establish a shared secret over an insecure channel.
: *In Signal*: X25519 variant of DH on Curve25519

**Double Ratchet**
: Core Signal Protocol mechanism providing forward secrecy and self-healing. Combines:
  1. **Symmetric-Key Ratchet**: Advances chain keys
  2. **Diffie-Hellman Ratchet**: Periodically performs new DH exchanges
: *Specification*: https://signal.org/docs/specifications/doubleratchet/
: *Implementation*: `rust/protocol/src/ratchet.rs`

---

## E

**ECDH (Elliptic Curve Diffie-Hellman)**
: Diffie-Hellman using elliptic curve cryptography. More efficient than traditional DH.
: *Signal's Choice*: X25519

**Ed25519**
: EdDSA signature scheme using Curve25519 (twisted Edwards form). Used for identity key signatures.
: *Signature Size*: 64 bytes
: *Public Key Size*: 32 bytes

**Enclave**
: Secure execution environment (e.g., Intel SGX, ARM TrustZone). Code and data inside are protected from the host OS.
: *Use in Signal*: CDSI, SVR

**Endorsement**
: Zero-knowledge credential allowing group actions without revealing identity. Part of zkgroup system.
: *Implementation*: `rust/zkgroup/` and `rust/zkcredential/`

**Ephemeral Key**
: Short-lived cryptographic key, typically used for a single session or message. Provides forward secrecy.

---

## F

**FFI (Foreign Function Interface)**
: Mechanism for calling functions across language boundaries. Swift bridge uses C FFI.
: *Implementation*: `rust/bridge/ffi/`

**Fingerprint**
: Human-readable representation of a public key for verification. Signal uses safety numbers (6 groups of 5 digits) or QR codes.
: *Types*:
  - **Displayable**: Numeric string
  - **Scannable**: QR code with protobuf encoding
: *Implementation*: `rust/protocol/src/fingerprint.rs`

**Forward Secrecy**
: Property ensuring past messages remain secret even if long-term keys are compromised. Achieved through ephemeral keys and ratcheting.

**Fuzz Testing**
: Testing technique using semi-random input to find bugs. libsignal uses libfuzzer.
: *Targets*: `rust/protocol/fuzz/` and `rust/attest/fuzz/`

---

## G

**GHASH**
: Authentication component of GCM mode. Galois field multiplication-based MAC.
: *Implementation*: `ghash` crate

**Group Send Endorsement**
: Zero-knowledge proof allowing group message sending without revealing sender identity to server.
: *Related*: zkgroup

---

## H

**HKDF (HMAC-based Key Derivation Function)**
: Standard key derivation function (RFC 5869). Takes input key material and derives multiple keys.
: *Formula*: `HKDF(salt, IKM, info, length) → OKM`
: *Usage in Signal*: Deriving root keys, chain keys, message keys
: *Implementation*: `hkdf` crate

**HMAC (Hash-based Message Authentication Code)**
: MAC construction using cryptographic hash function.
: *Signal's Choice*: HMAC-SHA256 (32-byte output)
: *Usage*: Chain key advancement, message authentication

**HPKE (Hybrid Public Key Encryption)**
: RFC 9180 standard combining KEM, KDF, and AEAD. Used in Sealed Sender.
: *Signal's Suite*: DHKEM(X25519) + HKDF-SHA256 + AES-256-GCM
: *Implementation*: `rust/crypto/src/hpke.rs`

**HSM (Hardware Security Module)**
: Physical device for managing cryptographic keys. Some Signal infrastructure uses HSMs with attestation.

---

## I

**Identity Key**
: Long-term public key identifying a user/device. Unlike fingerprints, but can be verified via fingerprints (safety numbers).
: *Lifetime*: Permanent until device reset/reinstall
: *Type*: Curve25519 public key
: *Implementation*: `rust/protocol/src/identity_key.rs`

**Incremental MAC**
: MAC computed over chunks of data, allowing streaming verification of large files.
: *Use Case*: Message backups
: *Implementation*: `rust/protocol/src/incremental_mac.rs`

---

## J

**JNI (Java Native Interface)**
: Java's FFI for calling native code. Used by libsignal's Java bindings.
: *Implementation*: `rust/bridge/jni/`
: *Entry Points*: `Java_org_signal_libsignal_internal_Native_*`

---

## K

**KDF (Key Derivation Function)**
: Function that derives cryptographic keys from source material.
: *Signal's Choices*: HKDF, HMAC (for chain keys)

**KEM (Key Encapsulation Mechanism)**
: Public-key encryption designed for encapsulating symmetric keys. Returns (ciphertext, shared_secret) for sender and shared_secret for receiver.
: *Examples*: Kyber1024, ML-KEM1024
: *Implementation*: `rust/protocol/src/kem/`

**Key Transparency**
: Public log of user keys enabling detection of malicious key changes. Uses Merkle trees and VRFs.
: *Status*: In development for Signal
: *Implementation*: `rust/keytrans/`

**Kyber** → See CRYSTALS-Kyber

---

## L

**libcrux**
: Formally verified cryptography library. Signal uses it for ML-KEM (Kyber) implementation.
: *Verification*: Proven correct using F* formal methods
: *Migration*: Replaced pqcrypto crate in 2024

**libsignal-net**
: Network services library for Signal, including CDSI, SVR, Chat, and infrastructure.
: *Path*: `rust/net/`
: *Subcrates*: infra, chat, grpc

**Linkme**
: Rust library for distributed slices (compile-time registration). Used by bridge to collect all bridged functions.
: *Pattern*: `#[distributed_slice]` for automatic function registration

---

## M

**MAC (Message Authentication Code)**
: Cryptographic checksum proving message authenticity and integrity.
: *Signal's Usage*: HMAC-SHA256 for messages (8-byte truncated)

**Message Key**
: Symmetric key derived from chain key, used to encrypt exactly one message.
: *Derivation*: `MessageKey = HKDF(ChainKey, "WhisperText" || version)`
: *Components*: Cipher Key (32 bytes) + MAC Key (32 bytes) + IV (16 bytes)

**Merkle Tree**
: Tree structure where each node is the hash of its children. Used in key transparency.
: *Property*: Efficiently proves membership

**ML-KEM (Module-Lattice Key Encapsulation Mechanism)**
: NIST-standardized version of CRYSTALS-Kyber. Post-quantum KEM.
: *Standard*: FIPS 203
: *Signal's Variant*: ML-KEM-1024

**Monorepo**
: Repository containing multiple projects/crates. libsignal consolidated from separate repos in 2020.
: *Structure*: Cargo workspace with 24 crates

---

## N

**Neon**
: Rust framework for building native Node.js addons. Used by libsignal's Node.js bridge.
: *Features*: N-API bindings, async support, type-safe JS value conversion
: *Implementation*: `rust/bridge/node/`

**NIST (National Institute of Standards and Technology)**
: US standards body. Standardized AES, SHA-2, and post-quantum algorithms (ML-KEM, ML-DSA).

**Noise Protocol**
: Framework for building crypto protocols with various handshake patterns. Signal uses Noise for some network connections (CDSI, SVR).
: *Implementation*: `snow` crate

**Nonce**
: Number used once. Critical for many crypto schemes (GCM requires unique nonces).
: *Misuse*: Can completely break security
: *GCM-SIV*: Nonce-misuse resistant

---

## O

**Oblivious**
: Property where server cannot determine what client is requesting. Used in CDSI.

**One-Time PreKey**
: PreKey used for exactly one session establishment, then deleted. Provides forward secrecy against compromise.

**OPRF (Oblivious Pseudorandom Function)**
: PRF protocol where server computes PRF without learning the input. Used in SVR for PIN-based recovery.

**OTR (Off-the-Record Messaging)**
: Earlier encrypted messaging protocol (2004). Signal Protocol improves upon OTR with asynchronous support.

---

## P

**Padding**
: Extra bytes added to meet block size or hide message length.
: *PKCS7*: Standard padding for block ciphers
: *Length Hiding*: Optional padding to obscure actual message size

**poksho (Proof of Knowledge, Stateful Hash Object)**
: Library for Schnorr-style zero-knowledge proofs. Foundation of zkgroup.
: *Implementation*: `rust/poksho/`
: *Technique*: SHO (sponge hash object) for challenge generation

**PQXDH (Post-Quantum Extended Diffie-Hellman)**
: Signal's post-quantum session establishment protocol. Combines X25519 and Kyber1024 for hybrid security.
: *Announcement*: September 2023
: *Specification*: https://signal.org/docs/specifications/pqxdh/
: *Implementation*: `rust/protocol/src/session.rs`

**PreKey**
: Public key uploaded to server before communication. Enables asynchronous messaging.
: *Types*:
  - **Signed PreKey**: Long-lived, signed by identity key
  - **One-Time PreKey**: Single-use
  - **Kyber PreKey**: Post-quantum KEM public key

**PreKey Bundle**
: Collection of public keys needed for session establishment (identity key, signed prekey, one-time prekey, kyber prekey).
: *Implementation*: `rust/protocol/src/state/bundle.rs`

**Profile Key**
: Key controlling access to user profile information. Used in zkgroup to prove possession without revealing the key.

**Proptest**
: Property-based testing library for Rust. Generates random inputs to test invariants.
: *Usage*: Extensive use in libsignal-net, protocol, usernames

**Protobuf (Protocol Buffers)**
: Google's serialization format. Used for Signal Protocol messages and storage.
: *Tool*: `prost` for Rust
: *Definitions*: `*.proto` files compiled by build.rs

---

## Q

**QR Code**
: 2D barcode encoding data. Used for scannable fingerprints and device linking.

**Quantum Computer**
: Computer leveraging quantum mechanics for computation. Threatens current public-key cryptography (Shor's algorithm).
: *Signal's Response*: PQXDH, SPQR (post-quantum protocols)

---

## R

**Ratchet**
: Key evolution mechanism providing forward secrecy. "Ratcheting" means keys only move forward, never backward.
: *Types in Signal*:
  - **Symmetric-Key Ratchet**: Chain key advancement
  - **DH Ratchet**: Periodic DH exchanges
  - **Post-Quantum Ratchet**: SPQR

**Receiver Chain**
: State for receiving messages from a particular sending DH ratchet key. Multiple receiver chains stored for out-of-order messages.

**Ristretto**
: Technique for using Curve25519 in prime-order group. Used in zkgroup for Schnorr proofs.
: *Implementation*: `curve25519-dalek` crate

**Root Key**
: Master key in Double Ratchet that derives chain keys after each DH ratchet step.
: *Derivation*: `RootKey, ChainKey = HKDF(RootKey, DH_output, "WhisperText")`

**RustCrypto**
: Collection of pure-Rust cryptographic implementations. Signal uses various RustCrypto crates (aes, sha2, hmac, etc.).

---

## S

**Safety Number**
: User-facing term for fingerprint. 60-digit number (6 groups of 5 digits) for manual verification.

**Schnorr Signature**
: Signature scheme using discrete log. Basis for zkgroup proofs.
: *Advantage*: Enables zero-knowledge proofs

**Sealed Sender**
: Encryption mode hiding sender identity from server. Only recipient can decrypt sender info.
: *Versions*:
  - **v1**: AESGCM-based
  - **v2**: ChaCha20-Poly1305, optimized structure
: *Implementation*: `rust/protocol/src/sealed_sender.rs`

**Sender Chain**
: State for sending messages with current DH ratchet key and chain key.

**Sender Key**
: Symmetric key shared within a group for efficient group messaging. Distributed via Sender Key Distribution Message (SKDM).
: *Rotation*: New sender keys generated periodically or when members change
: *Implementation*: `rust/protocol/src/sender_keys.rs`

**Server Certificate**
: Certificate proving server authenticity in Sealed Sender. Contains server public key signed by trust root.

**Session**
: Cryptographic session between two parties. Contains Double Ratchet state.
: *Storage*: SessionRecord serialized to protobuf
: *Implementation*: `rust/protocol/src/state/session.rs`

**SGX (Software Guard Extensions)**
: Intel CPU feature creating secure enclaves. Used by CDSI and SVR.
: *Attestation*: Remote attestation proves enclave code integrity

**SHA-256 / SHA-512**
: Cryptographic hash functions from SHA-2 family.
: *Output Sizes*: 256 bits (32 bytes) and 512 bits (64 bytes)
: *Usage*: HMAC, HKDF, signatures

**SHO (Stateful Hash Object)**
: Sponge construction for hash-based random oracles. Used in poksho for proof generation.
: *Variants*: HMAC-SHA256-based, SHA256-based

**Signal Foundation**
: 501(c)(3) nonprofit supporting Signal development. Founded 2018 with $50M from Brian Acton.

**Signal Protocol**
: End-to-end encryption protocol combining X3DH/PQXDH and Double Ratchet. Powers Signal, WhatsApp, Facebook Messenger, Google Messages.
: *Previous Names*: Axolotl, TextSecure Protocol
: *Standardization*: Open specification, academic analysis

**Signed PreKey**
: Medium-lived PreKey (rotated weekly/monthly) signed by identity key to prove authenticity.

**SPQR (Signal Post-Quantum Ratchet)**
: Post-quantum extension to Double Ratchet providing PQ forward secrecy.
: *Integration*: Added to SignalMessage as `pq_ratchet` field
: *Mandatory*: As of October 2024

**SVR (Secure Value Recovery)**
: Service for backing up secrets (like encryption keys) using SGX enclaves.
: *Versions**:
  - **SVR2**: PIN-based, OPRF
  - **SVR3**: Raft consensus
  - **SVR-B**: Next generation

---

## T

**TCB (Trusted Computing Base)**
: The set of hardware/software that must be trusted for security. SGX aims to minimize TCB.

**TextSecure**
: Original Android app (2010-2015) that became Signal. Also refers to the early protocol.

**tokio**
: Async runtime for Rust. Used throughout libsignal-net for networking.
: *Features*: Multi-threaded runtime, async I/O, timers

**Triple Ratchet** → See X3DH

**Trust on First Use (TOFU)**
: Trust model where first key encountered is trusted. Signal adds safety number verification on top of TOFU.

**Type Tagging**
: Prefixing serialized data with a type byte. Curve25519 public keys use 0x05.

---

## U

**Unidentified Sender** → See Sealed Sender

**Username**
: User-chosen identifier (alternative to phone number). Signal uses hashed usernames for privacy.
: *Format*: nickname.discriminator (e.g., "alice.42")
: *Implementation*: `rust/usernames/`

---

## V

**VRF (Verifiable Random Function)**
: Cryptographic function proving output is correctly computed. Used in key transparency for monitoring.

---

## W

**WebSocket**
: Protocol for bidirectional communication over HTTP. Used in Signal's chat service.
: *Implementation*: `tungstenite` crate (Signal fork)

**WhatsApp**
: Messaging app owned by Meta. Adopted Signal Protocol in 2014-2016, rolled out encryption to 1+ billion users.

**Workspace**
: Cargo feature for managing multiple related crates. libsignal is a workspace with 24 member crates.
: *Config*: `Cargo.toml` at repository root

---

## X

**X25519**
: Diffie-Hellman function using Curve25519 (Montgomery form). Used for key agreement in Signal Protocol.
: *Key Size*: 32 bytes
: *Shared Secret Size*: 32 bytes
: *Implementation*: `x25519-dalek` crate

**X3DH (Extended Triple Diffie-Hellman)**
: Original Signal session establishment protocol. Performs 4 DH operations for forward secrecy and deniability.
: *Superseded by*: PQXDH (adds Kyber)
: *Specification*: https://signal.org/docs/specifications/x3dh/

**XEdDSA**
: Signature scheme converting X25519 keys to Ed25519 form for signing. Allows same key for DH and signatures.
: *Usage*: Identity key signatures
: *Implementation*: `rust/core/src/curve/curve25519.rs`

---

## Z

**Zero-Knowledge Proof**
: Cryptographic proof revealing nothing except the truth of a statement.
: *Example*: Prove you're in a group without revealing which member

**zkgroup**
: Signal's zero-knowledge group system. Enables group operations without server learning group membership.
: *Components*:
  - **Profile Key Credentials**: Prove profile key possession
  - **Receipt Credentials**: Prove payment/subscription
  - **Group Send Endorsements**: Prove group membership
: *Implementation*: `rust/zkgroup/`

**zkcredential**
: Generic zero-knowledge credential system abstracted from zkgroup specifics.
: *Implementation*: `rust/zkcredential/`

---

## Acronyms Quick Reference

- **AEAD**: Authenticated Encryption with Associated Data
- **AES**: Advanced Encryption Standard
- **API**: Application Programming Interface
- **CBC**: Cipher Block Chaining
- **CDSI**: Contact Discovery Service Interface
- **CI/CD**: Continuous Integration / Continuous Deployment
- **CTR**: Counter Mode
- **DH**: Diffie-Hellman
- **ECDH**: Elliptic Curve Diffie-Hellman
- **FFI**: Foreign Function Interface
- **GCM**: Galois/Counter Mode
- **GHASH**: Galois Hash
- **HKDF**: HMAC-based Key Derivation Function
- **HMAC**: Hash-based Message Authentication Code
- **HPKE**: Hybrid Public Key Encryption
- **HSM**: Hardware Security Module
- **JNI**: Java Native Interface
- **KDF**: Key Derivation Function
- **KEM**: Key Encapsulation Mechanism
- **MAC**: Message Authentication Code
- **ML-KEM**: Module-Lattice Key Encapsulation Mechanism
- **NIST**: National Institute of Standards and Technology
- **OPRF**: Oblivious Pseudorandom Function
- **OTR**: Off-the-Record Messaging
- **PQ**: Post-Quantum
- **PQXDH**: Post-Quantum Extended Diffie-Hellman
- **SGX**: Software Guard Extensions
- **SHA**: Secure Hash Algorithm
- **SHO**: Stateful Hash Object
- **SPQR**: Signal Post-Quantum Ratchet
- **SVR**: Secure Value Recovery
- **TCB**: Trusted Computing Base
- **TOFU**: Trust on First Use
- **VRF**: Verifiable Random Function
- **X3DH**: Extended Triple Diffie-Hellman
- **ZK**: Zero-Knowledge

---

## Symbol Conventions

Throughout this encyclopedia:

- **`→`**: Points to related terms or concepts
- **`*`**: See also / related information
- **`[Term]`**: Link to glossary entry
- **`filename.rs:123`**: Reference to source code location
- **`commit_hash`**: Git commit reference

---

*This glossary contains 100+ terms essential for understanding libsignal.*

