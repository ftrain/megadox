# Chapter 2: Cryptographic Primitives

**Part II: Cryptographic Foundations**

---

## Introduction

The Signal Protocol stands on a foundation of carefully chosen cryptographic primitives, each selected for specific security properties, performance characteristics, and implementation safety. This chapter provides a literate programming tour through libsignal's cryptographic implementations, explaining not just *what* they do but *why* they were chosen and *how* they work together to provide end-to-end encryption.

Unlike many cryptographic libraries that simply wrap existing implementations, libsignal carefully integrates primitives from multiple sources—RustCrypto, curve25519-dalek, libcrux—ensuring constant-time operations, memory safety, and cross-platform consistency.

---

## 2.1 Symmetric Cryptography

Symmetric encryption forms the bulk of Signal's cryptographic operations. While public-key cryptography handles key agreement, the actual message content encryption uses symmetric algorithms for their speed and efficiency.

### 2.1.1 AES-256-CBC: Legacy Compatibility

**Historical Context**: AES-256 in Cipher Block Chaining (CBC) mode was one of the earliest encryption modes used in Signal. While modern implementations prefer authenticated encryption modes like GCM, CBC remains necessary for backward compatibility with older message formats.

**Security Properties**:
- 256-bit keys provide quantum-resistant symmetric security
- CBC mode requires proper IV management (never reuse IVs)
- Requires separate authentication (HMAC)
- Vulnerable to padding oracle attacks if not carefully implemented

**Implementation**: `/home/user/libsignal/rust/crypto/src/aes_cbc.rs`

```rust
use aes::Aes256;
use aes::cipher::block_padding::Pkcs7;
use aes::cipher::{BlockDecryptMut, BlockEncryptMut, KeyIvInit};

pub fn aes_256_cbc_encrypt(
    ptext: &[u8],
    key: &[u8],
    iv: &[u8],
) -> Result<Vec<u8>, EncryptionError> {
    // Create an encryptor from key and IV slices
    // The KeyIvInit trait ensures type-safe key/IV initialization
    Ok(cbc::Encryptor::<Aes256>::new_from_slices(key, iv)
        .map_err(|_| EncryptionError::BadKeyOrIv)?
        .encrypt_padded_vec_mut::<Pkcs7>(ptext))
}
```

The implementation uses RustCrypto's `aes` crate with several safety features:

1. **Type-safe initialization**: `new_from_slices()` validates key and IV lengths at runtime
2. **PKCS#7 padding**: Automatically handles padding to block boundaries
3. **Memory safety**: Rust's ownership prevents use-after-free bugs common in C implementations

**Decryption with validation**:

```rust
pub fn aes_256_cbc_decrypt(
    ctext: &[u8],
    key: &[u8],
    iv: &[u8],
) -> Result<Vec<u8>, DecryptionError> {
    // Validate ciphertext length before attempting decryption
    // Must be a non-zero multiple of 16 (AES block size)
    if ctext.is_empty() || ctext.len() % 16 != 0 {
        return Err(DecryptionError::BadCiphertext(
            "ciphertext length must be a non-zero multiple of 16",
        ));
    }

    cbc::Decryptor::<Aes256>::new_from_slices(key, iv)
        .map_err(|_| DecryptionError::BadKeyOrIv)?
        .decrypt_padded_vec_mut::<Pkcs7>(ctext)
        .map_err(|_| DecryptionError::BadCiphertext("failed to decrypt"))
}
```

**Test Vector** (from the implementation):

```rust
let key = hex!("4e22eb16d964779994222e82192ce9f747da72dc4abe49dfdeeb71d0ffe3796e");
let iv = hex!("6f8a557ddc0a140c878063a6d5f31d3d");
let ptext = hex!("30736294a124482a4159");

let ctext = aes_256_cbc_encrypt(&ptext, &key, &iv);
// Result: "dd3f573ab4508b9ed0e45e0baf5608f3"
```

Note how the 10-byte plaintext expands to 16 bytes due to PKCS#7 padding (6 bytes of padding added).

---

### 2.1.2 AES-256-CTR: Stream Cipher Mode

**Historical Context**: Counter (CTR) mode transforms AES into a stream cipher, allowing parallel encryption/decryption and avoiding padding altogether. Signal uses CTR mode for session message encryption combined with HMAC for authentication.

**Security Properties**:
- No padding required
- Parallelizable encryption/decryption
- Random-access decryption (can decrypt any position)
- **Critical**: Never reuse nonce+counter combinations

**Implementation**: `/home/user/libsignal/rust/crypto/src/aes_ctr.rs`

```rust
use aes::Aes256;
use aes::cipher::{InnerIvInit, KeyInit, StreamCipher, StreamCipherSeek};

/// A wrapper around ctr::Ctr32BE that uses a smaller nonce
/// and supports an initial counter.
pub struct Aes256Ctr32(ctr::Ctr32BE<Aes256>);

impl Aes256Ctr32 {
    // Nonce size: 12 bytes (96 bits)
    // Remaining 4 bytes for 32-bit counter
    pub const NONCE_SIZE: usize = 12;

    pub fn new(aes256: Aes256, nonce: &[u8], init_ctr: u32) -> Result<Self> {
        if nonce.len() != Self::NONCE_SIZE {
            return Err(Error::InvalidNonceSize);
        }

        // Construct full 16-byte IV: 12-byte nonce + 4-byte counter
        let mut nonce_block = [0u8; 16];
        nonce_block[0..Self::NONCE_SIZE].copy_from_slice(nonce);

        let mut ctr = ctr::Ctr32BE::from_core(
            ctr::CtrCore::inner_iv_init(aes256, &nonce_block.into())
        );

        // Seek to initial counter position (for resuming encryption)
        ctr.seek((16 as u64) * (init_ctr as u64));

        Ok(Self(ctr))
    }

    pub fn process(&mut self, buf: &mut [u8]) {
        // In-place encryption/decryption (XOR with keystream)
        self.0.apply_keystream(buf);
    }
}
```

**Key Design Decisions**:

1. **12-byte nonce + 4-byte counter**: Follows NIST SP 800-38A recommendations, allowing 2^32 blocks (64GB) per nonce
2. **Seekable counter**: Enables resuming encryption at any block position
3. **In-place processing**: Memory-efficient, no allocation needed

**Usage in Signal Protocol** (`/home/user/libsignal/rust/protocol/src/crypto.rs`):

```rust
fn aes_256_ctr_encrypt(ptext: &[u8], key: &[u8]) -> Result<Vec<u8>, EncryptionError> {
    let key: [u8; 32] = key.try_into()
        .map_err(|_| EncryptionError::BadKeyOrIv)?;

    let zero_nonce = [0u8; 16];
    let mut cipher = ctr::Ctr32BE::<Aes256>::new(
        key[..].into(),
        zero_nonce[..].into()
    );

    let mut ctext = ptext.to_vec();
    cipher.apply_keystream(&mut ctext);
    Ok(ctext)
}
```

**Authenticated Encryption Wrapper**:

Signal never uses CTR mode alone—it's always combined with HMAC-SHA256:

```rust
pub(crate) fn aes256_ctr_hmacsha256_encrypt(
    msg: &[u8],
    cipher_key: &[u8],
    mac_key: &[u8],
) -> Result<Vec<u8>, EncryptionError> {
    // Encrypt message
    let mut ctext = aes_256_ctr_encrypt(msg, cipher_key)?;

    // Compute MAC over ciphertext (Encrypt-then-MAC)
    let mac = hmac_sha256(mac_key, &ctext);

    // Append truncated MAC (10 bytes for space efficiency)
    ctext.extend_from_slice(&mac[..10]);
    Ok(ctext)
}
```

This implements the **Encrypt-then-MAC** paradigm, considered the safest approach to authenticated encryption.

---

### 2.1.3 AES-256-GCM: Authenticated Encryption

**Historical Context**: Galois/Counter Mode (GCM) combines CTR mode encryption with GMAC authentication in a single primitive. It's the modern standard for authenticated encryption, used extensively in TLS 1.3 and HPKE.

**Security Properties**:
- Authenticated Encryption with Associated Data (AEAD)
- Single primitive for confidentiality + integrity
- Parallelizable
- **Critical weakness**: Catastrophic failure if nonce is reused

**Implementation**: `/home/user/libsignal/rust/crypto/src/aes_gcm.rs`

The implementation is particularly interesting as it's built from components rather than using a pre-packaged AEAD:

```rust
use aes::Aes256;
use ghash::GHash;
use ghash::universal_hash::UniversalHash;

pub const TAG_SIZE: usize = 16;
pub const NONCE_SIZE: usize = 12;

struct GcmGhash {
    ghash: GHash,
    ghash_pad: [u8; TAG_SIZE],
    msg_buf: [u8; TAG_SIZE],
    msg_buf_offset: usize,
    ad_len: usize,  // Associated data length
    msg_len: usize, // Message length
}
```

**GCM Setup Phase**:

```rust
fn setup_gcm(
    key: &[u8],
    nonce: &[u8],
    associated_data: &[u8]
) -> Result<(Aes256Ctr32, GcmGhash)> {
    // GCM standard nonce size is 12 bytes
    if nonce.len() != NONCE_SIZE {
        return Err(Error::InvalidNonceSize);
    }

    let aes256 = Aes256::new_from_slice(key)
        .map_err(|_| Error::InvalidKeySize)?;

    // Compute H = AES(K, 0^128) for GHASH
    let mut h = [0u8; TAG_SIZE];
    aes256.encrypt_block(GenericArray::from_mut_slice(&mut h));

    // Counter starts at 1 for encryption
    let mut ctr = Aes256Ctr32::new(aes256, nonce, 1)?;

    // Counter 0 generates the GHASH pad
    let mut ghash_pad = [0u8; 16];
    ctr.process(&mut ghash_pad);

    let ghash = GcmGhash::new(&h, ghash_pad, associated_data)?;
    Ok((ctr, ghash))
}
```

**Encryption with Streaming Updates**:

```rust
pub struct Aes256GcmEncryption {
    ctr: Aes256Ctr32,
    ghash: GcmGhash,
}

impl Aes256GcmEncryption {
    pub fn encrypt(&mut self, buf: &mut [u8]) {
        // First encrypt with CTR mode
        self.ctr.process(buf);
        // Then update GHASH with ciphertext
        self.ghash.update(buf);
    }

    pub fn compute_tag(self) -> [u8; TAG_SIZE] {
        self.ghash.finalize()
    }
}
```

**Decryption with Authentication**:

```rust
pub struct Aes256GcmDecryption {
    ctr: Aes256Ctr32,
    ghash: GcmGhash,
}

impl Aes256GcmDecryption {
    pub fn decrypt(&mut self, buf: &mut [u8]) {
        // Update GHASH with ciphertext BEFORE decrypting
        self.ghash.update(buf);
        // Then decrypt
        self.ctr.process(buf);
    }

    pub fn verify_tag(self, tag: &[u8]) -> Result<()> {
        if tag.len() != TAG_SIZE {
            return Err(Error::InvalidTag);
        }

        let computed_tag = self.ghash.finalize();

        // Constant-time comparison prevents timing attacks
        let tag_ok = tag.ct_eq(&computed_tag);

        if !bool::from(tag_ok) {
            return Err(Error::InvalidTag);
        }

        Ok(())
    }
}
```

**Why Constant-Time Comparison Matters**:

```rust
use subtle::ConstantTimeEq;

// ❌ WRONG: Variable-time comparison
if tag == computed_tag { /* ... */ }

// ✅ CORRECT: Constant-time comparison
let tag_ok = tag.ct_eq(&computed_tag);
if !bool::from(tag_ok) { /* ... */ }
```

Variable-time comparisons can leak information through timing side-channels. The `subtle` crate ensures comparisons take the same time regardless of where tags differ.

**Usage in HPKE** (see Section 2.4):

GCM is the AEAD used in Signal's HPKE implementation for sealed sender metadata protection.

---

### 2.1.4 Why No AES-GCM-SIV?

The table of contents mentions AES-GCM-SIV (a nonce-misuse resistant variant), but searching the codebase reveals **no implementation**. This is notable because:

1. **GCM-SIV** is designed to degrade gracefully under nonce reuse (unlike GCM's catastrophic failure)
2. Signal's architecture ensures **nonce uniqueness through key rotation** (Double Ratchet)
3. **Performance cost**: GCM-SIV requires two AES passes vs. GCM's one
4. **Not needed**: Signal's protocol design makes nonce reuse virtually impossible

This demonstrates Signal's philosophy: **design protocols that don't require misuse-resistant primitives**, rather than relying on them as a safety net.

---

## 2.2 Hash Functions and Key Derivation

Cryptographic hash functions and key derivation functions (KDFs) are the workhorses of Signal's key management.

### 2.2.1 Hash Functions: SHA-256 and SHA-512

**Implementation**: `/home/user/libsignal/rust/crypto/src/hash.rs`

```rust
use sha2::{Digest, Sha256, Sha512};

#[derive(Clone)]
pub enum CryptographicHash {
    Sha1(Sha1),
    Sha256(Sha256),
    Sha512(Sha512),
}

impl CryptographicHash {
    pub fn new(algo: &str) -> Result<Self> {
        match algo {
            "SHA-256" | "SHA256" | "Sha256" => Ok(Self::Sha256(Sha256::new())),
            "SHA-512" | "SHA512" | "Sha512" => Ok(Self::Sha512(Sha512::new())),
            _ => Err(Error::UnknownAlgorithm("digest", algo.to_string())),
        }
    }

    pub fn update(&mut self, input: &[u8]) {
        match self {
            Self::Sha256(sha256) => sha256.update(input),
            Self::Sha512(sha512) => sha512.update(input),
            // ... SHA-1 for legacy support only
        }
    }

    pub fn finalize(&mut self) -> Vec<u8> {
        match self {
            Self::Sha256(sha256) => sha256.finalize_reset().to_vec(),
            Self::Sha512(sha512) => sha512.finalize_reset().to_vec(),
            // ...
        }
    }
}
```

**Design Note**: SHA-1 support exists solely for legacy compatibility. Modern Signal code uses SHA-256 or SHA-512 exclusively.

---

### 2.2.2 HMAC-SHA256: Message Authentication

**HMAC** (Hash-based Message Authentication Code) provides integrity and authenticity without encryption.

**Implementation**:

```rust
use hmac::{Hmac, Mac};
use sha2::Sha256;

#[derive(Clone)]
pub enum CryptographicMac {
    HmacSha256(Hmac<Sha256>),
    HmacSha1(Hmac<Sha1>),
}

impl CryptographicMac {
    pub fn new(algo: &str, key: &[u8]) -> Result<Self> {
        match algo {
            "HMACSha256" | "HmacSha256" => Ok(Self::HmacSha256(
                Hmac::<Sha256>::new_from_slice(key)
                    .expect("HMAC accepts any key length")
            )),
            _ => Err(Error::UnknownAlgorithm("MAC", algo.to_string())),
        }
    }

    pub fn update(&mut self, input: &[u8]) {
        match self {
            Self::HmacSha256(sha256) => sha256.update(input),
            // ...
        }
    }

    pub fn finalize(&mut self) -> Vec<u8> {
        match self {
            Self::HmacSha256(sha256) => {
                sha256.finalize_reset().into_bytes().to_vec()
            }
            // ...
        }
    }
}
```

**Direct HMAC-SHA256** (used throughout the protocol):

```rust
// From rust/protocol/src/crypto.rs
pub(crate) fn hmac_sha256(key: &[u8], input: &[u8]) -> [u8; 32] {
    let mut hmac = Hmac::<Sha256>::new_from_slice(key)
        .expect("HMAC-SHA256 should accept any size key");
    hmac.update(input);
    hmac.finalize().into_bytes().into()
}
```

**Usage in Chain Key Derivation**:

```rust
// From rust/protocol/src/ratchet/keys.rs
impl ChainKey {
    const MESSAGE_KEY_SEED: [u8; 1] = [0x01u8];
    const CHAIN_KEY_SEED: [u8; 1] = [0x02u8];

    pub(crate) fn next_chain_key(&self) -> Self {
        Self {
            key: self.calculate_base_material(Self::CHAIN_KEY_SEED),
            index: self.index + 1,
        }
    }

    fn calculate_base_material(&self, seed: [u8; 1]) -> [u8; 32] {
        crypto::hmac_sha256(&self.key, &seed)
    }
}
```

This shows HMAC used as a **PRF (Pseudorandom Function)** to derive new chain keys from previous ones—a critical part of the Double Ratchet (see Chapter 3).

---

### 2.2.3 HKDF: Key Derivation Function

**HKDF** (HMAC-based Key Derivation Function, RFC 5869) is Signal's primary tool for deriving multiple cryptographic keys from a single secret.

**Two-Phase Operation**:

1. **Extract**: `PRK = HMAC-Hash(salt, IKM)` - Extract pseudorandom key from input
2. **Expand**: `OKM = HMAC-Hash(PRK, info || counter)` - Expand to desired length

**Usage in Message Key Derivation**:

```rust
// From rust/protocol/src/ratchet/keys.rs
use hkdf::Hkdf;
use sha2::Sha256;

impl MessageKeys {
    pub(crate) fn derive_keys(
        input_key_material: &[u8],
        optional_salt: Option<&[u8]>,
        counter: u32,
    ) -> Self {
        #[derive(Default, KnownLayout, IntoBytes, FromBytes)]
        #[repr(C, packed)]
        struct DerivedSecretBytes([u8; 32], [u8; 32], [u8; 16]);
        let mut okm = DerivedSecretBytes::default();

        // Extract and expand in one step
        Hkdf::<Sha256>::new(optional_salt, input_key_material)
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

**Key Insights**:

1. **Info string** `"WhisperMessageKeys"`: Domain separation ensures keys for different purposes are cryptographically independent
2. **Structured output**: Using `zerocopy` for safe structured parsing
3. **Optional salt**: Enables both extract+expand (with salt) and expand-only (without salt)

**Root Key to Chain Key Derivation**:

```rust
impl RootKey {
    pub(crate) fn create_chain(
        self,
        their_ratchet_key: &PublicKey,
        our_ratchet_key: &PrivateKey,
    ) -> Result<(RootKey, ChainKey)> {
        let shared_secret = our_ratchet_key
            .calculate_agreement(their_ratchet_key)?;

        #[repr(C, packed)]
        struct DerivedSecretBytes([u8; 32], [u8; 32]);
        let mut derived_secret_bytes = DerivedSecretBytes::default();

        // HKDF with root key as salt, shared secret as IKM
        Hkdf::<Sha256>::new(Some(&self.key), &shared_secret)
            .expand(b"WhisperRatchet", derived_secret_bytes.as_mut_bytes())
            .expect("valid output length");

        let DerivedSecretBytes(root_key, chain_key) = derived_secret_bytes;

        Ok((
            RootKey { key: root_key },
            ChainKey { key: chain_key, index: 0 },
        ))
    }
}
```

This demonstrates the **Double Ratchet's symmetric-key ratchet**: each DH ratchet step derives new root and chain keys from the shared secret.

**Test Vector** (from the implementation):

```rust
#[test]
fn test_chain_key_derivation() -> Result<()> {
    let seed = hex!("8ab72d6f4cc5ac0d387eaf463378ddb28edd07385b1cb01250c715982e7ad48f");
    let message_key = hex!("bf51e9d75e0e31031051f82a2491ffc084fa298b7793bd9db620056febf45217");
    let mac_key = hex!("c6c77d6a73a354337a56435e34607dfe48e3ace14e77314dc6abc172e7a7030b");
    let next_chain_key = hex!("28e8f8fee54b801eef7c5cfb2f17f32c7b334485bbb70fac6ec10342a246d15d");

    let chain_key = ChainKey::new(seed, 0);
    assert_eq!(&message_key, chain_key.message_keys().generate_keys(None).cipher_key());
    assert_eq!(&mac_key, chain_key.message_keys().generate_keys(None).mac_key());
    assert_eq!(&next_chain_key, chain_key.next_chain_key().key());

    Ok(())
}
```

---

## 2.3 Elliptic Curve Cryptography

Signal's elliptic curve operations use **Curve25519**, chosen for its security, performance, and resistance to implementation bugs.

### 2.3.1 Curve25519 Background

**Historical Context**: Proposed by Daniel J. Bernstein in 2006, Curve25519 was designed to be **difficult to implement incorrectly**:

- No special cases or edge cases
- Complete addition formulas
- Twist-secure (invalid curve points don't leak information)
- Fast constant-time implementations possible

**Mathematical Properties**:
- **Montgomery curve**: y² = x³ + 486662x² + x over 𝔽ₚ where p = 2²⁵⁵ - 19
- **Discrete log security**: ~128 bits (equivalent to 3072-bit RSA)
- **Cofactor**: 8 (cleared by clamping and point multiplication)

**Implementation**: `/home/user/libsignal/rust/core/src/curve/curve25519.rs`

```rust
use curve25519_dalek::scalar::Scalar;
use curve25519_dalek::edwards::EdwardsPoint;
use curve25519_dalek::montgomery::MontgomeryPoint;
use x25519_dalek::{PublicKey, StaticSecret};
use rand::{CryptoRng, Rng};

pub const PRIVATE_KEY_LENGTH: usize = 32;
pub const PUBLIC_KEY_LENGTH: usize = 32;
pub const SIGNATURE_LENGTH: usize = 64;

#[derive(Clone)]
pub struct PrivateKey {
    secret: StaticSecret,
}
```

---

### 2.3.2 X25519: Diffie-Hellman Key Agreement

**X25519** performs ECDH on Curve25519's Montgomery form, providing the building block for all Signal key agreements.

**Key Generation**:

```rust
impl PrivateKey {
    pub fn new<R>(csprng: &mut R) -> Self
    where
        R: CryptoRng + Rng,
    {
        // Generate random 32 bytes
        let mut bytes = [0u8; 32];
        csprng.fill_bytes(&mut bytes);

        // Clamp the scalar according to Curve25519 spec:
        // - Clear bits 0, 1, 2 (ensures multiple of 8)
        // - Clear bit 255 (ensures < 2^255)
        // - Set bit 254 (ensures >= 2^254)
        bytes = scalar::clamp_integer(bytes);

        let secret = StaticSecret::from(bytes);
        PrivateKey { secret }
    }

    pub fn derive_public_key_bytes(&self) -> [u8; PUBLIC_KEY_LENGTH] {
        *PublicKey::from(&self.secret).as_bytes()
    }
}
```

**Why Clamping?**

1. **Multiple of 8**: Clears cofactor, preventing small subgroup attacks
2. **Fixed high bit**: Ensures constant-time scalar multiplication
3. **Standard practice**: All Curve25519 implementations must clamp

**Diffie-Hellman Agreement**:

```rust
impl PrivateKey {
    pub fn calculate_agreement(
        &self,
        their_public_key: &[u8; PUBLIC_KEY_LENGTH],
    ) -> [u8; 32] {
        *self
            .secret
            .diffie_hellman(&PublicKey::from(*their_public_key))
            .as_bytes()
    }
}
```

**Test Vector**:

```rust
#[test]
fn test_agreement() {
    let alice_public = hex!("1bb75966f2e93a3691dfff942bb2a466a1c08b8d78ca3f4d6df8b8bfa2e4ee28");
    let alice_private = hex!("c806439dc9d2c476ffed8f2580c0888d58ab406bf7ae36988790219b6bb4bf59");
    let bob_public = hex!("6536149932b15ee9e5fd3d86ce719ef4ec1dae18868a7b3f5fa9565a27a22f");
    let bob_private = hex!("b03b34c33a1c44f225b662d2bf4859b8135411fa7b0386d45fb75dc5b91b4466");
    let shared = hex!("325f23932894ced6e673b86ba410174489b649a9c3806c1dd7cac4c477e6e29");

    let alice_key = PrivateKey::from(alice_private);
    let bob_key = PrivateKey::from(bob_private);

    assert_eq!(alice_public, alice_key.derive_public_key_bytes());
    assert_eq!(bob_public, bob_key.derive_public_key_bytes());

    let alice_computed = alice_key.calculate_agreement(&bob_public);
    let bob_computed = bob_key.calculate_agreement(&alice_public);

    assert_eq!(shared, alice_computed);
    assert_eq!(shared, bob_computed);
}
```

---

### 2.3.3 XEdDSA: Signatures from X25519 Keys

**Problem**: X25519 keys use the Montgomery form (x-coordinate only), but signatures require Edwards form (both coordinates).

**Solution**: **XEdDSA** (eXtended EdDSA) allows signing with X25519 private keys by converting to Ed25519 form internally.

**Why This Matters**: Signal uses the same key for both ECDH (X25519) and signatures (XEdDSA), simplifying key management.

**Signature Generation**:

```rust
impl PrivateKey {
    pub fn calculate_signature<R>(
        &self,
        csprng: &mut R,
        message: &[&[u8]],
    ) -> [u8; SIGNATURE_LENGTH]
    where
        R: CryptoRng + Rng,
    {
        let mut random_bytes = [0u8; 64];
        csprng.fill_bytes(&mut random_bytes);

        // Convert X25519 private key to Ed25519 scalar
        let key_data = self.secret.to_bytes();
        let a = Scalar::from_bytes_mod_order(key_data);

        // Compute Ed25519 public key: A = a * G
        let ed_public_key_point = &a * ED25519_BASEPOINT_TABLE;
        let ed_public_key = ed_public_key_point.compress();

        // Extract sign bit (for Edwards decompression)
        let sign_bit = ed_public_key.as_bytes()[31] & 0b1000_0000_u8;

        // XEdDSA uses a special hash prefix for domain separation
        let hash_prefix = [0xFFu8; 32];

        let mut hash1 = Sha512::new();
        hash1.update(&hash_prefix[..]);
        hash1.update(&key_data[..]);
        for message_piece in message {
            hash1.update(message_piece);
        }
        hash1.update(&random_bytes[..]);

        // r = H(prefix || key || message || randomness)
        let r = Scalar::from_hash(hash1);
        let cap_r = (&r * ED25519_BASEPOINT_TABLE).compress();

        // h = H(R || A || message)
        let mut hash = Sha512::new();
        hash.update(cap_r.as_bytes());
        hash.update(ed_public_key.as_bytes());
        for message_piece in message {
            hash.update(message_piece);
        }
        let h = Scalar::from_hash(hash);

        // s = h * a + r  (Schnorr signature structure)
        let s = (h * a) + r;

        // Signature format: R || s || sign_bit
        let mut result = [0u8; SIGNATURE_LENGTH];
        result[..32].copy_from_slice(cap_r.as_bytes());
        result[32..].copy_from_slice(s.as_bytes());
        result[SIGNATURE_LENGTH - 1] &= 0b0111_1111_u8;
        result[SIGNATURE_LENGTH - 1] |= sign_bit;
        result
    }
}
```

**Verification**:

```rust
pub fn verify_signature(
    their_public_key: &[u8; PUBLIC_KEY_LENGTH],
    message: &[&[u8]],
    signature: &[u8; SIGNATURE_LENGTH],
) -> bool {
    // Convert Montgomery to Edwards using sign bit from signature
    let mont_point = MontgomeryPoint(*their_public_key);
    let ed_pub_key_point = match mont_point.to_edwards(
        (signature[SIGNATURE_LENGTH - 1] & 0b1000_0000_u8) >> 7
    ) {
        Some(x) => x,
        None => return false,
    };

    let cap_a = ed_pub_key_point.compress();
    let mut cap_r = [0u8; 32];
    cap_r.copy_from_slice(&signature[..32]);
    let mut s = [0u8; 32];
    s.copy_from_slice(&signature[32..]);
    s[31] &= 0b0111_1111_u8;

    // Recompute h = H(R || A || message)
    let mut hash = Sha512::new();
    hash.update(&cap_r[..]);
    hash.update(cap_a.as_bytes());
    for message_piece in message {
        hash.update(message_piece);
    }
    let h = Scalar::from_hash(hash);

    // Verify: s * G = h * A + R
    let minus_cap_a = -ed_pub_key_point;
    let cap_r_check_point = EdwardsPoint::vartime_double_scalar_mul_basepoint(
        &h,
        &minus_cap_a,
        &Scalar::from_bytes_mod_order(s),
    );
    let cap_r_check = cap_r_check_point.compress();

    bool::from(cap_r_check.as_bytes().ct_eq(&cap_r))
}
```

---

## 2.4 HPKE: Hybrid Public Key Encryption

**HPKE** (RFC 9180) is a modern public-key encryption scheme combining KEM + KDF + AEAD. Signal uses it for **Sealed Sender** metadata protection.

**Implementation**: `/home/user/libsignal/rust/crypto/src/hpke.rs`

### 2.4.1 Signal's HPKE Configuration

```rust
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum SignalHpkeCiphertextType {
    Base_X25519_HkdfSha256_Aes256Gcm = 1,
}

impl SignalHpkeCiphertextType {
    fn kem_algorithm(self) -> hpke_types::KemAlgorithm {
        hpke_types::KemAlgorithm::DhKem25519
    }

    fn kdf_algorithm(self) -> hpke_types::KdfAlgorithm {
        hpke_types::KdfAlgorithm::HkdfSha256
    }

    fn aead_algorithm(self) -> hpke_types::AeadAlgorithm {
        hpke_types::AeadAlgorithm::Aes256Gcm
    }
}
```

Signal uses:
- **KEM**: DHKEM(X25519, HKDF-SHA256)
- **KDF**: HKDF-SHA256
- **AEAD**: AES-256-GCM
- **Mode**: Base (unauthenticated sender)

### 2.4.2 Encryption (Seal)

```rust
pub trait SimpleHpkeSender {
    fn seal(&self, info: &[u8], aad: &[u8], plaintext: &[u8])
        -> Result<Vec<u8>, HpkeError>;
}

impl SimpleHpkeSender for libsignal_core::curve::PublicKey {
    fn seal(&self, info: &[u8], aad: &[u8], plaintext: &[u8])
        -> Result<Vec<u8>, HpkeError>
    {
        let ciphertext_type = SignalHpkeCiphertextType::Base_X25519_HkdfSha256_Aes256Gcm;

        let hpke_key = HpkePublicKey::from(self.public_key_bytes());

        // HPKE seal returns (encapsulated_secret, ciphertext)
        let (encapsulated_secret, mut ciphertext) = ciphertext_type
            .set_up()
            .seal(&hpke_key, info, aad, plaintext, None, None, None)?;

        // Prepend type byte and encapsulated secret
        ciphertext.splice(
            0..0,
            [ciphertext_type.into()]
                .into_iter()
                .chain(encapsulated_secret),
        );

        Ok(ciphertext)
    }
}
```

**Ciphertext Format**: `[type_byte(1) || enc(32) || ciphertext || tag(16)]`

### 2.4.3 Decryption (Open)

```rust
pub trait SimpleHpkeReceiver {
    fn open(&self, info: &[u8], aad: &[u8], ciphertext: &[u8])
        -> Result<Vec<u8>, HpkeError>;
}

impl SimpleHpkeReceiver for libsignal_core::curve::PrivateKey {
    fn open(&self, info: &[u8], aad: &[u8], ciphertext: &[u8])
        -> Result<Vec<u8>, HpkeError>
    {
        // Parse type byte
        let (ciphertext_type, ciphertext) = ciphertext
            .split_at_checked(1)
            .ok_or(HpkeError::InvalidInput)?;
        let ciphertext_type = ciphertext_type[0]
            .try_into()
            .map_err(|_| HpkeError::UnknownMode)?;

        // Extract encapsulated secret
        let (encapsulated_secret, ciphertext) = ciphertext
            .split_at_checked(32)  // X25519 public key size
            .ok_or(HpkeError::InvalidInput)?;

        let hpke_key = HpkePrivateKey::from(self.serialize());

        ciphertext_type.set_up().open(
            encapsulated_secret,
            &hpke_key,
            info,
            aad,
            ciphertext,
            None,
            None,
            None,
        )
    }
}
```

**Usage in Sealed Sender**: See Chapter 5 for how HPKE encrypts sender certificates and metadata.

---

## 2.5 Post-Quantum Cryptography

Signal is at the forefront of post-quantum cryptography deployment, having integrated **ML-KEM** (formerly Kyber) into the protocol.

### 2.5.1 Why Post-Quantum?

**Threat Model**: "Store now, decrypt later" attacks where adversaries capture encrypted traffic and wait for quantum computers capable of breaking ECDH.

**Timeline**:
- 2016: NIST post-quantum competition begins
- 2023: Kyber selected as ML-KEM standard
- 2023: Signal deploys PQXDH (X3DH + Kyber)
- 2024: Signal deploys SPQR (Double Ratchet + Kyber)

### 2.5.2 KEM (Key Encapsulation Mechanism)

Unlike traditional public-key encryption, KEMs directly encapsulate a shared secret:

```
(ciphertext, shared_secret) = Encapsulate(public_key, randomness)
shared_secret = Decapsulate(secret_key, ciphertext)
```

**Implementation Structure**: `/home/user/libsignal/rust/protocol/src/kem/`

```rust
pub enum KeyType {
    Kyber768,   // NIST security level 3
    Kyber1024,  // NIST security level 5
    MLKEM1024,  // Standardized ML-KEM-1024
}

trait Parameters {
    const KEY_TYPE: KeyType;
    const PUBLIC_KEY_LENGTH: usize;
    const SECRET_KEY_LENGTH: usize;
    const CIPHERTEXT_LENGTH: usize;
    const SHARED_SECRET_LENGTH: usize;

    fn generate<R: CryptoRng>(csprng: &mut R)
        -> (KeyMaterial<Public>, KeyMaterial<Secret>);

    fn encapsulate<R: CryptoRng>(
        pub_key: &KeyMaterial<Public>,
        csprng: &mut R,
    ) -> Result<(SharedSecret, Ciphertext), BadKEMKeyLength>;

    fn decapsulate(
        secret_key: &KeyMaterial<Secret>,
        ciphertext: &[u8],
    ) -> Result<SharedSecret, DecapsulateError>;
}
```

### 2.5.3 ML-KEM-1024 Implementation

**Using libcrux**: Signal chose [libcrux](https://github.com/cryspen/libcrux), a formally verified implementation of ML-KEM.

```rust
// From rust/protocol/src/kem/mlkem1024.rs
use libcrux_ml_kem::SHARED_SECRET_SIZE;
use libcrux_ml_kem::mlkem1024::{
    self, MlKem1024Ciphertext, MlKem1024PrivateKey, MlKem1024PublicKey,
};

pub(crate) struct Parameters;

impl super::Parameters for Parameters {
    const KEY_TYPE: KeyType = KeyType::Kyber1024;
    const PUBLIC_KEY_LENGTH: usize = MlKem1024PublicKey::LENGTH;  // 1568
    const SECRET_KEY_LENGTH: usize = MlKem1024PrivateKey::LENGTH; // 3168
    const CIPHERTEXT_LENGTH: usize = MlKem1024Ciphertext::LENGTH; // 1568
    const SHARED_SECRET_LENGTH: usize = SHARED_SECRET_SIZE;       // 32

    fn generate<R: rand::CryptoRng>(
        csprng: &mut R,
    ) -> (KeyMaterial<Public>, KeyMaterial<Secret>) {
        let (sk, pk) = mlkem1024::generate_key_pair(csprng.random())
            .into_parts();
        (KeyMaterial::from(pk), KeyMaterial::from(sk))
    }

    fn encapsulate<R: rand::CryptoRng>(
        pub_key: &KeyMaterial<Public>,
        csprng: &mut R,
    ) -> Result<(Box<[u8]>, Box<[u8]>), BadKEMKeyLength> {
        let mlkem_pk = MlKem1024PublicKey::try_from(pub_key.as_ref())
            .map_err(|_| BadKEMKeyLength)?;

        let (mlkem_ct, mlkem_ss) = mlkem1024::encapsulate(
            &mlkem_pk,
            csprng.random()
        );

        Ok((
            mlkem_ss.as_ref().into(),  // Shared secret
            mlkem_ct.as_ref().into(),  // Ciphertext
        ))
    }

    fn decapsulate(
        secret_key: &KeyMaterial<Secret>,
        ciphertext: &[u8],
    ) -> Result<Box<[u8]>, DecapsulateError> {
        let mlkem_sk = MlKem1024PrivateKey::try_from(secret_key.as_ref())
            .map_err(|_| DecapsulateError::BadKeyLength)?;

        let mlkem_ct = MlKem1024Ciphertext::try_from(ciphertext)
            .map_err(|_| DecapsulateError::BadCiphertext)?;

        let mlkem_ss = mlkem1024::decapsulate(&mlkem_sk, &mlkem_ct);

        Ok(mlkem_ss.as_ref().into())
    }
}
```

### 2.5.4 Key Serialization

Signal adds a type byte to distinguish KEM types:

```rust
pub struct Key<T: KeyKind> {
    key_type: KeyType,
    key_data: KeyMaterial<T>,
}

impl<T: KeyKind> Key<T> {
    pub fn serialize(&self) -> Box<[u8]> {
        let mut result = Vec::with_capacity(1 + self.key_data.len());
        result.push(self.key_type.value());  // 0x08 for Kyber1024
        result.extend_from_slice(&self.key_data);
        result.into_boxed_slice()
    }

    pub fn deserialize(value: &[u8]) -> Result<Self> {
        if value.is_empty() {
            return Err(SignalProtocolError::NoKeyTypeIdentifier);
        }
        let key_type = KeyType::try_from(value[0])?;
        if value.len() != T::key_length(key_type) + 1 {
            return Err(SignalProtocolError::BadKEMKeyLength(
                key_type,
                value.len()
            ));
        }
        Ok(Key {
            key_type,
            key_data: KeyMaterial::new(value[1..].into()),
        })
    }
}
```

### 2.5.5 Example Usage

```rust
use libsignal_protocol::kem::*;

let mut rng = rand::rng();

// Generate Kyber1024 key pair
let kp = KeyPair::generate(KeyType::Kyber1024, &mut rng);

// Sender: encapsulate shared secret
let (ss_sender, ct) = kp.public_key
    .encapsulate(&mut rng)
    .expect("encapsulation succeeds");

// Receiver: decapsulate shared secret
let ss_receiver = kp.secret_key
    .decapsulate(&ct)
    .expect("decapsulation succeeds");

assert_eq!(ss_sender, ss_receiver);
```

### 2.5.6 Hybrid Approach

Signal uses **hybrid construction**: combine classical (X25519) and post-quantum (Kyber) shared secrets:

```
combined_secret = HKDF-Expand(
    HKDF-Extract(salt, x25519_secret || kyber_secret),
    info
)
```

This provides:
- **Security now**: X25519 protects against current attacks
- **Security later**: Kyber protects against future quantum attacks
- **Failure tolerance**: If Kyber is broken, X25519 still provides security

**Implementation in PQXDH**: See Chapter 3 for how Signal combines X25519 and Kyber in key agreement.

---

## 2.6 Summary and Security Properties

### Cryptographic Primitive Selection Rationale

| Primitive | Why Chosen | Security Level |
|-----------|-----------|----------------|
| **AES-256** | Industry standard, hardware support | 256-bit symmetric |
| **GCM** | AEAD, parallelizable | 128-bit authentication |
| **SHA-256** | Fast, secure, well-analyzed | 256-bit collision resistance |
| **HMAC-SHA256** | Provably secure MAC | 256-bit |
| **HKDF** | Extract-then-expand KDF | Depends on underlying hash |
| **Curve25519** | Safe by design, fast | ~128-bit DLP |
| **XEdDSA** | Reuse X25519 keys for signatures | ~128-bit |
| **HPKE** | Modern standard, composable | Depends on components |
| **ML-KEM-1024** | Post-quantum secure | NIST Level 5 (~256-bit classical) |

### Cross-References

- **Chapter 3**: How these primitives compose into the Signal Protocol
- **Chapter 5**: HPKE usage in Sealed Sender
- **Chapter 6**: Zero-knowledge proofs using Curve25519
- **Appendix C**: Complete protocol specifications

### Implementation Safety

libsignal's implementations demonstrate several best practices:

1. **Constant-time operations**: Prevents timing attacks
2. **Memory safety**: Rust eliminates buffer overflows
3. **Type safety**: Distinct types for keys, nonces, tags
4. **Verified implementations**: libcrux for ML-KEM
5. **Comprehensive testing**: Unit tests, property tests, test vectors

---

## 2.7 Code Provenance and Dependencies

**Cryptographic Libraries Used**:

- **RustCrypto** (`aes`, `ctr`, `ghash`, `hmac`, `sha2`, `hkdf`): Pure Rust implementations
- **curve25519-dalek**: Extensively audited Ed25519/X25519 library
- **x25519-dalek**: X25519 key agreement
- **libcrux**: Formally verified ML-KEM from Cryspen
- **hpke-rs**: RFC 9180 implementation
- **subtle**: Constant-time comparison primitives

**Why Multiple Sources?**

1. **Best-of-breed**: Each library excels in its domain
2. **Auditability**: Multiple independent implementations reduce risk
3. **Formal verification**: libcrux provides mathematical proofs
4. **Community trust**: Well-reviewed, widely-used libraries

---

## Conclusion

This chapter examined Signal's cryptographic foundations—the primitives that make secure messaging possible. We saw:

- **Symmetric encryption** balancing performance and security
- **Hash functions and KDFs** enabling secure key derivation
- **Elliptic curve cryptography** providing efficient public-key operations
- **HPKE** modernizing public-key encryption
- **Post-quantum cryptography** preparing for future threats

In Chapter 3, we'll see how these primitives combine into the **Signal Protocol**—the Double Ratchet, X3DH/PQXDH key agreement, and message encryption that powers billions of secure conversations.

---

**Next Chapter**: [Chapter 3: The Signal Protocol →](04-CHAPTER-03-SIGNAL-PROTOCOL.md)

**File Paths Referenced**:
- `/home/user/libsignal/rust/crypto/src/aes_cbc.rs` - AES-CBC implementation
- `/home/user/libsignal/rust/crypto/src/aes_ctr.rs` - AES-CTR wrapper
- `/home/user/libsignal/rust/crypto/src/aes_gcm.rs` - AES-GCM AEAD
- `/home/user/libsignal/rust/crypto/src/hash.rs` - Hash and HMAC wrappers
- `/home/user/libsignal/rust/core/src/curve/curve25519.rs` - Curve25519 operations
- `/home/user/libsignal/rust/crypto/src/hpke.rs` - HPKE implementation
- `/home/user/libsignal/rust/protocol/src/kem/` - Post-quantum KEM modules
- `/home/user/libsignal/rust/protocol/src/crypto.rs` - Protocol-level crypto utilities
- `/home/user/libsignal/rust/protocol/src/ratchet/keys.rs` - Key derivation

---

*Generated from libsignal v0.86.5 • November 2025*
