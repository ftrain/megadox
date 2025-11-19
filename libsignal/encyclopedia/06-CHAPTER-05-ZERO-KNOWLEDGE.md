# Chapter 5: Zero-Knowledge Cryptography

## Introduction

Zero-knowledge proofs are cryptographic protocols that allow one party (the prover) to convince
another party (the verifier) that a statement is true without revealing any information beyond the
validity of the statement itself. In Signal's architecture, these proofs enable privacy-preserving
authentication and authorization—allowing users to prove they belong to a group or have certain
credentials without revealing their identity.

This chapter explores libsignal's zero-knowledge infrastructure across three layers:
- **poksho**: A foundational library for Schnorr-based zero-knowledge proofs
- **zkgroup**: Domain-specific credential systems for Signal's group features
- **zkcredential**: A generic, attribute-based credential framework

## 1. Zero-Knowledge Proofs: Core Concepts

### What Are Zero-Knowledge Proofs?

A zero-knowledge proof system allows a prover to demonstrate knowledge of some secret value without
revealing the secret itself. Consider the classic example: proving you know a password without
transmitting the password.

Zero-knowledge proofs satisfy three properties:

1. **Completeness**: If the statement is true and both parties follow the protocol, the verifier
   will be convinced.
2. **Soundness**: If the statement is false, no cheating prover can convince the verifier (except
   with negligible probability).
3. **Zero-knowledge**: The verifier learns nothing beyond the truth of the statement.

### Why Signal Uses Zero-Knowledge Proofs

Signal employs zero-knowledge proofs to achieve several privacy goals:

1. **Anonymous group operations**: Users can prove they belong to a group without revealing their
   identity to the group server.
2. **Receipt verification**: Users can prove they made a payment without linking their identity
   across requests.
3. **Profile credentials**: Users can demonstrate authorization without exposing their account
   identifier.
4. **Group send endorsements**: Efficient tokens that prove membership without repeated ZK proof
   verification.

The key innovation is **attribute-based anonymous credentials (ABCs)**, where credentials encode
attributes (like a user ID) that can be proven in zero-knowledge while remaining encrypted.

### Privacy Properties

Signal's zero-knowledge systems provide:

- **Unlinkability**: Different uses of the same credential cannot be correlated
- **Unforgeability**: Only the issuing server can create valid credentials
- **Hiding**: Attributes remain encrypted to the verifying server
- **Binding**: Credentials cannot be transferred or modified
- **Selective disclosure**: Some attributes can be revealed while others stay hidden

## 2. The poksho Library

### Overview

`poksho` (Proof Of Knowledge of Secrets using Homomorphisms) is libsignal's foundational library
for creating and verifying Schnorr-style zero-knowledge proofs. It implements the "Sigma protocol
for arbitrary linear relations" described in Boneh-Shoup section 19.5.3.

Location: `/home/user/libsignal/rust/poksho/src/`

### Mathematical Foundation: Group Homomorphisms

poksho treats zero-knowledge proofs as demonstrating knowledge of a preimage under a group
homomorphism. Consider:

- **G1**: A group of scalar vectors
- **G2**: A group of Ristretto point vectors
- **Homomorphism F**: G1 → G2

The homomorphism can be expressed as a system of equations:

```
P₁ = s₁·P₁' + s₂·P₂' + s₃·P₃' + ...
P₂ = s₄·P₄' + s₅·P₅' + s₆·P₆' + ...
P₃ = s₇·P₇' + s₈·P₈' + ...
```

Where:
- Left-hand side: Known points (the image in G2)
- Right-hand side: Linear combinations of scalars (witnesses) and points
- The scalars form an element in G1 that we prove knowledge of

### SHO: Stateful Hash Object

The `ShoHmacSha256` type provides a stateful hash object for deriving randomness and challenges:

```rust
// From: rust/poksho/src/shohmacsha256.rs
pub struct ShoHmacSha256 {
    hasher: Hmac<Sha256>,
    cv: [u8; HASH_LEN],  // Chaining value
    mode: Mode,           // ABSORBING or RATCHETED
}

impl ShoApi for ShoHmacSha256 {
    fn new(label: &[u8]) -> ShoHmacSha256 {
        let mut sho = ShoHmacSha256 {
            hasher: Hmac::<Sha256>::new_from_slice(&[0; HASH_LEN])
                .expect("HMAC accepts 256-bit keys"),
            cv: [0; HASH_LEN],
            mode: Mode::RATCHETED,
        };
        sho.absorb_and_ratchet(label);
        sho
    }

    fn absorb(&mut self, input: &[u8]) {
        if let Mode::RATCHETED = self.mode {
            self.hasher = Hmac::<Sha256>::new_from_slice(&self.cv)
                .expect("HMAC accepts 256-bit keys");
            self.mode = Mode::ABSORBING;
        }
        self.hasher.update(input);
    }

    fn ratchet(&mut self) {
        if let Mode::RATCHETED = self.mode {
            return;
        }
        self.hasher.update(&[0x00]);
        self.cv.copy_from_slice(&self.hasher.clone().finalize().into_bytes());
        self.hasher.reset();
        self.mode = Mode::RATCHETED;
    }

    fn squeeze_and_ratchet_into(&mut self, mut target: &mut [u8]) {
        // Produce arbitrary-length output...
        // (implementation details)
    }
}
```

**Key operations**:
- `absorb()`: Mix data into the hash state
- `ratchet()`: Finalize current state and prepare for next operation
- `squeeze_and_ratchet()`: Extract pseudorandom output

This provides domain separation and ensures that different protocol steps cannot interfere with
each other.

### Statements and Proofs

A `Statement` defines the system of equations to prove:

```rust
// From: rust/poksho/src/statement.rs
pub struct Statement {
    equations: Vec<Equation>,
    scalar_map: HashMap<Cow<'static, str>, ScalarIndex>,
    scalar_vec: Vec<Cow<'static, str>>,
    point_map: HashMap<Cow<'static, str>, PointIndex>,
    point_vec: Vec<Cow<'static, str>>,
}

impl Statement {
    pub fn add(&mut self, lhs_str: &str, rhs_pairs: &[(&str, &str)]) {
        // Add equation: lhs = Σ(scalar_i * point_i)
        // Example: st.add("A", &[("a", "G")]) means A = a*G
    }
}
```

**Example: Schnorr Signature**

```rust
// From: rust/poksho/src/sign.rs
pub fn sign(
    private_key: Scalar,
    public_key: RistrettoPoint,
    message: &[u8],
    randomness: &[u8],
) -> Result<Vec<u8>, PokshoError> {
    let mut st = Statement::new();
    st.add("public_key", &[("private_key", "G")]);  // A = a*G

    let mut scalar_args = ScalarArgs::new();
    scalar_args.add("private_key", private_key);

    let mut point_args = PointArgs::new();
    point_args.add("public_key", public_key);

    st.prove(&scalar_args, &point_args, message, randomness)
}
```

This creates a proof of knowledge of the discrete logarithm: given public key `A`, prove knowledge
of private key `a` such that `A = a·G`.

### Proof Generation Protocol

The Fiat-Shamir transformed Schnorr protocol works as follows:

```rust
// From: rust/poksho/src/statement.rs (simplified)
pub fn prove(
    &self,
    scalar_args: &ScalarArgs,  // Witness (secret scalars)
    point_args: &PointArgs,    // Public points
    message: &[u8],
    randomness: &[u8],
) -> Result<Vec<u8>, PokshoError> {
    // 1. Initialize SHO with protocol label
    let mut sho = ShoHmacSha256::new(b"POKSHO_Ristretto_SHOHMACSHA256");

    // 2. Absorb statement description and public points
    sho.absorb(&self.to_bytes());
    for point in &all_points {
        sho.absorb(&point.compress().to_bytes());
    }
    sho.ratchet();

    // 3. Generate synthetic nonce by hashing randomness + witness
    let mut sho2 = sho.clone();
    sho2.absorb(randomness);
    for scalar in &witness {
        sho2.absorb(&scalar.to_bytes());
    }
    sho2.ratchet();
    sho2.absorb_and_ratchet(message);
    let nonce = sho2.squeeze_scalars(witness.len());

    // 4. Compute commitment: R = F(nonce)
    let commitment = self.homomorphism(&nonce, &all_points);

    // 5. Generate challenge by hashing commitment + message
    for point in &commitment {
        sho.absorb(&point.compress().to_bytes());
    }
    sho.absorb_and_ratchet(message);
    let challenge = sho.squeeze_scalar();

    // 6. Compute response: s = r + c·w (for each scalar)
    let response = nonce.iter()
        .zip(witness)
        .map(|(r, w)| r + (w * challenge))
        .collect();

    Ok(Proof { challenge, response }.to_bytes())
}
```

**The proof is "compact"**: it sends only the challenge and response, not the commitments, saving
bandwidth.

### Proof Verification

```rust
// Verification reconstructs the commitment from the response
pub fn verify_proof(
    &self,
    proof_bytes: &[u8],
    point_args: &PointArgs,
    message: &[u8],
) -> Result<(), PokshoError> {
    let proof = Proof::from_slice(proof_bytes)?;

    // Absorb same public data as prover
    let mut sho = ShoHmacSha256::new(b"POKSHO_Ristretto_SHOHMACSHA256");
    sho.absorb(&self.to_bytes());
    for point in &all_points {
        sho.absorb(&point.compress().to_bytes());
    }
    sho.ratchet();

    // Reconstruct commitment: R = F(s) - c·A
    // This works because s = r + c·w, so:
    //   F(s) = F(r + c·w) = F(r) + c·F(w) = R + c·A
    //   Therefore: R = F(s) - c·A
    let commitment = self.homomorphism_with_subtraction(
        &proof.response,
        &all_points,
        Some(proof.challenge)
    );

    // Recompute challenge from reconstructed commitment
    for point in &commitment {
        sho.absorb(&point.compress().to_bytes());
    }
    sho.absorb_and_ratchet(message);
    let expected_challenge = sho.squeeze_scalar();

    // Verify challenges match (constant-time comparison)
    if challenge == proof.challenge {
        Ok(())
    } else {
        Err(VerificationFailure)
    }
}
```

### Security Properties

**Synthetic Nonce Generation**: By hashing together randomness, the witness, and the message,
poksho ensures that:
1. The nonce appears random to attackers
2. Different challenges never use the same nonce (preventing private key leakage)
3. Hardware glitches causing bad randomness don't leak secrets

**Self-Verification**: Before returning a proof, the prover verifies it:

```rust
// Verify before returning, since a bad proof could indicate
// a glitched/faulty response that leaks private keys
match self.verify_proof(&proof_bytes, point_args, message) {
    Err(VerificationFailure) => Err(ProofCreationVerificationFailure),
    Ok(_) => Ok(proof_bytes),
}
```

## 3. Ristretto Group Operations

### The Ristretto Group

All of libsignal's zero-knowledge cryptography operates over the **Ristretto group**, which is
built on top of Curve25519. Ristretto provides:

1. **Prime-order group**: No cofactor issues (all elements have the same order)
2. **Efficient operations**: Fast point addition and scalar multiplication
3. **Canonical encoding**: Each group element has exactly one byte representation
4. **Indistinguishability**: Points look uniformly random

Location: `rust/zkgroup/src/crypto/`

### Point Representation

```rust
use curve25519_dalek::ristretto::RistrettoPoint;
use curve25519_dalek::scalar::Scalar;

// Points are 32 bytes when compressed
let compressed = point.compress();  // CompressedRistretto
let bytes: [u8; 32] = compressed.to_bytes();

// Scalars are also 32 bytes
let scalar_bytes: [u8; 32] = scalar.to_bytes();
```

### Cryptographic Operations

**Point Addition** (Homomorphic):
```rust
let sum = point1 + point2;  // Group operation
```

**Scalar Multiplication**:
```rust
let result = scalar * point;  // Fast using Montgomery ladder
```

**Multi-scalar multiplication** (more efficient than individual operations):
```rust
use curve25519_dalek::traits::MultiscalarMul;

let result = RistrettoPoint::multiscalar_mul(
    &[scalar1, scalar2, scalar3],
    &[point1, point2, point3]
);
// Computes: scalar1*point1 + scalar2*point2 + scalar3*point3
```

### Point Derivation

libsignal derives deterministic points by hashing:

```rust
// From a Sho (Stateful Hash Object)
impl ShoExt for dyn ShoApi {
    fn get_point(&mut self) -> RistrettoPoint {
        let buf = self.squeeze_and_ratchet(64);
        RistrettoPoint::from_uniform_bytes(&buf)
    }

    fn get_scalar(&mut self) -> Scalar {
        let buf = self.squeeze_and_ratchet(64);
        Scalar::from_bytes_mod_order_wide(&buf)
    }
}
```

This ensures derived values are unpredictable and uniformly distributed.

## 4. The zkgroup System

### Overview

`zkgroup` is Signal's domain-specific implementation of anonymous credentials, supporting:

- **Profile key credentials**: Prove account ownership without revealing the account ID
- **Receipt credentials**: Verify payments without linking across requests
- **Authentication credentials**: Prove identity with phone number indices (PNI)
- **Group send endorsements**: Efficient membership tokens

Location: `/home/user/libsignal/rust/zkgroup/src/`

### System Parameters

All zkgroup credentials share a common set of generator points:

```rust
// From: rust/zkgroup/src/crypto/credentials.rs
#[derive(Copy, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct SystemParams {
    pub(crate) G_w: RistrettoPoint,       // For W commitment
    pub(crate) G_wprime: RistrettoPoint,  // For W commitment
    pub(crate) G_x0: RistrettoPoint,      // For x0 in MAC
    pub(crate) G_x1: RistrettoPoint,      // For x1 in MAC
    pub(crate) G_y: OneBased<[RistrettoPoint; 6]>,  // For attributes
    pub(crate) G_m1: RistrettoPoint,      // Message point 1
    pub(crate) G_m2: RistrettoPoint,      // Message point 2
    pub(crate) G_m3: RistrettoPoint,      // Message point 3
    pub(crate) G_m4: RistrettoPoint,      // Message point 4
    pub(crate) G_m5: RistrettoPoint,      // Message point 5
    pub(crate) G_V: RistrettoPoint,       // For verification
    pub(crate) G_z: RistrettoPoint,       // For zero-knowledge
}
```

These are generated deterministically from a fixed seed, ensuring all parties use the same values.

### Credential Structure

A credential is essentially a **MAC (Message Authentication Code)** over encrypted attributes:

```rust
pub struct Credential {
    pub(crate) t: Scalar,           // Random value
    pub(crate) U: RistrettoPoint,   // Random point
    pub(crate) V: RistrettoPoint,   // MAC value
}
```

The MAC equation is:
```
V = W + (x₀ + x₁·t)·U + Σ(yᵢ·Mᵢ)
```

Where:
- `W`, `x₀`, `x₁`, `yᵢ`: Server's secret key components
- `t`, `U`: Random values chosen during issuance
- `Mᵢ`: Attribute points (possibly encrypted)

### Key Pairs

```rust
pub struct KeyPair<S: AttrScalars> {
    // Private components
    pub(crate) w: Scalar,
    pub(crate) wprime: Scalar,
    pub(crate) W: RistrettoPoint,
    pub(crate) x0: Scalar,
    pub(crate) x1: Scalar,
    pub(crate) y: OneBased<S::Storage>,

    // Public components
    pub(crate) C_W: RistrettoPoint,  // Commitment to W
    pub(crate) I: RistrettoPoint,     // Verification point
}

impl<S: AttrScalars> KeyPair<S> {
    pub fn generate(sho: &mut Sho) -> Self {
        let system = SystemParams::get_hardcoded();
        let w = sho.get_scalar();
        let W = w * system.G_w;
        let wprime = sho.get_scalar();
        let x0 = sho.get_scalar();
        let x1 = sho.get_scalar();
        let y = OneBased::<S::Storage>::create(|| sho.get_scalar());

        let C_W = (w * system.G_w) + (wprime * system.G_wprime);
        let mut I = system.G_V - (x0 * system.G_x0) - (x1 * system.G_x1);

        for (yn, G_yn) in y.iter().zip(system.G_y.iter()).take(S::NUM_ATTRS) {
            I -= yn * G_yn;
        }

        KeyPair { w, wprime, W, x0, x1, y, C_W, I }
    }
}
```

### Credential Issuance

```rust
impl KeyPair<ExpiringProfileKeyCredential> {
    pub fn create_blinded_expiring_profile_key_credential(
        &self,
        uid: uid_struct::UidStruct,
        public_key: profile_key_credential_request::PublicKey,
        ciphertext: profile_key_credential_request::Ciphertext,
        credential_expiration_time: Timestamp,
        sho: &mut Sho,
    ) -> BlindedExpiringProfileKeyCredentialWithSecretNonce {
        // Convert user ID to points
        let M = [uid.M1, uid.M2];

        // Generate random credential values
        let t = sho.get_scalar();
        let U = sho.get_point();

        // Compute MAC: V' = W + (x₀ + x₁·t)·U + Σ(yᵢ·Mᵢ)
        let mut Vprime = self.W + (self.x0 + self.x1 * t) * U;
        for (yn, Mn) in self.y.iter().zip(&M) {
            Vprime += yn * Mn;
        }

        // Add expiration time
        let params = SystemParams::get_hardcoded();
        let m5 = TimestampStruct::calc_m_from(credential_expiration_time);
        let M5 = m5 * params.G_m5;
        let Vprime_with_expiration = Vprime + (self.y[5] * M5);

        // Blind the credential with client's public key
        let rprime = sho.get_scalar();
        let R1 = rprime * RISTRETTO_BASEPOINT_POINT;
        let R2 = rprime * public_key.Y + Vprime_with_expiration;
        let S1 = R1 + (self.y[3] * ciphertext.D1) + (self.y[4] * ciphertext.E1);
        let S2 = R2 + (self.y[3] * ciphertext.D2) + (self.y[4] * ciphertext.E2);

        BlindedExpiringProfileKeyCredentialWithSecretNonce {
            rprime, t, U, S1, S2
        }
    }
}
```

This demonstrates **blind issuance**: the server creates a credential over encrypted attributes
without learning the plaintext values.

### Profile Key Credentials

Profile key credentials allow users to prove they have a valid profile key without revealing it:

**Use case**: A user wants to prove to a group server that they're a valid Signal user without
revealing their account ID.

**Attributes**:
1. User ID (ACI) - encrypted
2. Profile key - encrypted
3. Profile key version - encrypted
4. Expiration timestamp

**Protocol flow**:
1. Client creates blinded request containing encrypted attributes
2. Server issues credential over blinded attributes
3. Client unblinds and verifies the credential
4. Client creates presentation proof when joining a group
5. Group server verifies presentation without learning client's identity

### Receipt Credentials

Receipt credentials prove a user made a payment without linking requests:

```rust
impl KeyPair<ReceiptCredential> {
    pub fn create_blinded_receipt_credential(
        &self,
        public_key: receipt_credential_request::PublicKey,
        ciphertext: receipt_credential_request::Ciphertext,
        receipt_expiration_time: Timestamp,
        receipt_level: ReceiptLevel,
        sho: &mut Sho,
    ) -> BlindedReceiptCredentialWithSecretNonce {
        let params = SystemParams::get_hardcoded();
        let m1 = ReceiptStruct::calc_m1_from(
            receipt_expiration_time,
            receipt_level
        );
        let M = [m1 * params.G_m1];

        let (t, U, Vprime) = self.credential_core(&M, sho);

        // Blind with client's key
        let rprime = sho.get_scalar();
        let R1 = rprime * RISTRETTO_BASEPOINT_POINT;
        let R2 = rprime * public_key.Y + Vprime;
        let S1 = self.y[2] * ciphertext.D1 + R1;
        let S2 = self.y[2] * ciphertext.D2 + R2;

        BlindedReceiptCredentialWithSecretNonce {
            rprime, t, U, S1, S2
        }
    }
}
```

**Attributes**:
- Receipt serial number (blinded)
- Expiration time
- Receipt level (donation tier)

## 5. The zkcredential Abstraction

### Design Philosophy

While `zkgroup` provides domain-specific implementations, `zkcredential` is a **generic framework**
for building custom attribute-based credentials. It's designed for reusability and type safety.

Location: `/home/user/libsignal/rust/zkcredential/src/`

### Architecture Overview

The crate is organized into modules:

```
zkcredential/
├── attributes.rs      # Attribute types and encryption
├── credentials.rs     # Core credential types
├── issuance.rs       # Credential issuance
├── presentation.rs   # Credential presentation
├── endorsements.rs   # Lightweight tokens
└── sho.rs            # Hash utilities
```

### Attribute Types

zkcredential supports three kinds of attributes:

```rust
// 1. Public attributes (not hidden from anyone)
pub trait PublicAttribute {
    fn hash_into(&self, sho: &mut dyn ShoApi);
}

// Example implementations:
impl PublicAttribute for [u8] { /* hash bytes */ }
impl PublicAttribute for u64 { /* hash integer */ }

// 2. Hidden attributes (encrypted, two points)
pub trait Attribute {
    fn as_points(&self) -> [RistrettoPoint; 2];
}

// 3. Revealed attributes (blinded during issuance, revealed in presentation)
pub trait RevealedAttribute {
    fn as_point(&self) -> RistrettoPoint;
}
```

### Attribute Encryption

Attributes use **verifiable encryption** via key pairs:

```rust
pub struct KeyPair<D> {
    pub a1: Scalar,
    pub a2: Scalar,
    pub public_key: PublicKey<D>,
}

impl<D: Domain> KeyPair<D> {
    // Encrypt an attribute: E_A1 = a1·M1, E_A2 = a2·E_A1 + M2
    pub fn encrypt(&self, attr: &D::Attribute) -> Ciphertext<D> {
        let [M1, M2] = attr.as_points();
        let E_A1 = self.a1 * M1;
        let E_A2 = (self.a2 * E_A1) + M2;
        Ciphertext { E_A1, E_A2, domain: PhantomData }
    }

    // Decrypt to recover M2 (M1 must be recomputed and verified)
    pub fn decrypt_to_second_point(
        &self,
        ciphertext: &Ciphertext<D>,
    ) -> Result<RistrettoPoint, VerificationFailure> {
        if ciphertext.E_A1 == RISTRETTO_BASEPOINT_POINT {
            return Err(VerificationFailure);
        }
        Ok(ciphertext.E_A2 - self.a2 * ciphertext.E_A1)
    }
}
```

**Domain separation**: Each attribute type has its own `Domain` implementation:

```rust
pub trait Domain {
    type Attribute: Attribute;
    const ID: &'static str;  // Unique identifier
    fn G_a() -> [RistrettoPoint; 2];  // Generator points
}

// Example:
struct UserIdEncryption;
impl Domain for UserIdEncryption {
    type Attribute = UserId;
    const ID: &'static str = "Signal_UserIdEncryption_20231011";

    fn G_a() -> [RistrettoPoint; 2] {
        static STORAGE: OnceLock<[RistrettoPoint; 2]> = OnceLock::new();
        *derive_default_generator_points::<Self>(&STORAGE)
    }
}
```

### Credential System

```rust
pub struct CredentialKeyPair {
    private_key: CredentialPrivateKey,
    public_key: CredentialPublicKey,
}

struct CredentialPrivateKey {
    w: Scalar,
    wprime: Scalar,
    W: RistrettoPoint,
    x0: Scalar,
    x1: Scalar,
    y: [Scalar; NUM_SUPPORTED_ATTRS],  // Up to 7 attributes
}

pub struct CredentialPublicKey {
    C_W: RistrettoPoint,
    I: [RistrettoPoint; NUM_SUPPORTED_ATTRS - 1],
}
```

The public key contains multiple `I` values, one for each possible number of attributes. This
optimizes presentation proofs—a credential with 3 attributes uses a smaller proof than one with 7.

### Credential Issuance

The issuance protocol uses a builder pattern:

```rust
let proof = IssuanceProofBuilder::new(b"MyCredential_v1")
    .add_public_attribute(&timestamp)
    .add_attribute(&encrypted_user_id)
    .add_attribute(&encrypted_profile_key)
    .issue(&server_key_pair, randomness)?;

// Client side:
let credential = IssuanceProofBuilder::new(b"MyCredential_v1")
    .add_public_attribute(&timestamp)
    .add_attribute(&encrypted_user_id)
    .add_attribute(&encrypted_profile_key)
    .verify(proof, &server_public_key)?;
```

Internally, this creates a poksho proof demonstrating:
1. The server knows the private key corresponding to its public key
2. The credential's MAC is correctly computed over the attributes
3. All randomness is properly generated

### Credential Presentation

When using a credential, the client creates a presentation proof:

```rust
let presentation = PresentationProofBuilder::new(b"MyCredential_v1")
    .add_public_attribute(&timestamp)
    .add_attribute_with_key(&encrypted_user_id, &encryption_key)
    .add_attribute_with_key(&encrypted_profile_key, &encryption_key)
    .present(&credential, randomness)?;

// Verifying server:
PresentationProofVerifier::new(b"MyCredential_v1")
    .add_public_attribute(&timestamp)
    .add_attribute_with_key(&encrypted_user_id, &encryption_public_key)
    .add_attribute_with_key(&encrypted_profile_key, &encryption_public_key)
    .verify(presentation, &server_public_key)?;
```

The presentation proof demonstrates:
1. The client possesses a valid credential
2. The credential's attributes match the provided encrypted values
3. The encryption is correctly performed

**Critically**, the verifying server learns nothing about the plaintext attributes, only that they
match the encrypted forms.

## 6. Endorsements: Lightweight Alternatives

### Motivation

Full credential presentation proofs are powerful but computationally expensive. For scenarios where:
- No attributes need to be hidden from the verifying server
- Only one attribute needs to be hidden from the issuing server
- Tokens can be reused

Signal uses **endorsements**, a lighter-weight alternative based on 3HashSDHI and PrivacyPass.

Location: `/home/user/libsignal/rust/zkcredential/src/endorsements.rs`

### Endorsement Structure

```rust
pub struct Endorsement<Storage = RistrettoPoint> {
    R: Storage,  // Server's signature on a point
}
```

An endorsement is simply `R = sk' · E`, where:
- `sk'`: Server's derived signing key (depends on "tag info")
- `E`: Client's encrypted/blinded point

### Issuance Protocol

```rust
impl EndorsementResponse {
    pub fn issue(
        hidden_attribute_points: impl IntoIterator<Item = RistrettoPoint>,
        private_key: &ServerDerivedKeyPair,
        randomness: [u8; RANDOMNESS_LEN],
    ) -> EndorsementResponse {
        let points = Vec::from_iter(hidden_attribute_points);

        // Sign each point: R_i = sk' · E_i
        let R = points.iter()
            .map(|E_i| (private_key.sk_prime * E_i).compress())
            .collect();

        // Generate batch proof using random linear combination
        let weights = Self::generate_weights_for_proof(&private_key.public, &points, &R);
        let sum_E = points[0] + RistrettoPoint::multiscalar_mul(&weights, &points[1..]);
        let sum_R = private_key.sk_prime * sum_E;

        let statement = EndorsementResponse::proof_statement();
        // Proves: sum_R = sk' · sum_E and G = sk' · PK_prime
        let proof = statement.prove(/* ... */);

        EndorsementResponse { R, proof }
    }
}
```

The batch proof uses **random linear combinations** for efficiency: instead of proving each
signature individually, prove one combined signature. The weights prevent the server from cheating.

### Client Verification

```rust
let endorsements = response.receive(
    hidden_attribute_points,
    &server_public_key,
)?;
```

The client:
1. Verifies the batch proof
2. Decompresses all endorsement points
3. Returns both compressed (for storage) and decompressed (for operations) forms

### Token Generation

```rust
impl Endorsement {
    pub fn to_token(&self, client_key: &ClientDecryptionKey) -> Box<[u8]> {
        // Unblind: P = R · a_inv
        let P = self.R * client_key.a_inv;

        // Hash to create fixed-size token
        sha2::Sha256::digest(P.compress().as_bytes()).as_slice()[..TOKEN_LEN].into()
    }
}
```

This produces a 16-byte token that can be reused.

### Combining Endorsements

Endorsements support set operations:

```rust
// Combine multiple endorsements
let combined = Endorsement::combine([endorsement1, endorsement2, endorsement3]);

// Remove an endorsement
let subset = combined.remove(&endorsement2);
```

This works because point addition is homomorphic:
```
R_combined = R_1 + R_2 + R_3
           = sk'·E_1 + sk'·E_2 + sk'·E_3
           = sk'·(E_1 + E_2 + E_3)
```

### Group Send Endorsements

Signal's `GroupSendEndorsement` uses this framework:

```rust
// Server issues endorsements for all group members
let response = GroupSendEndorsementsResponse::issue(
    member_ciphertexts,
    &derived_key_pair,
    randomness,
);

// Client receives and validates
let endorsements = response.receive_with_service_ids(
    user_ids,
    now,
    &group_params,
    &root_public_key,
)?;

// Combine endorsements for multiple recipients
let combined = GroupSendEndorsement::combine(
    endorsements.iter().map(|e| e.decompressed)
);

// Generate token for verification
let token = combined.to_token(&group_params.uid_enc_key_pair);
let full_token = token.into_full_token(expiration);
```

**Tag info** includes the expiration timestamp, ensuring endorsements can only be used during their
validity period.

## 7. Real-World Usage

### Group Operations with Zero-Knowledge

When a user joins a Signal group, they must prove membership without revealing their identity to
the group server:

1. **Credential Request**: Client creates a blinded request containing their encrypted user ID
2. **Issuance**: Chat server (which knows the user's identity) issues a credential
3. **Presentation**: Client generates a presentation proof for the group server
4. **Verification**: Group server validates the proof without learning the user's ID

This architecture ensures the chat server and group server cannot collude to track users.

### Receipt Verification Flow

For donation receipts:

1. Payment processor notifies chat server of successful payment
2. Chat server issues receipt credential with serial number, level, and expiration
3. Client stores credential locally
4. When making requests requiring donation status, client presents credential
5. Server verifies presentation without linking it to the original payment

Each presentation uses fresh randomness, preventing correlation across requests.

### Profile Key Distribution

Profile key credentials enable secure profile sharing:

1. User generates profile key locally
2. User creates credential request with encrypted profile key
3. Server issues credential over the encrypted key
4. User presents credential to group members
5. Group members verify and decrypt the profile key

The group server never learns profile keys, maintaining end-to-end encryption.

### Performance Characteristics

**Computational costs**:
- Credential issuance: ~10-20ms (depends on attribute count)
- Credential presentation: ~15-30ms
- Endorsement issuance (100 members): ~50-100ms
- Endorsement verification: ~2-5ms per token

**Bandwidth**:
- Credential: ~160 bytes + ~64 bytes per attribute
- Presentation proof: ~200 bytes + ~64 bytes per attribute
- Endorsement: ~32 bytes (compressed point)
- Token: 16 bytes

**Trade-offs**:
- Credentials: Higher cost, maximum privacy, single-use
- Endorsements: Lower cost, less privacy, reusable tokens

## 8. Security Analysis

### Cryptographic Assumptions

All zero-knowledge systems in libsignal rely on:

1. **Decisional Diffie-Hellman (DDH)**: Cannot distinguish (g, g^a, g^b, g^ab) from (g, g^a, g^b, g^c)
2. **Discrete Logarithm (DLog)**: Cannot compute a from g^a
3. **Random Oracle Model**: Hash functions behave like random oracles

These are well-established assumptions in the Ristretto group.

### Attack Resistance

**Replay attacks**: Prevented by including fresh randomness in every proof/presentation

**Credential sharing**: Prevented by binding credentials to encrypted attributes that can't be
transferred

**Forgery**: Computationally infeasible without server's private key (DLog hardness)

**Malleability**: Fiat-Shamir transform ensures proofs are non-malleable

**Timing attacks**: Constant-time operations used for secret-dependent branches

### Privacy Guarantees

**Unlinkability**: Two presentations of the same credential are computationally indistinguishable

**Attribute hiding**: Encrypted attributes reveal no information to verifying server

**Issuer privacy**: Blind issuance prevents issuing server from learning blinded attributes

## Conclusion

libsignal's zero-knowledge infrastructure demonstrates a sophisticated layered architecture:

- **poksho** provides foundational Schnorr proof capabilities
- **zkgroup** delivers domain-specific credential systems for Signal's features
- **zkcredential** offers a generic, reusable framework for new use cases

Together, these components enable Signal to implement advanced privacy features while maintaining
strong security guarantees. The use of Ristretto groups, careful protocol design, and defensive
programming practices (like self-verification of proofs) ensure robust protection against both
cryptographic and implementation-level attacks.

As Signal evolves, this zero-knowledge foundation provides the flexibility to add new privacy-
preserving features without compromising on performance or security.

---

**Further Reading**:
- Chase, Perrin, Zaverucha: "The Signal Private Group System" (2019)
- Boneh & Shoup: "A Graduate Course in Applied Cryptography" Chapter 19
- PrivacyPass specification: https://privacypass.github.io
- Ristretto group specification: https://ristretto.group
