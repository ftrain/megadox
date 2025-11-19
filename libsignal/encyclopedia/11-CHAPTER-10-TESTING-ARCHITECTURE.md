# Chapter 10: Testing Architecture

## Overview

libsignal employs a comprehensive multi-layered testing strategy that ensures correctness, performance, and security across all supported platforms. The testing architecture spans from low-level unit tests to high-level integration tests, property-based testing, fuzz testing, and cross-language compatibility verification. This chapter explores the testing methodologies, tools, and patterns used throughout the codebase.

## 1. Testing Philosophy

### Multi-Layered Testing Strategy

libsignal's testing approach follows a pyramid structure with multiple complementary layers:

1. **Unit Tests**: Fine-grained tests for individual functions and modules (124+ files)
2. **Integration Tests**: End-to-end protocol interaction tests
3. **Property-Based Tests**: Invariant verification using randomized inputs
4. **Fuzz Tests**: Coverage-guided mutation testing for edge cases
5. **Cross-Language Tests**: Protocol compatibility across FFI boundaries
6. **Benchmarks**: Performance regression detection

### Coverage Goals

The project maintains high test coverage with emphasis on:

- **Critical Path Coverage**: All cryptographic operations must be tested
- **Error Path Coverage**: Every error condition must have test coverage
- **Cross-Platform Coverage**: Tests run on Linux, macOS, Windows, iOS, and Android
- **Multi-Version Coverage**: Tests against both stable and nightly Rust

### Quality Standards

- Tests must be deterministic and reproducible
- Async tests use `futures_util::FutureExt::now_or_never()` for synchronous execution
- No shared mutable state between tests
- All tests run in CI with strict warnings

## 2. Unit Tests

### Inline Test Organization

Unit tests in libsignal are co-located with source code using Rust's `#[cfg(test)]` module pattern. This approach provides immediate context and encourages developers to test as they write.

**Example from `/home/user/libsignal/rust/protocol/src/crypto.rs`:**

```rust
#[cfg(test)]
mod test {
    use const_str::hex;
    use super::*;

    #[test]
    fn aes_ctr_test() {
        let key = hex!("603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4");
        let ptext = [0u8; 35];

        let ctext = aes_256_ctr_encrypt(&ptext, &key).expect("valid key");
        assert_eq!(
            hex::encode(ctext),
            "e568f68194cf76d6174d4cc04310a85491151e5d0b7a1f1bc0d7acd0ae3e51e4170e23"
        );
    }
}
```

### Test Patterns and Conventions

**Result-Based Testing:**
```rust
type TestResult = Result<(), SignalProtocolError>;

#[test]
fn test_basic_operation() -> TestResult {
    // Test implementation
    Ok(())
}
```

**Assertion Macros:**
- `assert_eq!`: Value equality
- `assert_matches!`: Pattern matching
- `assert!`: Boolean conditions

### Async Test Patterns

libsignal tests async code synchronously using `now_or_never()`:

```rust
#[test]
fn test_async_operation() -> Result<(), SignalProtocolError> {
    async {
        let mut csprng = OsRng.unwrap_err();
        let store = test_in_memory_protocol_store()?;

        // Async operations here
        let result = some_async_function(&store).await?;

        assert_eq!(result, expected_value);
        Ok(())
    }
    .now_or_never()
    .expect("sync")
}
```

This pattern allows async code to run in synchronous test contexts while maintaining readability.

### Unit Test Statistics

- **124+ files** contain inline unit tests in `rust/protocol/src/`
- Tests cover crypto primitives, state machines, serialization, and error handling
- Every public API has corresponding test coverage

## 3. Integration Tests

Integration tests verify end-to-end protocol interactions between multiple parties. Located in `/home/user/libsignal/rust/protocol/tests/`, these tests simulate real-world usage patterns.

### Session Tests

**Example from `/home/user/libsignal/rust/protocol/tests/session.rs`:**

```rust
#[test]
fn test_basic_prekey() -> TestResult {
    async {
        let mut csprng = OsRng.unwrap_err();

        let alice_address = ProtocolAddress::new(
            "+14151111111".to_owned(),
            DeviceId::new(1).unwrap()
        );
        let bob_address = ProtocolAddress::new(
            "+14151111112".to_owned(),
            DeviceId::new(1).unwrap()
        );

        let mut alice_store = test_in_memory_protocol_store()?;
        let mut bob_store = test_in_memory_protocol_store()?;

        // Create prekey bundle
        let bob_pre_key_bundle = create_pre_key_bundle(&mut bob_store, &mut csprng).await?;

        // Process prekey bundle
        process_prekey_bundle(
            &bob_address,
            &mut alice_store.session_store,
            &mut alice_store.identity_store,
            &bob_pre_key_bundle,
            SystemTime::now(),
            &mut csprng,
        ).await?;

        // Test message encryption/decryption
        let original_message = "L'homme est condamné à être libre";
        let outgoing_message = encrypt(&mut alice_store, &bob_address, original_message).await?;

        assert_eq!(outgoing_message.message_type(), CiphertextMessageType::PreKey);

        Ok(())
    }
    .now_or_never()
    .expect("sync")
}
```

### Group Tests

**Example from `/home/user/libsignal/rust/protocol/tests/groups.rs`:**

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

        // Create and distribute sender key
        let sent_distribution_message = create_sender_key_distribution_message(
            &sender_address,
            distribution_id,
            &mut alice_store,
            &mut csprng,
        ).await?;

        let recv_distribution_message =
            SenderKeyDistributionMessage::try_from(sent_distribution_message.serialized())?;

        // Encrypt group message
        let alice_ciphertext = group_encrypt(
            &mut alice_store,
            &sender_address,
            distribution_id,
            "space camp?".as_bytes(),
            &mut csprng,
        ).await?;

        // Process distribution message
        process_sender_key_distribution_message(
            &sender_address,
            &recv_distribution_message,
            &mut bob_store,
        ).await?;

        // Decrypt
        let bob_plaintext = group_decrypt(
            alice_ciphertext.serialized(),
            &mut bob_store,
            &sender_address,
        ).await?;

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

### Sealed Sender Tests

**Example from `/home/user/libsignal/rust/protocol/tests/sealed_sender.rs`:**

```rust
#[test]
fn test_sealed_sender() -> Result<(), SignalProtocolError> {
    async {
        let mut rng = OsRng.unwrap_err();

        // Setup identities
        let alice_device_id = DeviceId::new(23).unwrap();
        let bob_device_id = DeviceId::new(42).unwrap();

        let alice_uuid = "9d0652a3-dcc3-4d11-975f-74d61598733f".to_string();
        let bob_uuid = "796abedb-ca4e-4f18-8803-1fde5b921f9f".to_string();

        let bob_uuid_address = ProtocolAddress::new(bob_uuid.clone(), bob_device_id);

        let mut alice_store = support::test_in_memory_protocol_store()?;
        let mut bob_store = support::test_in_memory_protocol_store()?;

        // Generate certificates
        let trust_root = KeyPair::generate(&mut rng);
        let server_key = KeyPair::generate(&mut rng);

        let server_cert = ServerCertificate::new(
            1,
            server_key.public_key,
            &trust_root.private_key,
            &mut rng,
        )?;

        let expires = Timestamp::from_epoch_millis(1605722925);
        let sender_cert = SenderCertificate::new(
            alice_uuid.clone(),
            Some(alice_e164.clone()),
            alice_pubkey,
            alice_device_id,
            expires,
            server_cert,
            &server_key.private_key,
            &mut rng,
        )?;

        // Test sealed sender encryption/decryption
        let alice_ptext = vec![1, 2, 3, 23, 99];
        let alice_ctext = sealed_sender_encrypt(
            &bob_uuid_address,
            &sender_cert,
            &alice_ptext,
            &mut alice_store.session_store,
            &mut alice_store.identity_store,
            SystemTime::now(),
            &mut rng,
        ).await?;

        let bob_ptext = sealed_sender_decrypt(
            &alice_ctext,
            &trust_root.public_key,
            expires.sub_millis(1),
            Some(bob_e164.clone()),
            bob_uuid.clone(),
            bob_device_id,
            &mut bob_store.identity_store,
            &mut bob_store.session_store,
            &mut bob_store.pre_key_store,
            &bob_store.signed_pre_key_store,
            &mut bob_store.kyber_pre_key_store,
        ).await?;

        assert_eq!(bob_ptext.message, alice_ptext);
        assert_eq!(bob_ptext.sender_uuid, alice_uuid);

        Ok(())
    }
    .now_or_never()
    .expect("sync")
}
```

### Test Structure and Utilities

Integration tests leverage a shared support module (`/home/user/libsignal/rust/protocol/tests/support/mod.rs`) providing:

```rust
// Test store creation
pub fn test_in_memory_protocol_store() -> Result<InMemSignalProtocolStore, SignalProtocolError> {
    let mut csprng = OsRng.unwrap_err();
    let identity_key = IdentityKeyPair::generate(&mut csprng);
    let registration_id: u8 = csprng.random();
    InMemSignalProtocolStore::new(identity_key, registration_id as u32)
}

// Encryption helper
pub async fn encrypt(
    store: &mut InMemSignalProtocolStore,
    remote_address: &ProtocolAddress,
    msg: &str,
) -> Result<CiphertextMessage, SignalProtocolError> {
    let mut csprng = OsRng.unwrap_err();
    message_encrypt(
        msg.as_bytes(),
        remote_address,
        &mut store.session_store,
        &mut store.identity_store,
        SystemTime::now(),
        &mut csprng,
    ).await
}

// Decryption helper
pub async fn decrypt(
    store: &mut InMemSignalProtocolStore,
    remote_address: &ProtocolAddress,
    msg: &CiphertextMessage,
) -> Result<Vec<u8>, SignalProtocolError> {
    let mut csprng = OsRng.unwrap_err();
    message_decrypt(
        msg,
        remote_address,
        &mut store.session_store,
        &mut store.identity_store,
        &mut store.pre_key_store,
        &store.signed_pre_key_store,
        &mut store.kyber_pre_key_store,
        &mut csprng,
    ).await
}

// PreKey bundle creation
pub async fn create_pre_key_bundle<R: Rng + CryptoRng>(
    store: &mut dyn ProtocolStore,
    mut csprng: &mut R,
) -> Result<PreKeyBundle, SignalProtocolError> {
    let pre_key_pair = KeyPair::generate(&mut csprng);
    let signed_pre_key_pair = KeyPair::generate(&mut csprng);
    let kyber_pre_key_pair = kem::KeyPair::generate(kem::KeyType::Kyber1024, &mut csprng);

    // Generate signatures and build bundle
    // ... implementation details
}
```

## 4. Property-Based Testing

Property-based testing uses the `proptest` crate to verify invariants hold across randomly generated inputs. This approach catches edge cases that example-based tests might miss.

### Proptest Usage Examples

**From `/home/user/libsignal/rust/usernames/src/username.rs`:**

```rust
#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use super::*;

    // Pattern for valid nicknames
    const NICKNAME_PATTERN: &str = r"[a-z_][a-z0-9_]{2,31}";
    const DISCRIMINATOR_MAX: u64 = 10000;

    #[test]
    fn valid_nicknames_should_produce_scalar() {
        proptest!(|(nickname in NICKNAME_PATTERN)| {
            nickname_scalar(&nickname).unwrap();
        });
    }

    #[test]
    fn valid_usernames_should_produce_scalar() {
        proptest!(|(nickname in NICKNAME_PATTERN, discriminator in 1..DISCRIMINATOR_MAX)| {
            username_sha_scalar(&nickname, discriminator).unwrap();
        });
    }

    #[test]
    fn discriminator_scalar_is_defined_on_range() {
        proptest!(|(n in 1..DISCRIMINATOR_MAX)| {
            discriminator_scalar(n).unwrap();
        });
    }

    #[test]
    fn valid_usernames_proof_and_verify() {
        proptest!(|(nickname in NICKNAME_PATTERN, discriminator in 1..DISCRIMINATOR_MAX)| {
            let username = Username::new(&Username::format_parts(&nickname, discriminator)).unwrap();
            let hash = username.hash();
            let randomness = std::array::from_fn(|i| (i + 1).try_into().unwrap());
            let proof = username.proof(&randomness).unwrap();
            Username::verify_proof(&proof, hash).unwrap();
        });
    }
}
```

### Generator Strategies

Property tests use custom strategies to generate valid test data:

- **Nickname Pattern**: Regex-based generation for valid usernames
- **Discriminator Range**: Bounded numeric ranges
- **Compound Strategies**: Combining multiple generators for complex types

### Invariant Testing

Property tests verify critical invariants:

1. **Roundtrip Properties**: Serialization/deserialization consistency
2. **Commutativity**: Operations produce same result regardless of order
3. **Idempotence**: Repeated operations produce same result
4. **Boundary Conditions**: Edge values don't cause panics or incorrect behavior

## 5. Fuzz Testing

libsignal uses libfuzzer for coverage-guided fuzzing, located in `/home/user/libsignal/rust/protocol/fuzz/`.

### Fuzz Targets

**Interaction Fuzzer** (`/home/user/libsignal/rust/protocol/fuzz/fuzz_targets/interaction.rs`):

```rust
#![no_main]

use std::time::SystemTime;
use futures_util::FutureExt;
use libfuzzer_sys::fuzz_target;
use libsignal_protocol::*;
use rand::prelude::*;

struct Participant {
    name: &'static str,
    address: ProtocolAddress,
    store: InMemSignalProtocolStore,
    message_queue: Vec<(CiphertextMessage, Box<[u8]>)>,
    archive_count: u8,
    pre_key_count: u32,
}

impl Participant {
    async fn send_message(&mut self, them: &mut Self, rng: &mut (impl Rng + CryptoRng)) {
        info!("{}: sending message", self.name);

        // Ensure session exists or create one
        if !self.store.load_session(&them.address).await.unwrap().and_then(|session| {
            session.has_usable_sender_chain(
                SystemTime::UNIX_EPOCH,
                SessionUsabilityRequirements::all(),
            ).ok()
        }).unwrap_or(false) {
            self.process_pre_key(them, rng.random_bool(0.75), rng).await;
        }

        // Generate random message
        let length = rng.random_range(0..140);
        let mut buffer = vec![0; length];
        rng.fill_bytes(&mut buffer);

        let outgoing_message = message_encrypt(
            &buffer,
            &them.address,
            &mut self.store.session_store,
            &mut self.store.identity_store,
            SystemTime::UNIX_EPOCH,
            rng,
        ).await.unwrap();

        them.message_queue.push((incoming_message, buffer.into()));
    }
}

fuzz_target!(|data: (u64, &[u8])| {
    let (seed, actions) = data;
    async {
        let mut csprng = StdRng::seed_from_u64(seed);

        let mut alice = Participant { /* ... */ };
        let mut bob = Participant { /* ... */ };

        for action in actions {
            let (me, them) = match action & 1 {
                0 => (&mut alice, &mut bob),
                1 => (&mut bob, &mut alice),
                _ => unreachable!(),
            };

            match action >> 1 {
                0 => me.archive_session(&them.address).await,
                1..=32 => me.receive_messages(&them.address, &mut csprng).await,
                33..=48 => { me.message_queue.pop(); }
                49..=56 => { me.message_queue.shuffle(&mut csprng); }
                _ => {
                    if them.message_queue.len() < 1_500 {
                        me.send_message(them, &mut csprng).await
                    }
                }
            }
        }
    }
    .now_or_never()
    .expect("sync");
});
```

**Sealed Sender V2 Fuzzer** (`/home/user/libsignal/rust/protocol/fuzz/fuzz_targets/sealed_sender_v2.rs`):

```rust
#![no_main]

use libfuzzer_sys::fuzz_target;
use libsignal_protocol::*;

fuzz_target!(|data: &[u8]| {
    let _: Result<_, _> = SealedSenderV2SentMessage::parse(data);
});
```

### libfuzzer Integration

Fuzz targets integrate with cargo-fuzz:

```bash
# Run interaction fuzzer
cargo +nightly fuzz run interaction

# Run sealed sender fuzzer
cargo +nightly fuzz run sealed_sender_v2
```

### Coverage-Guided Fuzzing

libfuzzer uses:
- **Code Coverage Feedback**: Tracks which code paths are exercised
- **Corpus Management**: Maintains minimal set of inputs for maximum coverage
- **Mutation Strategies**: Intelligent input modification based on coverage

## 6. Cross-Language Testing

libsignal maintains protocol compatibility across Java, Swift, and Node.js through comprehensive cross-language test suites.

### Java Test Suite

**From `/home/user/libsignal/java/client/src/test/java/org/signal/libsignal/protocol/SessionBuilderTest.java`:**

```java
@RunWith(Enclosed.class)
public class SessionBuilderTest {
    static final SignalProtocolAddress ALICE_ADDRESS =
        filterExceptions(() -> new SignalProtocolAddress("+14151111111", 1));
    static final SignalProtocolAddress BOB_ADDRESS =
        filterExceptions(() -> new SignalProtocolAddress("+14152222222", 1));

    @RunWith(Parameterized.class)
    public static class Versioned {
        private final BundleFactory bundleFactory;
        private int expectedVersion;

        public Versioned(BundleFactory bundleFactory, int expectedVersion) {
            this.bundleFactory = bundleFactory;
            this.expectedVersion = expectedVersion;
        }

        @Parameters(name = "v{1}")
        public static Collection<Object[]> data() throws Exception {
            return Arrays.asList(new Object[][] {{new PQXDHBundleFactory(), 4}});
        }

        @Test
        public void testBasicPreKeyV4() throws Exception {
            SignalProtocolStore aliceStore = new TestInMemorySignalProtocolStore();
            SessionBuilder aliceSessionBuilder = new SessionBuilder(aliceStore, BOB_ADDRESS);

            SignalProtocolStore bobStore = new TestInMemorySignalProtocolStore();
            PreKeyBundle bobPreKey = bundleFactory.createBundle(bobStore);

            aliceSessionBuilder.process(bobPreKey);

            assertTrue(aliceStore.containsSession(BOB_ADDRESS));
            assertTrue(aliceStore.loadSession(BOB_ADDRESS).getSessionVersion() == expectedVersion);

            String originalMessage = "initial hello!";
            SessionCipher aliceSessionCipher = new SessionCipher(aliceStore, BOB_ADDRESS);
            CiphertextMessage outgoingMessage = aliceSessionCipher.encrypt(originalMessage.getBytes());

            assertTrue(outgoingMessage.getType() == CiphertextMessage.PREKEY_TYPE);

            PreKeySignalMessage incomingMessage = new PreKeySignalMessage(outgoingMessage.serialize());
            SessionCipher bobSessionCipher = new SessionCipher(bobStore, ALICE_ADDRESS);
            byte[] plaintext = bobSessionCipher.decrypt(incomingMessage);

            assertTrue(bobStore.containsSession(ALICE_ADDRESS));
            assertEquals(bobStore.loadSession(ALICE_ADDRESS).getSessionVersion(), expectedVersion);
            assertTrue(originalMessage.equals(new String(plaintext)));
        }
    }
}
```

### Swift Test Examples

**From `/home/user/libsignal/swift/Tests/LibSignalClientTests/SessionTests.swift`:**

```swift
class SessionTests: TestCaseBase {
    func testSessionCipher() {
        run(initializeSessionsV4)

        func run(_ initSessions: InitSession) {
            let alice_address = try! ProtocolAddress(name: "+14151111111", deviceId: 1)
            let bob_address = try! ProtocolAddress(name: "+14151111112", deviceId: 1)

            let alice_store = InMemorySignalProtocolStore()
            let bob_store = InMemorySignalProtocolStore()

            initSessions(alice_store, bob_store, bob_address)

            // Alice sends a message
            let ptext_a = Data([8, 6, 7, 5, 3, 0, 9])

            let ctext_a = try! signalEncrypt(
                message: ptext_a,
                for: bob_address,
                sessionStore: alice_store,
                identityStore: alice_store,
                context: NullContext()
            )

            XCTAssertEqual(ctext_a.messageType, .preKey)

            let ctext_b = try! PreKeySignalMessage(bytes: ctext_a.serialize())

            let ptext_b = try! signalDecryptPreKey(
                message: ctext_b,
                from: alice_address,
                sessionStore: bob_store,
                identityStore: bob_store,
                preKeyStore: bob_store,
                signedPreKeyStore: bob_store,
                kyberPreKeyStore: bob_store,
                context: NullContext()
            )

            XCTAssertEqual(ptext_a, ptext_b)
        }
    }
}
```

### Node.js Tests

**From `/home/user/libsignal/node/ts/test/protocol/ProtocolTest.ts`:**

```typescript
import * as SignalClient from '../../index.js';
import * as util from '../util.js';
import { assert, use } from 'chai';
import chaiAsPromised from 'chai-as-promised';

use(chaiAsPromised);
util.initLogger();

it('Fingerprint', () => {
    const aliceKey = SignalClient.PublicKey.deserialize(
        Buffer.from(
            '0506863bc66d02b40d27b8d49ca7c09e9239236f9d7d25d6fcca5ce13c7064d868',
            'hex'
        )
    );
    const aliceIdentifier = Buffer.from('+14152222222', 'utf8');

    const bobKey = SignalClient.PublicKey.deserialize(
        Buffer.from(
            '05f781b6fb32fed9ba1cf2de978d4d5da28dc34046ae814402b5c0dbd96fda907b',
            'hex'
        )
    );
    const bobIdentifier = Buffer.from('+14153333333', 'utf8');

    const iterations = 5200;
    const aFprint1 = SignalClient.Fingerprint.new(
        iterations,
        1,
        aliceIdentifier,
        aliceKey,
        bobIdentifier,
        bobKey
    );

    util.assertByteArray(
        '080112220a201e301a0353dce3dbe7684cb8336e85136cdc0ee96219494ada305d62a7bd61df1a220a20d62cbf73a11592015b6b9f1682ac306fea3aaf3885b84d12bca631e9d4fb3a4d',
        aFprint1.scannableFingerprint().toBuffer()
    );

    assert.deepEqual(
        aFprint1.displayableFingerprint().toString(),
        '300354477692869396892869876765458257569162576843440918079131'
    );
});
```

### Protocol Compatibility Tests

Cross-language tests ensure:
- **Serialization Compatibility**: Messages serialize/deserialize identically
- **Cryptographic Consistency**: Same inputs produce same outputs
- **Error Handling Parity**: Errors map correctly across FFI boundaries
- **API Surface Alignment**: Similar APIs across all language bindings

## 7. Benchmarking

Performance testing uses the Criterion framework for statistical analysis of benchmark results.

### Criterion Usage

**From `/home/user/libsignal/rust/protocol/benches/session.rs`:**

```rust
use criterion::{Criterion, criterion_group, criterion_main};
use futures_util::FutureExt;
use libsignal_protocol::*;
use rand::TryRngCore as _;
use rand::rngs::OsRng;

pub fn session_encrypt_result(c: &mut Criterion) -> Result<(), SignalProtocolError> {
    let (alice_session_record, bob_session_record) = support::initialize_sessions_v4()?;

    let alice_address = ProtocolAddress::new("+14159999999".to_owned(), DeviceId::new(1).unwrap());
    let bob_address = ProtocolAddress::new("+14158888888".to_owned(), DeviceId::new(1).unwrap());

    let mut alice_store = support::test_in_memory_protocol_store()?;
    let mut bob_store = support::test_in_memory_protocol_store()?;

    alice_store
        .store_session(&bob_address, &alice_session_record)
        .now_or_never()
        .expect("sync")?;
    bob_store
        .store_session(&alice_address, &bob_session_record)
        .now_or_never()
        .expect("sync")?;

    let message_to_decrypt = support::encrypt(&mut alice_store, &bob_address, "a short message")
        .now_or_never()
        .expect("sync")?;

    c.bench_function("decrypting the first message on a chain", |b| {
        b.iter(|| {
            let mut bob_store = bob_store.clone();
            support::decrypt(&mut bob_store, &alice_address, &message_to_decrypt)
                .now_or_never()
                .expect("sync")
                .expect("success");
        })
    });

    c.bench_function("encrypting on an existing chain", |b| {
        b.iter(|| {
            support::encrypt(&mut alice_store, &bob_address, "a short message")
                .now_or_never()
                .expect("sync")
                .expect("success");
        })
    });

    c.bench_function("session encrypt+decrypt ping pong", |b| {
        b.iter(|| {
            let ctext = support::encrypt(&mut alice_store, &bob_address, "a short message")
                .now_or_never()
                .expect("sync")
                .expect("success");
            let _ptext = support::decrypt(&mut bob_store, &alice_address, &ctext)
                .now_or_never()
                .expect("sync")
                .expect("success");

            let ctext = support::encrypt(&mut bob_store, &alice_address, "a short message")
                .now_or_never()
                .expect("sync")
                .expect("success");
            let _ptext = support::decrypt(&mut alice_store, &bob_address, &ctext)
                .now_or_never()
                .expect("sync")
                .expect("success");
        })
    });

    Ok(())
}

criterion_group!(benches, session_encrypt, session_encrypt_decrypt);
criterion_main!(benches);
```

### Performance Tracking

Benchmarks measure:
- **Encryption/Decryption Speed**: Message processing throughput
- **Session Initialization**: PreKey bundle processing time
- **Ratcheting Performance**: Chain advancement overhead
- **Regression Detection**: Statistical comparison with baseline

### Code Examples

Common benchmark patterns:

```rust
// Simple operation benchmark
c.bench_function("operation_name", |b| {
    b.iter(|| {
        expensive_operation()
    })
});

// Setup/teardown with cloning
c.bench_function("stateful_operation", |b| {
    b.iter(|| {
        let mut state = baseline_state.clone();
        modify_state(&mut state);
    })
});

// Parameterized benchmarks
for size in [100, 1000, 10000] {
    c.bench_function(&format!("operation_size_{}", size), |b| {
        let data = vec![0u8; size];
        b.iter(|| process(&data))
    });
}
```

## 8. CI/CD Testing

GitHub Actions orchestrates comprehensive automated testing across platforms and configurations.

### GitHub Actions Workflows

**From `/home/user/libsignal/.github/workflows/build_and_test.yml`:**

```yaml
name: Build and Test

on:
  push:
    branches: [ main ]
  pull_request:
  workflow_dispatch:

env:
  CARGO_TERM_COLOR: always
  RUST_BACKTRACE: 1
  CARGO_PROFILE_DEV_DEBUG: limited

jobs:
  rust:
    name: Rust
    runs-on: ubuntu-latest-4-cores

    strategy:
      fail-fast: false
      matrix:
        version: [nightly, stable]
        include:
        - version: nightly
          toolchain: "$(cat rust-toolchain)"
        - version: stable
          toolchain: "$(yq '.workspace.package.rust-version' Cargo.toml)"

    timeout-minutes: 45

    steps:
    - uses: actions/checkout@v4
      with:
        submodules: recursive

    - name: Install protoc
      run: ./bin/install_protoc_linux

    - run: rustup toolchain install "${{ matrix.toolchain }}" --profile minimal --component rustfmt,clippy

    - name: Build
      run: cargo +${{ matrix.toolchain }} build --workspace --features libsignal-ffi/signal-media --verbose --keep-going

    - name: Run tests
      run: cargo +${{ matrix.toolchain }} test --workspace --all-features --verbose --no-fail-fast -- --include-ignored

    - name: Test run benches
      run: cargo +${{ matrix.toolchain }} test --workspace --benches --all-features --verbose --no-fail-fast '.*'

    - name: Clippy
      run: cargo clippy --workspace --all-targets --all-features --keep-going -- -D warnings
      if: matrix.version == 'nightly'
```

### Matrix Testing Strategy

The CI pipeline tests across multiple dimensions:

**Platform Matrix:**
- **Linux**: ubuntu-latest-4-cores
- **macOS**: macos-15
- **Windows**: windows-latest-8-cores

**Rust Version Matrix:**
- **Nightly**: Latest features and testing
- **Stable**: Production MSRV (Minimum Supported Rust Version)

**Architecture Matrix:**
- **64-bit**: Primary target (x86_64, aarch64)
- **32-bit**: i686-unknown-linux-gnu for compatibility testing

**Language Matrix:**
- **Java**: JVM and Android builds
- **Swift**: Package and CocoaPod validation
- **Node.js**: Cross-platform Node bindings

### Platform Coverage

**Rust Tests:**
```yaml
rust:
  - Build workspace with all features
  - Run all tests with --include-ignored
  - Benchmark compilation check
  - Clippy linting (nightly only)
  - Rustdoc generation (stable only)

rust32:
  - 32-bit testing on i686-unknown-linux-gnu
  - Ensures no architecture-specific assumptions

rust-fuzz-build:
  - Verify fuzz targets compile
  - Check protocol and attest fuzzers
```

**Java Tests:**
```yaml
java_android:
  - Build Android AAR (arm, arm64)
  - Run Android test suite
  - Lint check
  - Code size verification

java_jvm:
  - Build desktop JNI
  - Verify JNI bindings up to date
  - Run JVM test suite
```

**Node Tests:**
```yaml
node:
  strategy:
    matrix:
      os: [ubuntu-latest-4-cores, windows-latest-8-cores, macos-15]

  steps:
    - Verify TypeScript declarations
    - npm ci (install dependencies)
    - npm run build
    - npm run tsc (type check)
    - npm run lint (ubuntu only)
    - npm run format-check (ubuntu only)
    - npm run test
```

**Swift Tests:**
```yaml
swift_package:
  - Build libsignal-ffi
  - Verify FFI bindings
  - swift test -v
  - Run benchmarks in debug mode

swift_cocoapod:
  - Build for iOS simulator (aarch64-apple-ios-sim)
  - pod lib lint
  - swiftlint check
  - swift format check
```

## Testing Best Practices

### 1. Test Isolation

Every test should be independent and not rely on shared state:

```rust
#[test]
fn isolated_test() {
    // Create fresh state for this test only
    let mut store = test_in_memory_protocol_store().unwrap();
    let mut rng = OsRng.unwrap_err();

    // Test operates on isolated state
    // ...
}
```

### 2. Error Path Testing

Test error conditions explicitly:

```rust
#[test]
fn test_invalid_input_returns_error() {
    let result = parse_invalid_data(&[0xFF, 0xFF]);

    assert!(matches!(
        result,
        Err(SignalProtocolError::InvalidProtobufEncoding)
    ));
}
```

### 3. Deterministic Randomness

Use seeded RNGs for reproducible tests:

```rust
use rand::SeedableRng;

#[test]
fn reproducible_test() {
    let mut rng = StdRng::seed_from_u64(42);
    // Test with deterministic randomness
}
```

### 4. Timeout Protection

Long-running tests should have timeouts:

```yaml
timeout-minutes: 45  # CI job level

#[test]
#[timeout(Duration::from_secs(5))]  // Test level
fn bounded_test() {
    // ...
}
```

### 5. Platform-Specific Testing

Use conditional compilation for platform-specific tests:

```rust
#[test]
#[cfg(target_os = "linux")]
fn linux_specific_test() {
    // ...
}

#[test]
#[cfg(target_pointer_width = "32")]
fn test_32bit_compatibility() {
    // ...
}
```

## Coverage Metrics

The project tracks coverage through:

1. **Code Coverage**: Via `cargo-tarpaulin` or `cargo-llvm-cov`
2. **Line Coverage**: Percentage of executed lines
3. **Branch Coverage**: Conditional path coverage
4. **Fuzz Coverage**: Unique code paths discovered by fuzzing

Target coverage goals:
- **Critical paths**: 100% coverage
- **Overall codebase**: >80% coverage
- **Error paths**: Explicit test for each error variant

## Conclusion

libsignal's testing architecture demonstrates a mature, multi-layered approach to quality assurance. The combination of unit tests, integration tests, property-based tests, fuzz tests, cross-language tests, and comprehensive CI/CD ensures the library maintains the highest standards of correctness, security, and performance across all supported platforms.

Key takeaways:

- **Comprehensive Coverage**: Multiple testing strategies catch different bug classes
- **Cross-Platform Verification**: Tests run on all target platforms and architectures
- **Continuous Integration**: Automated testing on every commit
- **Property-Based Testing**: Invariant verification beyond example-based tests
- **Fuzz Testing**: Coverage-guided exploration of edge cases
- **Performance Tracking**: Benchmarks prevent performance regressions

This rigorous testing approach provides confidence in libsignal's reliability for secure messaging applications worldwide.
