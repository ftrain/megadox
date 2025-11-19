# Libsignal Encyclopedia - Research Data Summary

This document contains comprehensive research findings from automated analysis of the libsignal codebase.

## Codebase Statistics

- **Total Rust Crates**: 24 workspace members
- **Source Files**: 1,000+ Rust files
- **Test Files**: 124+ files with unit tests, 26 integration test files
- **Benchmark Files**: 18 performance benchmark files
- **Lines of Code**: Hundreds of thousands across Rust, Java, Swift, TypeScript
- **Git Commits**: 3,683 commits (2020-2025)
- **Contributors**: 200+ individuals

## Top Contributors (by commit count)
1. Jordan Rose - 1,958 commits
2. Jack Lloyd - 483 commits  
3. Alex Konradi - 284 commits
4. Alex Bakon - 249 commits
5. moiseev-signal - 170 commits

## Major Milestones

### 2020: Foundation
- January 18: Initial commit (poksho library)
- April 20: Pivot to Signal Protocol Rust implementation  
- October 16: Repository consolidation (monorepo created)
- October 23: Node.js bridge added
- November 3: Java integration
- December: Swift integration complete

### 2021: Expansion
- February: Async bridge support
- October: zkgroup integration
- Throughout: Protocol maturation

### 2022-2023: Network Services
- May 2022: CDS2/CDSI contact discovery
- February 2023: SVR2 (PIN-based recovery)
- September 2023: libsignal-net architecture
- October 2023: CDSI production deployment

### 2023-2025: Post-Quantum Era
- September 2023: PQXDH announcement
- May 2023: Kyber integration begins
- March 2024: SPQR (post-quantum ratchet)
- June 2024: X3DH deprecated, PQXDH mandatory
- April 2024: libcrux migration (formally verified crypto)
- October 2024: SPQR mandatory

## Cryptographic Implementations

### Primitives
- **Symmetric**: AES-256 (CBC, CTR, GCM, GCM-SIV)
- **Hash**: SHA-256, SHA-512, HMAC-SHA256
- **KDF**: HKDF, PBKDF2
- **Curves**: Curve25519 (X25519 DH, Ed25519 signatures)
- **AEAD**: AES-GCM, AES-GCM-SIV, ChaCha20-Poly1305
- **HPKE**: RFC 9180 implementation
- **Post-Quantum**: Kyber768, Kyber1024, ML-KEM1024

### Protocol Stack
- **Session Establishment**: X3DH → PQXDH
- **Message Encryption**: Double Ratchet + SPQR
- **Group Messaging**: Sender Keys
- **Metadata Protection**: Sealed Sender v1 & v2
- **Zero-Knowledge**: zkgroup (Schnorr proofs, Ristretto)

## Architecture Layers

### Rust Core (24 Crates)
1. **libsignal-core**: Shared types and utilities
2. **libsignal-protocol**: Signal Protocol implementation
3. **signal-crypto**: Cryptographic primitives
4. **attest**: SGX/HSM attestation
5. **device-transfer**: Device-to-device migration
6. **media**: MP4 sanitization
7. **message-backup**: Backup format and validation
8. **usernames**: Username hashing and proof
9. **zkgroup**: Zero-knowledge group operations
10. **zkcredential**: Generic ZK credentials
11. **poksho**: Proof-of-knowledge library
12. **keytrans**: Key transparency
13. **account-keys**: Account key operations
14. **libsignal-net**: Network services core
15. **libsignal-net-infra**: Network infrastructure
16. **libsignal-net-chat**: Chat service
17. **libsignal-net-grpc**: gRPC integration
18. **svr2/svr3/svrb**: Secure value recovery
19. **bridge/shared**: Bridge infrastructure
20. **bridge/shared/types**: Type conversions
21. **bridge/ffi**: Swift C FFI
22. **bridge/jni**: Java JNI  
23. **bridge/node**: Node.js Neon
24. **cli-utils**: Command-line utilities

### Language Bindings
- **Java**: JNI bridge → Android (ARM64, x86_64) + Desktop + Server
- **Swift**: FFI bridge → iOS/macOS (x86_64, ARM64)
- **Node.js**: Neon bridge → npm package with prebuilds

## Testing Infrastructure

### Test Types
- **Unit Tests**: Inline with `#[test]` macros (124+ files)
- **Integration Tests**: Dedicated `tests/` directories (26 files)
- **Property-Based**: proptest for invariant testing
- **Fuzz Tests**: libfuzzer coverage-guided fuzzing
- **Cross-Version**: Protocol compatibility testing
- **Cross-Language**: Java, Swift, Node.js test suites
- **Benchmarks**: Criterion performance tests (18 files)

### Test Data
- AES-GCM: 256 test vectors from Cryptofuzz/Wycheproof
- KEM: Kyber768/1024 and ML-KEM test data
- Attestation: SGX DCAP test fixtures
- Message Backup: JSON/protobuf test cases
- Protocol: Session, group, sealed sender test scenarios

## Build System

### Cross-Compilation Targets
- **Android**: arm64-v8a, armeabi-v7a, x86_64, x86
- **iOS**: x86_64-apple-ios, aarch64-apple-ios (sim + device)
- **macOS**: x86_64-apple-darwin, aarch64-apple-darwin
- **Linux**: x86_64, aarch64
- **Windows**: x86_64-pc-windows-msvc, aarch64-pc-windows-msvc

### CI/CD
- **GitHub Actions**: 11 workflows
- **Matrix Testing**: Rust (nightly + stable), Java, Swift, Node.js
- **Platform Coverage**: Ubuntu, macOS, Windows
- **Architecture Coverage**: x86_64, ARM64, i686 (32-bit)
- **Release Automation**: Version sync, artifact publishing
- **Code Size Tracking**: Android binary size monitoring

### Reproducible Builds
- Docker environments for Java/Android
- Pinned dependencies and toolchains
- Signal-hosted APT mirrors
- Binary verification in CI

## Historical Context

### Origins (2010-2013)
- 2010: Whisper Systems founded (TextSecure, RedPhone)
- 2011: Twitter acquisition & open-source release
- 2013: Open Whisper Systems founded by Moxie Marlinspike
- 2013: Signal Protocol development begins (Moxie + Trevor Perrin)

### Mass Adoption (2014-2016)
- Feb 2014: Axolotl Protocol (later renamed Signal Protocol)
- Nov 2014: WhatsApp partnership announced
- Apr 2016: WhatsApp encryption rollout (1+ billion users)

### Foundation Era (2018-)
- Feb 2018: Signal Foundation established
- Brian Acton invests $50M
- 501(c)(3) nonprofit structure

### Rust Era (2020-)
- Jan 2020: libsignal repository created
- Oct 2020: Monorepo consolidation
- 2020-2021: Multi-platform bridge architecture
- 2021-2023: Network services expansion
- 2023-2025: Post-quantum transition

## Security Research

### Academic Analysis
- "A Formal Security Analysis of the Signal Messaging Protocol" (2016, Journal of Cryptology)
- Formal verification using ProVerif and CryptoVerif
- Multiple security audit reports
- Peer-reviewed protocol specifications

### Known Security Audits
- Signal Protocol audit (2016): Cryptographically sound
- Ongoing academic research and formal analysis
- Public vulnerability disclosure process

## Community

### Communication Channels
- **Mailing List (Historical)**: whispersystems@lists.riseup.net
- **Discourse Forum**: whispersystems.discoursehosting.net
- **GitHub**: Primary development platform
- **Issue Tracker**: Public bug reports and feature requests

### Development Practices
- **Code Review**: All changes reviewed
- **Testing**: Comprehensive test coverage required
- **Documentation**: Inline docs, specifications
- **Versioning**: Synchronized across platforms
- **Release Process**: Automated with version validation

## Mobile Hardware Context

### 2013-2014 Constraints
- **CPU**: ARMv7 32-bit, 800 MHz - 1.5 GHz, no crypto extensions
- **RAM**: 512 MB - 1 GB
- **Encryption Impact**: 4x read slowdown without hardware acceleration
- **Battery**: Limited capacity forced efficiency focus

### Modern Capabilities (2025)
- **CPU**: ARMv8 64-bit, 2-3 GHz, dedicated crypto accelerators
- **RAM**: 6-12 GB
- **Storage**: 128-512 GB
- **Performance**: Enables post-quantum crypto, ZK proofs, rich media

## Evolution Patterns

### Crypto Library Migrations
- **curve25519-dalek**: v2 → v3 → v4 (various forks)
- **BoringSSL**: Integrated 2023 for performance
- **RustCrypto**: Gradual adoption of pure-Rust implementations
- **libcrux**: 2024 migration for formally verified PQ crypto

### Architectural Shifts
- **2020**: C/Java → Rust with bridges
- **2020-2021**: Async/await adoption
- **2021**: zkgroup integration
- **2023**: Network services stack (libsignal-net)
- **2023-2025**: Post-quantum cryptography
- **2024-2025**: Rust 2024 edition, modern patterns

### Protocol Upgrades
- **X3DH → PQXDH**: Hybrid classical + post-quantum key agreement
- **Double Ratchet → SPQR**: Post-quantum forward secrecy
- **Sealed Sender v1 → v2**: ChaCha20, optimized structure
- **Protobuf Evolution**: Continuous protocol extensions

## File Count Summary

- Rust source files: 1,000+
- Java files: 300+
- Swift files: 150+
- TypeScript files: 200+
- Test files: 150+
- Protobuf definitions: 30+
- Build scripts: 50+
- CI workflows: 11

## Dependencies

### Key Rust Crates
- **Crypto**: aes, sha2, hmac, hkdf, chacha20poly1305, curve25519-dalek, ed25519-dalek, x25519-dalek, libcrux-ml-kem, boring
- **Async**: tokio, futures
- **Networking**: hyper, rustls, tungstenite, h2
- **Serialization**: prost (protobuf), serde
- **Testing**: proptest, criterion, libfuzzer-sys
- **Bridge**: jni, neon

### Custom Forks
- boring/boring-sys: signal-v4.18.0
- curve25519-dalek: signal-curve25519-4.1.3
- tungstenite: signal-v0.27.0

---

This research data forms the foundation for the encyclopedic documentation of libsignal.
