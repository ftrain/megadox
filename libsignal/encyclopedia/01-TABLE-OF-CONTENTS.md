# The libsignal Encyclopedia
## Table of Contents

---

## Front Matter

- **00 - Introduction**
  - About This Work
  - Historical Context and Significance
  - Scope of This Encyclopedia
  - Original Hardware Context
  - Philosophical Foundations
  - How to Use This Encyclopedia
  - Acknowledgments
  - Conventions Used

---

## Part I: History and Context

### Chapter 1: Historical Timeline (2013-2025)
- 1.1 The Privacy Revolution (2013)
  - Edward Snowden Revelations
  - Birth of Open Whisper Systems
  - Community Formation
- 1.2 Early Development (2013-2014)
  - TextSecure and RedPhone
  - The Axolotl Protocol
  - Academic Foundations
- 1.3 Mass Adoption (2014-2016)
  - WhatsApp Integration
  - Facebook Messenger Adoption
  - One Billion Encrypted Users
- 1.4 Signal Foundation Era (2018-2020)
  - Brian Acton's $50M Investment
  - Nonprofit Structure
  - Independence and Mission
- 1.5 The Rust Rewrite (2020)
  - Rationale for Rust
  - Repository Consolidation
  - Monorepo Architecture
- 1.6 Modern Era (2021-2023)
  - Zero-Knowledge Groups
  - Network Services
  - Protocol Maturation
- 1.7 Post-Quantum Transition (2023-2025)
  - PQXDH Announcement
  - Kyber Integration
  - SPQR and Mandatory PQ
- 1.8 Community and Contributors
  - Top Contributors
  - Development Patterns
  - Mailing Lists and Forums

---

## Part II: Cryptographic Foundations

### Chapter 2: Cryptographic Primitives
- 2.1 Elliptic Curve Cryptography
  - Curve25519 / X25519
  - Ed25519 Signatures
  - XEdDSA for Signal
  - Implementation (`rust/core/src/curve/`)
- 2.2 Symmetric Encryption
  - AES-256-CBC
  - AES-256-CTR
  - AES-256-GCM
  - AES-256-GCM-SIV
  - Implementation (`rust/crypto/src/`)
- 2.3 Hash Functions and Key Derivation
  - SHA-256 and SHA-512
  - HMAC-SHA256
  - HKDF (Key Derivation)
  - Implementation Details
- 2.4 Hybrid Public Key Encryption (HPKE)
  - RFC 9180 Implementation
  - DHKEM(X25519, HKDF-SHA256)
  - Use in Sealed Sender
  - Code Walkthrough
- 2.5 Post-Quantum Cryptography
  - ML-KEM (formerly Kyber)
  - Key Encapsulation Mechanisms
  - libcrux Integration
  - Test Vectors and Validation

### Chapter 3: The Signal Protocol
- 3.1 Protocol Overview
  - Design Goals
  - Security Properties
  - Academic Analysis
- 3.2 X3DH (Extended Triple Diffie-Hellman)
  - Key Agreement Protocol
  - PreKey Bundles
  - Identity Keys, Signed PreKeys, One-Time PreKeys
  - Implementation (`rust/protocol/src/session.rs`)
- 3.3 PQXDH (Post-Quantum X3DH)
  - Hybrid Key Agreement
  - Kyber Integration
  - Migration from X3DH
  - Security Analysis
- 3.4 The Double Ratchet
  - Symmetric-Key Ratchet
  - Diffie-Hellman Ratchet
  - Forward Secrecy and Backward Secrecy
  - Implementation (`rust/protocol/src/ratchet.rs`)
- 3.5 SPQR (Signal Post-Quantum Ratchet)
  - Post-Quantum Forward Secrecy
  - Integration with Double Ratchet
  - Out-of-Order Message Handling
  - Code Analysis
- 3.6 Message Encryption
  - Session Cipher
  - Message Format and Serialization
  - MAC and Authentication
  - Implementation (`rust/protocol/src/session_cipher.rs`)

### Chapter 4: Group Messaging
- 4.1 Sender Keys
  - Group Key Distribution
  - Sender Key Messages
  - Rotation and Security
  - Implementation (`rust/protocol/src/sender_keys.rs`)
- 4.2 Group Cipher
  - Encryption and Decryption
  - Multi-Recipient Messages
  - Code Walkthrough

### Chapter 5: Sealed Sender
- 5.1 Metadata Protection
  - Anonymous Sender
  - Server Certificates
  - Trust Model
- 5.2 Multi-Layer Encryption
  - Ephemeral Layer
  - Static Layer
  - Implementation (`rust/protocol/src/sealed_sender.rs`)
- 5.3 Multi-Recipient Sealed Sender
  - Optimized Group Messages
  - Shared Payload
  - Per-Recipient Headers

### Chapter 6: Zero-Knowledge Cryptography
- 6.1 zkgroup Overview
  - Group Credentials
  - Profile Keys
  - Receipt Credentials
- 6.2 Ristretto Group Operations
  - Curve25519-based Group
  - Point Compression
  - Implementation (`rust/zkgroup/src/crypto/`)
- 6.3 Schnorr Signatures and Proofs
  - poksho Library
  - Proof Generation
  - Verification
  - Code Analysis (`rust/poksho/src/`)
- 6.4 zkcredential System
  - Attribute-based Credentials
  - Issuance and Presentation
  - Endorsements
  - Implementation (`rust/zkcredential/src/`)

---

## Part III: System Architecture

### Chapter 7: Codebase Structure
- 7.1 Workspace Organization
  - 24 Rust Crates
  - Dependency Graph
  - Module Boundaries
- 7.2 Core Libraries
  - libsignal-core: Shared types
  - libsignal-protocol: Signal Protocol
  - signal-crypto: Cryptographic primitives
- 7.3 Specialized Libraries
  - attest: SGX/HSM attestation
  - device-transfer: Device migration
  - media: MP4 sanitization
  - message-backup: Backup format
  - usernames: Username handling
  - keytrans: Key transparency
- 7.4 Network Stack
  - libsignal-net: Core networking
  - libsignal-net-infra: Infrastructure
  - libsignal-net-chat: Chat services
  - libsignal-net-grpc: gRPC integration

### Chapter 8: Language Bindings
- 8.1 Bridge Architecture
  - Unified Bridge Design
  - Procedural Macros
  - Type Conversion
  - Error Handling
- 8.2 Java/JNI Bridge
  - JNI Entry Points
  - Object Handle Management
  - Build System (Gradle)
  - Code Generation
  - Walkthrough (`rust/bridge/jni/`)
- 8.3 Swift/FFI Bridge
  - C FFI Layer
  - cbindgen Header Generation
  - Swift Wrappers
  - Resource Management
  - Walkthrough (`rust/bridge/ffi/`)
- 8.4 Node.js/Neon Bridge
  - Neon Framework
  - Async/Promise Support
  - TypeScript Definitions
  - NPM Packaging
  - Walkthrough (`rust/bridge/node/`)

### Chapter 9: Build System and Infrastructure
- 9.1 Cargo Workspace
  - Multi-Crate Management
  - Shared Dependencies
  - Feature Flags
- 9.2 Cross-Compilation
  - Android (ARM, x86)
  - iOS (x86_64, ARM64)
  - Desktop (Linux, macOS, Windows)
  - Server Deployments
- 9.3 CI/CD Pipeline
  - GitHub Actions Workflows
  - Matrix Testing
  - Release Automation
  - Code Size Tracking
- 9.4 Reproducible Builds
  - Docker Environments
  - Dependency Pinning
  - Build Verification

### Chapter 10: Testing Architecture
- 10.1 Testing Philosophy
  - Unit Tests
  - Integration Tests
  - Property-Based Testing
  - Fuzz Testing
- 10.2 Test Infrastructure
  - Test Utilities
  - Mock Stores
  - Test Data and Fixtures
- 10.3 Cross-Language Testing
  - Java Tests
  - Swift Tests
  - Node.js Tests
  - Protocol Compatibility Tests
- 10.4 Continuous Testing
  - CI Test Matrix
  - Slow Tests and Nightly Runs
  - Non-Hermetic Tests

---

## Part IV: Network Services

### Chapter 11: Contact Discovery (CDSI)
- 11.1 Privacy-Preserving Contact Discovery
  - Rate Limiting
  - Oblivious Requests
  - SGX Enclaves
- 11.2 Protocol Flow
  - Token Retrieval
  - Encrypted Requests
  - Attestation Verification
- 11.3 Implementation
  - Code Analysis (`rust/net/src/cdsi.rs`)
  - Client Integration

### Chapter 12: Secure Value Recovery (SVR)
- 12.1 SVR Evolution
  - SVR2 (PIN-based)
  - SVR3 (Raft-based)
  - SVR-B (Next Generation)
- 12.2 Cryptographic Protocol
  - OPRF (Oblivious PRF)
  - Enclave Architecture
  - Backup and Restore Flow
- 12.3 Implementation
  - Code Walkthrough (`rust/svr/`)
  - Testing Strategy

### Chapter 13: Chat Services
- 13.1 Authenticated Chat
  - WebSocket Connections
  - Noise Protocol Handshake
  - Request/Response Protocol
- 13.2 Service Architecture
  - ChatConnection
  - ChatService
  - Listener Pattern
- 13.3 Implementation
  - Code Analysis (`rust/net/chat/`)
  - Async Patterns

### Chapter 14: Key Transparency
- 14.1 Verifiable Key Directory
  - Merkle Trees
  - Consistency Proofs
  - VRF for Monitoring
- 14.2 Protocol Operations
  - Search and Verify
  - Monitoring Keys
  - Audit Process
- 14.3 Implementation
  - Code Walkthrough (`rust/keytrans/`)

---

## Part V: Literate Programming Deep-Dives

### Chapter 15: Session Establishment
- Full walkthrough of session creation
- Code flow with annotations
- Key operations explained
- Error handling patterns

### Chapter 16: Message Encryption Flow
- Encrypting a message end-to-end
- Ratchet advancement
- Key derivation steps
- Serialization format

### Chapter 17: Group Message Handling
- Sender key distribution
- Group encryption
- Multi-recipient optimization
- Code analysis

### Chapter 18: Sealed Sender Operation
- Creating anonymous messages
- Certificate validation
- Decryption flow
- Privacy guarantees

### Chapter 19: Zero-Knowledge Proof Generation
- Credential issuance
- Proof creation with poksho
- Verification process
- Security properties

### Chapter 20: Network Request Flow
- Connection establishment
- Noise handshake
- Authenticated requests
- Error recovery

### Chapter 21: Message Backup Format
- Backup structure
- Encryption scheme
- Validation framework
- Export/import flow

### Chapter 22: Device Transfer Protocol
- Secure device pairing
- Data encryption
- Transfer process
- Implementation details

---

## Part VI: Evolution and Patterns

### Chapter 23: Architectural Evolution
- 23.1 Major Refactorings Timeline
  - Monorepo Creation (2020)
  - Bridge Unification (2020-2021)
  - Network Stack Introduction (2023)
  - Post-Quantum Migration (2023-2025)
- 23.2 Crypto Library Migrations
  - curve25519-dalek Evolution
  - BoringSSL Integration
  - RustCrypto Adoption
  - libcrux for ML-KEM
- 23.3 Protocol Upgrades
  - Axolotl to Signal Protocol
  - X3DH to PQXDH
  - Double Ratchet to SPQR
  - Sealed Sender v1 to v2
- 23.4 Async/Await Adoption
  - Early Callback Patterns
  - Future-based APIs
  - tokio Integration
  - Cross-Language Async

### Chapter 24: Development Patterns
- 24.1 Error Handling Evolution
  - Early Result Types
  - Bridge Error Conversion
  - Specialized Error Types
  - Error Context Enrichment
- 24.2 Type Safety Improvements
  - NewType Patterns
  - Generic Bridge Functions
  - Handle Management
  - Lifetime Annotations
- 24.3 Testing Maturity
  - Unit Test Growth
  - Property-Based Testing Addition
  - Fuzz Testing Integration
  - Cross-Version Testing
- 24.4 Code Organization
  - Module Structure Evolution
  - Workspace Dependency Management
  - Feature Flag Strategy
  - API Surface Design

### Chapter 25: Lessons Learned
- 25.1 What Worked Well
  - Rust's Memory Safety
  - Bridge Macro Approach
  - Property-Based Testing
  - Post-Quantum Proactivity
- 25.2 Challenges Overcome
  - Cross-Platform Complexity
  - Async Across Languages
  - Reproducible Builds
  - Dependency Management
- 25.3 Community Insights
  - Contributor Patterns
  - Code Review Evolution
  - Documentation Practices
  - Release Management

---

## Part VII: Reference Materials

### Appendix A: Comprehensive Glossary
- Cryptographic Terms
- Protocol Concepts
- Rust Terminology
- Signal-Specific Terms

### Appendix B: Complete API Reference
- Core Types
- Protocol Functions
- Crypto Operations
- Network Services

### Appendix C: Protocol Specifications
- X3DH Specification
- PQXDH Specification
- Double Ratchet Specification
- SPQR Specification
- Sealed Sender Specification

### Appendix D: Test Vector Catalog
- Cryptographic Test Vectors
- Protocol Test Cases
- Cross-Version Test Data
- Fuzz Corpus

### Appendix E: Build and Deployment Guide
- Setting Up Development Environment
- Building for Each Platform
- Running Tests
- Creating Releases
- Docker Usage

### Appendix F: Security Audits and Analysis
- Academic Papers
- Formal Verification Studies
- Security Audit Reports
- Known Issues and Mitigations

### Appendix G: Bibliography
- Academic Papers
- Technical Specifications
- Blog Posts and Articles
- Historical Documents

### Appendix H: Complete Index
- Concept Index
- Function Index
- File Index
- Contributor Index

---

## Colophon

- **Total Pages**: ~1,500 estimated
- **Code Samples**: 500+
- **Diagrams**: 100+
- **Cross-References**: 2,000+
- **Index Entries**: 3,000+

**Created**: November 2025
**Version**: 1.0
**Codebase Version**: libsignal 0.86.5

---

