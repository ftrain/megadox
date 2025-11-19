# Chapter 6: Network Services
## Contact Discovery, Secure Value Recovery, Chat, and Key Transparency

---

## Introduction

Signal's network services represent the infrastructure layer that connects clients to the Signal ecosystem while maintaining the platform's commitment to privacy and security. This chapter explores four critical network service subsystems:

1. **libsignal-net Architecture**: The unified networking layer built on `tokio` async runtime
2. **Contact Discovery Service (CDSI)**: Privacy-preserving contact discovery using SGX enclaves
3. **Secure Value Recovery (SVR)**: PIN-based secret backup with forward secrecy
4. **Chat Services**: WebSocket-based messaging with Noise protocol encryption
5. **Key Transparency**: Verifiable public key infrastructure with VRF-based monitoring

These services evolved from separate implementations into a unified architecture in 2023-2024, reflecting Signal's maturation from a startup project into critical infrastructure serving hundreds of millions of users.

---

## 6.1 The libsignal-net Architecture

### Historical Context

Prior to 2023, Signal's network operations were implemented separately in each client platform (iOS, Android, Desktop). The `libsignal-net` project unified these implementations into a shared Rust codebase, providing:

- **Consistent behavior** across all platforms
- **Shared connection management** and route failover
- **Common attestation logic** for SGX enclave verification
- **Unified DNS resolution** with DoH (DNS-over-HTTPS) support

### Directory Structure

The networking stack is organized into three main components:

```
rust/net/
├── infra/          # libsignal-net-infra: Transport infrastructure
│   ├── dns/        # DNS resolution (UDP, DoH, system resolver)
│   ├── tcp_ssl/    # TLS connection establishment
│   ├── ws/         # WebSocket protocol implementation
│   └── route/      # Connection routing and failover
├── chat/           # libsignal-net-chat: Chat service client
└── src/            # libsignal-net: Service implementations
    ├── cdsi.rs     # Contact Discovery
    ├── svr.rs      # Secure Value Recovery
    ├── svrb.rs     # Next-gen SVR with forward secrecy
    └── chat.rs     # Chat WebSocket client
```

**Key Insight**: The three-layer architecture separates transport concerns (infra), service protocols (src), and high-level APIs (chat). This enables:
- Testing services with mock transports
- Reusing transport logic across services
- Platform-specific optimizations in the infrastructure layer

### Connection Management

From `/home/user/libsignal/rust/net/infra/src/lib.rs`:

```rust
pub struct ConnectionParams {
    /// High-level classification of the route (mostly for logging)
    pub route_type: RouteType,
    /// Host name used in the HTTP headers.
    pub http_host: Arc<str>,
    /// Prefix prepended to the path of all HTTP requests.
    pub path_prefix: Option<&'static str>,
    /// If present, differentiates HTTP responses that actually come from the remote endpoint
    pub connection_confirmation_header: Option<HeaderName>,
    /// Transport-level connection configuration
    pub transport: TransportConnectionParams,
}

pub struct TransportConnectionParams {
    /// Host name to be used in the TLS handshake SNI field.
    pub sni: Arc<str>,
    /// Host name used for DNS resolution.
    pub tcp_host: Host<Arc<str>>,
    /// Port to connect to.
    pub port: NonZeroU16,
    /// Trusted certificates for this connection.
    pub certs: RootCertificates,
}
```

**Design Pattern**: Separation of HTTP-layer configuration (`ConnectionParams`) from transport-layer configuration (`TransportConnectionParams`). This enables:

1. **Domain Fronting**: Different values for `http_host`, `sni`, and `tcp_host` allow censorship circumvention
2. **Connection Confirmation**: The `connection_confirmation_header` prevents MITM attacks by verifying responses came from Signal servers
3. **Certificate Pinning**: Platform-specific root certificates via `RootCertificates`

### Async Patterns with Tokio

libsignal-net is built on the tokio async runtime, using modern Rust patterns:

```rust
/// Recommended WebSocket configuration
pub const RECOMMENDED_WS_CONFIG: ws::Config = {
    ws::Config {
        local_idle_timeout: WS_KEEP_ALIVE_INTERVAL,      // 30 seconds
        remote_idle_ping_timeout: WS_KEEP_ALIVE_INTERVAL, // 30 seconds
        remote_idle_disconnect_timeout: WS_MAX_IDLE_INTERVAL, // 60 seconds
    }
};
```

**Timeout Philosophy**: Three-tier timeout system:
- **Local idle**: Client sends pings if no activity
- **Remote idle ping**: Expect pong responses from server
- **Remote idle disconnect**: Maximum time without any server activity

This prevents zombie connections while tolerating network fluctuations.

### Route Types and Failover

From the `RouteType` enum:

```rust
pub enum RouteType {
    /// Direct connection to the service.
    Direct,
    /// Connection over the Google proxy
    ProxyF,
    /// Connection over the Fastly proxy
    ProxyG,
    /// Connection over a custom TLS proxy
    TlsProxy,
    /// Connection over a SOCKS proxy
    SocksProxy,
}
```

**Censorship Resistance**: Signal supports multiple connection methods:
1. Try direct connection first (fastest)
2. Fall back to Google/Fastly domain fronting (defeats SNI-based blocking)
3. Support SOCKS/TLS proxies (user-configured circumvention)

---

## 6.2 Contact Discovery Service (CDSI)

### Privacy Problem

Traditional contact discovery has a privacy problem: revealing your entire contact list to the server. Signal's solution uses **Intel SGX enclaves** to ensure the server cannot observe queries.

### Architecture Overview

```
Client                         SGX Enclave                    Signal Server
  |                                 |                               |
  |---(1) WebSocket Connection------------------------------------->|
  |                                 |                               |
  |<--(2) Attestation Evidence-------------------------------------|
  |                                 |                               |
  |---(3) Verify Attestation)----->|                               |
  |                                 |                               |
  |---(4) Noise Handshake)-------->|                               |
  |                                 |                               |
  |<--(5) Noise Handshake)---------| ============================  |
  |                                 |    Noise encrypted channel    |
  |---(6) Encrypted Query)-------->| ============================  |
  |                                 |                               |
  |<--(7) Token)-------------------| (Server cannot see contents)  |
  |                                 |                               |
  |---(8) Token Ack)-------------->|                               |
  |                                 |                               |
  |<--(9) Encrypted Results)-------|                               |
```

### Protocol Flow

From `/home/user/libsignal/rust/net/src/cdsi.rs`:

```rust
pub struct LookupRequest {
    pub new_e164s: Vec<E164>,           // Phone numbers to look up
    pub prev_e164s: Vec<E164>,          // Previously queried numbers
    pub acis_and_access_keys: Vec<AciAndAccessKey>, // Known ACIs to check
    pub token: Box<[u8]>,               // Rate-limiting token
}

pub struct LookupResponse {
    pub records: Vec<LookupResponseEntry>,
    pub debug_permits_used: i32,
}

pub struct LookupResponseEntry {
    pub e164: E164,
    pub aci: Option<Aci>,  // Account Identifier (UUID)
    pub pni: Option<Pni>,  // Phone Number Identifier (UUID)
}
```

**Request Optimization**: The `prev_e164s` field enables incremental queries. If the client previously queried numbers, it can tell the enclave "I already know about these, only give me updates."

### SGX Attestation

The attestation process verifies the enclave is running genuine Intel SGX hardware with expected code. From `/home/user/libsignal/rust/attest/src/dcap.rs`:

```rust
pub fn verify_remote_attestation(
    evidence_bytes: &[u8],
    endorsement_bytes: &[u8],
    expected_mrenclave: &MREnclave,
    acceptable_sw_advisories: &[&str],
    current_time: SystemTime,
) -> Result<HashMap<String, Vec<u8>>, AttestationError>
```

**Verification Steps** (from DCAP attestation):

1. **Verify signature chain**: Quote → PCK certificate → Intel root
2. **Check revocation**: No keys in the chain have been revoked
3. **Verify Quoting Enclave**: The QE is from Intel and up-to-date
4. **Check TCB status**: Platform Trusted Computing Base is current
5. **Match MRENCLAVE**: The enclave code hash matches expected value

The MRENCLAVE is a SHA-256 hash of the enclave code. Signal publishes expected MRENCLAVEs in the app, ensuring the server runs only audited code.

### Serialization Format

CDSI uses efficient binary serialization:

```rust
trait FixedLengthSerializable {
    const SERIALIZED_LEN: usize;
    fn serialize_into(&self, target: &mut [u8]);
}

impl FixedLengthSerializable for E164 {
    const SERIALIZED_LEN: usize = 8;  // 8 bytes for phone number
    fn serialize_into(&self, target: &mut [u8]) {
        target.copy_from_slice(&self.to_be_bytes())
    }
}

impl FixedLengthSerializable for AciAndAccessKey {
    const SERIALIZED_LEN: usize = 32;  // 16 bytes UUID + 16 bytes key
    fn serialize_into(&self, target: &mut [u8]) {
        let (aci_bytes, access_key_bytes) = target.split_at_mut(16);
        Uuid::from(self.aci).serialize_into(aci_bytes);
        access_key_bytes.copy_from_slice(&self.access_key)
    }
}
```

**Optimization**: Fixed-length encoding enables constant-time operations and predictable memory allocation. A query with 1000 phone numbers is exactly `1000 * 8 = 8000` bytes.

### Error Handling

```rust
pub enum LookupError {
    /// SGX attestation failed.
    AttestationError(attest::enclave::Error),
    /// retry later
    RateLimited(RetryLater),
    /// request token was invalid
    InvalidToken,
    /// protocol error after establishing a connection
    EnclaveProtocol(AttestedProtocolError),
    /// websocket error
    WebSocket(WebSocketError),
    /// request was invalid: {server_reason}
    InvalidArgument { server_reason: String },
}
```

**Close Code Mapping**: WebSocket close frames carry detailed errors:

```rust
enum CdsiCloseCode {
    InvalidArgument = 4003,
    RateLimitExceeded = 4008,
    ServerInternalError = 4013,
    ServerUnavailable = 4014,
    InvalidToken = 4101,
}
```

This enables clients to distinguish permanent failures (InvalidToken) from transient ones (ServerUnavailable).

### Connection Establishment

```rust
impl CdsiConnection {
    pub async fn connect_with(
        connection_resources: ConnectionResources<'_, impl WebSocketTransportConnectorFactory>,
        route_provider: impl RouteProvider<Route = UnresolvedWebsocketServiceRoute>,
        ws_config: crate::infra::ws::Config,
        params: &EndpointParams<'_, Cdsi>,
        auth: &Auth,
    ) -> Result<Self, LookupError> {
        let (connection, _route_info) = connection_resources
            .connect_attested_ws(route_provider, auth, ws_config, "cdsi".into(), params)
            .await?;
        Ok(Self(connection))
    }
}
```

**Abstraction Layers**:
- `ConnectionResources`: Provides DNS, transport, network change events
- `RouteProvider`: Supplies connection routes (direct, proxied, domain-fronted)
- `EndpointParams`: SGX enclave-specific configuration (MRENCLAVE, etc.)
- `Auth`: Username/password credentials

---

## 6.3 Secure Value Recovery (SVR)

### The PIN Problem

Users forget passwords. But for end-to-end encrypted systems, there's no password reset. Signal's solution: **Secure Value Recovery** backs up secrets using a user PIN, but the server cannot brute-force the PIN.

### Evolution: SVR2 → SVR3 → SVR-B

**SVR2** (2020-2023): OPRF-based PIN verification in SGX enclaves
- Used Oblivious Pseudorandom Function (OPRF)
- Single enclave, no replication
- Limited to ~10 tries before lockout

**SVR3** (2023-2024): Raft consensus for reliability
- Multiple replicas using Raft protocol
- Better availability and durability
- Still OPRF-based

**SVR-B** (2024-present): Forward secrecy with PPSS
- Uses **PPSS** (Privacy-Preserving Secret Sharing)
- Forward secrecy: old backups decrypt even if future PIN compromised
- Migration-friendly architecture

### SVR-B Architecture

From `/home/user/libsignal/rust/net/src/svrb.rs`:

```rust
pub async fn store_backup<B: traits::Backup + traits::Prepare, R: traits::Remove>(
    current_svrbs: &[B],        // New enclave instances
    previous_svrbs: &[R],       // Old instances to remove from
    backup_key: &BackupKey,     // Derived from account entropy
    previous_backup_data: BackupPreviousSecretDataRef<'_>,
) -> Result<BackupStoreResponse, Error>
```

**Migration Strategy**: When Signal deploys new SVR enclaves:
1. Write secrets to `current_svrbs` (new instances)
2. Delete from `previous_svrbs` (old instances)
3. Metadata includes keys for **both** old and new backups
4. Client can restore from either until migration completes

### PPSS Protocol

The PPSS (Privacy-Preserving Secret Sharing) protocol uses:

```rust
fn create_backup<SvrB: traits::Prepare, R: Rng + CryptoRng>(
    svrb: &SvrB,
    backup_key: &BackupKey,
    rng: &mut R,
) -> (Backup4, [u8; 32]) {
    let password_salt = random_32b(rng);
    let password_key = backup_key.derive_forward_secrecy_password(&password_salt).0;
    (svrb.prepare(&password_key), password_salt)
}
```

**Key Derivation**:
```
BackupKey (from account entropy)
    |
    +--> derive_forward_secrecy_password(salt) --> Password for PPSS
    |
    +--> derive_forward_secrecy_encryption_key(salt) --> AES-256 key
```

### Forward Secrecy Mechanism

```rust
pub struct BackupStoreResponse {
    pub forward_secrecy_token: BackupForwardSecrecyToken,
    pub next_backup_data: BackupPreviousSecretData,
    pub metadata: BackupFileMetadata,
}
```

**Encryption Dance**:

1. Generate random `forward_secrecy_token` (32 bytes)
2. Create PPSS backup with `password_salt_1`
3. Encrypt token with `AES-256-CTR` using `encryption_key_1 = derive(password_salt_1)`
4. Store encrypted token in metadata
5. For next backup, create new PPSS with `password_salt_2`
6. Metadata now contains encrypted tokens for **both** salts

```rust
fn aes_256_ctr_encrypt_hmacsha256(
    ek: &BackupForwardSecrecyEncryptionKey,
    iv: &[u8; IV_SIZE],
    ptext: &[u8],
) -> Vec<u8> {
    let mut aes = Aes256Ctr32::from_key(&ek.cipher_key, iv, 0).expect("key size valid");
    let mut ctext = ptext.to_vec();
    aes.process(&mut ctext);
    ctext.extend_from_slice(&hmac_sha256(&ek.hmac_key, iv, &ctext)[..16]); // 16-byte MAC
    ctext
}
```

**Encrypt-then-MAC**: Prevents padding oracle attacks by verifying MAC before decryption.

### Restore Flow

```rust
pub async fn restore_backup<R: traits::Restore>(
    current_and_previous_svrbs: &[R],
    backup_key: &BackupKey,
    metadata: BackupFileMetadataRef<'_>,
) -> Result<BackupRestoreResponse, Error>
```

**Parallel Restore Strategy**:

```rust
let mut futures = itertools::iproduct!(
    current_and_previous_svrbs.iter().enumerate(),
    metadata.pair.iter().enumerate()
)
.map(async |((enclave_index, svrb), (pair_index, pair))| {
    tokio::time::sleep(delay(enclave_index, pair_index, metadata.pair.len())).await;
    let result = restore_backup_attempt(svrb, backup_key, &iv, pair).await;
    (enclave_index, pair_index, result)
})
.collect::<futures_util::stream::FuturesUnordered<_>>();
```

**Optimization**: Try all combinations of (enclave, metadata pair) in parallel with staggered delays. Return the first success.

**Why This Works**:
- Old backups have old metadata pairs
- New backups have new metadata pairs
- During migration, metadata has both
- Any successful restore is valid

### Error Prioritization

When multiple operations fail, SVR-B prioritizes errors:

```rust
fn prioritize_error(first: Self, second: Self) -> Self {
    match (first, second) {
        // Structural errors (shouldn't happen, but don't hide them)
        (e @ Self::PreviousBackupDataInvalid, _) => e,
        (e @ Self::MetadataInvalid, _) => e,

        // Data decryption errors (wrong backup)
        (e @ Self::DecryptionError(_), _) => e,

        // Connection errors (maybe another enclave works)
        (e @ Self::AttestationError(_), _) => e,
        (e @ Self::Protocol(_), _) => e,

        // Actionable errors
        (e @ Self::RateLimited(_), _) => e,

        // Generic retry errors
        (e @ Self::Service(_), _) => e,

        // Content errors (report only if connection succeeded)
        (e @ Self::RestoreFailed(_), _) => e,
        (e @ Self::DataMissing, _) => e,
    }
}
```

**Principle**: Prefer errors that indicate Signal's responsibility (attestation, protocol) over errors that might be user error (wrong PIN).

---

## 6.4 Chat Services

### WebSocket-Based Messaging

Unlike REST APIs, Signal chat uses persistent WebSocket connections for:
- **Immediate message delivery** (no polling)
- **Bidirectional communication** (server can push)
- **Connection state awareness** (online/offline)

From `/home/user/libsignal/rust/net/src/chat.rs`:

```rust
pub struct ChatConnection {
    inner: self::ws::Chat,
    connection_info: ConnectionInfo,
}

pub struct Request {
    pub method: ::http::Method,
    pub path: PathAndQuery,
    pub headers: HeaderMap,
    pub body: Option<Bytes>,
}

pub struct Response {
    pub status: StatusCode,
    pub message: Option<String>,
    pub headers: HeaderMap,
    pub body: Option<Bytes>,
}
```

**HTTP-over-WebSocket**: Chat uses HTTP-like request/response semantics over WebSocket. This provides:
- Familiar HTTP methods (GET, PUT, POST)
- Standard status codes (200, 403, 429)
- Header-based metadata
- Binary body content

### Connection Establishment

```rust
impl ChatConnection {
    pub async fn start_connect_with<TC>(
        connection_resources: ConnectionResources<'_, TC>,
        http_route_provider: impl RouteProvider<Route = UnresolvedHttpsServiceRoute>,
        user_agent: &UserAgent,
        ws_config: self::ws::Config,
        enable_permessage_deflate: EnablePermessageDeflate,
        headers: Option<ChatHeaders>,
        log_tag: &str,
    ) -> Result<PendingChatConnection, ConnectError>
```

**Two-Phase Connect**:
1. `start_connect_with()` → Establishes WebSocket, returns `PendingChatConnection`
2. `finish_connect(runtime, pending, listener)` → Spawns async tasks

This separation allows:
- Collecting connection metadata before activation
- Configuring event listeners
- Associating connections with tokio runtimes

### Chat Headers

```rust
pub struct AuthenticatedChatHeaders {
    pub auth: Auth,                    // Username/password
    pub receive_stories: ReceiveStories, // Feature flag
    pub languages: LanguageList,       // For localized responses
}

pub struct UnauthenticatedChatHeaders {
    pub languages: LanguageList,
}
```

**Authenticated vs Unauthenticated**: Some operations (registration, rate limit recovery) don't require authentication. The type system prevents accidentally sending auth headers for public endpoints.

### WebSocket Message Protocol

From the protobuf definition:

```rust
pub type MessageProto = proto::chat_websocket::WebSocketMessage;
pub type RequestProto = proto::chat_websocket::WebSocketRequestMessage;
pub type ResponseProto = proto::chat_websocket::WebSocketResponseMessage;

pub enum ChatMessageType {
    Unknown = 0,
    Request = 1,
    Response = 2,
}
```

**Wire Format**: WebSocket binary frames contain protobuf-encoded messages. The `type` field distinguishes:
- **Request**: Client → Server or Server → Client (for pushes)
- **Response**: Reply to a Request

### Request Timeout Handling

```rust
pub async fn send(&self, msg: Request, timeout: Duration) -> Result<Response, SendError> {
    let send_result = tokio::time::timeout(timeout, self.inner.send(msg))
        .await
        .map_err(|_elapsed| SendError::RequestTimedOut)?;
    Ok(send_result?)
}
```

**Timeout Policy**: Each request has independent timeout. Long-running requests (fetching large attachments) can specify longer timeouts without affecting the connection.

### Error Handling

```rust
pub enum SendError {
    RequestTimedOut,
    Disconnected,
    ConnectedElsewhere,
    ConnectionInvalidated,
    WebSocket(WebSocketError),
    IncomingDataInvalid,
    RequestHasInvalidHeader,
}
```

**ConnectedElsewhere**: Signal allows only one active WebSocket per device. If another connection authenticates, the server closes previous connections with this error code.

### Response Validation

```rust
impl TryFrom<ResponseProto> for Response {
    type Error = ResponseProtoInvalidError;

    fn try_from(response_proto: ResponseProto) -> Result<Self, Self::Error> {
        let status = status
            .unwrap_or_default()
            .try_into()
            .and_then(|code| StatusCode::from_u16(code))?;

        let headers = headers.into_iter().try_fold(
            HeaderMap::new(),
            |mut headers, header_string| {
                let (name, value) = header_string
                    .split_once(':')
                    .ok_or(ResponseProtoInvalidError)?;
                headers.append(
                    HeaderName::try_from(name)?,
                    HeaderValue::from_str(value.trim())?
                );
                Ok(headers)
            }
        )?;

        Ok(Response { status, message, body, headers })
    }
}
```

**Header Parsing**: Protocol buffers carry headers as strings (`"Host: chat.signal.org"`). The parser validates:
- Header name is valid (no spaces, valid characters)
- Header value is valid ASCII
- Format matches `name: value`

### Listener Pattern

```rust
pub type EventListener = Box<dyn Fn(&Event) + Send + Sync>;

pub enum Event {
    ConnectionInterrupted,
    IncomingMessage(ServerRequest),
    // ... other events
}
```

**Observer Pattern**: Chat connections can register listeners for:
- Connection state changes
- Incoming server-initiated messages (like push notifications)
- Error conditions

This enables reactive UI updates without polling.

---

## 6.5 Key Transparency

### The Trust Problem

Public-key cryptography requires knowing someone's public key. But how do you trust the server gave you the right key? **Key Transparency** provides verifiable proof.

### Architecture Overview

```
    Client A                  Key Transparency Log                 Client B
       |                              |                                 |
       |---(1) Upload Public Key)---->|                                 |
       |                              |                                 |
       |<--(2) Tree Head + Proof)-----|                                 |
       |                              |                                 |
       |                   (Log appends to Merkle tree)                 |
       |                              |                                 |
       |                              |<--(3) Lookup B's Key)-----------|
       |                              |                                 |
       |                              |---(4) Key + Inclusion Proof)--->|
       |                              |                                 |
       |                              |<--(5) Monitor A's Key)----------|
       |                              |                                 |
       |                              |---(6) Audit Proof)------------->|
```

### Merkle Tree Structure

From `/home/user/libsignal/rust/keytrans/src/lib.rs`:

```rust
pub type TreeRoot = [u8; 32];
pub type LastTreeHead = (TreeHead, TreeRoot);

pub struct TreeHead {
    pub tree_size: u64,     // Number of leaves
    pub timestamp: i64,     // Unix timestamp
    pub signatures: Vec<Signature>,  // Server + auditor signatures
}
```

**Merkle Tree**: Hash tree where each leaf is a (key, value) pair:
```
                    Root (32 bytes)
                    /            \
            H(Left, Right)    H(Left, Right)
              /    \             /    \
          Leaf1  Leaf2       Leaf3  Leaf4
```

Each leaf hash: `SHA256(prefix_root || commitment)`

### VRF for Deterministic Positioning

**Problem**: Server could create different trees for different users (fork attack).

**Solution**: Use **VRF** (Verifiable Random Function) to determine leaf position:

```rust
pub struct MonitoringData {
    pub index: [u8; 32],        // VRF output (deterministic position)
    pub pos: u64,               // Position in log
    pub ptrs: HashMap<u64, u32>, // Map position → version
    pub owned: bool,            // Whether client owns this key
}
```

**VRF Property**: Given search key and VRF secret key, produces:
- **Output**: Deterministic position in tree
- **Proof**: Anyone can verify output is correct for given input

Server cannot show different positions to different users without detection.

### Search Operation

```rust
pub struct VerifiedSearchResult {
    pub value: Vec<u8>,         // The public key
    pub state_update: SearchStateUpdate,
}

pub struct SearchStateUpdate {
    pub tree_head: TreeHead,
    pub tree_root: TreeRoot,
    pub monitoring_data: Option<MonitoringData>,
}
```

**Search Flow**:
1. Client sends `search_key` (e.g., phone number)
2. Server computes VRF proof: `(index, proof) = VRF(secret_key, search_key)`
3. Server returns:
   - Current value at that position
   - Merkle inclusion proof
   - VRF proof
   - Tree head and root
4. Client verifies:
   - VRF proof is valid
   - Value is in tree at VRF position
   - Tree head signature is valid

### Monitoring

**Key Transparency Monitoring**: Ensures server shows same tree to everyone.

```rust
pub struct MonitorRequest {
    pub search_keys: Vec<MonitorKey>,
}

pub struct MonitorKey {
    pub search_key: Vec<u8>,
    pub entries: Vec<u64>,  // Positions to check
}
```

**Monitor Protocol**:
1. Client maintains `MonitoringData` for keys it cares about
2. Periodically sends `MonitorRequest` with known positions
3. Server returns proofs that those positions still have expected values
4. If any value changed unexpectedly → attack detected

From the verification code:

```rust
pub fn verify_monitor<'a>(
    &'a self,
    request: &'a MonitorRequest,
    response: &'a MonitorResponse,
    context: MonitorContext,
    now: SystemTime,
) -> Result<MonitorStateUpdate, verify::Error>
```

**Monitoring State**: The `MonitorContext` includes:
- `last_tree_head`: Previous tree size/root
- `last_distinguished_tree_head`: Auditor-signed tree head
- `data`: Map of search key → `MonitoringData`

### Consistency Proofs

**Problem**: Server could create different tree versions (rollback attack).

**Solution**: Consistency proofs show tree only grows.

```rust
pub fn verify_distinguished(
    &self,
    full_tree_head: &FullTreeHead,
    last_tree_head: Option<&LastTreeHead>,
    last_distinguished_tree_head: &LastTreeHead,
) -> Result<(), verify::Error>
```

**Consistency Proof**: For trees of size N and M (N < M):
- Proves tree[0..N] is prefix of tree[0..M]
- Uses O(log M) hashes
- Prevents rollback or modification of history

### Deployment Modes

```rust
pub enum DeploymentMode {
    ContactMonitoring,                  // Users monitor their contacts
    ThirdPartyManagement(VerifyingKeys), // External service manages keys
    ThirdPartyAuditing(VerifyingKeys),  // External auditors verify tree
}
```

**Third-Party Auditing**: External organizations can:
- Run independent tree auditors
- Sign tree heads with their keys
- Clients require multiple signatures (Signal + auditor)
- Detects if Signal shows different trees to auditors vs users

### Signature Verification

```rust
fn verify_tree_head_signature(
    config: &PublicConfig,
    head: &impl VerifiableTreeHead,
    root: &[u8; 32],
    verifying_key: &VerifyingKey,
    maybe_auditor_key: Option<&VerifyingKey>,
) -> Result<()> {
    let to_be_signed = head.to_signable_header(root, config, maybe_auditor_key);
    let signature = Signature::from_slice(head.signature_bytes())?;
    verifying_key.verify(&to_be_signed, &signature)?;
    Ok(())
}
```

**Signature Format**:
```
to_be_signed = [
    ciphersuite (2 bytes),
    deployment_mode (1 byte),
    signature_key_len (2 bytes), signature_key,
    vrf_key_len (2 bytes), vrf_key,
    [auditor_key_len (2 bytes), auditor_key,]  // if applicable
    tree_size (8 bytes),
    timestamp (8 bytes),
    root_hash (32 bytes)
]
```

**Tamper Resistance**: Signature covers all configuration parameters. Server cannot:
- Switch VRF keys without detection
- Change deployment mode
- Forge auditor signatures

---

## 6.6 Security Properties and Threat Model

### CDSI Security

**Guarantees**:
- Server cannot observe query contents (SGX confidentiality)
- Server cannot modify results without detection (SGX integrity)
- Server cannot link queries to accounts (encrypted transport)

**Limitations**:
- Server observes query timing and size
- Side-channel attacks on SGX (mitigated by patches)
- Rate limiting prevents bulk queries

### SVR Security

**Guarantees**:
- Server cannot brute-force PINs (PPSS rate limiting)
- Forward secrecy: Old backups decrypt even if future PIN leaked
- Multi-enclave redundancy prevents data loss

**Limitations**:
- Limited guess attempts (~10 before lockout)
- Requires trust in SGX hardware
- Server can deny service (delete backups)

### Chat Security

**Guarantees**:
- End-to-end encryption (Signal Protocol)
- Transport layer encryption (TLS 1.3 + optional domain fronting)
- Message authentication (prevents tampering)

**Limitations**:
- Server observes metadata (who talks to whom, when)
- Server controls message ordering and delivery
- Traffic analysis possible

### Key Transparency Security

**Guarantees**:
- Detects if server shows different keys to different users
- Append-only log prevents key history modification
- VRF prevents selective targeting
- Third-party auditors provide additional verification

**Limitations**:
- Requires active monitoring
- Detection is after-the-fact (not prevention)
- Depends on monitoring frequency

---

## 6.7 Lessons Learned and Design Patterns

### Pattern: Layered Abstraction

The network stack separates concerns:

```
Application Layer (chat APIs)
     ↓
Protocol Layer (CDSI, SVR, Chat protocols)
     ↓
Infrastructure Layer (connections, routing, DNS)
     ↓
Transport Layer (TLS, WebSocket)
```

**Benefit**: Each layer can be tested and modified independently.

### Pattern: Failover and Resilience

Multiple strategies for connection resilience:

1. **Route Diversity**: Direct + proxies + domain fronting
2. **Parallel Attempts**: Try multiple routes simultaneously
3. **Graceful Degradation**: Fallback to less optimal routes
4. **Exponential Backoff**: Prevent thundering herd

### Pattern: Type-Safe Protocols

Rust's type system enforces protocol correctness:

```rust
// Can't send auth headers to unauthenticated endpoint
let headers = UnauthenticatedChatHeaders { ... };
connect(..., Some(headers.into()), ...);

// Won't compile:
// let headers = AuthenticatedChatHeaders { ... };
// send_to_public_endpoint(headers);  // Type error!
```

### Pattern: Forward Compatibility

SVR-B's migration design enables:
- **Zero-downtime upgrades**: Old and new enclaves coexist
- **Rollback capability**: Restore from either version
- **Gradual migration**: No flag day required

### Pattern: Defense in Depth

Multiple security layers:
1. **SGX attestation**: Verify code integrity
2. **Noise encryption**: Protect data in transit
3. **TLS**: Prevent network-level attacks
4. **Rate limiting**: Prevent abuse
5. **Monitoring**: Detect anomalies

---

## Conclusion

Signal's network services demonstrate how privacy-preserving systems can be built at scale. The evolution from SVR2 → SVR3 → SVR-B shows iterative improvement: each version learned from operational experience while maintaining backward compatibility.

The unified `libsignal-net` architecture (2023-2024) represents Signal's maturation from startup to critical infrastructure. By consolidating platform-specific code into shared Rust implementations, Signal can:

- Deliver consistent security guarantees across platforms
- Iterate faster with unified testing and deployment
- Leverage Rust's safety guarantees to prevent entire classes of vulnerabilities

The integration of SGX enclaves (CDSI, SVR), WebSocket protocols (Chat), and cryptographic verifiability (Key Transparency) creates a robust foundation for private communication at global scale.

**Next**: Chapter 7 will explore the Protocol Buffers and FFI boundaries that expose these services to Swift, Java, and Node.js clients.

---

## References

- `/home/user/libsignal/rust/net/src/cdsi.rs` - Contact Discovery implementation
- `/home/user/libsignal/rust/net/src/svrb.rs` - SVR-B implementation
- `/home/user/libsignal/rust/net/src/chat.rs` - Chat service client
- `/home/user/libsignal/rust/keytrans/src/lib.rs` - Key Transparency library
- `/home/user/libsignal/rust/attest/src/dcap.rs` - SGX DCAP attestation
- `/home/user/libsignal/rust/net/infra/src/ws/attested.rs` - Attested WebSocket connections

## Further Reading

- Intel SGX DCAP Attestation: https://download.01.org/intel-sgx/latest/dcap-latest/linux/docs/
- PPSS Protocol: Signal's blog post on forward-secret backups
- Key Transparency specification: https://github.com/google/keytransparency/blob/master/docs/design.md
- Noise Protocol Framework: https://noiseprotocol.org/
