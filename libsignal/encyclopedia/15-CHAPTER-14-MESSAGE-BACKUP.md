# Chapter 14: Message Backup System

The message backup system provides encrypted, authenticated backup and restore functionality for Signal conversations, settings, and media. This chapter explores the backup format, encryption scheme, validation framework, and the complete export/import flow.

## 1. Backup Architecture

### 1.1 Why Backups Are Needed

Signal's message backup system serves two primary purposes:

1. **Remote Backup**: Allows users to store encrypted backups remotely for restoration at a later time, protecting against device loss or damage
2. **Device Transfer**: Enables immediate transfer of conversation history from one device to another during device migration

Additionally, the system supports:
- **Takeout Export**: Human-readable-ish exports that exclude disappearing content for data portability

### 1.2 Design Goals

The backup system is designed with several key goals:

**Completeness**: Backs up all conversation data including:
- Account settings and preferences
- All recipients (contacts, groups, distribution lists)
- Chat metadata and messages
- Sticker packs
- Notification profiles
- Chat folders

**Efficiency**: Uses compression and streaming to handle large backups efficiently, with multi-threaded processing for validation.

**Forward Compatibility**: Includes a version field and supports unknown fields, allowing newer clients to create backups that older clients can still partially process.

**Deterministic Ordering**: Enforces strict ordering rules to ensure backups can be validated and processed consistently.

### 1.3 Privacy Properties

The backup system provides strong privacy guarantees:

**End-to-End Encryption**: All backup content is encrypted with keys derived from the user's account entropy pool. The backup service never has access to plaintext data.

**Authenticated Encryption**: HMAC-SHA256 provides integrity protection, preventing tampering with backup contents.

**Forward Secrecy**: Modern backups include forward secrecy metadata, allowing key rotation without re-encrypting the entire backup.

**Metadata Protection**: Even the structure of the backup (number of messages, recipients, etc.) is encrypted.

## 2. Backup Format

### 2.1 Protobuf Structure

Backups use Protocol Buffers for serialization. The main structure is defined in `/rust/message-backup/src/proto/backup.proto`:

```protobuf
message BackupInfo {
  uint64 version = 1;
  uint64 backupTimeMs = 2;
  bytes mediaRootBackupKey = 3;  // 32-byte random value
  string currentAppVersion = 4;
  string firstAppVersion = 5;
  bytes debugInfo = 6;
}

message Frame {
  oneof item {
    AccountData account = 1;
    Recipient recipient = 2;
    Chat chat = 3;
    ChatItem chatItem = 4;
    StickerPack stickerPack = 5;
    AdHocCall adHocCall = 6;
    NotificationProfile notificationProfile = 7;
    ChatFolder chatFolder = 8;
  }
}
```

### 2.2 Frame-Based Format

Backups are structured as a sequence of length-delimited protobuf frames:

1. **BackupInfo**: The first message, containing metadata
2. **Frames**: Zero or more Frame messages containing actual data

Each frame is encoded using varint length-delimited format, making the backup a stream of:
```
[varint-length][protobuf-bytes][varint-length][protobuf-bytes]...
```

### 2.3 Ordering Rules

Frames must follow strict ordering rules defined in the protobuf comments:

```rust
// From backup.proto:
// 1. There is exactly one AccountData and it is the first frame.
// 2. A frame referenced by ID must come before the referencing frame.
//    e.g. a Recipient must come before any Chat referencing it.
// 3. All ChatItems must appear in global Chat rendering order.
//    (The order in which they were received by the client.)
// 4. ChatFolders must appear in render order (e.g., left to right for
//    LTR locales), but can appear anywhere relative to other frames
//    respecting rule 2 (after Recipients and Chats).
```

These rules enable streaming validation without requiring the entire backup to be loaded into memory.

### 2.4 Incremental MAC

The backup uses an incremental HMAC-SHA256 that covers all encrypted bytes:

```rust
// From frame.rs
const HMAC_LEN: usize = 32;

// Structure: [encrypted-data][32-byte-HMAC]
```

The HMAC is verified:
1. **Initially**: When opening the backup (check the appended HMAC)
2. **Incrementally**: As data is read through `MacReader`
3. **Finally**: After all data is consumed, before reporting success

This prevents time-of-check-time-of-use (TOC/TOU) attacks:

```rust
// From lib.rs
// Before reporting success, check that the HMAC still matches. This
// prevents TOC/TOU issues.
reader.into_inner().verify_hmac().await?;
```

## 3. Backup Encryption

### 3.1 Backup Key Derivation

The encryption key is derived using HKDF-SHA256 from the user's `BackupKey` and `BackupId`:

```rust
// From key.rs
impl MessageBackupKey {
    pub const HMAC_KEY_LEN: usize = 32;
    pub const AES_KEY_LEN: usize = 32;
    pub const LEN: usize = 64;  // Total key material

    pub fn derive<const VERSION: u8>(
        backup_key: &BackupKey<VERSION>,
        backup_id: &BackupId,
        backup_nonce: Option<&BackupForwardSecrecyToken>,
    ) -> Self {
        let mut full_bytes = [0; 64];

        let (salt, dst) = match backup_nonce {
            Some(nonce) => (
                Some(&nonce.0[..]),
                b"20250708_SIGNAL_BACKUP_ENCRYPT_MESSAGE_BACKUP:"
            ),
            None => (
                None,
                b"20241007_SIGNAL_BACKUP_ENCRYPT_MESSAGE_BACKUP:"
            ),
        };

        Hkdf::<Sha256>::new(salt, &backup_key.0)
            .expand_multi_info(&[dst, &backup_id.0], &mut full_bytes)
            .expect("valid length");

        Self {
            hmac_key: full_bytes[..32].try_into().unwrap(),
            aes_key: full_bytes[32..].try_into().unwrap(),
        }
    }
}
```

The domain separation tags (DST) ensure that keys derived for different purposes or versions are independent.

### 3.2 Encryption Scheme

The backup encryption uses multiple layers:

```
Plaintext
    ↓ (Gzip compression)
Compressed
    ↓ (AES-256-CBC encryption with random IV)
[IV (16 bytes)][Encrypted data]
    ↓ (HMAC-SHA256)
[IV][Encrypted data][HMAC (32 bytes)]
```

Modern backups also include an unencrypted metadata header:

```
[MAGIC_NUMBER (8 bytes)][Metadata protobuf][Encrypted backup]
```

The magic number is `b"SBACKUP\x01"`, which includes a structural version byte.

### 3.3 Code Walkthrough

**Encryption Flow** (from test code):

```rust
// 1. Compress the plaintext
let mut compressed = {
    let mut gz_writer = GzipEncoder::new(Cursor::new(Vec::new()));
    gz_writer.write_all(plaintext).await?;
    gz_writer.close().await?;
    gz_writer.into_inner().into_inner()
};

// 2. Encrypt with AES-256-CBC
let mut iv = [0; 16];
OsRng.fill_bytes(&mut iv);
let mut ctext = signal_crypto::aes_256_cbc_encrypt(
    &compressed,
    &key.aes_key,
    &iv
)?;

// 3. Prepend IV
ctext = iv.into_iter().chain(ctext).collect();

// 4. Append HMAC
let hmac = hmac_sha256(&key.hmac_key, &[], Cursor::new(&ctext)).await?;
ctext.extend_from_slice(&hmac);
```

**Decryption Flow**:

```rust
// From frame.rs
pub async fn new(
    key: &MessageBackupKey,
    mut reader_factory: impl ReaderFactory<Reader = R>,
) -> Result<Self, ValidationError> {
    let mut reader = reader_factory.make_reader()?;

    // 1. Check for magic number and metadata
    let mut maybe_magic_number = [0; 8];
    reader.read_exact(&mut maybe_magic_number).await?;
    if maybe_magic_number == MAGIC_NUMBER {
        Self::verify_metadata(&mut reader).await?;
    }

    // 2. Verify HMAC over entire encrypted content
    let (content_len, hmac) = Self::check_hmac(key, &[], reader).await?;

    // 3. Create readers: MAC reader → AES reader → Gzip reader
    let mut content = MacReader::new_sha256(reader.take(content_len), &key.hmac_key);

    let mut iv = [0; 16];
    content.read_exact(&mut iv).await?;

    let decrypted = Aes256CbcReader::new(&key.aes_key, &iv, content);
    let decompressed = GzipDecoder::new(BufReader::new(decrypted));

    Ok(Self { reader: decompressed, expected_hmac: hmac })
}
```

### 3.4 Forward Secrecy Metadata

Modern backups include unencrypted metadata for forward secrecy key rotation:

```rust
// From forward_secrecy.rs
pub const MAGIC_NUMBER: &[u8] = b"SBACKUP\x01";

pub async fn verify_metadata(reader: &mut R) -> Result<(), ValidationError> {
    let metadata = read_varint_delimited_message(reader).await?;
    let MetadataPb { iv, pair: forward_secrecy_pairs, .. } = metadata;

    // Verify 1-2 forward secrecy pairs
    // Each pair contains:
    // - ct: encrypted token (32 bytes + 16 byte MAC)
    // - pw_salt: salt for key derivation (32 bytes)

    for pair in forward_secrecy_pairs {
        verify_ct_length(pair.ct, 48)?;
        verify_salt_length(pair.pw_salt, 32)?;
    }

    verify_iv_length(iv, 12)?;
    Ok(())
}
```

## 4. Validation Framework

### 4.1 Validation Rules

The backup validator enforces numerous rules to ensure data integrity:

**Structural Rules**:
- Exactly one AccountData frame (must be first)
- At least one Self recipient
- Valid frame ordering (dependencies before references)
- No empty oneofs

**Uniqueness Rules**:
```rust
// From backup.rs - duplicate detection
fn check_for_duplicate_recipients(
    recipients: &IntMap<RecipientId, M::RecipientData>
) -> Result<(), CompletionError> {
    // Check for duplicate:
    // - E164 phone numbers
    // - Usernames
    // - ACIs (Account IDs)
    // - PNIs (Phone Number IDs)
    // - Group master keys
    // - Distribution list IDs
    // - Call link root keys
    // - Self recipient (only one allowed)
    // - Release notes recipient (only one allowed)
}
```

**Referential Integrity**:
- Chat must reference existing Recipient
- ChatItem must reference existing Chat and Recipient (author)
- Distribution list members must reference existing Contacts

**Data Validity**:
- Service IDs must be valid UUIDs
- E164 phone numbers must be valid
- Profile keys must be 32 bytes
- Identity keys must be valid libsignal public keys

### 4.2 Test Case Structure

Test cases use the `dir_test` macro for declarative testing:

```rust
// From test_cases.rs
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/res/test-cases",
    glob: "valid/*.jsonproto",
    postfix: "jsonproto"
)]
fn is_valid_json_proto(input: Fixture<&str>) {
    let json_contents = input.into_content();
    let json_array = json5::from_str(json_contents)?;
    let binproto = convert_from_json(json_array)?;
    validate_proto(&binproto)
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/res/test-cases",
    glob: "invalid/*.jsonproto",
    loader: PathBuf::from
)]
fn invalid_jsonproto(input: Fixture<PathBuf>) {
    let path = input.into_content();
    let expected_path = path.with_extension("jsonproto.expected");

    let result = validate_backup(&path).expect_err("should fail");
    let expected = std::fs::read_to_string(expected_path)?;

    assert_eq!(result.to_string(), expected);
}
```

### 4.3 dir_test Usage

The `dir_test` crate enables data-driven testing:

**Valid Test Cases**:
```
tests/res/test-cases/valid/
├── account-data.jsonproto
├── simple-chat-update-message.jsonproto
├── incoming-message-with-edits.jsonproto
└── ...
```

**Invalid Test Cases with Expected Errors**:
```
tests/res/test-cases/invalid/
├── missing-account-data.jsonproto
├── missing-account-data.jsonproto.expected  # "no AccountData frames found"
├── missing-recipient.jsonproto
├── missing-recipient.jsonproto.expected     # Error message
└── ...
```

Example invalid test case:

```json
// missing-account-data.jsonproto
[
  {
    "version": "1",
    "backupTimeMs": "1705692409729",
    "mediaRootBackupKey": "q6urq6urq6urq6urq6urq6urq6urq6urq6urq6urq6s=",
  },
  {
    "recipient": {
      "id": "1",
      "self": {}
    }
  }
]
```

Expected error:
```
// missing-account-data.jsonproto.expected
no AccountData frames found
```

### 4.4 Multi-Threaded Processing

The backup reader uses multi-threading for efficient processing:

```rust
// From lib.rs
async fn read_all_frames<M: Method + ReferencedTypes>(
    purpose: Purpose,
    mut reader: VarintDelimitedReader<impl AsyncRead + Unpin + VerifyHmac>,
    visitor: impl FnMut(&dyn Debug) + Send + 'static,
    unknown_fields: &mut Vec<FoundUnknownField>,
) -> Result<PartialBackup<M>, Error> {
    // Read BackupInfo (first frame)
    let backup_info = read_first_frame(&mut reader).await?;
    let mut backup = PartialBackup::new(backup_info, purpose)?;

    // Split work into two threads:
    // 1. Reader thread: reads frames from input
    // 2. Processing thread: parses and validates frames

    const FRAMES_IN_FLIGHT: usize = 20;
    let (frame_tx, frame_rx) = std::sync::mpsc::sync_channel(FRAMES_IN_FLIGHT);

    let processing_thread = std::thread::Builder::new()
        .name("libsignal-backup-processing".to_owned())
        .spawn(move || {
            for frame in frame_rx {
                let unknown = backup.parse_and_add_frame(&frame, |f| visitor(f))?;
                unknown_fields.extend(unknown);
            }
            Ok((backup, unknown_fields))
        })?;

    // Reader thread reads and sends frames
    while let Some(buf) = reader.read_next().await? {
        frame_tx.send(buf)?;
    }

    // Wait for processing to complete
    let (backup, unknown_fields) = processing_thread.join()??;

    // Final HMAC verification
    reader.into_inner().verify_hmac().await?;

    Ok(backup)
}
```

## 5. Backup Contents

### 5.1 Account Data

The AccountData frame contains user settings and preferences:

```protobuf
message AccountData {
  bytes profileKey = 1;
  optional string username = 2;
  UsernameLink usernameLink = 3;
  string givenName = 4;
  string familyName = 5;
  string avatarUrlPath = 6;
  AccountSettings accountSettings = 9;
  string svrPin = 11;
}

message AccountSettings {
  bool readReceipts = 1;
  bool typingIndicators = 3;
  bool linkPreviews = 4;
  uint32 universalExpireTimerSeconds = 7;
  repeated string preferredReactionEmoji = 8;
  PhoneNumberSharingMode phoneNumberSharingMode = 17;
  ChatStyle defaultChatStyle = 18;
  SentMediaQuality defaultSentMediaQuality = 23;
  // ... many more settings
}
```

### 5.2 Recipients

Recipients represent all entities that can be messaged:

```protobuf
message Recipient {
  uint64 id = 1;  // Generated ID for references within backup
  oneof destination {
    Contact contact = 2;
    Group group = 3;
    DistributionListItem distributionList = 4;
    Self self = 5;
    ReleaseNotes releaseNotes = 6;
    CallLink callLink = 7;
  }
}

message Contact {
  optional bytes aci = 1;      // Account ID (16 bytes)
  optional bytes pni = 2;      // Phone Number ID (16 bytes)
  optional string username = 3;
  optional uint64 e164 = 4;    // Phone number
  bool blocked = 5;
  optional bytes profileKey = 9;
  bool profileSharing = 10;
  optional bytes identityKey = 14;
  IdentityState identityState = 15;
  // ... more fields
}
```

Validation ensures:
- Contacts have at least one identifier (ACI, PNI, or E164)
- If a contact has a PNI, they should have an E164
- No duplicate identifiers across contacts

### 5.3 Chat Messages

ChatItem represents individual messages:

```protobuf
message ChatItem {
  uint64 chatId = 1;      // References Chat.id
  uint64 authorId = 2;    // References Recipient.id
  uint64 dateSent = 3;
  optional uint64 expireStartDate = 4;
  optional uint64 expiresInMs = 5;
  repeated ChatItem revisions = 6;  // Message edit history

  oneof directionalDetails {
    IncomingMessageDetails incoming = 8;
    OutgoingMessageDetails outgoing = 9;
    DirectionlessMessageDetails directionless = 10;
  }

  oneof item {
    StandardMessage standardMessage = 11;
    ContactMessage contactMessage = 12;
    StickerMessage stickerMessage = 13;
    RemoteDeletedMessage remoteDeletedMessage = 14;
    ChatUpdateMessage updateMessage = 15;
    PaymentNotification paymentNotification = 16;
    GiftBadge giftBadge = 17;
    ViewOnceMessage viewOnceMessage = 18;
    Poll poll = 20;
  }
}

message StandardMessage {
  optional Quote quote = 1;
  optional Text text = 2;
  repeated MessageAttachment attachments = 3;
  repeated LinkPreview linkPreview = 4;
  optional FilePointer longText = 5;
  repeated Reaction reactions = 6;
}
```

### 5.4 Stickers and Media

Media attachments use the FilePointer structure:

```protobuf
message FilePointer {
  message LocatorInfo {
    bytes key = 1;  // Encryption key

    oneof integrityCheck {
      bytes plaintextHash = 10;    // If downloaded
      bytes encryptedDigest = 11;  // If not downloaded
    }

    uint32 size = 3;  // Plaintext size

    // Transit tier (temporary storage)
    optional string transitCdnKey = 4;
    optional uint32 transitCdnNumber = 5;
    optional uint64 transitTierUploadTimestamp = 6;

    // Media tier (long-term storage)
    optional uint32 mediaTierCdnNumber = 7;

    // Local backup encryption
    optional bytes localKey = 9;
  }

  optional string contentType = 4;
  optional bytes incrementalMac = 5;
  optional uint32 incrementalMacChunkSize = 6;
  optional string fileName = 7;
  optional uint32 width = 8;
  optional uint32 height = 9;
  optional string caption = 10;
  optional string blurHash = 11;
  LocatorInfo locatorInfo = 13;
}
```

Sticker packs:

```protobuf
message StickerPack {
  bytes packId = 1;   // 16 bytes
  bytes packKey = 2;  // 32 bytes
}

message Sticker {
  bytes packId = 1;
  bytes packKey = 2;
  uint32 stickerId = 3;
  optional string emoji = 4;
  FilePointer data = 5;
}
```

### 5.5 Settings and Preferences

Chat styles, notification profiles, and chat folders:

```protobuf
message ChatStyle {
  oneof wallpaper {
    WallpaperPreset wallpaperPreset = 1;
    FilePointer wallpaperPhoto = 2;
  }

  oneof bubbleColor {
    AutomaticBubbleColor autoBubbleColor = 3;
    BubbleColorPreset bubbleColorPreset = 4;
    uint64 customColorId = 5;
  }

  bool dimWallpaperInDarkMode = 7;
}

message NotificationProfile {
  string name = 1;
  optional string emoji = 2;
  fixed32 color = 3;
  uint64 createdAtMs = 4;
  bool allowAllCalls = 5;
  bool allowAllMentions = 6;
  repeated uint64 allowedMembers = 7;
  // ... schedule settings
}

message ChatFolder {
  enum FolderType {
    ALL = 1;      // The default "All chats" folder
    CUSTOM = 2;   // User-created folder
  }

  string name = 1;
  bool showOnlyUnread = 2;
  bool showMutedChats = 3;
  bool includeAllIndividualChats = 4;
  bool includeAllGroupChats = 5;
  repeated uint64 includedRecipientIds = 7;
  repeated uint64 excludedRecipientIds = 8;
}
```

## 6. Export/Import Flow

### 6.1 Creating Backups

**High-level flow**:

```
1. Collect backup data
   - Account settings
   - Recipients
   - Chats and messages
   - Sticker packs
   - Notification profiles

2. Serialize to protobuf frames
   - Enforce ordering rules
   - Generate IDs for cross-references

3. Compress with gzip

4. Encrypt with AES-256-CBC
   - Generate random IV
   - Prepend IV to ciphertext

5. Calculate HMAC-SHA256
   - Over IV + ciphertext
   - Append to end

6. (Optional) Add forward secrecy metadata
   - Magic number
   - Encrypted key material
```

Example from test code:

```rust
// Create frames
let frames = vec![
    backup_info_frame,
    account_data_frame,
    self_recipient_frame,
    // ... more frames
];

// Serialize
let mut plaintext = Vec::new();
for frame in frames {
    frame.write_length_delimited_to(&mut plaintext)?;
}

// Compress
let compressed = gzip_compress(&plaintext)?;

// Encrypt
let iv = random_iv();
let encrypted = aes_256_cbc_encrypt(&compressed, &key.aes_key, &iv)?;
let with_iv = [&iv, &encrypted].concat();

// HMAC
let hmac = hmac_sha256(&key.hmac_key, &with_iv)?;
let final_backup = [&with_iv, &hmac].concat();
```

### 6.2 Restoring from Backup

**High-level flow**:

```
1. Read backup file

2. Verify HMAC
   - Extract last 32 bytes
   - Compute HMAC over rest
   - Compare in constant time

3. Extract IV (first 16 bytes of content)

4. Decrypt with AES-256-CBC

5. Decompress with gzip

6. Parse protobuf frames
   - Validate BackupInfo
   - Process frames in order
   - Build in-memory representation

7. Validate completed backup
   - Check for required frames
   - Verify referential integrity
   - Check for duplicates

8. Apply to local database
```

The restoration process:

```rust
// Create encrypted reader
let reader = BackupReader::new_encrypted_compressed(
    &key,
    FileReaderFactory { path: &backup_file },
    Purpose::RemoteBackup
).await?;

// Read and validate all frames
let ReadResult { result, found_unknown_fields } = reader.read_all().await;

// Handle unknown fields (forward compatibility)
if !found_unknown_fields.is_empty() {
    log::warn!("Found unknown fields in backup:");
    for field in found_unknown_fields {
        log::warn!("  {}", field);
    }
}

// Get validated backup
let backup: CompletedBackup = result?;

// Apply to database
database.restore_from_backup(backup)?;
```

### 6.3 Error Handling

The system defines comprehensive error types:

```rust
#[derive(Debug, thiserror::Error)]
pub enum Error {
    BackupValidation(ValidationError),
    BackupCompletion(CompletionError),
    Parse(std::io::Error),
    NoFrames,
    InvalidProtobuf(protobuf::Error),
    HmacMismatch(HmacMismatchError),
}

#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    EmptyFrame,
    BackupInfoError(MetadataError),
    MultipleAccountData,
    AccountData(AccountDataError),
    RecipientError(RecipientFrameError),
    ChatError(ChatFrameError),
    // ... more variants
}

#[derive(Debug, thiserror::Error)]
pub enum CompletionError {
    MissingAccountData,
    MissingAllChatFolder,
    MissingSelfRecipient,
    DuplicateContactE164(RecipientId, RecipientId),
    DuplicateContactAci(RecipientId, RecipientId),
    // ... more variants
}
```

Error messages are designed to be specific and actionable:

```
"Chat frame ChatId(42) error: unknown recipient RecipientId(100)"
"Recipient error: contact has neither an ACI, nor a PNI, nor an e164"
"no AccountData frames found"
"RecipientId(5) and RecipientId(12) have the same ACI"
```

## 7. Testing

### 7.1 Valid Test Cases

Example minimal valid backup (`account-data.jsonproto`):

```json
[
  {
    "version": "1",
    "backupTimeMs": "1715636551000",
    "mediaRootBackupKey": "q6urq6urq6urq6urq6urq6urq6urq6urq6urq6urq6s=",
  },
  {
    "account": {
      "profileKey": "YQKRq+3DQklInaOaMcmlzZnN0m/1hzLiaONX7gB12dg=",
      "username": "boba_fett.66",
      "givenName": "Boba",
      "familyName": "Fett",
      "accountSettings": {
        "readReceipts": true,
        "typingIndicators": true,
        "linkPreviews": false,
        // ... more settings
      }
    }
  },
  {
    "recipient": {
      "id": "1",
      "self": {}
    }
  },
  {
    "recipient": {
      "id": "2",
      "releaseNotes": {}
    }
  },
  {
    "recipient": {
      "id": "3",
      "distributionList": {
        "distributionId": "AAAAAAAAAAAAAAAAAAAAAA==",
        "distributionList": {
          "allowReplies": true,
          "memberRecipientIds": [],
          "name": "My Story",
          "privacyMode": "ALL"
        }
      }
    }
  }
]
```

This includes:
- BackupInfo metadata
- AccountData (required, must be first frame)
- Self recipient (required)
- Release notes recipient
- My Story distribution list

### 7.2 Invalid Test Cases

**Missing required frame** (`missing-account-data.jsonproto`):
```json
[
  {
    "version": "1",
    "backupTimeMs": "1705692409729",
    "mediaRootBackupKey": "q6urq6urq6urq6urq6urq6urq6urq6urq6urq6urq6s=",
  },
  {
    "recipient": {
      "id": "1",
      "self": {}
    }
  }
]
```

Error: `no AccountData frames found`

**Missing referenced recipient** (`missing-recipient.jsonproto`):
```json
[
  {
    "version": "1",
    "backupTimeMs": "1705692409729",
    "mediaRootBackupKey": "..."
  },
  {
    "account": { /* ... */ }
  },
  {
    "recipient": {
      "id": "1",
      "self": {}
    }
  },
  {
    "chat": {
      "id": "1",
      "recipientId": "999",  // Does not exist!
      "archived": false
    }
  }
]
```

Error: `Chat frame ChatId(1) error: unknown recipient RecipientId(999)`

**Duplicate PIN order** (`chat-pinned-order-conflict.jsonproto`):
```json
[
  /* BackupInfo, AccountData, Recipients... */
  {
    "chat": {
      "id": "1",
      "recipientId": "2",
      "pinnedOrder": 1
    }
  },
  {
    "chat": {
      "id": "2",
      "recipientId": "3",
      "pinnedOrder": 1  // Duplicate!
    }
  }
]
```

Error: `multiple chats with pinned order 1`

### 7.3 Encrypted Test Cases

The test suite includes encrypted backups to verify the full encryption/decryption flow:

```
tests/res/test-cases/valid-encrypted/
├── new-account.binproto.encrypted
├── new-account.binproto.encrypted.source.jsonproto
├── legacy-account.binproto.encrypted
└── legacy-account.binproto.encrypted.source.jsonproto
```

Tests verify:
1. Encrypted backup can be decrypted with correct key
2. Decrypted content matches source
3. HMAC verification works
4. Forward secrecy metadata is valid (modern format)
5. Legacy format (without metadata) still works

```rust
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/res/test-cases",
    glob: "valid-encrypted/*.binproto.encrypted"
)]
fn is_valid_encrypted_proto(path: PathBuf) {
    let key = derive_test_key();

    let reader = BackupReader::new_encrypted_compressed(
        &key,
        FileReaderFactory { path: &path },
        Purpose::RemoteBackup
    ).await?;

    let backup = reader.read_all().await.result?;

    // Verify against .source.jsonproto
    let source = load_source_jsonproto(&path)?;
    assert_backup_matches_source(backup, source);
}
```

### 7.4 Edge Cases

The test suite covers numerous edge cases:

**Message Edits**:
- `incoming-message-with-edits.jsonproto`
- `outgoing-message-with-edits.jsonproto`

Verify that the `revisions` field properly tracks edit history.

**Chat Updates**:
- `simple-chat-update-message.jsonproto`
- `expiration-timer-chat-update-message.jsonproto`
- `profile-change-chat-update-message.jsonproto`

Cover various types of chat update messages.

**Invalid Service IDs**:
- `group-update-invalid-aci.jsonproto`

Tests that invalid UUIDs are properly rejected.

**Sticker Validation**:
- `sticker-pack-id.jsonproto`

Verifies sticker pack ID validation (must be 16 bytes).

## Security Properties

The message backup system provides several critical security properties:

### Confidentiality
All backup content (messages, contacts, settings) is encrypted with AES-256-CBC. The backup service only sees encrypted blobs.

### Integrity
HMAC-SHA256 provides cryptographic assurance that backups have not been tampered with. Any modification will cause verification to fail.

### Authentication
The HMAC key is derived from the user's BackupKey, which only the user possesses. This prevents an attacker from creating valid backups for another user.

### Forward Secrecy
The modern backup format includes encrypted key material that allows rotating the backup encryption key without re-encrypting all historical backups.

### Padding
The `padded_length` function obscures the exact size of backups:

```rust
pub fn padded_length(content_length: u32) -> u32 {
    const BASE: f64 = 1.05;
    let exp = f64::log(content_length.into(), BASE).ceil();
    u32::max(541, BASE.powf(exp).floor() as u32)
}
```

This rounds up sizes by approximately 5%, making it harder to infer backup contents from size alone.

### Deterministic Unknown Field Handling
The system collects and reports unknown protobuf fields rather than failing:

```rust
pub struct FoundUnknownField {
    pub frame_index: usize,
    pub path: Vec<PathPart>,
    pub value: UnknownValue,
}
```

This allows graceful forward compatibility: newer clients can add fields that older clients safely ignore, while still preserving the data.

## Conclusion

The Signal message backup system demonstrates careful attention to privacy, security, and reliability. Key design decisions include:

- **Frame-based streaming format**: Enables processing of large backups without loading everything into memory
- **Strong cryptography**: HKDF key derivation, AES-256-CBC encryption, HMAC-SHA256 authentication
- **Comprehensive validation**: Multi-layer validation catches errors early and provides clear error messages
- **Forward compatibility**: Unknown field handling and version numbers enable evolution
- **Extensive testing**: Hundreds of test cases covering valid, invalid, and edge cases

The system successfully balances competing concerns: it must be efficient enough to handle large backups, secure enough to protect sensitive data, and robust enough to handle versioning and edge cases across a diverse ecosystem of clients.

---

*For implementation details, see:*
- *Protobuf definitions: `/rust/message-backup/src/proto/backup.proto`*
- *Encryption: `/rust/message-backup/src/key.rs`, `/rust/message-backup/src/frame.rs`*
- *Validation: `/rust/message-backup/src/backup.rs`*
- *Test cases: `/rust/message-backup/tests/res/test-cases/`*
