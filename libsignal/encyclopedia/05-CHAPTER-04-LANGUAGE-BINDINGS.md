# Chapter 4: Language Bindings Architecture

## Executive Summary

libsignal's language bindings system is a sophisticated procedural macro framework that generates FFI (C), JNI (Java), and Neon (Node.js) entry points from a single Rust function definition. This chapter documents the complete architecture, type conversion system, error handling mechanisms, and code generation pipeline.

**Key Architectural Principles:**
- **Single Source of Truth**: One Rust function generates three platform bindings
- **Type Safety**: Strong typing enforced at compile time across language boundaries
- **Panic Safety**: All panics caught and converted to platform-native exceptions
- **Zero Overhead**: Minimal runtime cost through static generation

---

## 1. Bridge Architecture Overview

### 1.1 The Unified Bridge Design

The bridge system is built around a core macro `#[bridge_fn]` that transforms a single Rust function into three platform-specific entry points:

```rust
// Location: rust/bridge/shared/macros/src/lib.rs

#[bridge_fn]
fn Aes256GcmSiv_New(key: &[u8]) -> Result<Aes256GcmSiv> {
    Ok(Aes256GcmSiv(
        aes_gcm_siv::Aes256GcmSiv::new_from_slice(key)
            .map_err(|_| Error::InvalidKeySize)?
    ))
}
```

This single definition generates:

**1. FFI Entry Point (C/Swift):**
```c
SignalFfiError *signal_aes256_gcm_siv_new(
    SignalAes256GcmSiv **out,
    const unsigned char *key,
    size_t key_len
);
```

**2. JNI Entry Point (Java):**
```java
public static native long Aes256GcmSiv_New(byte[] key) throws Exception;
```

**3. Node Entry Point (TypeScript):**
```typescript
export function Aes256GcmSiv_New(key: Buffer): Aes256GcmSiv;
```

### 1.2 Procedural Macro System Architecture

The macro system follows a multi-stage code generation pipeline:

```
┌────────────────────────────────────────────────────────────┐
│  bridge_fn Attribute Macro                                 │
│  (rust/bridge/shared/macros/src/lib.rs)                    │
└─────────────┬──────────────────────────────────────────────┘
              │
              ├─► Parse function signature
              ├─► Extract argument types and names
              ├─► Determine result kind (Regular/Void)
              │
              v
    ┌─────────────────────────────────────────────────────────┐
    │  Platform-Specific Code Generation                      │
    └───┬─────────────────┬──────────────────┬────────────────┘
        │                 │                  │
        v                 v                  v
   ┌────────┐       ┌─────────┐       ┌──────────┐
   │  FFI   │       │   JNI   │       │   Node   │
   │ ffi.rs │       │  jni.rs │       │ node.rs  │
   └────┬───┘       └────┬────┘       └────┬─────┘
        │                │                  │
        v                v                  v
   [C header]      [Java native]      [TS definition]
```

### 1.3 Type Conversion Infrastructure

Each bridge defines traits for bidirectional type conversion:

**FFI (rust/bridge/shared/types/src/ffi/convert.rs):**
```rust
pub trait ArgTypeInfo<'storage>: Sized {
    type ArgType;
    type StoredType: 'storage;
    fn borrow(foreign: Self::ArgType) -> SignalFfiResult<Self::StoredType>;
    fn load_from(stored: &'storage mut Self::StoredType) -> Self;
}

pub trait ResultTypeInfo: Sized {
    type ResultType;
    fn convert_into(self) -> SignalFfiResult<Self::ResultType>;
}
```

**JNI (rust/bridge/shared/types/src/jni/convert.rs):**
```rust
pub trait ArgTypeInfo<'storage, 'param: 'storage, 'context: 'param>: Sized {
    type ArgType: 'param;
    type StoredType: 'storage;
    fn borrow(
        env: &mut JNIEnv<'context>,
        foreign: &'param Self::ArgType,
    ) -> Result<Self::StoredType, BridgeLayerError>;
    fn load_from(stored: &'storage mut Self::StoredType) -> Self;
}

pub trait ResultTypeInfo<'a>: Sized {
    type ResultType: Into<JValueOwned<'a>>;
    fn convert_into(self, env: &mut JNIEnv<'a>)
        -> Result<Self::ResultType, BridgeLayerError>;
}
```

**Node (rust/bridge/shared/types/src/node/convert.rs):**
```rust
pub trait ArgTypeInfo<'storage, 'context: 'storage>: Sized {
    type ArgType: neon::types::Value;
    type StoredType: 'storage;
    fn borrow(
        cx: &mut FunctionContext<'context>,
        foreign: Handle<'context, Self::ArgType>,
    ) -> NeonResult<Self::StoredType>;
    fn load_from(stored: &'storage mut Self::StoredType) -> Self;
}

pub trait ResultTypeInfo<'a>: Sized {
    type ResultType: neon::types::Value;
    fn convert_into(self, cx: &mut impl Context<'a>)
        -> JsResult<'a, Self::ResultType>;
}
```

---

## 2. Java/JNI Bridge

### 2.1 JNI Entry Point Generation

The JNI bridge generates entry points conforming to the Java Native Interface specification:

**Macro Code (rust/bridge/shared/macros/src/jni.rs):**
```rust
pub(crate) fn bridge_fn(
    name: &str,
    sig: &Signature,
    bridging_kind: &BridgingKind,
) -> Result<TokenStream2> {
    let wrapper_name = format_ident!("__bridge_fn_jni_{}", name);
    let orig_name = &sig.ident;

    let input_names_and_types = extract_arg_names_and_types(sig)?;
    let input_args = input_names_and_types
        .iter()
        .map(|(name, ty)| quote!(#name: jni_arg_type!(#ty)));

    let output = result_type(&sig.output);
    let result_ty = quote!(jni_result_type!(#output));

    Ok(quote! {
        #[cfg(feature = "jni")]
        #[unsafe(export_name = concat!(
            env!("LIBSIGNAL_BRIDGE_FN_PREFIX_JNI"),
            #name
        ))]
        #[allow(non_snake_case)]
        pub unsafe extern "C" fn #wrapper_name<'local>(
            mut env: ::jni::JNIEnv<'local>,
            _class: ::jni::objects::JClass,
            #(#input_args),*
        ) -> #result_ty {
            jni::run_ffi_safe(&mut env, |env| {
                // Load arguments
                #(#input_processing)*
                // Call original function
                let __result = #orig_name(#(#input_names),*);
                // Convert result
                jni::ResultTypeInfo::convert_into(__result, env)
                    .map_err(Into::into)
            })
        }
    })
}
```

### 2.2 Object Handle Management

JNI uses `long` values as opaque handles to Rust objects:

**Handle Declaration (rust/bridge/shared/types/src/jni/convert.rs):**
```rust
// Implement for any type marked as a handle
impl<'storage, 'param: 'storage, 'context: 'param>
    ArgTypeInfo<'storage, 'param, 'context> for &Aes256GcmSiv
{
    type ArgType = ObjectHandle;  // ObjectHandle = jlong (i64)
    type StoredType = Self::ArgType;

    fn borrow(
        _env: &mut JNIEnv<'context>,
        foreign: &'param Self::ArgType,
    ) -> Result<Self::StoredType, BridgeLayerError> {
        Ok(*foreign)
    }

    fn load_from(stored: &'storage mut Self::StoredType) -> Self {
        unsafe { native_handle_cast(*stored) }
            .expect("invalid handle")
    }
}
```

**Native.kt Integration (java/shared/java/org/signal/libsignal/internal/Native.kt):**
```kotlin
public typealias ObjectHandle = Long

internal object Native {
    // Auto-generated by gen_java_decl.py
    @JvmStatic
    public external fun Aes256GcmSiv_Destroy(handle: ObjectHandle): Unit

    @JvmStatic
    @Throws(Exception::class)
    public external fun Aes256GcmSiv_New(key: ByteArray): ObjectHandle

    @JvmStatic
    @Throws(Exception::class)
    public external fun Aes256GcmSiv_Encrypt(
        aesGcmSivObj: ObjectHandle,
        ptext: ByteArray,
        nonce: ByteArray,
        associatedData: ByteArray
    ): ByteArray
}
```

### 2.3 Java Code Generation Pipeline

**Script: bin/gen_java_decl.py**

This Python script scans the Rust bridge code and generates Java method declarations:

```python
# Extracts function signatures from Rust macros
def extract_bridge_fns(rust_source):
    for match in re.finditer(r'#\[bridge_fn.*?\]', rust_source):
        # Parse function signature
        # Generate JNI method signature
        # Output Java external declaration

# Type mappings
RUST_TO_JAVA = {
    'u32': 'int',
    '&[u8]': 'byte[]',
    'String': 'String',
    'Result<T>': 'T',  # unwrapped, throws Exception
}
```

### 2.4 Complete Function Trace: Aes256GcmSiv_New

**Step 1: Rust Bridge Definition**
```rust
// Location: rust/bridge/shared/src/crypto.rs

#[bridge_fn]
fn Aes256GcmSiv_New(key: &[u8]) -> Result<Aes256GcmSiv> {
    Ok(Aes256GcmSiv(
        aes_gcm_siv::Aes256GcmSiv::new_from_slice(key)
            .map_err(|_| Error::InvalidKeySize)?
    ))
}
```

**Step 2: Macro Expansion (generated code)**
```rust
#[cfg(feature = "jni")]
#[unsafe(export_name =
    "Java_org_signal_libsignal_internal_Native_Aes256GcmSiv_1New")]
pub unsafe extern "C" fn __bridge_fn_jni_Aes256GcmSiv_New<'local>(
    mut env: ::jni::JNIEnv<'local>,
    _class: ::jni::objects::JClass,
    key: jni::objects::JByteArray<'local>,
) -> jlong {
    jni::run_ffi_safe(&mut env, |env| {
        // Borrow: Get array elements from JVM
        let mut key_stored = unsafe {
            env.get_array_elements(&key, ReleaseMode::NoCopyBack)
        }?;

        // Load: Convert to &[u8]
        let key = <&[u8]>::load_from(&mut key_stored);

        // Call original function
        let __result = Aes256GcmSiv_New(key);

        // Unwrap Result<T> -> T (errors become exceptions)
        let __result = TransformHelper(__result).ok_if_needed()?.0;

        // Convert to ObjectHandle (box and return pointer)
        jni::ResultTypeInfo::convert_into(__result, env)
            .map_err(Into::into)
    })
}
```

**Step 3: Java Declaration (auto-generated)**
```kotlin
@JvmStatic
@Throws(Exception::class)
public external fun Aes256GcmSiv_New(key: ByteArray): ObjectHandle
```

**Step 4: Java Usage**
```kotlin
val key = ByteArray(32) { 0 }
val handle = Native.Aes256GcmSiv_New(key)
// handle is a Long representing a Box<Aes256GcmSiv> in Rust
```

---

## 3. Swift/FFI Bridge

### 3.1 C FFI Layer

The FFI bridge generates pure C entry points with explicit error handling:

**Macro Code (rust/bridge/shared/macros/src/ffi.rs):**
```rust
pub(crate) fn bridge_fn(
    name: &str,
    sig: &Signature,
    result_kind: ResultKind,
    bridging_kind: &BridgingKind,
) -> Result<TokenStream2> {
    let wrapper_name = format_ident!("__bridge_fn_ffi_{}", name);

    // Convert MyFunction -> my_function
    let ffi_name = name.to_snake_case();

    let input_args = input_names_and_types
        .iter()
        .map(|(name, ty)| quote!(#name: ffi_arg_type!(#ty)));

    let implicit_args = match result_kind {
        ResultKind::Regular => quote!(out: *mut ffi_result_type!(#ty),),
        ResultKind::Void => quote!(),
    };

    Ok(quote! {
        #[cfg(feature = "ffi")]
        #[unsafe(export_name = concat!(
            env!("LIBSIGNAL_BRIDGE_FN_PREFIX_FFI"),
            #ffi_name
        ))]
        pub unsafe extern "C" fn #wrapper_name(
            #implicit_args
            #(#input_args),*
        ) -> *mut ffi::SignalFfiError {
            ffi::run_ffi_safe(|| {
                // Borrow and load arguments
                #(#input_processing)*

                // Call original function
                let __result = #orig_name(#(#input_names),*);

                // Write result to out pointer
                ffi::write_result_to(out, __result)?;
                Ok(())
            })
        }
    })
}
```

### 3.2 cbindgen Configuration and Header Generation

**cbindgen.toml:**
```toml
language = "C"
autogen_warning = "/* WARNING: automatically generated */"
include_guard = "SIGNAL_FFI_H_"
sys_includes = ["stdint.h", "stddef.h"]

[export]
prefix = "Signal"
include = ["PublicKey", "PrivateKey", "Aes256GcmSiv"]

[export.rename]
"Aes256GcmSiv" = "SignalAes256GcmSiv"
```

**Generated Header:**
```c
// signal_ffi.h (auto-generated)

typedef struct SignalAes256GcmSiv SignalAes256GcmSiv;
typedef struct SignalFfiError SignalFfiError;

SignalFfiError *signal_aes256_gcm_siv_new(
    SignalAes256GcmSiv **out,
    const unsigned char *key_data,
    size_t key_len
);

void signal_aes256_gcm_siv_destroy(SignalAes256GcmSiv *obj);
```

### 3.3 Swift Wrapper Patterns

**Swift Integration (swift/Sources/LibSignalClient/PublicKey.swift):**

```swift
import Foundation
import SignalFfi  // Generated C headers

// Base class for handle ownership
public class PublicKey: ClonableHandleOwner<SignalMutPointerPublicKey>,
                        @unchecked Sendable {

    // Constructor from bytes
    public convenience init<Bytes: ContiguousBytes>(_ bytes: Bytes) throws {
        let handle = try bytes.withUnsafeBorrowedBuffer { bytes in
            try invokeFnReturningValueByPointer(.init()) {
                signal_publickey_deserialize($0, bytes)
            }
        }
        self.init(owned: NonNull(handle)!)
    }

    // Destructor registration
    override internal class func destroyNativeHandle(
        _ handle: NonNull<SignalMutPointerPublicKey>
    ) -> SignalFfiErrorRef? {
        return signal_publickey_destroy(handle.pointer)
    }

    // Clone support
    override internal class func cloneNativeHandle(
        _ newHandle: inout SignalMutPointerPublicKey,
        currentHandle: SignalConstPointerPublicKey
    ) -> SignalFfiErrorRef? {
        return signal_publickey_clone(&newHandle, currentHandle)
    }

    // Method wrapper
    public func verifySignature(
        message: some ContiguousBytes,
        signature: some ContiguousBytes
    ) throws -> Bool {
        return try withAllBorrowed(
            self,
            .bytes(message),
            .bytes(signature)
        ) {
            nativeHandle,
            messageBuffer,
            signatureBuffer in

            try invokeFnReturningBool {
                signal_publickey_verify(
                    $0,
                    nativeHandle.const(),
                    messageBuffer,
                    signatureBuffer
                )
            }
        }
    }
}
```

### 3.4 Resource Management

**Swift RAII Pattern:**
```swift
// NativeHandleOwner.swift - Base class for all FFI types

internal protocol NativeHandleOwner: AnyObject {
    associatedtype Handle: SignalMutPointer
    var nativeHandle: NonNull<Handle> { get }

    static func destroyNativeHandle(_ handle: NonNull<Handle>)
        -> SignalFfiErrorRef?
}

// Automatic cleanup via deinit
internal class SimpleNativeHandleOwner<Handle: SignalMutPointer>:
    NativeHandleOwner {

    var nativeHandle: NonNull<Handle>

    deinit {
        failOnError(Self.destroyNativeHandle(nativeHandle))
    }
}
```

### 3.5 Complete Function Trace: PublicKey.verifySignature

**Step 1: Rust Bridge Definition**
```rust
// rust/bridge/shared/src/protocol.rs

#[bridge_fn]
fn PublicKey_Verify(
    key: &PublicKey,
    message: &[u8],
    signature: &[u8],
) -> bool {
    key.verify_signature(message, signature)
}
```

**Step 2: FFI Generation (expanded)**
```rust
#[cfg(feature = "ffi")]
#[unsafe(export_name = "signal_publickey_verify")]
pub unsafe extern "C" fn __bridge_fn_ffi_publickey_verify(
    out: *mut bool,
    key: *const ffi::SignalPublicKey,
    message_data: *const u8,
    message_len: usize,
    signature_data: *const u8,
    signature_len: usize,
) -> *mut ffi::SignalFfiError {
    ffi::run_ffi_safe(|| {
        // Borrow pointer
        let mut key = <&PublicKey as ffi::ArgTypeInfo>::borrow(key)?;
        let key = <&PublicKey as ffi::ArgTypeInfo>::load_from(&mut key);

        // Borrow slice
        let mut message = BorrowedSliceOf {
            ptr: message_data,
            len: message_len
        };
        let message = <&[u8]>::load_from(&mut message);

        // Borrow slice
        let mut signature = BorrowedSliceOf {
            ptr: signature_data,
            len: signature_len
        };
        let signature = <&[u8]>::load_from(&mut signature);

        // Call original
        let result = PublicKey_Verify(key, message, signature);

        // Write to out
        ffi::write_result_to(out, result)?;
        Ok(())
    })
}
```

**Step 3: C Header (auto-generated)**
```c
SignalFfiError *signal_publickey_verify(
    bool *out,
    const SignalPublicKey *key,
    const uint8_t *message_data,
    size_t message_len,
    const uint8_t *signature_data,
    size_t signature_len
);
```

**Step 4: Swift Wrapper**
```swift
public func verifySignature(
    message: some ContiguousBytes,
    signature: some ContiguousBytes
) throws -> Bool {
    // withAllBorrowed manages lifetime of all arguments
    return try withAllBorrowed(
        self,              // PublicKey handle
        .bytes(message),   // Convert to buffer
        .bytes(signature)  // Convert to buffer
    ) { nativeHandle, messageBuffer, signatureBuffer in
        // invokeFnReturningBool handles error checking
        try invokeFnReturningBool {
            signal_publickey_verify(
                $0,                     // out: bool*
                nativeHandle.const(),   // key
                messageBuffer,          // message buffer
                signatureBuffer         // signature buffer
            )
        }
    }
}
```

**Step 5: Swift Usage**
```swift
let publicKey = try PublicKey(keyBytes)
let message = "Hello, World!".data(using: .utf8)!
let signature = signatureData

let isValid = try publicKey.verifySignature(
    message: message,
    signature: signature
)
```

---

## 4. Node.js/Neon Bridge

### 4.1 Neon Framework Integration

The Node bridge uses the Neon framework to create safe JavaScript/Rust bindings:

**Macro Code (rust/bridge/shared/macros/src/node.rs):**
```rust
pub(crate) fn bridge_fn(
    name: &str,
    sig: &Signature,
    bridging_kind: &BridgingKind,
) -> Result<TokenStream2> {
    let name_with_prefix = format_ident!("node_{}", name);
    let name_without_prefix = Ident::new(name, Span::call_site());

    let ts_signature_comment =
        generate_ts_signature_comment(name, sig, bridging_kind);

    let body = match sig.asyncness {
        Some(_) => bridge_fn_async_body(&sig.ident, name, &input_args),
        None => bridge_fn_body(&sig.ident, &input_args),
    };

    Ok(quote! {
        #[cfg(feature = "node")]
        #[allow(non_snake_case)]
        #[doc = #ts_signature_comment]
        pub fn #name_with_prefix(
            mut cx: node::FunctionContext,
        ) -> node::JsResult<node::JsValue> {
            #body
        }

        #[cfg(feature = "node")]
        node_register!(#name_without_prefix);
    })
}
```

### 4.2 Async/Promise Support

Node uniquely supports true async operations through Promises:

**Async Function Body Generation:**
```rust
fn bridge_fn_async_body(
    orig_name: &Ident,
    custom_name: &str,
    input_args: &[(&Ident, &Type)],
) -> TokenStream2 {
    // Save arguments in context-independent form
    let input_saving = input_args.iter().map(|(name, ty)| {
        let name_arg = format_ident!("{}_arg", name);
        let name_stored = format_ident!("{}_stored", name);
        quote! {
            let #name_arg = cx.borrow_mut()
                .argument::<<#ty as node::AsyncArgTypeInfo>::ArgType>(#i)?;
            let #name_stored =
                <#ty as node::AsyncArgTypeInfo>::save_async_arg(
                    &mut cx.borrow_mut(),
                    #name_arg
                )?;
        }
    });

    // Load arguments inside future
    let input_loading = input_args.iter().map(|(name, ty)| {
        let name_stored = format_ident!("{}_stored", name);
        quote! {
            let #name = <#ty as node::AsyncArgTypeInfo>::load_async_arg(
                &mut #name_stored
            );
        }
    });

    quote! {
        // Save args to context-independent storage
        #(#input_saving)*

        // Create and return promise
        Ok(node::run_future_on_runtime(
            &mut cx,
            async_runtime,
            #custom_name,
            |__cancel| async move {
                // Catch panics
                let __future = node::catch_unwind(
                    std::panic::AssertUnwindSafe(async {
                        #(#input_loading)*

                        // Support cancellation
                        ::tokio::select! {
                            __result = #orig_name(#(#input_names),*) => {
                                Ok(__result)
                            }
                            _ = __cancel => {
                                Err(node::CancellationError)
                            }
                        }
                    })
                );

                // Report result, finalize args
                node::FutureResultReporter::new(
                    __future.await,
                    (#(#inputs_to_finalize),*)
                )
            }
        )?.upcast())
    }
}
```

### 4.3 TypeScript Definition Generation

**Script: bin/gen_ts_decl.py**

Generates TypeScript definitions from Rust doc comments:

```python
# Extract signature from doc comment
def parse_ts_signature(doc_comment):
    # Look for "ts: export function ..."
    match = re.search(r'ts:\s*export\s+function\s+(\w+)', doc_comment)
    if match:
        return match.group(0)[4:]  # Strip "ts: "

# Type mappings
RUST_TO_TS = {
    'u32': 'number',
    '&[u8]': 'Buffer',
    'String': 'string',
    'bool': 'boolean',
    'Result<T>': 'T',  # unwrapped, throws exception
}
```

**Generated Native.d.ts:**
```typescript
// Auto-generated TypeScript definitions

export class PublicKey {
  constructor(keyBytes: Buffer);

  verifySignature(message: Buffer, signature: Buffer): boolean;

  seal(
    message: Buffer,
    info: Buffer,
    associatedData: Buffer
  ): Buffer;
}

export class Aes256GcmSiv {
  static new(key: Buffer): Aes256GcmSiv;

  encrypt(
    plaintext: Buffer,
    nonce: Buffer,
    associatedData: Buffer
  ): Buffer;

  decrypt(
    ciphertext: Buffer,
    nonce: Buffer,
    associatedData: Buffer
  ): Buffer;
}
```

### 4.4 npm Packaging

**package.json Structure:**
```json
{
  "name": "@signalapp/libsignal-client",
  "version": "0.86.5",
  "main": "dist/index.js",
  "types": "dist/index.d.ts",
  "files": [
    "dist/",
    "build/",
    "prebuilds/"
  ],
  "scripts": {
    "build": "neon build --release && tsc",
    "prebuild": "prebuildify --napi --strip"
  },
  "dependencies": {
    "@neon-rs/load": "^0.0.4"
  }
}
```

### 4.5 Complete Async Function Trace: CdsiLookup.complete

**Step 1: Rust Bridge Definition**
```rust
// rust/bridge/shared/src/net.rs

#[bridge_io(TokioAsyncContext)]
async fn CdsiLookup_complete(
    lookup: &mut cdsi::LookupRequest,
) -> Result<cdsi::LookupResponse> {
    lookup.complete().await
}
```

**Step 2: Node Generation (expanded)**
```rust
#[cfg(feature = "node")]
#[doc = "ts: export function CdsiLookup_complete(\
         asyncRuntime: &TokioAsyncContext, \
         lookup: Wrapper<CdsiLookup>\
         ): Promise<LookupResponse>"]
pub fn node_CdsiLookup_complete(
    mut cx: node::FunctionContext,
) -> node::JsResult<node::JsValue> {
    // Load async runtime from arg 0
    let async_runtime_arg = cx.borrow_mut()
        .argument::<<&TokioAsyncContext as node::AsyncArgTypeInfo>::ArgType>(0)?;
    let async_runtime_stored =
        <&TokioAsyncContext as node::AsyncArgTypeInfo>::save_async_arg(
            &mut cx.borrow_mut(),
            async_runtime_arg
        )?;

    // Load lookup handle from arg 1
    let lookup_arg = cx.borrow_mut()
        .argument::<<&mut cdsi::LookupRequest as node::AsyncArgTypeInfo>::ArgType>(1)?;
    let lookup_stored =
        <&mut cdsi::LookupRequest as node::AsyncArgTypeInfo>::save_async_arg(
            &mut cx.borrow_mut(),
            lookup_arg
        )?;

    // Create and return promise
    Ok(node::run_future_on_runtime(
        &mut cx,
        &async_runtime_stored,
        "CdsiLookup_complete",
        |__cancel| async move {
            let __future = node::catch_unwind(
                std::panic::AssertUnwindSafe(async {
                    // Load args in async context
                    let async_runtime =
                        <&TokioAsyncContext>::load_async_arg(
                            &mut async_runtime_stored
                        );
                    let lookup =
                        <&mut cdsi::LookupRequest>::load_async_arg(
                            &mut lookup_stored
                        );

                    // Call with cancellation support
                    ::tokio::select! {
                        __result = CdsiLookup_complete(lookup) => {
                            Ok(__result)
                        }
                        _ = __cancel => {
                            Err(node::CancellationError)
                        }
                    }
                })
            );

            // Return reporter to convert Result to JS
            node::FutureResultReporter::new(
                __future.await,
                (async_runtime_stored, lookup_stored)
            )
        }
    )?.upcast())
}
```

**Step 3: TypeScript Usage**
```typescript
import {
    TokioAsyncContext,
    CdsiLookup
} from '@signalapp/libsignal-client';

const runtime = TokioAsyncContext.new();
const lookup = await CdsiLookup.new(
    runtime,
    connectionManager,
    username,
    password,
    request
);

// Returns Promise<LookupResponse>
const response = await CdsiLookup.complete(runtime, lookup);

console.log(`Found ${response.entries.length} entries`);
```

---

## 5. Bridge Macro Deep-Dive

### 5.1 Type Mapping Tables

**Primitive Type Mappings:**

| Rust Type | FFI Type | JNI Type | Node Type |
|-----------|----------|----------|-----------|
| `bool` | `bool` | `jboolean` | `JsBoolean` |
| `u8` | `uint8_t` | `jbyte` | `JsNumber` |
| `u32` | `uint32_t` | `jint` | `JsNumber` |
| `u64` | `uint64_t` | `jlong` | `JsNumber` |
| `&[u8]` | `BorrowedSliceOf<u8>` | `JByteArray` | `JsBuffer` |
| `String` | `*const c_char` | `JString` | `JsString` |
| `Vec<u8>` | `OwnedBufferOf<u8>` | `jbyteArray` | `JsBuffer` |
| `Result<T>` | `SignalFfiResult<T>` | `T` (throws) | `T` (throws) |
| `Option<T>` | `*const T` (null) | Nullable | `null | T` |

**Handle Type Mappings:**

| Rust Type | FFI Type | JNI Type | Node Type |
|-----------|----------|----------|-----------|
| `&PublicKey` | `*const SignalPublicKey` | `long` | `PublicKey` |
| `&mut Aes256Ctr32` | `*mut SignalAes256Ctr32` | `long` | `Aes256Ctr32` |
| `Box<PrivateKey>` | `*mut SignalPrivateKey` | `long` | `PrivateKey` |

### 5.2 Macro Expansion Example

**Input:**
```rust
#[bridge_fn]
fn HKDF_DeriveSecrets(
    output_length: u32,
    ikm: &[u8],
    label: Option<&[u8]>,
    salt: Option<&[u8]>,
) -> Result<Vec<u8>> {
    let hkdf = hkdf::Hkdf::<sha2::Sha256>::new(
        salt.map(|s| s as &[u8]),
        ikm
    );
    let mut output = vec![0u8; output_length as usize];
    hkdf.expand(
        label.unwrap_or(b""),
        &mut output
    )
    .map_err(|_| Error::InvalidInput)?;
    Ok(output)
}
```

**FFI Output:**
```rust
#[unsafe(export_name = "signal_hkdf_derive_secrets")]
pub unsafe extern "C" fn __bridge_fn_ffi_hkdf_derive_secrets(
    out: *mut OwnedBufferOf<u8>,
    output_length: u32,
    ikm_data: *const u8,
    ikm_len: usize,
    label_data: *const u8,
    label_len: usize,
    salt_data: *const u8,
    salt_len: usize,
) -> *mut SignalFfiError {
    run_ffi_safe(|| {
        // Load output_length (trivial copy)
        let output_length = output_length;

        // Borrow ikm
        let mut ikm = BorrowedSliceOf { ptr: ikm_data, len: ikm_len };
        let ikm = <&[u8]>::load_from(&mut ikm);

        // Borrow label (optional)
        let mut label = if label_data.is_null() {
            None
        } else {
            Some(BorrowedSliceOf { ptr: label_data, len: label_len })
        };
        let label = <Option<&[u8]>>::load_from(&mut label);

        // Borrow salt (optional)
        let mut salt = if salt_data.is_null() {
            None
        } else {
            Some(BorrowedSliceOf { ptr: salt_data, len: salt_len })
        };
        let salt = <Option<&[u8]>>::load_from(&mut salt);

        // Call function
        let result = HKDF_DeriveSecrets(output_length, ikm, label, salt)?;

        // Write to out
        write_result_to(out, result)?;
        Ok(())
    })
}
```

**JNI Output:**
```rust
#[unsafe(export_name =
    "Java_org_signal_libsignal_internal_Native_HKDF_1DeriveSecrets")]
pub unsafe extern "C" fn __bridge_fn_jni_HKDF_DeriveSecrets<'local>(
    mut env: JNIEnv<'local>,
    _class: JClass,
    output_length: jint,
    ikm: JByteArray<'local>,
    label: JByteArray<'local>,
    salt: JByteArray<'local>,
) -> jbyteArray {
    jni::run_ffi_safe(&mut env, |env| {
        // Load output_length
        let output_length = u32::try_from(output_length)?;

        // Borrow ikm
        let mut ikm = unsafe {
            env.get_array_elements(&ikm, ReleaseMode::NoCopyBack)?
        };
        let ikm = <&[u8]>::load_from(&mut ikm);

        // Borrow label (null check)
        let mut label = if env.is_null(&label)? {
            None
        } else {
            Some(unsafe {
                env.get_array_elements(&label, ReleaseMode::NoCopyBack)?
            })
        };
        let label = <Option<&[u8]>>::load_from(&mut label);

        // Borrow salt (null check)
        let mut salt = if env.is_null(&salt)? {
            None
        } else {
            Some(unsafe {
                env.get_array_elements(&salt, ReleaseMode::NoCopyBack)?
            })
        };
        let salt = <Option<&[u8]>>::load_from(&mut salt);

        // Call and convert
        let result = HKDF_DeriveSecrets(output_length, ikm, label, salt)?;
        jni::ResultTypeInfo::convert_into(result, env)?
    })
}
```

**Node Output:**
```rust
#[doc = "ts: export function HKDF_DeriveSecrets(\
         outputLength: number, \
         ikm: Buffer, \
         label: Buffer | null, \
         salt: Buffer | null\
         ): Buffer"]
pub fn node_HKDF_DeriveSecrets(
    mut cx: FunctionContext,
) -> JsResult<JsValue> {
    // Get arg 0: output_length
    let output_length_arg = cx.argument::<JsNumber>(0)?;
    let mut output_length_stored =
        <u32 as ArgTypeInfo>::borrow(&mut cx, output_length_arg)?;
    let output_length = <u32>::load_from(&mut output_length_stored);

    // Get arg 1: ikm
    let ikm_arg = cx.argument::<JsBuffer>(1)?;
    let mut ikm_stored =
        <&[u8] as ArgTypeInfo>::borrow(&mut cx, ikm_arg)?;
    let ikm = <&[u8]>::load_from(&mut ikm_stored);

    // Get arg 2: label (nullable)
    let label_arg = cx.argument(2)?;
    let mut label_stored =
        <Option<&[u8]> as ArgTypeInfo>::borrow(&mut cx, label_arg)?;
    let label = <Option<&[u8]>>::load_from(&mut label_stored);

    // Get arg 3: salt (nullable)
    let salt_arg = cx.argument(3)?;
    let mut salt_stored =
        <Option<&[u8]> as ArgTypeInfo>::borrow(&mut cx, salt_arg)?;
    let salt = <Option<&[u8]>>::load_from(&mut salt_stored);

    // Call function
    let result = HKDF_DeriveSecrets(output_length, ikm, label, salt);

    // Convert result
    match TransformHelper(result).ok_if_needed() {
        Ok(TransformHelper(success)) =>
            Ok(ResultTypeInfo::convert_into(success, &mut cx)?.upcast()),
        Err(failure) => {
            let throwable = SignalNodeError::into_throwable(
                failure,
                &mut cx,
                "HKDF_DeriveSecrets"
            );
            cx.throw(throwable)?
        }
    }
}
```

---

## 6. Error Handling Across Bridges

### 6.1 Panic Catching

All bridge entry points catch panics to prevent unwinding across FFI boundaries:

**FFI Panic Handler (rust/bridge/shared/types/src/ffi/convert.rs):**
```rust
pub fn run_ffi_safe<F>(f: F) -> *mut SignalFfiError
where
    F: FnOnce() -> SignalFfiResult<()> + std::panic::UnwindSafe,
{
    match std::panic::catch_unwind(f) {
        Ok(Ok(())) => std::ptr::null_mut(),
        Ok(Err(err)) => Box::into_raw(Box::new(err.into())),
        Err(panic) => {
            let panic_msg = describe_panic(&panic);
            Box::into_raw(Box::new(SignalFfiError::UnexpectedPanic(
                panic_msg
            )))
        }
    }
}

pub fn describe_panic(any: &Box<dyn Any + Send>) -> String {
    if let Some(msg) = any.downcast_ref::<&str>() {
        msg.to_string()
    } else if let Some(msg) = any.downcast_ref::<String>() {
        msg.clone()
    } else {
        "(break on rust_panic to debug)".to_owned()
    }
}
```

**JNI Panic Handler (rust/bridge/shared/types/src/jni/convert.rs):**
```rust
pub fn run_ffi_safe<'local, F, R>(
    env: &mut JNIEnv<'local>,
    f: F,
) -> R::ResultType
where
    F: FnOnce(&mut JNIEnv<'local>) -> Result<R, BridgeLayerError>
         + std::panic::UnwindSafe,
    R: ResultTypeInfo<'local>,
{
    match std::panic::catch_unwind(AssertUnwindSafe(|| f(env))) {
        Ok(Ok(result)) => result.convert_into(env).expect("conversion"),
        Ok(Err(err)) => {
            throw_error(env, err);
            R::default_on_error()
        }
        Err(panic) => {
            let panic_msg = describe_panic(&panic);
            throw_error(
                env,
                BridgeLayerError::UnexpectedPanic(panic_msg)
            );
            R::default_on_error()
        }
    }
}

fn throw_error(env: &mut JNIEnv, error: BridgeLayerError) {
    let exception_class = error.exception_class();
    let _ = env.throw_new(exception_class, error.to_string());
}
```

**Node Panic Handler (rust/bridge/shared/types/src/node/convert.rs):**
```rust
pub fn catch_unwind<F, T>(future: F) -> impl Future<Output = Result<T>>
where
    F: Future<Output = T> + std::panic::UnwindSafe,
{
    async move {
        match AssertUnwindSafe(future).catch_unwind().await {
            Ok(result) => Ok(result),
            Err(panic) => {
                let panic_msg = describe_panic(&panic);
                Err(SignalNodeError::UnexpectedPanic(panic_msg))
            }
        }
    }
}
```

### 6.2 Result Type Conversion

**FFI Result Handling:**
```rust
impl<T> ResultTypeInfo for Result<T, SignalProtocolError>
where
    T: ResultTypeInfo,
{
    type ResultType = T::ResultType;

    fn convert_into(self) -> SignalFfiResult<Self::ResultType> {
        match self {
            Ok(value) => value.convert_into(),
            Err(err) => Err(err.into()),
        }
    }
}

// Writing results to output pointers
pub unsafe fn write_result_to<T>(
    out: *mut <T as ResultTypeInfo>::ResultType,
    value: T,
) -> SignalFfiResult<()>
where
    T: ResultTypeInfo,
{
    if out.is_null() {
        return Err(NullPointerError.into());
    }
    unsafe {
        *out = value.convert_into()?;
    }
    Ok(())
}
```

**JNI Exception Throwing:**
```rust
impl<'a, T> ResultTypeInfo<'a> for Result<T, SignalProtocolError>
where
    T: ResultTypeInfo<'a>,
{
    type ResultType = T::ResultType;

    fn convert_into(
        self,
        env: &mut JNIEnv<'a>,
    ) -> Result<Self::ResultType, BridgeLayerError> {
        match self {
            Ok(value) => value.convert_into(env),
            Err(err) => Err(err.into()),
        }
    }
}

// Exception mapping
impl From<SignalProtocolError> for BridgeLayerError {
    fn from(err: SignalProtocolError) -> Self {
        match err {
            SignalProtocolError::InvalidArgument(_) =>
                BridgeLayerError::IllegalArgument(err.to_string()),
            SignalProtocolError::InvalidState(_, _) =>
                BridgeLayerError::InvalidState(err.to_string()),
            // ... more mappings
        }
    }
}

impl BridgeLayerError {
    fn exception_class(&self) -> &str {
        match self {
            Self::IllegalArgument(_) =>
                "java/lang/IllegalArgumentException",
            Self::InvalidState(_) =>
                "java/lang/IllegalStateException",
            Self::Protocol(_) =>
                "org/signal/libsignal/protocol/InvalidMessageException",
            // ... more mappings
        }
    }
}
```

**Node Error Conversion:**
```rust
pub enum SignalNodeError {
    Signal(SignalProtocolError),
    IllegalArgument(String),
    CancellationError,
    UnexpectedPanic(String),
}

impl SignalNodeError {
    pub fn into_throwable<'a>(
        self,
        cx: &mut impl Context<'a>,
        operation_name: &str,
    ) -> Handle<'a, JsError> {
        let message = match &self {
            Self::Signal(err) => format!("{}: {}", operation_name, err),
            Self::IllegalArgument(msg) =>
                format!("{}: {}", operation_name, msg),
            Self::CancellationError =>
                format!("{}: operation cancelled", operation_name),
            Self::UnexpectedPanic(msg) =>
                format!("{}: panic: {}", operation_name, msg),
        };

        JsError::error(cx, message)
    }
}

impl<T> From<Result<T, SignalProtocolError>> for Result<T, SignalNodeError> {
    fn from(result: Result<T, SignalProtocolError>) -> Self {
        result.map_err(SignalNodeError::Signal)
    }
}
```

### 6.3 Error Propagation Example

**Complete error flow for invalid input:**

```rust
// Rust function
#[bridge_fn]
fn PublicKey_Deserialize(data: &[u8]) -> Result<PublicKey> {
    PublicKey::deserialize(data)
        .map_err(|_| SignalProtocolError::InvalidArgument(
            "invalid public key".to_string()
        ))
}
```

**FFI Error:**
```c
// C caller
SignalPublicKey *pub_key = NULL;
SignalFfiError *error = signal_publickey_deserialize(
    &pub_key,
    invalid_data,
    invalid_len
);

if (error != NULL) {
    char *msg = signal_error_get_message(error);
    printf("Error: %s\n", msg);  // "invalid public key"
    signal_free_string(msg);
    signal_error_free(error);
}
```

**JNI Exception:**
```kotlin
// Kotlin caller
try {
    val publicKey = Native.ECPublicKey_Deserialize(
        invalidData,
        0,
        invalidData.size
    )
} catch (e: IllegalArgumentException) {
    // Exception caught: "invalid public key"
    Log.e(TAG, "Failed to deserialize", e)
}
```

**Node Error:**
```typescript
// TypeScript caller
try {
    const publicKey = PublicKey.deserialize(invalidData);
} catch (e) {
    // Error object with message:
    // "PublicKey_Deserialize: invalid public key"
    console.error('Failed:', e.message);
}
```

---

## 7. Build System Integration

### 7.1 Feature Flags

Each bridge is enabled via Cargo features:

**Cargo.toml:**
```toml
[features]
default = []
ffi = ["dep:libsignal-bridge-types"]
jni = ["dep:jni", "dep:libsignal-bridge-types"]
node = ["dep:neon", "dep:libsignal-bridge-types"]

[dependencies]
libsignal-bridge-types = { version = "0.1", optional = true }
jni = { version = "0.21", optional = true }
neon = { version = "1.0", optional = true, default-features = false }
```

### 7.2 Environment Variables for Prefixes

**build.rs:**
```rust
fn main() {
    // Set FFI prefix for C exports
    println!(
        "cargo:rustc-env=LIBSIGNAL_BRIDGE_FN_PREFIX_FFI=signal_"
    );

    // Set JNI prefix for Java exports
    println!(
        "cargo:rustc-env=LIBSIGNAL_BRIDGE_FN_PREFIX_JNI=\
         Java_org_signal_libsignal_internal_Native_"
    );
}
```

### 7.3 Platform-Specific Compilation

**FFI (Swift):**
```bash
# Build for iOS
cargo build --release \
    --target aarch64-apple-ios \
    --features ffi

# Generate headers
cbindgen --config cbindgen.toml \
    --output signal_ffi.h
```

**JNI (Android):**
```bash
# Build for multiple Android ABIs
for target in \
    aarch64-linux-android \
    armv7-linux-androideabi \
    x86_64-linux-android \
    i686-linux-android
do
    cargo build --release \
        --target $target \
        --features jni
done

# Generate Java declarations
python bin/gen_java_decl.py
```

**Node:**
```bash
# Build native module
neon build --release

# Generate TypeScript definitions
python bin/gen_ts_decl.py
tsc --declaration
```

---

## 8. Advanced Topics

### 8.1 Callback Support

Some bridges support callbacks from Rust to the host language:

**Protocol Store Callbacks (JNI):**
```rust
// Rust trait
pub trait IdentityKeyStore {
    async fn get_identity_key_pair(&self)
        -> Result<IdentityKeyPair>;
    async fn get_local_registration_id(&self)
        -> Result<u32>;
}

// JNI implementation that calls back to Java
impl IdentityKeyStore for JniIdentityKeyStore<'_> {
    async fn get_identity_key_pair(&self)
        -> Result<IdentityKeyPair>
    {
        // Call Java method via JNI
        call_method_returning_serialized(
            self.env,
            self.store_obj,
            "getIdentityKeyPair",
            jni_args!(() -> org.signal.libsignal.protocol.IdentityKeyPair),
        )
    }
}
```

### 8.2 Custom Type Serialization

Complex types serialize through bridge-defined formats:

```rust
// Serializable handle type
bridge_serializable_handle_fns!(PreKeyRecord);

// Generates:
#[bridge_fn]
fn PreKeyRecord_Deserialize(data: &[u8]) -> Result<PreKeyRecord> {
    PreKeyRecord::deserialize(data)
}

#[bridge_fn]
fn PreKeyRecord_GetSerialized(record: &PreKeyRecord) -> Result<Vec<u8>> {
    Ok(record.serialize()?)
}
```

### 8.3 Zero-Copy Optimization

Byte slices use zero-copy borrows where possible:

```rust
// FFI: BorrowedSliceOf doesn't copy
impl<'a> ArgTypeInfo<'a> for &'a [u8] {
    type ArgType = BorrowedSliceOf<c_uchar>;
    type StoredType = Self::ArgType;

    fn borrow(foreign: Self::ArgType)
        -> SignalFfiResult<Self::StoredType>
    {
        Ok(foreign)  // Just store pointer/length
    }

    fn load_from(stored: &'a mut Self::StoredType) -> Self {
        unsafe {
            std::slice::from_raw_parts(stored.ptr, stored.len)
        }
    }
}
```

---

## Conclusion

The libsignal language bindings system demonstrates a sophisticated approach to multi-language FFI:

1. **Single Definition, Multiple Targets**: Write once in Rust, deploy to Swift, Java, and TypeScript
2. **Type Safety**: Compile-time guarantees across all language boundaries
3. **Error Safety**: Comprehensive panic catching and exception translation
4. **Performance**: Zero-overhead abstractions with minimal runtime cost
5. **Maintainability**: Procedural macros reduce boilerplate and ensure consistency

This architecture allows libsignal to maintain a single implementation while providing idiomatic APIs for each platform. The type conversion system, error handling mechanisms, and code generation pipeline work together to create safe, efficient bindings that feel native to each language ecosystem.

**Key Files Reference:**
- Core macros: `rust/bridge/shared/macros/src/{lib,ffi,jni,node}.rs`
- Type conversion: `rust/bridge/shared/types/src/{ffi,jni,node}/convert.rs`
- Bridge implementations: `rust/bridge/{ffi,jni,node}/src/lib.rs`
- Code generation: `bin/{gen_java_decl,gen_ts_decl}.py`
- Platform wrappers: `swift/Sources/LibSignalClient/*.swift`
- Java integration: `java/shared/java/org/signal/libsignal/internal/Native.kt`

For platform-specific implementation details, consult the individual bridge documentation and the test suites in each bridge's directory.
