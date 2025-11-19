# Gnus: Emacs Newsreader and Mail Client

**Files**: 106 files, 120,363 lines
**Location**: `/lisp/gnus/`
**Primary Authors**: Lars Magne Ingebrigtsen, Masanobu UMEDA
**Version**: 5.13

## Overview

Gnus is a sophisticated newsreader and mail client for Emacs, designed with a highly modular, pluggable backend architecture. Originally created as a newsreader, it has evolved into a comprehensive message handling system that can read news (NNTP), email (IMAP, POP3, local mail spools), RSS feeds, and more through a unified interface.

The name "Gnus" is pronounced "news" and stands for "Gnus Network User Services" (recursive acronym). The system exemplifies literate programming through its clear separation of concerns: user interface buffers (group, summary, article), backend abstraction layer, and pluggable storage backends.

### Architectural Philosophy

```
┌─────────────────────────────────────────────────────────────┐
│                      User Interface Layer                    │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │ Group Buffer │→ │Summary Buffer│→ │Article Buffer│       │
│  │ (gnus-group) │  │ (gnus-sum)   │  │ (gnus-art)   │       │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                   Core Abstraction Layer                     │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │  gnus-int    │  │    nnoo      │  │  gnus-start  │       │
│  │  (Backend    │  │  (Backend    │  │  (Startup &  │       │
│  │  Interface)  │  │  OO System)  │  │   Newsrc)    │       │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                    Backend Implementations                   │
│  ┌────────┐ ┌────────┐ ┌────────┐ ┌────────┐ ┌─────────┐  │
│  │  nntp  │ │ nnimap │ │  nnml  │ │nnmaildir│ │  nnrss  │  │
│  │ (NNTP) │ │ (IMAP) │ │ (Mail) │ │(Maildir)│ │  (RSS)  │  │
│  └────────┘ └────────┘ └────────┘ └────────┘ └─────────┘  │
│  ... and 20+ other backends ...                             │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                    Feature Modules                           │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐      │
│  │  Agent   │ │  Score   │ │  Search  │ │ Registry │      │
│  │(Offline) │ │ (Filter) │ │ (Index)  │ │(Tracking)│      │
│  └──────────┘ └──────────┘ └──────────┘ └──────────┘      │
└─────────────────────────────────────────────────────────────┘
```

## Core Architecture

### 1. Entry Point: gnus.el (4,204 lines)

**Purpose**: Main entry point, customization groups, and global configuration.

**File**: `/lisp/gnus/gnus.el`

The core entry point defines:
- **Customization Groups**: Hierarchical organization of all Gnus options
- **Global Variables**: Version info, home directory, select methods
- **Group Levels**: Subscription levels (subscribed, unsubscribed, zombie, killed)

```elisp
;; From gnus.el:
(defgroup gnus nil
  "The coffee-brewing, all singing, all dancing, kitchen sink newsreader."
  :group 'news
  :group 'mail)

;; Five subscription levels control group visibility
(defconst gnus-level-subscribed 5
  "Groups with levels less than or equal to this are subscribed.")
(defconst gnus-level-unsubscribed 7)
(defconst gnus-level-zombie 8)
(defconst gnus-level-killed 9)
```

**Design Pattern**: Gnus uses extensive customization groups to organize its hundreds of options. Each major component (group, summary, article, score, etc.) has dedicated customization hierarchies.

### 2. Group Buffer: gnus-group.el (4,869 lines)

**Purpose**: The *Group* buffer displays available newsgroups and mailboxes.

**File**: `/lisp/gnus/gnus-group.el`

The Group buffer is Gnus's "home screen" where users:
- Browse subscribed and unsubscribed groups
- See unread message counts
- Manage subscriptions and group parameters
- Access server configuration

**Key Data Structures**:

```elisp
;; Groups are stored in gnus-newsrc-hashtb and gnus-newsrc-alist
;; Each group entry contains:
;; - Group name (e.g., "nnimap+gmail:INBOX")
;; - Subscription level (1-9)
;; - Read articles (as ranges: "1-100,150,200-300")
;; - Group parameters (custom settings per group)

;; Group line format is customizable via specs:
(defcustom gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)\n"
  "Format of group lines.
%M    Only marked articles
%S    Whether subscribed (U/K/Z or space)
%y    Number of unread, unticked articles
%g    Qualified group name")
```

**Threading Model**: The group buffer maintains:
1. `gnus-newsrc-alist` - Complete list of groups with their state
2. `gnus-newsrc-hashtb` - Hash table for O(1) group lookup
3. Display list (potentially filtered/sorted) shown to user

### 3. Summary Buffer: gnus-sum.el (13,241 lines - largest file)

**Purpose**: Displays article lists with threading, scoring, and marking.

**File**: `/lisp/gnus/gnus-sum.el`

The Summary buffer is where Gnus's sophistication shines. It displays articles with:
- **Threading**: Builds conversation trees from References/In-Reply-To headers
- **Scoring**: Automatic and manual article prioritization
- **Marks**: Read, ticked, dormant, expirable, etc.
- **Limiting**: Filter articles by various criteria
- **Sorting**: Multiple sort orders (date, score, author, etc.)

**Threading Algorithm**:

```elisp
;; Threading builds a tree structure from article headers
;; Each article can have:
;; - Parent (article it replies to)
;; - Children (articles replying to it)
;; - Siblings (at same thread level)

;; Key variables for threading:
(defcustom gnus-summary-make-false-root 'adopt
  "How to handle threads with missing root articles.
- adopt: Make one child the parent
- dummy: Create a dummy root
- empty: Show with empty subject
- none: Don't gather loose threads")

(defcustom gnus-fetch-old-headers nil
  "Fetch old headers to build complete threads.
- nil: Don't fetch
- t: Fetch all old headers
- some: Fetch only connecting headers
- NUMBER: Fetch at most NUMBER old headers")
```

**Thread Building Process**:
1. Fetch article headers from backend
2. Extract Message-ID, References, In-Reply-To
3. Build hash table of all articles
4. Link articles to parents via References chain
5. Handle missing roots (adopt/dummy/empty)
6. Sort threads and sub-threads
7. Apply scoring and marks
8. Generate display lines

**Summary Line Format**:

```elisp
;; Summary lines use format specs similar to printf:
;; Example: "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"
;;
;; Common specs:
;; %U = User-defined marks (!, ?, etc.)
;; %R = Whether read (R or space)
;; %z = Zcore (article score)
;; %I = Indentation (for threading)
;; %L = Lines in article
;; %f = From header
;; %s = Subject
```

**Performance Optimizations**:
- Summary lines are pre-formatted and cached
- Threading uses hash tables for O(1) lookups
- Partial group entry (fetch headers in chunks)
- Prefetching of article bodies

### 4. Article Buffer: gnus-art.el (9,061 lines)

**Purpose**: Display and manipulate article content.

**File**: `/lisp/gnus/gnus-art.el`

The Article buffer handles:
- **Header Display**: Selective header showing/hiding
- **MIME Handling**: Multipart messages, attachments
- **Washing**: Remove quoted text, signatures, HTML rendering
- **Highlighting**: Citations, headers, signatures
- **Buttons**: Clickable URLs, email addresses, message IDs
- **Treatments**: Charset decoding, overstrike, ROT13, etc.

**MIME Processing Pipeline**:

```elisp
;; Article display pipeline:
;; 1. Fetch raw article from backend
;; 2. Parse MIME structure (mm-decode.el)
;; 3. Apply article treatments
;; 4. Render each MIME part
;; 5. Add buttons and highlighting
;; 6. Display in article buffer

;; Header visibility control:
(defcustom gnus-visible-headers
  "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^To:\\|..."
  "Headers matching this regexp are shown.
If non-nil, gnus-ignored-headers is ignored.")

(defcustom gnus-ignored-headers
  '("^Path:" "^Expires:" "^X-.*" ...)
  "Headers matching these regexps are hidden.")
```

**Treatment System**:

Gnus applies a series of "treatments" to articles:
- `gnus-treat-highlight-headers` - Colorize headers
- `gnus-treat-highlight-citation` - Color quoted text
- `gnus-treat-strip-trailing-blank-lines`
- `gnus-treat-hide-citation` - Hide excessive quoting
- `gnus-treat-decode-encoded-words` - MIME word decoding
- `gnus-treat-display-smileys` - Show emoji
- `gnus-treat-overstrike` - Handle _underline_

Each treatment can be:
- `nil` (never)
- `t` (always)
- `head` (only in headers)
- `last` (only in last part)
- A predicate function

### 5. Message Composition: message.el (9,065 lines)

**Purpose**: Compose and send email/news messages.

**File**: `/lisp/gnus/message.el`

**Not Gnus-Specific**: message.el is a standalone package used by Gnus but also usable independently. It provides:

- **Mail Composition**: Headers, body, attachments
- **News Posting**: Newsgroups, Followup-To, etc.
- **MIME Support**: via mml.el (MIME Meta Language)
- **Sending**: Multiple backends (SMTP, sendmail, feedmail)
- **Encryption**: PGP/MIME, S/MIME support

**Message Structure**:

```elisp
;; A message buffer contains:
;; 1. Headers (To:, Subject:, etc.)
;; 2. Separator line ("--text follows this line--")
;; 3. Body (may contain MML tags for attachments)

;; MML (MIME Meta Language) example:
;; <#multipart type=mixed>
;; Here's the text body
;; <#part type=image/png filename=screenshot.png disposition=attachment>
;; <#/multipart>

;; When sent, MML tags are converted to proper MIME structure
```

**Sending Pipeline**:
1. User composes message with optional MML tags
2. `message-send` validates headers
3. MML tags converted to MIME (mml.el)
4. Encoding applied (charset, transfer-encoding)
5. Send via configured method (SMTP, etc.)
6. Optionally save copy (Fcc header)
7. Update Gnus state (marks, registry, etc.)

## Backend System: The nnoo Architecture

### Backend Abstraction: nnoo.el

**Purpose**: Object-oriented backend system allowing pluggable storage.

**File**: `/lisp/gnus/nnoo.el`

Gnus's backend abstraction is one of its most elegant designs. The `nnoo` (nn-object-oriented) system allows backends to:
- Inherit from parent backends
- Override specific methods
- Share variable state
- Provide consistent interface

**Core Macros**:

```elisp
;; nnoo-declare: Declare a backend
(nnoo-declare nnml)        ; Declare nnml backend
(nnoo-declare nnimap)      ; Declare nnimap backend

;; defvoo: Define backend variable (like defvar)
(defvoo nnml-directory message-directory
  "Spool directory for the nnml mail backend.")

;; deffoo: Define backend function (like defun)
(deffoo nnml-retrieve-headers (articles &optional group server fetch-old)
  "Retrieve headers for ARTICLES in GROUP.")

;; nnoo-import: Inherit functions from parent backend
(nnoo-import nnml
  (nnmail))  ; Import functions from nnmail
```

**Backend Protocol**:

Every backend must implement these core functions:

```elisp
;; Essential functions:
(nnXXX-retrieve-headers articles group)
  ;; Return article headers in NOV format

(nnXXX-request-article article group)
  ;; Return article content

(nnXXX-request-group group &optional server)
  ;; Select a group, return article range

(nnXXX-close-group group)
  ;; Close the group

(nnXXX-request-list &optional server)
  ;; Return list of all groups

(nnXXX-open-server server)
  ;; Open connection to server

(nnXXX-close-server)
  ;; Close server connection

;; Optional functions:
(nnXXX-request-post)          ; Post news article
(nnXXX-request-move-article)  ; Move between groups
(nnXXX-request-accept-article); Accept incoming article
(nnXXX-request-expire-articles) ; Expire old articles
```

### Backend Interface: gnus-int.el

**Purpose**: Mediates between Gnus core and backends.

**File**: `/lisp/gnus/gnus-int.el`

The interface layer:
1. Dispatches requests to appropriate backend
2. Handles server state (opened, denied, offline)
3. Manages backend selection methods
4. Provides hooks for agent/registry integration

```elisp
;; Server status states:
;; - opened: Connection active
;; - closed: Not connected
;; - denied: Connection rejected
;; - offline: Agent mode (working unplugged)

(defun gnus-request-article (article group)
  "Request ARTICLE from GROUP."
  ;; 1. Find backend for this group
  ;; 2. Ensure server is open
  ;; 3. Call backend's request-article function
  ;; 4. Handle errors/retries
  )
```

### Major Backends

#### NNTP Backend: nntp.el (2,000+ lines)

**Purpose**: Read news via NNTP protocol (RFC 3977).

**File**: `/lisp/gnus/nntp.el`

```elisp
(defvoo nntp-address nil
  "Address of the physical nntp server.")

(defvoo nntp-port-number "nntp"
  "Port number (default 119).")

(defvoo nntp-open-connection-function 'nntp-open-network-stream
  "How to connect:
  - nntp-open-network-stream: TLS via STARTTLS
  - nntp-open-tls-stream: Direct TLS
  - nntp-open-plain-stream: Unencrypted
  - nntp-open-via-*: Via intermediate host")
```

**Connection Management**:
- Maintains persistent connections
- Handles authentication (AUTHINFO)
- Manages pipelining (multiple commands in flight)
- Detects server capabilities

**NOV (News Overview) Support**:
- Fetches headers efficiently via XOVER
- Parses NOV format (tab-separated)
- Falls back to HEAD for old servers

#### IMAP Backend: nnimap.el (2,700+ lines)

**Purpose**: Read email via IMAP protocol (RFC 3501).

**File**: `/lisp/gnus/nnimap.el`

```elisp
(defvoo nnimap-address nil
  "The address of the IMAP server.")

(defvoo nnimap-stream 'undecided
  "Connection type: undecided, tls, network, starttls, ssl, shell")

(defvoo nnimap-inbox nil
  "Mailbox for incoming mail splitting.
  Can be string or list: \"INBOX\" or (\"INBOX\" \"SENT\")")

(defvoo nnimap-split-methods nil
  "Mail splitting rules (same as nnmail-split-methods).")
```

**Key Features**:
- **Streaming**: Pipelines IMAP commands for speed
- **UID EXPUNGE**: Selective deletion support
- **IDLE**: Real-time notification of new mail
- **Namespaces**: Handles IMAP folder hierarchies
- **Splitting**: Server-side mail filtering

**Authentication**:
- Login, Plain, CRAM-MD5
- OAuth2 support
- Integration with auth-source

#### Mail Spool Backend: nnml.el (1,700+ lines)

**Purpose**: Local mail storage (one file per article).

**File**: `/lisp/gnus/nnml.el`

```elisp
(defvoo nnml-directory message-directory
  "Spool directory for nnml backend.")

(defvoo nnml-get-new-mail t
  "If non-nil, check incoming mail and split it.")

(defvoo nnml-nov-is-evil nil
  "If non-nil, don't use NOV databases.
  Using NOV is much faster but requires generation.")
```

**Storage Structure**:
```
~/Mail/
  active              ; List of groups and article ranges
  newsgroups          ; Group descriptions
  mail/
    misc/
      1               ; Article 1
      2               ; Article 2
      .overview       ; NOV database
    work/
      1
      2
      .overview
```

**NOV Database**:
- Pre-computed header cache
- Tab-separated format
- Generated by `nnml-generate-nov-databases`
- Dramatically speeds up summary generation

#### Other Notable Backends

**nnmaildir.el** - Maildir format (qmail, Courier)
- Atomic delivery (tmp/new/cur structure)
- Safe for concurrent access
- No file locking needed

**nnrss.el** - RSS/Atom feed reader
- Fetches feeds as "groups"
- Articles are feed items
- Supports enclosures

**nnvirtual.el** - Virtual groups
- Combines multiple groups
- Useful for searching, merging

**nnselect.el** - Search results as groups
- Used by gnus-search
- Ephemeral groups

**nndoc.el** - Files as groups
- Digest messages
- Mail archives
- Babyl, MMDF formats

**nnfolder.el** - Unix mbox format
- Single file per group
- Berkeley mail format

## Startup and State Management: gnus-start.el (3,199 lines)

**Purpose**: Initialize Gnus, read/write newsrc files, manage group state.

**File**: `/lisp/gnus/gnus-start.el`

### Startup Sequence

```elisp
;; Entry point: M-x gnus
(defun gnus ()
  "Read network news."
  ;; 1. Load ~/.gnus.el (user config)
  ;; 2. Read ~/.newsrc.eld (group state)
  ;; 3. Contact servers
  ;; 4. Check for new groups
  ;; 5. Update active files
  ;; 6. Display group buffer
  )
```

### The Newsrc Files

**~/.newsrc.eld**: Emacs Lisp Data file (primary state)
```elisp
;; Format:
(setq gnus-newsrc-alist
  '(("nnimap+gmail:INBOX" 3 ((1 . 1500)) nil)
    ("nnml:mail.misc" 1 ((1 . 250) 300) nil)))
;;     ^group-name    ^level ^read-articles ^params

(setq gnus-newsrc-hashtb
  #s(hash-table ...))  ; Hash table for fast lookup
```

**~/.newsrc**: Traditional newsreader format (compatibility)
```
nnimap+gmail:INBOX: 1-1500
nnml:mail.misc! 1-250,300
```

### Group Activation

```elisp
(defcustom gnus-activate-level (1+ gnus-level-subscribed)
  "Groups higher than this level won't be activated on startup.
  Setting this low speeds startup for users with many groups.")

;; Activation process:
;; 1. Contact backend for group
;; 2. Request article range (e.g., "1-5000")
;; 3. Update read marks
;; 4. Store in newsrc structures
```

### Level System

Groups have levels 1-9:
- **1-5**: Subscribed (shown by default)
- **6-7**: Unsubscribed (shown with 'L')
- **8**: Zombie (dead groups)
- **9**: Killed (completely hidden)

Levels allow:
- Selective display (show only level 1-3)
- Faster startup (don't activate high levels)
- Organizational hierarchy

## Feature Modules

### Offline Mode: gnus-agent.el (4,143 lines)

**Purpose**: Work with Gnus while disconnected from servers.

**File**: `/lisp/gnus/gnus-agent.el`

The Agent allows:
- **Fetching**: Download articles for offline reading
- **Queueing**: Compose messages while offline, send later
- **Synchronization**: Merge changes when reconnecting
- **Predicates**: Control what to download

```elisp
(defcustom gnus-agent-directory (concat gnus-directory "agent/")
  "Where the agent stores downloaded articles.")

;; Agent states:
;; - Plugged: Online, accessing servers directly
;; - Unplugged: Offline, using local cache

;; Download predicates control what to fetch:
(defcustom gnus-agent-predicate 'false
  "Predicate to control fetching.
  - true: Fetch all
  - false: Fetch none
  - (or (short) (scored 1000)): Fetch short or high-scoring")
```

**Agent Storage**:
```
~/News/agent/
  nnimap+gmail/
    INBOX/
      1.INC         ; Article 1
      2.INC         ; Article 2
      .agentview    ; Downloaded article list
  nntp+news/
    comp.emacs/
      100.INC
      101.INC
```

**Synchronization**:
When plugging in:
1. Upload queued mail/news
2. Optionally sync flags (read marks)
3. Optionally fetch new articles
4. Update active ranges

### Scoring System: gnus-score.el (3,188 lines)

**Purpose**: Automatically prioritize articles based on rules.

**File**: `/lisp/gnus/gnus-score.el`

Scoring assigns numeric values to articles based on:
- Subject keywords
- Author
- Thread depth
- Age
- Cross-posts
- Lines
- Custom predicates

```elisp
;; Score file format:
(("subject"
  ("emacs" 1000 nil s)      ; +1000 for "emacs" (substring)
  ("spam" -500 nil e))      ; -500 for "spam" (exact)
 ("from"
  ("alice@example.com" 100 nil s)))

;; Match types:
;; s = substring
;; e = exact
;; r = regexp
;; f = fuzzy
```

**Adaptive Scoring**:

Gnus can learn from your reading:
```elisp
(defcustom gnus-use-adaptive-scoring nil
  "If non-nil, learn scoring rules from reading behavior.

  Reading an article: increase score
  Marking as read: decrease score
  Following up: increase score significantly")

;; Adaptive rules are saved to GROUP.ADAPT files
```

**Score Files**:
- **Global**: Apply to all groups
- **Hierarchical**: `all.SCORE`, `comp.SCORE`, `comp.emacs.SCORE`
- **Group-specific**: `comp.emacs.SCORE`
- **Adaptive**: Auto-generated from behavior

**Decay**:
Scores can decay over time:
```elisp
(defcustom gnus-decay-scores nil
  "If non-nil, reduce scores over time.
  Prevents old rules from dominating.")
```

### Search System: gnus-search.el (2,363 lines)

**Purpose**: Unified search interface for multiple backends.

**File**: `/lisp/gnus/gnus-search.el`

The search system provides:
- **Unified Query Language**: Same syntax across backends
- **Multiple Engines**: IMAP, Mairix, Notmuch, Namazu, Swish++
- **Results as Groups**: Search results appear as nnselect groups

```elisp
;; Search query syntax:
;; "from:alice subject:emacs since:1w"
;;
;; Parsed to:
;; (and (from "alice")
;;      (subject "emacs")
;;      (since "1w"))

;; Search engines:
;; - gnus-search-imap: Use IMAP SEARCH
;; - gnus-search-notmuch: Use notmuch
;; - gnus-search-mairix: Use mairix
;; - gnus-search-namazu: Use Namazu
```

**Search Flow**:
1. User enters query string
2. Parse query to s-expression
3. Categorize groups by server
4. Find search engine for each server
5. Transform query to engine-specific format
6. Execute searches
7. Collect results
8. Create nnselect group displaying results

### Article Registry: gnus-registry.el (1,304 lines)

**Purpose**: Track articles across groups, backends, and time.

**File**: `/lisp/gnus/gnus-registry.el`

The registry maintains a database of:
- Article Message-IDs
- Groups where article appears
- Custom marks
- Thread relationships
- Keywords/tags

```elisp
;; Registry database structure:
;; Message-ID -> {groups, marks, keywords, subjects, senders}

;; Use cases:
;; 1. Split by parent: Reply goes to same group as parent
;; 2. Track moved articles
;; 3. Find all copies of an article
;; 4. Persistent marks across backends
;; 5. Thread reconstruction

(defcustom gnus-registry-max-entries 2500
  "Maximum number of articles to track.")

(defcustom gnus-registry-track-extra '(sender subject recipient)
  "What extra data to track for each article.")
```

**Registry Splitting**:
```elisp
;; In fancy-split rules:
(: gnus-registry-split-fancy-with-parent)

;; This places replies in the same group as the parent,
;; even across backends!
```

### Topic Mode: gnus-topic.el (1,798 lines)

**Purpose**: Organize groups hierarchically.

**File**: `/lisp/gnus/gnus-topic.el`

Topics provide:
- **Hierarchical Grouping**: Organize groups in trees
- **Folding**: Hide/show topic branches
- **Bulk Operations**: Act on all groups in topic
- **Visual Organization**: Indented display

```elisp
;; Topic structure:
;; Gnus
;;   Mail
;;     Work
;;       nnimap+work:INBOX
;;       nnimap+work:Projects
;;     Personal
;;       nnml:mail.misc
;;   News
;;     Emacs
;;       gmane.emacs.gnus.general
;;       comp.emacs
```

**Topic Topology**:
```elisp
(defvar gnus-topic-topology
  '(("Gnus" visible)
    (("Mail" visible)
     (("Work" visible))
     (("Personal" visible)))
    (("News" visible)
     (("Emacs" visible)))))

(defvar gnus-topic-alist
  '(("Work" "nnimap+work:INBOX" "nnimap+work:Projects")
    ("Personal" "nnml:mail.misc")
    ("Emacs" "gmane.emacs.gnus.general" "comp.emacs")))
```

## MIME Handling

### MIME Meta Language: mml.el (1,800+ lines)

**Purpose**: User-friendly MIME message composition.

**File**: `/lisp/gnus/mml.el`

MML provides a simple tag syntax for creating MIME messages:

```elisp
;; MML tags in message buffer:
;; <#part type=text/plain>
;; This is plain text.
;; <#/part>
;; <#part type=image/png filename=screenshot.png disposition=attachment>
;; <#/part>

;; Converted to MIME on send:
;; Content-Type: multipart/mixed; boundary="=-=-="
;;
;; --=-=-=
;; Content-Type: text/plain
;;
;; This is plain text.
;; --=-=-=
;; Content-Type: image/png
;; Content-Disposition: attachment; filename=screenshot.png
;; Content-Transfer-Encoding: base64
;;
;; iVBORw0KGgoAAAANSUhEUgAA...
;; --=-=-=--
```

**MML Functions**:
- `mml-attach-file`: Attach a file
- `mml-insert-part`: Insert MIME part
- `mml-to-mime`: Convert MML tags to MIME
- `mml-preview`: Preview message as MIME

### MIME Decoding: mm-decode.el (2,000+ lines)

**Purpose**: Parse and display MIME messages.

**File**: `/lisp/gnus/mm-decode.el`

```elisp
;; MIME handle structure:
;; (buffer type encoding undisplayer disposition description cache id)

;; Display actions:
(defcustom mm-text-html-renderer
  (cond ((fboundp 'libxml-parse-html-region) 'shr)
        ((executable-find "w3m") 'gnus-w3m)
        (t 'shr))
  "How to render HTML:
  - shr: Built-in Emacs HTML renderer
  - gnus-w3m: Use w3m in Emacs
  - w3m: External w3m
  - links/lynx: External text browsers")
```

**MIME Decoding Pipeline**:
1. Parse Content-Type headers
2. Build handle tree for multipart messages
3. Decode transfer encodings (base64, quoted-printable)
4. Convert charsets
5. Display each part via appropriate viewer
6. Handle alternative parts (prefer HTML vs text)

### Other MIME Modules

**mm-encode.el**: Encode content for sending
- Choose transfer encoding
- Handle charsets
- Generate boundaries

**mm-view.el**: Display MIME parts
- Inline images
- External viewers
- Button creation

**mm-util.el**: MIME utilities
- Charset handling
- Encoding detection
- Multibyte operations

**mm-uu.el**: Uuencode/shar detection
- Find encoded sections in plain text
- Decode automatically

## Integration Features

### Cloud Synchronization: gnus-cloud.el (600+ lines)

**Purpose**: Sync newsrc and other files via IMAP.

**File**: `/lisp/gnus/gnus-cloud.el`

```elisp
(defcustom gnus-cloud-synced-files
  '("~/.authinfo.gpg"
    "~/.gnus.el"
    (:directory "~/News" :match ".*.SCORE\\'"))
  "Files to sync across machines.")

(defcustom gnus-cloud-storage-method
  (if (featurep 'epg) 'epg 'base64-gzip)
  "How to encode data:
  - epg: Encrypt with GPG
  - base64-gzip: Compress and encode
  - base64: Just encode")
```

**Sync Process**:
1. Upload files to special IMAP folder
2. Store as email messages
3. Download on other machine
4. Decrypt/decompress
5. Write to files

### Message Encryption: mml-sec.el

**Purpose**: PGP/MIME and S/MIME support.

**File**: `/lisp/gnus/mml-sec.el`

```elisp
;; MML security tags:
;; <#secure method=pgpmime mode=sign>
;; Message to sign
;; <#/secure>

;; <#secure method=smime mode=encrypt>
;; Encrypted message
;; <#/secure>

;; Methods:
;; - pgpmime: PGP/MIME (RFC 3156)
;; - smime: S/MIME
;; - pgp: Old-style PGP

;; Modes:
;; - sign: Digital signature only
;; - encrypt: Encryption only
;; - signencrypt: Both
```

### Spam Filtering: spam.el (3,000+ lines)

**Purpose**: Integrate with spam filters (SpamAssassin, Bogofilter, etc.)

**File**: `/lisp/gnus/spam.el`

```elisp
;; Spam processing:
;; 1. Mark messages as spam/ham
;; 2. Train filter
;; 3. Move to appropriate group
;; 4. Report to blacklists

(defcustom spam-split-group "spam"
  "Group for suspected spam.")

;; Backends:
;; - spam-use-bogofilter
;; - spam-use-spamassassin
;; - spam-use-spamoracle
;; - spam-use-BBDB (check against address book)
;; - spam-use-regex-headers
```

### Utilities and Infrastructure

**gnus-util.el** (1,544 lines): Core utilities
- Date parsing
- String operations
- Hash table helpers
- Process management

**gnus-spec.el**: Format specifications
- Compile format strings to functions
- Caching for performance

**gnus-range.el**: Range operations
- Compact representation (1-100,150,200-300)
- Union, intersection, difference
- Efficient storage

**gnus-undo.el**: Undo system
- Track operations
- Restore group/summary state
- Transactional changes

## Data Flow Examples

### Reading News

```
User presses RET on group
  ↓
gnus-group-read-group
  ↓
gnus-summary-read-group
  ↓
gnus-select-newsgroup
  ↓
gnus-request-group (via gnus-int)
  ↓
Backend: nntp-request-group
  → "GROUP comp.emacs" to server
  ← "211 450 1 450 comp.emacs"
  ↓
gnus-get-unread-articles-in-group
  ↓
gnus-retrieve-headers (via gnus-int)
  ↓
Backend: nntp-retrieve-headers
  → "XOVER 1-450" to server
  ← NOV data
  ↓
gnus-get-newsgroup-headers
  → Parse NOV lines
  → Build threading
  → Apply scoring
  ↓
gnus-summary-prepare
  → Format summary lines
  → Display in buffer
```

### Sending Mail

```
User composes message
  ↓
M-x message-send-and-exit
  ↓
message-send
  ↓
message-do-fcc (save copy)
  → gnus-request-accept-article
  → Backend saves to Sent group
  ↓
mml-to-mime (process MML tags)
  → Build MIME structure
  → Encode attachments
  → Generate boundaries
  ↓
message-send-mail
  ↓
smtpmail-send-it
  → Connect to SMTP server
  → Send EHLO
  → STARTTLS (if supported)
  → AUTH (if needed)
  → MAIL FROM
  → RCPT TO
  → DATA
  → Send message
  → QUIT
  ↓
gnus-registry-handle-action
  → Record in registry
```

### Mail Splitting

```
New mail arrives in INBOX
  ↓
gnus-request-scan (or backend auto-check)
  ↓
nnmail-split-incoming
  ↓
nnmail-split-fancy (or nnmail-split-methods)
  → Evaluate split rules:
    ("^From:.*alice" "mail.alice")
    ("^Subject:.*work" "mail.work")
    ((: gnus-registry-split-fancy-with-parent))
  ↓
For each message:
  → Check rules in order
  → First match wins
  → Move to target group
  ↓
gnus-request-accept-article
  → Backend saves to group
  → Update active file
  → Generate NOV entry
```

## Performance Considerations

### Startup Performance

```elisp
;; Techniques to speed startup:

;; 1. Lazy server connection
(setq gnus-check-new-newsgroups nil)  ; Don't scan for new groups

;; 2. Limited activation
(setq gnus-activate-level 3)  ; Only activate levels 1-3

;; 3. Partial group entry
(setq gnus-large-newsgroup 1000)  ; Prompt for partial entry

;; 4. Asynchronous operations
(setq gnus-asynchronous t)    ; Prefetch in background
(setq gnus-use-article-prefetch 15)  ; Prefetch next 15 articles
```

### Summary Generation

```elisp
;; Threading performance:
;; - Hash tables for O(1) message lookup
;; - Compiled format specs (gnus-spec.el)
;; - Cached summary lines

;; NOV databases:
;; - Pre-computed header cache
;; - Single file read vs. N file reads
;; - Dramatically faster than parsing articles

;; Example speedup:
;; Without NOV: 50 articles/second
;; With NOV: 5000 articles/second
```

### Memory Management

```elisp
;; Summary buffer data structures:
;; - gnus-newsgroup-data: Article headers
;; - gnus-newsgroup-threads: Thread tree
;; - gnus-summary-buffer: Formatted display

;; Memory is freed when exiting group:
(defcustom gnus-kill-summary-on-exit t
  "Kill summary buffer on exit to reclaim memory.")

;; Article buffer reuse:
;; Single article buffer is reused, not created per article
```

## Customization Patterns

### Group Parameters

Groups can have custom parameters:

```elisp
;; Set group parameter:
(gnus-group-set-parameter "nnimap+gmail:INBOX"
                          'display 'all)

;; Common parameters:
;; - to-address: Mailing list address
;; - to-list: Mailing list ID
;; - broken-reply-to: Override broken Reply-To
;; - gcc-self: Save copies of sent messages
;; - posting-style: Custom From/Sig for group
;; - expire-days: Group-specific expiry
;; - score-file: Group score file
```

### Select Methods

```elisp
;; Primary select method:
(setq gnus-select-method
      '(nntp "news.example.com"))

;; Secondary select methods:
(setq gnus-secondary-select-methods
      '((nnimap "gmail"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl))
        (nnml "mail"
              (nnml-directory "~/Mail"))))

;; Method format:
;; (BACKEND SERVER-NAME PARAMETER...)
```

### Hooks

Gnus provides numerous hooks:

```elisp
;; Startup:
gnus-started-hook              ; After Gnus starts
gnus-before-startup-hook       ; Before connecting

;; Group buffer:
gnus-group-mode-hook           ; Group buffer created
gnus-select-group-hook         ; Before entering group

;; Summary buffer:
gnus-summary-mode-hook         ; Summary buffer created
gnus-summary-prepared-hook     ; After summary generated
gnus-select-article-hook       ; Article selected

;; Article buffer:
gnus-article-mode-hook         ; Article buffer created
gnus-article-prepare-hook      ; Before displaying article

;; Message composition:
message-mode-hook              ; Message buffer created
message-send-hook              ; Before sending
message-sent-hook              ; After sending
```

## Design Patterns and Idioms

### The Three-Buffer Model

Gnus's core UI uses three connected buffers:

1. **Group Buffer** (*Group*): Directory of newsgroups
   - Entry point
   - Shows unread counts
   - Low-frequency updates

2. **Summary Buffer** (*Summary GROUPNAME*): Article list
   - Threading display
   - High-frequency scanning
   - Marks and scores visible

3. **Article Buffer** (*Article GROUPNAME*): Content display
   - Read-only (usually)
   - MIME rendering
   - Large content

**Navigation**: Each buffer has commands to move "deeper":
- Group → Summary: `RET` or `SPC`
- Summary → Article: `RET` or `SPC`
- Back: `q` quits current buffer

### Format Specifications

Gnus uses printf-style format strings extensively:

```elisp
;; Format specs are compiled to functions for speed
;; Example: "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"

;; Compilation process (gnus-spec.el):
;; 1. Parse format string
;; 2. Generate Lisp code
;; 3. Byte-compile function
;; 4. Cache compiled function

;; Result: ~10x faster than interpreting format string
```

### Backend Inheritance

```elisp
;; Backends inherit via nnoo-import:

(nnoo-declare nndraft nnmh)  ; nndraft inherits from nnmh
(nnoo-import nndraft (nnmh))  ; Import all nnmh functions

;; Override specific functions:
(deffoo nndraft-request-accept-article (...)
  ;; Custom implementation
  )

;; Benefit: Code reuse, minimal duplication
```

### Range Compression

```elisp
;; Article numbers stored as ranges:
;; "1-100,105,110-150" instead of 145 individual numbers

;; Operations:
(gnus-range-add '((1 . 100)) 101)
  → ((1 . 101))

(gnus-range-difference '((1 . 100)) '((50 . 60)))
  → ((1 . 49) (61 . 100))

;; Benefits:
;; - Compact storage
;; - Fast operations
;; - Efficient wire protocol
```

## Testing and Debugging

### Debug Variables

```elisp
;; Enable debugging:
(setq gnus-verbose 10)          ; Max verbosity
(setq gnus-verbose-backends 10) ; Backend verbosity
(setq nntp-record-commands t)   ; Log NNTP commands

;; Network tracing:
(setq nnimap-log-commands t)    ; IMAP command log
```

### Repair Commands

```elisp
;; Rebuild summary:
M-x gnus-summary-rescan-group

;; Regenerate NOV:
M-x nnml-generate-nov-databases

;; Repair newsrc:
M-x gnus-group-clear-data

;; Reset server:
M-x gnus-close-server
M-x gnus-open-server
```

## Historical Context

Gnus evolved from GNUS (written by Masanobu UMEDA in 1987), which itself was based on NNTP (Network News Transfer Protocol) readers of the 1980s.

**Evolution**:
- **GNUS** (1987): Original newsreader
- **Gnus 5.x** (1995): Major rewrite by Lars Ingebrigtsen
  - Pluggable backend system (nnoo)
  - Scoring and adaptive scoring
  - MIME support
- **Gnus 5.8+** (2000s): Email features mature
  - IMAP support
  - Agent (offline mode)
  - Registry
- **Gnus 5.13** (2010s-present): Modern features
  - HTML rendering (shr)
  - OAuth2 authentication
  - Cloud synchronization
  - Unified search

**Philosophy**: "Gnus is not just a newsreader; it's a way of life."

The design emphasizes:
- **Flexibility**: Highly customizable
- **Extensibility**: Plugin architecture
- **Power**: Complex features for advanced users
- **Integration**: Deep Emacs integration

## Code Statistics

```
Component               Lines    Purpose
────────────────────────────────────────────────────────────
gnus-sum.el           13,241    Summary buffer (article lists)
gnus-art.el            9,061    Article display
message.el             9,065    Message composition
gnus-group.el          4,869    Group buffer
gnus.el                4,204    Core definitions
gnus-agent.el          4,143    Offline mode
gnus-start.el          3,199    Startup/newsrc
gnus-score.el          3,188    Scoring system
nntp.el                2,700    NNTP backend
nnimap.el              2,700    IMAP backend
gnus-search.el         2,363    Search system
nnmail.el              2,300    Mail backend utilities
mm-decode.el           2,100    MIME decoding
gnus-uu.el             2,149    Binary extraction
gnus-msg.el            1,947    Message interface
mml.el                 1,800    MIME composition
gnus-topic.el          1,798    Topic mode
nnml.el                1,700    Mail spool backend
nnmaildir.el           1,900    Maildir backend
gnus-registry.el       1,304    Article registry
────────────────────────────────────────────────────────────
28 Backends           ~30,000   (nntp, nnimap, nnml, nnrss, etc.)
10 MIME modules        ~8,000   (mm-*.el)
30+ Feature modules   ~40,000   (cache, cite, cloud, demon, etc.)
────────────────────────────────────────────────────────────
Total                120,363    106 files
```

## Key Takeaways

**Gnus demonstrates**:

1. **Layered Architecture**: Clean separation between UI, core, and backends
2. **Plugin System**: Backends are swappable implementations of protocol
3. **Data Abstraction**: Ranges, hash tables, format specs optimize performance
4. **Extensive Customization**: Hundreds of options, hooks, parameters
5. **Feature Modularity**: Agent, scoring, registry are independent modules
6. **Protocol Support**: NNTP, IMAP, local spools, RSS via unified interface
7. **MIME Handling**: Comprehensive multipart message support
8. **Threading**: Sophisticated conversation reconstruction
9. **Offline Operation**: Agent enables disconnected workflows
10. **Long-term Evolution**: 35+ years of development, maintaining backwards compatibility

**Modern Relevance**:

Despite email clients like Thunderbird and webmail, Gnus remains relevant because:
- **Emacs Integration**: Unified environment for email, news, RSS
- **Keyboard Efficiency**: No mouse required
- **Programmability**: Elisp customization for any workflow
- **Backend Flexibility**: Read from multiple sources simultaneously
- **Privacy**: Complete control over data
- **Power Features**: Scoring, threading, splitting beyond typical clients

Gnus exemplifies how literate, modular design enables a complex system to evolve while remaining maintainable.
