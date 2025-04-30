;; Stacks Patients Registry
;;
;; 
;; ==========================================
;; SECTION 1: CONSTANT DEFINITIONS AND ERROR CODES
;; ==========================================

;; System-wide error codes for operation validation
(define-constant FAILURE_DOCUMENT_MISSING (err u301))       ;; Requested document cannot be found
(define-constant FAILURE_DOCUMENT_DUPLICATE (err u302))     ;; Document with given ID already exists
(define-constant FAILURE_PROVIDER_VALIDATION (err u306))    ;; Provider validation unsuccessful
(define-constant FAILURE_ADMIN_REQUIRED (err u300))         ;; Action restricted to admin users
(define-constant FAILURE_TEXT_FORMAT (err u303))            ;; Text format requirements not met
(define-constant FAILURE_DIMENSION_CONSTRAINT (err u304))   ;; Size constraints not satisfied
(define-constant FAILURE_CREDENTIALS (err u305))            ;; User lacks required credentials
(define-constant FAILURE_CATEGORY_VALIDATION (err u307))    ;; Category validation failed
(define-constant FAILURE_ACCESS_RESTRICTION (err u308))     ;; Access restrictions prevent operation

;; Registry administration configuration
(define-constant registry-administrator tx-sender)  ;; Registry administrator is set to contract deployer

;; ==========================================
;; SECTION 2: GLOBAL STATE VARIABLES
;; ==========================================

;; Registry statistics tracking
(define-data-var document-counter uint u0)  ;; Tracks total number of documents in the registry

;; ==========================================
;; SECTION 3: DATA STORAGE STRUCTURES
;; ==========================================

;; Primary storage for healthcare documentation entries
(define-map healthcare-documentation
  { document-identifier: uint }
  {
    subject-identifier: (string-ascii 64),  ;; Subject's identifying information
    provider-identifier: principal,         ;; Healthcare provider's identity
    document-bytes: uint,                   ;; Size measurement of the document
    timestamp: uint,                        ;; Creation timestamp (block height)
    clinical-summary: (string-ascii 128),   ;; Brief clinical summary
    categories: (list 10 (string-ascii 32)) ;; Classification categories
  }
)

;; Access control registry for documentation
(define-map access-control
  { document-identifier: uint, accessor-identifier: principal }
  { access-enabled: bool }  ;; Access status flag
)

;; ==========================================
;; SECTION 4: PRIVATE UTILITY FUNCTIONS
;; ==========================================

;; Verifies existence of a document in the registry
(define-private (document-registered? (document-identifier uint))
  (is-some (map-get? healthcare-documentation { document-identifier: document-identifier }))
)

;; Confirms provider ownership of document
(define-private (is-provider-authorized? (document-identifier uint) (provider principal))
  (match (map-get? healthcare-documentation { document-identifier: document-identifier })
    document-info (is-eq (get provider-identifier document-info) provider)
    false
  )
)

;; Retrieves byte size of specified document
(define-private (calculate-document-size (document-identifier uint))
  (default-to u0
    (get document-bytes
      (map-get? healthcare-documentation { document-identifier: document-identifier })
    )
  )
)

;; Performs validation on individual category string
(define-private (validate-category-format (category (string-ascii 32)))
  (and 
    (> (len category) u0)
    (< (len category) u33)
  )
)

;; Validates the entire category list against defined constraints
(define-private (validate-category-list (categories (list 10 (string-ascii 32))))
  (and
    (> (len categories) u0)                                   ;; At least one category required
    (<= (len categories) u10)                                 ;; Maximum 10 categories allowed
    (is-eq (len (filter validate-category-format categories)) (len categories))  ;; All categories must be valid
  )
)

;; ==========================================
;; SECTION 5: PUBLIC INTERFACE FUNCTIONS
;; ==========================================

;; Register new healthcare documentation in the system
(define-public (register-document 
  (subject-identifier (string-ascii 64))      ;; Subject identification information
  (document-bytes uint)                       ;; Document size measurement
  (clinical-summary (string-ascii 128))       ;; Clinical observations and notes
  (categories (list 10 (string-ascii 32)))    ;; Classification categories
)
  (let
    (
      (document-identifier (+ (var-get document-counter) u1))  ;; Generate unique document identifier
    )
    ;; Input validation for all parameters
    (asserts! (> (len subject-identifier) u0) FAILURE_TEXT_FORMAT)        ;; Subject identifier cannot be empty
    (asserts! (< (len subject-identifier) u65) FAILURE_TEXT_FORMAT)       ;; Subject identifier length constraint
    (asserts! (> document-bytes u0) FAILURE_DIMENSION_CONSTRAINT)         ;; Document must have positive size
    (asserts! (< document-bytes u1000000000) FAILURE_DIMENSION_CONSTRAINT) ;; Maximum size constraint
    (asserts! (> (len clinical-summary) u0) FAILURE_TEXT_FORMAT)          ;; Clinical summary cannot be empty
    (asserts! (< (len clinical-summary) u129) FAILURE_TEXT_FORMAT)        ;; Clinical summary length constraint
    (asserts! (validate-category-list categories) FAILURE_CATEGORY_VALIDATION) ;; Categories must be valid

    ;; Store document information in primary storage
    (map-insert healthcare-documentation
      { document-identifier: document-identifier }
      {
        subject-identifier: subject-identifier,
        provider-identifier: tx-sender,        ;; Current transaction sender is the provider
        document-bytes: document-bytes,
        timestamp: block-height,               ;; Current block height as timestamp
        clinical-summary: clinical-summary,
        categories: categories
      }
    )

    ;; Initialize access permissions for the provider
    (map-insert access-control
      { document-identifier: document-identifier, accessor-identifier: tx-sender }
      { access-enabled: true }
    )

    ;; Update registry statistics
    (var-set document-counter document-identifier)
    (ok document-identifier)  ;; Return newly created document identifier
  )
)

;; Update healthcare provider association for an existing document
(define-public (reassign-document-provider (document-identifier uint) (new-provider principal))
  (let
    (
      (document-details (unwrap! (map-get? healthcare-documentation { document-identifier: document-identifier }) FAILURE_DOCUMENT_MISSING))
    )
    ;; Verify document exists and authorization
    (asserts! (document-registered? document-identifier) FAILURE_DOCUMENT_MISSING)
    (asserts! (is-eq (get provider-identifier document-details) tx-sender) FAILURE_CREDENTIALS)

    ;; Update provider information
    (map-set healthcare-documentation
      { document-identifier: document-identifier }
      (merge document-details { provider-identifier: new-provider })
    )
    (ok true)  ;; Confirm successful update
  )
)

;; Retrieve category classifications for a document
(define-public (retrieve-document-categories (document-identifier uint))
  (let
    (
      (document-details (unwrap! (map-get? healthcare-documentation { document-identifier: document-identifier }) FAILURE_DOCUMENT_MISSING))
    )
    ;; Return categories associated with the document
    (ok (get categories document-details))
  )
)

;; Lookup the provider responsible for a document
(define-public (lookup-document-provider (document-identifier uint))
  (let
    (
      (document-details (unwrap! (map-get? healthcare-documentation { document-identifier: document-identifier }) FAILURE_DOCUMENT_MISSING))
    )
    ;; Return provider identifier
    (ok (get provider-identifier document-details))
  )
)

;; Retrieve document creation timestamp
(define-public (get-document-timestamp (document-identifier uint))
  (let
    (
      (document-details (unwrap! (map-get? healthcare-documentation { document-identifier: document-identifier }) FAILURE_DOCUMENT_MISSING))
    )
    ;; Return block height timestamp when document was created
    (ok (get timestamp document-details))
  )
)

;; Get total number of documents in registry
(define-public (query-document-count)
  ;; Return current document counter value
  (ok (var-get document-counter))
)

;; Query size of a specific document by identifier
(define-public (query-document-size (document-identifier uint))
  (let
    (
      (document-details (unwrap! (map-get? healthcare-documentation { document-identifier: document-identifier }) FAILURE_DOCUMENT_MISSING))
    )
    ;; Return document byte size
    (ok (get document-bytes document-details))
  )
)

;; Retrieve clinical summary for a document
(define-public (retrieve-clinical-summary (document-identifier uint))
  (let
    (
      (document-details (unwrap! (map-get? healthcare-documentation { document-identifier: document-identifier }) FAILURE_DOCUMENT_MISSING))
    )
    ;; Return clinical summary notes
    (ok (get clinical-summary document-details))
  )
)

;; Verify if a specific user has access to a document
(define-public (verify-access-authorization (document-identifier uint) (accessor-identifier principal))
  (let
    (
      (access-details (unwrap! (map-get? access-control { document-identifier: document-identifier, accessor-identifier: accessor-identifier }) FAILURE_ACCESS_RESTRICTION))
    )
    ;; Return access status (enabled/disabled)
    (ok (get access-enabled access-details))
  )
)

;; Update an existing document's details
(define-public (modify-document-details 
  (document-identifier uint)                   ;; Target document identifier
  (updated-subject-identifier (string-ascii 64))    ;; Updated subject information
  (updated-document-bytes uint)                     ;; Updated size measurement
  (updated-clinical-summary (string-ascii 128))     ;; Updated clinical summary
  (updated-categories (list 10 (string-ascii 32)))  ;; Updated categories
)
  (let
    (
      (document-details (unwrap! (map-get? healthcare-documentation { document-identifier: document-identifier }) FAILURE_DOCUMENT_MISSING))
    )
    ;; Validation checks
    (asserts! (document-registered? document-identifier) FAILURE_DOCUMENT_MISSING)
    (asserts! (is-eq (get provider-identifier document-details) tx-sender) FAILURE_CREDENTIALS)
    (asserts! (> (len updated-subject-identifier) u0) FAILURE_TEXT_FORMAT)
    (asserts! (< (len updated-subject-identifier) u65) FAILURE_TEXT_FORMAT)
    (asserts! (> updated-document-bytes u0) FAILURE_DIMENSION_CONSTRAINT)
    (asserts! (< updated-document-bytes u1000000000) FAILURE_DIMENSION_CONSTRAINT)
    (asserts! (> (len updated-clinical-summary) u0) FAILURE_TEXT_FORMAT)
    (asserts! (< (len updated-clinical-summary) u129) FAILURE_TEXT_FORMAT)
    (asserts! (validate-category-list updated-categories) FAILURE_CATEGORY_VALIDATION)

    ;; Update document with new details
    (map-set healthcare-documentation
      { document-identifier: document-identifier }
      (merge document-details { 
        subject-identifier: updated-subject-identifier, 
        document-bytes: updated-document-bytes, 
        clinical-summary: updated-clinical-summary, 
        categories: updated-categories 
      })
    )
    (ok true)  ;; Confirm successful update
  )
)

