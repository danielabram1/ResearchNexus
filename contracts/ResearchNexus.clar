;; ResearchNexus: Decentralized academic research collaboration platform
;; Enables researchers to propose studies, peers to contribute, and institutions to validate findings

(define-data-var lead-institution principal tx-sender)

(define-map research-registry
  { study-id: uint }
  {
    principal-investigator: principal,
    funding-required: uint,
    research-title: (string-ascii 50),
    methodology: (string-ascii 500),
    timeline-months: uint,
    peer-reviewed: bool
  }
)

(define-map collaboration-records
  { study-id: uint, record-id: uint }
  {
    collaborator: principal,
    contribution-date: uint,
    role: (string-ascii 20)
  }
)

(define-data-var next-study-id uint u1)

(define-map record-tracker
  { study-id: uint }
  { total-records: uint }
)

;; Propose new research study
(define-public (propose-study (title-input (string-ascii 50)) (method-input (string-ascii 500)) (timeline-input uint) (funding-input uint))
  (let
    (
      (study-id (var-get next-study-id))
      (record-id u0)
      (title title-input)
      (method method-input)
      (timeline timeline-input)
      (funding funding-input)
    )
    ;; Input validation
    (asserts! (> funding u0) (err u1))
    (asserts! (> (len title) u0) (err u5))
    (asserts! (> (len method) u0) (err u6))
    (asserts! (> timeline u0) (err u7))
    
    (map-set research-registry
      { study-id: study-id }
      {
        principal-investigator: tx-sender,
        funding-required: funding,
        research-title: title,
        methodology: method,
        timeline-months: timeline,
        peer-reviewed: false
      }
    )
    (map-set collaboration-records
      { study-id: study-id, record-id: record-id }
      {
        collaborator: tx-sender,
        contribution-date: study-id,
        role: "lead-researcher"
      }
    )
    (map-set record-tracker
      { study-id: study-id }
      { total-records: u1 }
    )
    (var-set next-study-id (+ study-id u1))
    (ok study-id)
  )
)

;; Join research collaboration
(define-public (join-research (study-id-input uint))
  (let
    (
      (study-id study-id-input)
      (study-info (unwrap! (map-get? research-registry { study-id: study-id }) (err u2)))
      (funding (get funding-required study-info))
      (investigator (get principal-investigator study-info))
      (record-data (default-to { total-records: u0 } (map-get? record-tracker { study-id: study-id })))
      (record-id (get total-records record-data))
      (new-record-id (+ record-id u1))
    )
    ;; Input validation
    (asserts! (> study-id u0) (err u8))
    (asserts! (not (is-eq tx-sender investigator)) (err u3))
    
    (try! (stx-transfer? funding tx-sender investigator))
    (map-set collaboration-records
      { study-id: study-id, record-id: record-id }
      {
        collaborator: tx-sender,
        contribution-date: (var-get next-study-id),
        role: "contributor"
      }
    )
    (map-set record-tracker
      { study-id: study-id }
      { total-records: new-record-id }
    )
    (ok true)
  )
)

;; Validate research findings (institution only)
(define-public (validate-research (study-id-input uint))
  (let
    (
      (study-id study-id-input)
      (study-info (unwrap! (map-get? research-registry { study-id: study-id }) (err u2)))
      (record-data (default-to { total-records: u0 } (map-get? record-tracker { study-id: study-id })))
      (record-id (get total-records record-data))
      (new-record-id (+ record-id u1))
    )
    ;; Input validation
    (asserts! (> study-id u0) (err u8))
    (asserts! (is-eq tx-sender (var-get lead-institution)) (err u4))
    
    (map-set research-registry
      { study-id: study-id }
      (merge study-info { peer-reviewed: true })
    )
    (map-set collaboration-records
      { study-id: study-id, record-id: record-id }
      {
        collaborator: (get principal-investigator study-info),
        contribution-date: (var-get next-study-id),
        role: "peer-reviewed"
      }
    )
    (map-set record-tracker
      { study-id: study-id }
      { total-records: new-record-id }
    )
    (ok true)
  )
)

;; Get research study details
(define-read-only (get-study (study-id uint))
  (map-get? research-registry { study-id: study-id })
)

;; Get collaboration record
(define-read-only (get-collaboration-record (study-id uint) (record-id uint))
  (map-get? collaboration-records { study-id: study-id, record-id: record-id })
)

;; Get total collaboration records
(define-read-only (get-collaboration-count (study-id uint))
  (let
    (
      (record-data (default-to { total-records: u0 } (map-get? record-tracker { study-id: study-id })))
    )
    (get total-records record-data)
  )
)