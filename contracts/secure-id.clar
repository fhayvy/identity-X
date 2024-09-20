;; Decentralized Social Login Contract

;; Define the data map for storing user information
(define-map users principal
  {
    username: (string-ascii 50),
    email: (string-ascii 100),
    profile-image: (optional (string-utf8 256))
  }
)

;; Define a data var to keep track of the number of registered users
(define-data-var user-count uint u0)

;; Function to validate username
(define-private (validate-username (username (string-ascii 50)))
  (let 
    (
      (length (len username))
    )
    (and 
      (>= length u3) 
      (<= length u50)
    )
  )
)

;; Function to validate email
(define-private (validate-email (email (string-ascii 100)))
  (let
    (
      (length (len email))
      (has-at (is-some (index-of email "@")))
      (has-dot (is-some (index-of email ".")))
    )
    (and 
      (>= length u5) 
      (<= length u100)
      has-at
      has-dot
    )
  )
)

;; Function to register a new user
(define-public (register-user (username (string-ascii 50)) (email (string-ascii 100)))
  (let 
    (
      (caller tx-sender)
      (safe-username (as-max-len? username u50))
      (safe-email (as-max-len? email u100))
    )
    (if (and 
          (is-some safe-username)
          (is-some safe-email)
          (validate-username (unwrap-panic safe-username))
          (validate-email (unwrap-panic safe-email)))
      (if (is-none (map-get? users caller))
        (begin
          (map-set users caller {
            username: (unwrap-panic safe-username), 
            email: (unwrap-panic safe-email), 
            profile-image: none
          })
          (var-set user-count (+ (var-get user-count) u1))
          (ok true)
        )
        (err u0)) ;; User already exists
      (err u3) ;; Invalid username or email
    )
  )
)

;; Function to update user profile
(define-public (update-profile (new-username (string-ascii 50)) (new-email (string-ascii 100)))
  (let 
    (
      (caller tx-sender)
      (safe-username (as-max-len? new-username u50))
      (safe-email (as-max-len? new-email u100))
    )
    (if (and 
          (is-some safe-username)
          (is-some safe-email)
          (validate-username (unwrap-panic safe-username))
          (validate-email (unwrap-panic safe-email)))
      (if (is-some (map-get? users caller))
        (begin
          (map-set users caller 
            (merge (unwrap-panic (map-get? users caller))
                   {username: (unwrap-panic safe-username), email: (unwrap-panic safe-email)}))
          (ok true)
        )
        (err u1)) ;; User does not exist
      (err u3) ;; Invalid username or email
    )
  )
)

;; Function to set profile image
(define-public (set-profile-image (image-url (string-utf8 256)))
  (let 
    (
      (caller tx-sender)
      (safe-url (as-max-len? image-url u256))
    )
    (match safe-url
      url (if (is-some (map-get? users caller))
          (begin
            (map-set users caller 
              (merge (unwrap-panic (map-get? users caller))
                     {profile-image: (some url)}))
            (ok true)
          )
          (err u2)) ;; User does not exist
      (err u4) ;; Invalid URL
    )
  )
)

;; Read-only function to get user information
(define-read-only (get-user-info (user principal))
  (map-get? users user)
)

;; Read-only function to get the total number of registered users
(define-read-only (get-user-count)
  (var-get user-count)
)

;; Function to check if a user is registered
(define-read-only (is-user-registered (user principal))
  (is-some (map-get? users user))
)