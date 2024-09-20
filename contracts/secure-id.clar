;; Decentralized Social Login Contract

;; Define error messages
(define-constant ERR-USER-EXISTS (err "User already exists"))
(define-constant ERR-USER-NOT-FOUND (err "User not found"))
(define-constant ERR-INVALID-USERNAME (err "Invalid username: must be between 3 and 50 characters"))
(define-constant ERR-INVALID-EMAIL (err "Invalid email: must be between 5 and 100 characters and contain '@' and '.'"))
(define-constant ERR-INVALID-IMAGE-URL (err "Invalid image URL: must be a valid URL string"))

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
    (and (>= length u3) (<= length u50))
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
    (and (>= length u5) (<= length u100) has-at has-dot)
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
    (asserts! (is-none (map-get? users caller)) ERR-USER-EXISTS)
    (asserts! (is-some safe-username) ERR-INVALID-USERNAME)
    (asserts! (is-some safe-email) ERR-INVALID-EMAIL)
    (asserts! (validate-username (unwrap-panic safe-username)) ERR-INVALID-USERNAME)
    (asserts! (validate-email (unwrap-panic safe-email)) ERR-INVALID-EMAIL)
    (map-set users caller
      {
        username: (unwrap-panic safe-username),
        email: (unwrap-panic safe-email),
        profile-image: none
      }
    )
    (var-set user-count (+ (var-get user-count) u1))
    (ok true)
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
    (asserts! (is-some (map-get? users caller)) ERR-USER-NOT-FOUND)
    (asserts! (is-some safe-username) ERR-INVALID-USERNAME)
    (asserts! (is-some safe-email) ERR-INVALID-EMAIL)
    (asserts! (validate-username (unwrap-panic safe-username)) ERR-INVALID-USERNAME)
    (asserts! (validate-email (unwrap-panic safe-email)) ERR-INVALID-EMAIL)
    (map-set users caller
      (merge (unwrap-panic (map-get? users caller))
        {
          username: (unwrap-panic safe-username),
          email: (unwrap-panic safe-email)
        }
      )
    )
    (ok true)
  )
)

;; Function to set profile image
(define-public (set-profile-image (image-url (string-utf8 256)))
  (let
    (
      (caller tx-sender)
      (safe-url (as-max-len? image-url u256))
    )
    (asserts! (is-some (map-get? users caller)) ERR-USER-NOT-FOUND)
    (asserts! (is-some safe-url) ERR-INVALID-IMAGE-URL)
    (map-set users caller
      (merge (unwrap-panic (map-get? users caller))
        { profile-image: safe-url }
      )
    )
    (ok true)
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