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

;; Function to register a new user
(define-public (register-user (username (string-ascii 50)) (email (string-ascii 100)))
  (let ((caller tx-sender))
    (if (is-none (map-get? users caller))
      (begin
        (map-set users caller {username: username, email: email, profile-image: none})
        (var-set user-count (+ (var-get user-count) u1))
        (ok true)
      )
      (err u0) ;; User already exists
    )
  )
)

;; Function to update user profile
(define-public (update-profile (new-username (string-ascii 50)) (new-email (string-ascii 100)))
  (let ((caller tx-sender))
    (if (is-some (map-get? users caller))
      (begin
        (map-set users caller 
          (merge (unwrap-panic (map-get? users caller))
                 {username: new-username, email: new-email}))
        (ok true)
      )
      (err u1) ;; User does not exist
    )
  )
)

;; Function to set profile image
(define-public (set-profile-image (image-url (string-utf8 256)))
  (let ((caller tx-sender))
    (if (is-some (map-get? users caller))
      (begin
        (map-set users caller 
          (merge (unwrap-panic (map-get? users caller))
                 {profile-image: (some image-url)}))
        (ok true)
      )
      (err u2) ;; User does not exist
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