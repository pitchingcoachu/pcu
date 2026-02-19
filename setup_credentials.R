# Setup script for CBU Baseball Dashboard credentials
# Run this once to create or reset the credentials database

library(shinymanager)

# Define initial users
# Using actual admin emails as usernames
initial_users <- data.frame(
  user = c(
    "jgaynor@pitchingcoachu.com",
    "banni17@yahoo.com",
    "micaiahtucker@gmail.com",
    "joshtols21@gmail.com",
    "james.a.gaynor@gmail.com",
    "tblank@mariners.com",
    "admin"              # Keep one generic admin account
  ),
  password = c(
    "cbu2024",           # Change this!
    "cbu2024",           # Change this!
    "cbu2024",           # Change this!
    "cbu2024",           # Change this!
    "cbu2024",           # Change this!
    "cbu2024",           # Change this!
    "admin123"           # Change this!
  ),
  admin = c(
    TRUE,                # jgaynor is admin
    TRUE,                # banni is admin
    TRUE,                # micaiah is admin
    TRUE,                # josh is admin
    TRUE,                # james is admin
    TRUE,                # tblank is admin
    TRUE                 # admin is admin
  ),
  email = c(
    "jgaynor@pitchingcoachu.com",
    "banni17@yahoo.com",
    "micaiahtucker@gmail.com",
    "joshtols21@gmail.com",
    "james.a.gaynor@gmail.com",
    "tblank@mariners.com",
    "admin@cbu.edu"
  ),
  stringsAsFactors = FALSE
)

# Create the database
# IMPORTANT: Keep this passphrase secret and use the same one in app.R!
create_db(
  credentials_data = initial_users,
  sqlite_path = "credentials.sqlite",
  passphrase = "cbu_baseball_2024_secure_passphrase"
)

cat("\n✓ Credentials database created successfully!\n")
cat("✓ File location: credentials.sqlite\n")
cat("\n📋 Admin credentials (use emails as usernames):\n")
cat("   Username: jgaynor@pitchingcoachu.com | Password: cbu2024 | Role: Admin\n")
cat("   Username: banni17@yahoo.com          | Password: cbu2024 | Role: Admin\n")
cat("   Username: micaiahtucker@gmail.com    | Password: cbu2024 | Role: Admin\n")
cat("   Username: joshtols21@gmail.com       | Password: cbu2024 | Role: Admin\n")
cat("   Username: james.a.gaynor@gmail.com   | Password: cbu2024 | Role: Admin\n")
cat("   Username: tblank@mariners.com        | Password: cbu2024 | Role: Admin\n")
cat("   Username: admin                      | Password: admin123 | Role: Admin (fallback)\n")
cat("\n⚠️  IMPORTANT SECURITY NOTES:\n")
cat("   1. Change ALL default passwords after first login!\n")
cat("   2. Keep credentials.sqlite file secure\n")
cat("   3. This file will be deployed with your app to shinyapps.io\n")
cat("   4. Admins can add/manage users through the app's admin panel\n")
cat("\n🔑 To add more users:\n")
cat("   - Edit this script and run it again, OR\n")
cat("   - Log in as admin and use the admin panel in the app\n")
cat("\n✅ You're all set! Run the app with: shiny::runApp()\n\n")
