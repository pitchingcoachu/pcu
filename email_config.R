# Email Configuration for Password Reset
# ======================================
# 
# This file configures email settings for the "Forgot Password" functionality.
# The app uses the 'blastula' package to send password reset emails.
#
# SETUP INSTRUCTIONS:
# -------------------
# You have two options for email configuration:
#
# OPTION 1: Use Gmail (Recommended for testing)
# ----------------------------------------------
# 1. Enable 2-factor authentication on your Gmail account
# 2. Generate an App Password:
#    - Go to: https://myaccount.google.com/apppasswords
#    - Select "Mail" and "Other (Custom name)"
#    - Name it "CBU Baseball App"
#    - Copy the 16-character password
# 3. Set environment variables in your .Renviron file:
#
#    MAIL_SERVER = "smtp.gmail.com"
#    MAIL_PORT = 587
#    MAIL_USERNAME = "your-email@gmail.com"
#    MAIL_PASSWORD = "your-app-password"
#    MAIL_FROM = "your-email@gmail.com"
#
# OPTION 2: Use a Custom SMTP Server
# -----------------------------------
# Set these environment variables in your .Renviron file:
#
#    MAIL_SERVER = "smtp.yourserver.com"
#    MAIL_PORT = 587
#    MAIL_USERNAME = "your-username"
#    MAIL_PASSWORD = "your-password"
#    MAIL_FROM = "noreply@yourdomain.com"
#
# OPTION 3: Use shinyapps.io Environment Variables
# -------------------------------------------------
# When deploying to shinyapps.io, set environment variables in the dashboard:
#   1. Go to your app settings on shinyapps.io
#   2. Click "Vars" tab
#   3. Add the MAIL_* variables listed above
#
# TO EDIT .Renviron FILE:
# -----------------------
# Run this command in R:
#   usethis::edit_r_environ()
# 
# Add your email configuration variables, save, and restart R.
#
# SECURITY NOTE:
# --------------
# NEVER commit your .Renviron file or actual credentials to version control!
# The .Renviron file should be in your .gitignore.

# Function to test email configuration
test_email_config <- function() {
  required_vars <- c("MAIL_SERVER", "MAIL_PORT", "MAIL_USERNAME", "MAIL_PASSWORD", "MAIL_FROM")
  
  cat("\n=== Email Configuration Check ===\n\n")
  
  all_set <- TRUE
  for (var in required_vars) {
    value <- Sys.getenv(var, "")
    if (value == "") {
      cat("❌", var, "is NOT set\n")
      all_set <- FALSE
    } else {
      # Mask password for security
      if (var == "MAIL_PASSWORD") {
        cat("✓", var, "is set (", paste0(rep("*", 8), collapse = ""), "...)\n")
      } else {
        cat("✓", var, "is set:", value, "\n")
      }
    }
  }
  
  cat("\n")
  
  if (all_set) {
    cat("✅ All email configuration variables are set!\n")
    cat("📧 Password reset emails will be sent from:", Sys.getenv("MAIL_FROM"), "\n\n")
    
    # Try to load blastula package
    if (requireNamespace("blastula", quietly = TRUE)) {
      cat("✅ blastula package is installed\n\n")
      cat("You can now use the 'Forgot Password' feature!\n")
    } else {
      cat("⚠️  blastula package is NOT installed\n")
      cat("   Install it with: install.packages('blastula')\n\n")
    }
  } else {
    cat("❌ Email configuration is incomplete\n")
    cat("   Please set the missing environment variables\n")
    cat("   Run: usethis::edit_r_environ()\n\n")
  }
  
  invisible(all_set)
}

# Uncomment to test your configuration:
# test_email_config()
