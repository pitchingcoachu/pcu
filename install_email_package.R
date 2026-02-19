# Install Required Package for Password Reset
# ============================================
# 
# This script installs the 'blastula' package needed for
# sending password reset emails.

cat("\n=== Installing Password Reset Email Package ===\n\n")

if (!requireNamespace("blastula", quietly = TRUE)) {
  cat("📦 Installing blastula package...\n")
  install.packages("blastula", repos = "https://cloud.r-project.org")
  cat("✅ blastula package installed successfully!\n\n")
} else {
  cat("✅ blastula package is already installed\n\n")
}

cat("Next steps:\n")
cat("1. Configure your email settings (see email_config.R)\n")
cat("2. Test your configuration: source('email_config.R'); test_email_config()\n")
cat("3. Deploy your app and users can use 'Forgot Password' feature!\n\n")
