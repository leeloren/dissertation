library(googledrive)
library(rvest)
library(rmarkdown)

# Load environment variables from .env file
# Install dotenv if not already installed: install.packages("dotenv")
if (!require(dotenv)) {
  install.packages("dotenv")
}
library(dotenv)

# Load environment variables
load_dot_env()

# Get sensitive information from environment variables
doc_id <- Sys.getenv("GOOGLE_DOC_ID")
service_account_path <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_PATH")
doc_file <- "temp_files/dissertation.html"

# Check if required environment variables are set
if (doc_id == "") {
  stop("GOOGLE_DOC_ID environment variable not set. Please check your .env file.")
}

if (service_account_path == "") {
  stop("GOOGLE_SERVICE_ACCOUNT_PATH environment variable not set. Please check your .env file.")
}

# Authenticate using service account credentials
options(gargle_oauth_email = FALSE)
drive_auth(path = service_account_path)

# Get document metadata
doc <- drive_get(id = doc_id)

# Download as HTML
drive_download(
  file = doc,
  path = doc_file,
  type = "text/html",
  overwrite = TRUE
)

# Note: To set up service account authentication:
# 1. Go to Google Cloud Console
# 2. Create a new project or select existing one
# 3. Enable the Google Drive API
# 4. Create a service account
# 5. Download the JSON key file
# 6. Share your Google Doc with the service account email
# 7. Create a .env file with your sensitive information


