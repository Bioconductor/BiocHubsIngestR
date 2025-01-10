#' Set AWS credentials for Bioconductor Hubs Ingest endpoint
#'
#' @param username Character string of the username provided by administrator
#' @param password Character string of the password/key provided by administrator
#' @param endpoint Optional custom endpoint URL. If NULL, constructs from username
#'
#' @return Invisible NULL, sets environment variables as side effect
#' @export
#'
#' @examples
#' \dontrun{
#' BiocHubsIngestR::auth("myusername", "mypassword")
#' }
auth <- function(username, password, endpoint = NULL) {
  if (!is.character(username) || !is.character(password))
    stop("Username and password must be character strings")

  if (is.null(endpoint)) {
    endpoint <- sprintf("https://%s.hubsingest.bioconductor.org", username)
  }

  Sys.setenv(
    AWS_ACCESS_KEY_ID = username,
    AWS_SECRET_ACCESS_KEY = password,
    AWS_DEFAULT_REGION = "us-east-1",
    AWS_S3_ENDPOINT = endpoint
  )

  invisible(NULL)
}
