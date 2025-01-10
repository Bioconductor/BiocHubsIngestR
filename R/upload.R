#' Upload local directory to Bioconductor Hubs Ingest S3 endpoint
#'
#' Uses paws S3 client to upload files to the configured endpoint. Requires prior
#' authentication using the auth() function to set up the correct endpoint and
#' credentials.
#'
#' @param path Character string path to local directory or file to upload
#' @param bucket Character string of bucket name, defaults to "userdata"
#'
#' @return Invisible list of uploaded files
#' @export
#'
#' @examples
#' \dontrun{
#' BiocHubsIngestR::upload("path/to/data")
#' }
upload <- function(path, bucket = "userdata") {
  if (!file.exists(path))
    stop("Path does not exist: ", path)

  s3_client <- paws::s3(region = "", endpoint = Sys.getenv("AWS_S3_ENDPOINT"))

  # Check if bucket exists
  tryCatch({
    s3_client$head_bucket(Bucket = bucket)
  }, error = function(e) {
    message("Creating bucket: ", bucket)
    s3_client$create_bucket(Bucket = bucket)
  })

  if (file.info(path)$isdir) {
    files <- list.files(path, recursive = TRUE, full.names = TRUE)
  } else {
    files <- path
  }

  uploaded <- lapply(files, function(f) {
    rel_path <- if(file.info(path)$isdir) {
      sub(paste0("^", path, "/?"), "", f)
    } else {
      basename(f)
    }
    message("Uploading: ", rel_path)

    s3_client$put_object(
      Body = readBin(f, "raw", file.info(f)$size),
      Bucket = bucket,
      Key = rel_path
    )
    return(rel_path)
  })

  invisible(uploaded)
}
