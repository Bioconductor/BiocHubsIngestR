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
  # Hardcoded part size: 1GB
  MULTIPART_THRESHOLD <- 1024 * 1024 * 1024  # 1GB
  PART_SIZE <- 1024 * 1024 * 1024  # 1GB
  
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

  is_dir <- file.info(path)$isdir
  base_dir <- if(is_dir) basename(path) else NULL
  files <- if(is_dir) list.files(path, recursive = TRUE, full.names = TRUE) else path

  uploaded <- lapply(files, function(f) {
    rel_path <- if(is_dir) {
      file.path(base_dir, sub(paste0("^", path, "/?"), "", f))
    } else {
      basename(f)
    }
    
    file_size <- file.size(f)
    
    if (file_size < MULTIPART_THRESHOLD) {
      # Small file, upload directly
      message("Uploading: ", rel_path)
      s3_client$put_object(
        Body = f,
        Bucket = bucket,
        Key = rel_path
      )
    } else {
      # Large file, use multipart upload
      message("Uploading large file: ", rel_path, " (", round(file_size / (1024 * 1024 * 1024), 2), " GB)")
      multipart_upload(s3_client, f, bucket, rel_path, PART_SIZE)
    }
    
    return(rel_path)
  })

  invisible(uploaded)
}

# Internal function for multipart upload
multipart_upload <- function(client, file, bucket, key, part_size) {
  multipart <- client$create_multipart_upload(
    Bucket = bucket,
    Key = key
  )
  
  resp <- NULL
  on.exit({
    if (is.null(resp) || inherits(resp, "try-error")) {
      message("Aborting multipart upload due to error")
      client$abort_multipart_upload(
        Bucket = bucket,
        Key = key,
        UploadId = multipart$UploadId
      )
    }
  })
  
  resp <- try({
    parts <- upload_multipart_parts(client, file, bucket, key, multipart$UploadId, part_size)
    client$complete_multipart_upload(
      Bucket = bucket,
      Key = key,
      MultipartUpload = list(Parts = parts),
      UploadId = multipart$UploadId
    )
  })
  
  return(resp)
}

# Internal function to upload individual parts
upload_multipart_parts <- function(client, file, bucket, key, upload_id, part_size) {
  file_size <- file.size(file)
  num_parts <- ceiling(file_size / part_size)
  
  con <- base::file(file, open = "rb")
  on.exit({
    close(con)
  })
  
  message("Uploading in ", num_parts, " parts")
  pb <- utils::txtProgressBar(min = 0, max = num_parts, style = 3)
  parts <- list()
  
  for (i in 1:num_parts) {
    # For the last part, read the remaining bytes
    bytes_to_read <- if (i == num_parts) {
      file_size - (i - 1) * part_size
    } else {
      part_size
    }
    
    part <- readBin(con, what = "raw", n = bytes_to_read)
    part_resp <- client$upload_part(
      Body = part,
      Bucket = bucket,
      Key = key,
      PartNumber = i,
      UploadId = upload_id
    )
    
    parts <- c(parts, list(list(ETag = part_resp$ETag, PartNumber = i)))
    utils::setTxtProgressBar(pb, i)
  }
  
  close(pb)
  message("\nMultipart upload completed successfully")
  return(parts)
}
