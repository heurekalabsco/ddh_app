download_methods <- function(){
  s3 <- paws::s3()
  methods_file <- "methods.zip"
  methods_path <- here::here("code", "app", "www")
  methods_key <- here::here(methods_path, methods_file)
  s3_download <- 
    s3$get_object(
      Bucket = Sys.getenv("AWS_DATA_BUCKET_ID"),
      Key = methods_file
    )
  writeBin(s3_download$Body, con = methods_key)
  unzip(zipfile = methods_key, 
        exdir = methods_path, 
        overwrite = TRUE)
  on.exit(unlink(methods_key))
  message("Downloaded and unzipped methods")
}
try(download_methods())
