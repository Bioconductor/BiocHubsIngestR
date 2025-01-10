test_that("auth sets environment variables correctly", {
  old_env <- Sys.getenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", 
                         "AWS_DEFAULT_REGION", "AWS_S3_ENDPOINT"))
  on.exit(do.call(Sys.setenv, as.list(old_env)))

  username <- "testuser"
  password <- "testpass"
  auth(username, password)

  expect_equal(Sys.getenv("AWS_ACCESS_KEY_ID"), username)
  expect_equal(Sys.getenv("AWS_SECRET_ACCESS_KEY"), password)
  expect_equal(Sys.getenv("AWS_DEFAULT_REGION"), "")
  expect_equal(
    Sys.getenv("AWS_S3_ENDPOINT"),
    "https://testuser.hubsingest.bioconductor.org"
  )
})

test_that("auth validates inputs", {
  expect_error(auth(123, "pass"))
  expect_error(auth("user", 123))
})
