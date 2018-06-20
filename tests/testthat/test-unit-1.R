context("test-unit-1.R")

test_that("Connection to the provided triple-store works.", {
  graphdb = rdf4r::basic_triplestore_access(
    server_url = "http://graph.openbiodiv.net:7777",
    user = "dbuser",
    password = "public-access",
    repository = "obkms_i6"
  )
  expect_match(class(graphdb), "triplestore_access_options", all = FALSE)
})
