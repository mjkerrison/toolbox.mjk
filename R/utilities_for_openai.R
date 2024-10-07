
# usethis::edit_r_environ()

OAI_API_KEY <- Sys.getenv("OAI_API_KEY")

assertthat::assert_that(nzchar(OAI_API_KEY), msg = "Sys env not set!")

openai::create_chat_completion(...)
