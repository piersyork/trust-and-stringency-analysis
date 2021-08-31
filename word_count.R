library(dplyr)

pdf <- pdftools::pdf_text("Final Output/write_up_test.pdf")

words <- pdf %>% 
  paste(collapse = " ") %>% 
  stringr::str_split("\n") %>%
  unlist() %>% 
  stringr::str_split(" ") %>% 
  unlist() %>% 
  stringr::str_remove_all("\\.") %>% 
  stringr::str_subset("", negate = FALSE) %>% 
  stringr::str_to_lower()

length(words) - 33


wordcloud::wordcloud(words)
table(words) %>% 
  as_tibble() %>% 
  arrange(desc(n)) %>% 
  print(n = 100)





library(blastula)
# Get a nicely formatted date/time string
date_time <- add_readable_time()

# Create an image string using an on-disk
# image file
img_file_path <-
  system.file(
    "img", "pexels-photo-267151.jpeg",
    package = "blastula"
  )

img_string <- add_image(file = img_file_path)


email <-
  compose_email(
    body = md(glue::glue(
      "Hello,

This is a *great* picture I found when looking
for sun + cloud photos:

{img_string}
")),
footer = md(glue::glue("Email sent on {date_time}."))
  )
email

get_html_str(email)














