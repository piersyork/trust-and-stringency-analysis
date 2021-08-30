
box::use(magrittr[use_series],
         dplyr[...])

today <- Sys.Date()
done_by <- as.Date("2021-08-24")
days_to_go <- as.numeric(done_by - today)

word_count <- function(con) {
  
  split_con <- stringr::str_split(con, "")[[1]]
  
  if (!split_con[length(split_con)] == "/") {
    all_words <- qdapTools::read_docx(con) %>% 
      paste(collapse = "") %>% 
      # stringr::str_remove_all("\\sCITATION[\\s](\\d|[a-zA-Z]){1,10}\\s\\\\[a-z]\\s\\d{4}\\s?") %>% print() %>% 
      stringr::str_split(" ") %>% 
      unlist() %>% 
      stringr::str_subset("")
    
    num_citations <- stringr::str_detect(all_words, "CITATION") %>% 
      sum(na.rm = TRUE)
    
    return(length(all_words) - (num_citations*4))
    
  } else {
    files <- list.files(con) %>% 
      stringr::str_subset(".docx", negate = FALSE) %>% 
      stringr::str_subset("~", negate = TRUE)
    n_words <- 0
    for (i in 1:length(files)) {
      n_words <- n_words + word_count(paste0(con, files[i]))
    }
    return(n_words)
  }
}
total_words <- word_count("../Write Up/Introduction.docx") +
  word_count(con = "../Write Up/Literature Review.docx") +
  word_count("../Write Up/Research Design.docx") +
  word_count("../Write Up/Results.docx") +
  word_count("../Write Up/Limitations.docx") +
  word_count("../Write Up/Conclusion.docx")
total_words <- word_count("../Write Up/")

word_count("../Write Up/")




words_per_day <- ( 8000 - word_count("../Write Up/")) / days_to_go # words needed per day



words_per_day

(1161 + 685 + 506 + 2129)  # 17/08

(450 + 685 + 1359 + 2146 + 406) # 18/08

# 5980 - 21/08


intro_raw <- officer::read_docx("../Write Up/Introduction.docx")
intro_text <- officer::docx_summary(intro_raw)$text

package_dir <- tempfile()
unpack_folder(file = "../Write Up/Introduction.docx", folder = package_dir)

intro_raw$doc_obj$

officer::docx_(intro_raw) 

intro_sum$text

qdapTools::read_docx("../Write Up/Research Design.docx") %>% 
  stringr::str_remove_all("\\sCITATION[\\s](\\d|[a-zA-Z]){1,10}") %>% 
  stringr::str_remove_all("\\s\\\\[a-z]\\s(\\d|[a-zA-Z]){1,10}") #\\s\\d{4}\\s?





intro$default_styles$numbering


ran_data <- owidR::owid()



