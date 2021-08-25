box::use(officer[read_docx, body_add_docx])
read_docx("../Write Up/Introduction.docx") %>% 
  body_add_docx(src = "../Write Up/Literature Review.docx") %>% 
  body_add_docx(src = "../Write Up/Research Design.docx") %>% 
  body_add_docx(src = "../Write Up/Results.docx") %>% 
  body_add_docx(src = "../Write Up/Limitations.docx") %>% 
  body_add_docx(src = "../Write Up/Conclusion.docx") %>% 
  print(target = "../combine_docs.docx")

xml2::read_xml("../Write Up/Introduction.docx")

intro_raw$doc_obj$get()
xml2::xml_child(intro_raw$doc_obj$get(), "w:body")
library(xml2)

intro_raw$doc_obj$get() %>% 
  xml_ns()
