

#' @export
test_box = function(test) {
  test <- enquo(test)
  print(test)
  
  print(as_label(test))
  
}

test_box(piers)

