library(R6)

setwd("C:/Users/paddy/Git_repos/R-spaceship-tracking")

rescue_list <- list.files(pattern = 'rescue_*')


index <- 0
for (item in rescue_list) {
  print(item)
  cat(paste("source(\"", item, "\")", sep = ""))
  eval(parse(text = paste("source(\"", item, "\")", sep = "")))
  
  rescuer_id <- paste("rescue_", 1, sep = "")
  rescuer_class <- paste(gsub('.R', '', item), "$new(100,100)", sep = "")
  
  cat(rescuer_id, "<-", rescuer_class)
  eval(parse(text = paste(rescuer_id, "<-", rescuer_class)))
  
  
}
