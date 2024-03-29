readFlagDefine <- function(Directory, filename= "data_flag_definitions.txt") {
   
  X <- read.delim(file.path(Directory,filename, fsep =.Platform$file.sep), 
                  comment.char = "%",                    
                  header = FALSE,                 
                  stringsAsFactors= FALSE)
  
  colnames(X)<- c("Flag","Description")
  X$Flag <- as.integer(sub(":","",X$Flag))
  
  return(X) 
}