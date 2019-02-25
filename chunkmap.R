chunkmap <- function(){
  
  lines <- readLines(rstudioapi::getActiveDocumentContext()$path)  # here I change a bit - it now reads the current active document
  
  # find which lines are chunk starts
  chunk_header_indices <- which(grepl("^```\\{[a-zA-Z0-9]", lines))
  
  # null if no chunks
  if(length(chunk_header_indices) == 0){
    return(NULL)
  }
  
  # fill a datatable with chunk number, identifying info in between "{r" and either "," or "}", and the linenumber of chunk header
chunkMap <- data.table(
  'chunk #' = seq(1:length(chunk_header_indices)), 
  name =   sub('(.*\\{r )(.*?)([\\}|,].*)', "\\2", lines[chunk_header_indices]),
  startline = chunk_header_indices)
  
  return(chunkMap)
  
}
