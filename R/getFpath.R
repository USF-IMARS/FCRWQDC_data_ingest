getFpath <- function(programName, fpath, formatStr){
  # check for fpath or programName
  if (is.null(fpath)) and is.null(programName)){
    stop("Must provide either fpath or programName")
  } elif (!is.null(fpath)) and !is.null(programName){
    stop("Cannot provide both fpath and programName")
  } elif (!is.null(fpath)){  # programName given
    print('using given fpath')
  } elif (!is.null(programName)){
    print('using given programName')
    fpath <- here(glue(formatStr))
  } else{
    stop('confused')
  }
  return(fpath)
}
  