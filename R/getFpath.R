getFpath <- function(programName, fpath, formatStr){
  # check for fpath or programName
  if (is.null(fpath) && is.null(programName)){
    stop("Must provide either fpath or programName")
  } else if (!is.null(fpath) && !is.null(programName)){
    stop("Cannot provide both fpath and programName")
  } else if (!is.null(fpath)){  # programName given
    print('using given fpath')
  } else if (!is.null(programName)){
    print('using given programName')
    fpath <- here(glue(formatStr))
  } else {
    stop('confused')
  }
  return(fpath)
}
  