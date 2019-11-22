#' Retrieve a Java instance of TrapGrid.
#'
#' Retrieve a Java instance of SpellCorrector, with the training file
#' specified. Language model is trained before the instance is returned.
#' The spell corrector is adapted from Peter Norvig's demonstration.
#'
#' @param filepath Path of the corpus.
#' @return a Java instance of SpellCorrector
#' @export
trapgridR<-function(filepath='foogrid') {
  .jaddLibrary('trapgrid', 'inst/java/TrapGrid.jar')
  .jaddClassPath('inst/java/TrapGrid.jar')
  trapgrid<- .jnew('com.reallymany.trapgrid.Driver')
  foogrid<- .jnew('java/io/File', filepath)
  .jcall(obj=trapgrid, method="main", c("-tg",foogrid), returnSig = "V")
  return(trapgrid)
}
