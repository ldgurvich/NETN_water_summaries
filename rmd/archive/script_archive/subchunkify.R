# Based on code from http://michaeljw.com/blog/post/subchunkify/

#' Generate a sub-chunk to be interpreted by knitr.  The enclosing chunk
#' must have "results='asis'"
#'
#' @param g The output to chunkify (only tested with figures to date)
#' @param ... Additional named arguments to the chunk
#' @return NULL
#' @details The chunk is automatically output to the console.  There is
#'   no need to print/cat its result.
#' @export
subchunkify <- local({
  chunk_count <- 0
  function(g, ...) {
    chunk_count <<- chunk_count + 1
    g_deparsed <-
      paste0(deparse(
        function() {g}
      ),
      collapse = '')
    args <- list(...)
    args <-
      lapply(names(args),
             FUN=function(nm, arglist) {
               current <- arglist[[nm]]
               if (length(current) > 1) {
                 stop("Only scalars are supported by subchunkify")
               } else if (is.character(current) | is.factor(current)) {
                 current <- as.character(current)
                 ret <- paste0('"', gsub('"', '\"', current, fixed=TRUE), '"')
               } else if (is.numeric(current) | is.logical(current)) {
                 ret <- as.character(current)
               } else {
                 stop("Unhandled class in subchunkify argument handling")
               }
               paste0(nm, "=", ret)
             },
             arglist=args)
    args <- paste0(unlist(args), collapse=", ")
    chunk_header <-
      paste(
        paste0("{r sub_chunk_", chunk_count),
        if (nchar(args) > 0) {
          paste(",", args)
        } else {
          NULL
        },
        ", echo=FALSE}")
    
    sub_chunk <- paste0(
      "\n```",chunk_header, "\n",
      "(", 
      g_deparsed
      , ")()\n",
      "```\n")
    cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
  }
})