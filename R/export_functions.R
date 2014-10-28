#' Exports R objects to a .xlsx file.  
#' 
#' This function is wrapper function by Rob Kabacoff. 
#' See: http://bit.ly/1wbMCe8.
#' It will save as many objects as you need to new sheets in Excel
#' 
#' @param file Filename and necessary path to place new file.
#' @param ... R objects to write to file (each to a new sheet).
#' @keywords export, Excel
#' @examples
#' save.xlsx("example.xlsx", mtcars)


save.xlsx <- function (file, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i])
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE)
  }
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
}

