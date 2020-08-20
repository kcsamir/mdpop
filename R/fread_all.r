#' Read the output from MSDem
#'
#' This function allows you to read all files for a projection
#' @param input.dir as input directory
#' @param output.dir as folder the projections are saved
#' @param model.patt is as in fread.data function of msproj
#' @return R list with five data.table 1) var_def 2) state_space 3) mig_dom 4) mig_int 5) results
#' @keywords read
#' @export
#' @examples
fread_all <- function(input.dir = NULL,output.dir=NULL, model.patt = NULL) {
    input.data.names <- list.files(input.dir, pattern = model.patt)
    input.data.names <- input.data.names[str_replace_all(input.data.names,
                                      c("_mig|_mig_dom|_mig_int|_var_def|_state_space|_axmx|.csv"), "") == model.patt]
    output.data.names <- list.files(output.dir, pattern = "Full")
    all.data.path.names <- c(paste(input.dir, input.data.names, sep = "/"),
                             paste(output.dir, output.data.names, sep = "/"))
    datasets <- lapply(all.data.path.names, function(x)
      #fread from data.table
      fread(x, showProgress = FALSE, stringsAsFactors = FALSE, na.strings = c("NA", "#N/A")))

    names(datasets) = c(gsub(".csv","",gsub(paste(model.patt,"_",sep=""),"",input.data.names)),"results")
    return(datasets)
  }
