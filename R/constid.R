#' Get constituency identifier
#'
#' This function converts between four different sets of constituency
#' identifiers.
#' @param sourcevar Vector which contains the codes to be converted
#' @param origin Coding scheme to convert from
#' @param destination Coding scheme to convert to
#' @param warn Flags IDs which return zero or multiple matches
#' @keywords pano, Press Association, Hansard, ParlParse
#' @note Supports the following coding schemes: Press Association
#'     reference number ('pa_id'), Hansard ID ('hansard_id'),
#'     ParlParseID ('parlpase_id'), Office of National Statistics ID
#'     ('ons_id').
#' @seealso
#'     \url{election.pressassociation.com/Constituencies/general.php},
#'     \url{https://en.wikipedia.org/wiki/ONS_coding_system},
#'     \url{https://github.com/mysociety/parlparse}
#' @export
#' @examples
#' codes.of.origin <- pano::pano_data$pa_id # Vector of values to be converted
#' constid(codes.of.origin, "pa_id", "ons_id")

constid <- function(sourcevar, origin, destination, warn = TRUE){
    codes <- c("hansard_id","parlparse_id","name","ons_id","pa_id")
    if (!origin %in% codes) {
        stop("Invalid origin code supplied")
    }
    if (!destination %in% codes) {
        stop("Invalid destination origin code supplied")
    }
    dict <- na.omit(pano::pano_data[,c(origin, destination)])
    destination_vector <- rep(NA, length(sourcevar))
    matches <- match(sourcevar, dict[, origin])
    destination_vector <- dict[matches, destination]

    # Warnings
    if(warn){
        nomatch <- sort(unique(sourcevar[is.na(destination_vector)]))
        if(length(nomatch) > 0){
            warning("Some values were not matched: ",
                    paste(nomatch, collapse=", "), "\n")
        }
    }
    return(destination_vector)
}
