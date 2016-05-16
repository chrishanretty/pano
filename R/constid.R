#' Get constituency identifier
#'
#' This function converts between four different sets of constituency
#' identifiers.
#' @param sourcevar Vector which contains the codes to be converted
#' @param origin Coding scheme to convert from
#' @param destination Coding scheme to convert to
#' @param warn Flags IDs which return zero or multiple matches
#' @note Supports the following coding schemes: Press Association
#'     reference number ('pa_id'), Hansard ID ('hansard_id'),
#'     ParlParseID ('parlparse_id'), Office of National Statistics ID
#'     ('ons_id'). Will try and handle constituency names ('name').
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
    if (origin == "name") { 
        dict <- na.omit(pano::pano_data[,c('regex', destination)])
    } else {
        dict <- na.omit(pano::pano_data[,c(origin, destination)])
    }
    destination_vector <- rep(NA, length(sourcevar))

    if (origin == "name") {
        # Normalize sourcevar
        sourcevar <- gsub("&","and",
                          tolower(sourcevar),
                          fixed = TRUE)
       	sourcevar <- gsub(",","",sourcevar)
		sourcevar <- gsub(".","",sourcevar, fixed = TRUE)
		sourcevar <- gsub("(","",sourcevar, fixed = TRUE)
		sourcevar <- gsub(")","",sourcevar, fixed = TRUE)
        sourcevar <- gsub(" burgh const| co const| boro const","",sourcevar)

        # For each regex in the database -> find matches
        destination_list <- lapply(sourcevar, function(k) k)
        for (i in 1:nrow(dict)){
            matches <- grep(dict$regex[i],
                            sourcevar,
                            perl=TRUE,
                            value=FALSE)
            destination_vector[matches] <- dict[i, destination]
            # Warning-related
            destination_list[matches] <- lapply(destination_list[matches],
                                                function(k) c(k, dict[i, destination]))
        }
        destination_list <- destination_list[lapply(destination_list, length) > 2]
    } else {
        matches <- match(sourcevar, dict[, origin])
        destination_vector <- dict[matches, destination]
    }
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
