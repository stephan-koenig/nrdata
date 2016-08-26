#' Load a single package or a vector of packages.
#'
#' This function takes a character object or character vector of package names.
#' Any packages that are not installed, are being installed, and any packages
#' that are not loaded are being loaded.
#' @param required_packages A single character obect or character vector, each
#' object a character name.
#'
#' @export

load_packages <- function(required_packages) {
    # Takes character or character vector of required_packages, installs any
    # missing packages and only attaches those that are not yet attached.

    # Check which packages are not installed.
    missing_packages <- !is.element(required_packages, installed.packages()[,1])
    missing_packages <- required_packages[missing_packages]

    # Install missing packages.
    if (length(missing_packages) != 0) {
        install.packages(missing_packages)
        packages <- paste(missing_packages, sep = ", ")
        system_message(paste("Package(s)", packages, "installed."))
    }

    # Check which packages are already loaded.
    unattached_packages <- !is.element(required_packages, (.packages()))
    attached_packages <- required_packages[!unattached_packages]
    unattached_packages <- required_packages[unattached_packages]

    # Attach any unattached packages.
    if (length(unattached_packages != 0)) {
        lapply(unattached_packages, library, character.only = TRUE)
        packages <- paste(unattached_packages, sep = ", ")
        system_message(paste("Package(s)", packages, "attached."))
    }

    if (length(attached_packages != 0)) {
        packages <- paste(attached_packages, sep = ", ")
        system_message(paste("Package(s)", packages, "already attached."))
    }
}
