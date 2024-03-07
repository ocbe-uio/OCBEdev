# Functions to add, update or remove a feature version number in an R package.
# Currently, a feature version is defined by adding a dash after the build
# version and then the epoch at the time the command was ran. In other words,
# a package numbered 0.0.0.9000 on the develop branch would become something
# like 0.0.0.9000-1709728345 on the feature branch. This should minimize
# version number confusion and make it easier to have multiple features
# concurrently, none of which interfere with the build number on develop (9000).
# There is, however, a slight risk of collision if two features are started
# within the same second. This is considered an acceptable risk.

#' @title Add a feature version number to an R package
#' @description Adds a feature version number to an R package, namely by adding a
#' dash and the Unix epoch at the time the command was ran to the build version.
#' @param pkg_path The path to the package. Defaults to the current working
#' @return An updated package DESCRIPTION, with the feature version number added.
#' @author Waldir Leoncio
#' @export
add_feature_version <- function(pkg_path = ".") {
  # Load the DESCRIPTION file
  desc_path <- file.path(pkg_path, "DESCRIPTION")
  desc <- readLines(desc_path)

  # Find the version number
  version_line <- grep("^Version:", desc)
  version <- desc[version_line]
  version <- gsub("Version: ", "", version)

  # Check if there is already a feature version number
  if (grepl("-", version)) {
    # Remove version number
    remove_feature_version(pkg_path)
    add_feature_version(pkg_path)
  } else {
    # Add the feature version number
    feature_version <- paste0(version, "-", format(Sys.time(), format = "%s"))
    desc[version_line] <- paste0("Version: ", feature_version)

    # Write the updated DESCRIPTION file
    writeLines(desc, desc_path)
  }
}

#' @title Removes the feature version number from an R package
#' @description Reverts the work done by [add_feature_version()] by removing the
#' feature version number from the package's DESCRIPTION file.
#' @param pkg_path The path to the package. Defaults to the current working
#' @return An updated package DESCRIPTION, with the feature version number removed.
#' @author Waldir Leoncio
#' @export
remove_feature_version <- function(pkg_path = ".") {
  # Load the DESCRIPTION file
  desc_path <- file.path(pkg_path, "DESCRIPTION")
  desc <- readLines(desc_path)

  # Find the version number
  version_line <- grep("^Version:", desc)
  version <- desc[version_line]
  version <- gsub("Version: ", "", version)

  # Remove the feature version number
  version <- gsub("-.*$", "", version)
  desc[version_line] <- paste0("Version: ", version)

  # Write the updated DESCRIPTION file
  writeLines(desc, desc_path)
}
