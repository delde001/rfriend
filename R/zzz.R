.onAttach <- function(libname, pkgname) {
  # # Get the package version
  # pkg_version <- utils::packageVersion(pkgname)
  #
  # # Print the library path, package name, and version
  # packageStartupMessage("Package '", pkgname, "' version ", pkg_version, " loaded from ", libname)

  # Check R version compatibility (assuming the package requires R >= 4.0.0)
  required_R_version <- "4.0.0"
  current_R_version <- getRversion()

  if (current_R_version < as.package_version(required_R_version)) {
    warning("This package was built for R version ", required_R_version,
            ", but you are running R version ", current_R_version, ".")
  }

  # # Check dependent packages (example: package "rstudioapi" and "ggplot2" are required)
  # required_deps <- c("rmarkdown",
  #                    "rstudioapi",
  #                    "knitr",
  #                    "pander",
  #                    "writexl",
  #                    "nortest",
  #                    "multcomp",
  #                    "emmeans",
  #                    "ggplot2",
  #                    "rstatix",
  #                    "grDevices"
  #                    )  # List of required packages
  # for (dep in required_deps) {
  #   if (!requireNamespace(dep, quietly = TRUE)) {
  #     warning("The required package '", dep, "' is not installed.")
  #
  #     # Offer to install the missing package
  #     install_choice <- readline(prompt = paste("Would you like to install the missing package '", dep, "'? (yes/no): ", sep = ""))
  #     if (tolower(install_choice) == "yes") {
  #       install.packages(dep)
  #       message("Package '", dep, "' has been installed.")
  #
  #       # Load the installed package
  #       library(dep, character.only = TRUE)
  #       message("Package '", dep, "' has been loaded.")
  #     } else {
  #       message("Package '", dep, "' was not installed. You may experience issues if the package is required.")
  #     }
  #   } else {
  #     # Package is installed, so load it
  #     library(dep, character.only = TRUE)
  #     #message("Package '", dep, "' is already installed and has been loaded.")
  #
  #     # Check if the version is compatible
  #     dep_version <- packageVersion(dep)
  #     required_version <- "0.1.0"
  #
  #     if (dep_version < as.package_version(required_version)) {
  #       warning("Package '", dep, "' is installed but its version (", dep_version,
  #               ") is older than the required version (", required_version, ").")
  #
  #       # Offer to update the package
  #       update_choice <- readline(prompt = paste("Would you like to update '", dep, "' to the latest version? (yes/no): ", sep = ""))
  #       if (tolower(update_choice) == "yes") {
  #         install.packages(dep)
  #         message("Package '", dep, "' has been updated.")
  #
  #         # Load the updated package
  #         library(dep, character.only = TRUE)
  #         message("Package '", dep, "' has been loaded.")
  #       } else {
  #         message("Package '", dep, "' was not updated. You may experience issues if the version is outdated.")
  #       }
  #     }
  #   }
  # }
}



.onUnload <- function(libpath) {
  # Print the library path from where the package is being unloaded
  message("Package unloaded from: ", libpath)
}
