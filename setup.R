# Setup Script for CSS Course

# Define the list of required packages
required_packages <- c(
    "tidyverse",
    "haven",
    "tidytext",
    "tidymodels",
    "quanteda",
    "quanteda.textmodels",
    "quanteda.textplots",
    "quanteda.textstats",
    "here",
    "needs",
    "janitor",
    "sotu",
    "topicmodels",
    "broom",
    "ldatuning",
    "stm",
    "rvest",
    "stopwords",
    "textrecipes",
    "vip",
    "gghighlight",
    "naivebayes",
    "caret",
    "jsonlite"
)

# Install any missing packages
install_missing_packages <- function(packages) {
    missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
    if (length(missing_packages)) {
        cat(
            "Installing missing packages:\n",
            paste(missing_packages, collapse = ", "),
            "\n"
        )
        
        # Track installation failures
        failed_packages <- c()
        for (pkg in missing_packages) {
            tryCatch({
                install.packages(pkg)
            }, error = function(e) {
                failed_packages <<- c(failed_packages, pkg)
            })
        }
        
        if (length(failed_packages)) {
            cat("\nThe following packages failed to install:\n", 
                paste(failed_packages, collapse = ", "), "\n")
        } else {
            cat("\nAll missing packages installed successfully!\n")
        }
    } else {
        cat("All required packages are already installed!\n")
    }
}

# Invoke the function with the list of required packages
install_missing_packages(required_packages)
