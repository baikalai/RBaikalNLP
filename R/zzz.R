.onAttach <- function(libname, pkgName) {
    if (interactive()) {
        packageStartupMessage("baikalnlp ", packageVersion("baikalnlp"),
                              " using baikalNLP 1.7.")
    }
}