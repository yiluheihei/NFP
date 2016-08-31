.onLoad <- function(libname, pkgname) {
  repos = getOption("repos")
  repos["my_repo"] = "https://yiluheihei.github.io/datarepo"
  options(repos = repos)
  invisible(repos)
}