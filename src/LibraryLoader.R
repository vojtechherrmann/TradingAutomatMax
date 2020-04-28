loadListOfLibraries <- function(requirements_path) {
  return(
    read.delim(
      file.path(requirements_path), 
      header = FALSE,
      stringsAsFactors = FALSE,
      sep = "\n"
    )[,"V1"]
  )
}

loadOrInstallLibraries <- function(requirements_path, first_time = FALSE) {
  
  list.of.packages = loadListOfLibraries(requirements_path)
  
  list.of.installed.packages <- rownames(installed.packages())
  for (package in list.of.packages) {
    if (!(package %in% list.of.installed.packages)) {
      tryCatch(
        {
          install.packages(package)
          eval(bquote(library(.(package))))        
        }, 
        error=function(e){}
      )
    } else {
      if (first_time) {
        eval(bquote(library(.(package))))
      } else {
        suppressPackageStartupMessages(eval(bquote(library(.(package)))))
      }
    }
  }
  
  # doubled because of safety reasons:
  # after installation sourcing is rarely successful
  if (first_time) {
    loadOrInstallLibraries(config)
  }
  
}
