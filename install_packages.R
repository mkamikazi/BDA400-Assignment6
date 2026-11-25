# Install missing dependencies once, using CRAN.
pkgs <- c('shiny','ggplot2','quantmod','TTR')
missing <- pkgs[!vapply(pkgs,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) install.packages(missing,repos='https://cloud.r-project.org')
