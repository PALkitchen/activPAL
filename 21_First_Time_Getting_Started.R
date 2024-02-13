################# 	first time getting started script	####################

writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

install.packages("devtools", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("Rcpp", dependencies = TRUE)

devtools::install_github("PALkitchen/activPAL")

"force = TRUE"
