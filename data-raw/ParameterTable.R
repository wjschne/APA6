parameterTable <- read.csv("data-raw\\ParameterTable.csv",stringsAsFactors = FALSE)
devtools::use_data(parameterTable, internal = TRUE,overwrite = TRUE)
