rm(list = ls())

### INSTALLING AND SOURCING LIBRARIES
source("src/LibraryLoader.R")
loadOrInstallLibraries("requirements/requirements.txt")

### LOADING DATA
data_control <- read.csv(
  file = "data/sample.csv",
  sep = ";",
  dec = ",",
  encoding = "UTF-8",
  stringsAsFactors = FALSE
)
colnames(data_control)[1] = "id"
# only inputs
data <- data_control %>%
  select(
    -buy_price,
    -sell_price,
    -mtm,
    -status
  )

### LOADING cpp function
source("src/functions.R")

### PARAMS
stop_loss = -10
take_profit = 10
pip_value_stock_1 = 2.5
pip_value_stock_2 = 10

### CALCULATIONS
data_result = make_scenarios(
  data,
  stop_loss,
  take_profit,
  pip_value_stock_1,
  pip_value_stock_2
)
