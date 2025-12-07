library(reticulate)
library(dplyr)

# Setup python
use_virtualenv(file.path(getwd(), ".venv"), required = TRUE)
pd <- import("pandas")

# Read data
model_df <- pd$read_excel("Model.xlsx")

print("Structure of model_df:")
str(model_df)

# Filter
best <- model_df %>% filter(Model == "RandomForestRegressor")
print("Structure of best model data:")
str(best)

print("MAE value:")
print(best$MAE)
print(class(best$MAE))

print("Numeric conversion:")
print(as.numeric(best$MAE))
