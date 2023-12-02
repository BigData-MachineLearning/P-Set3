#### import

source("00_packages.R")

train_personas <- import("stores/train_personas.csv")
train_hogares <- import("stores/train_hogares.csv")

test_personas <- import("stores/test_personas.csv")
test_hogares <- import("stores/test_hogares.csv")


# Exporto en rds porque usar csv es medio paila.
rio::export(train_personas, "scripts/01_import/output/train_personas.rds")
rio::export(train_hogares, "scripts/01_import/output/train_hogares.rds")
rio::export(test_personas, "scripts/01_import/output/test_personas.rds")
rio::export(test_hogares, "scripts/01_import/output/test_hogares.rds")


