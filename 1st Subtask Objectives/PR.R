# loading packages
library(readxl)
library(dplyr)
library(caTools)

# Import the data set
dataset = read_xlsx('uow_consumption.xlsx')

# Rename column
colnames(dataset)[2] = "18th"
colnames(dataset)[3] = "19th"
colnames(dataset)[4] = "20th"

# Data shift
shifted_dataset = dataset
shifted_dataset = shift.column(data=shifted_dataset, columns="20th",len = 1, up = FALSE,newNames = sprintf("t-1", "20th"))
shifted_dataset = shift.column(data=shifted_dataset, columns="20th",len = 2, up = FALSE,newNames = sprintf("t-2", "20th"))
shifted_dataset = shift.column(data=shifted_dataset, columns="20th",len = 3, up = FALSE,newNames = sprintf("t-3", "20th"))
shifted_dataset = shift.column(data=shifted_dataset, columns="20th",len = 4, up = FALSE,newNames = sprintf("t-4", "20th"))
shifted_dataset = shift.column(data=shifted_dataset, columns="20th",len = 7, up = FALSE,newNames = sprintf("t-7", "20th"))

# normalization
shifted_dataset = as.data.frame(shifted_dataset %>% mutate_at(vars(-date), scale, center=T))

#remove date column from dataset
shifted_dataset = shifted_dataset[-1]

