# loading packages
library(readxl)
library(dplyr)
library(caTools)
library(useful)
library(neuralnet)

# Import the data set
dataset = read_xlsx('uow_consumption.xlsx')

# Rename column
colnames(dataset)[2] = "18th"
colnames(dataset)[3] = "19th"
colnames(dataset)[4] = "20th"

# Data shift
shifted_dataset = dataset
shifted_dataset = shift.column(data=shifted_dataset, columns="20th",len = 1, up = FALSE,newNames = sprintf("t_1", "20th"))
shifted_dataset = shift.column(data=shifted_dataset, columns="20th",len = 2, up = FALSE,newNames = sprintf("t_2", "20th"))
shifted_dataset = shift.column(data=shifted_dataset, columns="20th",len = 3, up = FALSE,newNames = sprintf("t_3", "20th"))
shifted_dataset = shift.column(data=shifted_dataset, columns="20th",len = 4, up = FALSE,newNames = sprintf("t_4", "20th"))
shifted_dataset = shift.column(data=shifted_dataset, columns="20th",len = 7, up = FALSE,newNames = sprintf("t_7", "20th"))

# normalization
uow_data_scaled = as.data.frame(shifted_dataset %>% mutate_at(vars(-date), scale, center=T))

# Split the dataset
set.seed(123)
split = sample.split(shifted_dataset$date, SplitRatio = 0.8)

training_set = subset(shifted_dataset, split == TRUE)
test_set = subset(shifted_dataset, split == FALSE)

#remove date column from dataset
training_set = training_set[-1]
test_set = test_set[-1]

# Create NN model list
nn_models = list()
formula = `20th`~t_1 + t_2 + t_3 + t_4 + t_7


time.start <- Sys.time()
nn_models[[1]] <- neuralnet(formula,
                            data = training_set,
                            hidden = c(50),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time.start <- Sys.time()
nn_models[[2]] <- neuralnet(formula,
                            data = training_set,
                            hidden = c(100),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time.start <- Sys.time()
nn_models[[3]] <- neuralnet(formula,
                            data = training_set,
                            hidden = c(150),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time.start <- Sys.time()
nn_models[[4]] <- neuralnet(formula,
                            data = training_set,
                            hidden = c(50, 50),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time.start <- Sys.time()
nn_models[[5]] <- neuralnet(formula,
                            data = training_set,
                            hidden = c(50, 100 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time.start <- Sys.time()
nn_models[[6]] <- neuralnet(formula,
                            data = training_set,
                            hidden = c(50, 150 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time.start <- Sys.time()
nn_models[[7]] <- neuralnet(formula,
                            data = training_set,
                            hidden = c(50, 50, 50 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time.start <- Sys.time()
nn_models[[8]] <- neuralnet(formula,
                            data = training_set,
                            hidden = c(50, 100, 50 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time.start <- Sys.time()
nn_models[[9]] <- neuralnet(formula,
                            data = training_set,
                            hidden = c(50, 100, 150 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)

time.start <- Sys.time()
nn_models[[10]] <- neuralnet(formula,
                            data = training_set,
                            hidden = c(100, 100, 150 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time.start <- Sys.time()
nn_models[[11]] <- neuralnet(formula,
                            data = training_set,
                            hidden = c(50, 150, 150 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time.start <- Sys.time()
nn_models[[12]] <- neuralnet(formula,
                            data = training_set,
                            hidden = c(100, 100, 150 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time.start <- Sys.time()
nn_models[[13]] <- neuralnet(formula,
                            data = training_set,
                            hidden = c(100, 150, 150 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time.start <- Sys.time()
nn_models[[14]] <- neuralnet(formula,
                            data = training_set,
                            hidden = c(150, 150, 150 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)

