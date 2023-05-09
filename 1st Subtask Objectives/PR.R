# loading packages
library(readxl)
library(dplyr)
library(caTools)
library(useful)
library(neuralnet)
library(Metrics)

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
model = list()
formula = `20th`~t_1 + t_2 + t_3 + t_4 + t_7


time_begin = Sys.time()
model[[1]] = neuralnet(formula,
                            data = training_set,
                            hidden = c(50),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time_stop = Sys.time()
train_time_1 = time_stop - time_begin

time_begin = Sys.time()
model[[2]] = neuralnet(formula,
                            data = training_set,
                            hidden = c(100),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time_stop = Sys.time()
train_time_2 = time_stop - time_begin

time_begin = Sys.time()
model[[3]] = neuralnet(formula,
                            data = training_set,
                            hidden = c(150),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time_stop = Sys.time()
train_time_3 = time_stop - time_begin

time_begin = Sys.time()
model[[4]] = neuralnet(formula,
                            data = training_set,
                            hidden = c(50, 50),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time_stop = Sys.time()
train_time_4 = time_stop - time_begin

time_begin = Sys.time()
model[[5]] = neuralnet(formula,
                            data = training_set,
                            hidden = c(50, 100 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time_stop = Sys.time()
train_time_5 = time_stop - time_begin

time_begin = Sys.time()
model[[6]] = neuralnet(formula,
                            data = training_set,
                            hidden = c(50, 150 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time_stop = Sys.time()
train_time_6 = time_stop - time_begin

time_begin = Sys.time()
model[[7]] = neuralnet(formula,
                            data = training_set,
                            hidden = c(50, 50, 50 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time_stop = Sys.time()
train_time_7 = time_stop - time_begin

time_begin = Sys.time()
model[[8]] = neuralnet(formula,
                            data = training_set,
                            hidden = c(50, 100, 50 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time_stop = Sys.time()
train_time_8 = time_stop - time_begin

time_begin = Sys.time()
model[[9]] = neuralnet(formula,
                            data = training_set,
                            hidden = c(50, 100, 150 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time_stop = Sys.time()
train_time_9 = time_stop - time_begin


time_begin = Sys.time()
model[[10]] = neuralnet(formula,
                            data = training_set,
                            hidden = c(100, 100, 150 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time_stop = Sys.time()
train_time_10 = time_stop - time_begin

time_begin = Sys.time()
model[[11]] = neuralnet(formula,
                            data = training_set,
                            hidden = c(50, 150, 150 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time_stop = Sys.time()
train_time_11 = time_stop - time_begin

time_begin = Sys.time()
model[[12]] = neuralnet(formula,
                            data = training_set,
                            hidden = c(100, 100, 150 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time_stop = Sys.time()
train_time_12 = time_stop - time_begin

time_begin = Sys.time()
model[[13]] = neuralnet(formula,
                            data = training_set,
                            hidden = c(100, 150, 150 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time_stop = Sys.time()
train_time_13 = time_stop - time_begin

time_begin = Sys.time()
model[[14]] = neuralnet(formula,
                            data = training_set,
                            hidden = c(150, 150, 150 ),
                            linear.output = F,
                            rep = 5,
                            act.fct = "logistic",
                            threshold = 2
)
time_stop = Sys.time()
train_time_14 = time_stop - time_begin

score = sapply(model,function(x) {min(x$result.matrix[c("error"),])})

cat("Training score with `20th`~t_1 + t_2 + t_3 + t_4 + t_7 \n")
cat(paste(c("1 Hiddent layer with 50 hidden neurons: ",
            "1 Hiddent layer with 100 hidden neurons: ",
            "1 Hiddent layer with 150 hidden neurons: ",
            "2 Hiddent layer with 50 and 50 hidden neurons: ",
            "2 Hiddent layer with 50 and 100 hidden neurons: ",
            "2 Hiddent layer with 50 and 150 hidden neurons: ",
            "3 Hiddent layer with 50, 50 and 50 hidden neurons: ",
            "3 Hiddent layer with 50, 100 and 50 hidden neurons: ",
            "3 Hiddent layer with 50, 100 and 150 hidden neurons: ",
            "3 Hiddent layer with 100, 100 and 150 hidden neurons: ",
            "3 Hiddent layer with 50, 150 and 150 hidden neurons: ",
            "3 Hiddent layer with 100, 100 and 150 hidden neurons: ",
            "3 Hiddent layer with 100, 150 and 150 hidden neurons: ",
            "3 Hiddent layer with 150, 150 and 150 hidden neurons: "),
          score,
          collapse = "\n"))

cat("\n")

cat("Training times \n")
cat(paste(c("1 Hiddent layer with 50 hidden neurons: ", train_time_1,"\n",
            "1 Hiddent layer with 100 hidden neurons: ", train_time_2,"\n",
            "1 Hiddent layer with 150 hidden neurons: ", train_time_3, "\n",
            "2 Hiddent layer with 50 and 50 hidden neurons: ", train_time_4, "\n",
            "2 Hiddent layer with 50 and 100 hidden neurons: ", train_time_5, "\n",
            "2 Hiddent layer with 50 and 150 hidden neurons: ", train_time_6, "\n",
            "3 Hiddent layer with 50, 50 and 50 hidden neurons: ", train_time_7, "\n",
            "3 Hiddent layer with 50, 100 and 50 hidden neurons: ", train_time_8, "\n",
            "3 Hiddent layer with 50, 100 and 150 hidden neurons: ", train_time_9, "\n",
            "3 Hiddent layer with 100, 100 and 150 hidden neurons: ", train_time_10, "\n",
            "3 Hiddent layer with 50, 150 and 150 hidden neurons: ", train_time_11, "\n",
            "3 Hiddent layer with 100, 100 and 150 hidden neurons: ", train_time_12, "\n",
            "3 Hiddent layer with 100, 150 and 150 hidden neurons: ",train_time_13, "\n",
            "3 Hiddent layer with 150, 150 and 150 hidden neurons: ", train_time_14),
          collapse = ""))

cat("\n")


predct = lapply(model, function(x) predict(x, test_set))

score = sapply(predct, function(x){
  rmse(test_set$`20th`, x)
})

mae = sapply(predct, function(x){
  mae(test_set$`20th`, x)
})

mape = sapply(predct, function(x){
  mape(test_set$`20th`, x)
})

smape <- sapply(predct, function(x){
  smape(test_set$`20th`, x)
})

cat("RMSE:\n")
cat(paste(c("1 Hiddent layer with 50 hidden neurons: ",
            "1 Hiddent layer with 100 hidden neurons: ",
            "1 Hiddent layer with 150 hidden neurons: ",
            "2 Hiddent layer with 50 and 50 hidden neurons: ",
            "2 Hiddent layer with 50 and 100 hidden neurons: ",
            "2 Hiddent layer with 50 and 150 hidden neurons: ",
            "3 Hiddent layer with 50, 50 and 50 hidden neurons: ",
            "3 Hiddent layer with 50, 100 and 50 hidden neurons: ",
            "3 Hiddent layer with 50, 100 and 150 hidden neurons: ",
            "3 Hiddent layer with 100, 100 and 150 hidden neurons: ",
            "3 Hiddent layer with 50, 150 and 150 hidden neurons: ",
            "3 Hiddent layer with 100, 100 and 150 hidden neurons: ",
            "3 Hiddent layer with 100, 150 and 150 hidden neurons: ",
            "3 Hiddent layer with 150, 150 and 150 hidden neurons: "),
          score,
          collapse = "\n"))

cat("\n")

cat("MAE:\n")
cat(paste(c("1 Hiddent layer with 50 hidden neurons: ",
            "1 Hiddent layer with 100 hidden neurons: ",
            "1 Hiddent layer with 150 hidden neurons: ",
            "2 Hiddent layer with 50 and 50 hidden neurons: ",
            "2 Hiddent layer with 50 and 100 hidden neurons: ",
            "2 Hiddent layer with 50 and 150 hidden neurons: ",
            "3 Hiddent layer with 50, 50 and 50 hidden neurons: ",
            "3 Hiddent layer with 50, 100 and 50 hidden neurons: ",
            "3 Hiddent layer with 50, 100 and 150 hidden neurons: ",
            "3 Hiddent layer with 100, 100 and 150 hidden neurons: ",
            "3 Hiddent layer with 50, 150 and 150 hidden neurons: ",
            "3 Hiddent layer with 100, 100 and 150 hidden neurons: ",
            "3 Hiddent layer with 100, 150 and 150 hidden neurons: ",
            "3 Hiddent layer with 150, 150 and 150 hidden neurons: "),
          mae,
          collapse = "\n"))

cat("\n")

cat("MAPE:\n")
cat(paste(c("1 Hiddent layer with 50 hidden neurons: ",
            "1 Hiddent layer with 100 hidden neurons: ",
            "1 Hiddent layer with 150 hidden neurons: ",
            "2 Hiddent layer with 50 and 50 hidden neurons: ",
            "2 Hiddent layer with 50 and 100 hidden neurons: ",
            "2 Hiddent layer with 50 and 150 hidden neurons: ",
            "3 Hiddent layer with 50, 50 and 50 hidden neurons: ",
            "3 Hiddent layer with 50, 100 and 50 hidden neurons: ",
            "3 Hiddent layer with 50, 100 and 150 hidden neurons: ",
            "3 Hiddent layer with 100, 100 and 150 hidden neurons: ",
            "3 Hiddent layer with 50, 150 and 150 hidden neurons: ",
            "3 Hiddent layer with 100, 100 and 150 hidden neurons: ",
            "3 Hiddent layer with 100, 150 and 150 hidden neurons: ",
            "3 Hiddent layer with 150, 150 and 150 hidden neurons: "),
          mape,
          collapse = "\n"))

cat("\n")

cat("SMAPE:\n")
cat(paste(c("1 Hiddent layer with 50 hidden neurons: ",
            "1 Hiddent layer with 100 hidden neurons: ",
            "1 Hiddent layer with 150 hidden neurons: ",
            "2 Hiddent layer with 50 and 50 hidden neurons: ",
            "2 Hiddent layer with 50 and 100 hidden neurons: ",
            "2 Hiddent layer with 50 and 150 hidden neurons: ",
            "3 Hiddent layer with 50, 50 and 50 hidden neurons: ",
            "3 Hiddent layer with 50, 100 and 50 hidden neurons: ",
            "3 Hiddent layer with 50, 100 and 150 hidden neurons: ",
            "3 Hiddent layer with 100, 100 and 150 hidden neurons: ",
            "3 Hiddent layer with 50, 150 and 150 hidden neurons: ",
            "3 Hiddent layer with 100, 100 and 150 hidden neurons: ",
            "3 Hiddent layer with 100, 150 and 150 hidden neurons: ",
            "3 Hiddent layer with 150, 150 and 150 hidden neurons: "),
          smape,
          collapse = "\n"))

cat("\n")