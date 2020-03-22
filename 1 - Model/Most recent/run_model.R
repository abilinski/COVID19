dir.name = "1 - Model/Most recent/"
source(file.path(dir.name, "model_3strat_18_mar_2020.R"))


setwd("./0 - Parameters")
# read in parameters
df = read.csv("parameters_17_mar_2020.csv", as.is = T)
params = df[1,]
# run scenario for 30 days
test = run_param_vec(params = df[1,], params2 = NULL, days_out1 = 30,
                   days_out2 = NULL, model_type = run_basic)

cat(format_csv(test))
