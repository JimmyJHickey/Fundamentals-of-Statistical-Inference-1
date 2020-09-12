###
# Jimmy Hickey
# 2020-09-10
# Lecture by Shannon Holloway
###

library(modelObj)
library(DynTxRegime)

df = read.csv("data/dyntx_lecture.txt")
summary(df)

moMain <- buildModelObj(model = ~x1+x2,
                        solver.method = 'lm',
                        predict.method = 'predict.lm')

moCont <- buildModelObj(mode = ~x2+x3,
                        solver.method = 'lm',
                        predict.method = 'predict.lm')

qObj <- qLearn(moMain = moMain, moCont = moCont, iter = 0L,
               data = df, response = df$y, txName = 'A',
               verbose = TRUE)

coef(qObj)

fitObj <- fitObject(object = qObj)
fitObj

outcome(qObj)
