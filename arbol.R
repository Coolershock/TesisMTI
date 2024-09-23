# Input load. Please do not change #
`dataset` = read.csv('C:/Users/coole/REditorWrapper_2f90b6dc-879d-4d46-b427-2c0fc1844eda/input_df_e7ea8102-f69a-42a1-b48a-135c2b8c7f62.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #
# El código siguiente, que crea un dataframe y quita las filas duplicadas, siempre se ejecuta y actúa como un preámbulo del script: 

# dataset <- data.frame(venta, idprod, promdist, zona)
# dataset <- unique(dataset)

# Pegue o escriba aquí el código de script:

data <- dataset

# training set
set.seed(pi)
itrain     <- sample( 1:4898, size=3750, replace = FALSE)
zona_train <- data[itrain, ]
nrow(zona_train)

# test set
zona_test  <- data[-itrain, ]

library(rpart)
m.rpart <- rpart(zona ~. 
                 , data = zona_train)
m.rpart 
library(rpart.plot)     
rpart.plot(m.rpart)
p.rpart <- predict( m.rpart, zona_test )
summary(p.rpart)
summary( zona_test$zona )

MAE <- function(actual, predicted){
  mean(abs (actual - predicted))
}

MAE(zona_test$zona, p.rpart)

# ajuste
m.m5p <- rpart(zona ~. , data = zona_train)

# Construimos un predictor
p.m5p <- predict(m.m5p, zona_test)

m.m5p

MAE( zona_test$zona, p.m5p )

test <- data.frame(idprod = 200, venta = 4000, promdist = 1)

test_pred <- predict(m.m5p, test)

test_pred
