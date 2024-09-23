data <- produzonadis2

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

library("Rgraphviz")
# ajuste
m.m5p <- rpart(zona ~. , data = zona_train)

# Construimos un predictor
p.m5p <- predict(m.m5p, zona_test)

m.m5p

MAE( zona_test$zona, p.m5p )

test <- data.frame(idprod = 200, venta = 4000, promdist = 1)

test_pred <- predict(m.m5p, test)

test_pred

plot(test_pred)
