# Input load. Please do not change #
`dataset` = read.csv('C:/Users/coole/REditorWrapper_c4521b5b-f759-4757-8afc-5b7fe7445f18/input_df_e7ebec34-c13a-418d-94e2-8cca69614b74.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #
# El código siguiente, que crea un dataframe y quita las filas duplicadas, siempre se ejecuta y actúa como un preámbulo del script: 

# dataset <- data.frame(venta, zona, promdist, idprod)
# dataset <- unique(dataset)

# Pegue o escriba aquí el código de script:

# Read the Data
data = dataset
data

# Random sampling
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )

# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))


## Fit neural network 

# load library
library(neuralnet)

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]

# fit neural network
set.seed(2)
NN = neuralnet(promdist ~ idprod + venta + zona + promdist, trainNN, hidden = 3 , linear.output = T )

# plot neural network
plot(NN)
