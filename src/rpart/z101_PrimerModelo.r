rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Digodat\\OneDrive - Económicas - UBA\\Documentos\\Maestría Exactas\\EyF")

# cargo el dataset
dataset <- fread("./datasets/data_cleaned_04.csv")

# chequeamos
dataset[, .(Count = .N), by = clase_ternaria]


dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.48, # esto significa no limitar la complejidad de los splits
        minsplit = 842, # minima cantidad de registros para que se haga el split. Cuanto más chico, más overfitting
        minbucket = 419, # tamaño minimo de una hoja. Cuanto más chico, más overfitting.
        maxdepth = 7 # profundidad. Cuanto más grande, más overfitting.
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "baja t+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_39.csv",
        sep = ","
)





# Extract the variable importances
#variable_importance <- modelo$variable.importance

# Sort the split variables by importance in descending order
#sorted_split_variables <- names(variable_importance)[order(-variable_importance)]

# Select the top 10 split variables
#top_15_split_variables <- sorted_split_variables[1:15]

# Create a vector of columns to keep
#columns_to_keep <- c("foto_mes", "clase_ternaria","numero_de_cliente", top_15_split_variables)

# Keep only the selected columns in your_data
#dataset <- dataset[, ..columns_to_keep]



