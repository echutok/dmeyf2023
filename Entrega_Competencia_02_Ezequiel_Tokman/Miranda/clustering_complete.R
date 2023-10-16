# --- --- Carga de librerías y datasets --- --- #
# --- --- CORRER UNA SÓLA VEZ --- --- #

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

library(data.table)
library(randomForest)
library(dplyr)
setwd("./buckets/b1/datasets")
getwd()


# Cargamos el dataset original.
dt <- fread("./competencia_02.csv.gz", stringsAsFactors = TRUE)

# ordenamos el dataset
dt <- dt[order(numero_de_cliente, foto_mes)]


# Nos quedamos con los BAJA+2 del dataset original. Esto es dataset1.
dt_dataset1 <- dt[clase_ternaria == "BAJA+2"]

# Convertimos data.table a dataframe
df_dataset1 <- as.data.frame(dt_dataset1)




# --- --- Construimos los clusters --- --- #
# --- --- CORRER UNA SÓLA VEZ --- --- #

# Tratamos los nulos
df_dataset1 <- na.roughfix(df_dataset1)
any_NAs <- any(is.na(df_dataset1))
any_NAs


# Excluimos las columnas que no tienen que entrar en el modelo
df_dataset1_modelo <- df_dataset1 %>% select(-foto_mes, -numero_de_cliente, -clase_ternaria)

# Creamos el modelo de clustering.
rf <- randomForest(df_dataset1_modelo, y = NULL, ntree = 1000, proximity = TRUE, oob.prox = TRUE, na.action=na.roughfix)
hclust.rf <- hclust(as.dist(1-rf$proximity), method = "ward.D2")
rf.cluster = cutree(hclust.rf, k=7)

# Vemos cuántos quedan en cada cluster.
table(rf.cluster)

# Agregamos la columna del cluster dataset
dt_dataset1$rf.cluster <- rf.cluster


# Guardamos el archivo dt_dataset1
output_file <- "dataset1_clusters.csv"
fwrite(dt_dataset1, file = output_file, quote = FALSE, sep = ",")



# Perform a left join to bring the "cluster" column from dataset1 to dataset2
setkey(dt_dataset1, numero_de_cliente)
setkey(dt_dataset2, numero_de_cliente)

# Merge dt_dataset2 with dt_dataset1 to add the "cluster" column
dt_dataset2 <- merge(dt_dataset2, dt_dataset1[, .(numero_de_cliente, rf.cluster)], by = "numero_de_cliente", all.x = TRUE)


# Guardamos el archivo dt_dataset2
output_file <- "dataset2_clusters.csv"
fwrite(dt_dataset2, file = output_file, quote = FALSE, sep = ",")







# --- --- Comenzar desde acá, lo anterior se ejecuta una sóla vez --- --- #
# --- --- Cargamos las librerías y los datasets construidos --- --- #

library(data.table)
library(dplyr)
library(ggplot2)
setwd("./buckets/b1/datasets")
getwd()


dataset1 <- fread("./dataset1_clusters.csv", stringsAsFactors = TRUE)
dataset2 <- fread("./dataset2_clusters.csv", stringsAsFactors = TRUE)

# Verificamos que esté todo OK.
unique_numero_de_cliente_dataset1 <- unique(dataset1$numero_de_cliente)
unique_numero_de_cliente_dataset2 <- unique(dataset2$numero_de_cliente)
length(unique_numero_de_cliente_dataset1) = length(unique_numero_de_cliente_dataset2)



# --- --- Generamos el dataset de percentiles --- --- #

dataset1[is.na(dataset1)] <- 0
percentiles <- dataset1[, 
                        lapply(.SD, function(x) quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 1))), 
                        by = rf.cluster, 
                        .SDcols = setdiff(names(dataset1), c("foto_mes", "clase_ternaria", "numero_de_cliente"))
]

# Eliminamos ultima columna que quedó duplicada
percentiles <- percentiles[, .SD, .SDcols = -ncol(percentiles)]

# Define the desired probability levels
probs <- c(0.1, 0.25, 0.5, 0.75, 0.9, 1)

# Create a data.table for the probability levels
probs_reference <- data.table(probs = probs)

# Merge the 'percentiles' data.table with the 'probs_reference' data.table
percentiles[, probs := rep(probs, length.out = .N), by = rf.cluster]

# Move the "probs" column to the second position
percentiles <- percentiles[, .(rf.cluster, probs, .SD), .SDcols = -c("rf.cluster", "probs")]

# Exportamos CSV percentiles. 
output_file <- "dataset_percentiles.csv"
fwrite(percentiles, file = output_file, quote = FALSE, sep = ",")


# --- --- Generamos los gráficos. Un PDF por cada grupo de percentil --- --- #


percentil_10 <- percentiles[probs == 0.10]
percentil_25 <- percentiles[probs == 0.25]
percentil_50 <- percentiles[probs == 0.50]
percentil_75 <- percentiles[probs == 0.75]
percentil_90 <- percentiles[probs == 0.90]
percentil_100 <- percentiles[probs == 0.100]


# Create a PDF file to save the barplots
pdf("percentil_10.pdf")

# Define the columns to be plotted
columns_to_plot <- setdiff(names(percentil_10), c("rf.cluster", "probs"))

# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the columns and create barplots
for (column in columns_to_plot) {
  p <- ggplot(data = percentil_10, aes(x = rf.cluster, y = .data[[column]])) +
    geom_bar(stat = "identity", fill = "darkseagreen") +
    labs(
      title = paste(column, "by Cluster"),
      x = "Cluster",
      y = column
    )
  
  plot_list[[column]] <- p
  print(p)
}
# Close the PDF file
dev.off()




# Create a PDF file to save the barplots
pdf("percentil_25.pdf")

# Define the columns to be plotted
columns_to_plot <- setdiff(names(percentil_25), c("rf.cluster", "probs"))

# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the columns and create barplots
for (column in columns_to_plot) {
  p <- ggplot(data = percentil_25, aes(x = rf.cluster, y = .data[[column]])) +
    geom_bar(stat = "identity", fill = "darkseagreen") +
    labs(
      title = paste(column, "by Cluster"),
      x = "Cluster",
      y = column
    )
  
  plot_list[[column]] <- p
  print(p)
}
# Close the PDF file
dev.off()






# Create a PDF file to save the barplots
pdf("percentil_50.pdf")

# Define the columns to be plotted
columns_to_plot <- setdiff(names(percentil_50), c("rf.cluster", "probs"))

# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the columns and create barplots
for (column in columns_to_plot) {
  p <- ggplot(data = percentil_50, aes(x = rf.cluster, y = .data[[column]])) +
    geom_bar(stat = "identity", fill = "darkseagreen") +
    labs(
      title = paste(column, "by Cluster"),
      x = "Cluster",
      y = column
    )
  
  plot_list[[column]] <- p
  print(p)
}
# Close the PDF file
dev.off()




# Create a PDF file to save the barplots
pdf("percentil_75.pdf")

# Define the columns to be plotted
columns_to_plot <- setdiff(names(percentil_75), c("rf.cluster", "probs"))

# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the columns and create barplots
for (column in columns_to_plot) {
  p <- ggplot(data = percentil_75, aes(x = rf.cluster, y = .data[[column]])) +
    geom_bar(stat = "identity", fill = "darkseagreen") +
    labs(
      title = paste(column, "by Cluster"),
      x = "Cluster",
      y = column
    )
  
  plot_list[[column]] <- p
  print(p)
}
# Close the PDF file
dev.off()





# Create a PDF file to save the barplots
pdf("percentil_90.pdf")

# Define the columns to be plotted
columns_to_plot <- setdiff(names(percentil_90), c("rf.cluster", "probs"))

# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the columns and create barplots
for (column in columns_to_plot) {
  p <- ggplot(data = percentil_90, aes(x = rf.cluster, y = .data[[column]])) +
    geom_bar(stat = "identity", fill = "darkseagreen") +
    labs(
      title = paste(column, "by Cluster"),
      x = "Cluster",
      y = column
    )
  
  plot_list[[column]] <- p
  print(p)
}
# Close the PDF file
dev.off()





# Create a PDF file to save the barplots
pdf("percentil_100.pdf")

# Define the columns to be plotted
columns_to_plot <- setdiff(names(percentil_100), c("rf.cluster", "probs"))

# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the columns and create barplots
for (column in columns_to_plot) {
  p <- ggplot(data = percentil_100, aes(x = rf.cluster, y = .data[[column]])) +
    geom_bar(stat = "identity", fill = "darkseagreen") +
    labs(
      title = paste(column, "by Cluster"),
      x = "Cluster",
      y = column
    )
  
  plot_list[[column]] <- p
  print(p)
}
# Close the PDF file
dev.off()




# --- --- Preparamos datos para análisis de datos históricos --- --- #

rm(list = ls()) # remove all objects
gc() # garbage collection


setwd("/Documents and Settings/Digodat/OneDrive - Económicas - UBA/Documentos/Maestría Exactas/EyF/Miranda")
library(data.table)
library(ggplot2)

dataset2 <- fread("./datasets_dataset2_clusters.csv", stringsAsFactors = TRUE)

clientes_por_cluster <-  dataset2[, .N, by = rf.cluster]
clientes_por_cluster

# Creamos una lista de las columnas que empiezan con "m", ya que son todas las columnas que hay que ajustar por inflación.
columns_starting_with_m <- names(dataset2)[grep("^m", names(dataset2))]
columns_starting_with_m <- columns_starting_with_m[!grepl("mcaja_ahorro_dolares", columns_starting_with_m)]


# Creating a factor_inflation column with values based on the conditions
dataset2[foto_mes == 201901, factor_inflation := NA_real_][
  foto_mes == 201902, factor_inflation := 0.0288][
  foto_mes == 201903, factor_inflation := 0.0678][
  foto_mes == 201904, factor_inflation := 0.1177][
  foto_mes == 201905, factor_inflation := 0.1563][
  foto_mes == 201906, factor_inflation := 0.1915][
  foto_mes == 201907, factor_inflation := 0.2236][
  foto_mes == 201908, factor_inflation := 0.2507][
  foto_mes == 201909, factor_inflation := 0.3001][
  foto_mes == 201910, factor_inflation := 0.3766][
  foto_mes == 201911, factor_inflation := 0.4221][
  foto_mes == 201912, factor_inflation := 0.4824][
  foto_mes == 202001, factor_inflation := 0.5377][
  foto_mes == 202002, factor_inflation := 0.5724][
  foto_mes == 202003, factor_inflation := 0.6045][
  foto_mes == 202004, factor_inflation := 0.6582][
  foto_mes == 202005, factor_inflation := 0.6826][
  foto_mes == 202006, factor_inflation := 0.7086][
  foto_mes == 202007, factor_inflation := 0.7472][
  foto_mes == 202008, factor_inflation := 0.7808][
  foto_mes == 202009, factor_inflation := 0.8291][
  foto_mes == 202010, factor_inflation := 0.8807][
  foto_mes == 202011, factor_inflation := 0.9517][
  foto_mes == 202012, factor_inflation := 1.0131][
  foto_mes == 202101, factor_inflation := 1.0939][
  foto_mes == 202102, factor_inflation := 1.1785][
  foto_mes == 202103, factor_inflation := 1.2567][
  foto_mes == 202104, factor_inflation := 1.3652][
  foto_mes == 202105, factor_inflation := 1.4618][
  foto_mes == 202106, factor_inflation := 1.5432][
  foto_mes == 202107, factor_inflation := 1.6240]

# Assuming 'dt' is your data.table
# Looping through all the columns that need to be adjusted
for (col in columns_starting_with_m) {
  dataset2[, (paste0(col, "_new")) := fifelse(foto_mes == 201901, get(col), get(col) - get(col) * factor_inflation)]
}


# Validamos para una columna específica. 
# La cuenta que hay que hacer es:
# mtransferencias_recibidas_new = mtransferencias_recibidas - mtransferencias_recibidas * factor_inflation
subset_data_validacion <- dataset2[, .(foto_mes, 
                                       mtransferencias_recibidas, 
                                       mtransferencias_recibidas_new, 
                                       factor_inflation)]


# Eliminamos las columnas viejas
dataset2 <- dataset2[, !(names(dataset2) %in% columns_starting_with_m), with = FALSE]



# Calculamos date_custom
calculate_date_custom <- function(foto_mes, clase_ternaria) {
  baja_row_index <- which(clase_ternaria == "BAJA+2")
  baja_year <- as.integer(substr(foto_mes[baja_row_index], 1, 4))
  baja_month <- as.integer(substr(foto_mes[baja_row_index], 5, 6))
  cur_year <- as.integer(substr(foto_mes, 1, 4))
  cur_month <- as.integer(substr(foto_mes, 5, 6))
  months_diff <- (baja_year - cur_year) * 12 + (baja_month - cur_month)
  -months_diff
}

# Set the key for the data.table
setkey(dataset2, numero_de_cliente)

# Calculate date_custom for each numero_de_cliente
dataset2[, date_custom := calculate_date_custom(foto_mes, clase_ternaria), by = numero_de_cliente]

# Algunos dan mal porque no tienen completado clase_ternaria. Los excluimos.
dataset2 <- dataset2[!date_custom > 1]
unique(dataset2$date_custom)

# Validamos para clientes random
client_test <- dataset2[numero_de_cliente == 182617325]
client_test <- dataset2[numero_de_cliente == 57010109]
client_test <- dataset2[numero_de_cliente == 57765533]
client_test <- dataset2[numero_de_cliente == 56808477]



# Agrupamos
cols_to_exclude <- setdiff(names(dataset2), c("clase_ternaria", 
                                          "foto_mes", 
                                          "numero_de_cliente", 
                                          "date_custom",
                                          "rf.cluster",
                                          "factor_inflation"))

# Calculate the median for each group based on data_custom and rf.cluster
#dataset2_grouped <- dataset2[, lapply(.SD, median), by = .(date_custom, rf.cluster), .SDcols = cols_to_exclude]

# Calculate the 60th percentile for each group based on data_custom and rf.cluster, allowing for missing values
dataset2_grouped <- dataset2[, lapply(.SD, function(x) quantile(x, probs = 0.80, na.rm = TRUE)), 
                             by = .(date_custom, rf.cluster), .SDcols = cols_to_exclude]


# Hacemos un dt para cada cluster
dataset2_grouped_cluster1 <- dataset2_grouped[rf.cluster == 1]
dataset2_grouped_cluster2 <- dataset2_grouped[rf.cluster == 2]
dataset2_grouped_cluster3 <- dataset2_grouped[rf.cluster == 3]
dataset2_grouped_cluster4 <- dataset2_grouped[rf.cluster == 4]
dataset2_grouped_cluster5 <- dataset2_grouped[rf.cluster == 5]
dataset2_grouped_cluster6 <- dataset2_grouped[rf.cluster == 6]
dataset2_grouped_cluster7 <- dataset2_grouped[rf.cluster == 7]


# --- --- Graficamos. Un PDF pro cada cluster --- --- #

# Specify specific columns for plotting
columns_to_plot <- c("ctrx_quarter", 
                     "cproductos", 
                     "ctarjeta_debito_transacciones",
                     "cseguro_vida",
                     "mcaja_ahorro_new",
                     "mcaja_ahorro_dolares")



# Creamos PDF.
pdf("historico_cluster1.pdf")

# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the columns and create line plots
for (column in columns_to_plot) {
  if (is.numeric(dataset2_grouped_cluster1[[column]])) {
    p <- ggplot(dataset2_grouped_cluster1, aes(x = date_custom, y = .data[[column]])) +
      geom_line(color = "darkseagreen") +
      geom_point(color = "white", size = 3, shape = 21, fill = "darkseagreen") +
      labs(x = "date_custom", y = column) +
      ggtitle(paste("Cluster 1: evolución del percentil 80 de", column, "\nlos meses previos a darse de baja")) +
      theme_bw() +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      labs(color = "Legend", linetype = "Legend") +
      annotate("text", x = -1, y = quantile(dataset2_grouped_cluster1[[column]], 0.9, na.rm = TRUE), label = "BAJA+2", angle = 90)
    
    plot_list[[column]] <- p
    print(p)
  }
}

# Close the PDF file
dev.off()





# Creamos PDF.
pdf("historico_cluster2.pdf")

# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the columns and create line plots
for (column in columns_to_plot) {
  if (is.numeric(dataset2_grouped_cluster2[[column]])) {
    p <- ggplot(dataset2_grouped_cluster2, aes(x = date_custom, y = .data[[column]])) +
      geom_line(color = "darkseagreen") +
      geom_point(color = "white", size = 3, shape = 21, fill = "darkseagreen") +
      labs(x = "date_custom", y = column) +
      ggtitle(paste("Cluster 2: evolución del percentil 80 de", column, "\nlos meses previos a darse de baja")) +
      theme_bw() +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      labs(color = "Legend", linetype = "Legend") +
      annotate("text", x = -1, y = quantile(dataset2_grouped_cluster2[[column]], 0.9, na.rm = TRUE), label = "BAJA+2", angle = 90)
    
    plot_list[[column]] <- p
    print(p)
  }
}

# Close the PDF file
dev.off()



# Creamos PDF.
pdf("historico_cluster3.pdf")

# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the columns and create line plots
for (column in columns_to_plot) {
  if (is.numeric(dataset2_grouped_cluster3[[column]])) {
    p <- ggplot(dataset2_grouped_cluster3, aes(x = date_custom, y = .data[[column]])) +
      geom_line(color = "darkseagreen") +
      geom_point(color = "white", size = 3, shape = 21, fill = "darkseagreen") +
      labs(x = "date_custom", y = column) +
      ggtitle(paste("Cluster 3: evolución del percentil 80 de", column, "\nlos meses previos a darse de baja")) +
      theme_bw() +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      labs(color = "Legend", linetype = "Legend") +
      annotate("text", x = -1, y = quantile(dataset2_grouped_cluster3[[column]], 0.9, na.rm = TRUE), label = "BAJA+2", angle = 90)
    
    plot_list[[column]] <- p
    print(p)
  }
}

# Close the PDF file
dev.off()




# Creamos PDF.
pdf("historico_cluster4.pdf")

# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the columns and create line plots
for (column in columns_to_plot) {
  if (is.numeric(dataset2_grouped_cluster4[[column]])) {
    p <- ggplot(dataset2_grouped_cluster4, aes(x = date_custom, y = .data[[column]])) +
      geom_line(color = "darkseagreen") +
      geom_point(color = "white", size = 3, shape = 21, fill = "darkseagreen") +
      labs(x = "date_custom", y = column) +
      ggtitle(paste("Cluster 4: evolución del percentil 80 de", column, "\nlos meses previos a darse de baja")) +
      theme_bw() +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      labs(color = "Legend", linetype = "Legend") +
      annotate("text", x = -1, y = quantile(dataset2_grouped_cluster4[[column]], 0.9, na.rm = TRUE), label = "BAJA+2", angle = 90)
    
    plot_list[[column]] <- p
    print(p)
  }
}

# Close the PDF file
dev.off()





# Creamos PDF.
pdf("historico_cluster5.pdf")

# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the columns and create line plots
for (column in columns_to_plot) {
  if (is.numeric(dataset2_grouped_cluster5[[column]])) {
    p <- ggplot(dataset2_grouped_cluster5, aes(x = date_custom, y = .data[[column]])) +
      geom_line(color = "darkseagreen") +
      geom_point(color = "white", size = 3, shape = 21, fill = "darkseagreen") +
      labs(x = "date_custom", y = column) +
      ggtitle(paste("Cluster 5: evolución del percentil 80 de", column, "\nlos meses previos a darse de baja")) +
      theme_bw() +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      labs(color = "Legend", linetype = "Legend") +
      annotate("text", x = -1, y = quantile(dataset2_grouped_cluster5[[column]], 0.9, na.rm = TRUE), label = "BAJA+2", angle = 90)
    
    plot_list[[column]] <- p
    print(p)
  }
}

# Close the PDF file
dev.off()




# Creamos PDF.
pdf("historico_cluster6.pdf")

# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the columns and create line plots
for (column in columns_to_plot) {
  if (is.numeric(dataset2_grouped_cluster6[[column]])) {
    p <- ggplot(dataset2_grouped_cluster6, aes(x = date_custom, y = .data[[column]])) +
      geom_line(color = "darkseagreen") +
      geom_point(color = "white", size = 3, shape = 21, fill = "darkseagreen") +
      labs(x = "date_custom", y = column) +
      ggtitle(paste("Cluster 6: evolución del percentil 80 de", column, "\nlos meses previos a darse de baja")) +
      theme_bw() +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      labs(color = "Legend", linetype = "Legend") +
      annotate("text", x = -1, y = quantile(dataset2_grouped_cluster6[[column]], 0.9, na.rm = TRUE), label = "BAJA+2", angle = 90)
    
    plot_list[[column]] <- p
    print(p)
  }
}

# Close the PDF file
dev.off()




# Creamos PDF.
pdf("historico_cluster7.pdf")

# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the columns and create line plots
for (column in columns_to_plot) {
  if (is.numeric(dataset2_grouped_cluster7[[column]])) {
    p <- ggplot(dataset2_grouped_cluster7, aes(x = date_custom, y = .data[[column]])) +
      geom_line(color = "darkseagreen") +
      geom_point(color = "white", size = 3, shape = 21, fill = "darkseagreen") +
      labs(x = "date_custom", y = column) +
      ggtitle(paste("Cluster 7: evolución del percentil 80 de", column, "\nlos meses previos a darse de baja")) +
      theme_bw() +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      labs(color = "Legend", linetype = "Legend") +
      annotate("text", x = -1, y = quantile(dataset2_grouped_cluster7[[column]], 0.9, na.rm = TRUE), label = "BAJA+2", angle = 90)
    
    plot_list[[column]] <- p
    print(p)
  }
}

# Close the PDF file
dev.off()


colnames(dataset2)
