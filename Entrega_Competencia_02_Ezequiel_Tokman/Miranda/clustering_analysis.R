setwd("/Documents and Settings/Digodat/OneDrive - Económicas - UBA/Documentos/Maestría Exactas/EyF/Miranda")

percentiles <- read.csv("./datasets_percentiles.csv")

library(data.table)
library(ggplot2)

percentiles <- data.table(percentiles)

percentil_10 <- percentiles[probs == 0.10]
percentil_25 <- percentiles[probs == 0.25]
percentil_50 <- percentiles[probs == 0.50]
percentil_75 <- percentiles[probs == 0.75]
percentil_90 <- percentiles[probs == 0.90]
percentil_100 <- percentiles[probs == 0.100]


# --- --- --- --- --- --- CREAMOS LOS GRÁFICOS --- --- --- --- --- --- #



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



# --- --- --- --- --- ---  --- --- --- --- --- --- #




























