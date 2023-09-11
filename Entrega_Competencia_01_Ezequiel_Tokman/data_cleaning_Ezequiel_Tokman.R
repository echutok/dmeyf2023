rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Digodat\\OneDrive - Económicas - UBA\\Documentos\\Maestría Exactas\\EyF\\datasets")

# cargo el dataset
dataset <- fread("./competencia_01_new.csv")


dt <- as.data.table(dataset)


# Contamos nulos
count_na_null <- function(x) {
  sum(is.na(x) | is.null(x))
}
na_null_counts <- dt[, lapply(.SD, count_na_null)]
melted_nulls <- melt(na_null_counts, variable.name = "Column", value.name = "Nulos")
melted_nulls <- melted_nulls[order(-Nulos)]


## Reemplazamos nulos por 0

# Para muchas
columns_to_replace <- c("Master_Finiciomora",
                       "Visa_Finiciomora",
                       "Master_mconsumospesos",
                       "Master_mconsumosdolares",
                       "Master_madelantopesos",
                       "Master_madelantodolares",
                       "Master_mpagospesos",
                       "Master_mpagosdolares",
                       "Master_mconsumototal",
                       "Master_cconsumos",
                       "Master_cadelantosefectivo",
                       "Visa_mconsumospesos",
                       "Visa_mconsumosdolares",
                       "Visa_madelantopesos",
                       "Visa_madelantodolares",
                       "Visa_mpagospesos",
                       "Visa_mpagosdolares",
                       "Visa_mconsumototal",
                       "Visa_cconsumos",
                       "Visa_cadelantosefectivo",
                       "Master_fultimo_cierre",
                       "Master_delinquency",
                       "Master_status",
                       "Master_mfinanciacion_limite",
                       "Master_Fvencimiento",
                       "Master_msaldototal",
                       "Master_msaldopesos",
                       "Master_msaldodolares",
                       "Master_mlimitecompra",
                       "Master_mpagado",
                       "Master_fechaalta",
                       "Master_mpagominimo",
                       "Visa_fultimo_cierre",
                       "Visa_delinquency",
                       "Visa_status",
                       "Visa_mfinanciacion_limite",
                       "Visa_Fvencimiento",
                       "Visa_msaldototal",
                       "Visa_msaldopesos",
                       "Visa_msaldodolares",
                       "Visa_mlimitecompra",
                       "Visa_mpagado",
                       "Visa_fechaalta",
                       "Visa_mpagominimo",
                       "mtarjeta_master_descuentos",
                       "mtarjeta_visa_descuentos"
)

# Loop a las columnas de la lista y reemplazamos NULLs y NAs por 0.
for (column_name in columns_to_replace) {
  dt[is.null(dt[[column_name]]) | is.na(dt[[column_name]]), (column_name) := 0]
}




## Analizamos correlación
numeric_cols <- dt[, .SD, .SDcols = sapply(dt, is.numeric)]
correlation_matrix <- cor(numeric_cols)

# Seteamos un límite arbitrario de correlación (definido luego de hacer algunos experimentos).
threshold <- 0.65

# Creamos matriz de correlación.
filtered_matrix <- correlation_matrix
filtered_matrix[abs(filtered_matrix) < threshold] <- 0

# Creamos un dataframe de la matriz de correlación.
correlation_df <- as.data.frame(as.table(filtered_matrix))
colnames(correlation_df) <- c("Variable1", "Variable2", "Correlation")

# Eliminamos las filas con correlación 1 y con correlación 0.
correlation_df <- correlation_df[correlation_df$Correlation != 0, ]
correlation_df <- correlation_df[correlation_df$Correlation != 1, ]
values_to_exclude <- c("foto_mes", "numero_de_cliente")
filtered_df <- subset(correlation_df, !(Variable1 %in% values_to_exclude | Variable2 %in% values_to_exclude))
filtered_df <- filtered_df[!duplicated(t(apply(filtered_df[c("Variable1", "Variable2")], 1, sort))), ]

# Estas son las variables que voy a tener que eliminar.
unique_values_variable1 <- unique(filtered_df$Variable1)
unique_values_variable2 <- unique(filtered_df$Variable2)
combined_unique_values <- c(unique_values_variable1, unique_values_variable2)
unique_combined_values <- unique(combined_unique_values)
print(unique_combined_values)
unique_combined_values <- as.character(unique_combined_values)




# Creamos la interacción entre las variables con correlación.

dt[, Visa_fechaalta___cliente_antiguedad := Visa_fechaalta*cliente_antiguedad]
dt[, mrentabilidad_annual___mrentabilidad := mrentabilidad_annual*mrentabilidad]
dt[, mpasivos_margen___mrentabilidad := mpasivos_margen*mrentabilidad]
dt[, mcomisiones_otras___mcomisiones := mcomisiones_otras*mcomisiones]
dt[, mcaja_ahorro___mpasivos_margen := mcaja_ahorro*mpasivos_margen]
dt[, mcuentas_saldo___mcaja_ahorro_dolares := mcuentas_saldo*mcaja_ahorro_dolares]
dt[, mautoservicio___ctarjeta_debito_transacciones := mautoservicio*ctarjeta_debito_transacciones]
dt[, mtarjeta_visa_consumo___ctarjeta_visa_transacciones := mtarjeta_visa_consumo*ctarjeta_visa_transacciones]
dt[, Visa_cconsumos___ctarjeta_visa_transacciones := Visa_cconsumos*ctarjeta_visa_transacciones]
dt[, Visa_msaldototal___mtarjeta_visa_consumo := Visa_msaldototal*mtarjeta_visa_consumo]
dt[, Visa_msaldopesos___mtarjeta_visa_consumo := Visa_msaldopesos*mtarjeta_visa_consumo]
dt[, Visa_mconsumospesos___mtarjeta_visa_consumo := Visa_mconsumospesos*mtarjeta_visa_consumo]
dt[, Visa_mpagospesos___mtarjeta_visa_consumo := Visa_mpagospesos*mtarjeta_visa_consumo]
dt[, Visa_mconsumototal___mtarjeta_visa_consumo := Visa_mconsumototal*mtarjeta_visa_consumo]
dt[, Visa_cconsumos___mtarjeta_visa_consumo := Visa_cconsumos*mtarjeta_visa_consumo]
dt[, Master_cconsumos___ctarjeta_master_transacciones := Master_cconsumos*ctarjeta_master_transacciones]
dt[, Master_msaldototal___mtarjeta_master_consumo := Master_msaldototal*mtarjeta_master_consumo]
dt[, Master_mconsumospesos___mtarjeta_master_consumo := Master_mconsumospesos*mtarjeta_master_consumo]
dt[, Master_mconsumototal___mtarjeta_master_consumo := Master_mconsumototal*mtarjeta_master_consumo]
dt[, mprestamos_hipotecarios___cprestamos_hipotecarios := mprestamos_hipotecarios*cprestamos_hipotecarios]
dt[, mttarjeta_visa_debitos_automaticos___ctarjeta_visa_debitos_automaticos := mttarjeta_visa_debitos_automaticos*ctarjeta_visa_debitos_automaticos]
dt[, mcomisiones_mantenimiento___ccomisiones_mantenimiento := mcomisiones_mantenimiento*ccomisiones_mantenimiento]
dt[, cforex_sell___cforex := cforex_sell*cforex]
dt[, mforex_sell___cforex := mforex_sell*cforex]
dt[, mforex_sell___cforex_sell := mforex_sell*cforex_sell]
dt[, mextraccion_autoservicio___cextraccion_autoservicio := mextraccion_autoservicio*cextraccion_autoservicio]
dt[, catm_trx___cextraccion_autoservicio := catm_trx*cextraccion_autoservicio]
dt[, matm___cextraccion_autoservicio := matm*cextraccion_autoservicio]
dt[, catm_trx___mextraccion_autoservicio := catm_trx*mextraccion_autoservicio]
dt[, matm___mextraccion_autoservicio := matm*mextraccion_autoservicio]
dt[, ccallcenter_transacciones___tcallcenter := ccallcenter_transacciones*tcallcenter]
dt[, matm___catm_trx := matm*catm_trx]
dt[, matm_other___catm_trx_other := matm_other*catm_trx_other]
dt[, Master_Finiciomora___Master_delinquency := Master_Finiciomora*Master_delinquency]
dt[, Master_msaldopesos___Master_msaldototal := Master_msaldopesos*Master_msaldototal]
dt[, Master_mpagominimo___Master_msaldototal := Master_mpagominimo*Master_msaldototal]
dt[, Master_mpagominimo___Master_msaldopesos := Master_mpagominimo*Master_msaldopesos]
dt[, Master_mconsumosdolares___Master_msaldodolares := Master_mconsumosdolares*Master_msaldodolares]
dt[, Visa_mlimitecompra___Master_mlimitecompra := Visa_mlimitecompra*Master_mlimitecompra]
dt[, Master_cadelantosefectivo___Master_madelantopesos := Master_cadelantosefectivo*Master_madelantopesos]
dt[, Visa_fechaalta___Master_fechaalta := Visa_fechaalta*Master_fechaalta]
dt[, Visa_msaldopesos___Visa_msaldototal := Visa_msaldopesos*Visa_msaldototal]
dt[, Visa_mconsumospesos___Visa_msaldototal := Visa_mconsumospesos*Visa_msaldototal]
dt[, Visa_mconsumototal___Visa_msaldototal := Visa_mconsumototal*Visa_msaldototal]
dt[, Visa_mpagominimo___Visa_msaldototal := Visa_mpagominimo*Visa_msaldototal]
dt[, Visa_mconsumospesos___Visa_msaldopesos := Visa_mconsumospesos*Visa_msaldopesos]
dt[, Visa_mconsumototal___Visa_msaldopesos := Visa_mconsumototal*Visa_msaldopesos]
dt[, Visa_mpagominimo___Visa_msaldopesos := Visa_mpagominimo*Visa_msaldopesos]
dt[, Visa_mconsumosdolares___Visa_msaldodolares := Visa_mconsumosdolares*Visa_msaldodolares]
dt[, Visa_cadelantosefectivo___Visa_madelantopesos := Visa_cadelantosefectivo*Visa_madelantopesos]

# Eliminamos las variables individuales
dt[, (unique_combined_values) := NULL]

   



## Creamos otras variables

# Rentabilidad / antiguedad
#dt[, rentabilidad_antiguedad := mrentabilidad_annual / cliente_antiguedad]

# Cantidad de tarjetas
dt[, cantidad_total_tarjetas := ctarjeta_visa + ctarjeta_master]

# Cantidad de transacciones con tarjetas
#dt[, cantidad_total_transacciones_tarjetas := ctarjeta_visa_transacciones + ctarjeta_master_transacciones]

# Monto consumo tarjetas
#dt[, monto_total_consumo_tarjetas := mtarjeta_visa_consumo + mtarjeta_master_consumo]

# cantidad de préstamos
#dt[, cantidad_total_prestamos := cprestamos_personales + cprestamos_prendarios + cprestamos_hipotecarios]

# monto de préstamos
#dt[, monto_total_prestamos := mprestamos_personales + mprestamos_prendarios + mprestamos_hipotecarios]

# Cantidad de seguros
dt[, cantidad_total_seguros := cseguro_vida + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales]

# Total debitos automáticos
#dt[, cantidad_total_tarjetas_debitos_automaticos := ctarjeta_visa_debitos_automaticos + ctarjeta_master_debitos_automaticos]

# Monto total debitos automáticos
#dt[, monto_total_tarjetas_debitos_automaticos := mttarjeta_visa_debitos_automaticos + mttarjeta_master_debitos_automaticos]

# Cantidad de tarjetas con descuento
dt[, cantidad_total_tarjetas_descuento:= ctarjeta_visa_descuentos  + ctarjeta_master_descuentos]

# Monto total de tarjetas con descuento
dt[, monto_total_tarjetas_descuento:= mtarjeta_visa_descuentos  + mtarjeta_master_descuentos]

# Cantidad total de comisiones que paga el cliente
#dt[, cantidad_total_comisiones_pagadas := ccomisiones_mantenimiento + ccomisiones_otras]

# Monto total de comisiones que paga el cliente
#dt[, monto_total_comisiones_pagadas := mcomisiones_mantenimiento + mcomisiones_otras]

# Total de transferencias (recibidas + emitidas)
dt[, cantidad_total_transferencias := ctransferencias_recibidas + ctransferencias_emitidas]

# Neto de transferencias (recibidas - emitidas)
dt[, monto_neto_transferencias := ctransferencias_recibidas - ctransferencias_emitidas]

# Ratio de transferencias (recibidas - emitidas)
dt[, ratio_transferencias := ifelse(ctransferencias_recibidas == 0 | ctransferencias_emitidas == 0, 0, ctransferencias_recibidas / ctransferencias_emitidas)]

# Monto total de financiacion
dt[, monto_total_financiacion := Master_mfinanciacion_limite + Visa_mfinanciacion_limite]

# Monto total de saldo
#dt[, monto_total_saldo := Master_msaldototal + Visa_msaldototal]

# Monto consumo total en pesos
#dt[, monto_total_consumo_pesos := Master_mconsumospesos + Visa_mconsumospesos]

# Monto consumo total en dolares
#dt[, monto_total_consumo_dolares := Master_mconsumosdolares + Visa_mconsumosdolares]

# Cantidad total de consumos
#dt[, cantidad_total_saldo := Master_cconsumos + Visa_cconsumos]

# tiene tarjeta visa y no la usa
#dt[, usa_tarjeta_visa := ifelse(ctarjeta_visa == 1 & ctarjeta_visa_transacciones == 0, 0, 
 #                               ifelse(ctarjeta_visa == 1 & ctarjeta_visa_transacciones > 0, 1, 0))]

# tiene tarjeta master y no la usa
#dt[, usa_tarjeta_master := ifelse(ctarjeta_master == 1 & ctarjeta_master_transacciones == 0, 0, 
 #                                 ifelse(ctarjeta_master == 1 & ctarjeta_master_transacciones > 0, 1, 0))]


# Lista de columnas a eliminar, ya que están ahora dentro de las nuevas creadas.
columns_to_remove <- c("ctarjeta_visa", "ctarjeta_master", 
                      # "ctarjeta_visa_transacciones", "ctarjeta_master_transacciones",
                      # "mtarjeta_visa_consumo", "mtarjeta_master_consumo",
                      # "cprestamos_personales", "cprestamos_prendarios", "cprestamos_hipotecarios",
                      # "mprestamos_personales", "mprestamos_prendarios", "mprestamos_hipotecarios",
                      # "ctarjeta_visa_debitos_automaticos", "ctarjeta_master_debitos_automaticos",
                      # "mttarjeta_visa_debitos_automaticos", "mttarjeta_master_debitos_automaticos",
                       "ctarjeta_visa_descuentos", "ctarjeta_master_descuentos",
                       "mtarjeta_visa_descuentos", "mtarjeta_master_descuentos",
                      # "ccomisiones_mantenimiento", "ccomisiones_otras",
                      # "mcomisiones_mantenimiento", "mcomisiones_otras",
                       "Master_mfinanciacion_limite", "Visa_mfinanciacion_limite"
                      # "Master_msaldototal", "Visa_msaldototal",
                      # "Master_mconsumospesos", "Visa_mconsumosdolares",
                      # "Master_cconsumos", "Visa_cconsumos"
                      )

# Eliminamos esas columnas.
dt[, (columns_to_remove) := NULL]



dataset <- as.data.frame(dt)


# Guardamos el dataframe a un CSV.
write.csv(dataset, file = "data_cleaned_04.csv")#, row.names = FALSE)
