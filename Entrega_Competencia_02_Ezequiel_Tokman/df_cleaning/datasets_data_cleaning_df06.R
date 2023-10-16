# df_06_cleaned
# en este archivo:
# Tratamos los Ceros, y NAs.
# Un poco de feature engineering manual.
# Generamos tres lags.



# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

getwd()
setwd("/home/ezequieltokman/buckets/b1")
getwd()
require("data.table")
require("dplyr")
library("ggplot2")

# cargamos los datos originales
dt <- fread("./datasets/competencia_02.csv.gz", stringsAsFactors = TRUE)

# ordenamos el dataset
dt <- dt[order(numero_de_cliente, foto_mes)]


### NOS QUEDAMOS SÓLO CON LOS MESES QUE VAMOS A USAR EN EL MODELO Y EN SU TESTEO.

# Listamos los meses que queremos
#selected_months <- c(202101, 202102, 202103, 202104, 202105, 202107)

# Filtramos los meses que queremos
#dt <- dt[foto_mes %in% selected_months]


### TRATAMOS LOS CEROS Y NAs.
# Luego de haber detectado las variables / meses con problemas, agrupamos por numero de cliente y reemplazamos todos los valores
# por el promedio del mes siguiente y el mes previo.

# Creamos subset de validación
subset_dt <- dt[, .(numero_de_cliente, foto_mes,
                    ccajeros_propios_descuentos,
                    mcajeros_propios_descuentos, 
                    ctarjeta_visa_descuentos,
                    mtarjeta_visa_descuentos,
                    ctarjeta_master_descuentos,
                    mtarjeta_master_descuentos,
                    Master_fultimo_cierre, 
                    Visa_fultimo_cierre)]
                   
# ccajeros_propios_descuentos
setDT(dt)[, ccajeros_propios_descuentos := ifelse(foto_mes == "202102",
                                          (shift(ccajeros_propios_descuentos) + shift(ccajeros_propios_descuentos, type = "lead")) / 2,
                                          ccajeros_propios_descuentos),
          by = numero_de_cliente]

# mcajeros_propios_descuentos
setDT(dt)[, mcajeros_propios_descuentos := ifelse(foto_mes == "202102",
                                                  (shift(mcajeros_propios_descuentos) + shift(mcajeros_propios_descuentos, type = "lead")) / 2,
                                                  mcajeros_propios_descuentos),
          by = numero_de_cliente]

# ctarjeta_visa_descuentos
setDT(dt)[, ctarjeta_visa_descuentos := ifelse(foto_mes == "202102",
                                                  (shift(ctarjeta_visa_descuentos) + shift(ctarjeta_visa_descuentos, type = "lead")) / 2,
                                               ctarjeta_visa_descuentos),
          by = numero_de_cliente]

# mtarjeta_visa_descuentos
setDT(dt)[, mtarjeta_visa_descuentos := ifelse(foto_mes == "202102",
                                               (shift(mtarjeta_visa_descuentos) + shift(mtarjeta_visa_descuentos, type = "lead")) / 2,
                                               mtarjeta_visa_descuentos),
          by = numero_de_cliente]


# ctarjeta_master_descuentos
setDT(dt)[, ctarjeta_master_descuentos := ifelse(foto_mes == "202102",
                                               (shift(ctarjeta_master_descuentos) + shift(ctarjeta_master_descuentos, type = "lead")) / 2,
                                               ctarjeta_master_descuentos),
          by = numero_de_cliente]

# mtarjeta_master_descuentos
setDT(dt)[, mtarjeta_master_descuentos := ifelse(foto_mes == "202102",
                                                 (shift(mtarjeta_master_descuentos) + shift(mtarjeta_master_descuentos, type = "lead")) / 2,
                                                 mtarjeta_master_descuentos),
          by = numero_de_cliente]

# Master_fultimo_cierre
setDT(dt)[, Master_fultimo_cierre := ifelse(foto_mes == "202102",
                                                 (shift(Master_fultimo_cierre) + shift(Master_fultimo_cierre, type = "lead")) / 2,
                                            Master_fultimo_cierre),
          by = numero_de_cliente]

# Visa_fultimo_cierre
setDT(dt)[, Visa_fultimo_cierre := ifelse(foto_mes == "202102",
                                            (shift(Visa_fultimo_cierre) + shift(Visa_fultimo_cierre, type = "lead")) / 2,
                                          Visa_fultimo_cierre),
          by = numero_de_cliente]



# ccajas_depositos
setDT(dt)[, ccajas_depositos := ifelse(foto_mes == "202105",
                                                  (shift(ccajas_depositos) + shift(ccajas_depositos, type = "lead")) / 2,
                                       ccajas_depositos),
          by = numero_de_cliente]

# Master_mfinanciacion_limite
setDT(dt)[, Master_mfinanciacion_limite := ifelse(foto_mes == "202104",
                                       (shift(Master_mfinanciacion_limite) + shift(Master_mfinanciacion_limite, type = "lead")) / 2,
                                       Master_mfinanciacion_limite),
          by = numero_de_cliente]





# Creamos subset de validación
subset_dt_2 <- dt[, .(numero_de_cliente, foto_mes,
                    ccajeros_propios_descuentos,
                    mcajeros_propios_descuentos, 
                    ctarjeta_visa_descuentos,
                    mtarjeta_visa_descuentos,
                    ctarjeta_master_descuentos,
                    mtarjeta_master_descuentos,
                    Master_fultimo_cierre, 
                    Visa_fultimo_cierre,
                    Master_mfinanciacion_limite,
                    ccajas_depositos)]

# Template gráfico de validación

avg_data <- dt %>%
  group_by(foto_mes) %>%
  summarise(average_mtarjeta_master_descuentos = mean(mtarjeta_master_descuentos, na.rm = TRUE))

# Create a ggplot object
ggplot(avg_data, aes(x = foto_mes, y = average_mtarjeta_master_descuentos)) +
  geom_line() +
  labs(
    x = "foto_mes",
    y = "Average ccajeros_propios_descuentos",
    title = "Average ccajeros_propios_descuentos Over Time"
  )







### FEATURE ENGINEERING MANUAL

# Rentabilidad / antiguedad
dt[, rentabilidad_antiguedad := mrentabilidad_annual / cliente_antiguedad]

# Cantidad de tarjetas
dt[, cantidad_total_tarjetas := ctarjeta_visa + ctarjeta_master]

# Cantidad de transacciones con tarjetas
dt[, cantidad_total_transacciones_tarjetas := ctarjeta_visa_transacciones + ctarjeta_master_transacciones]

# Monto consumo tarjetas
dt[, monto_total_consumo_tarjetas := mtarjeta_visa_consumo + mtarjeta_master_consumo]

# cantidad de préstamos
dt[, cantidad_total_prestamos := cprestamos_personales + cprestamos_prendarios + cprestamos_hipotecarios]

# monto de préstamos
dt[, monto_total_prestamos := mprestamos_personales + mprestamos_prendarios + mprestamos_hipotecarios]

# Cantidad de seguros
dt[, cantidad_total_seguros := cseguro_vida + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales]

# Total debitos automáticos
dt[, cantidad_total_tarjetas_debitos_automaticos := ctarjeta_visa_debitos_automaticos + ctarjeta_master_debitos_automaticos]

# Monto total debitos automáticos
dt[, monto_total_tarjetas_debitos_automaticos := mttarjeta_visa_debitos_automaticos + mttarjeta_master_debitos_automaticos]

# Cantidad de tarjetas con descuento
dt[, cantidad_total_tarjetas_descuento:= ctarjeta_visa_descuentos  + ctarjeta_master_descuentos]

# Monto total de tarjetas con descuento
dt[, monto_total_tarjetas_descuento:= mtarjeta_visa_descuentos  + mtarjeta_master_descuentos]

# Cantidad total de comisiones que paga el cliente
dt[, cantidad_total_comisiones_pagadas := ccomisiones_mantenimiento + ccomisiones_otras]

# Monto total de comisiones que paga el cliente
dt[, monto_total_comisiones_pagadas := mcomisiones_mantenimiento + mcomisiones_otras]

# Total de transferencias (recibidas + emitidas)
dt[, cantidad_total_transferencias := ctransferencias_recibidas + ctransferencias_emitidas]

# Neto de transferencias (recibidas - emitidas)
dt[, monto_neto_transferencias := ctransferencias_recibidas - ctransferencias_emitidas]

# Ratio de transferencias (recibidas - emitidas)
dt[, ratio_transferencias := ifelse(ctransferencias_recibidas == 0 | ctransferencias_emitidas == 0, 0, ctransferencias_recibidas / ctransferencias_emitidas)]

# Monto total de financiacion
dt[, monto_total_financiacion := Master_mfinanciacion_limite + Visa_mfinanciacion_limite]

# Monto total de saldo
dt[, monto_total_saldo := Master_msaldototal + Visa_msaldototal]

# Monto consumo total en pesos
dt[, monto_total_consumo_pesos := Master_mconsumospesos + Visa_mconsumospesos]

# Monto consumo total en dolares
dt[, monto_total_consumo_dolares := Master_mconsumosdolares + Visa_mconsumosdolares]

# Cantidad total de consumos
dt[, cantidad_total_saldo := Master_cconsumos + Visa_cconsumos]

# tiene tarjeta visa y no la usa
dt[, usa_tarjeta_visa := ifelse(ctarjeta_visa == 1 & ctarjeta_visa_transacciones == 0, 0, 
                                ifelse(ctarjeta_visa == 1 & ctarjeta_visa_transacciones > 0, 1, 0))]

# tiene tarjeta master y no la usa
dt[, usa_tarjeta_master := ifelse(ctarjeta_master == 1 & ctarjeta_master_transacciones == 0, 0, 
                                  ifelse(ctarjeta_master == 1 & ctarjeta_master_transacciones > 0, 1, 0))]


# Lista de columnas a eliminar, ya que están ahora dentro de las nuevas creadas.
columns_to_remove <- c("ctarjeta_visa", "ctarjeta_master", 
                       "ctarjeta_visa_transacciones", "ctarjeta_master_transacciones",
                       "mtarjeta_visa_consumo", "mtarjeta_master_consumo",
                       "cprestamos_personales", "cprestamos_prendarios", "cprestamos_hipotecarios",
                       "mprestamos_personales", "mprestamos_prendarios", "mprestamos_hipotecarios",
                       "ctarjeta_visa_debitos_automaticos", "ctarjeta_master_debitos_automaticos",
                       "mttarjeta_visa_debitos_automaticos", "mttarjeta_master_debitos_automaticos",
                       "ctarjeta_visa_descuentos", "ctarjeta_master_descuentos",
                       "mtarjeta_visa_descuentos", "mtarjeta_master_descuentos",
                       "ccomisiones_mantenimiento", "ccomisiones_otras",
                       "mcomisiones_mantenimiento", "mcomisiones_otras",
                       "Master_mfinanciacion_limite", "Visa_mfinanciacion_limite",
                       "Master_msaldototal", "Visa_msaldototal",
                       "Master_mconsumospesos", "Visa_mconsumosdolares",
                       "Master_cconsumos", "Visa_cconsumos"
)

# Eliminamos esas columnas.
dt[, (columns_to_remove) := NULL]


# Definimos columnas a las cuales no le aplicamos transformaciones
cols_to_exclude <- c("numero_de_cliente", "foto_mes", "clase_ternaria")

# Variables no dummies 
non_dummy_cols <- names(dt)[!sapply(dt, function(col) all(col %in% c(0, 1)))]
cols_to_shift <- setdiff(non_dummy_cols, cols_to_exclude)


# diff lag 1
dt[, paste0(cols_to_shift, "_diff1") := lapply(.SD, function(col) (col - shift(col, type = "lag", fill = col[1])) / shift(col, type = "lag", fill = col[1])), 
   by = numero_de_cliente, .SDcols = cols_to_shift]

# diff lag 2
dt[, paste0(cols_to_shift, "_diff2") := lapply(.SD, function(col) (col - shift(col, type = "lag", n = 2, fill = col[1])) / shift(col, type = "lag", n = 2, fill = col[1])), 
   by = numero_de_cliente, .SDcols = cols_to_shift]

# diff lag 3
dt[, paste0(cols_to_shift, "_diff3") := lapply(.SD, function(col) (col - shift(col, type = "lag", n = 3, fill = col[1])) / shift(col, type = "lag", n = 3, fill = col[1])), 
   by = numero_de_cliente, .SDcols = cols_to_shift]


dt <- as.data.frame(dt)



output_file <- "df_cleaned_06.csv.gz"
fwrite(dt, file = output_file, quote = FALSE, sep = ",")
