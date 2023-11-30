# en este archivo:
# Tratamos los Ceros, y NAs: para los meses rotos, los reemplazamos por el valor promedio del mes previo y mes posterior.
# Un poco de feature engineering manual, creación de nuevas variables.
# Generamos tres lags.




# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

getwd()
setwd("/home/ezequieltokman/buckets/b1/datasets/Datasets/Competencia_03/")
getwd()
require("data.table")
require("dplyr")
library("ggplot2")

# cargamos los datos originales
dt <- fread("./competencia_03.csv.gz", stringsAsFactors = TRUE)

# ordenamos el dataset
dt <- dt[order(numero_de_cliente, foto_mes)]


### TRATAMOS LOS CEROS Y NAs.
# Luego de haber detectado las variables / meses con problemas, agrupamos por numero de cliente y reemplazamos todos los valores
# por el promedio del mes siguiente y el mes previo.


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


# active_quarter
setDT(dt)[, active_quarter := ifelse(foto_mes == "202006",
                                     (shift(active_quarter) + shift(active_quarter, type = "lead")) / 2,
                                     active_quarter),
          by = numero_de_cliente]


# cliente_vip
setDT(dt)[, cliente_vip := ifelse(foto_mes == "202006",
                                  (shift(cliente_vip) + shift(cliente_vip, type = "lead")) / 2,
                                  cliente_vip),
          by = numero_de_cliente]

# internet
setDT(dt)[, internet := ifelse(foto_mes == "202006",
                               (shift(internet) + shift(internet, type = "lead")) / 2,
                               internet),
          by = numero_de_cliente]

# mrentabilidad
setDT(dt)[, mrentabilidad := ifelse(foto_mes %in% c("201906", "201910", "202006"),
                                    (shift(mrentabilidad) + shift(mrentabilidad, type = "lead")) / 2,
                                    mrentabilidad),
          by = numero_de_cliente]

# mrentabilidad_annual
setDT(dt)[, mrentabilidad_annual := ifelse(foto_mes %in% c("201906", "201910", "202006"),
                                           (shift(mrentabilidad_annual) + shift(mrentabilidad_annual, type = "lead")) / 2,
                                           mrentabilidad_annual),
          by = numero_de_cliente]

# mcomisiones
setDT(dt)[, mcomisiones := ifelse(foto_mes %in% c("201906", "201910", "202006"),
                                  (shift(mcomisiones) + shift(mcomisiones, type = "lead")) / 2,
                                  mcomisiones),
          by = numero_de_cliente]

# mactivos_margen
setDT(dt)[, mactivos_margen := ifelse(foto_mes %in% c("201906", "201910", "202006"),
                                      (shift(mactivos_margen) + shift(mactivos_margen, type = "lead")) / 2,
                                      mactivos_margen),
          by = numero_de_cliente]

# mpasivos_margen
setDT(dt)[, mpasivos_margen := ifelse(foto_mes %in% c("201906", "201910", "202006"),
                                      (shift(mpasivos_margen) + shift(mpasivos_margen, type = "lead")) / 2,
                                      mpasivos_margen),
          by = numero_de_cliente]


# mcuentas_saldo
setDT(dt)[, mcuentas_saldo := ifelse(foto_mes == "202006",
                                     (shift(mcuentas_saldo) + shift(mcuentas_saldo, type = "lead")) / 2,
                                     mcuentas_saldo),
          by = numero_de_cliente]

# ctarjeta_debito_transacciones
setDT(dt)[, ctarjeta_debito_transacciones := ifelse(foto_mes == "202006",
                                                    (shift(ctarjeta_debito_transacciones) + shift(ctarjeta_debito_transacciones, type = "lead")) / 2,
                                                    ctarjeta_debito_transacciones),
          by = numero_de_cliente]

# mautoservicio
setDT(dt)[, mautoservicio := ifelse(foto_mes == "202006",
                                    (shift(mautoservicio) + shift(mautoservicio, type = "lead")) / 2,
                                    mautoservicio),
          by = numero_de_cliente]

# ctarjeta_visa_transacciones
setDT(dt)[, ctarjeta_visa_transacciones := ifelse(foto_mes == "202006",
                                                  (shift(ctarjeta_visa_transacciones) + shift(ctarjeta_visa_transacciones, type = "lead")) / 2,
                                                  ctarjeta_visa_transacciones),
          by = numero_de_cliente]

# mtarjeta_visa_consumo
setDT(dt)[, mtarjeta_visa_consumo := ifelse(foto_mes == "202006",
                                            (shift(mtarjeta_visa_consumo) + shift(mtarjeta_visa_consumo, type = "lead")) / 2,
                                            mtarjeta_visa_consumo),
          by = numero_de_cliente]

# ctarjeta_master_transacciones
setDT(dt)[, ctarjeta_master_transacciones := ifelse(foto_mes == "202006",
                                                    (shift(ctarjeta_master_transacciones) + shift(ctarjeta_master_transacciones, type = "lead")) / 2,
                                                    ctarjeta_master_transacciones),
          by = numero_de_cliente]

# mtarjeta_master_consumo
setDT(dt)[, mtarjeta_master_consumo := ifelse(foto_mes == "202006",
                                              (shift(mtarjeta_master_consumo) + shift(mtarjeta_master_consumo, type = "lead")) / 2,
                                              mtarjeta_master_consumo),
          by = numero_de_cliente]

# cprestamos_prendarios
setDT(dt)[, cprestamos_prendarios := ifelse(foto_mes == "202006",
                                            (shift(cprestamos_prendarios) + shift(cprestamos_prendarios, type = "lead")) / 2,
                                            cprestamos_prendarios),
          by = numero_de_cliente]

# mprestamos_prendarios
setDT(dt)[, mprestamos_prendarios := ifelse(foto_mes == "202006",
                                            (shift(mprestamos_prendarios) + shift(mprestamos_prendarios, type = "lead")) / 2,
                                            mprestamos_prendarios),
          by = numero_de_cliente]

# cprestamos_hipotecarios
setDT(dt)[, cprestamos_hipotecarios := ifelse(foto_mes == "202006",
                                              (shift(cprestamos_hipotecarios) + shift(cprestamos_hipotecarios, type = "lead")) / 2,
                                              cprestamos_hipotecarios),
          by = numero_de_cliente]

# mprestamos_hipotecarios
setDT(dt)[, mprestamos_hipotecarios := ifelse(foto_mes == "202006",
                                              (shift(mprestamos_hipotecarios) + shift(mprestamos_hipotecarios, type = "lead")) / 2,
                                              mprestamos_hipotecarios),
          by = numero_de_cliente]

# ctarjeta_visa_debitos_automaticos
setDT(dt)[, ctarjeta_visa_debitos_automaticos := ifelse(foto_mes == "201904",
                                                        (shift(ctarjeta_visa_debitos_automaticos) + shift(ctarjeta_visa_debitos_automaticos, type = "lead")) / 2,
                                                        ctarjeta_visa_debitos_automaticos),
          by = numero_de_cliente]

# mttarjeta_visa_debitos_automaticos
setDT(dt)[, mttarjeta_visa_debitos_automaticos := ifelse(foto_mes == "201904",
                                                        (shift(mttarjeta_visa_debitos_automaticos) + shift(mttarjeta_visa_debitos_automaticos, type = "lead")) / 2,
                                                        mttarjeta_visa_debitos_automaticos),
          by = numero_de_cliente]


# ccomisiones_otras
setDT(dt)[, ccomisiones_otras := ifelse(foto_mes %in% c("201904", "201910", "202006"),
                                        (shift(ccomisiones_otras) + shift(ccomisiones_otras, type = "lead")) / 2,
                                        ccomisiones_otras),
          by = numero_de_cliente]

# mcomisiones_otras
setDT(dt)[, mcomisiones_otras := ifelse(foto_mes %in% c("201904", "201910", "202006"),
                                        (shift(mcomisiones_otras) + shift(mcomisiones_otras, type = "lead")) / 2,
                                        mcomisiones_otras),
          by = numero_de_cliente]

# cextraccion_autoservicio
setDT(dt)[, cextraccion_autoservicio := ifelse(foto_mes == "202006",
                                               (shift(cextraccion_autoservicio) + shift(cextraccion_autoservicio, type = "lead")) / 2,
                                               cextraccion_autoservicio),
          by = numero_de_cliente]

# mextraccion_autoservicio
setDT(dt)[, mextraccion_autoservicio := ifelse(foto_mes == "202006",
                                               (shift(mextraccion_autoservicio) + shift(mextraccion_autoservicio, type = "lead")) / 2,
                                               mextraccion_autoservicio),
          by = numero_de_cliente]

# ccheques_depositados
setDT(dt)[, ccheques_depositados := ifelse(foto_mes == "202006",
                                           (shift(ccheques_depositados) + shift(ccheques_depositados, type = "lead")) / 2,
                                           ccheques_depositados),
          by = numero_de_cliente]

# mcheques_depositados
setDT(dt)[, mcheques_depositados := ifelse(foto_mes == "202006",
                                           (shift(mcheques_depositados) + shift(mcheques_depositados, type = "lead")) / 2,
                                           mcheques_depositados),
          by = numero_de_cliente]

# ccheques_emitidos
setDT(dt)[, ccheques_emitidos := ifelse(foto_mes == "202006",
                                        (shift(ccheques_emitidos) + shift(ccheques_emitidos, type = "lead")) / 2,
                                        ccheques_emitidos),
          by = numero_de_cliente]

# mcheques_emitidos
setDT(dt)[, mcheques_emitidos := ifelse(foto_mes == "202006",
                                        (shift(mcheques_emitidos) + shift(mcheques_emitidos, type = "lead")) / 2,
                                        mcheques_emitidos),
          by = numero_de_cliente]

# tcallcenter
setDT(dt)[, tcallcenter := ifelse(foto_mes == "202006",
                                  (shift(tcallcenter) + shift(tcallcenter, type = "lead")) / 2,
                                  tcallcenter),
          by = numero_de_cliente]

# ccallcenter_transacciones
setDT(dt)[, ccallcenter_transacciones := ifelse(foto_mes == "202006",
                                                (shift(ccallcenter_transacciones) + shift(ccallcenter_transacciones, type = "lead")) / 2,
                                                ccallcenter_transacciones),
          by = numero_de_cliente]

# thomebanking
setDT(dt)[, thomebanking := ifelse(foto_mes == "202006",
                                   (shift(thomebanking) + shift(thomebanking, type = "lead")) / 2,
                                   thomebanking),
          by = numero_de_cliente]

# chomebanking_transacciones
setDT(dt)[, chomebanking_transacciones := ifelse(foto_mes %in% c("201910", "202006"),
                                                 (shift(chomebanking_transacciones) + shift(chomebanking_transacciones, type = "lead")) / 2,
                                                 chomebanking_transacciones),
          by = numero_de_cliente]


# catm_trx
setDT(dt)[, catm_trx := ifelse(foto_mes == "202006",
                               (shift(catm_trx) + shift(catm_trx, type = "lead")) / 2,
                               catm_trx),
          by = numero_de_cliente]

# matm
setDT(dt)[, matm := ifelse(foto_mes == "202006",
                           (shift(matm) + shift(matm, type = "lead")) / 2,
                           matm),
          by = numero_de_cliente]

# catm_trx_other
setDT(dt)[, catm_trx_other := ifelse(foto_mes == "202006",
                                     (shift(catm_trx_other) + shift(catm_trx_other, type = "lead")) / 2,
                                     catm_trx_other),
          by = numero_de_cliente]

# matm_other
setDT(dt)[, matm_other := ifelse(foto_mes == "202006",
                                 (shift(matm_other) + shift(matm_other, type = "lead")) / 2,
                                 matm_other),
          by = numero_de_cliente]

# ctrx_quarter
setDT(dt)[, ctrx_quarter := ifelse(foto_mes == "202006",
                                   (shift(ctrx_quarter) + shift(ctrx_quarter, type = "lead")) / 2,
                                   ctrx_quarter),
          by = numero_de_cliente]

# tmobile_app
setDT(dt)[, tmobile_app := ifelse(foto_mes == "202006",
                                  (shift(tmobile_app) + shift(tmobile_app, type = "lead")) / 2,
                                  tmobile_app),
          by = numero_de_cliente]

# cmobile_app_trx
setDT(dt)[, cmobile_app_trx := ifelse(foto_mes == "202006",
                                      (shift(cmobile_app_trx) + shift(cmobile_app_trx, type = "lead")) / 2,
                                      cmobile_app_trx),
          by = numero_de_cliente]

# Master_mpagado
setDT(dt)[, Master_mpagado := ifelse(foto_mes == "202006",
                                     (shift(Master_mpagado) + shift(Master_mpagado, type = "lead")) / 2,
                                     Master_mpagado),
          by = numero_de_cliente]


# Visa_mpagado
setDT(dt)[, Visa_mpagado := ifelse(foto_mes == "202006",
                                   (shift(Visa_mpagado) + shift(Visa_mpagado, type = "lead")) / 2,
                                   Visa_mpagado),
          by = numero_de_cliente]




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
dt[, paste0(cols_to_shift, "_diff6") := lapply(.SD, function(col) (col - shift(col, type = "lag", n = 3, fill = col[1])) / shift(col, type = "lag", n = 3, fill = col[1])), 
   by = numero_de_cliente, .SDcols = cols_to_shift]


dt <- as.data.frame(dt)



output_file <- "df_cleaned_competencia03.csv.gz"
fwrite(dt, file = output_file, quote = FALSE, sep = ",")