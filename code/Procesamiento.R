
# Cargar librerías e importar datos ---------------------------------------

library(readxl)    # Leer archivos Excel (.xls y .xlsx)
library(lubridate) # Manipular fechas y horas fácilmente
library(hms)       # Representar tiempos del día (horas:minutos:segundos)
library(tidyverse) # Conjunto de paquetes para manipulación, análisis y visualización de datos
library(writexl)   # Escribir data frames en archivos Excel (.xlsx)

df <- read_excel("data/Base de datos vertederos.xlsx",
                 sheet = "Vertederos") %>% 
  select(1:6)

str(df)


# Procesamiento de las fechas y horas -------------------------------------

df <- df %>%
  mutate(
    # Extraer la hora del día como objeto hms
    Time = hms::as_hms(Time - hours(5)),
    # Convertir Date a formato Date (si aún no lo es)
    Date = as_date(Date)
  )

str(df)


# Filtrar la base de datos por ubicación (Location) -----------------------

valores_unicos <- unique(df$Location)

lista_dfs <- valores_unicos %>%
  set_names() %>%
  map(~ df %>% filter(Location == .x))


# Asignación de códigos estandarizados a la columna Location --------------

location_codes <- c(
  "V01" = "W01AS_RAS_UTM",
  "V02" = "W02AS_FOR_UTM",
  "V03" = "W03AS_MOS_UTM",
  "V04" = "W04AS_TEK_UTM",
  "V05" = "W05AS_COF_UTM",
  "V06" = "W06AS_NAT_UTM",
  "V07" = "W07AS_SEC_UTM",
  "V08" = "W08AS_TKU_UTM",
  "V09" = "W09AS_PAS_UTM",
  "V10" = "W10AS_SAC_UTM",
  "V11" = "W11AS_CTD_UTM",
  "V12" = "W12AS_CUT_UTM",
  "V13" = "W13AS_ARN_UTM"
)

lista_dfs <- map2(lista_dfs, names(lista_dfs), ~ {
  .x %>%
    mutate(
      Location = location_codes[.y]
    )
})


# Asignación de códigos estandarizados a la columna SubLocation -----------

lista_dfs <- map2(lista_dfs, names(lista_dfs), ~ {
  .x %>% 
    mutate(SubLocation = case_when(
      SubLocation == "H" ~ paste0(.y, "_ASH"),
      SubLocation == "L" ~ paste0(.y, "_ASL"),
      TRUE ~ SubLocation
    ))
})


# Exportar datos  ---------------------------------------------------------

walk2(
  .x = lista_dfs,
  .y = names(lista_dfs),
  .f = ~ write_xlsx(.x, 
                    path = file.path("data/2013/", paste0(.y, ".xlsx")))
)

