require(data.table)
require(plyr)
require(dplyr)
require(tidyr)


## Geodata ----
if (!"region" %in% names(individDB$variables)) {
  geotbl <- fread("Data//coordinate.csv") %>% tbl_dt()
  setnames(geotbl, "city", "region")
  
  individDB[] <- geotbl %>% select(region)
}

geoTblRegion <- read.csv("Data/coordinate_Region.csv", head = TRUE, sep = ";")


## Fonddata ----
fnd_data <- tbl_df(data.frame(
  FNDID = fondDB['FNDID'][[1]],
  Fondnamn = fondDB['FONDNAMN', c(2014, 4)][[1]],
  Fondtyp = fondDB['FONDTYP', c(2014, 4)][[1]],
  Kategori_bred = fondDB['KATEGORI_BRED', c(2014, 4)][[1]],
  Kategori_smal = fondDB['KATEGORI_SMAL', c(2014, 4)][[1]]
))

base_id <- base_data %>%
  mutate(PEDALID = individDB['PEDALID'][[1]]) %>%
  
  # Join to cross table
  left_join(MV_2014_4, by = "PEDALID") %>%
  filter(!is.na(FNDID)) %>%
  
  # Join to fund table
  left_join(fnd_data, by = "FNDID")

