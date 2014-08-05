require(data.table)
require(plyr)
require(dplyr)
require(tidyr)


## Geodata ----
if (!"region" %in% names(individDB$variables)) {
  geotbl <- fread("Data//coordinate.csv") %>% tbl_dt()
  individDB[] <- geotbl %>% select(region)
}

