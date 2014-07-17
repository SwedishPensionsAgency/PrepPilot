require(data.table)
require(plyr)
require(dplyr)
require(tidyr)


## Data
geotbl <- fread("Data//coordinate.csv") %>% tbl_dt()
setnames(geotbl, "city", "region")

individDB[] <- geotbl %>% select(region)
