## Explore the datasets in Fk-Prep
require(ggvis)


names(base_id)

base_id %>%
  ggvis(x = ~Kon, y = ~Alder) %>%
  layer_bars()