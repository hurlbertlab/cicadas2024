color_df <- data.frame(
  site = c("Eno River State Park",
           "NC Botanical Garden",
           "Prairie Ridge Ecostation",
           "Triangle Land Conservancy - Johnston Mill Nature Preserve",
           "UNC Chapel Hill Campus"),
  Symbol = c(16, 17, 8, 15, 18),
  Color = c("#0072B2", "#D55E00", "black", "#CC79A7", "yellow3")
) %>%
  mutate(Name = case_when(
    site ==  "Eno River State Park" ~ "Eno River",
    site ==  "Triangle Land Conservancy - Johnston Mill Nature Preserve" ~ "Johnston Mill",
    site ==  "NC Botanical Garden" ~ "NCBG",
    site ==  "UNC Chapel Hill Campus" ~ "UNC Campus",
    site ==  "Prairie Ridge Ecostation" ~ "Prairie Ridge"
  ))
