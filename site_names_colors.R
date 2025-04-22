#made dataframe for NoisePredation Colors
color_df <- data.frame(
  Name = c("Eno River", "NCBG", "Prairie Ridge", "Johnston Mill", "UNC Campus"),
  Symbol = c(16, 17, 8, 15, 18),
  Color = c("#0072B2", "#D55E00", "black", "#CC79A7", "yellow3")
)


#make dataframe with the plotting parameters.
site_colors <- c("#0072B2", "#D55E00", "black", "#CC79A7", "yellow3")
site_shapes <- c(16, 17, 8, 15, 18)
names(site_colors) <- unique(fracdiff$site)
names(site_shapes) <- unique(fracdiff$site)
plot_params <- data.frame(site_colors, site_shapes) %>%
  mutate(site = rownames(.))
fracdiff <- left_join(fracdiff, plot_params, by = "site")