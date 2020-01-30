# Features that can serve as aesthetics (visual mappings, short aes) in plots
features <- c(
  "",
  "value",
  "Sample"                     = "sample",
  "Sample replicate"           = "sample_replicate",
  "Sample replicate technical" = "sample_replicate_technical",
  "Class"                      = "class",
  "Lipid species"              = "lipid",
  "Category"                   = "category",
  "Functional category"        = "func_cat",
  "Double bonds"               = "db",
  "Hydroxylation state"        = "oh",
  "Chain Length"               = "length",
  "Chains"                     = "chains",
  "Chain sums"                 = "chain_sums"
)

# Global theme definition to add to ggplots
mainTheme <- list(
  theme_minimal(),
  theme(
    axis.line        = element_line(colour = "grey70", size = .75),
    text             = element_text(
      color          = "black",
      face           = "bold",
      family         = "sans"
    ),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    plot.background  = element_blank(),
    legend.position  = "bottom",
    panel.background = element_rect(color = "grey70", fill = NA, size = 1),
    strip.background = element_rect(fill = "grey80", color = "black"),
    strip.text       = element_text(color = "black")
  )
)



