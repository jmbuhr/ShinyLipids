# Debugging notes and scripts

# Notes -----------------------------------------------------------------------------------------------------------


# Scripts ---------------------------------------------------------------------------------------------------------

input
input %>% names()

dev.off()
names(df)
df

df$lipid %>% unique() %>% length()
df$chains %>% unique() %>% length()
df$chain_sums %>% unique() %>% length()
df$length %>% unique() %>% length()


df <- df %>% ungroup()

df %>% count(class, length, sample_replicate) %>% filter(n == 2)


# old preparaData -------------------------------------------------------------------------------------------------

rawdata




# Loading data




# * PCA -----------------------------------------------------------------------------------------------------------



