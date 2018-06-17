# Debugging notes and scripts

# Notes -----------------------------------------------------------------------------------------------------------


# Scripts ---------------------------------------------------------------------------------------------------------

dev.off()
graphic

data
data[!duplicated(data[c("sample", "xval")]), ] %>% anti_join(data,.)
data[!duplicated(data[c("sample", "xval")]), ]

pca.results %>% str()

pca.results %>% plot()


ggbiplot(pca.results)

autoplot(pca.results)

autoplot(pca.results, which="biplot")

pc <- pca.results

df <- merge(scores(pc),data.obj, by=0)

ggplot(df, aes(PC1, PC2)) +
    geom_point() +
    xlab(paste("PC1", pc@R2[1] * 100, "% of variance")) +
    ylab(paste("PC2", pc@R2[2] * 100, "% of variance"))+
    .theme


scores %>% as_tibble(rownames = "sample")
loadings %>% as_tibble(rownames = "class")
eigenvalues %>% as_tibble(rownames = "PC")

lev
