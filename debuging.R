# Debugging notes and scripts

# Notes -----------------------------------------------------------------------------------------------------------


# Scripts ---------------------------------------------------------------------------------------------------------



# * PCA -----------------------------------------------------------------------------------------------------------
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


# * ttest ---------------------------------------------------------------------------------------------------------
library(ggsignif)

dev.off()
graphic

data

graphic+
    aes(x = sample)+
    facet_wrap(xval, scales = "free")+
    geom_signif(comparisons = list(c("01 - LD", "05 - WT")),
                test = "t.test",
                tip_length = 0.01,
                step_increase = .05)

data

checkGroup

advanced
