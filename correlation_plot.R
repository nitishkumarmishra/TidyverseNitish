library(corrplot)
library("tidyr"); library(tidyverse); library(dplyr)
library("tidylog", warn.conflicts = FALSE)
library(RColorBrewer)

correlation <- readxl::read_xlsx("chr1_19.xlsx", col_names = TRUE)
correlation <- correlation %>%
  column_to_rownames(var = "Name")
correlation1 <- Matrix::forceSymmetric(as.matrix(correlation), uplo="L")
corrol <- as.matrix(correlation1)


corrplot(corrol, type = "lower")

col<- colorRampPalette(c("white", "blue", "red"))(10)
corrplot(corrol, type="upper", order="hclust", col=col, cl.lim = c(0.95, 1), is.corr = FALSE)

corrplot(corrol, type="lower", order="hclust", col=col, cl.lim = c(0.95, 1), is.corr = FALSE)

corrplot(corrol, order="hclust", col=col, cl.lim = c(0.95, 1), is.corr = FALSE)

col<- colorRampPalette(c("white", "blue", "red"))(20)
corrplot(corrol,method="color", col=col, cl.lim = c(0.95, 1), is.corr = FALSE)
#corrplot(corrol, type="upper", order="hclust", cl.lim = c(0.95, 1), col=brewer.pal(n=25, name="RdBu"), is.corr = FALSE)

## Heatmap
col<- colorRampPalette(c("blue", "white","red"))(10)
heatmap(x = corrol, col = col, symm = TRUE)

col<- colorRampPalette(c( "white","blue","red"))(20)
corrplot(corrol, type="lower", method = "pie", col=col, cl.lim = c(0.95, 1), is.corr = FALSE)

