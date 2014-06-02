source("demoData.R")
tmp <- list()
length(tmp) <- length(symbols)
for (i in 1:length(symbols)) {
  tmp[[i]] <-Cl(get(symbols[i]))
}
tmp <- do.call(cbind, tmp)
baseCors <- cor(tmp)
diag(baseCors) <- NA
instrumentAverageBaseCors <- rowMeans(baseCors, na.rm=TRUE)
names(instrumentAverageBaseCors) <- gsub(".Close", "", names(instrumentAverageBaseCors))
instrumentAverageBaseCors
(grandMeanBaseCors <- mean(instrumentAverageBaseCors))