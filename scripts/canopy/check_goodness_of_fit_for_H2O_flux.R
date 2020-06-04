check_goodness_of_fit_for_H2O_flux <- function (canopyDF) {
    
    id.list <- unique(canopyDF$Identity)
    
    outDF <- data.frame(id.list, NA, NA, NA, NA)
    colnames(outDF) <- c("Identity", "slope", "intercept", "r2", "p_value")
    
    
    for (i in id.list) {
        subDF <- subset(canopyDF, Identity == i)
        
        test <- lm(gs~Norm_H2O_flux2, data=subDF)
        
        outDF$slope[outDF$Identity==i] <- coef(test)[2]
        outDF$intercept[outDF$Identity==i] <- coef(test)[1]
        
        outDF$p_value[outDF$Identity==i] <- anova(test)$'Pr(>F)'[1]
        outDF$r2[outDF$Identity==i] <- summary(test)$adj.r.squared
        
    }
    
    m.r2 <- round(mean(outDF$r2), 2)
    se.r2 <- round(se(outDF$r2), 2)
    
    print(paste0("chamber mean r2 = ", m.r2, ", se = ", se.r2))
    
}