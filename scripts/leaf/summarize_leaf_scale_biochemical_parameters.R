summarize_leaf_scale_biochemical_parameters <- function() {
    
    
    ### inDF
    inDF <- read.csv("output/leaf/leaf_scale_parameters.csv", header=T)
    inDF <- subset(inDF, Chamber%in%c("1", "3", "11", "4", "8"))
    
    
    outDF <- summaryBy(Vcmax+Jmax+G1+GammaStar+Rd+JVratio+Ci_transition_Ac_Aj~CO2_treatment+Position,
                       FUN=c(mean, se), data=inDF,
                       na.rm=T, keep.names=T)
    
    write.csv(outDF, "output/leaf/leaf_scale_parameter_summary_table_for_two_leaf_model.csv",
              row.names=F)
    
}