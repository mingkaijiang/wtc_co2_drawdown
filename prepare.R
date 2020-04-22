
#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

if(!dir.exists("output/leaf")) {
    dir.create("output/leaf", showWarnings = FALSE)
}

if(!dir.exists("output/canopy")) {
    dir.create("output/canopy", showWarnings = FALSE)
}

if(!dir.exists("output/A-Ca")) {
    dir.create("output/A-Ca", showWarnings = FALSE)
}

#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr, 
               doBy, 
               lubridate,
               ggplot2,
               viridis,
               sciplot,
               scales,
               data.table,
               plantecophys,
               stringr,        # to subtract characters
               lme4,
               cowplot,
               nlme,
               gridExtra,
               ggthemes,
               lmerTest,
               multcomp)    


#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("scripts", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)



