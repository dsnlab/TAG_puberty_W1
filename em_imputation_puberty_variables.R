# Missing Data Procedures on Hormone Data
# T Cheng | 03.28.2019

# load packages, install as needed
packages = c("tidyr", "dplyr", "Amelia")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE) }})

hormone_mat_RDS <- readRDS("/Volumes/psych-cog/dsnlab/TAG/projects/W1_puberty_cca/hormone_df.RDS")
physical_mat_RDS <- readRDS("/Volumes/psych-cog/dsnlab/TAG/projects/W1_puberty_cca/physical_mat_df.RDS")

puberty_df <- full_join(hormone_mat_RDS, physical_mat_RDS, by = "SID")

a.outEM <- amelia(puberty_df, m = 1, 
                  idvars = c("SID"), 
                  noms = c("pds1_height", "pds2_hair", "pds3_skin", "pds4_breasts", "pds6_period", "pbip1_breasts", "pbip2_pubic_hair"), 
                  boot.type = "none")

setwd("/Volumes/psych-cog/dsnlab/TAG/projects/W1_puberty_cca/")
write.amelia(obj = a.outEM, file.stem = "puberty_df_EM_imputed")

puberty_EM <- read.csv("/Volumes/psych-cog/dsnlab/TAG/projects/W1_puberty_cca/puberty_df_EM_imputed1.csv")
