# Clean PSS 
# The Score_Qualtrics.Rmd script produced a bad version of PSS_Wave1.csv
# I interrupted that script to save PSS_raw.RDS output without changing the script too much
# Here I'm editing and cleaning that output to get PSS scores 

library(rio)

PSS <- import("/Volumes/psych-cog/dsnlab/TAG/projects/W1_puberty_cca/PSS_raw.RDS") %>% 
  distinct(tagid, item, value, survey_name, .keep_all = FALSE) %>% # remove duplicates
  filter(!grepl('W2|W3', survey_name)) %>% # remove wave 2 or wave 3 data

#saveRDS(PSS, "/Volumes/psych-cog/dsnlab/TAG/projects/W1_puberty_cca/PSS_Wave1.RDS")


