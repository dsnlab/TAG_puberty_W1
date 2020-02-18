# Prep stress/adversity variables for analysis
# T Cheng | Feb 18, 2020

# Conducts data cleaning and mean imputation in accordance with what is outlined in TWC's prereg entitled
# Preregistration: Dimensions relating the correspondence of pubertal hormones and physical maturation in early adolescent girls

packages = c("tidyr", "dplyr", "rio")

# load packages, install as needed
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE) }})

# data files
data_dir <- "/Volumes/psych-cog/dsnlab/TAG/behavior"
ctq_file <- "/Questionnaires/Wave1/CTQ_Wave1.csv"
pss_file <- "/Questionnaires/Wave1/PSS_Wave1.csv"
free_lunch_file <- "/Demographics/ParentQ_SES/TAG_W1_SES_ParentQ.xlsx"

# load ctq
ctq_df <- import(paste0(data_dir, ctq_file))
ctq_df <- ctq_df[order(ctq_df$tagid), ]# order by SID

ctq_items = list( # EA
  c("CTQ_3", #_calledstupid", 
    "CTQ_8", #_neverbeenborn", 
    "CTQ_14", #_familyinsulting",
    "CTQ_18", #_familyhatedme",
    "CTQ_25"), #_emotionallyabused"),
  # PA
  c("CTQ_17", #_beatenbadly",
    "CTQ_9", #_hithard",
    "CTQ_11", #_leftwithbruises",
    "CTQ_12", #_punishedwithbelt",
    "CTQ_15"), #_physicallyabused"),
  # SA
  c("CTQ_20", #_touchsexual",
    "CTQ_21", #_threatenedsexual",
    "CTQ_23", #_watchsexualthings",
    "CTQ_24", #_molested",
    "CTQ_27"), #_sexuallyabused"), 
  # EN
  c("CTQ_5", #_helpedfeelspecial",
    "CTQ_7", #_feltloved",
    "CTQ_13", #_familylookedout",
    "CTQ_19", #_familyclose",
    "CTQ_28"), #_familystrength"),
  # PN
  c("CTQ_1", #_notenoughtoeat",
    "CTQ_2", #_protectme",
    "CTQ_6", #_weardirtyclothes",
    "CTQ_26", #_takemetodoctor",
    "CTQ_4"), #_parentstoodrunk"),
  # Denial
  c("CTQ_10", #_nothingtochange",
    "CTQ_16", #_perfectchildhood",
    "CTQ_22")) #_bestfamily"))

subscale_info = list(scales = c("EA", "PA", "SA", "EN", "PN"), 
                     ctq_items = ctq_items)

ctq_SID <- ctq_df$tagid
ctq_df <- cbind(ctq_SID, ctq_df[, colnames(ctq_df) %in% unlist(ctq_items)])
colnames(ctq_df)[1] = "SID"

# items appear to have already been reverse-scored, with the exception of #28. i'll do so now. 
ctq_df$CTQ_28 = 6 - ctq_df$CTQ_28

#if >2 missing per subscale, then the value is missing; if <3 per subscale, then value is mean imputed for that subscale. then add entire scale. 
ctq_missing_vector <- list()
ctq_subscale_imputed <- list()

for (i in 1:length(subscale_info[[1]])){
  subscale = subscale_info[[1]][i]
  ctq_items = subscale_info[[2]][[i]]
  
  ctq_subscale_df <- cbind(ctq_SID, ctq_df[, colnames(ctq_df) %in% ctq_items])
  
  # count the number of nas
  num_ctq_nas <- ctq_subscale_df %>% 
    gather(., key = item, value = value, -ctq_SID) %>% 
    group_by(ctq_SID) %>% 
    summarise(total_ctq_na = sum(is.na(value)))
  
  # if the number of nas is 1 or 2, mean imput values
  ctq_na_vector <- c(which(num_ctq_nas$total_ctq_na == 1), which(num_ctq_nas$total_ctq_na == 2))
  ctq_na_vector <- ctq_na_vector[order(ctq_na_vector)]
  
  # calculate mean values
  mean_ctq_subscale <- ctq_subscale_df[ctq_na_vector, ] %>% 
    gather(., key = item, value = value, -ctq_SID) %>% 
    group_by(ctq_SID) %>% 
    summarise(mean = mean(value, na.rm = TRUE))
  
  for (j in 1:length(ctq_na_vector)) {
    idx = ctq_na_vector[j]
    ctq_subscale_df[idx, which(is.na(ctq_subscale_df[idx, ]))] <- mean_ctq_subscale[j, ]$mean # if index is na replace w/ mean 
  } # output looks weird bc some went out to two decimal places, but checked manually it works!
  
  ctq_subscale_imputed[[i]] <- ctq_subscale_df
  
  # if the number of nas is 3 or more, save the SID number to be censored
  ctq_missing_vector[i] <- c(which(num_ctq_nas$total_ctq_na > 2))
  
}

# reassemble ctq
ctq_df_mean_imputed <- as.data.frame.list(ctq_subscale_imputed)
ctq_df_mean_imputed <- ctq_df_mean_imputed[, !colnames(ctq_df_mean_imputed) %in% c("ctq_SID.1", "ctq_SID.2", "ctq_SID.3", "ctq_SID.4")]
temp <- ctq_df_mean_imputed %>% 
  gather(., key = item, value = value, -ctq_SID)

temp$value <- round(as.numeric(temp$value), 1)

temp <-  temp %>% 
  group_by(ctq_SID) %>% 
  mutate(ctq_sum = sum(value))

ctq_df_mean_imputed <- pivot_wider(temp, values_from = value, names_from = item)

# load and clean pss 
pss_df <- import(paste0(data_dir, pss_file))
pss_df <- pss_df[, c("tagid", "PSS_1", "PSS_2", "PSS_3", "PSS_4", "PSS_5", "PSS_6", "PSS_7", "PSS_8", "PSS_9", "PSS_10")]
colnames(pss_df)[1] = "SID"

# remove PSS duplicated row (TAG055)
pss_df <- pss_df[-which(duplicated(pss_df$SID)), ]

# get long dataframe
pss_df_long <- pss_df %>% 
  gather(., key = item, value = value, -SID)

# count the number of nas
num_pss_nas <- pss_df_long %>% 
  group_by(SID) %>% 
  summarise(total_pss_na = sum(is.na(value)))

# if the number of nas is 1, 2, or 3, mean imput values
pss_na_SID <- num_pss_nas$SID[which(num_pss_nas$total_pss_na > 0 & num_pss_nas$total_pss_na < 3)]

# calculate mean values
mean_pss <- pss_df %>% 
  filter(SID %in% pss_na_SID) %>% 
  gather(., key = item, value = value, -SID) %>% 
  group_by(SID) %>% 
  summarise(mean = mean(value, na.rm = TRUE))

# replace missing values with mean values
for (j in 1:length(pss_na_SID)) {
  temp_SID <- pss_na_SID[j]
  pss_df_long[which(pss_df_long$SID == temp_SID & is.na(pss_df_long$value)), ]$value <- mean_pss$mean[j] 
}

pss_df_mean_imputed <- pivot_wider(pss_df_long, id_cols = SID, names_from = item, values_from = value)
pss_df_mean_imputed$pss_sum = rowSums(pss_df_mean_imputed[, -1])

# if the number of nas is 3 or more, save the SID number to be censored
pss_missing_SID <- num_pss_nas$SID[which(num_pss_nas$total_pss_na > 3)]

# load and clean free lunch data
free_lunch_df <- import(paste0(data_dir, free_lunch_file))

# read in puberty data
puberty_EM <- read.csv("/Volumes/psych-cog/dsnlab/TAG/projects/W1_puberty_cca/puberty_df_EM_imputed1.csv")

# make mega-spreadsheet with relevant variables for 174 subjects 
temp <-  full_join(data.frame(SID = ctq_df_mean_imputed$ctq_SID, ctq_sum = ctq_df_mean_imputed$ctq_sum), data.frame(SID = pss_df_mean_imputed$SID, pss_sum = pss_df_mean_imputed$pss_sum))
temp2 <-  full_join(temp, data.frame(SID = free_lunch_df$TAG_ID, income = free_lunch_df$W1_Kiddo_Free_Lunch))
stress_df <- filter(temp2, SID %in% puberty_EM$SID)
#stress_df <- stress_df[order(stress_df$SID), ] # NEXT: re-order these, AND re-order puberty EM

full_SIDs <- data.frame(puberty_EM$SID)
colnames(full_SIDs) = "SID"

stress_df <- left_join(full_SIDs, stress_df)

saveRDS(stress_df, file = "/Volumes/psych-cog/dsnlab/TAG/projects/W1_puberty_cca/stress_df.rds")
