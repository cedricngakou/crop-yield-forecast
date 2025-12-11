
library(anytime)
library(lubridate)

d <- read.csv("~/carob1/data/compiled/carob_agronomy.csv")
meta <- read.csv("~/carob1/data/compiled/carob_agronomy_metadata.csv")
d[d==""] <- NA
d <- merge(d, meta[, c("dataset_id", "treatment_vars")], all.x = TRUE)

### selecting variables of interest (for ML project)
dd <- d[, c("dataset_id", "treatment_vars", "N_fertilizer", "P_fertilizer", "K_fertilizer","Zn_fertilizer","S_fertilizer","lime", "yield", "dmy_roots", "dmy_storage", "fwy_total","longitude", "latitude", "planting_date", "harvest_date", "rain", "temp", "crop", "country", "rep")]
soil <- d[, c("soil_pH", "soil_texture", "soil_sand", "soil_silt", "soil_clay", "soil_K", "soil_SOC", "soil_N", "soil_P_total", "soil_SOM")]
dd <- cbind(dd, soil)

dd <- dd[grepl("N_fertilizer;P_fertilize|K_fertilizer", dd$treatment_vars), ]

dd1 <- dd[!is.na(dd$dataset_id),]

### 
dd1$yield <- ifelse(is.na(dd1$yield) & !is.na(dd1$dmy_roots), dd1$dmy_roots,
                    ifelse(is.na(dd1$yield) & !is.na(dd1$dmy_storage), dd1$dmy_storage,
                           ifelse(is.na(dd1$yield) & !is.na(dd1$fwy_total), dd1$fwy_total, dd1$yield)))
#### Keep rows with yield value (key variables)
dd1 <- dd1[!is.na(dd1$yield),]

### keep rows with planting date (key variable)
dd1 <- dd1[!is.na(dd1$planting_date),]

### Selecting only the african country in the data 
country_SSA <- c("Nigeria", "Niger", "Mali", "Malawi", "Togo", "Côte d'Ivoire", "Ghana", "Uganda", "Democratic Republic of the Congo", "Senegal", "Zimbabwe", "Madagascar", "Botswana", "Gambia", "Burundi", "Guinea-Bissau", "Benin", "Sudan", "Tunisia", "Burkina Faso", "Egypt", "Zambia", "Mozambique", "Ethiopia", "Tanzania", 
                 "Kenya", "Lesotho")
ddf <- dd1[which(dd1$country %in% country_SSA), ]

ddf$treatment_vars <- ddf$dmy_roots <- ddf$dmy_storage <- ddf$rep <- ddf$lime <- ddf$soil_depth <- ddf$fwy_total <-  NULL

### subset maize crop 
df <- ddf[grepl("maize", ddf$crop),]

### Fixing the harvest date where it missing  
df$harvest_date <- ifelse(is.na(df$harvest_date)|df$harvest_date ==df$planting_date, paste0(substr(df$planting_date, 1, 4),"-12-31"), df$harvest_date)

### all the data should have lon and lat coordinate 
df <- df[!is.na(df$latitude),]
## 
df$planting_date <- anydate(df$planting_date)
df$harvest_date <- anydate(df$harvest_date)
### Keep only the data with the planting date  stating from 1980 
df1 <-  df[as.Date(df$planting_date) > "1980-01-01", ]
df1$harvest_date <- ifelse(df1$planting_date> df1$harvest_date, paste0(substr(df1$planting_date, 1, 4),"-12-31"), as.character(df1$harvest_date))

#write.csv(df1, "~/crop-yield-forecast/Data/carob_clean_ML.csv", row.names = FALSE)

