library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinythemes)
library(lubridate)
#library(shinydashboard)
library(plotly)
library(ggplot2)
#library(pracma)
library(stringr)
library(fastDummies)
library(readxl)
library(data.table)
library(forcats)

# DATA =========================================================================

#get data
read_excel_filename <- function(filename){
  d <- read_excel(filename)
  d$source <- filename
  return(d)
}

# define function that loops through data folders to get data
read_excel_files <- function(path, type_char) {
  files <- list.files(path=path, pattern="*.xlsx", full.names = TRUE)
  s <- rbindlist(lapply(files, read_excel_filename),
                 use.names = TRUE, fill = TRUE)
  s$year <- str_extract(s$source, "([0-9]{4})")
  s$quarter <- str_extract(s$source, "([0-4]{1}q)")
  s$yearq <- paste(s$year, "-", str_extract(s$quarter, "^([0-4]{1})"))
  assign(paste("s", type_char, "_all", sep = ""), s, envir = globalenv())
}

read_excel_files('data/school', 's')
read_excel_files('data/pct', 'pct')

##### Cleaning ---------------------------------------------------------------

#import school shapefile - locations
schools_loc <- read_sf('data/public_schools_points/Public_Schools_Points_2011-2012A.shp')

#clean boroughs
schools_loc$City <- toupper(schools_loc$City)

schools_loc <- schools_loc %>% 
  mutate(City = if_else((City == 'ROCKAWAY PARK' | City =='JAMAICA' | City =='LONG ISLAND CITY' | City == 'JACKSON HEIGHTS'), "QUEENS", 
                        if_else((City == 'NEW YORK'), "MANHATTAN",
                                City))) %>% 
  filter(City != "HEMPSTEAD" & City != "YONKERS", City != "HOLLIS")

schools_loc$City <- as.factor(schools_loc$City)

# separate borough and campus name in school safety dfs
ss_all$borough <- sapply(str_extract_all(ss_all$`School Campus`, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"), paste0, collapse =",")
ss_all$campus <- gsub("\\([^)(]+\\)", "", ss_all$`School Campus`) 

#make all upper
ss_all$campus <- toupper(ss_all$campus)
ss_all$borough <- toupper(ss_all$borough)

ss_all$borough <- as.factor(ss_all$borough)
ss_all$borough <- as.character(ss_all$borough)

ss_all <- ss_all %>% 
  mutate(borough2 = if_else((borough == 'XW' | borough =='BX' | borough =='XE'), "BRONX", 
                            if_else((borough == 'BN' | borough == 'BKLYN' | borough == 'BS' | borough == 'BS-K722' | borough == 'GLOBAL STUDIES-BS' | borough =='UPK695K'), "BROOKLYN",
                                    if_else((borough == 'MN' | borough == 'MS' | borough == 'MANHATAN' | borough == 'MANSOUTH' | borough == 'MLK EDUCATIONAL CAMPUS' | borough =='02M933'), "MANHATTAN",
                                            if_else((borough == 'SI' | borough == 'R070'), "STATEN ISLAND",
                                                    if_else((borough == 'QN' | borough =='QS' | borough == 'MS 379Q' | borough == 'Q392' | borough == 'IS/PS499' | borough == 'OLD PS 182Q' | borough == 'OLD PS 182Q - QNS' | borough == 'OLD PS182Q-QNS' | borough == 'QUEENS-FORMER:QNS VOC & TECH HS'), "QUEENS", borough))))))

ss_all$borough2 <- as.factor(ss_all$borough2)

check_boroughs <- ss_all %>% 
  group_by(campus,borough2) %>% 
  summarise(n=n())

check_boroughs %>% 
  group_by(campus) %>% 
  summarise(n=n()) %>% 
  filter(n>1)

ss_all$Restraints <- as.factor(ss_all$Restraints)

# remove punctuation from school location file
schools_loc$SCHOOLNAME <- str_replace_all(schools_loc$SCHOOLNAME, "[[:punct:]]", "")

#make a new school name column where all special characters and "and" and "the" are removed
schools_loc$SCHOOLNAME2 <- gsub("[^A-z0-9 ]", "", schools_loc$SCHOOLNAME)
check_boroughs$campus2 <- gsub("[^A-z0-9 ]", "", check_boroughs$campus)

schools_loc$SCHOOLNAME2 <- gsub(" THE ", " ", schools_loc$SCHOOLNAME2)
check_boroughs$campus2 <- gsub(" THE ", " ", check_boroughs$campus2)

schools_loc$SCHOOLNAME2 <- gsub("^THE ", " ", schools_loc$SCHOOLNAME2)
check_boroughs$campus2 <- gsub("^THE ", " ", check_boroughs$campus2)

schools_loc$SCHOOLNAME2 <- gsub(" AND ", " ", schools_loc$SCHOOLNAME2)
check_boroughs$campus2 <- gsub(" AND ", " ", check_boroughs$campus2)


# fuzzy merge by school name
for(i in 1:dim(check_boroughs)[1]) {
  x <- agrep(check_boroughs$campus2[i], schools_loc$SCHOOLNAME2,
             ignore.case=TRUE, value=TRUE,
             max.distance = 0.05, useBytes = TRUE)
  x <- paste0(x,"")
  check_boroughs$school[i] <- x
} 


#gets PS preceded by space
check_boroughs$ps_inside <- str_extract(check_boroughs$campus2,  "\\s+(PS)+\\s+(\\d+)")

#gets PS at beginning of string
check_boroughs$ps_beginning <- str_extract(check_boroughs$campus2,  "^(PS)+\\s+(\\d+)")

check_boroughs <- check_boroughs %>% 
  mutate(ps = if_else(is.na(ps_beginning), ps_inside, ps_beginning))

check_boroughs$ps <- trimws(check_boroughs$ps)

check_boroughs$ps_num = gsub("^(PS)+\\s", "", check_boroughs$ps)
check_boroughs$ps_num <- str_pad(check_boroughs$ps_num, 3, pad = "0")
check_boroughs <- check_boroughs %>% 
  mutate(ps = if_else(is.na(ps), ps, paste("PS", ps_num)))


#gets IS preceded by space
check_boroughs$is_inside <- str_extract(check_boroughs$campus2,  "\\s+(IS)+\\s+(\\d+)")

#gets IS at beginning of string
check_boroughs$is_beginning <- str_extract(check_boroughs$campus2,  "^(IS)+\\s+(\\d+)")

check_boroughs <- check_boroughs %>% 
  mutate(is = if_else(is.na(is_beginning), is_inside, is_beginning))

check_boroughs$is <- trimws(check_boroughs$is)

check_boroughs$is_num = gsub("^(IS)+\\s", "", check_boroughs$is)
check_boroughs$is_num <- str_pad(check_boroughs$is_num, 3, pad = "0")
check_boroughs <- check_boroughs %>% 
  mutate(is = if_else(is.na(is), is, paste("IS", is_num)))




#SCHOOLS_LOC: gets PS preceded by space
schools_loc$ps_inside <- str_extract(schools_loc$SCHOOLNAME2,  "\\s+(PS)+\\s+(\\d+)")

#SCHOOLS_LOC: gets PS at beginning of string
schools_loc$ps_beginning <- str_extract(schools_loc$SCHOOLNAME2,  "^(PS)+\\s+(\\d+)")

schools_loc <- schools_loc %>% 
  mutate(ps = if_else(is.na(ps_beginning), ps_inside, ps_beginning))

schools_loc$ps <- trimws(schools_loc$ps)

schools_loc$ps_num = gsub("^(PS)+\\s", "", schools_loc$ps)
schools_loc$ps_num <- str_pad(schools_loc$ps_num, 3, pad = "0")
schools_loc <- schools_loc %>% 
  mutate(ps = if_else(is.na(ps), ps, paste("PS", ps_num)))



#SCHOOLS_LOC: gets IS preceded by space
schools_loc$is_inside <- str_extract(schools_loc$SCHOOLNAME2,  "\\s+(IS)+\\s+(\\d+)")

#SCHOOLS_LOC: gets IS at beginning of string
schools_loc$is_beginning <- str_extract(schools_loc$SCHOOLNAME2,  "^(IS)+\\s+(\\d+)")

schools_loc <- schools_loc %>% 
  mutate(is = if_else(is.na(is_beginning), is_inside, is_beginning))

schools_loc$is <- trimws(schools_loc$is)

schools_loc$is_num = gsub("^(IS)+\\s", "", schools_loc$is)
schools_loc$is_num <- str_pad(schools_loc$is_num, 3, pad = "0")
schools_loc <- schools_loc %>% 
  mutate(is = if_else(is.na(is), is, paste("IS", is_num)))


###make new dataframes for matching
schools_loc_match <- schools_loc %>% 
  select(SCHOOLNAME, ATS_CODE, SCHOOLNAME2, City, ps, is) %>% 
  mutate(ps_is = if_else(is.na(ps), is, ps)) %>% 
  filter(!is.na(ps_is)) %>% 
  select(-ps, -is)

check_boroughs_match <- check_boroughs %>% 
  select(campus, campus2, borough2, ps, is) %>% 
  mutate(ps_is = if_else(is.na(ps), is, ps)) %>% 
  filter(!is.na(ps_is)) %>% 
  select(-ps, -is)

match_ps_is <- left_join(check_boroughs_match, 
                         schools_loc_match, 
                         by=c('ps_is'='ps_is', 'borough2'='City'))

rm(schools_loc_match)
rm(check_boroughs_match)



#join correct ps/is matches to the fuzzy merged matches: 
match_ps_is2 <- match_ps_is %>% 
  select(campus, borough2, SCHOOLNAME, ps_is)

rm(match_ps_is)

check_boroughs <- left_join(check_boroughs, 
                            match_ps_is2, 
                            by=c('campus'="campus", 'borough2'="borough2"))

rm(match_ps_is2)


check_boroughs_ps_is <- check_boroughs %>% 
  filter(!is.na(ps_is))

#get new school column
check_boroughs_ps_is <- check_boroughs_ps_is %>% 
  mutate(school_new = SCHOOLNAME)


check_boroughs <- check_boroughs %>% 
  filter(is.na(ps_is))

#get new school column
check_boroughs <- check_boroughs %>% 
  mutate(school_new = school) 


check_boroughs <- rbind(check_boroughs, check_boroughs_ps_is)

rm(check_boroughs_ps_is)

### replace corrected school names
schools_corrected <- read.csv("data/schools_corrected_manual.csv")

check_boroughs <- check_boroughs %>% 
  left_join(schools_corrected,  by=c('campus'="campus", 'borough2'="borough2"))

rm(schools_corrected)

check_boroughs <- check_boroughs %>% 
  mutate(school_final = if_else(!is.na(school_corrected), 
                                school_corrected, 
                                school_new)) %>% 
  select(-ps_inside, -ps_beginning, -ps, -ps_num, 
         -is_inside, -is_beginning, -is, -is_num, 
         -ps_is)

#prepare strings in SCHOOLNAME with same cleaning 
check_boroughs$school_final2 <- gsub("[^A-z0-9 ]", "", check_boroughs$school_final)
check_boroughs$school_final2 <- gsub(" THE ", " ", check_boroughs$school_final2)
check_boroughs$school_final2 <- gsub("^THE ", " ", check_boroughs$school_final2)
check_boroughs$school_final2 <- gsub(" AND ", " ", check_boroughs$school_final2)

check_merge <- check_boroughs %>% 
  left_join(schools_loc, by=c('school_final2'='SCHOOLNAME2', 'borough2'='City'))

rm(check_boroughs)

ss_all_merged <- ss_all %>% 
  left_join(check_merge, by='campus')

ss_all_merged$`Intervention Type` <- as.factor(ss_all_merged$`Intervention Type`)
ss_all_merged$school <- as.factor(ss_all_merged$school)

ss_all_merged$school <- as.factor(ss_all_merged$school)
# ss_all_merged["school"][ss_all_merged["school"] == ''] <- NA
ss_all_merged <- ss_all_merged %>% mutate_at(c('school'), ~na_if(., ''))

# get dummies
ss_all_merged <- dummy_cols(ss_all_merged, select_columns = "Intervention Type")
ss_all_merged <- dummy_cols(ss_all_merged, select_columns = "Restraints")
ss_all_merged <- dummy_cols(ss_all_merged, select_columns = "Incident Location")
ss_all_merged <- dummy_cols(ss_all_merged, select_columns = "School Related")
ss_all_merged <- dummy_cols(ss_all_merged, select_columns = "Force")

#clean boroughs
ss_all_merged <- ss_all_merged %>% 
  select(-borough2.y, -borough) %>% 
  rename(borough = borough2.x) 

# get summary file for mapping
ss_map <- ss_all_merged %>% 
  filter(!is.na(ADDRESS)) %>% 
  group_by(school_final2, ADDRESS, ZIP, ATS_CODE, borough, GRADES, campus, year,geometry) %>% 
  summarise(total = n(), 
            count_arrests = sum(`Intervention Type_Arrested`),
            count_crisis = sum(`Intervention Type_Child in Crisis`),
            count_jvreport = sum(`Intervention Type_Juvenile Report`),
            count_mitigate = sum(`Intervention Type_Mitigated`),
            count_warrant = sum(`Intervention Type_PINS / Warrant`),
            count_summons = sum(`Intervention Type_Summons`),
            count_metal = sum(`Restraints_Metal`),
            count_velcro = sum(`Restraints_Velcro`),
            count_norestraint = sum(`Restraints_No Restraints`),
            count_offsite = sum(`Incident Location_Off-Site`),
            count_onsite = sum(`Incident Location_On-Site`),
            count_unrelated = sum(`School Related_N`),
            count_related = sum(`School Related_Y`)) %>% 
  rename(school = school_final2)


# get summary file for all years
ss_map_all <- ss_all_merged %>% 
  filter(!is.na(ADDRESS)) %>% 
  group_by(school_final2, ADDRESS, ZIP, ATS_CODE, borough, GRADES, campus, geometry) %>% 
  summarise(total = n(), 
            count_arrests = sum(`Intervention Type_Arrested`),
            count_crisis = sum(`Intervention Type_Child in Crisis`),
            count_jvreport = sum(`Intervention Type_Juvenile Report`),
            count_mitigate = sum(`Intervention Type_Mitigated`),
            count_warrant = sum(`Intervention Type_PINS / Warrant`),
            count_summons = sum(`Intervention Type_Summons`),
            count_metal = sum(`Restraints_Metal`),
            count_velcro = sum(`Restraints_Velcro`),
            count_norestraint = sum(`Restraints_No Restraints`),
            count_offsite = sum(`Incident Location_Off-Site`),
            count_onsite = sum(`Incident Location_On-Site`),
            count_unrelated = sum(`School Related_N`),
            count_related = sum(`School Related_Y`)) %>% 
  mutate(year = "All")  %>% 
  rename(school = school_final2)

ss_map <- rbind(ss_map, ss_map_all)

rm(ss_map_all)


#get latitude and longitdue points from geometry feature

coords <- schools_loc %>%
  st_transform(4326) %>%
  st_coordinates() %>%
  as.data.frame()

# merge coordinates to school file
schools_loc <- cbind(schools_loc, coords)

rm(coords)


schools_coords <- schools_loc %>% 
  select(SCHOOLNAME,X,Y) %>% 
  as.data.frame()

rm(schools_loc)

ss_map <- left_join(ss_map, schools_coords, by = c('school' = 'SCHOOLNAME'))

#turn into sf
ss_map <- st_as_sf(ss_map)

# turn ZIP, schools, and year into factors
ss_map$ZIP <- as.factor(ss_map$ZIP)
ss_map$school <- as.factor(ss_map$school)
ss_map$year <- as.factor(ss_map$year)


ss_map$borough <- str_to_title(ss_map$borough)
ss_map$school <- str_to_title(ss_map$school)
ss_map$school <- as.factor(ss_map$school)

#clear dataframes

##### Clean Precinct ------------------------------------------------------
#create age buckets in precinct data
spct_all <- spct_all %>% 
  mutate(age_buckets = as.factor(if_else(Age <=5, "Under 5", 
                                         if_else(Age > 5 & Age <=8, "6-8",
                                                 if_else(Age > 8 & Age <=11, "9-11",
                                                         if_else(Age > 11 & Age <= 14, "12-14",
                                                                 if_else(Age > 14 & Age <=15, "14-15",
                                                                         if_else(Age > 15 & Age <=17, "16-17",
                                                                                 if_else(Age > 17 & Age <=20, "18-20",
                                                                                         if_else(Age > 20, "20+", "Check"))))))))))



spct_all <- spct_all %>% 
  mutate(age_buckets = fct_relevel(age_buckets, 
                                   "Under 5", "6-8", "9-11", 
                                   "12-14", "14-15", "16-17", 
                                   "18-20", "20+"))

spct_all$Race <- str_to_title(spct_all$Race)

spct_all$Race[spct_all$Race == "Black Hispanic"] <- "Black"
spct_all$Race[spct_all$Race == "White Hispanic"] <- "Hispanic/Latinx"
spct_all$Race[spct_all$Race == "Hispanic"] <- "Hispanic/Latinx"
spct_all$Race[spct_all$Race == "Arabic"] <- "Other"
spct_all$Race[spct_all$Race == "East Indian"] <- "Asian/Pacific Isl."
spct_all$Gender[spct_all$Gender == "M"] <- "Male"
spct_all$Gender[spct_all$Gender == "F"] <- "Female"
spct_all$Gender[spct_all$Gender == "U"] <- "Unknown"

spct_all$Race <- as.factor(spct_all$Race)
spct_all$year <- as.factor(spct_all$year)
spct_all$Gender <- as.factor(spct_all$Gender)
spct_all$Pct <- as.factor(spct_all$Pct)
spct_all$`Intervention Type` <- as.factor(spct_all$`Intervention Type`)
spct_all$Restraints <- as.factor(spct_all$Restraints)



# get dummies
spct_all <- dummy_cols(spct_all, select_columns = "Intervention Type")
spct_all <- dummy_cols(spct_all, select_columns = "Restraints")
spct_all <- dummy_cols(spct_all, select_columns = "Gender")
spct_all <- dummy_cols(spct_all, select_columns = "Race")
spct_all <- dummy_cols(spct_all, select_columns = "age_buckets")



# get summary file for mapping
spct_map <- spct_all %>% 
  group_by(Pct, year) %>% 
  summarise(total = n(), 
            count_arrests = sum(`Intervention Type_Arrested`, na.rm=TRUE),
            count_crisis = sum(`Intervention Type_Child in Crisis`, na.rm=TRUE),
            count_jvreport = sum(`Intervention Type_Juvenile Report`, na.rm=TRUE),
            count_mitigate = sum(`Intervention Type_Mitigated`, na.rm=TRUE),
            count_warrant = sum(`Intervention Type_PINS / Warrant`, na.rm=TRUE),
            count_summons = sum(`Intervention Type_Summons`, na.rm=TRUE),
            count_metal = sum(`Restraints_Metal`, na.rm=TRUE),
            count_velcro = sum(`Restraints_Velcro`, na.rm=TRUE),
            count_norestraint = sum(`Restraints_No Restraints`, na.rm=TRUE),
            count_male = sum(`Gender_Male`, na.rm=TRUE),
            count_female = sum(`Gender_Female`, na.rm=TRUE),
            count_gender_unknown = sum(`Gender_Unknown`, na.rm=TRUE),
            count_white = sum(`Race_White`, na.rm=TRUE),
            count_black = sum(`Race_Black`, na.rm=TRUE),
            count_latinx = sum(`Race_Hispanic/Latinx`, na.rm=TRUE),
            count_na = sum(`Race_American Indian`, na.rm=TRUE),
            count_asian = sum(`Race_Asian/Pacific Isl.`, na.rm=TRUE),
            count_other = sum(`Race_Other`, na.rm=TRUE),
            #count_indian = sum(`Race_East Indian`, na.rm=TRUE),
            #count_arabic = sum(`Race_Arabic`, na.rm=TRUE),
            count_unknown = sum(`Race_Unknown`, na.rm=TRUE),
            count_age_5 = sum(`age_buckets_Under 5`, na.rm=TRUE),
            count_age_6_8 = sum(`age_buckets_6-8`, na.rm=TRUE),
            count_age_9_11 = sum(`age_buckets_9-11`, na.rm=TRUE),
            count_age_12_14 = sum(`age_buckets_12-14`, na.rm=TRUE),
            count_age_14_15 = sum(`age_buckets_14-15`, na.rm=TRUE),
            count_age_16_17 = sum(`age_buckets_16-17`, na.rm=TRUE),
            count_age_18_20 = sum(`age_buckets_18-20`, na.rm=TRUE),
            count_age_20 = sum(`age_buckets_20+`, na.rm=TRUE),
            count_age_unknown = sum(`age_buckets_NA`, na.rm=TRUE))

#get summary of totals to use for "All" 
spct_map_all <- spct_all %>% 
  group_by(Pct) %>% 
  summarise(total = n(), 
            count_arrests = sum(`Intervention Type_Arrested`, na.rm=TRUE),
            count_crisis = sum(`Intervention Type_Child in Crisis`, na.rm=TRUE),
            count_jvreport = sum(`Intervention Type_Juvenile Report`, na.rm=TRUE),
            count_mitigate = sum(`Intervention Type_Mitigated`, na.rm=TRUE),
            count_warrant = sum(`Intervention Type_PINS / Warrant`, na.rm=TRUE),
            count_summons = sum(`Intervention Type_Summons`, na.rm=TRUE),
            count_metal = sum(`Restraints_Metal`, na.rm=TRUE),
            count_velcro = sum(`Restraints_Velcro`, na.rm=TRUE),
            count_norestraint = sum(`Restraints_No Restraints`, na.rm=TRUE),
            count_male = sum(`Gender_Male`, na.rm=TRUE),
            count_female = sum(`Gender_Female`, na.rm=TRUE),
            count_gender_unknown = sum(`Gender_Unknown`, na.rm=TRUE),
            count_white = sum(`Race_White`, na.rm=TRUE),
            count_black = sum(`Race_Black`, na.rm=TRUE),
            count_latinx = sum(`Race_Hispanic/Latinx`, na.rm=TRUE),
            count_na = sum(`Race_American Indian`, na.rm=TRUE),
            count_asian = sum(`Race_Asian/Pacific Isl.`, na.rm=TRUE),
            count_other = sum(`Race_Other`, na.rm=TRUE),
            #count_indian = sum(`Race_East Indian`, na.rm=TRUE),
            #count_arabic = sum(`Race_Arabic`, na.rm=TRUE),
            count_unknown = sum(`Race_Unknown`, na.rm=TRUE),
            count_age_5 = sum(`age_buckets_Under 5`, na.rm=TRUE),
            count_age_6_8 = sum(`age_buckets_6-8`, na.rm=TRUE),
            count_age_9_11 = sum(`age_buckets_9-11`, na.rm=TRUE),
            count_age_12_14 = sum(`age_buckets_12-14`, na.rm=TRUE),
            count_age_14_15 = sum(`age_buckets_14-15`, na.rm=TRUE),
            count_age_16_17 = sum(`age_buckets_16-17`, na.rm=TRUE),
            count_age_18_20 = sum(`age_buckets_18-20`, na.rm=TRUE),
            count_age_20 = sum(`age_buckets_20+`, na.rm=TRUE),
            count_age_unknown = sum(`age_buckets_NA`, na.rm=TRUE)) %>% 
  mutate(year = "All")

spct_map <- rbind(spct_map, spct_map_all)

rm(spct_map_all)

#get preinct shapefile and merge
precinct_shape <- read_sf('data/Precinct Shapefile/geo_export_9c5942c5-6f39-4241-8aa4-01bfc95c5a32.shp')
precinct_shape$precinct <- as.factor(precinct_shape$precinct)
spct_map <- left_join(spct_map, precinct_shape, by = c('Pct' = 'precinct'))

rm(precinct_shape)

#get boroughs of precincts and merge
precinct_boroughs <- read.csv("data/precinct_boroughs.csv")
precinct_boroughs$Precinct.Name <- as.factor(precinct_boroughs$Precinct.Name)
spct_map <- left_join(spct_map, precinct_boroughs, by = c('Pct' = 'Precinct.Name'))

#turn into sf
spct_map <- st_as_sf(spct_map)

# turn ZIP, schools, and year into factors
spct_map$Pct <- as.factor(spct_map$Pct)
spct_map$year <- as.factor(spct_map$year)
spct_map$Borough <- as.factor(spct_map$Borough)





# UI ========================================================================

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

ui <- fluidPage(
  theme = shinytheme('flatly'),
  navbarPage(
    "NYPD Student Safety Act Data",
    tabPanel("About",
             mainPanel(
               h2("About the dashboard"),
               HTML("<p>This app provides a cursory look at the NYPD’s School Safety Data 
                    (available <a href= https://www.nyc.gov/site/nypd/stats/reports-analysis/school-safety.page)>here</a>) 
                    from 2019 to 2022.</p>"),
               br(),
               HTML("<p>The Student Safety Act went into effect in New York City in 2016, following growing criticism from communities 
                    and advocates over the role of School Safety Agents (SSA), civilian NYPD employees stationed in schools, in 
                    criminalizing student behavior and contributing to the school to prison pipeline for Black and brown youth.</p>"),
               br(),
               HTML("<p>In addition to allowing students to file complaints against SSAs with the Civilian Review Complaint Board (CCRB), 
                    the Student Safety Act mandates quarterly reporting from the NYPD on School Safety Agent (SSA) interaction 
                    with students and others on NYC school campuses. This dashboard provides a tool to explore the trends in the data 
                    from 2019 - 2022.</p>"),
               br(),
               HTML("<p>For more information, see the ReadMe, avaibale on <a href=https://github.com/kellyann-hayes/Dashboard_U8353>GitHub.</a></p>") 
               
             )),
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput(
                   inputId = "mapdata",
                   label = "Choose a Dataset",
                   multiple = FALSE,
                   choices = c("All", "Precincts", "Schools"),
                   selected = "All"),
                 selectizeInput(
                   inputId = "schoolsearchmap",
                   label = "Search for a School",
                   multiple = FALSE,
                   choices = c("All", levels(ss_map$school)),
                   selected = "All"),
                 radioButtons("map_year", "Select Year:", 
                              choices = c(levels(ss_map$year)), 
                              selected = 'All'),
                 radioButtons("map_outcome", "Intervention Type:", 
                              choices = c("All", "Arrests", "Child in Crisis", 
                                          "Juvenile Reports", "Mitigated", 
                                          "PIN/Warrant", "Summons"), 
                              selected = 'All'),
                 downloadButton("downloadData", "Download School Locations CSV:"),
                 HTML("<p style='font-size:9px;'> The NYPD data available does not include unique school identifiers, making mapping to school locations difficult; the schools mtched to their coordinate locations used to create this dataset is available for download above. Note that not all schools were able to mapped to their locations; see ReadMe for more information.</p>")),
               mainPanel(HTML("<p>Each green circle represents a NYC public school, with circle size based on the amount of NYPD interventions.</p>"),
                         leafletOutput("schoolmap", width = "100%", height = 550)))),

    
    tabPanel("Incident Summary",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput(
                   inputId = "borough_intervention",
                   label = "Borough:",
                   multiple = FALSE,
                   choices = c("All", levels(ss_all$borough2)),
                   selected = 'All'),
                 radioButtons(inputId = "restraints_used",
                              label = "Restraints Used:",
                              choices = c("All", levels(ss_all$Restraints)),
                              selected = 'All')),
               mainPanel(
                 fluidRow(plotlyOutput("graph_intervention"),
                          br(),
                          HTML("<p style='font-size:11px;'><strong>Definitions found 
                          <a href= https://www.nyc.gov/assets/nypd/downloads/pdf/school_safety/student-safety-act-report-definitions.pdf)>here</a>:</strong>
                               <br><strong><i>Child in Crisis:</strong></i> situations in which a student was 'displaying signs of emotional distress' and 
                               was removed to the for psychological evaluation. Only students for which restraints were used are reported in this data.
                               <br><strong><i>Juvenile Report:</strong></i> a report taken for a student under 16 who allegedly committed an act that
                               would constitute an offense if committed by an adult. The report is prepared in lieu of an arrest or summons and the 
                               student is normally detained for the time it takes to gather the facts and complete the report.
                               <br><strong><i>Mitigated:</strong></i> situations in which a student committed what would amount to an offense but was released to the school for
                               discipline and/or mitigation rather than being processed as an arrest or summonsed. Only subjects for which mechanical restraints
                               were used are reported here.
                               <br><strong><i>PINS/Warrant:</strong></i> situations in which a person is in 'need of supervision'‐of the Family Court and is issued a warrant.
                               </p>"),
                          br(),
                          plotlyOutput("graph_enforcement"),
                          HTML("<p style='font-size:11px;'><strong>Definitions found 
                          <a href= https://www.nyc.gov/assets/nypd/downloads/pdf/school_safety/student-safety-act-report-definitions.pdf)>here</a>:</strong>
                               <br><strong><i>DB:</strong></i> NYPD Detective Bureau
                               <br><strong><i>DOE:</strong></i> NYC Department of Education 
                               student is normally detained for the time it takes to gather the facts and complete the report.
                               <br><strong><i>Housing:</strong></i> NYPD Housing Division
                               <br><strong><i>Patrol:</strong></i> NYPD Patrol Division
                               <br><strong><i>SSA:</strong></i> School Sfety Agents
                               <br><strong><i>Transit:</strong></i> NYPD Transit Division
                               <br><strong><i>UTF:</strong></i> Uniformed Task Force - NYPD Officers assigned to the School Safety Division
                               </p>"))))
                 #plotlyOutput("ss1", width = "100%", height = 500))
                 ),
    
    
    tabPanel("Demographics",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput(
                   inputId = "precinctsearch",
                   label = "Search for a Precinct",
                   multiple = FALSE,
                   choices = c("All", levels(spct_all$Pct)),
                   selected = 'All'),
                 selectizeInput(inputId = "gender",
                                label = "Gender (only Male, Female, and Unknown are available):",
                                choices = c("All", levels(spct_all$Gender)),
                                multiple = FALSE,
                                selected = 'All'),
                 radioButtons(inputId = "intervention_graph",
                              label = "Contact Type:",
                              choices = c("All", levels(spct_all$`Intervention Type`)),
                              selected = 'All'),
                 radioButtons(inputId = "age",
                              label = "Age:",
                              choices = c("All", levels(spct_all$age_buckets)),
                              selected = 'All')),
               mainPanel(
                 #HTML("<p>How to use the zoom feature:</p>"),
                 plotlyOutput("ss_race_line", width = "100%", height = 500)))),

))
  


# SERVER ---------------------------------------------------------------------

server <- function(input, output, session) {
  
  
  conditional <- function(condition, success) {
    if (condition) success else TRUE
  }
  
  reactive_school <- reactive({
    spct_all %>%
      filter(
        conditional(input$intervention_graph != "All", `Intervention Type` == input$intervention_graph),
        conditional(input$age != "All", age_buckets == input$age),
        #conditional(input$year_graph != "All", year == input$year_graph),
        conditional(input$gender != "All", Gender == input$gender),
        conditional(input$precinctsearch != "All", Pct == input$precinctsearch)
      )
  })
  
  
  reactive_graphs <- reactive({
    ss_all %>%
      filter(
        conditional(input$borough_intervention != "All", borough == input$borough_intervention),
        conditional(input$restraints_used != "All", Restraints == input$restraints_used),
      )
  })
  
  
  ss_map_year <- reactive({
      ss_map %>% 
        filter(year == input$map_year,
               conditional(input$schoolsearchmap != "All", school == input$schoolsearchmap))
      
    })
  
  
  spct_map_year <- reactive({
      spct_map %>% 
        filter(year == input$map_year)
    })
  
  
  ss_outcome_var <- reactive({
    if (input$map_outcome == 'All'){
      return(ss_map$total)
    } 
    else if (input$map_outcome == 'Arrests'){
      return(ss_map$count_arrests)
    } 
    else if (input$map_outcome == 'Child in Crisis'){
      return(ss_map$count_crisis)
    } 
    else if (input$map_outcome == 'Summons'){
      return(ss_map$count_summons)
    } 
    else if (input$map_outcome == 'PIN/Warrant'){
      return(ss_map$count_warrant)
    } 
    else if (input$map_outcome == 'Juvenile Reports'){
      return(ss_map$count_jvreport)
    } 
    else if (input$map_outcome == 'Mitigated'){
      return(ss_map$count_mitigate)
    }})
  
  
  spct_outcome_var <- reactive({
    if (input$map_outcome == 'All'){
      return(spct_map$total)
    } 
    else if (input$map_outcome == 'Arrests'){
      return(spct_map$count_arrests)
    } 
    else if (input$map_outcome == 'Child in Crisis'){
      return(spct_map$count_crisis)
    } 
    else if (input$map_outcome == 'Summons'){
      return(spct_map$count_summons)
    } 
    else if (input$map_outcome == 'PIN/Warrant'){
      return(spct_map$count_warrant)
    } 
    else if (input$map_outcome == 'Juvenile Reports'){
      return(spct_map$count_jvreport)
    } 
    else if (input$map_outcome == 'Mitigated'){
      return(spct_map$count_mitigate)
    }})
  
  
  intervention_name <- reactive({
    if (input$map_outcome == 'All'){
      return("Incidents")
    } 
    else if (input$map_outcome == 'Arrests'){
      return("Arrests")
    } 
    else if (input$map_outcome == 'Child in Crisis'){
      return("Child in Crisis")
    } 
    else if (input$map_outcome == 'Summons'){
      return("Summons")
    } 
    else if (input$map_outcome == 'PIN/Warrant'){
      return("PIN/Warrants")
    } 
    else if (input$map_outcome == 'Juvenile Reports'){
      return("Juvenile Reports")
    } 
    else if (input$map_outcome == 'Mitigated'){
      return("Mitigations")
    }})
  

  # OUTPUT ---------------------------------------------------------------------
  
  ##time series by race
  
  output$ss_race_line <- renderPlotly({
    spct_line <- reactive_school()
    line_race <- ggplotly(spct_line %>%
             group_by(yearq, Race) %>%
             summarise(count = n()) %>%
             ggplot(aes(x = yearq,
                        y = count,
                        color = Race,
                        group = Race)) +
             geom_line() +
             theme_minimal() +
             xlab("Year and Quarter") +
             ylab("Count") +
             scale_y_continuous(limits = c(0, 2500)) +
             theme(axis.text.x=element_text(angle=-45, hjust=0.001)) + 
               ggtitle("Incidents by Race, 2019 - 2022")
    )
    
    print(ggplotly(line_race))

    
  })
  
  
   
  output$graph_intervention <- renderPlotly({
    ss_intervention <- reactive_graphs()
    intervention_plot <- ggplotly(ggplot(ss_intervention, 
                                         aes(x=`Intervention Type`, 
                                             fill=`year`)) + 
                                    geom_bar(position="dodge") + 
                                    scale_fill_manual(values = c("#fde725","#35b779",
                                                                          "#31688e","#440154")) +
                                                                            theme_minimal() + 
                                    ggtitle("Count of Intervention Types, 2019 - 2022") + 
                                    ylab("Count")+ 
                                    labs(fill='Year'))
    print(ggplotly(intervention_plot))

    
  })
  
  
  output$graph_enforcement <- renderPlotly({
    ss_enforcement <- reactive_graphs()
    enforcement_plot <- ggplotly(ggplot(ss_enforcement,
                                         aes(x=`Enforcment By`,
                                             fill=`year`)) +
                                    geom_bar(position="dodge") +
                                    scale_fill_manual(values = c("#fde725","#35b779",
                                                                          "#31688e","#440154")) +
                                                                            theme_minimal() +
                                    ggtitle("Count of Enfocement by Agency Type, 2019 - 2022") +
                                   ylab("Count") + 
                                   labs(fill='Year'))
    
    print(ggplotly(enforcement_plot))
  })
  

  
  output$schoolmap <- renderLeaflet({
    
    school_map_out <- ss_map_year()
    spct_map_out <- spct_map_year()
    intervention <- intervention_name()
  
    pal <- colorNumeric(palette = "Blues", domain = spct_outcome_var())
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
      addPolygons(data = spct_map,
                  fillColor = ~pal(spct_outcome_var()),
                  fillOpacity = 0.3,
                  color = "black",
                  weight = .5,
                  popup = paste('<strong>', "Precinct No. ",'</strong>', spct_map$Pct, "<br>",
                                '<strong>',"Precinct-Wide ", intervention,": ",'</strong>', spct_outcome_var()),
                  group = "precinct") %>%
      addLegend(pal = pal,
                values =spct_outcome_var(),
                opacity = 0.7,
                title = paste("Precinct-Wide ", intervention),
                "bottomright") %>%
      addCircleMarkers(data = school_map_out,
                       lng = ~X,
                       lat = ~Y,
                       popup = paste(school_map_out$school,
                                     "<br>",
                                     "<br>",
                                     '<strong>',"Total: ",'</strong>', school_map_out$total,
                                     "<br>",
                                     '<strong>',"Arrests: ",'</strong>', school_map_out$count_arrests,
                                     "<br>",
                                     '<strong>', "Juvenile Reports: ",'</strong>', school_map_out$count_jvreport,
                                     "<br>",
                                     '<strong>', "Mitigations: ",'</strong>', school_map_out$count_mitigate,
                                     "<br>",
                                     '<strong>', "PINS/Warrants: ",'</strong>', school_map_out$count_warrant,
                                     "<br>",
                                     '<strong>', "Summons: ", '</strong>',school_map_out$count_summons,
                                     "<br>",
                                     '<strong>', "Metal Restraints Used: ",'</strong>', school_map_out$count_metal,
                                     "<br>",
                                     '<strong>', "Velcro Restraints Used: ",'</strong>', school_map_out$count_velcro,
                                     "<br>",
                                     '<strong>', "No Restraints Used: ",'</strong>', school_map_out$count_norestraint,
                                     "<br>",
                                     '<strong>', "Off-Site: ",'</strong>', school_map_out$count_offsite,
                                     "<br>",
                                     '<strong>',  "On-Site: ",'</strong>', school_map_out$count_onsite,
                                     "<br>",
                                     '<strong>', "Not School Related: ",'</strong>', school_map_out$count_unrelated,
                                     "<br>",
                                     '<strong>', "School Related: ",'</strong>', school_map_out$count_related),
                       #radius = ~ sqrt(ss_outcome_var()),
                       radius = ~ sqrt(ss_outcome_var()), #((nthroot(ss_outcome_var(),3) + sqrt(ss_outcome_var()))/2)
                       stroke = FALSE,
                       fillOpacity = 0.2,
                       opacity = 0.2,
                       color = "green",
                       group = "schools")
  })
  
  observeEvent(input$mapdata, {
    if(input$mapdata == "Precincts"){
      
      #school_map_out <- ss_map_year()
      spct_map_out <- spct_map_year()
      intervention <- intervention_name()
      
      pal <- colorNumeric(palette = "Blues", domain = spct_outcome_var())
      
      leafletProxy("schoolmap") %>%
        clearGroup("precinct") %>%
        clearGroup("schools") %>%
        clearControls() %>%
        addPolygons(data = spct_map,
                    fillColor = ~pal(spct_outcome_var()),
                    fillOpacity = 0.3,
                    color = "black",
                    weight = .5,
                    popup = paste("Precinct No. ", spct_map$Pct, "<br>",
                                  "Precinct-Wide Incidents: ", spct_map$total),
                    group = "precinct") %>%
        addLegend(pal = pal,
                  values =spct_outcome_var(),
                  opacity = 0.7,
                  title = "Precinct-Wide Incidents",
                  "bottomright")

    }
    else if(input$mapdata == "Schools"){
      school_map_out <- ss_map_year()
      #spct_map_out <- spct_map_year()
      intervention <- intervention_name()
      
      leafletProxy("schoolmap") %>%
        clearGroup("precinct") %>%
        clearGroup("schools") %>%
        clearControls() %>%
        addCircleMarkers(data = school_map_out,
                         lng = ~X,
                         lat = ~Y,
                         popup = paste(school_map_out$school,
                                       "<br>",
                                       "<br>",
                                       '<strong>',"Total: ",'</strong>', school_map_out$total,
                                       "<br>",
                                       '<strong>',"Arrests: ",'</strong>', school_map_out$count_arrests,
                                       "<br>",
                                       '<strong>', "Juvenile Reports: ",'</strong>', school_map_out$count_jvreport,
                                       "<br>",
                                       '<strong>', "Mitigations: ",'</strong>', school_map_out$count_mitigate,
                                       "<br>",
                                       '<strong>', "PINS/Warrants: ",'</strong>', school_map_out$count_warrant,
                                       "<br>",
                                       '<strong>', "Summons: ", '</strong>',school_map_out$count_summons,
                                       "<br>",
                                       '<strong>', "Metal Restraints Used: ",'</strong>', school_map_out$count_metal,
                                       "<br>",
                                       '<strong>', "Velcro Restraints Used: ",'</strong>', school_map_out$count_velcro,
                                       "<br>",
                                       '<strong>', "No Restraints Used: ",'</strong>', school_map_out$count_norestraint,
                                       "<br>",
                                       '<strong>', "Off-Site: ",'</strong>', school_map_out$count_offsite,
                                       "<br>",
                                       '<strong>',  "On-Site: ",'</strong>', school_map_out$count_onsite,
                                       "<br>",
                                       '<strong>', "Not School Related: ",'</strong>', school_map_out$count_unrelated,
                                       "<br>",
                                       '<strong>', "School Related: ",'</strong>', school_map_out$count_related),
                         #radius = ~ sqrt(ss_outcome_var()),
                         radius = ~ sqrt(ss_outcome_var()), #((nthroot(ss_outcome_var(),3) + sqrt(ss_outcome_var()))/2)
                         stroke = FALSE,
                         fillOpacity = 0.2,
                         opacity = 0.2,
                         color = "green",
                         group = "schools")
    }
    else {
      school_map_out <- ss_map_year()
      spct_map_out <- spct_map_year()
      intervention <- intervention_name()
      
      pal <- colorNumeric(palette = "Blues", domain = spct_outcome_var())
      
      leafletProxy("schoolmap") %>%
        clearGroup("precinct") %>%
        clearGroup("schools") %>%
        clearControls() %>%
        addPolygons(data = spct_map,
                    fillColor = ~pal(spct_outcome_var()),
                    fillOpacity = 0.3,
                    color = "black",
                    weight = .5,
                    popup = paste("Precinct No. ", spct_map$Pct, "<br>",
                                  "Precinct-Wide Incidents: ", spct_map$total),
                    group = "precinct") %>%
        addLegend(pal = pal,
                  values =spct_outcome_var(),
                  opacity = 0.7,
                  title = "Precinct-Wide Incidents",
                  "bottomright") %>%
        addCircleMarkers(data = school_map_out,
                         lng = ~X,
                         lat = ~Y,
                         popup = paste(school_map_out$school,
                                       "<br>",
                                       "<br>",
                                       '<strong>',"Total: ",'</strong>', school_map_out$total,
                                       "<br>",
                                       '<strong>',"Arrests: ",'</strong>', school_map_out$count_arrests,
                                       "<br>",
                                       '<strong>', "Juvenile Reports: ",'</strong>', school_map_out$count_jvreport,
                                       "<br>",
                                       '<strong>', "Mitigations: ",'</strong>', school_map_out$count_mitigate,
                                       "<br>",
                                       '<strong>', "PINS/Warrants: ",'</strong>', school_map_out$count_warrant,
                                       "<br>",
                                       '<strong>', "Summons: ", '</strong>',school_map_out$count_summons,
                                       "<br>",
                                       '<strong>', "Metal Restraints Used: ",'</strong>', school_map_out$count_metal,
                                       "<br>",
                                       '<strong>', "Velcro Restraints Used: ",'</strong>', school_map_out$count_velcro,
                                       "<br>",
                                       '<strong>', "No Restraints Used: ",'</strong>', school_map_out$count_norestraint,
                                       "<br>",
                                       '<strong>', "Off-Site: ",'</strong>', school_map_out$count_offsite,
                                       "<br>",
                                       '<strong>',  "On-Site: ",'</strong>', school_map_out$count_onsite,
                                       "<br>",
                                       '<strong>', "Not School Related: ",'</strong>', school_map_out$count_unrelated,
                                       "<br>",
                                       '<strong>', "School Related: ",'</strong>', school_map_out$count_related),
                         #radius = ~ sqrt(ss_outcome_var()),
                         radius = ~ sqrt(ss_outcome_var()), #((nthroot(ss_outcome_var(),3) + sqrt(ss_outcome_var()))/2)
                         stroke = FALSE,
                         fillOpacity = 0.2,
                         opacity = 0.2,
                         color = "green",
                         group = "schools")
    }
  })
  
  
  data_download <- ss_map
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("nypd_ssa_locations.csv", sep="")  #, Sys.Date(),
    },
    content = function(file) {
      write.csv(data_download, file)
    }
  )

  
  
}


shinyApp(ui, server)