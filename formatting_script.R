library(tidyr)
library(plyr)
library(dplyr)

# THIS SCRIPT COMPILES AND FORMATS QUARTERLY STATE-LEVEL ECONOMIC DATA FROM 2005 TO 2015
# CSV FILES FROM MUST BE SAVED IN THE FILEPATH SPECIFIED BELOW

# Data sources: 
# BEA = http://www.bea.gov/regional/downloadzip.cfm 
#   Quarterly GDP by State (nominal)
#   Quarterly State & Personal Income, all tables & areas
# BLS = http://www.bls.gov/lau/rdscnp16.htm
#   Monthly population, employment and unemployment statistics

# Set filepath location where csv documents are stored

filepath <- "C:/Users/jtryker/Documents/R/UM_korniotis"


# Create a list of 50 US states plus the District of Columbia 

state_list <- read.csv(paste(filepath,"/state_list.csv", sep = ""), 
                       header = FALSE, 
                       stringsAsFactors = FALSE)

state_list <- state_list$V1


# Read in csv file containing nominal gdp data as a data.frame (BEA)

nom_gdp <- read.csv(paste(filepath,"/state_gdp_edited.csv", sep = ""), 
                    header = TRUE,
                    stringsAsFactors = FALSE)


# Modify original data.frame to exclude non-relevant information, filter for only total gdp,
# and convert table from wide- to long-form

nom_gdp1 <- nom_gdp %>%
              filter(Description == "All industry total",
                     GeoName %in% state_list) %>%
              gather(Period, GDP, X2005Q1:X2015Q4) %>%
              spread(Description, GDP) %>%            
              separate(Period, c("Year", "Quarter"), sep = "Q") %>%
              select(-ComponentId, -ComponentName, -IndustryId, -IndustryClassification) %>%
              mutate(index = paste(GeoName, Year, Quarter, sep = ":"))

# Rename column for ease of reference
colnames(nom_gdp1)[6] <- "GDP_All"


# Repeat process as for nom_gdp1, except now filter for Private industies

nom_gdp2 <- nom_gdp %>%
              filter(Description == " Private industries",
                     GeoName %in% state_list) %>%
              gather(Period, GDP, X2005Q1:X2015Q4) %>%
              spread(Description, GDP) %>%            
              separate(Period, c("Year", "Quarter"), sep = "Q") %>%
              mutate(index = paste(GeoName, Year, Quarter, sep = ":"))

colnames(nom_gdp2)[10] <- "GDP_Pri"

# Select only index and private industry gdp columns to merge into a combined data.frame later on

nom_gdp2 <- select(nom_gdp2, GDP_Pri:index)


# Repeat process as for nom_gdp1&2, except now filter for Private industies

nom_gdp3 <- nom_gdp %>%
              filter(Description == " Government",
                     GeoName %in% state_list) %>%
              gather(Period, GDP, X2005Q1:X2015Q4) %>%
              spread(Description, GDP) %>%            
              separate(Period, c("Year", "Quarter"), sep = "Q") %>%
              mutate(index = paste(GeoName, Year, Quarter, sep = ":"))

colnames(nom_gdp3)[10] <- "GDP_Gov"

nom_gdp3 <- select(nom_gdp3, GDP_Gov:index)


# Remove erroneous labelling from year column and redefine Year/Quarter columns as integers in nom_gdp1

nom_gdp1$Year <- gsub("X", "", nom_gdp1$Year)
nom_gdp1$Year <- as.integer(nom_gdp1$Year)
nom_gdp1$Quarter <- as.integer(nom_gdp1$Quarter)


# Read in personal income data as a data.frame (BEA)

pi <- read.csv(paste(filepath,"/state_pi_edited.csv", sep = ""),
               header = TRUE,
               stringsAsFactors = FALSE)


# Repeat similar process as nom_gdp1-3 tables above to tidy the original files

pi1 <- pi %>%
        select(GeoName, Description, X2005.1:X2015.4) %>%
        filter(GeoName %in% state_list,
               Description == "Personal income (thousands of dollars, seasonally adjusted)") %>%
        gather(Period, PI, X2005.1:X2015.4) %>%
        spread(Description, PI) %>%            
        separate(Period, c("Year", "Quarter"), sep = "\\.") %>%
        mutate(index = paste(GeoName, Year, Quarter, sep = ":"))

colnames(pi1)[4] <- "PI"
pi1 <- select(pi1, PI:index)

pi2 <- pi %>%
        select(GeoName, Description, X2005.1:X2015.4) %>%
        filter(GeoName %in% state_list,
               Description == "Per capita personal income (dollars) 2/") %>%
        gather(Period, PCPI, X2005.1:X2015.4) %>%
        spread(Description, PCPI) %>%            
        separate(Period, c("Year", "Quarter"), sep = "\\.") %>%
        mutate(index = paste(GeoName, Year, Quarter, sep = ":"))

colnames(pi2)[4] <- "PCPI"
pi2 <- select(pi2, PCPI:index)


# Read in population, employment and unemployment data as a data.frame (BLS)

pop_emp <- read.csv(paste(filepath,"/state_employment_edited.csv", sep = ""),
                    header = TRUE,
                    stringsAsFactors = FALSE)

# Tidy dataset similar to process above for nom_gdp and pi

month_list <- c(3,6,9,12)
quarter_list <- c(1,2,3,4)

pop_emp <- pop_emp %>%
            filter(State.and.area %in% state_list,
                   Year >= 2005 & Year <= 2015,
                   Month %in% month_list) 

pop_emp$Month <- as.integer(mapvalues(pop_emp$Month, month_list, quarter_list))

colnames(pop_emp)[5:11] <- c("Civ_Pop", 
                             "Civ_LF", "Civ_LFP", 
                             "Civ_E", "Civ_EP",
                             "Civ_UE", "Civ_UEP")
pop_emp <- pop_emp %>%
              mutate(xYear = paste("X", Year, sep = ""), 
                     index = paste(State.and.area, xYear, Month, sep = ":")) %>%
              select(Civ_Pop:index, -xYear)


# Merge data frames thus far to create a final data set [almost]

final_data <- merge(nom_gdp1, merge(nom_gdp2, merge(nom_gdp3, merge(pi1, merge(pi2, pop_emp)))))
final_data <- select(final_data, -index)


# Read in GDP release dates gleaned manually from news released schedules published by BEA
# Merge with final_data; source URL (change year) = http://www.bea.gov/newsreleases/2016rd.htm

release_dates_gdp <- read.csv(paste(filepath,"/release_dates_gdp.csv", sep = ""),
                              header = TRUE,
                              stringsAsFactors = FALSE)

final_data <- mutate(final_data, qyear = paste(Quarter,Year, sep = ""))
final_data$qyear <- as.integer(final_data$qyear)

final_data <- merge(final_data, release_dates_gdp)


# Read in personal income data scraped from BEA
# Merge with final_data;source URL = "http://www.bea.gov/regional/histdata/"

release_dates_pi <- read.csv(paste(filepath,"/release_dates_pi.csv", sep = ""),
                             header = TRUE,
                             stringsAsFactors = FALSE)

final_data <- merge(final_data, release_dates_pi)


# Read in unemployement release dates from BLS 
# Merge; source URL = http://www.bls.gov/schedule/archives/laus_nr.htm#lau_0913_release

release_dates_ue <- read.csv(paste(filepath,"/release_dates_ue.csv", sep = ""),
                             header = TRUE,
                             stringsAsFactors = FALSE)

final_data <- merge(final_data, release_dates_ue)


# Remove qyear index column and write to folder

final_data <- select(final_data, -qyear)

write.csv(final_data, paste(filepath, "/state_final_data.csv", sep = ""))


# Create key and footnotes files with descriptive labels for column names in final dataset

keyname <- c("Federal Information Processing Standard Code",
             "State or Region Name",
             "Region Number",
             "Year corresponding to the period covered by each data value",
             "Quarter corresponding to the period covered by each data value",
             "Total GDP, All Industries (Millions of dollars, seasonally adjusted at annual rates)",
             "GDP, Private Industries",
             "GDP, Government",
             "Personal Income (civilian noninstitutional population, seasonally adjusted)",
             "Personal Income per Capita",
             "Civilian noninstitutional population",
             "Civilian Labor Force",
             "Civilian Labor Force, percent of population",
             "Employed portion of labor force",
             "Employed portion of labor force, percent of population",
             "Unemployed portion of labor force",
             "Unemployed portion of labor force, percent of population",
             "GDP: Release publication text",
             "GDP: Scheduled date/time of publication release - BEA",
             "GDP: Day of the week for each release date",
             "PI: Release publication text",
             "PI: Scheduled date/time of publication release - BEA",
             "PI: Day of the week for each release date",
             "UE: Release pulication text",
             "UE: Scheduled date/time of publication release - BLS",
             "UE: Day of the week for each release date")

footnotes <- c("1) BLS - state employment figures are provided on a monthly basis. The data herein are the monthly releases taken at the end of each quarter.",
               "2) BEA - Quarterly State GDP only became available on a quarterly basis from 2015 onwards. Prior to 2015, quarterly state GDP was provided annually as part of the annual state GDP release. Time stamps are for scheduled release dates. Actual release dates could vary.",
               "3) Timestamps are in Eastern Standard Time (EST)",
               "4) State list includes the District of Columbia")

final_data_key <- data.frame(colname = colnames(final_data), 
                             keyname = keyname)

final_data_footnotes <- data.frame(footnotes = footnotes)


# Write final dataset key and footnotes files to the filepath specified above

write.csv(final_data_key, paste(filepath, "/state_final_data_key.csv", sep = ""))
write.csv(final_data_footnotes, paste(filepath, "/state_final_data_footnotes.csv", sep = ""))

