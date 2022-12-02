case <- data.table::fread("Statewide COVID-19 Cases Deaths Tests.csv")
waterwaste <- data.table::fread("master-covid-public.csv")
waterwaste<-select(waterwaste, epaid,wwtp_name,reporting_jurisdiction,county_names
                   ,test_result_date,pcr_target_avg_conc,Sample_Location_Longitude,Sample_Location_Latitude)

#switch the FIPS code in waterwaste data to county name for a easier identification and further expolore
waterwaste$county_names[waterwaste$county_names=="06067"]<-"Sacramento"
waterwaste$county_names[waterwaste$county_names=="06001, 06013"]<-"Alameda"
waterwaste$county_names[waterwaste$county_names=="06063"]<-"Plumas"
waterwaste$county_names[waterwaste$county_names=="06029"]<-"Kern"
waterwaste$county_names[waterwaste$county_names=="06025"]<-"Imperial"
waterwaste$county_names[waterwaste$county_names=="06013"]<-"Contra Costa"
waterwaste$county_names[waterwaste$county_names=="06019"]<-"Fresno"
waterwaste$county_names[waterwaste$county_names=="06037"]<-"Los Angeles"
waterwaste$county_names[waterwaste$county_names=="06037, 06111"]<-"Los Angeles"
waterwaste$county_names[waterwaste$county_names=="06033"]<-"Lake"
waterwaste$county_names[waterwaste$county_names=="06059"]<-"Orange"
waterwaste$county_names[waterwaste$county_names=="06071"]<-"San Bernardino"
waterwaste$county_names[waterwaste$county_names=="06073"]<-"San Diego"
waterwaste$county_names[waterwaste$county_names=="06075, 06081"]<-"San Francisco"
waterwaste$county_names[waterwaste$county_names=="06099"]<-"Stanislaus"
waterwaste$county_names[waterwaste$county_names=="06047"]<-"Merced"
waterwaste$county_names[waterwaste$county_names=="06081"]<-"San Mateo"
waterwaste$county_names[waterwaste$county_names=="06085"]<-"Santa Clara"
waterwaste$county_names[waterwaste$county_names=="06111"]<-"Ventura"
waterwaste$county_names[waterwaste$county_names=="06113"]<-"Yolo"

waterwaste$county_names[waterwaste$county_names=="Orange County"]<-"Orange"

### 2. Look at the data

dim(case)
head(case)
tail(case)
dim(waterwaste)
head(waterwaste)
tail(waterwaste)
str(case)
str(waterwaste)
waterwaste$pcr_target_avg_conc<-as.numeric(waterwaste$pcr_target_avg_conc)
summary(waterwaste$pcr_target_avg_conc)
summary(waterwaste$county_names)

### 3. Format the data
#removing the rows  "NA" 
waterwaste<-waterwaste[, pcr_target_avg_conc   := fifelse(waterwaste$pcr_target_avg_conc == "", NA_integer_, pcr_target_avg_conc)]
waterwaste <- waterwaste[!is.na(pcr_target_avg_conc)]
case<-case[, cases   := fifelse(case$cases == "", NA_integer_, cases)]
case <- case[!is.na(cases)]
case<-case[, cumulative_cases   := fifelse(case$cumulative_cases == "", NA_integer_, cumulative_cases)]
case <- case[!is.na(cumulative_cases)]

# format the date
#merge the data

colnames(case)[2] <- "county_names"
colnames(waterwaste)[5] <- "date"
waterwaste$date<-as.Date(waterwaste$date, "%m/%d/%Y")
case$date<-as.Date(case$date, "%m/%d/%Y")
res1<-semi_join(case, waterwaste, by = "county_names")
case_final<-semi_join(res1,waterwaste,by="date")
# calculate the mean

#county name
#date
data_avg1<-
  case_final[ , .(
    new_case_mean      =mean(cases, na.rm=T),
    cumulative_mean       =mean(cumulative_cases, na.rm=T))
    ,by = c("date", "county_names")]
data_avg1
data_avg2<-
  waterwaste[ , .(
    pcr_target_mean      =mean(pcr_target_avg_conc, na.rm=T),
    lng= Sample_Location_Longitude,
    lat=Sample_Location_Latitude
  )
  ,by = c("date", "county_names")]
data_avg2

data_avg<-merge(
  # Data
  x     = data_avg1,      
  y     = data_avg2, 
  # List of variables to match
  by.x  = c("date", "county_names"),
  by.y  = c("date", "county_names"), 
  # Which obs to keep?
  all.x = FALSE,      
  all.y = TRUE
) 
data_avg


#county
county_avg1<-
  case_final[ , .(
    new_case_mean      =mean(cases, na.rm=T),
    cumulative_mean       =mean(cumulative_cases, na.rm=T))
    ,by =  "county_names"]
county_avg1
county_avg2<-
  waterwaste[ , .(
    pcr_target_mean      =mean(pcr_target_avg_conc, na.rm=T)
  )
  ,by = 'county_names']
county_avg2$lat<-c(39.9927, 35.4937, 33.0114, 37.8534, 37.6017, 36.7378,34.0522,39.0840,33.7175,34.1083,32.7157,37.7749,37.5091,37.2010,37.4337,37.3337,38.7646,38.4747,34.3705)
county_avg2$lon<-c(-120.8039, -118.8597,-115.4734, -121.9018,-121.7195,-119.7871,-118.2437,-122.8084,-117.8311,-117.2898,-117.1611,-122.4194,-120.9876,-120.7120,-122.4014,-121.8907,-121.9018,-121.3542,-119.1391)

county_avg2
county_avg<-merge(
  # Data
  x     = county_avg1,      
  y     = county_avg2, 
  # List of variables to match
  by.x  =  "county_names",
  by.y  =  "county_names", 
  # Which obs to keep?
  all.x = FALSE,      
  all.y = TRUE
) 
county_avg





