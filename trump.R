library(pdftools)
library(tidyverse)
library(lubridate)
library(hms)
library(here)

#read in data
data <- pdf_text(here('Trump_private_schedule',
                      'Axios-President-Donald-Trump-Private-Schedules.pdf'))%>%
  read_lines()%>%
  reduce(paste)

#https://stackoverflow.com/a/45941699/7175713
#had issues with some strange characters
#applying the change to ascii fixed it
data <- iconv(data, from = 'UTF-8',to = 'ASCII//TRANSLIT')

date_extraction <- data %>%
  str_split("SCHEDULE OF THE PRESIDENT")%>%
  #regex captures day of the week, 
  #followed by a comma,space, month, 
  #day in number,comma, space and the four digit year
  str_extract_all("(Monday|Tuesday|Wednesday|Thursday|Friday),[\\sa-zA-Z]+\\d+,\\s\\d{4}")%>%
  .[[1]]%>%
  enframe()%>%
  select(dates=value)

activities <- data %>%
  str_split("SCHEDULE OF THE PRESIDENT")%>%
  #split based on regex
  str_split("(Monday|Tuesday|Wednesday|Thursday|Friday),[\\sa-zA-Z]+\\d+,\\s\\d{4}") %>%
  .[[1]]%>%
  enframe()%>%
  #effectively removes the first row, which is empty
  filter(!str_detect(value, "\"\\s+ \", \"")) %>% 
  select(activities = value)


temp <- date_extraction%>%
  bind_cols(activities)


#create dataframe to extract the time component
#create a temp_Time to pull out the time data
#create a temp_content to pull out the contents
#merge them to get a single dataframe containing date, time and content

time_pattern <- "\\s\\d+:\\d{2}\\s(AM|PM)\\s(CST|EST|Local)*[\\s]*|TBD"

temp_Time <- temp %>%
  #picks the time, 
  #including the AM|PM and timezone, 
  #or the one that has TBD in the time column
  mutate(Time = str_extract_all(activities,time_pattern))%>%
  unnest_longer(Time)%>%
  select(dates,Time)

temp_content <- temp %>%
  mutate(content = str_split(activities, time_pattern))%>%
  select(content)%>%
  unnest_longer(content)%>%
  filter(!content=="", !content==" ")

schedule <- temp_Time %>%
  bind_cols(temp_content)

#now the real munging begins
#extract all the relevant columns
#plus cleaning along the way

duration_pattern <- "\\([\\w\\s,]+\\)"
alternative_time <- "\\(\\d{1,2}:\\d{2}\\s(AM|PM)\\s(CST|EST)\\)"

schedule <- schedule %>%
           #convert dates to proper format
  mutate(dates = parse_date_time(dates,"A, b! d!, Y!"),
         dates = as_date(dates),
         Time = str_trim(Time),
         #pull out only the time component... no timezones
         Time_no_tz = str_extract(Time,"[\\d:\\s]+(AM|PM)"), 
         #convert to proper datetime format
         Time_no_tz = parse_date_time(Time_no_tz,"H!:M! p!"),
         #this pulls out time zones and local
         Time_tz_if = str_extract(Time, "(CST|EST|Local)"),
         #this refactors the time based on timezone
         #I concluded that time in Washington D.C is based on EST
         Time_no_tz_update = case_when(is.na(Time_tz_if) ~Time_no_tz,
                                       Time_tz_if=="CST" ~Time_no_tz+dhours(1),
                                       Time_tz_if%in% c("EST","Local") ~Time_no_tz),
         Time_no_tz_update = hms(minutes = minute(Time_no_tz_update),
                                 hours = hour(Time_no_tz_update)),
         #Extract duration from contents and clean it up
         Duration = str_extract(content,duration_pattern),
         Duration = str_remove_all(Duration, "[\\(),]"),
         Duration = str_remove_all(Duration,"\\s+"),
         Duration_hr = as.numeric(str_extract(Duration,"\\d+(?=hr)")),
         Duration_hr = coalesce(Duration_hr,0),
         Duration_min = as.numeric(str_extract(Duration,"\\d+(?=min)")),
         Duration_min = coalesce(Duration_min, 0),
         Duration_cleaned = hms(minutes = Duration_min, hours = Duration_hr),
         Duration_cleaned = na_if(Duration_cleaned,00:00),
         #Extract Location from content and clean it up
         Location = str_extract(content,"Location:[\\s\\w-,]+"),
         Location = str_remove_all(Location,"Location:"),
         Location= str_remove_all(Location,"RON|Note|Press"),
         Location= str_trim(Location),
         #Extract Project officer and clean it up
         Project_Officer = str_extract(content,"Project Officer?:[\\s[:alpha:],]+"),
         Project_Officer = str_remove_all(Project_Officer,"Project Officer?:"),
         Project_Officer = str_remove_all(Project_Officer,'(Press|Location)[[:punct:]\\s\\w]*'),
         Project_Officer = str_trim(Project_Officer),
         listed_title = str_remove_all(content,"(Location|Project Officer?|Note):.*"),
         listed_title = str_remove_all(listed_title,duration_pattern),
         listed_title = str_remove_all(listed_title,"RON:.*"),
         listed_title = str_remove_all(listed_title,alternative_time),
         listed_title = str_remove_all(listed_title,"[\\)]"),
         listed_title = str_replace(listed_title,'"',''),
         listed_title = str_squish(listed_title),
         #Extract Notes and clean it up
         Notes = str_extract(content,"Note:[\\s[:alpha:]]+(-\\d+)*"),
         Notes = str_remove(Notes,"Note:"),
         Notes = str_trim(Notes),
         #never got to use these columns
         RON_Location = str_extract(content,"RON:.*"),
         RON_Location = str_remove_all(RON_Location,"RON:"),
         TZ = str_extract(content,alternative_time),
         TZ = str_remove_all(TZ, "[\\(\\)]"),
         )%>%
  filter(!is.na(Time_no_tz_update))


#Notes to self
#the times are quite inconsistent (cst, est, local, foreign country time etc). Dont bother; it's just an exercise.
#Thursday Nov 29, 2018 ...10:05PM Local ... Argentina ... hard code it? don't bother

final <- schedule%>%
  select(dates,
         time = Time_no_tz_update,
         duration = Duration_cleaned,
         listed_title,
         Location,
         Project_Officer,
         Notes)

tail(final)

#https://stackoverflow.com/a/58236201/7175713
#list of packages used for this exercise
installed.packages()[(.packages()),3]
