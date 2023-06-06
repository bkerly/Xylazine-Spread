library(tidyverse)
library(usmap)
library(lubridate)
library(readr)


# Load Data ---------------------------------------------------------------

NFLIS_Data <- read_csv("NFLIS_Drug_DQS_2023_06_06_13_44_32.csv")

Census_Bureau_Regions_and_Divisions_by_State_FIPS <- read_csv("Census Bureau Regions and Divisions by State FIPS.csv")

analysis_data <- NFLIS_Data %>%
  mutate(date = 
           case_when(PERIOD == "SA1 (JAN-JUN)" ~
                     ymd(
                       paste0(
                         YYYY,"-1-1"
                       )
                     ),
                     TRUE ~ ymd(
                       paste0(YYYY,"-7-1")
                     )
             
           )
         ) %>%
  transmute(
    state_abb = STATE,
    fips = STATE_FIPS,
    date = date,
    drug_reports = DRUG_REPORTS,
    base_drug = BASE_DESCRIPTION
    
  ) %>%
  group_by(
    state_abb,fips,date,base_drug
  ) %>%
  summarize(drug_reports = sum(drug_reports,na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(numfips = as.numeric(fips)) %>%
  left_join(Census_Bureau_Regions_and_Divisions_by_State_FIPS,
            by=c("numfips" = "fips")) %>%
  select(-numfips) %>%
  filter(base_drug != "Unspecified Prescription Analgesic")




# Basic California Stats --------------------------------------------------

CA_data <- analysis_data %>%
  filter(state_abb == "CA")

CA_data %>%
  ggplot(aes(x=date,y=drug_reports,color = base_drug)) +
  geom_line()


# National Data -----------------------------------------------------------

US_data <- analysis_data %>%
  group_by(
    date,base_drug
  ) %>%
  summarize(drug_reports = sum(drug_reports,na.rm = TRUE)) %>%
  ungroup()

US_data %>%
  ggplot(aes(x=date,y=drug_reports,color = base_drug)) +
  geom_line()


# Xylazine, Fentanyl, and Heroin by Region --------------------------------

region_data <- analysis_data %>%
  group_by(    date,base_drug,region_name
)%>%
  summarize(drug_reports = sum(drug_reports,na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(region_name))

region_data %>%
  ggplot(aes(x=date,y=drug_reports,color = base_drug)) +
  geom_line() +
  facet_wrap(~region_name)
