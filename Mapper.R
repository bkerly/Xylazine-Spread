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
  group_by(date,base_drug,region_name
)%>%
  summarize(drug_reports = sum(drug_reports,na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(region_name))

region_data %>%
  ggplot(aes(x=date,y=drug_reports,color = base_drug)) +
  geom_line() +
  facet_wrap(~region_name)


# Big 3 Detections --------------------------------------------------------

pct_detections <- analysis_data %>%
  pivot_wider(names_from = "base_drug",values_from = "drug_reports") %>%
  group_by(state_abb,region_name,division_name,date) %>%
  mutate(across(Heroin:Xylazine, 
                ~ .x / sum(Heroin,Oxycodone,Fentanyl,Xylazine,na.rm=TRUE),
                 .names = "pct_{.col}")) %>%
  ungroup()

# four states
pct_detections %>%
  filter(state_abb %in% c("NY","PA","OH","CA")) %>%
  select(state_abb,date,starts_with("pct_")) %>%
  pivot_longer(cols = starts_with("pct_"),
               names_to = "Drug",
               values_to = "Percent"
  ) %>%
  filter(date >= ymd("2012-1-1")) %>%
  filter(Drug != "pct_Oxycodone") %>%
  ggplot(aes(x=date,y=Percent,color = Drug)) +
  geom_line() +
  ggthemes::theme_igray() +
  ylab("Relative Percent")+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~state_abb,nrow=4)


pct_detections %>% 
  filter(state_abb %in% c("NY","PA","OH","CA")) %>%
  group_by(state_abb) %>%
  arrange(date) %>%
  mutate(xylaxine_increase = pct_Xylazine / lag(pct_Xylazine)) %>%
  ungroup() %>%
  ggplot(aes(x=date,y=pct_Xylazine,color = state_abb)) +
  geom_line() +
  ylim(0,0.05)+
  ylab("Relative Percent Xylazine")+
  scale_y_continuous(labels = scales::percent)
