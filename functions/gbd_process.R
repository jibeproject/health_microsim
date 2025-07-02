
gbd_process <- function(data) {


gbdp <- data %>%
  filter(year == "2018") %>%  #decreases the size file
  filter(metric %in% "Rate") %>%
  select(-c(upper, lower)) %>%
  filter(!age %in% "All ages") %>%
  mutate(rate_1=val/100000) %>% 
  # some ages have 'years' (eg 5-9 years), while others don't (eg 80-84); omit 'years'
  mutate(age = gsub(" years", "", age)) %>%
  tidyr::extract(age, c("from_age", "to_age"), "(.+)-(.+)", remove=FALSE, convert=TRUE) %>%
  mutate(from_age = case_when(age=="95+"  ~  95L,
                              age=="<5"  ~  0L,
                              TRUE  ~  from_age),
         to_age = case_when(age=="95+"  ~  99L,
                            age=="<5"  ~  4L,
                            TRUE  ~  to_age),
         agediff = to_age - from_age + 1,
         val1yr = rate_1) %>% 
  #we do not distribute among age groups as it is a rate but assume same within age group
  rename(agegroup = age) 

# Data preparation to match diseases and names from Cambridge physical activity meta-analysis

# Some specific cancers need adjustment to match GBD data to standardised JIBE disease list.

gbdp <- gbdp %>%
  filter (cause %in% c("Stroke", "Ischemic heart disease", "Breast cancer", 
                       "Uterine cancer", "Tracheal, bronchus, and lung cancer", 
                       "Colon and rectum cancer", "Esophageal cancer", 
                       "Liver cancer",  
                       "Stomach cancer", "Chronic myeloid leukemia", "Multiple myeloma", 
                       "Larynx cancer", "Lip and oral cavity cancer", "Nasopharynx cancer",
                       "Other pharynx cancer", "Bladder cancer",  
                       "Depressive disorders",  "Alzheimer's disease and other dementias", 
                       "Diabetes mellitus type 2", "Chronic obstructive pulmonary disease", "Parkinson's disease")) %>%
  mutate(cause = case_when(
    cause == "Ischemic heart disease"    ~ "coronary_heart_disease",
    cause == "Uterine cancer"            ~ "endometrial_cancer",
    cause == "Stomach cancer"            ~ "gastric_cardia_cancer",
    cause == "Chronic myeloid leukemia"  ~ "myeloid_leukemia",
    cause == "Multiple myeloma"          ~ "myeloma",
    cause == "Depressive disorders" ~ "depression",
    cause == "Alzheimer's disease and other dementias" ~ "all_cause_dementia",
    cause == "Diabetes mellitus type 2"  ~ "diabetes",
    cause == "Stroke" ~ "stroke", 
    cause == "Tracheal, bronchus, and lung cancer" ~ "lung_cancer", 
    cause == "Breast cancer" ~ "breast_cancer", 
    cause == "Colon and rectum cancer" ~ "colon_cancer", 
    cause == "Bladder cancer" ~ "bladder_cancer", 
    cause == "Esophageal cancer" ~ "esophageal_cancer",
    cause == "Liver cancer" ~ "liver_cancer", 
    cause == "Chronic obstructive pulmonary disease" ~ "COPD",
    cause == "Parkinson's disease"  ~ "parkinson’s_disease",
    .default = cause)) %>% 
  mutate(val = case_when(
    cause == "colon_cancer" ~ val * 2/3,  
    cause == "lung_cancer" ~ val * 0.54,   
    TRUE ~ val                             
  ))


# Sum rates for head an neck cancers
hanc <- c("Larynx cancer", "Lip and oral cavity cancer", "Nasopharynx cancer",
          "Other pharynx cancer")

gbdp_hanc <- gbdp %>%
  filter(cause %in% hanc) %>%
  group_by(measure, location, sex, agegroup, from_age, to_age, metric, year, agediff) %>%
  summarise(val = sum(val),
            rate_1 = sum(rate_1),
            val1yr = sum(val1yr),.groups = "drop") %>%
  mutate(cause = "head_and_neck_cancer") %>% 
  select(1:6,13,7,8,10,11,9,12)

gbdp <- gbdp %>%
  filter(!cause %in% hanc) %>%
  bind_rows(gbdp_hanc) 

}