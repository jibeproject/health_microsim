require(tidyverse)

manchester_folder <- "/media/ali/Expansion/backup_tabea/manchester-main/"
base <- read_csv(paste0(manchester_folder, "input/health/pp_exposure_2021_base_140725.csv"))
ss <- read_csv(paste0(manchester_folder, "input/health/pp_exposure_2021_safeStreet_300725.csv"))
both <- read_csv(paste0(manchester_folder, "input/health/pp_exposure_2021_both_010825.csv"))
green <- read_csv(paste0(manchester_folder, "input/health/pp_exposure_2021_green_310725.csv"))
agp <- read_csv(paste0(manchester_folder, "input/accident/age_gender_rr.csv"))

agp <- agp |>  
  mutate(gender = case_when(
    gender == "Male" ~ 1,
    gender == "Female" ~ 2
  ))

add_agroup <- function(df){
  return(df |> 
           mutate(
             age_group = as.character(cut(
               age,
               breaks = c(-Inf, 16, 20, 29, 39, 49, 59, 69, Inf),
               labels = c("< 17", "17-20", "21-29", "30-39", "40-49", "50-59", "60-69", "70+"),
               right = TRUE
             )
             )))
}

calc_wshare <- function(df, agp, scen = "base"){
  # df <- ss
  # scen <- 'ss'
  cf <- 0
  if (scen == "base"){
    cf <- 0.741037452
  } else if (scen == "ss"){
    cf <- 0.734002674
  } else if (scen == "green"){
    cf <- 0.720561993
  } else if (scen == "both"){
    cf <- 0.714050054
  }
  df <- df |> left_join(filter(agp, mode == "Walk"), by = c("age_group", "gender")) |> 
    mutate(injuryRiskWalk = severeFatalInjuryWalk * RR * cf,
           fRiskWalk = severeFatalInjuryWalk * RR * cf * percent_killed)
  print(scen)
  print(paste("Casualties: ", round(sum(df$injuryRiskWalk), 2)))
  print(paste("Fatalities: ", round(sum(df$fRiskWalk), 2)))
  print(paste("Injuries: ", round(sum(df$injuryRiskWalk) - sum(df$fRiskWalk), 2)))
}

base <- add_agroup(base)
ss <- add_agroup(ss)
both <- add_agroup(both)
green <- add_agroup(green)

calc_wshare(base, agp, scen = "base")
calc_wshare(ss, agp, scen = "ss")
calc_wshare(green, agp, scen = "green")
calc_wshare(both, agp, scen = "both")



# sswi <- ss |> left_join(filter(agp, mode == "Walk"), by = c("age_group", "gender")) |> 
#   mutate(injuryRiskWalk = severeFatalInjuryWalk * RR * 0.734002674,
#          fRiskWalk = severeFatalInjuryWalk * RR * 0.734002674 * percent_killed)
# print(sum(sswi$injuryRiskWalk))
# print(sum(sswi$fRiskWalk))
