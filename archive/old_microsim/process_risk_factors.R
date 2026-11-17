# Replace dash with underscore
# Add missing causes
# Rename risk disease
disease_risks <- read_csv("jibe health/health/disease risks.csv")

# Replace dash in outcome with underscore
disease_risks$outcome <- gsub("-", "_", disease_risks$outcome)

# Rename parkinsons_disease to parkinson
disease_risks[disease_risks$risk_factor == "parkinsons_disease",]$risk_factor <- "parkinson"

# # Add a new row for myeloid_leukemia that has cancer risks
# disease_risks <- rbind(disease_risks,
#       disease_risks |>  
#         filter(risk_factor == "cancer") |> 
#         mutate(risk_factor = "myeloid_leukemia"))
# 
# # Add a new row for myeloma that has cancer risks
# disease_risks <- rbind(disease_risks,
#                        disease_risks |>  
#                          filter(risk_factor == "cancer") |> 
#                          mutate(risk_factor = "myeloma"))


# Write this to health folder as mod_disease_risks
write_csv(disease_risks, "jibe health/mod_disease_risks.csv")
