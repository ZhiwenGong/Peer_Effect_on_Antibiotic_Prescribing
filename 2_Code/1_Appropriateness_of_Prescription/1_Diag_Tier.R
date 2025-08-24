# R code
# identify the category of disease conditions related to antibiotic prescription


# -- 0. package & path
library(dplyr)
library(stringr)

dir_work <- "C:/Users/AMD/Desktop/Peer_Effect_on_Antibiotic_Prescribing"
dir_data_raw <- file.path(dir_work, "1_Data/1_Data_Raw")
dir_data_clean <- file.path(dir_work, "1_Data/2_Data_Clean")
dir_code <- file.path(dir_work, "2_Code")

# import data
df_atb_pres_with_diag <- read.csv(file.path(dir_data_raw, "atb_pres_with_diag.csv"))

df_atb_pres_with_diag <-df_atb_pres_with_diag %>%
  mutate(diag_std = "/")



# -- 1. identify the category
# Function: for standardizing diagnosis categories
f_diag_standardizing <- function(df, icd, category) {
  
  # create a regex pattern that matches any of the icd codes
  pattern_regex <- paste(gsub("\\.", "\\\\.", icd), collapse = "|")
  
  # reset diag_std to empty for rows matching reset icd
  df$diag_std[df$diag_std == "/" & grepl(pattern_regex, df$icd)] <- category

  return(df)
}


# Tier 1
# certain bacterial diseases
icd <- c("A01","A02.1","A04.7","A06","A1","A2","A3","A4","A5","A65","A66","A67","A68","A69","A7","B58","B59","B60","B90","B92","B94.0","B95","B96","R65.201","U80","U81","U88","U89")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "1_certain")
# exception
icd <- c("A49.800","A49.801","A49.900","B60.8","B90.001","B90.101","B90.201","B90.801","B90.802","B90.803","B90.901","B90.902","B90.903","B90.904","B96.2")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# infectious diseases of the blood, blood- forming organs and infectious diseases of endocrine, and metabolic system
icd <- c("D73.0","E06.002","E10.505","E11.505","E23.601","E27.802","E32.1")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "1_blood")

# infectious diseases of the nervous system
icd <- c("G00","G01","G04.2","G04.805","G04.807","G04.901","G06","G07")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "1_nervous")

# infectious diseases of the eye and adnexa, ear and mastoid process
icd <- c("H05.0","H60.2","H70","H75.0","H95.1")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "1_eye")

# infectious diseases of the circulatory system
icd <- c("I00","I01","I02","I30.101","I30.102","I33","I40.002","I70.202","I70.205","I70.207","I77.806","I96")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "1_circulatory")

# pneumonia
icd <- c("J13","J14","J15","J16","J17","J18","P23.1","P23.6")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "1_pneumonia")

# other bacterial infectious diseases of the respiratory system
icd <- c("J32.004","J32.102","J32.802","J32.905","J34.007","J36","J38.305","J38.707","J38.716","J39.0","J39.1","J85","J86","J98.501","J98.503")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "1_respiratory")

# infectious diseases of oral cavity and salivary glands
icd <- c("K04.003","K04.6","K04.7","K05.201","K05.202","K05.203","K11.3","K12.2","K14.001")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "1_oral")

# infectious diseases of the digestive system
icd <- c("K22.3","K25.1","K25.2","K25.5","K25.6","K26.1","K26.2","K26.5","K26.6","K27.1","K27.2","K27.5","K27.6","K28.1","K28.2","K28.5","K28.6","K35","K36","K37","K40.1","K40.4","K41.1","K41.4","K42.1","K43.1","K44.1","K45.1","K46.1","K55.005","K57.0","K57.106","K57.107","K57.108","K57.2","K57.304","K57.305","K57.4","K57.8","K63.0","K63.1","K65","K75.0","K80.0","K80.1","K80.2","K80.3","K80.4","K81","K82.2","K83.0","K83.2","K85.817","K85.818","K85.824","K85.825","K90.1")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "1_digestive")
# exception
icd <- c("K65.804","K65.806")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# infectious diseases of the skin and subcutaneous tissue
icd <- c("L00")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "1_skin")

# infectious diseases of the musculoskeletal system and connective tissue
icd <- c("M00","M46.2","M46.3","M46.501","M46.502","M46.503","M46.504","M46.801","M60.005","M60.006","M60.008","M65.0","M65.1","M72.6","M86","M94.805")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "1_musculoskeletal")
# exception
icd <- c("M86.801","M86.804","M86.805","M86.809","M86.921","M86.923")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# urinary tract infections
icd <- c("N10.x02","N13.6","N15.1","N15.901","N28.834","N30.0","N30.3","N30.801","N34.0","N34.101","N34.204","N34.205","N39.0")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "1_urinary")
# exception
icd <- c("N39.001")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# infectious diseases of the genitourinary system
icd <- c("N41.0","N41.2","N43.1","N45.0","N48.201,","N48.203","N49.102","N49.201","N49.202","N49.204","N70","N71","N72","N73","N74","N98.0")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "1_genitourinary")
# exception
icd <- c("N70.103","B70.104","N73.6","N73.801")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# infectious diseases related to pregnancy, childbirth and the puerperium and infectious conditions originating in the perinatal period
icd <- c("O03.0","O03.5","O04.0","O04.5","O05.0","O05.5","O06.0","O06.5","O07.0","O07.5","O08.0","O04.804","O23","O41.1","O85","O86","O99.6","P02.7","P38","P39","P77")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "1_pregnancy")
# exception
icd <- c("O88.3","O23.503","O23.504","O23.505","O23.506","O86.4","P39.402","P39.403","P39.8","P39.9")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# all other conditions that always justify antibiotics (Symptoms, signs and abnormal clinical and laboratory findings of infectious diseases)
icd <- c("R02","S[0-9]{2}.[0-9]{1}1[0-9]{1}","S05.7","S[0-9]{1}8","T80.2","T82.6","T82.7","T83.5","T83.6","T84.5","T84.6","T84.7","T85.7","T87.4","T87.5","W53","W54","W55","W56","W57","W58","W59")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "1_other")


# Tier 2
# other infectious diseases of the blood, bloodforming organs and infectious diseases of endocrine, and metabolic system
icd <- c("D57.0","D57.8","D70","E06.0","E06.9","E84.0","E84.9")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_blood")
# exception
icd <- c("E06.002")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# other infectious diseases of the nervous system
icd <- c("G03.9","G04","G05","G06.006","G08")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_nervous")
# exception
icd <- c("G03.907","G04.2","G04.805","G04.806","G04.807")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# acute otitis media
icd <- c("H66")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_otitis")

# other infectious diseases of the eye and adnexa, ear and mastoid process
icd <- c("H00.2","H00.3","H01.0","H02.808","H04.0","H04.3","H04.4","H05.1","H05.5","H16","H20","H30","H44.0","H44.1","H44.6","H44.7","H46","H60","H61.0","H72","H73","H83.0")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_eye")
# exception
icd <- c("H60.2","H73.800","H73.801","H73.804","H73.9")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# other infectious diseases of the circulatory system
icd <- c("I30","I32","I39","I40","I41.0","I80","I88","I89.1")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_circulatory")
# exception
icd <- c("I88.0")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# acute pharyngitis
icd <- c("J02","J03")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_pharyngitis")

# chronic obstructive pulmonary disease, COPD
icd <- c("J44")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_copd")

# acute sinusitis
icd <- c("J01")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_sinusitis")

# other infectious diseases of the respiratory system
icd <- c("J04.004","J04.1","J05.1","J10.000","J10.100","J11.000","J11.100","J22","J31.2","J32","J34.0","J35.0","J41","J42","J43","J47","J69.001","J90","J91","J95","J98")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_respiratory")
# exception
icd <- c("J98.0","J98.1","J98.2","J98.3","J98.6","J98.9")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# other infectious diseases of oral cavity and salivary glands
icd <- c("K04.0","K04.1","K04.2","K04.3","K04.4","K04.5","K05","K10.3","K11.2","K12.111","K13.011")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_oral")

# infectious gastroenteritis
icd <- c("A00","A02.0","A03","A04","A05","A07","A09")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_gastroenteritis")
# exception
icd <- c("A04.3","A04.7")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# other infectious diseases of the digestive system
icd <- c("K20","K25","K26","K27","K31.6","K38.3","K50","K51","K55","K60","K61","K33.2","K75.1","K82.0","K82.3","K83.1","K33.3","K91.8")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_digestive")
# exception
icd <- c("K55.1","K55.2","K55.8","K55.9","K91.800","K91.801","K91.803","K91.804","K91.805","K91.806")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# impetigo
icd <- c("L01")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_impetigo")

# acne
icd <- c("L70")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_acne")

# other infectious diseases of the skin and subcutaneous tissue
icd <- c("L02","L03","L04","L05","L08","L30.3","L60.0","L71","L73.804","L73.805","L88","L89","L97","L98")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_skin")

# other infectious diseases of the musculoskeletal system and connective tissue
icd <- c("M02","M46.4","M46.500","M54.0","M60.0","M60.8","M65.0","M60.9","M71.0","M71.1","M79.3","M79.5","M94.801")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_musculoskeletal")
# exception
icd <- c("M60.005","M60.006","M60.008")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# other infectious diseases of the genitourinary system
icd <- c("N10","N11","N12.x02","N13.204","N13.7","N20.901","N28.8","N30.1","N30.2","N30.8","N30.9","N32.1","N34.1","N34.2","N39.001","N41.1","N41.3","N41.8","N41.9","N45.9","N48.1","N48.2","N48.5","N49","N49.9","N61","N75.0","N75.1","N75.802","N75.9","N76","N77","N82","N85.8")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_genitourinary")
# exception
icd <- c("N30.801","N34.204","N34.205","N49.102","N49.201","N49.202","N49.204")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# other infectious diseases related to pregnancy, childbirth and the puerperium and infectious conditions originating in the perinatal period
icd <- c("O08.3","O22.2","O29.0","O42","O70","O71","O74.0","O75","O86.4","O87.0","O89.0","O91","O98","O99","P23.8","P23.9","P24","P37.8","P37.9","P39.4","P39.8")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_pregnancy")

# systemic inflammatory response syndrome (SIRS)
icd <- c("R65.0","R65.1","R65.9")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_sirs")

# other infectious diseases not listed above
icd <- c("A02.2","A02.8","A02.9","A49.800","A49.801","A49.900","A64","B55","B60.8","B64","B85","B89","B99")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_other")

# symptoms, signs and abnormal clinical and laboratory findings of other infectious diseases
icd <- c("R57","R68.801","R82.7","R83.5","R84.5","R85.5","R86.5","R87.5","R89.5","S[0-9]{2}","T2","T30","T31","T32","T33","T34","T35","T74.2","T79.3","T81.1","T81.4","T81.5","T86.807","T87.001","W44","W45","W46","Z20","Z22")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "2_symptoms")


# Tier 3
# viral infections
icd <- c("A08","A60","A63.0","A8","A9","B0","B1","B2","B30","B31","B32","B33","B34")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "3_viral")

# fungal infections 
icd <- c("B35","B36","B37","B38","B39","B4")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "3_fungal")

# non-infectious otitis media
icd <- c("H65")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "3_otitis")
# exception
icd <- c("H65.101")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "/")

# viral upper respiratory infection (URI)
icd <- c("J00","J04","J05","J06")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "3_uri")

# influenza
icd <- c("J09","J10","J11")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "3_influenza")

# acute bronchitis
icd <- c("J20","J21")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "3_bronchitis")

# allergy and asthma
icd <- c("J45","J30.1","J30.2","J30.3","J30.4","J39.3")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "3_allergy")

# non-infectious gastroenteritis
icd <- c("K29","K52")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "3_gastroenteritis")

# non-specific symptoms, signs of respiratory system
icd <- c("R04","R06","R07","R09")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "3_respiratory")

# cough
icd <- c("R05")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "3_cough")

# non-specific symptoms, signs of digestive system
icd <- c("R10","R11","R12","R13","R14","R15","R16","R17","R18","R19")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "3_digestive")

# fever
icd <- c("R50")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "3_fever")

# procedures and surgeries not included elsewhere
icd <- c("O04.9","Z98.8")
df_atb_pres_with_diag <- f_diag_standardizing(df_atb_pres_with_diag, icd, "3_procedures")

# all other conditions
df_atb_pres_with_diag$diag_std[df_atb_pres_with_diag$diag_std == "/"] <- "3_other"



# 2. determine the main diagnosis category for each antibiotic prescription
df_atb_pres_with_diag <- df_atb_pres_with_diag %>%
  mutate(main_tier = as.integer(str_extract(diag_std, "^[1-3]+"))) %>%
  group_by(pres_code) %>%
  slice_min(order_by = main_tier, n = 1, with_ties = FALSE) %>% 
  ungroup()



write.csv(df_atb_pres_with_diag, file.path(dir_data_raw, "output.csv"), row.names = FALSE)