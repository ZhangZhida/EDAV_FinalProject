library(readr)
library(tidyverse)

## Data preparation
make_college_rds = function(path = "MERGED2014_15_PP.csv", output = "college.rds"){
  ## loads, cleans up, and saves the college data as .csv
  
  # 1. clean up the data set
  college <- read_csv("MERGED2014_15_PP.csv") 
  var = c("UNITID","OPEID","MAIN","NUMBRANCH","INSTNM","CITY",
          "STABBR","ZIP","PREDDEG","HIGHDEG","CONTROL","SAT_AVG",
          "ADM_RATE","TUITIONFEE_IN","TUITIONFEE_OUT","PCTFLOAN","AVGFACSAL",
          "PFTFAC","D_PCTPELL_PCTFLOAN","GRADS","C150_4","FAMINC","MD_FAMINC",
          "MN_EARN_WNE_P10","MD_EARN_WNE_P10","FEMALE","UGDS_WHITE","UGDS_BLACK",
          "UGDS_HISP","UGDS_ASIAN","UGDS_AIAN","UGDS_NHPI","UGDS_2MOR","UGDS_NRA",
          "UGDS_UNKN")
  college <- subset(college, select = var)
  colnames(college) = c("campus_id","college_id","main_campus",
                        "num_branch","name","city","state","zip",
                        "predominant_degree","highest_degree","ownership",
                        "sat_avg", "admission_rate",
                        "tuition_instate","tuition_out",
                        "pct_loan","avg_faculty_salary","pct_faculty", "num_undergrad",
                        "num_grad","completion_rate","avg_family_income",
                        "median_family_income","avg_10yr_salary","med_10yr_salary",
                        "pct_female","race_white", "race_black", "race_hispanic", 
                        "race_asian", "race_native", "race_pacific", "race_2more",
                        "race_nonresident","race_unknown")
  
  # check classes of features
  sapply(college, class)
  class_char <- c("campus_id")
  class_fact <- c("main_campus", "predominant_degree", "highest_degree", "ownership")
  class_int <- c("tuition_instate", "tuition_out", "num_undergrad", "num_grad")
  class_num <- c("sat_avg", "admission_rate", "pct_loan", "avg_faculty_salary", 
                 "pct_faculty", "completion_rate", "avg_family_income", 
                 "median_family_income", "avg_10yr_salary","med_10yr_salary", "pct_female", 
                 "race_white", "race_black", "race_hispanic", "race_asian", "race_native", 
                 "race_pacific", "race_2more", "race_nonresident", "race_unknown")
  college <- college %>% mutate_at(class_char, as.character)  %>% 
    mutate_at(class_fact, as.factor) %>%
    mutate_at(class_int, as.integer) %>%
    mutate_at(class_num, as.numeric)
  sapply(college, class)
  
  # preliminary check of missing values
  sum(is.na(college)) #49430
  college[college == 'NULL'] <- NA
  
  saveRDS(college, output) 
}

make_college_rds()
college = read_rds("college.rds")
