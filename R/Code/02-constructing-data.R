# Reproducible Research Fundamentals 
# 02. Data construction
# RRF - 2024 - Construction

# Preliminary - Load Data ----
# Load household-level data (HH)
hh_data <- read_dta(file.path(data_path, "Intermediate/TZA_CCT_HH.dta"))

# Load HH-member data
mem_data <- read_dta(file.path(data_path, "Intermediate/TZA_CCT_HH_mem.dta"))

# Load secondary data
secondary_data <- read_dta(file.path(data_path, "Intermediate/TZA_amenity_tidy.dta"))

# Exercise 1: Plan construction outputs ----
# Plan the following outputs:
# 1. Area in acres. (ar_farm times ar_unit)
# 2. Household consumption (food and nonfood) in USD.(rowsum(food_cons nonfood_cons)ignore NAS)
# 3. Any HH member sick.(collapse mem by house counting)
# 4. Any HH member can read or write.("")
# 5. Average sick days. (collaps mem by housing averaging (ignoring NAS) the v ar)
# 6. Total treatment cost in USD.("")
# 7. Total medical facilities.(external)

#0. standardize val
acre_conv <- 2.47
usd <- 0.00037


# Exercise 2: Standardize conversion values ----
# Define standardized conversion values:
# 1. Conversion factor for acres.
# 2. USD conversion factor.
# 1. area in acres

hh_data <- hh_data %>% mutate(area_acre = case_when(ar_unit==1 ~ ar_farm,
                                                    ar_unit==2 ~ ar_farm * acre_conv
))%>%
    mutate(area_acre =replace_na(area_acre,0))%>%
    set_variable_labels(area_acre="Area farmed in acres")



#2.
hh_data <- hh_data %>% 
    mutate(across(c(food_cons,nonfood_cons),
                  ~ .x *usd,
                  .names="{.col}_usd"))


# Data construction: Household (HH) ----
# Instructions:
# 1. Convert farming area to acres where necessary.
# 2. Convert household consumption for food and nonfood into USD.

# Exercise 3: Handle outliers ----
# you can use custom Winsorization function to handle outliers.
winsor_function <- function(dataset, var, min = 0.00, max = 0.95) {
    var_sym <- sym(var)
    
    percentiles <- quantile(
        dataset %>% pull(!!var_sym), probs = c(min, max), na.rm = TRUE
    )
    
    min_percentile <- percentiles[1]
    max_percentile <- percentiles[2]
    
    dataset %>%
        mutate(
            !!paste0(var, "_w") := case_when(
                is.na(!!var_sym) ~ NA_real_,
                !!var_sym <= min_percentile ~ percentiles[1],
                !!var_sym >= max_percentile ~ percentiles[2],
                TRUE ~ !!var_sym
            )
        )
}

# Tips: Apply the Winsorization function to the relevant variables.
# Create a list of variables that require Winsorization and apply the function to each.
win_vars <- c("area_acre","food_cons_usd","nonfood_cons_usd")

for (vars in win_vars) {
    hh_data <- winsor_function(hh_data,vars)
    
}

hh_data <- hh_data %>% mutate(across(ends_with("_w"),
                                     ~labelled(.x,label=paste0(attr(.x,"label"),
                                                               "(winsorrized 0.05)"))))

# Exercise 4.1: Create indicators at household level ----
# Instructions:
# Collapse HH-member level data to HH level.
# Plan to create the following indicators:
# 1. Any member was sick.
# 2. Any member can read/write.
# 3. Average sick days.
# 4. Total treatment cost in USD.


hh_member_collapsed <- mem_data %>% group_by(hhid) %>%
    summarise(sick=max(sick,na.rm=T),
              read=max(read,na.rm=T),
              days_sick= ifelse(all(is.na(days_sick)),NA_real_,mean(days_sick,na.rm=T)),
              treat_cost_usd = ifelse(all(is.na(treat_cost)),NA_real_,sum(treat_cost,na.rm = T)*usd)
              )%>%
    ungroup()%>%
    mutate(treat_cost_usd=if_else(is.na(treat_cost_usd),mean(treat_cost_usd,na.rm=T),treat_cost_usd))%>%
    set_variable_labels(
        read="Any member can read or write"
    )



# Exercise 4.2: Data construction: Secondary data ----
# Instructions:
# Calculate the total number of medical facilities by summing relevant columns.
# Apply appropriate labels to the new variables created.

secondary_data<- secondary_data %>%
    mutate(n_mededical= rowSums(select(.,n_hospital,n_clinic), na.rm = T)) 

var_label(secondary_data$n_mededical)<- "No. of medical facilities"

# Exercise 5: Merge HH and HH-member data ----
# Instructions:
# Merge the household-level data with the HH-member level indicators.
# After merging, ensure the treatment status is included in the final dataset.

final_hh_data <- hh_data %>% 
    left_join(.,hh_member_collapsed,by=c("hhid"))

terat_status <- read_dta(file.path(data_path,"Raw/treat_status.dta"))

final_hh_data <- final_hh_data %>%
    left_join(terat_status,by=c("vid"))

# Exercise 6: Save final dataset ----
# Instructions:
# Only keep the variables you will use for analysis.
# Save the final dataset for further analysis.
# Save both the HH dataset and the secondary data.

write_dta(final_hh_data,file.path(data_path,"Final/TZA_CCT_analysis.dta"))
write_dta(secondary_data,file.path(data_path,"Final/TZA_amenity_analysis.dta"))

# Tip: Ensure all variables are correctly labeled 

