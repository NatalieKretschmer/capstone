setwd("/Users/New Natalie/Desktop/SP23/capstone/data")
library(data.table)
library(tidyverse)
library(tidycensus)

## pull data
adhd_treat <- read_csv("Percent of children (aged 3–17 years) with current ADHD who receive treatment.csv")
adhd_diag <- read_csv("Percent of children (aged 3–17 years) with ADHD.csv")
ed_fund <- read_csv("ed funding per student.csv")
#age_sex <- read_csv("age and sex 2021.csv")
#insurance <- read_csv("health insurance by age.csv")
#poverty <- read_csv("poverty.csv")
#race <- read_csv("race 2021.csv")
political_COL <- read_csv("political_COL.csv")
acs_data <- read.table("usa_00005.dat")

readLines("usa_00005.dat", 
          n=10)

fips_state <- fips_codes %>%
  distinct(state, state_code, state_name)

## clean, and merge adhd diagnosis data
adhd_diag[, 4] <- sapply(adhd_diag[, 4], as.numeric)
colnames(adhd_diag)[3] = "status"

adhd_diag_current <- filter(adhd_diag, adhd_diag$status == "Current")
adhd_diag_ever <- filter(adhd_diag, adhd_diag$status == "Ever")

adhd_diag <- inner_join(adhd_diag_current, adhd_diag_ever, by = "State", suffix = c("_current", "_ever"))
adhd_diag_current <- NULL
adhd_diag_ever <- NULL

colnames(adhd_diag)[1] = "state"
adhd_diag <- merge(adhd_diag,
                   fips_state,
                   by = "state")
adhd_diag <- adhd_diag %>%
  select(
    -starts_with("status"),
    -starts_with("Range"),
    -starts_with("Year")
  )

#head(adhd_diag)

## clean, and merge adhd treatment data
head(adhd_treat)
colnames(adhd_treat)[1] = "state"
colnames(adhd_treat)[3] = "treatment_type"

adhd_treat_BT <- filter(adhd_treat, adhd_treat$treatment_type == "ADHD Behavior Treatment")
adhd_treat_med <- filter(adhd_treat, adhd_treat$treatment_type == "ADHD Medication")
adhd_treat_medBT <- filter(adhd_treat, adhd_treat$treatment_type == "ADHD Meds or BT")
adhd_treat_any <- filter(adhd_treat, adhd_treat$treatment_type == "Any Treatment")

adhd_treat_1 <- inner_join(adhd_treat_BT, adhd_treat_med, by = "state", suffix = c("_BT", "_med"))
adhd_treat_2 <- inner_join(adhd_treat_medBT, adhd_treat_any, by = "state", suffix = c("_med_or_BT", "_any"))

adhd_treat <- merge(adhd_treat_1,
                    adhd_treat_2,
                    by = "state")
adhd_treat <- adhd_treat %>%
  select(
    -starts_with("treatment_type"),
    -starts_with("Range"),
    -starts_with("Year")
    )

adhd_treat_1 <- NULL
adhd_treat_2 <- NULL
adhd_treat_BT <- NULL
adhd_treat_med <- NULL
adhd_treat_medBT <- NULL
adhd_treat_any <- NULL

## centralize all adhd data
adhd <- merge(adhd_diag,
              adhd_treat,
              by = "state")

## clean and merge ed, policy and COL data
colnames(adhd)[4] = "fips"

big_data <- merge(adhd,
                  ed_fund,
                  by = "fips")
colnames(big_data)[2] = "state"

colnames(political_COL)[2] = "COL_index"
colnames(political_COL)[3] = "dem_pct"
colnames(political_COL)[4] = "rep_pct"
colnames(political_COL)[5] = "other_pct"

big_data <- merge(big_data,
                  political_COL,
                  by = "state")
big_data <- big_data %>%
  mutate(
    per_pupil_by_COL = amountPerPupil/COL_index,
    pol_temp = dem_pct-rep_pct
    # pol temp: positive = dem, negative = rep
  )


## plots n' regs
big_data %>%
  ggplot(aes(reorder(state, Percentage_current), Percentage_current)) +
  geom_col(color = "lightblue", fill = "navy") 

big_data %>%
  ggplot(aes(reorder(state, Percentage_any), Percentage_any)) +
  geom_col(color = "lightblue", fill = "navy") 


big_data %>%
  ggplot(aes(pol_temp, Percentage_current)) +
  geom_point() +
  geom_smooth(method = "lm")

big_data %>%
  ggplot(aes(per_pupil_by_COL, Percentage_current)) +
  geom_point() +
  geom_smooth(method = "lm")

big_data %>%
  ggplot(aes(pol_temp, Percentage_any)) +
  geom_point() +
  geom_smooth(method = "lm")

big_data %>%
  ggplot(aes(per_pupil_by_COL, Percentage_any)) +
  geom_point() +
  geom_smooth(method = "lm")

OLS_diag <- lm(formula = Percentage_current ~ per_pupil_by_COL + pol_temp, data = big_data)
summary(OLS_diag)

OLS_treat <- lm(formula = Percentage_any ~ per_pupil_by_COL + pol_temp, data = big_data)
summary(OLS_treat)





