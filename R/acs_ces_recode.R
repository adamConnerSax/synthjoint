# recoding acs_microdata to CCES
library(ccesMRPprep)

#SCHL
# acs_microdata %>% mutate(Edu6 = recode_education(SCHL))
recode_education <- function(x) {
    case_when(
        x < 16 ~ "No HS",
        x %in% c(16, 17) ~ "High School Graduate",
        x %in% c(17, 18, 19) ~ "Some College",
        x == 20 ~ "2-Year",
        x == 21 ~ "4-Year",
        x > 21 ~ "Post-Grad"
    )
}

#SCHL
# acs_microdata %>% mutate(Edu4 = recode_education4(SCHL))
recode_education4 <- function(x) {
    case_when(
        x <= 17 ~ "HS or Less",
        x> 17 & x < 21 ~ "Some College",
        x == 21 ~ "4-Year",
        x > 21 ~ "Post-Grad"
    )
}

# acs_microdata %>% filter(AGEP > 17) %>% mutate(Age5 = recode_age5(AGEP))
recode_age5 <- function(x) {
    case_when(
        x > 17 & x < 25 ~ "18 to 24 years",
        x > 24 & x < 35 ~ "25 to 34 years",
        x > 34 & x < 45 ~ "35 to 45 years",
        x > 44 & x < 65 ~ "45 to 64 years",
        x > 64 ~ "65 years and over"
    )
}

# acs_microdata %>% mutate(Sex2 = recode_sex2(SEX))
recode_sex2 <- function(x) {
    case_when(
        x == 1 ~ "Male",
        x == 2 ~ "Female"
    )
}

# acs_microdata %>% mutate(Race8 = recode_race8(RAC1P, HISP))
recode_race8 <- function(x, y) {
    case_when(
        y > 1 ~ "Hispanic",
        x == 1 ~ "White",
        x == 2 ~ "Black",
        x %in% c(3, 4, 5) ~ "Native American",
        x %in% c(6, 7) ~ "Asian",
        x == 8 ~ "Other",
        x == 9 ~ "Mixed",
        .default = "Middle Eastern"
    )
}

recode_PUMS <- function(x) {
    x %>%
        filter(AGEP > 17) %>%
        mutate(Age5=recode_age5(AGEP), Sex2=recode_sex2(SEX), Edu4=recode_education4(SCHL), Race8=recode_race8(RAC1P, HISP)) %>%
        select(PWGTP,PUMA,ST,Age5,Sex2,Edu4,Race8)
}

collapse_PUMS <- function(x) {
    x %>% collapse_table(area_var="PUMA",count_var="PWGTP",X_vars=c("Age5","Sex2","Edu4","Race8"))
}
