                                        # recodings acs_microdata to CCES

recode_education <- function(x) {
    case_when(
        x < 16 ~ "No HS",
        x >= 16 & x <= 17 ~ "High School Graduate",
        x > 17 & x <= 19 ~ "Some College",
        x == 20 ~ "2-Year",
        x == 21 ~ "4-Year",
        x > 21 ~ "Post_Grad"
    )
}
