library(haven)
library(discord)
library(tidyverse)

df_links <- read.csv("data/siblinks.csv", sep = ",")
df_p2018_c <- read_sas("data/cfps2018person_202012.sas7bdat")
df_c2018_c <- read_sas("data/cfps2018childproxy_202012.sas7bdat")

# functions
prettify_regression_results <- function(regression_object) {
    regression_object %>%
        gtsummary::tbl_regression(intercept=TRUE) %>%
        gtsummary::add_glance_source_note(
            label = list(statistic ~ "F-statistic",
                         df  ~ "DF1",
                         df.residual  ~ "DF2"),
            include = c(r.squared, statistic, df, df.residual, p.value, nobs)
        ) %>%
        gtsummary::modify_header(
            statistic ~ "**t-statistic**", p.value ~ "**p-value**"
        )
}


#data cleaning

df_target <- df_p2018_c[,c("PID", 
                           #("QM2..$", names(df_p2018_c), value=TRUE), #personality
                           grep("QN4..$", names(df_p2018_c), value=TRUE, ignore.case = TRUE), #mental health
                           #grep("Q..T...$", names(df_p2018_c), value=TRUE, ignore.case = TRUE), 
                           #grep("QM1..M$", names(df_p2018_c), value=TRUE, ignore.case = TRUE), #self-esteem
                           #grep("QM5..$", names(df_p2018_c), value=TRUE, ignore.case = TRUE), #life attitudes
                           #grep("QN100.", names(df_p2018_c), value=TRUE, ignore.case = TRUE),# trust tendency
                           #grep("WV10.$", names(df_p2018_c), value=TRUE, ignore.case = TRUE),# attitudes towards society
                           "mathtest18", # math test
                           "wordtest18",
                           "CFPS2018EDUY_IM"# word test
),]

df_target[df_target <= -1] <- as.numeric(NA)
df_target$QN406_r <- 5 - df_target$QN406
df_target$QN407_r <- 5 - df_target$QN407
df_target$qn411_r <- 5 - df_target$qn411
df_target$qn414_r <- 5 - df_target$qn414
df_target$qn418_r <- 5 - df_target$qn418
df_target$qn420_r <- 5 - df_target$qn420

df_target$MH <- apply(df_target[,c(6,8,14:19)],1,mean)

df_target_c <- df_target[,c(1,11:13,20)]


df_links_sib <- df_links
df_links_sib <- merge(df_links_sib, df_target_c, by.x = "X1", by.y = "PID", all.x = TRUE)
colnames(df_links_sib)[11:14] <- paste(colnames(df_target_c)[2:5], "_S1", sep = "")
df_links_sib <- merge(df_links_sib, df_target_c, by.x = "X2", by.y = "PID", all.x = TRUE)
colnames(df_links_sib)[15:18] <- paste(colnames(df_target_c)[2:5], "_S2", sep = "")

df_links_sib$rowid <- 1:nrow(df_links_sib)
df_links_sib <- df_links_sib[,c(19,8,5,10,7,11:18)]
df_links_clean <- df_links_sib[complete.cases(df_links_sib),]
#df_links_clean <- df_links_clean[df_links_clean$birthyear_S1<2000 & df_links_clean$birthyear_S2<2000,]
#df_links_clean$gender_S1 <- as.factor(df_links_clean$gender_S1)
#df_links_clean$gender_S2 <- as.factor(df_links_clean$gender_S2)

#
df_links_clean <- df_links_clean %>%
rename(edu_S1 = CFPS2018EDUY_IM_S1,
       edu_S2 = CFPS2018EDUY_IM_S2)


