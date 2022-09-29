library(haven)
library(discord)
library(dplyr)
df_links <- read.csv("siblinks.csv", sep = ",")
df_p2018_c <- read_sas("cfps2018person_202012.sas7bdat")
df_c2018_c <- read_sas("cfps2018childproxy_202012.sas7bdat")

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
colnames(df_links_clean)[8] <- "edu_S1"
colnames(df_links_clean)[12] <- "edu_S2"


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
# run discord model

df_final <- discord_data(data = df_links_clean,
             outcome = "MH",
             predictors = c("mathtest18","edu","birthyear","wordtest18"),
             id = "rowid",
             sex = NULL,
             race = NULL,
             pair_identifiers = c("_S1", "_S2"),
             demographics = "none")

# mathtest 
lm(MH_mean~edu_mean+birthyear_mean+mathtest18_mean, data = df_final) |> prettify_regression_results()
lm(MH_diff~edu_mean+birthyear_mean+MH_mean+mathtest18_mean+mathtest18_diff, data = df_final) |> prettify_regression_results()

lm(MH_mean~edu_mean+birthyear_mean+wordtest18_mean, data = df_final) |> summary()
lm(MH_diff~edu_mean+birthyear_mean+MH_mean+wordtest18_mean+wordtest18_diff, data = df_final) |> summary()
