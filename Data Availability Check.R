library(haven)
df_links <- read.csv("siblinks.csv", sep = ",")
df_p2018_c <- read_sas("cfps2018person_202012.sas7bdat")
df_c2018_c <- read_sas("cfps2018childproxy_202012.sas7bdat")

df_target <- df_p2018_c[,c("PID", 
                           #("QM2..$", names(df_p2018_c), value=TRUE), #personality
                           grep("QN4..$", names(df_p2018_c), value=TRUE, ignore.case = TRUE), #mental health
                           #grep("Q..T...$", names(df_p2018_c), value=TRUE, ignore.case = TRUE), 
                           grep("QM1..M$", names(df_p2018_c), value=TRUE, ignore.case = TRUE), #self-esteem
                           grep("QM5..$", names(df_p2018_c), value=TRUE, ignore.case = TRUE), #life attitudes
                           grep("QN100.", names(df_p2018_c), value=TRUE, ignore.case = TRUE),# trust tendency
                           grep("WV10.$", names(df_p2018_c), value=TRUE, ignore.case = TRUE),# attitudes towards society
                           "mathtest18", # math test
                           "wordtest18"  # word test
                           ),]

df_target[df_target <= -1] <- as.numeric(NA)

df_links_sib <- df_links
df_links_sib <- merge(df_links_sib, df_target, by.x = "X1", by.y = "PID", all.x = TRUE)
colnames(df_links_sib)[11:(11+45)] <- paste(colnames(df_target)[2:47], "_S1", sep = "")
df_links_sib <- merge(df_links_sib, df_target, by.x = "X2", by.y = "PID", all.x = TRUE)
colnames(df_links_sib)[(11+46):((11+46)+45)] <- paste(colnames(df_target)[2:47], "_S2", sep = "")

# check self-esteem
sum(!is.na(df_links_sib$QM101M_S1) & !is.na(df_links_sib$QM101M_S2))
# check mental health
sum(!is.na(df_links_sib$QN406_S1) & !is.na(df_links_sib$QN406_S2))
# check life attitudes
sum(!is.na(df_links_sib$QM501_S1) & !is.na(df_links_sib$QM501_S2))
# check trust tendency
sum(!is.na(df_links_sib$QN1001_S1) & !is.na(df_links_sib$QN1001_S2))
# check attitudes towards society
sum(!is.na(df_links_sib$WV101_S1) & !is.na(df_links_sib$WV101_S2))
# check math test
sum(!is.na(df_links_sib$mathtest18_S1) & !is.na(df_links_sib$mathtest18_S2))
# check word test
sum(!is.na(df_links_sib$wordtest18_S1) & !is.na(df_links_sib$wordtest18_S2))

# check mh~trust
sum(!is.na(df_links_sib$QN406_S1) & !is.na(df_links_sib$QN406_S2) & !is.na(df_links_sib$QN1001_S1) & !is.na(df_links_sib$QN1001_S2))
# check mh~life_att
sum(!is.na(df_links_sib$QN406_S1) & !is.na(df_links_sib$QN406_S2) & !is.na(df_links_sib$QM501_S1) & !is.na(df_links_sib$QM501_S2))
# check mh~society
sum(!is.na(df_links_sib$QN406_S1) & !is.na(df_links_sib$QN406_S2) & !is.na(df_links_sib$WV101_S1) & !is.na(df_links_sib$WV101_S2))
# check mh~math
sum(!is.na(df_links_sib$QN406_S1) & !is.na(df_links_sib$QN406_S2) & !is.na(df_links_sib$mathtest18_S1) & !is.na(df_links_sib$mathtest18_S2))
