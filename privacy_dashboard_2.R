library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(chisq.posthoc.test)

source('general_functions.R')

# load onboarding questionnaires
data_onboarding_groupassignment <- readRDS("data/questionnaires/data_onboarding_groupassignment.rds")
data_onboarding_baseline <- readRDS("data/questionnaires/data_onboarding_baseline.rds")
data_onboarding_transparency <- readRDS("data/questionnaires/data_onboarding_transparency.rds")
data_onboarding_control <- readRDS("data/questionnaires/data_onboarding_control.rds")
data_onboarding_both <- readRDS("data/questionnaires/data_onboarding_both.rds")

# load start + end questionnaires
data_startq <- readRDS("data/questionnaires/data_preq.rds")
data_postq <- readRDS("data/questionnaires/data_postq.rds")

### prepare app data
users <- readRDS("data/app/users.rds")
  




#### Tests #####
## Teilnahmeraten
# eingeteilte TN
n_assigned_ba <- nrow(data_onboarding_groupassignment %>% filter(RD01 == 1))
n_assigned_tr <- nrow(data_onboarding_groupassignment %>% filter(RD01 == 2))
n_assigned_co <- nrow(data_onboarding_groupassignment %>% filter(RD01 == 3))
n_assigned_bo <- nrow(data_onboarding_groupassignment %>% filter(RD01 == 4))

# Onboarding Fragebogen abgeschlossen
n_onboarding_finished_ba <- nrow(data_onboarding_baseline %>% filter(FINISHED == 1))
n_onboarding_finished_tr <- nrow(data_onboarding_transparency %>% filter(FINISHED == 1))
n_onboarding_finished_co <- nrow(data_onboarding_control %>% filter(FINISHED == 1))
n_onboarding_finished_bo <- nrow(data_onboarding_both %>% filter(FINISHED == 1))

# Startfragebogen
n_started_ba <- nrow(data_startq %>% filter(condition == "baseline"))
n_started_tr <- nrow(data_startq %>% filter(condition == "transparency"))
n_started_co <- nrow(data_startq %>% filter(condition == "control"))
n_started_bo <- nrow(data_startq %>% filter(condition == "both"))

# Endfragebogen
n_finished_ba <- nrow(data_postq %>% filter(condition == "baseline")) 
n_finished_tr <- nrow(data_postq %>% filter(condition == "transparency"))
n_finished_co <- nrow(data_postq %>% filter(condition == "control"))
n_finished_bo <- nrow(data_postq %>% filter(condition == "both"))

# ChiSquare Test: Gruppeneinteilung -> Onboarding Fragebogen abgeschlossen
mat_onboarding <- matrix(c(n_onboarding_finished_ba, 
                           n_onboarding_finished_tr,
                           n_onboarding_finished_co,
                           n_onboarding_finished_bo,
                        max(0,n_assigned_ba-n_onboarding_finished_ba), 
                        max(0,n_assigned_tr-n_onboarding_finished_tr),
                        max(0,n_assigned_co-n_onboarding_finished_co),
                        max(0,n_assigned_bo-n_onboarding_finished_bo)
                ), ncol=2)
colnames(mat_onboarding) <- c("Onboarded","Aborted")
row.names(mat_onboarding) <- c("baseline","transparency","control","both")
test_onboarding <- chisq.test(as.table(mat_onboarding), correct = FALSE)
print(test_onboarding)

### Test: Gruppeneinteilung -> App Installation
n_appinst_ba = nrow(users %>% filter(condition=="baseline"))
n_appinst_tr = nrow(users %>% filter(condition=="transparency"))
n_appinst_co = nrow(users %>% filter(condition=="control"))
n_appinst_bo = nrow(users %>% filter(condition=="both"))
mat_appinst <- matrix(c(n_appinst_ba, 
                           n_appinst_tr,
                           n_appinst_co,
                           n_appinst_bo,
                           max(0,n_assigned_ba-n_appinst_ba), 
                           max(0,n_assigned_tr-n_appinst_tr),
                           max(0,n_assigned_co-n_appinst_co),
                           max(0,n_assigned_bo-n_appinst_bo)
), ncol=2)
colnames(mat_appinst) <- c("App Installed","App Not Installed")
row.names(mat_appinst) <- c("baseline","transparency","control","both")
chi_sq_test("Gruppeneinteilung -> App Installation",mat_appinst)



### Test: Onboarding Q finished -> App Inst
mat_onbq_appinst <- matrix(c(n_appinst_ba, 
                        n_appinst_tr,
                        n_appinst_co,
                        n_appinst_bo,
                        max(0,n_onboarding_finished_ba-n_appinst_ba), 
                        max(0,n_onboarding_finished_tr-n_appinst_tr),
                        max(0,n_onboarding_finished_co-n_appinst_co),
                        max(0,n_onboarding_finished_bo-n_appinst_bo)
), ncol=2)
colnames(mat_onbq_appinst) <- c("App Installed","App Not Installed")
row.names(mat_onbq_appinst) <- c("baseline","transparency","control","both")

chi_sq_test("Onboarding Q finished -> App Inst", mat_onbq_appinst)

# test_onbq_appinst <- chisq.test(as.table(mat_onbq_appinst), correct = FALSE)
# print(test_onbq_appinst)
# chisq.posthoc.test(as.table(mat_onbq_appinst))
# sig <- .05
# sigAdj <- sig/(nrow(mat_onbq_appinst)*ncol(mat_onbq_appinst))
# qnorm(sigAdj/2) # Residuals müssen höher als dieser Wert sein
# # -> positiv über Wert (hier 2,7) heißt es gibt signifikant mehr (hier z.b- "App not installed" cases bei Transparency) als erwartet, negativ über 2,7 heißt weniger

#### Two Way Anova?
# df <- data.frame(sunlight = rep(c('Low', 'High'), each = 30),
#                  water = rep(c('Daily', 'Weekly'), each = 15, times = 2),
#                  growth = c(rnorm(15, 6, 2), rnorm(15, 7, 3), rnorm(15, 7, 2),
#                             rnorm(15, 10, 3)))
df <- data.frame(transparency = c(FALSE,FALSE,TRUE,TRUE),
                 control = c(FALSE,TRUE,FALSE,TRUE),
                 tnn = c(n_appinst_ba/n_onboarding_finished_ba,
                         n_appinst_co/n_onboarding_finished_co,
                         n_appinst_tr/n_onboarding_finished_tr,
                         n_appinst_bo/n_onboarding_finished_bo))
#fit the two-way ANOVA model
model <- aov(tnn ~ transparency * control, data = df)
#view the model output
summary(model)


### Test: Gruppeneinteilung -> App für mind. 1 Tage behalten
n_day1_ba = nrow(users %>% filter(condition=="baseline" & days_in_study >= ddays(1)))
n_day1_tr = nrow(users %>% filter(condition=="transparency" & days_in_study >= ddays(1)))
n_day1_co = nrow(users %>% filter(condition=="control" & days_in_study >= ddays(1)))
n_day1_bo = nrow(users %>% filter(condition=="both" & days_in_study >= ddays(1)))
mat_onbq_day1 <- matrix(c(n_day1_ba, 
                          n_day1_tr,
                             n_day1_co,
                             n_day1_bo,
                             max(0,n_onboarding_finished_ba-n_day1_ba), 
                             max(0,n_onboarding_finished_tr-n_day1_tr),
                             max(0,n_onboarding_finished_co-n_day1_co),
                             max(0,n_onboarding_finished_bo-n_day1_bo)
), ncol=2)
colnames(mat_onbq_day1) <- c("App 1+ day","App less than 1 day")
row.names(mat_onbq_day1) <- c("baseline","transparency","control","both")

chi_sq_test("Gruppeneinteilung -> App für mind. 1 Tage behalten",mat_onbq_day1)


### Test: App Inst -> App für mind. 1 Tage behalten
mat_inst_day1 <- matrix(c(n_day1_ba, 
                          n_day1_tr,
                          n_day1_co,
                          n_day1_bo,
                          max(0,n_appinst_ba-n_day1_ba), 
                          max(0,n_appinst_tr-n_day1_tr),
                          max(0,n_appinst_co-n_day1_co),
                          max(0,n_appinst_bo-n_day1_bo)
), ncol=2)
colnames(mat_inst_day1) <- c("App 1+ day","App less than 1 day")
row.names(mat_inst_day1) <- c("baseline","transparency","control","both")
test_inst_day1 <- chisq.test(as.table(mat_inst_day1), correct = FALSE)
print(test_inst_day1)


### Test: Gruppeneinteilung -> App für mind. 6 Tage behalten
n_day6_ba = nrow(users %>% filter(condition=="baseline" & days_in_study >= ddays(6)))
n_day6_tr = nrow(users %>% filter(condition=="transparency" & days_in_study >= ddays(6)))
n_day6_co = nrow(users %>% filter(condition=="control" & days_in_study >= ddays(6)))
n_day6_bo = nrow(users %>% filter(condition=="both" & days_in_study >= ddays(6)))
mat_onbq_day6 <- matrix(c(n_day6_ba, 
                          n_day6_tr,
                          n_day6_co,
                          n_day6_bo,
                          max(0,n_onboarding_finished_ba-n_day6_ba), 
                          max(0,n_onboarding_finished_tr-n_day6_tr),
                          max(0,n_onboarding_finished_co-n_day6_co),
                          max(0,n_onboarding_finished_bo-n_day6_bo)
), ncol=2)
colnames(mat_onbq_day6) <- c("App 6+ day","App less than 6 day")
row.names(mat_onbq_day6) <- c("baseline","transparency","control","both")

chi_sq_test("Gruppeneinteilung -> App für mind. 6 Tage behalten",mat_onbq_day6)

### Test: App min. 1 Tag -> App für mind. 6 Tage behalten
n_day6_ba = nrow(users %>% filter(condition=="baseline" & days_in_study >= ddays(6)))
n_day6_tr = nrow(users %>% filter(condition=="transparency" & days_in_study >= ddays(6)))
n_day6_co = nrow(users %>% filter(condition=="control" & days_in_study >= ddays(6)))
n_day6_bo = nrow(users %>% filter(condition=="both" & days_in_study >= ddays(6)))
mat_day1_day6 <- matrix(c(n_day6_ba, 
                          n_day6_tr,
                          n_day6_co,
                          n_day6_bo,
                          max(0,n_day1_ba-n_day6_ba), 
                          max(0,n_day1_tr-n_day6_tr),
                          max(0,n_day1_co-n_day6_co),
                          max(0,n_day1_bo-n_day6_bo)
), ncol=2)
colnames(mat_day1_day6) <- c("App 6+ day","App less than 6 day")
row.names(mat_day1_day6) <- c("baseline","transparency","control","both")

chi_sq_test("App min. 1 Tag -> App für mind. 6 Tage behalten",mat_day1_day6)


### Test: Gruppeneinteilung -> Startfragebogen
mat_assign_startq <- matrix(c(n_assigned_ba-n_started_ba, 
                           n_assigned_tr-n_started_tr,
                           n_assigned_co-n_started_co,
                           n_assigned_bo-n_started_bo,
                           n_started_ba, 
                           n_started_tr,
                           n_started_co,
                           n_started_bo
), ncol=2)
colnames(mat_assign_startq) <- c("Dropped out during Onboarding","StartQ finished")
row.names(mat_assign_startq) <- c("baseline","transparency","control","both")

chi_sq_test("Gruppeneinteilung -> Startfragebogen",mat_assign_startq)

### Test: Onboarding abgeschlossen -> Startfragebogen
mat_onb_startq <- matrix(c(n_onboarding_finished_ba-n_started_ba, 
                           n_onboarding_finished_tr-n_started_tr,
                           n_onboarding_finished_co-n_started_co,
                           n_onboarding_finished_bo-n_started_bo,
                           n_started_ba, 
                           n_started_tr,
                           n_started_co,
                           n_started_bo
                  ), ncol=2)
colnames(mat_onb_startq) <- c("Dropped out during App Setup","StartQ finished")
row.names(mat_onb_startq) <- c("baseline","transparency","control","both")

chi_sq_test("Onboarding abgeschlossen -> Startfragebogen", mat_onb_startq)


### Vergleich Dropout während Studie
mat_dropout <- matrix(c(n_finished_ba, 
                        n_finished_tr,
                        n_finished_co,
                        n_finished_bo,
                        max(0,n_started_ba-n_finished_ba), 
                        max(0,n_started_tr-n_finished_tr),
                        max(0,n_started_co-n_finished_co),
                        max(0,n_started_bo-n_finished_bo)
                        ), ncol=2)
colnames(mat_dropout) <- c("Finished","Aborted")
row.names(mat_dropout) <- c("baseline","transparency","control","both")

chi_sq_test("Dropout während Studie",mat_dropout)

# Chart: x-Achse Stages der Studie, y-Achse TN Zahl, Farbe Condition
df_studyprogress <- data.frame(studyprogress = factor(c("interested in study",
                                    "interested in study",
                                    "interested in study",
                                    "interested in study",
                                    "indicated participation",
                                    "indicated participation",
                                    "indicated participation",
                                    "indicated participation",
                                    "installed app",
                                    "installed app",
                                    "installed app",
                                    "installed app",
                                    "study day 1",
                                    "study day 1",
                                    "study day 1",
                                    "study day 1",
                                    # "4_n_start_q",
                                    # "4_n_start_q",
                                    # "4_n_start_q",
                                    # "4_n_start_q",
                                    # "5_n_post_q",
                                    # "5_n_post_q",
                                    # "5_n_post_q",
                                    # "5_n_post_q"
                                    "study end",
                                    "study end",
                                    "study end",
                                    "study end"
                                    ), levels=c("interested in study","indicated participation","installed app","study day 1","study end")), # this defines the order on the x axis
                 condition = factor(c("baseline", 
                               "transparency",
                               "control",
                               "both",
                               "baseline", 
                               "transparency",
                               "control",
                               "both",
                               "baseline", 
                               "transparency",
                               "control",
                               "both",
                               "baseline", 
                               "transparency",
                               "control",
                               "both",
                               "baseline", 
                               "transparency",
                               "control",
                               "both"),levels=c("baseline","transparency","control","both")),
                 N = c(n_assigned_ba,
                       n_assigned_tr,
                       n_assigned_co,
                       n_assigned_bo,
                       n_onboarding_finished_ba,
                       n_onboarding_finished_tr,
                       n_onboarding_finished_co,
                       n_onboarding_finished_bo,
                       n_appinst_ba,
                       n_appinst_tr,
                       n_appinst_co,
                       n_appinst_bo,
                       n_day1_ba,
                       n_day1_tr,
                       n_day1_co,
                       n_day1_bo,
                       # n_started_ba,
                       # n_started_tr,
                       # n_started_co,
                       # n_started_bo,
                       # n_finished_ba,
                       # n_finished_tr,
                       # n_finished_co,
                       # n_finished_bo
                       n_day6_ba,
                       n_day6_tr,
                       n_day6_co,
                       n_day6_bo
                       )
)

ggplot(
  data=data.frame(df_studyprogress), 
  aes(x = studyprogress, y = N, color = condition, group=condition)
  ) + 
  geom_line(size = 2)+
  scale_color_manual(values=c('#f3b94d','#00748d','#F241B8','#76F266'))+
  theme_minimal()+
  labs(x="phase of study",y="N")+
  scale_y_continuous(trans='log10') +
  theme(
    axis.text.x = element_text(angle = 45, hjust=1),
    legend.position = c(.7, .7)
    )




## table 3
df_table3 <- data.frame(
  baseline_n = c(n_assigned_ba,n_onboarding_finished_ba,n_appinst_ba,n_day1_ba,n_day6_ba),
  baseline_perc = c(1,n_onboarding_finished_ba/n_assigned_ba,n_appinst_ba/n_onboarding_finished_ba,n_day1_ba/n_appinst_ba,n_day6_ba/n_day1_ba),
  transparency_n = c(n_assigned_tr,n_onboarding_finished_tr,n_appinst_tr,n_day1_tr,n_day6_tr),
  transparency_perc = c(1,n_onboarding_finished_tr/n_assigned_tr,n_appinst_tr/n_onboarding_finished_tr,n_day1_tr/n_appinst_tr,n_day6_tr/n_day1_tr),
  control_n = c(n_assigned_co,n_onboarding_finished_co,n_appinst_co,n_day1_co,n_day6_co),
  control_perc = c(1,n_onboarding_finished_co/n_assigned_co,n_appinst_co/n_onboarding_finished_co,n_day1_co/n_appinst_co,n_day6_co/n_day1_co),
  both_n = c(n_assigned_bo,n_onboarding_finished_bo,n_appinst_bo,n_day1_bo,n_day6_bo),
  both_perc = c(1,n_onboarding_finished_bo/n_assigned_bo,n_appinst_bo/n_onboarding_finished_bo,n_day1_bo/n_appinst_bo,n_day6_bo/n_day1_bo),
  total_n = c(n_assigned_ba+n_assigned_tr+n_assigned_co+n_assigned_bo,
              n_onboarding_finished_ba+n_onboarding_finished_tr+n_onboarding_finished_co+n_onboarding_finished_bo,
              n_appinst_ba+n_appinst_tr+n_appinst_co+n_appinst_bo,
              n_day1_ba+n_day1_tr+n_day1_co+n_day1_bo,
              n_day6_ba+n_day6_tr+n_day6_co+n_day6_bo),
  total_perc = c(1,
                 (n_onboarding_finished_ba+n_onboarding_finished_tr+n_onboarding_finished_co+n_onboarding_finished_bo)/(n_assigned_ba+n_assigned_tr+n_assigned_co+n_assigned_bo),
                 (n_appinst_ba+n_appinst_co+n_appinst_bo+n_appinst_tr)/(n_onboarding_finished_ba+n_onboarding_finished_tr+n_onboarding_finished_co+n_onboarding_finished_bo),
                 (n_day1_ba+n_day1_bo+n_day1_co+n_day1_tr)/(n_appinst_ba+n_appinst_co+n_appinst_bo+n_appinst_tr),
                 (n_day6_ba+n_day6_bo+n_day6_co+n_day6_tr)/(n_day1_ba+n_day1_bo+n_day1_co+n_day1_tr)
                 )
  )



