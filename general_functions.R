library(ARTool)
library(car)

# assumes that column 'condition' exists, and adds new boolean columns 'transparency' and 'control'
create_2x2_columns <- function(df){
  return(df %>% mutate(
    transparency = (condition == "transparency" | condition == "both"),
    control = (condition == "control" | condition == "both")))
}

#df_perceivedcontrol <- data_postq %>% select(condition,transparency,control,PP_perceived_control)

twoway_anova_test <- function(testname,df,feature){
  # two way anova (see R Console)
  sig <- .05
  # check parametric test assumptions
  ## test for equal variance
  df_num <- df %>% mutate(!!feature := as.numeric(df[[feature]]))
  # vartest <- bartlett.test(as.formula(paste(feature,"~ condition")), data = df_num)
  # if (vartest$p.value < sig){
  #   warnings(paste(testname,": Variance among groups is unequal. p-value=",vartest$p.value))
  #   return;
  # }
  # normality test, for each group individually
  is_normal_distributed = TRUE
  for(group in list("baseline","transparency","control","both")){
    df_sub <- df_num %>% filter(condition == group)
    if (nrow(df_sub) == 0){
      next
    }
    #print(paste(testname,"Normality test for",group))
    s <- shapiro.test(df_sub[[feature]])
#    normtest <- ks.test(df_sub[[feature]], "pnorm", mean=mean(df_sub[[feature]]), sd=sd(df_sub[[feature]]))
#    if(normtest$p.value < sig){
#      warnings(testname, "Group",group,"is not normally distributed (p-value",normtest$p.value,")")
#      return;
#    }
    if (s$p <0.05){
      is_normal_distributed = FALSE
      warnings(testname, "Group",group,"is not normally distributed (p-value",s$p,")")
      break
    }
  }
  
  if (!is_normal_distributed){
    print("sample is not normally distributed, continuing with Sven's special anova test (ARTool)")
    model <- art(as.formula(paste(feature,"~ transparency * control")), data = df_num)
    anova(model)
  } else {
    print("sample is normally distributed, continuing with standard anova")
    model <- aov(as.formula(paste(feature,"~ transparency * control")), data = df_num)
    summary(model)
  }

}

# library(ordPens)
# x <- data.frame(f1 = as.factor(c(1,3,2,3,4,2)), f2=as.factor(c(2,4,1,3,2,4)))
# ordAOV(x=as.matrix(x), y=as.factor(c(1,2,3,4)), type = "RLRT", nsim=1000000)

independent_samples_t_test <- function(df1,df2,alldata){
  # normality test
  s1 <- shapiro.test(df1)
  s2 <- shapiro.test(df2)
  is_normal_distributed <- s1$p >=0.05 && s2$p >=0.05
  # for equal variance test: Levenes test
  lev1<-leveneTest(count ~ condition, data=alldata, center="mean")
  is_var_equal <- lev1$`Pr(>F)`[[1]] >= 0.05
  print(paste("is normally distributed: ",is_normal_distributed,", has equal variances: ",is_var_equal),sep="")
  if (is_normal_distributed){
    # normal distributed -> t test
    print("continuing with independent samples t test")
    m1<-t.test(df %>% filter(condition == "transparency") %>% .$count, df %>% filter(condition == "both") %>% .$count, var.equal=is_var_equal, na.rm=TRUE)
    print(m1)
  }
  else {
    # Mann-Whitney u test
    print("continuing with Mann-Whitney U Test")
    wt <- wilcox.test(df1,df2)
    print(wt)
  }
  # if (s$p <0.05){
  #   # NOT normal distributed -> Welch's t test
  #   print("sample is NOT normally distributed, continuing with ...")
  # }
  # else {
  #   # normal distributed -> independent samples t test
  #   print("sample IS normally distributed, continuing with independent samples t test")
  #   m1<-t.test(df %>% filter(condition == "transparency") %>% .$count, df %>% filter(condition == "both") %>% .$count, var.equal=s$p >=0.05, na.rm=TRUE)
  #   print(m1)
  # }
}

mean_se_tjebo <- function (x, mult = 1) {
  x <- stats::na.omit(x)
  se <- mult * sqrt(stats::var(x)/length(x))
  mean <- mean(x)
  if(length(x) != 1) {
    data.frame(y = mean, ymin = mean - se, ymax = mean + se)
  } else {
    data.frame(y = mean, ymin = mean, ymax = mean)
  }
}

chi_sq_test <- function(testname,mat){
  # params
  sig <- .05
  # check test assumptions
  # -- non --
  # Chi Square Test
  test <- chisq.test(as.table(mat), correct = FALSE)
  print(test)
  if (test$p.value < sig){
    print(paste(testname,": Significant relation found with p=",test$p.value))
    # Post Hoc Test
    posthoc_test <- chisq.posthoc.test(as.table(mat))
    sigAdj <- sig/(nrow(mat)*ncol(mat))
    critical_z <- qnorm(sigAdj/2)
    print(paste(testname,": Variables in the following table whose magnitude is higher than",critical_z,"occured significantly more/less often"))
    print(posthoc_test)
  } else {
    print(paste(testname,": No significant relation found"))
  }
}
