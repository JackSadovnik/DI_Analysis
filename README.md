# DI_Analysis

library(tidyverse)
library(readxl)
library(broom)

employees_12 <- read_xlsx('Diversity_12-18_Anonymised.xlsx', 
                          sheet = "Diversity 12")
employees_18 <- read_xlsx('Diversity_12-18_Anonymised.xlsx', 
                          sheet = "Diversity 18")
mat_info <- read_xlsx('Mat & Pat Leave_2012-18_Anonymised.xlsx',
                          sheet = "12")
pay_12 <- read_xlsx('Pay Info_12-18_Anonymised.xlsx', 
                          sheet = "12")
pay_18 <- read_xlsx('Pay Info_12-18_Anonymised.xlsx', 
                    sheet = "18")

empl_18 <- employees_18 %>%
  select(Emp_ID, Grade)

retained_emp <- semi_join(employees_12, empl_18, by = "Emp_ID") %>% # filters on employees_12 by empl_18, so only employees in empl_18 are left in employees_12
  left_join(empl_18, by = "Emp_ID") %>%
  filter(Grade.x != "Operational",
         Grade.y != "Operational") # filtered out Operational grades since it is hard to judge promotions within it

retained_emp$grade_num_12 <- as.numeric(as.character(factor(retained_emp$Grade.x,
  levels=c('Operational', 'Band 1', 'Band 2', 'Band 3', 'Band 4', 'Band 5', 'Director'), 
  labels=c(0, 1, 2, 3, 4, 5, 6))))

retained_emp$grade_num_18 <- as.numeric(as.character(factor(retained_emp$Grade.y,
  levels=c('Operational', 'Band 1', 'Band 2', 'Band 3', 'Band 4', 'Band 5', 'Director'), 
  labels=c(0, 1, 2, 3, 4, 5, 6))))

retained_emp$maternity <- retained_emp$Emp_ID %in% mat_info$Emp_ID 
# TRUE or FALSE depending on whether employees went on maternity

retained_emp$promotion <- factor(retained_emp$grade_num_18 - retained_emp$grade_num_12) 
# New column created showing whether we had a grade change

chisq.test(retained_emp$maternity, retained_emp$promotion)
## No statistical significance shown

t1 <- glm(maternity ~ promotion, data = retained_emp, family = "binomial")
tidy(t1)
## Logistic regression confirms that there is no statistically significant relationship


library(tidyverse)
library(readxl)
library(broom)

employees_18 <- read_xlsx('Diversity_12-18_extra.xlsx', 
                       sheet = "Diversity 18")
employees_14 <- read_xlsx('Diversity_12-18_extra.xlsx', 
                       sheet = "Diversity 14")
employees_13 <- read_xlsx('Diversity_12-18_extra.xlsx', 
                       sheet = "Diversity 13")
secondees <- read_xlsx('2014-18 Secondments Updated.xlsx') %>%
  distinct(`Employee No`) %>%
  mutate(seconded = TRUE)

cohort_18 <- anti_join(employees_14, employees_13, by = "Emp_ID") %>%
  semi_join(employees_18, by = "Emp_ID") %>%
  left_join(secondees, by = c('Emp_ID' = 'Employee No')) %>%
  replace_na(list(seconded = FALSE)) 
# filters for new employees in 2014, then for the ones still here in 2018, and adds extra column showing secondments

m1 <- glm(seconded ~ `Minority Race` + `Minority Age` + `Minority Faith` + `Minority Sexuality`
    + `Minority Disability` + `Minority Sex`, data = cohort_18, family = "binomial")
tidy(m1)
summary(m1)
## No statistical significance shown between secondments and diversity characteristics. 
## I.E. no group is advantaged or disadvanted.
