#Lesson 4: Examining Equity
#In this lesson, we will learn:
# - How to use R to explore questions of equity among students of different backgrounds, using the Civil Rights Data Collection as an example
# - Use R to identify schools or districts for further reporting
# - How to calculate risk ratios
# - What is regression (and why you should be very careful in using it)

library(tidyverse)
library(readr)
library(readxl)


# Notes ####

# The Office for Civil Rights (OCR) at the federal Department of Education requires every district in the U.S. to submit data to the Civil Rights Data Collection (CRDC) every two years. This data collection is the most comprehensive datasource we have on critical topics like school discipline, the use of restraint and seclusion, corporal punishment, law enforcement presence at schools, school safety and more, all available with detailed demographic breakdowns.

# Unfortunately the pandemic delayed the normal CRDC release schedule. However, at some point this year the OCR will release new data for the 2020-21 school year. When that happens, you will be able to update this code to quickly see the biggest trends in your coverage area.

#It is critical that you look at the "Documentation" folder in order to understand what the column headers mean.
#This will be the data layout for 2020-21:
#School-level data layout: crdc.communities.ed.gov/#communities/pdc/documents/17315
#LEA-level data: https://crdc.communities.ed.gov/#communities/pdc/documents/16875

# For this exercise we will examine trends in CRDC data for Fulton County, Georgia.

# Create a custom function to find enrollment by race ####

#Read school ID as a character type

leaid = '1302280'

lea_18 = read_csv('./data/crdc_201718_Public_Use_Files/Data/LEA/CRDC/CSV/LEA Characteristics.csv') %>%
  filter(LEAID == leaid)


enr_18 = read_csv('./data/crdc_201718_Public_Use_Files/Data/SCH/CRDC/CSV/Enrollment.csv') %>% filter(LEAID==leaid)

#Problem: NA values were represented by a -9. This could mess up our calculations.
#Reserve codes found on https://ocrdata.ed.gov/assets/downloads/2017-18%20CRDC%20Public-Use%20Data%20File%20Manual.pdf

#-3 Skip Logic Failure
#-5 Action Plan
#-6 Force Certified
#-8 EDFacts Missing Data
#-9 Not Applicable / Skipped
#-11 Suppressed Data

#Make all negative numbers into NA
enr_18[enr_18<0] = NA

#This code changes EVERY cell in the dataframe that had a number less than 0.
#Let's say we only wanted to do this in the columns that are numeric, and not change the other columns. You could use either of the following code:

#enr_18 %>% mutate(across(where(is.numeric), ~ ifelse(. < 0, NA, .)))
#enr_18 %>% mutate_if(is.numeric, ~ ifelse(. < 0, NA, .))

#Let's find the total enrollment

enr_18$total_enr = enr_18 %>% select(contains('SCH_ENR')) %>%
  select(!contains(c("IDEA","504","LEP"))) %>% #Don't want to double count special education/English language learner students
  rowSums(.)

#Now we want to find the total enrollment by each race. That will help us later when we investigate racial disparities in discipline.
#But there isn't one "race_total" column -- it's broken down by race AND gender. Need to add male + female together.

#All the columns that show enrollment start with SCH_ENR
enr_18 %>% select(contains('SCH_ENR')) %>% View()
#HI_M is Hispanic males, HI_F is Hispanic females, and so on for each race/gender combo

#We could tediously type out a new column for each race
enr_18 %>% mutate(hispanics = SCH_ENR_HI_M + SCH_ENR_HI_F,
                  asians = SCH_ENR_AS_M + SCH_ENR_AS_F) #and so on

#Or we could just use a for loop

races = c("HI", "AM", "AS", "HP", "WH", "TR", "BL")

for (race in races) {
  male = paste0("SCH_ENR_",race,"_M")
  female = paste0("SCH_ENR_",race,"_F")
  col_name = paste0(race,"_enr")
  enr_18[col_name] = enr_18[male] + enr_18[female]
}

#We could also do the same thing above by creating our own custom function called enr_by_race(). Then if you compare multiple years of data, you don't have to write the same for loop over and over.

enr_by_race = function(df) {

  races = c("HI", "AM", "AS", "HP", "WH", "TR", "BL")

  df$total_enr = df %>% select(contains('SCH_ENR')) %>%
    select(!contains(c("IDEA","504","LEP"))) %>%
    rowSums(.)

  for (race in races) {
    male = paste0("SCH_ENR_",race,"_M")
    female = paste0("SCH_ENR_",race,"_F")
    col_name = paste0(race,"_enr")
    df[col_name] = df[male] + df[female]
  }

  return(df)
}

enr_18 = enr_by_race(enr_18)



#Check that total enrollment is found by adding up enrollment of all races/genders

total_enr_18 = sum(enr_18$total_enr) #95,225 in 2018

lea_18$LEA_ENR #95,238 listed in the LEA file, so basically matches what we calculated

#Can you create a new function that calculates the *percent* of students that are each race?










percent_by_race = function(df) {

  races = c("HI", "AM", "AS", "HP", "WH", "TR", "BL")

  df$total_enr = df %>% select(contains('SCH_ENR')) %>%
    select(!contains(c("IDEA","504","LEP"))) %>%
    rowSums(.)

  for (race in races) {
    male = paste0("SCH_ENR_",race,"_M")
    female = paste0("SCH_ENR_",race,"_F")
    col_name = paste0(race,"_pct_enrollment")
    df[col_name] = (df[male] + df[female])/df['total_enr']*100
    df[col_name] = round(df[col_name], 2)
  }

  return(df)
}

enr_18 = percent_by_race(enr_18)

# Answer questions about school fights, shootings, school staff, restraint, suspensions ####

#SKIP TO STAFF

#How many school fights happened in 2017/2018?

offenses_18 = read_csv('./data/crdc_201718_Public_Use_Files/Data/SCH/CRDC/CSV/Offenses.csv') %>%
  filter(LEAID==leaid)

#How many fights without weapons occurred?
offenses_18 %>%
  summarize(total_attacks_without_weapon=sum(SCH_OFFENSE_ATTWOW))

#How many schools witnessed at least one school shooting?
offenses_18 %>% count(SCH_FIREARM_IND)

#How did the total number of teachers/counselors/cops in school change?

staff_18 = read_csv('./data/crdc_201718_Public_Use_Files/Data/SCH/CRDC/CSV/School Support.csv') %>%
  filter(LEAID == leaid) %>%
  select(COMBOKEY, SCH_NAME, SCH_FTETEACH_TOT, SCH_FTECOUNSELORS, SCH_FTESECURITY_LEO)

#Total teachers (full-time equivalent)
sum(staff_18$SCH_FTETEACH_TOT)

#Find total counselors (SCH_FTECOUNSELORS) and law enforcement officers (SCH_FTESECURITY_LEO)


#SKIP TO SUSPENSIONS
#How many incidents of physical restraint occurred?

rs_18 = read_csv('./data/crdc_201718_Public_Use_Files/Data/SCH/CRDC/CSV/Restraint and Seclusion.csv') %>%
  filter(LEAID == leaid) %>% select(SCH_NAME, COMBOKEY, contains('RSINSTANCES'))

#Much of this data is suppressed. So we need to replace the negative codes with NA.
rs_18[rs_18<0] = NA

rs_18$total_phys = rs_18$SCH_RSINSTANCES_PHYS_IDEA +
  rs_18$SCH_RSINSTANCES_PHYS_WODIS +
  rs_18$SCH_RSINSTANCES_PHYS_504

sum(rs_18$total_phys)
#Why is the output NA? When you take the sum of a column with NAs in it, you just get NA as a result. You have to remove the NA's with na.rm=TRUE

sum(rs_18$total_phys, na.rm=TRUE)

#How many instances of out-of-school suspensions occurred?

sus_18 = read_csv('./data/crdc_201718_Public_Use_Files/Data/SCH/CRDC/CSV/Suspensions.csv') %>%
  filter(LEAID == leaid) %>%
  select(SCH_NAME, COMBOKEY, contains('_OOSINSTANCES'))

sus_18 = sus_18 %>% mutate(total_oos_incidents = SCH_OOSINSTANCES_WODIS + SCH_OOSINSTANCES_IDEA + SCH_OOSINSTANCES_504)

sum(sus_18$total_oos_incidents) #7,554 out-of-school suspensions in 2018

#What percent of students suspended had disabilities covered under IDEA?





# Use risk ratios to examine racial inequity in suspensions ####

#How many students received out-of-school suspensions, by race?#

oos_18 = read_csv('./data/crdc_201718_Public_Use_Files/Data/SCH/CRDC/CSV/Suspensions.csv') %>%
  filter(LEAID == leaid) %>% select(SCH_NAME, COMBOKEY, contains('OOS_'))

#Make everything that's a negative reserve code NA
oos_18[oos_18<0] = NA

#Join enrollment data to suspension data
enr_18_race = enr_18 %>% select(SCH_NAME, COMBOKEY, total_enr, ends_with('_enr'))
oos_18 = left_join(oos_18, enr_18_race, by='COMBOKEY')

oos_by_race = function(df) {

  races = c("_HI", "_AM", "_AS", "_HP", "_WH", "_TR", "_BL")

  for (race in races) {
    col_name = paste0("oos", race)
    df[col_name] = df %>% select(contains('OOS_')) %>%
      select(contains(race)) %>% mutate_if(is.character, as.numeric) %>% rowSums(., na.rm=T)
  }

  return(df)
}


oos_18 = oos_by_race(oos_18)

oos_18 %>% select(SCH_NAME.x, starts_with("oos_")) %>% View()


#Which school had the highest number of Black students suspended?

#Which school had the highest number of suspensions White students suspended?


#Can we conclude from that information alone that schools in Fulton County disproportionately suspends Black students? Why or why not?




#We can't conclude whether the schools disproportionately suspend Black students unless we know how the suspension rate compares to the overall share of the school population that is Black.

#A risk ratio allows us to compare the probability of something happening to one demographic group to the probability of it happening to another group
#Example: You want to find out if Black students are at higher risk of being suspended.
#Say there is a school with 80 Black students and 100 White students. 20 Black students got suspended lasted year compared to 5 White students.
#Probability of getting suspended if you are Black: 20/80 = 25%
#Probability of getting suspended if you are White: 5/100 = 5%
#Risk ratio: 25/5 = 5
#Therefore: Black students last year were five times as likely to be suspended as White students.

#Let's make a function that calculates the risk ratio for every race, compared to White students.
#This function will take TWO parameters: a race ("AS", "HI", "BL", etc.) and a dataframe (in this case, it will be the oos_18 dataframe).

risk_ratio = function(race, df) {

  #Write variables for the columns that you want to reference
  oos = paste0("oos_",race) #E.g., "oos_BL" for Black students
  enr = paste0(race,"_enr") #E.g., "BL_enr" for Black students

  race_risk = sum(df[oos], na.rm=T)/sum(df[enr], na.rm=T)
  #This is the sum of out-of-school suspensions for that race, divided by the total number of students of that race

  wh_risk = sum(df$oos_WH, na.rm=T)/sum(df$WH_enr, na.rm=T)
  #The sum of out-of-school suspensions for white students, divided by the total number of white students

  risk_ratio = race_risk/wh_risk
  #Divide the risk of suspension for students of a particular race by the risk for white students

  return(risk_ratio)
  #You must always end functions by returning an output. If you don't specify what you want the function to give you at the end, you'll get nothing.

}



risk_ratio('BL', oos_18) #Black students were 6.3 times as likely as White students be suspended.
risk_ratio('HI', oos_18) #Risk ratios was 2.2 for Hispanic students

#Is there any racial group that is less likely to be suspended than White students?



#How could you use risk ratios to examine other parts of the CRDC?





#Risk ratios can show you whether there if certain groups of students (e.g. Black students, disabled students, boys) are disproportionately at risk for things like suspension, expulsion, restraint/seclusion, referrals to law enforcement, etc.

# Regressions ####

# Let's say we had a hypothesis that schools with more school resource officers and a higher share of Black students see more out-of-school suspensions.
#Is it possible to predict a school's suspension rate based on the number of law enforcement officers it has and the share of the population that is Black?
# To find out, we need to learn about regression.

#See regression.pptx

#Need to join sus_18, staff_18, enr_18

merged = full_join(sus_18, staff_18, by="COMBOKEY")
merged = full_join(merged, enr_18, by="COMBOKEY")

#Remove unnecessary SCH_NAME.y and SCH_NAME.x
merged = merged %>% select(-SCH_NAME.y, -SCH_NAME.x)
merged = merged %>% select(SCH_NAME, everything())
colnames(merged)

#Find total number of out-of-school suspensions per 100 students ####

merged = merged %>% mutate(oos_incidents_per_100 = total_oos_incidents/total_enr*100)

#Which schools had the highest suspension rates in 2018?
merged %>% select(SCH_NAME, oos_incidents_per_100) %>% arrange(-oos_incidents_per_100)

#Then do a regression
attach(merged)

model = lm(oos_incidents_per_100 ~ BL_pct_enrollment + SCH_FTESECURITY_LEO)
summary(model)

#For every extra SRO that is hired, the suspension rate increases by 2.7 percentage points. However, this result is just barely statistically significant.
#For every one percentage point increased in Black enrollment, the suspension rate increases by 0.17 percentage points. This result is highly statistically significant.
#Adjusted R squared is 0.31, so these two factors alone can predict 31% of the variation in suspension rates across schools.

#Can also look at the 95% confidence intervals on the model.
confint(model)
#The true coefficient for BL_pct_enrollment is somewhere between 0.12 and 0.22 percentage points

#Even just the share of Black students alone is quite predictive.
model = lm(oos_incidents_per_100 ~ BL_pct_enrollment)
summary(model)

#So maybe it would be easier to just give your readers a correlation instead of trying to explain to them what a regression is.
cor(BL_pct_enrollment, oos_incidents_per_100)

#More detailed correlation test
cor.test(BL_pct_enrollment, oos_incidents_per_100)


#What if we looked at Black and Hispanic enrollment?

model = lm(oos_incidents_per_100 ~ BL_pct_enrollment + HI_pct_enrollment)
summary(model)

#Hispanic enrollment is not a statistically significant predictor of the suspension rate

#What if we looked at counselors and law enforcement together?

model = lm(oos_incidents_per_100 ~ BL_pct_enrollment + SCH_FTECOUNSELORS + SCH_FTESECURITY_LEO)
summary(model)

#Not statistically significant. Although we could consider whether we should be looking at counselors per 250 students instead of the total number.

# What other variables could we bring in to make our model stronger? What other datasets could we join to the CRDC data?

# What type of analysis would you prefer to talk about in a story on disparate suspension rates: risk ratios, regression analysis or correlation?

