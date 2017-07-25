library(ysi)
ysi::ysi_init("Tax_Matrix")
#ysi::get_hilda(set = "Tax_Matrix", wave_n = 14)

#This is a script written to help Toby with his exploration of the new child care subsidy.
#It is not meant to be best-practice, more of an intial foray that will produce a rough
#estimate.


#The sript will attempt to use cond_eval to segregate people into the various groups so
#that they can be alloted the right amount of subsidy.

# INITIALISATION - Will need to group people into family units.

df <- Tax_Matrix_tbl %>% select(Wave_year,
                                Age,
                                PersonID,
                                HouseID,
                                HoursWorkedWkly,
                                NoChildren,
                                NoInHouse,
                                Children_in_care_working,
                                Childcare_exp,
                                Children0_4,
                                Children5_9,
                                Children10_14,
                                Children15_24,
                                Family,
                                FatherID,
                                MotherID,
                                HHTotInc_p,
                                HHTotInc_n,
                                HTotal_Assets,
                                Marital_Status,
                                Partner,
                                TotalCost_pw_DayCare,
                                Totalhrs_pw_Grandparent,
                                Totalhrs_pw_Grandparent2,
                                Totalhrs_pw_Kindy,
                                Totalhrs_pw_LongDayCare,
                                Totalhrs_pw_Nanny,
                                Totalhrs_pw_Neighbour,
                                Totalhrs_pw_Neighbour2,
                                Totalhrs_pw_PrivateDayCare,
                                Totalhrs_pw_Relatives,
                                Totalhrs_pw_Relatives2,
                                Totalhrs_pw_Sibling,
                                TotalCost_pw_Grandparent,
                                TotalCost_pw_Grandparent2,
                                TotalCost_pw_Relative,
                                TotalCost_pw_Relative2,
                                TotalCost_pw_Neighbour,
                                TotalCost_pw_Neighbour2,
                                TotalCost_pw_Nanny,
                                TotalCost_pw_DayCare,
                                TotalCost_pw_LongDayCare,
                                TotalCost_pw_PrivateDayCare,
                                TotalCost_pw_Kindy) %>%
  mutate_each(funs(na_to_zero)) %>%
  mutate(HHTotInc = HHTotInc_p - HHTotInc_n, Dependent_Child = if_else(Children0_4 + Children5_9 + Children10_14 + Children15_24 > 0, TRUE, FALSE)) %>%
  filter(Dependent_Child == TRUE) %>%
  #group_by(Wave_year) #%>%
  arrange(PersonID) %>%
  ungroup()

subsidy_allocation <- function(df){

  activity_test <- function(Wave_year, PersonID, HoursWorkedWkly){

    cond_eval(cond = c("(HoursWorkedWkly * 2) >= 8 & (HoursWorkedWkly * 2) <= 16", "(HoursWorkedWkly * 2) > 16 & (HoursWorkedWkly * 2) <= 48", "(HoursWorkedWkly * 2) > 48"),
              outcome = c("36", "72", "100"),
              Wave_year, PersonID, HoursWorkedWkly)
  }

  percentage <- function(Wave_year, PersonID, HHTotInc){

    cond_eval(cond = c("HHTotInc <= 65710", "HHTotInc > 65710 & HHTotInc < 170710", "HHTotInc >= 170710 & HHTotInc < 250000", "HHTotInc >= 250000 & HHTotInc < 350000", "HHTotInc >= 340000 & HHTotInc < 350000", "HHTotInc >= 350000"),
              outcome = c("85/100", "(85 - ((HHTotInc - 65710)/3000))/100", "50/100", "(50 - ((HHTotInc - 250000)/3000))/100","20/100", "0"),
              Wave_year, PersonID, HHTotInc)

  }

  df <- df %>% mutate(Max_Subsidy_Hours = activity_test(Wave_year, PersonID, HoursWorkedWkly),
                      Low_Income = ifelse(HHTotInc < 65710, TRUE, FALSE),
                      Actual_Subsidy_Hrs = ifelse(Low_Income == TRUE & Max_Subsidy_Hours == 0, 24, Max_Subsidy_Hours),
                      Subsidy_Percentage = percentage(Wave_year, PersonID, HHTotInc))



  return(df)
}

# FIRST CHECK - Is the child eligible for additional child care subsidy?
