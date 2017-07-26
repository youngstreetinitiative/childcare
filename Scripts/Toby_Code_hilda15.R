library(hilda15)
library(ysi)

generate_paths()

#build_fresh(childcare)

df <- get_fresh(childcare) %>%
  filter(Wave_n == 15)

activity_test <- function(Wave_year, PersonID, HoursWorkedWkly){
  
  cond_eval(cond = c("(HoursWorkedWkly * 2) >= 8 & (HoursWorkedWkly * 2) <= 16", "(HoursWorkedWkly * 2) > 16 & (HoursWorkedWkly * 2) <= 48", "(HoursWorkedWkly * 2) > 48"),
            outcome = c("36", "72", "100"),
            Wave_year, PersonID, HoursWorkedWkly)
}

percentage <- function(Wave_year, PersonID, HHTotInc){
  
  cond_eval(cond = c("HHTotInc <= 65710", "HHTotInc > 65710 & HHTotInc < 170710", "HHTotInc >= 170710 & HHTotInc < 250000", "HHTotInc >= 250000 & HHTotInc < 340000", "HHTotInc >= 340000 & HHTotInc < 350000", "HHTotInc >= 350000"),
            outcome = c("85/100", "(85 - ((HHTotInc - 65710)/3000))/100", "50/100", "(50 - ((HHTotInc - 250000)/3000))/100","20/100", "0"),
            Wave_year, PersonID, HHTotInc)
  
}
              
df1 <- df %>% mutate(hr_cost_grandparent = TotalCost_pw_Grandparent/Totalhrs_pw_Grandparent,
                     hr_cost_grandparent2 = TotalCost_pw_Grandparent2/Totalhrs_pw_Grandparent2,
                     hr_cost_kindy = TotalCost_pw_Kindy/Totalhrs_pw_Kindy,
                     hr_cost_daycare = TotalCost_pw_DayCare/Totalhrs_pw_DayCare,
                     hr_cost_longdaycare = TotalCost_pw_LongDayCare/Totalhrs_pw_LongDayCare,
                     hr_cost_nanny = TotalCost_pw_Nanny/Totalhrs_pw_Nanny,
                     hr_cost_neighbour = TotalCost_pw_Neighbour/Totalhrs_pw_Nanny,
                     hr_cost_neighbour2 = TotalCost_pw_Neighbour2/Totalhrs_pw_Neighbour2,
                     hr_cost_privatedaycare = TotalCost_pw_PrivateDayCare/Totalhrs_pw_PrivateDayCare,
                     hr_cost_relatives = TotalCost_pw_Relative/Totalhrs_pw_Relatives,
                     hr_cost_relatives2 = TotalCost_pw_Relative2/Totalhrs_pw_Relatives2) %>%
  mutate_all(funs(na_to_zero)) %>%
  mutate(HHTotInc = HHTotInc_p - HHTotInc_n, Dependent_Child = if_else(Children0_4 + Children5_9 + Children10_14 + Children15_24 > 0, TRUE, FALSE)) %>%
  filter(Dependent_Child == TRUE) %>%
  #group_by(Wave_n) #%>%
  arrange(PersonID) %>%
  ungroup() %>%
  mutate(Max_Subsidy_Hours = activity_test(Wave_year, PersonID, HoursWorkedWkly),
         Low_Income = ifelse(HHTotInc < 65710, TRUE, FALSE),
         Actual_Subsidy_Hrs = ifelse(Low_Income == TRUE & Max_Subsidy_Hours == 0, 24, Max_Subsidy_Hours),
         Subsidy_Percentage = percentage(Wave_year, PersonID, HHTotInc),
         Yearly_Hours = Max_Subsidy_Hours * 26,
         Asset_Quint = ntile(HTotal_Assets, 5),
         Inc_Quint = ntile(HHTotInc, 5)) %>%
  select(-hr_cost_grandparent, -hr_cost_grandparent2, -hr_cost_kindy, -hr_cost_nanny, -hr_cost_neighbour, -hr_cost_neighbour2, -hr_cost_relatives, -hr_cost_relatives2) %>%
  mutate(daycare_adjust = ifelse(hr_cost_daycare * Subsidy_Percentage > 10.70, 10.70, hr_cost_daycare * Subsidy_Percentage), 
         privatedaycare_adjust = ifelse(hr_cost_privatedaycare * Subsidy_Percentage > 10.70, 10.70, hr_cost_privatedaycare * Subsidy_Percentage),
         longdaycare_adjust = ifelse(hr_cost_longdaycare * Subsidy_Percentage > 11.55, 11.55, hr_cost_longdaycare * Subsidy_Percentage),
         Annual_daycare = Yearly_Hours * daycare_adjust,
         Annual_privatedaycare = Yearly_Hours * privatedaycare_adjust,
         Annual_longdaycare = Yearly_Hours * longdaycare_adjust,
         Annual_ChildCare = Annual_longdaycare + Annual_privatedaycare + Annual_daycare,
         Average_longdaycare = ifelse(Annual_ChildCare <= 0, 6.192566 * Subsidy_Percentage * Yearly_Hours, Annual_ChildCare)) %>%
  distinct(HouseID, .keep_all = TRUE)

# df2 <- df %>% group_by(Asset_Quint, Inc_Quint) %>%
#   summarise(Annual_ChildCare_PQ = sum(Annual_ChildCare))

dfAsset <- df1 %>% group_by(Asset_Quint) %>%
  summarise(Annual_ChildCare_PQ = sum(Annual_ChildCare)) %>%
  mutate(Type = "Assets") %>%
  ungroup() %>%
  rename(Quintile = Asset_Quint)

dfInc <- df1 %>% group_by(Inc_Quint) %>%
  summarise(Annual_ChildCare_PQ = sum(Annual_ChildCare)) %>%
  mutate(Type = "Income") %>%
  ungroup() %>%
  rename(Quintile = Inc_Quint)

dfall <- rbind(dfInc, dfAsset)

plotAsset <- ggplot(dfAsset, aes(x = Quintile, y = Annual_ChildCare_PQ)) + 
  geom_bar(stat = "Identity") + 
  ggtitle("Childcare Changes - Allocation per Wealth Quintiles in HILDA") +
  xlab("Household Wealth Quintiles") +
  ylab("Total Monies Allocated") +
  scale_y_continuous(labels = scales::dollar)

plotInc <- ggplot(dfInc, aes(x = Quintile, y = Annual_ChildCare_PQ)) + 
  geom_bar(stat = "Identity") + 
  ggtitle("Childcare Changes - Allocation per Income Quintiles in HILDA") +
  xlab("Household Income Quintiles") +
  ylab("Total Monies Allocated") + 
  scale_y_continuous(labels = scales::dollar)


plotall <- ggplot(dfall, aes(x = Quintile, y = Annual_ChildCare_PQ, fill = Quintile)) + 
  geom_bar(stat = "Identity") + 
  facet_grid(~Type) +
  ggtitle("Childcare Changes - Allocation per Income and Asset Quintiles in HILDA") +
  xlab("Household Income and Asset Quintiles") +
  ylab("Total Monies Allocated") + 
  scale_y_continuous(labels = scales::dollar)

print(plotAsset)
print(plotInc)
print(plotall)

