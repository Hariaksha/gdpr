# https://statsandr.com/blog/binary-logistic-regression-in-r
library(ggplot2)
library(MASS)
library(dplyr)
library(car)
library(tidyr)
library(ordinal)

# transfer data from spreadsheet to variable
survey <- read.csv("C:/Users/haria/Downloads/welle120_aufbereitet.csv")

# inspect structure of data set
str(survey)
summary(survey)

# plot counts + type for a variable with bar/column chart
ggplot(survey, aes(x = a_nachteil, fill = factor(nache))) + 
  geom_bar(stat = 'count') +
  scale_fill_discrete(
    name = "Expected turnover in next quarter",
    labels = c("gleich", "runter", "hoch")
  )

# Filter out rows with empty values in specific columns
survey <- survey %>% filter(!is.na(erf_speich)) # remove missing rows
survey <- survey %>% filter(!is.na(si_speich)) # remove missing rows
survey <- survey %>% filter(!is.na(erf_intern)) # remove missing rows
survey <- survey %>% filter(!is.na(si_intern)) # remove missing rows
survey <- survey %>% filter(!is.na(erf_extunt)) # remove missing rows
survey <- survey %>% filter(!is.na(si_extunt)) # remove missing rows
survey <- survey %>% filter(!is.na(erf_forsch)) # remove missing rows
survey <- survey %>% filter(!is.na(si_forsch)) # remove missing rows

survey <- survey %>% filter(!is.na(a_rsicher)) # remove missing rows

survey <- survey %>% filter(!is.na(pdwichtig)) # remove missing rows
survey <- survey %>% filter(!is.na(a_nachteil)) # remove missing rows
survey <- survey %>% filter(!is.na(a_gefahr)) # remove missing rows
survey <- survey %>% filter(!is.na(a_kompliz)) # remove missing rows
survey <- survey %>% filter(!is.na(a_vorteil)) # remove missing rows
survey <- survey %>% filter(!is.na(a_vertrau)) # remove missing rows
survey <- survey %>% filter(!is.na(a_inno)) # remove missing rows
survey <- survey %>% filter(!is.na(a_kost)) # remove missing rows
survey <- survey %>% filter(!is.na(a_berat)) # remove missing rows
survey <- survey %>% filter(!is.na(a_proz)) # remove missing rows
survey <- survey %>% filter(!is.na(a_stand)) # remove missing rows
survey <- survey %>% filter(!is.na(a_ki)) # remove missing rows
survey <- survey %>% filter(!is.na(a_forsch)) # remove missing rows
survey <- survey %>% filter(!is.na(a_nachteil)) # remove missing rows

survey <- survey %>% filter(!is.na(aufwand)) # remove missing rows
survey <- survey %>% filter(!is.na(datengm)) # remove missing rows
survey <- survey %>% filter(!is.na(umheute)) # remove missing rows

survey <- survey %>% filter(!is.na(nach)) # remove missing rows
survey <- survey %>% filter(!is.na(ums)) # remove missing rows
survey <- survey %>% filter(!is.na(pers)) # remove missing rows
survey <- survey %>% filter(!is.na(nache)) # remove missing rows
survey <- survey %>% filter(!is.na(umse)) # remove missing rows
survey <- survey %>% filter(!is.na(perse)) # remove missing rows

# get counts in table format
data_table = table(survey$pers)
data_table





# recode variables / create binary data variables for high certainty for 4 situational questions (si_speich, si_intern, si_extunt, si_forsch)
survey$sicher_speich_binary = ifelse(test = survey$si_speich > 3, yes = 1, no = 0)
survey$sicher_intern_binary = ifelse(test = survey$si_intern > 3, yes = 1, no = 0)
survey$sicher_extunt_binary = ifelse(test = survey$si_extunt > 3, yes = 1, no = 0)
survey$sicher_forsch_binary = ifelse(test = survey$si_forsch > 3, yes = 1, no = 0)
survey$sicher_rechts_binary = ifelse(test = survey$a_rsicher > 3, yes = 1, no = 0)

survey$sicher_speich_certainty_and_yes = ifelse(test = (survey$sicher_speich_binary == 1 & survey$erf_speich == 1), yes = 1, no = 0)
survey$sicher_intern_certainty_and_yes = ifelse(test = (survey$sicher_intern_binary == 1 & survey$erf_intern == 1), yes = 1, no = 0)
survey$sicher_extunt_certainty_and_yes = ifelse(test = (survey$sicher_extunt_binary == 1 & survey$erf_extunt == 1), yes = 1, no = 0)
survey$sicher_forsch_certainty_and_yes = ifelse(test = (survey$sicher_speich_binary == 1 & survey$erf_forsch == 1), yes = 1, no = 0)


# recode the agreement questions
survey$a_positiv_zustimmung = ifelse(test = survey$a_positiv > 3, yes = 1, no = 0)
survey$a_rsicher_zustimmung = ifelse(test = survey$a_rsicher > 3, yes = 1, no = 0)
survey$a_gefahr_zustimmung = ifelse(test = survey$a_gefahr > 3, yes = 1, no = 0)
survey$a_aufwand_zustimmung = ifelse(test = survey$a_aufwand > 3, yes = 1, no = 0)
survey$a_kompliz_zustimmung = ifelse(test = survey$a_kompliz > 3, yes = 1, no = 0)
survey$a_vorteil_zustimmung = ifelse(test = survey$a_vorteil > 3, yes = 1, no = 0)
survey$a_vertrau_zustimmung = ifelse(test = survey$a_vertrau > 3, yes = 1, no = 0)
survey$a_inno_zustimmung = ifelse(test = survey$a_inno > 3, yes = 1, no = 0)
survey$a_kost_zustimmung = ifelse(test = survey$a_kost > 3, yes = 1, no = 0)
survey$a_berat_zustimmung = ifelse(test = survey$a_berat > 3, yes = 1, no = 0)
survey$a_proz_zustimmung = ifelse(test = survey$a_proz > 3, yes = 1, no = 0)
survey$a_stand_zustimmung = ifelse(test = survey$a_stand > 3, yes = 1, no = 0)
survey$a_ki_zustimmung = ifelse(test = survey$a_ki > 3, yes = 1, no = 0)
survey$a_forsch_zustimmung = ifelse(test = survey$a_forsch > 3, yes = 1, no = 0)
survey$a_nachteil_zustimmung = ifelse(test = survey$a_nachteil > 3, yes = 1, no = 0)








# create binary variable to see if firm has high certainty in all scenarios
survey$always_high_certainty = ifelse(test = survey$si_speich > 3 & survey$si_intern > 3 & survey$si_extunt > 3 & survey$si_forsch > 3, yes = 1, no = 0)
table(survey$always_high_certainty)

# create binary variable for if firm has high certainty (4 or 5) on average
survey$avg_high_certainty = ifelse(test = survey$si_speich + survey$si_intern + survey$si_extunt + survey$si_forsch >= 16, yes = 1, no = 0)
table(survey$avg_high_certainty)




# use dummy coding to create new binary variables for pdwichtig
survey$pdwichtig2 <- ifelse(test = survey$pdwichtig == 2, yes = 1, no = 0)
survey$pdwichtig3 <- ifelse(test = survey$pdwichtig == 3, yes = 1, no = 0)
survey$pdwichtig4 <- ifelse(test = survey$pdwichtig == 4, yes = 1, no = 0)
survey$pdwichtig5 <- ifelse(test = survey$pdwichtig == 5, yes = 1, no = 0)

survey$pdwichtig23 <- ifelse(test = survey$pdwichtig == 2 | survey$pdwichtig == 3, yes = 1, no = 0)
survey$pdwichtig45 <- ifelse(test = survey$pdwichtig == 4 | survey$pdwichtig == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for aufwand
survey$aufwand2 <- ifelse(test = survey$aufwand == 2, yes = 1, no = 0)
survey$aufwand3 <- ifelse(test = survey$aufwand == 3, yes = 1, no = 0)
survey$aufwand4 <- ifelse(test = survey$aufwand == 4, yes = 1, no = 0)

# use dummy coding to create new binary variables for kund_dat
survey$kund_dat2 <- ifelse(test = survey$kund_dat == 2, yes = 1, no = 0)
survey$kund_dat3 <- ifelse(test = survey$kund_dat == 3, yes = 1, no = 0)
survey$kund_dat4 <- ifelse(test = survey$kund_dat == 4, yes = 1, no = 0)
survey$kund_dat5 <- ifelse(test = survey$kund_dat == 5, yes = 1, no = 0)

survey$kund_dat23 <- ifelse(test = survey$kund_dat == 2 | survey$kund_dat == 3, yes = 1, no = 0)
survey$kund_dat45 <- ifelse(test = survey$kund_dat == 4 | survey$kund_dat == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for datengm
survey$datengm2 <- ifelse(test = survey$datengm == 2, yes = 1, no = 0)
survey$datengm3 <- ifelse(test = survey$datengm == 3, yes = 1, no = 0)
survey$datengm4 <- ifelse(test = survey$datengm == 4, yes = 1, no = 0)
survey$datengm5 <- ifelse(test = survey$datengm == 5, yes = 1, no = 0)

survey$datengm23 <- ifelse(test = survey$datengm == 2 | survey$datengm == 3, yes = 1, no = 0)
survey$datengm45 <- ifelse(test = survey$datengm == 4 | survey$datengm == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for umheute
survey$umheute2 <- ifelse(test = survey$umheute == 2, yes = 1, no = 0)
survey$umheute3 <- ifelse(test = survey$umheute == 3, yes = 1, no = 0)
survey$umheute4 <- ifelse(test = survey$umheute == 4, yes = 1, no = 0)
survey$umheute5 <- ifelse(test = survey$umheute == 5, yes = 1, no = 0)

survey$umheute23 <- ifelse(test = survey$umheute == 2 | survey$umheute == 3, yes = 1, no = 0)
survey$umheute45 <- ifelse(test = survey$umheute == 4 | survey$umheute == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for ums
survey$umsgleich <- ifelse(test = survey$ums == "gleich", yes = 1, no = 0)
survey$umsgesunken <- ifelse(test = survey$ums == "runter", yes = 1, no = 0)

# use dummy coding to create new binary variables for nach
survey$nachgleich <- ifelse(test = survey$nach == "gleich", yes = 1, no = 0)
survey$nachgesunken <- ifelse(test = survey$nach == "runter", yes = 1, no = 0)

# use dummy coding to create new binary variables for pers
survey$persgleich <- ifelse(test = survey$pers == "gleich", yes = 1, no = 0)
survey$persgesunken <- ifelse(test = survey$pers == "runter", yes = 1, no = 0)

# use dummy coding to create new binary variables for umse
survey$umsegleich <- ifelse(test = survey$umse == "gleich", yes = 1, no = 0)
survey$umsegesunken <- ifelse(test = survey$umse == "runter", yes = 1, no = 0)

# use dummy coding to create new binary variables for nache
survey$nachegleich <- ifelse(test = survey$nache == "gleich", yes = 1, no = 0)
survey$nachegesunken <- ifelse(test = survey$nache == "runter", yes = 1, no = 0)

# use dummy coding to create new binary variables for perse
survey$persegleich <- ifelse(test = survey$perse == "gleich", yes = 1, no = 0)
survey$persegesunken <- ifelse(test = survey$perse == "runter", yes = 1, no = 0)

# use dummy coding to create new binary variables for a_positiv
survey$a_positiv2 <- ifelse(test = survey$a_positiv == 2, yes = 1, no = 0)
survey$a_positiv3 <- ifelse(test = survey$a_positiv == 3, yes = 1, no = 0)
survey$a_positiv4 <- ifelse(test = survey$a_positiv == 4, yes = 1, no = 0)
survey$a_positiv5 <- ifelse(test = survey$a_positiv == 5, yes = 1, no = 0)

survey$a_positiv23 <- ifelse(test = survey$a_positiv == 2 | survey$a_positiv == 3, yes = 1, no = 0)
survey$a_positiv45 <- ifelse(test = survey$a_positiv == 4 | survey$a_positiv == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for a_rsicher
survey$a_rsicher2 <- ifelse(test = survey$a_rsicher == 2, yes = 1, no = 0)
survey$a_rsicher3 <- ifelse(test = survey$a_rsicher == 3, yes = 1, no = 0)
survey$a_rsicher4 <- ifelse(test = survey$a_rsicher == 4, yes = 1, no = 0)
survey$a_rsicher5 <- ifelse(test = survey$a_rsicher == 5, yes = 1, no = 0)

survey$a_rsicher23 <- ifelse(test = survey$a_rsicher == 2 | survey$a_rsicher == 3, yes = 1, no = 0)
survey$a_rsicher45 <- ifelse(test = survey$a_rsicher == 4 | survey$a_rsicher == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for a_gefahr
survey$a_gefahr2 <- ifelse(test = survey$a_gefahr == 2, yes = 1, no = 0)
survey$a_gefahr3 <- ifelse(test = survey$a_gefahr == 3, yes = 1, no = 0)
survey$a_gefahr4 <- ifelse(test = survey$a_gefahr == 4, yes = 1, no = 0)
survey$a_gefahr5 <- ifelse(test = survey$a_gefahr == 5, yes = 1, no = 0)

survey$a_gefahr23 <- ifelse(test = survey$a_gefahr == 2 | survey$a_gefahr == 3, yes = 1, no = 0)
survey$a_gefahr45 <- ifelse(test = survey$a_gefahr == 4 | survey$a_gefahr == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for a_aufwand
survey$a_aufwand2 <- ifelse(test = survey$a_aufwand == 2, yes = 1, no = 0)
survey$a_aufwand3 <- ifelse(test = survey$a_aufwand == 3, yes = 1, no = 0)
survey$a_aufwand4 <- ifelse(test = survey$a_aufwand == 4, yes = 1, no = 0)
survey$a_aufwand5 <- ifelse(test = survey$a_aufwand == 5, yes = 1, no = 0)

survey$a_aufwand23 <- ifelse(test = survey$a_aufwand == 2 | survey$a_aufwand == 3, yes = 1, no = 0)
survey$a_aufwand45 <- ifelse(test = survey$a_aufwand == 4 | survey$a_aufwand == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for a_kompliz
survey$a_kompliz2 <- ifelse(test = survey$a_kompliz == 2, yes = 1, no = 0)
survey$a_kompliz3 <- ifelse(test = survey$a_kompliz == 3, yes = 1, no = 0)
survey$a_kompliz4 <- ifelse(test = survey$a_kompliz == 4, yes = 1, no = 0)
survey$a_kompliz5 <- ifelse(test = survey$a_kompliz == 5, yes = 1, no = 0)

survey$a_kompliz23 <- ifelse(test = survey$a_kompliz == 3 | survey$a_kompliz == 2, yes = 1, no = 0)
survey$a_kompliz45 <- ifelse(test = survey$a_kompliz == 4 | survey$a_kompliz == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for a_vorteil
survey$a_vorteil2 <- ifelse(test = survey$a_vorteil == 2, yes = 1, no = 0)
survey$a_vorteil3 <- ifelse(test = survey$a_vorteil == 3, yes = 1, no = 0)
survey$a_vorteil4 <- ifelse(test = survey$a_vorteil == 4, yes = 1, no = 0)
survey$a_vorteil5 <- ifelse(test = survey$a_vorteil == 5, yes = 1, no = 0)

survey$a_vorteil23 <- ifelse(test = survey$a_vorteil == 2 | survey$a_vorteil == 3, yes = 1, no = 0)
survey$a_vorteil45 <- ifelse(test = survey$a_vorteil == 4 | survey$a_vorteil == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for a_vertrau
survey$a_vertrau2 <- ifelse(test = survey$a_vertrau == 2, yes = 1, no = 0)
survey$a_vertrau3 <- ifelse(test = survey$a_vertrau == 3, yes = 1, no = 0)
survey$a_vertrau4 <- ifelse(test = survey$a_vertrau == 4, yes = 1, no = 0)
survey$a_vertrau5 <- ifelse(test = survey$a_vertrau == 5, yes = 1, no = 0)

survey$a_vertrau23 <- ifelse(test = survey$a_vertrau == 2 | survey$a_vertrau == 3, yes = 1, no = 0)
survey$a_vertrau45 <- ifelse(test = survey$a_vertrau == 4 | survey$a_vertrau == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for a_inno
survey$a_inno2 <- ifelse(test = survey$a_inno == 2, yes = 1, no = 0)
survey$a_inno3 <- ifelse(test = survey$a_inno == 3, yes = 1, no = 0)
survey$a_inno4 <- ifelse(test = survey$a_inno == 4, yes = 1, no = 0)
survey$a_inno5 <- ifelse(test = survey$a_inno == 5, yes = 1, no = 0)

survey$a_inno23 <- ifelse(test = survey$a_inno == 2 | survey$a_inno == 3, yes = 1, no = 0)
survey$a_inno45 <- ifelse(test = survey$a_inno == 4 | survey$a_inno == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for a_kost
survey$a_kost2 <- ifelse(test = survey$a_kost == 2, yes = 1, no = 0)
survey$a_kost3 <- ifelse(test = survey$a_kost == 3, yes = 1, no = 0)
survey$a_kost4 <- ifelse(test = survey$a_kost == 4, yes = 1, no = 0)
survey$a_kost5 <- ifelse(test = survey$a_kost == 5, yes = 1, no = 0)

survey$a_kost23 <- ifelse(test = survey$a_kost == 2 | survey$a_kost == 3, yes = 1, no = 0)
survey$a_kost45 <- ifelse(test = survey$a_kost == 4 | survey$a_kost == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for a_berat
survey$a_berat2 <- ifelse(test = survey$a_berat == 2, yes = 1, no = 0)
survey$a_berat3 <- ifelse(test = survey$a_berat == 3, yes = 1, no = 0)
survey$a_berat4 <- ifelse(test = survey$a_berat == 4, yes = 1, no = 0)
survey$a_berat5 <- ifelse(test = survey$a_berat == 5, yes = 1, no = 0)

survey$a_berat23 <- ifelse(test = survey$a_berat == 2 | survey$a_berat == 3, yes = 1, no = 0)
survey$a_berat45 <- ifelse(test = survey$a_berat == 4 | survey$a_berat == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for a_proz
survey$a_proz2 <- ifelse(test = survey$a_proz == 2, yes = 1, no = 0)
survey$a_proz3 <- ifelse(test = survey$a_proz == 3, yes = 1, no = 0)
survey$a_proz4 <- ifelse(test = survey$a_proz == 4, yes = 1, no = 0)
survey$a_proz5 <- ifelse(test = survey$a_proz == 5, yes = 1, no = 0)

survey$a_proz23 <- ifelse(test = survey$a_proz == 2 | survey$a_proz == 3, yes = 1, no = 0)
survey$a_proz45 <- ifelse(test = survey$a_proz == 4 | survey$a_proz == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for a_stand
survey$a_stand2 <- ifelse(test = survey$a_stand == 2, yes = 1, no = 0)
survey$a_stand3 <- ifelse(test = survey$a_stand == 3, yes = 1, no = 0)
survey$a_stand4 <- ifelse(test = survey$a_stand == 4, yes = 1, no = 0)
survey$a_stand5 <- ifelse(test = survey$a_stand == 5, yes = 1, no = 0)

survey$a_stand23 <- ifelse(test = survey$a_stand == 2 | survey$a_stand == 3, yes = 1, no = 0)
survey$a_stand45 <- ifelse(test = survey$a_stand == 4 | survey$a_stand == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for a_ki
survey$a_ki2 <- ifelse(test = survey$a_ki == 2, yes = 1, no = 0)
survey$a_ki3 <- ifelse(test = survey$a_ki == 3, yes = 1, no = 0)
survey$a_ki4 <- ifelse(test = survey$a_ki == 4, yes = 1, no = 0)
survey$a_ki5 <- ifelse(test = survey$a_ki == 5, yes = 1, no = 0)

survey$a_ki23 <- ifelse(test = survey$a_ki == 2 | survey$a_ki == 3, yes = 1, no = 0)
survey$a_ki45 <- ifelse(test = survey$a_ki == 4 | survey$a_ki == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for a_forsch
survey$a_forsch2 <- ifelse(test = survey$a_forsch == 2, yes = 1, no = 0)
survey$a_forsch3 <- ifelse(test = survey$a_forsch == 3, yes = 1, no = 0)
survey$a_forsch4 <- ifelse(test = survey$a_forsch == 4, yes = 1, no = 0)
survey$a_forsch5 <- ifelse(test = survey$a_forsch == 5, yes = 1, no = 0)

survey$a_forsch23 <- ifelse(test = survey$a_forsch == 2 | survey$a_forsch == 3, yes = 1, no = 0)
survey$a_forsch45 <- ifelse(test = survey$a_forsch == 4 | survey$a_forsch == 5, yes = 1, no = 0)

# use dummy coding to create new binary variables for a_nachteil
survey$a_nachteil2 <- ifelse(test = survey$a_nachteil == 2, yes = 1, no = 0)
survey$a_nachteil3 <- ifelse(test = survey$a_nachteil == 3, yes = 1, no = 0)
survey$a_nachteil4 <- ifelse(test = survey$a_nachteil == 4, yes = 1, no = 0)
survey$a_nachteil5 <- ifelse(test = survey$a_nachteil == 5, yes = 1, no = 0)

survey$a_nachteil23 <- ifelse(test = survey$a_nachteil == 2 | survey$a_nachteil == 3, yes = 1, no = 0)
survey$a_nachteil45 <- ifelse(test = survey$a_nachteil == 4 | survey$a_nachteil == 5, yes = 1, no = 0)

# turn the erf variables to 0s and 1s instead of 1s and 2s
survey$erf_extunt <- ifelse(test = survey$erf_extunt == 2, yes = 0, no = 1)
survey$erf_intern <- ifelse(test = survey$erf_intern == 2, yes = 0, no = 1)
survey$erf_forsch <- ifelse(test = survey$erf_forsch == 2, yes = 0, no = 1)
survey$erf_speich <- ifelse(test = survey$erf_speich == 2, yes = 0, no = 1)



two_variable_table = table(survey$sicher_speich_binary, survey$erf_speich)
two_variable_table
chi_sq_results = chisq.test(two_variable_table)
print(chi_sq_results)
# chi_sq test with always_high_certainty is significant with kund_dat (0.02265), umheute (9.62e-09), pdwichtig (0.0231), a_kost (0.01341)
# chi_sq test with avg_high_certainty is significant with a_kost (8.14e-07), kund_dat(0.001759), 



# create a multivariable binary logistic regression model to detect if firm has high certainty
# response variables of interest: ums + nach + pers + aufwand + kund_dat + umse + nache + perse + umheute + datengm + pdwichtig + a_kompliz
y = survey$erf_extunt
multivar_logistic_model = glm(formula = y ~ 
                                pdwichtig2 + pdwichtig3 + pdwichtig4 + pdwichtig5 
                              + aufwand2 + aufwand3 + aufwand4  
                              + kund_dat2 + kund_dat3 + kund_dat4 + kund_dat5
                              + datengm2 + datengm3 + datengm4 + datengm5
                              + umheute2 + umheute3 + umheute4 + umheute5
                              + umsgleich + umsgesunken + umsegleich + umsegesunken
                              + nachgleich + nachgesunken + nachegleich + nachegesunken
                              + persgleich + persgesunken + persegleich + persegesunken
                              + a_positiv2 + a_positiv3 + a_positiv4 + a_positiv5
                              + a_rsicher2 + a_rsicher3 + a_rsicher4 + a_rsicher5
                              + a_gefahr2 + a_gefahr3 + a_gefahr4 + a_gefahr5
                              + a_aufwand2 + a_aufwand3 + a_aufwand4 + a_aufwand5
                              + a_kompliz2 + a_kompliz3 + a_kompliz4 + a_kompliz5
                              + a_vorteil2 + a_vorteil3 + a_vorteil4 + a_vorteil5
                              + a_vertrau2 + a_vertrau3 + a_vertrau4 + a_vertrau5
                              + a_inno2 + a_inno3 + a_inno4 + a_inno5
                              + a_kost2 + a_kost3 + a_kost4 + a_kost5
                              + a_berat2 + a_berat3 + a_berat4 + a_berat5
                              + a_proz2 + a_proz3 + a_proz4 + a_proz5
                              + a_stand2 + a_stand3 + a_stand4 + a_stand5
                              + a_ki2 + a_ki3 + a_ki4 + a_ki5
                              + a_forsch2 + a_forsch3 + a_forsch4 + a_forsch5
                              + a_nachteil2 + a_nachteil3 + a_nachteil4 + a_nachteil5, 
                              data = survey, family = 'binomial')
summary(multivar_logistic_model)$coefficients

# Test Model Above
pred_probs <- predict(multivar_logistic_model, type = "response") # Get predicted probabilities
pred_class <- ifelse(pred_probs >= 0.5, 1, 0) # Convert probabilities to 0 or 1 using a threshold (commonly 0.5)
actual <- y # Actual values
accuracy <- mean(pred_class == actual) # Accuracy calculation
print(paste("Accuracy:", round(accuracy, 4)))



y = survey$erf_extunt
multivar_logistic_model6 = glm(formula = y ~ 
                                pdwichtig23 + pdwichtig45 
                              + aufwand2 + aufwand3 + aufwand4  
                              + kund_dat23 + kund_dat45
                              + datengm23 + datengm45
                              + umheute23 + umheute45
                              + umsgleich + umsgesunken + umsegleich + umsegesunken
                              + nachgleich + nachgesunken + nachegleich + nachegesunken
                              + persgleich + persgesunken + persegleich + persegesunken
                              + a_positiv23 + a_positiv45
                              + a_rsicher23 + a_rsicher45
                              + a_gefahr23 + a_gefahr45
                              + a_aufwand23 + a_aufwand45
                              + a_kompliz23 + a_kompliz45
                              + a_vorteil23 + a_vorteil45
                              + a_vertrau23 + a_vertrau45
                              + a_inno23 + a_inno45
                              + a_kost23 + a_kost45
                              + a_berat23 + a_berat45
                              + a_proz23 + a_proz45
                              + a_stand23 + a_stand45
                              + a_ki23 + a_ki45
                              + a_forsch23 + a_forsch45
                              + a_nachteil23 + a_nachteil45, 
                              data = survey, family = 'binomial')
summary(multivar_logistic_model6)$coefficients

# Test Model Above
pred_probs <- predict(multivar_logistic_model6, type = "response") # Get predicted probabilities
pred_class <- ifelse(pred_probs >= 0.5, 1, 0) # Convert probabilities to 0 or 1 using a threshold (commonly 0.5)
actual <- y # Actual values
print(paste("Accuracy:", round(mean(pred_class == actual), 4)))







# predictable variables: a_positiv, a_rsicher, a_gefahr, a_vorteil, a_vertrau, a_inno, a_berat (only with sicher_extunt_binary), a_proz, a_stand, a_ki, a_forsch, a_nachteil
y = survey$a_nachteil_zustimmung
multivar_logistic_model2 = glm(formula = y ~ 
                                 sicher_forsch_binary + sicher_extunt_binary + sicher_intern_binary
                               + sicher_speich_binary + sicher_rechts_binary, 
                              data = survey, family = 'binomial')
summary(multivar_logistic_model2)$coefficients


# Test Model Above
pred_probs <- predict(multivar_logistic_model2, type = "response") # Get predicted probabilities
pred_class <- ifelse(pred_probs >= 0.5, 1, 0) # Convert probabilities to 0 or 1 using a threshold (commonly 0.5)
print(paste("Accuracy:", round(mean(pred_class == y), 4)))



y = survey$a_nachteil_zustimmung
multivar_logistic_model3 = glm(formula = y ~ 
                                 sicher_forsch_binary + sicher_extunt_binary
                               + sicher_intern_binary + sicher_speich_binary 
                               + sicher_rechts_binary + erf_speich + erf_intern
                               + erf_forsch + erf_extunt, 
                               data = survey, family = 'binomial')
summary(multivar_logistic_model3)$coefficients


# Test Model Above
pred_probs <- predict(multivar_logistic_model3, type = "response") # Get predicted probabilities
pred_class <- ifelse(pred_probs >= 0.5, 1, 0) # Convert probabilities to 0 or 1 using a threshold (commonly 0.5)
print(paste("Accuracy:", round(mean(pred_class == y), 4)))





y = survey$a_positiv_zustimmung
multivar_logistic_model4 = glm(formula = y ~ 
                               erf_speich + erf_intern
                               + erf_forsch + erf_extunt, 
                               data = survey, family = 'binomial')
summary(multivar_logistic_model4)$coefficients


# Test Model Above
pred_probs <- predict(multivar_logistic_model4, type = "response") # Get predicted probabilities
pred_class <- ifelse(pred_probs >= 0.5, 1, 0) # Convert probabilities to 0 or 1 using a threshold (commonly 0.5)
print(paste("Accuracy:", round(mean(pred_class == y), 4)))











multivar_logistic_model5 = glm(formula = a_rsicher_zustimmung ~ 
                                 sicher_speich_certainty_and_yes + sicher_intern_certainty_and_yes
                               + sicher_extunt_certainty_and_yes + sicher_forsch_certainty_and_yes, 
                               data = survey, family = 'binomial')
summary(multivar_logistic_model5)$coefficients


# Test Model Above
pred_probs <- predict(multivar_logistic_model5, type = "response") # Get predicted probabilities
pred_class <- ifelse(pred_probs >= 0.5, 1, 0) # Convert probabilities to 0 or 1 using a threshold (commonly 0.5)
actual <- survey$a_rsicher_zustimmung # Actual values
accuracy <- mean(pred_class == actual) # Accuracy calculation
print(paste("Accuracy:", round(accuracy, 4)))











cat(names(survey), sep = "\n") # get list of current survey columns in order
survey <- survey[, c("ums", "nach", "pers", # reorder list of columns in survey
                     "umse", "nache", "perse",
                     "pdwichtig", "umheute", "aspekte", "aufwand",
                     "behoerde", "beh_grund", "unsicher", "datengm",
                     "ds_unter", "ds_forsch", "kund_dat",
                     "a_positiv", "a_rsicher", "a_gefahr", "a_aufwand",
                     "a_kompliz", "a_vorteil", "a_vertrau", "a_inno", "a_kost", 
                     "a_berat", "a_proz", "a_stand", "a_ki", "a_forsch","a_nachteil", 
                     "erf_speich", "erf_intern", "erf_extunt", "erf_forsch", "always_high_certainty", 
                     "si_speich", "si_intern", "si_forsch", "si_extunt",
                     "br_zahl", "br08", "bges", "gk9", "gk4", "aweight", "online", "vg", "br04", "br07",
                     "sicher_speich_binary", "sicher_intern_binary", "sicher_extunt_binary", "sicher_forsch_binary", "sicher_rechts_binary")]
