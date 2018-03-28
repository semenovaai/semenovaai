library("tidyverse") 
library("tidyr") 
library("stringr") 
library("dplyr") 	
library("tibble") 
library("readr") 
## Читаем файл 
{tb1=read.csv("C:/R/eddypro.csv", 
             skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
tb1}
#удаляем первую строчку 
{tb1 = tb1[-1,] 
tb1 }
#смотрим информацию по столбцам 
{glimpse(tb1) }
# удаляем из таблицы столбик ролл 
{tb1 = select(tb1, -(roll)) 
tb1<-tb1[,c(-1,-3,-9,-12,-15,-18,-21,-30,-35,-70,-88:-99)] }
# Преобразуем в факторы переменные типа char, которые содержат повторяющиеся значения 
{tb1 = tb1 %>% mutate_if(is.character, factor) }
# Убираем проблему со знаками в переменных 
{names(tb1) = str_replace_all(names(tb1), "[!]","_emph_") 
names(tb1) = names(tb1) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(tb1) 
sapply(tbl,is.numeric) }
#Оставляем только численные данные 
{tb1_numeric = tb1[,sapply(tb1,is.numeric) ] 
tb1_non_numeric = tbl[,!sapply(tb1,is.numeric) ] }
# Приводим к типу logical колонку daytime 
{tb1$daytime = as.logical(tb1$daytime) }
#Оставляем август 
{tb1 = subset(tb1, as.Date(date) >= as.Date("2013-08-13") & as.Date(date) <= as.Date("2013-08-31")
             tb1 }
             #Корреляционный анализ 
            { cor_td = cor(tb1_numeric) 
             cor_td }
             #Избавляемся от всех строк, где есть хоть одно значение NA 
             {cor_td = cor(drop_na(tb1_numeric)) %>% as.data.frame %>% select(h2o_flux) 
             vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude 
             vars }
             ## Собираем все переменные из вектора с именами переменных в одну формулу 
             {formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")) 
             formula }
             #создание обучающей выборки 
             {row_numbers = 1:length(tb1$date) 
             teach = sample(row_numbers, floor(length(tb1$date)*.7)) 
             test = row_numbers[-teach] 
             teaching_tb1_unq = tb1[teach,] 
             testing_tb1_unq = tb1[test,] }
             #создание и анализ модели множественной регрессии c взаимодействием 
            { model1=lm((h2o_flux ~ DOY + Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
                          rand_err_LE + co2_flux + rand_err_co2_flux + h2o_flux + qc_h2o_flux + 
                          rand_err_h2o_flux + H_strg + co2_molar_density + h2o_time_lag + 
                          sonic_temperature + air_temperature + air_density + air_molar_volume + 
                          es + RH + VPD + u. + TKE + T. + un_Tau + un_H + un_LE + un_co2_flux + 
                          un_h2o_flux + mean_value + u_var + v_var + w_var + h2o_var + 
                          w.ts_cov + w.co2_cov + w.h2o_cov + co2_signal_strength_7200 + 
                          h2o_signal_strength_7200 + flowrate)^2,data=tb1) 
             coef(model1) 
             resid(model1) 
             confint(model1) 
             summary(model1) 
             anova(model1) 
             
             
             model2=lm((h2o_flux ~ DOY + Tau + H + LE + 
                          co2_flux + h2o_flux + 
                          H_strg + co2_molar_density + h2o_time_lag + 
                          sonic_temperature + air_temperature + air_density + air_molar_volume + 
                          es + RH + VPD + u. + TKE + T.)^2,data=tb1) 
             coef(model2) 
             resid(model2) 
             confint(model2) 
             summary(model2) 
             anova(model2) 
             
             model3=lm((h2o_flux ~ DOY + Tau + H + LE + 
                          co2_flux + h2o_flux + 
                          H_strg + co2_molar_density + h2o_time_lag + 
                          sonic_temperature + air_temperature + air_density + air_molar_volume + 
                          es + RH + VPD + u. + TKE + T.)^2-Tau:H -Tau:LE -Tau:co2_flux - 
                         Tau:co2_molar_density-Tau:H_strg-Tau:h2o_time_lag-Tau:air_density - 
                         Tau:air_molar_volume-Tau:u.-Tau:T.-H:LE-H:co2_molar_density 
                       -H:h2o_time_lag -H:air_density-H_strg:es-H_strg:RH-H_strg:VPD-H_strg:u.-co2_molar_density:h2o_time_lag 
                       - co2_molar_density:u.-co2_molar_density:T.-h2o_time_lag:u. 
                       -h2o_time_lag:T.-sonic_temperature:RH -air_density:VPD-air_density:u. 
                       -air_molar_volume:u.-es:RH-air_molar_volume:es ,data=tb1) 
             coef(model3)
             resid(model3) 
             confint(model3) 
             summary(model3) 
             anova(model3) 
             plot(model3) }
             #Оптимальная модель-3
             