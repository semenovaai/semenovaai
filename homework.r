library("tidyverse") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr")

#Затем задаём программе расположение файла с исходными данными и читаем его
tb1=read.csv("C:/R/eddypro.csv", 
             skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
tb1

#Удаляем первую строчку
tb1 = tb1[-1,] tb1

#Смотрим информацию по столбцам
glimpse(tb1)

#Удаляем из таблицы столбик ролл
tb1 = select(tb1, -(roll)) 
tb1<-tb1[,c(-1,-3,-9,-12,-15,-18,-21,-30,-35,-70,-88:-99)]

#Преобразуем в факторы переменные типа char, которые содержат повторяющиеся значения
tb1 = tb1 %>% mutate_if(is.character, factor)

#Убираем проблему со знаками в переменных
names(tb1) = str_replace_all(names(tb1), "[!]","emph") 
names(tb1) = names(tb1) %>% 
  str_replace_all("[!]","emph") %>% 
  str_replace_all("[?]","quest") %>% 
  str_replace_all("[*]","star") %>% 
  str_replace_all("[+]","plus") %>% 
  str_replace_all("[-]","minus") %>% 
  str_replace_all("[@]","at") %>% 
  str_replace_all("[$]","dollar") %>% 
  str_replace_all("[#]","hash") %>% 
  str_replace_all("[/]","div") %>% 
  str_replace_all("[%]","perc") %>% 
  str_replace_all("[&]","amp") %>% 
  str_replace_all("[\^]","power") %>% 
  str_replace_all("[()]","_") 
glimpse(tb1) 
sapply(tbl,is.numeric)

#Оставляем только численные данные
tb1_numeric = tb1[,sapply(tb1,is.numeric) ] 
tb1_non_numeric = tbl[,!sapply(tb1,is.numeric) ]

#Приводим к типу logical колонку daytime
tb1$daytime = as.logical(tb1$daytime)

#Оставляем лето
tb1 = subset(tb1, as.Date(date) >= as.Date("2013-06-01") & as.Date(date) <= as.Date("2013-08-31") tb1
#Корреляционный анализ 
cor_td = cor(tb1_numeric) cor_td
             
#Избавляемся от всех строк, где есть хоть одно значение NA
cor_td = cor(drop_na(tb1_numeric)) %>% as.data.frame %>% select(h2o_flux) 
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude 
vars
             
#Собираем все переменные из вектора с именами переменных в одну формулу
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")) 
formula
             
#Создание обучающей выборки
row_numbers = 1:length(tb1$date) 
teach = sample(row_numbers, floor(length(tb1$date)*.7)) 
test = row_numbers[-teach] 
teaching_tb1_unq = tb1[teach,] 
testing_tb1_unq = tb1[test,]

#Создание и анализ модели множественной регрессии c взаимодействием
mod1=lm((h2o_flux ~ (Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + rand_err_LE + co2_flux + rand_err_co2_flux + h2o_flux + qc_h2o_flux + rand_err_h2o_flux + H_strg + co2_molar_density + h2o_time_lag + sonic_temperature + air_temperature + air_density + air_molar_volume + es + RH + VPD + u. + TKE + T. + un_Tau + un_H + un_LE + un_co2_flux + un_h2o_flux + mean_value + u_var + v_var + w_var + h2o_var + w.ts_cov + w.co2_cov + w.h2o_cov + co2_signal_strength_7200 + h2o_signal_strength_7200 + flowrate)^2,data=tb1) 
coef(mod1) 
resid(mod1) 
confint(mod1) 
summary(mod1) 
anova(mod1)
                   
#получена модель 2 
mod2=lm(h2o_flux ~ (Tau + H + LE + co2_flux + h2o_flux + H_strg + co2_molar_density + h2o_time_lag + sonic_temperature + air_temperature + air_density + air_molar_volume + es+ RH + VPD + u. + TKE + T.),data=tb1) 
coef(mod2) 
resid(mod2) 
confint(mod2) 
summary(mod2) 
anova(mod2)
                     
#получена модель 3 
mod3=lm(h2o_flux ~ (Tau + H + LE + co2_flux + h2o_flux + H_strg + co2_molar_density + h2o_time_lag + sonic_temperature + air_temperature + air_density + air_molar_volume + es+ RH + VPD + u. + TKE + T.)^2,data=tb1) 
coef(mod3) 
resid(mod3) 
confint(mod3) 
summary(mod3) 
anova(mod3)
                     
#получена модель 4 
mod4=lm(h2o_flux ~ (Tau + H + LE + co2_flux + h2o_flux + H_strg + co2_molar_density + h2o_time_lag + sonic_temperature + air_temperature + air_density + air_molar_volume + es + RH + VPD + u. + TKE + T.)^2-Tau:H -Tau:LE -Tau:co2_flux - Tau:co2_molar_density-Tau:H_strg-Tau:h2o_time_lag-Tau:air_density - Tau:air_molar_volume-Tau:u.-Tau:T.-H:LE-H:co2_molar_density-H:h2o_time_lag -H:air_density-H_strg:es-H_strg:RH-H_strg:VPD-H_strg:u.-co2_molar_density:h2o_time_lag - co2_molar_density:u.-co2_molar_density:T.-h2o_time_lag:u. -h2o_time_lag:T.-sonic_temperature:RH -air_density:VPD-air_density:u. -air_molar_volume:u.-es:RH-air_molar_volume:es ,data=tb1) 
coef(mod4) 
resid(mod4) 
confint(mod4) 
summary(mod4) 
anova(mod4) 
plot(mod4)
                     
#получена модель 5 
mod5=lm(h2o_flux ~ (Tau + H + LE + co2_flux + h2o_flux + H_strg + co2_molar_density + h2o_time_lag + sonic_temperature + air_temperature + VPD)^2-air_density - air_molar_volume - es - RH - u. - TKE + T. - Tau:H -Tau:LE -Tau:co2_flux - Tau:co2_molar_density-Tau:H_strg-Tau:h2o_time_lag-Tau:air_density - Tau:air_molar_volume-Tau:u.-Tau:T.-H:LE-H:co2_molar_density-H:h2o_time_lag -H:air_density-H_strg:es-H_strg:RH-H_strg:VPD-H_strg:u.-co2_molar_density:h2o_time_lag - co2_molar_density:u.-co2_molar_density:T.-h2o_time_lag:u. -h2o_time_lag:T.-sonic_temperature:RH -air_density:VPD-air_density:u. -air_molar_volume:u.-es:RH-air_molar_volume:es ,data=tb1) 
coef(mod5) 
resid(mod5) 
confint(mod5) 
summary(mod5) 
anova(mod5) 
plot(mod5)
                     
#получена модель 6 
mod6=lm(h2o_flux ~ (Tau + H + LE + co2_flux + h2o_flux)^2-air_density - air_molar_volume - es - RH - u. - TKE - T. - co2_molar_density - h2o_time_lag - sonic_temperature - air_temperature - VPD - H_strg - Tau:H -Tau:LE -Tau:co2_flux - Tau:co2_molar_density-Tau:H_strg-Tau:h2o_time_lag-Tau:air_density - Tau:air_molar_volume-Tau:u.-Tau:T.-H:LE-H:co2_molar_density-H:h2o_time_lag -H:air_density-H_strg:es-H_strg:RH-H_strg:VPD-H_strg:u.-co2_molar_density:h2o_time_lag - co2_molar_density:u.-co2_molar_density:T.-h2o_time_lag:u. -h2o_time_lag:T.-sonic_temperature:RH -air_density:VPD-air_density:u. -air_molar_volume:u.-es:RH-air_molar_volume:es ,data=tb1) 
coef(mod6) 
resid(mod6) 
confint(mod6) 
summary(mod6) 
anova(mod6) 
plot(mod6)
                     
#получена модель 7 
mod7=lm(h2o_flux ~ (Tau + H)^2-air_density - air_molar_volume - es - RH - u. - TKE - T. - co2_molar_density - h2o_time_lag - sonic_temperature - air_temperature - VPD - H_strg - co2_flux + h2o_flux - LE - Tau:H -Tau:LE -Tau:co2_flux - Tau:co2_molar_density-Tau:H_strg-Tau:h2o_time_lag-Tau:air_density - Tau:air_molar_volume-Tau:u.-Tau:T.-H:LE-H:co2_molar_density-H:h2o_time_lag -H:air_density-H_strg:es-H_strg:RH-H_strg:VPD-H_strg:u.-co2_molar_density:h2o_time_lag - co2_molar_density:u.-co2_molar_density:T.-h2o_time_lag:u. -h2o_time_lag:T.-sonic_temperature:RH -air_density:VPD-air_density:u. -air_molar_volume:u.-es:RH-air_molar_volume:es ,data=tb1) 
coef(mod7) 
resid(mod7) 
confint(mod7) 
summary(mod7) 
anova(mod7) 
plot(mod7)
