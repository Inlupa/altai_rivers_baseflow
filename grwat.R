#Sys.setlocale("LC_ALL","russian")
# Считываем библиотеки
library(grwat)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(prettyunits)

# функция подгона исходных данных под нужный формат библиотеки GrWat
convert_data = function(df){
  df$q = str_replace(df$q, ',', '.')
  df$p = str_replace(df$p, ',', '.')
  df$t = str_replace(df$t, ',', '.')
  
  df$q = as.numeric(df$q)
  df$p = as.numeric(df$p)
  df$t = as.numeric(df$t)
  
  df$date = as.Date(df$date, format = "%Y-%m-%d")
  return(df)
}
# Считываем данные исходника для каждой реки
data_belaya = read.csv("D:\\Аспирантура\\altai\\исходники для каждой реки\\Белая.csv", header=TRUE,  sep=";")
data_alambai = read.csv("D:\\Аспирантура\\altai\\исходники для каждой реки\\Аламбай.csv", header=TRUE,  sep=";")
data_bolshaya = read.csv("D:\\Аспирантура\\altai\\исходники для каждой реки\\Большая Речка.csv", header=TRUE,  sep=";")
data_locktevka = read.csv("D:\\Аспирантура\\altai\\исходники для каждой реки\\Локтевка.csv", header=TRUE,  sep=";")
data_maima = read.csv("D:\\Аспирантура\\altai\\исходники для каждой реки\\Майма.csv", header=TRUE,  sep=";")
# Список рек (кроме Белой, она считается только при указании 7 региона)
list_of_rivers = list(data_alambai, data_bolshaya, data_locktevka, data_maima)
# параметры территории
p = gr_get_params()
p$filter = 'kudelin'
# Приводим данные к нужному виду
list_of_rivers = map(list_of_rivers, convert_data)

# функция для расчета подземного стока для кажой из реки
# return(vars) возвращает статистику
# return(kudelin) возвращает подземный сток
grwat_analitics = function(df){
  data = tibble(Date = df$date, Q = df$q, Temp = df$t, Perc = df$p)
  #тут идет обработка данных для метода куделина(то что нужно больше всего)
  kudelin = gr_separate(data, p)
  vars = gr_summarize(kudelin)
  return(vars)
  # return(kudelin)
}
# Расчет значений при помощи GrWat
result_baseflow = map(list_of_rivers, grwat_analitics)

# Сохраняем графики

# png(file= 'belaya_stats.png', width = 1000, height = 600)
# gr_plot_vars(tibble(result_baseflow[[1]]), Qygr, tests = TRUE)
# dev.off()

png(file= 'alambai_pond.png', width = 1000, height = 600)
gr_plot_vars(tibble(result_baseflow[[1]]), Qsmin, tests = TRUE)
dev.off()

png(file= 'bolshaya_pond.png', width = 1000, height = 600)
gr_plot_vars(tibble(result_baseflow[[2]]), Qsmin, tests = TRUE)
dev.off()

png(file= 'locktevka_pond.png', width = 1000, height = 600)
gr_plot_vars(tibble(result_baseflow[[3]]), Qsmin, tests = TRUE)
dev.off()

png(file= 'maima_pond.png', width = 1000, height = 600)
gr_plot_vars(tibble(result_baseflow[[4]]), Qsmin, tests = TRUE)
dev.off()
result_baseflow


# сохраняем статистики по массивам данных рек
write.csv(gr_test_vars(result_baseflow[[1]])$pvalues, "D:\\Аспирантура\\altai\\tests_alambai.csv", sep=";",row.names=TRUE)
write.csv(gr_test_vars(result_baseflow[[2]])$pvalues, "D:\\Аспирантура\\altai\\tests_bolshaya.csv", sep=";",row.names=TRUE)
write.csv(gr_test_vars(result_baseflow[[3]])$pvalues, "D:\\Аспирантура\\altai\\tests_locktevka.csv", sep=";",row.names=TRUE)
write.csv(gr_test_vars(result_baseflow[[4]])$pvalues, "D:\\Аспирантура\\altai\\tests_maima.csv", sep=";",row.names=TRUE)

# сохраняем данные по рассчитанному подземному стоку
write.csv(result_baseflow[[1]], "D:\\Аспирантура\\altai\\alambai_kudelin.csv", row.names=TRUE)
write.csv(result_baseflow[[2]], "D:\\Аспирантура\\altai\\bolshaya_kudelin.csv", row.names=TRUE)
write.csv(result_baseflow[[3]], "D:\\Аспирантура\\altai\\locktevka_kudelin.csv", row.names=TRUE)
write.csv(result_baseflow[[4]], "D:\\Аспирантура\\altai\\maima_kudelin.csv", row.names=TRUE)

# Это какие переменные находятся в статистическом анализе
x = gr_help_vars()
x$Name

