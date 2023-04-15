library(readr)
install.packages("tidyverse")
library(tidyverse)

data = read.csv("../data/lab_sodra.csv")

#Duomenų filtravimas pagal kodą
filtered_data = data %>%
  filter(ecoActCode == 412000)

#1 užduotis
#Atfiltravę priskirtą įmonę pagal ekonominės veiklos kodą, nubrėžkite histogramą vidutiniam atlyginimui.

filtered_data %>%
  ggplot(aes(x=avgWage)) +
  theme_minimal() +
  geom_histogram(fill = "pink", col = "black", bins = 100) + 
  labs(title = "Histogram of average wages")

#2 užduotis
#Išrinkite 5 įmones, kurių vidutinis darbo užmokestis buvo didžiausias pagal nurodytą veiklos sritį.
#Atvaizduokite šių įmoniu˛ vidutinio atlyginimo kitimo dinamiką metų eigoje.

#Atskiria menėsius nuo pilnos datos
bymonth_filtered_data = filtered_data %>% mutate(month_value = as.integer(substr(month, 5, 7)))

#Suranda 5 įmones su didžiausiais vidutiniais atlyginimais
highest_wages_bymonth = bymonth_filtered_data %>%
  group_by(name) %>%
  slice_max(avgWage, n = 1)%>%
  ungroup() %>%
  top_n(avgWage, n = 5) %>%
  select(name)

#Atrenka įmones iš bendro sąrašo pagal prieš tai surastus įmonių vardus
highest_wages = bymonth_filtered_data%>% filter(name %in% highest_wages_bymonth$name)
#Vaizduoja duomenis 
highest_wages %>%
  ggplot(aes(x = month_value, y = avgWage, group = name)) +
  theme_minimal() +
  geom_point(aes(colour = name)) +
  scale_x_continuous("month",breaks=1:12,limits=c(1,12)) + 
  geom_line(aes(colour = name)) +
  labs(title = "Average wage of employees", x = "Month", y = "Average wage")

#3 Iš anksčiau išrinktų 5 įmonių, išrinkite maksimalų apdraustų darbuotojų
#skaičių per šiuos metus. Atvaizduokite stulpeline diagrama mažėjimo tvarka.

highest_wages %>%
  group_by(name) %>%
  slice_max(numInsured, with_ties = FALSE) %>%
  head(5) %>%
  ggplot(aes(x = reorder(name, -numInsured), y = numInsured)) +
  geom_col(aes(fill = name)) +
  theme(axis.text.x = element_blank()) +
  theme_minimal() +
  labs(title = "Insured employees", x = "Companies", y = "Count")

