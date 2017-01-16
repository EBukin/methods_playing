###########################################################################
###########################################################################
# Panel data example ------------------------------------------------------

# Це приклад аналізу панельних даних
# Автор     Eduard Bukin
# Дані      Andrii Skrypnyk


# Підготовка середовища R -------------------------------------------------

# Список використаних пакетів
packs <- c("plyr", "dplyr", "tidyr", "readxl", "car", "plm", "systemfit")

# Пакети "plyr", "dplyr", "tidyr" необхідні для маніпуляцій з даними
# Пакет "readxl" необхідний для завантаження даних із фалів Excel
# Пакети "plm", "systemfit", необхідні для побудови панельних регресій
# Пакет "car" необхідний для оцінки результатів регресій

# Завантаження та вствновлення відсутніх пакетів
lapply(packs[!packs %in% installed.packages()[,1]], 
       install.packages,
       dependencies = TRUE)

# Розпаковка та встановлення необхідних пакетів у робочу сесію
lapply(packs, require, character.only = TRUE)


# Дані --------------------------------------------------------------------

# Завантаження даних та трансформація даних у необхідний формат
data <- 
  read_excel(path = "2016-12-27 Panel data/data/Panel_test_data.xlsx",  
             sheet = 1,
             col_names =  TRUE) %>% 
  tbl_df() %>% 
  gather(Year, Value, 3:length(.)) %>% 
  spread(Variable, Value)

# Побудова звичайної не панельнної регресії -------------------------------

# Модель
simple_ols <- lm(y ~ x1 + x2, data = data)

# Результати регресії
summary(simple_ols)

# Діагностика результатів регресії
# 1. Перевірка рівномірності розподілу залишків
# 2. Перевірка нормальності розподілу залишків
# 3. Розподіл масштабу по відношенню до передбачених величин
# 4. Розподідл залишків відносно впливових величин
# Для більшої інформації звертайтеся: 
#       http://data.library.virginia.edu/diagnostic-plots/
#       http://socserv.socsci.mcmaster.ca/jfox/Courses/Brazil-2009/index.html

# Побудова діагностичних графіків
par(mfrow=c(2,2))               # Побудова графіків поряд а не на різних сторінках
plot(simple_ols)                # Фактична побудова графіків
par(mfrow=c(1,1))               # Повернення відображення до 1 графіку на сторінку

# Висновки:
#     1. Побудована регресія має слабкі передбачувальні якості 
#           R^2 adjusted = 0.63% 
#           модельне p-value = 0.3021
#     2. Відповідно до діагностичних зображень, усі припуження є задоволеними. 
#     3. Зважаючи на те, що модель погано передбачує результуючу ознаку зважаючи
#           спираючись на незілежні змінні, необхідно побудувати іншу модель.


# Побудова пінельної регресії ---------------------------------------------

# Для того, щоб побудувати панельну регресію необхідно, першим чином,
#   перетворити існуючі дані на панельні.
p_data <- 
  pdata.frame(data, 
              index = c("Firm", "Year"), 
              drop.index = TRUE, 
              row.names = TRUE)

# Підчас перетворення даних на панельні, функція `pdata.frame` перетворила
#   змінні "Firm" та "Year" на індекс масиву даних.
head(data)
head(p_data)

# Check applied econometrics with R pages 93-96 (84->)

# CONTINUE HERE -----------------------------------------------------------





#################### 1.1.oneway only individual (no time dimension) effect ####################

#The specification for fixed effects estimations is 'within'
#Take the individual (not time) effect into account 
pls_fixed <- plm(y ~ x1 + x2, data = p_data, model = "within", effect="twoways")

# See the summary of the model
summary(pls_fixed)

# See the individual fixed effect of each firm 
fixef(pls_fixed, effect="individual", type = "level")
fixef(pls_fixed, effect="time", type = "level")



#################### 1.2.twoway: time + individual effect ####################

p_twoway <- plm(y ~ x1 + x2, data = p_data, model = "within", effect = "twoways")
summary(p_twoway)

# See the individual fixed effect of each firm 
fixef(p_twoway, effect="time", type = "dmean")

#Ask for the individual effects on the mean score

fixef(p_twoway, effect="individual", type = "dmean")


fixef(p_twoway)

#notice that the coefficients' estimates change
#The coefficients indicates how much Y changes in controlling for differences in time and between individuals




## Fixed effects model

pmodel4 <- plm(inf ~ cbilag1 + glag1 + tlag1 + flag1 + infalag1 + ulag1 + cwb, data=BankData, index=c("ctry", "year"), na.action=na.omit, model="within")
summary(pmodel4)

## F test for fixed effects

pFtest(pmodel4,olsmodel2)

## Random effects model 

pmodel5 <- plm(inf ~ cbilag1 + glag1 + tlag1 + flag1 + infalag1 + ulag1 + cwb, data=BankData, index=c("ctry", "year"), na.action=na.omit, model="random")

## Crash because of negative estimated variance of individual effects

## Use different method of calculating variance of random effects
## random.method options only work on balanced panels

pmodel5 <- plm(inf ~ cbilag1 + glag1 + tlag1 + flag1 + infalag1 + ulag1 + cwb, data=BankData, index=c("ctry", "year"), na.action=na.omit, model="random", random.method="walhus")
summary(pmodel5)

## Breusch-Pagan test 

plmtest(pmodel5, effect="individual", type="bp")

## Hausman test

phtest(pmodel4, pmodel5)


