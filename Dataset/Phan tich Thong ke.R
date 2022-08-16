library(readxl)
library(dplyr)
library(janitor) # GROUP SUMMARY
library(purrr)
library(readr) # MA TRAN PEARSON
library(strucchange) # KIEM DINH CHOW


Data <- read_excel("Working/UEH/KhoaLuanUEH/Dataset/Report.xlsx", 
                     sheet = "Output")

ViewData<-select(Data,-1)
View(Data)

# THONG KE MO TA --------------------------
Data %>%
  select(`Nhom nganh`,SIZE,PROF,LIQD,UNIQ,TANG) %>%
  split(.$`Nhom nganh`) %>%
  map(summary)


Data %>%
  select(SIZE,PROF,LIQD,UNIQ,TANG,`GDP (%)`,COVID) %>%
  summary()

# PEARSON TABULATE CORRELATION
Corr <- cor(Data %>% 
              na.omit %>% 
              select(SIZE,PROF,LIQD,UNIQ,TANG,`GDP (%)`,COVID)) # lenh na.omit de loai bo dong na (missing value)

upper<-round(Corr,4)
upper[upper.tri(Corr)]<-""
upper<-as.data.frame(upper)

View(upper)


#MODEL: Short Term STLEV



# KIEM DINH CHOW TEST
sctest(Data$STLEV~Data$SIZE+Data$PROF+Data$LIQD+Data$UNIQ+Data$TANG, type = "Chow", point = 10)







