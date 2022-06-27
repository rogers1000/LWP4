
library(tidyverse)

lwp4_df<- read.csv(url("https://raw.githubusercontent.com/rogers1000/LWP4/main/shinydata_05_04_1.csv"))

lwp4_df2 <- lwp4_df %>%
  filter(season == 2022) %>%
  select(yearly_id,full_name,position,lw) %>%
  mutate(lw = round(lw,3)*100) %>%
  view()

write.csv(lwp4_df2,"C:\\Users\\zacro\\Downloads\\lwp4 shinyapp\\zr_rankings_manual.csv")  
