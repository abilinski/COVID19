#***************************************** GRAPHS *****************************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
# By: Alyssa Bilinski                                                                      #
# Created: March 14, 2020                                                                  #
#******************************************************************************************#

#### SETUP ####

# libraries
library(tidyverse)
library(stringr)

# set working directory
setwd("~/Dropbox/COVID19/0 - Parameters")

#### DEMOGRAPHIC GRAPHS ####

df = read.csv("Demographic data 14_mar_2020.csv") %>% gather(var, value, LT18, X18to64, X65plus) %>%
  mutate(var2 = ifelse(var=="LT18", "<18", "19-64"),
         var2 = ifelse(var=="X65plus", "65+", var2),
         Location = str_wrap(Location, width = 20))

g = ggplot(df, aes(x = Location, y = value, fill = var2, group = var2)) + geom_bar(stat = "identity") + 
  theme_minimal() + scale_fill_discrete(name = "") + geom_text(aes(label = value),
                                                               position = position_stack(vjust = 0.5)) + 
  labs(y = "Percentage", title = "Age distribution by location") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#### OUTPUT GRAPHS ####
setwd("~/Dropbox/COVID19/2 - Figures")


pdf("dem.pdf",width=5, height=5)
g
dev.off()
