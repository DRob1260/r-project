library("dplyr")
library("zoo")
library("tidyverse")

moderna <- read.csv(file = 'moderna-distribution.csv')
moderna$Jurisdiction <- as.factor(moderna$Jurisdiction)
moderna$Week.of.Allocations <- as.Date(moderna$Week.of.Allocations, "%m/%d/%Y")

census <- read.csv(file = 'census-by-state-2019.csv')
popByState <- census %>% select(NAME, POPESTIMATE2019)

modernaWithPop <- left_join(moderna, popByState, by = c("Jurisdiction" = "NAME"))

modernaWithPop <- modernaWithPop %>%
  arrange(Week.of.Allocations) %>%
  na.omit %>%
  mutate(percPopAlloc = ((X1st.Dose.Allocations / POPESTIMATE2019) * 100.0)) %>%
  group_by(Jurisdiction) %>%
  mutate(grandTotalAlloc = cumsum(percPopAlloc))

ggplot(modernaWithPop, aes(modernaWithPop$Week.of.Allocations, modernaWithPop$grandTotalAlloc, color = Jurisdiction)) + geom_point()
