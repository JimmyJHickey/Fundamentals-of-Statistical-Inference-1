# Jimmy code
practice <- read.csv("TestData.csv", header = TRUE)

library(dplyr)

names(practice)[1] = "IDvar"

output = practice %>%
  group_by(IDvar) %>%
  summarize(count = n(), test1_total = sum(Test1), test2_total = sum(Test2), )

write.csv(output, "summarized_data.csv", row.names = FALSE)
