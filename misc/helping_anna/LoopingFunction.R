# Jimmy code
practice <- read.csv("TestData.csv", header = TRUE)

# helpful library
library(dplyr)

# first column had weird symbols in name
names(practice)[1] = "IDvar"

# group practice by column "IDvar",
# then the summarize command will work across groups
# n() gives count of each group
# sum(ColName) gives the sum of the elements in that column for that group
output = practice %>%
  group_by(IDvar) %>%
  summarize(count = n(), test1_total = sum(Test1), test2_total = sum(Test2), )

# row.names = FALSE gets rid of extra column
write.csv(output, "summarized_data.csv", row.names = FALSE)
