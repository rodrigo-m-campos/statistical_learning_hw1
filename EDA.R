#install.packages("Ecdat")
library(Ecdat)
library(ggplot2)
#data(DoctorContacts)
data = DoctorContacts

# Check for NAs
hist(rowMeans(is.na(data)))
barplot(colMeans(is.na(data)), las=2)

any(is.na(data))

# No NAs :)

summary(data)

# Have male as 0 and female as 1
data$sex = factor(data$sex, levels = c('male', 'female'), labels = c(0, 1))

levels(data$health)
# They are correctly ordered!

# Outliers
# Linear models
# Bootstrap?