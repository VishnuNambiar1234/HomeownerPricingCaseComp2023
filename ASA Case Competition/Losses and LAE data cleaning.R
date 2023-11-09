library(readxl)
library(tidyverse)
library(ggplot2)

Losses_LAE <- read_excel("C:/Users/Vishnu Nambiar/Desktop/Fall Case Comp 2023/06 - CAS Homeowners Pricing Case - Case Workbook.xlsx", 
                                                            sheet = "Losses and LAE", skip = 1)
Premiums <- read_excel("C:/Users/Vishnu Nambiar/Desktop/Fall Case Comp 2023/06 - CAS Homeowners Pricing Case - Case Workbook.xlsx", 
                                                            sheet = "Premiums", skip = 1)
Claim_Counts <- read_excel("C:/Users/Vishnu Nambiar/Desktop/Fall Case Comp 2023/06 - CAS Homeowners Pricing Case - Case Workbook.xlsx", 
                  sheet = "Claim Counts", skip = 1)
Expenses <- read_excel("C:/Users/Vishnu Nambiar/Desktop/Fall Case Comp 2023/06 - CAS Homeowners Pricing Case - Case Workbook.xlsx", 
                       sheet = "Company Expenses")

#Renaming Losses and LAE columns
colnames(Losses_LAE)[1] <- "Water_Sensor"
colnames(Losses_LAE)[2] <- "Alarm"
colnames(Losses_LAE)[3] <- "Roof_Type"

#Renaming Premiums columns
colnames(Premiums)[1] <- "Water_Sensor"
colnames(Premiums)[2] <- "Alarm"
colnames(Premiums)[3] <- "Roof_Type"

#Renaming Claim Counts columns
colnames(Claim_Counts)[1] <- "Water_Sensor"
colnames(Claim_Counts)[2] <- "Alarm"
colnames(Claim_Counts)[3] <- "Roof_Type"


# Reshape the data into a long format
Losses_LAE_long <- Losses_LAE %>%
  gather(key = "Category", value = "Value", -Water_Sensor, -Alarm, -Roof_Type)
Premiums_long <- Premiums %>%
  gather(key = "Category", value = "Value", -Water_Sensor, -Alarm, -Roof_Type)
Claim_Counts_long <- Claim_Counts %>%
  gather(key = "Category", value = "Value", -Water_Sensor, -Alarm, -Roof_Type)

#This will show you each dollar amount with all their characteristics
view(Losses_LAE_long)
view(Premiums_long)
view(Claim_Counts_long)

Premium_Claim_Ratio <- (Premiums_long$Value - Losses_LAE_long$Value)/Claim_Counts_long$Value
view(Premium_Claim_Ratio)

Net_Profit <- sum(Premiums_long$Value - Losses_LAE_long$Value) - sum(Expenses$`Yearly Expense`)
print(Net_Profit)

print(Net_Profit - (sum(Premiums_long$Value) - sum(NewPremiums$Value)))

Loss_Ratio <- (sum(Expenses$`Yearly Expense`) + sum(Losses_LAE_long$Value)) / sum(Premiums_long$Value)
print(Loss_Ratio)

#Obtain interesting graphs from here on out

#Loss Claim Ratio for Water Sensor
#1

lossClaimRatio <- Losses_LAE_long$Value/Claim_Counts_long$Value
view(lossClaimRatio)
LCRBase <- Losses_LAE_long

Loss_Profit_Ratio <- (Losses_LAE_long$Value) / (Premiums_long$Value)

LCRBase$Value <- Loss_Profit_Ratio
view(LCRBase)
YesWS_LCR <- LCRBase[LCRBase$Water_Sensor == 'Y', ]
NoWS_LCR <- LCRBase[LCRBase$Water_Sensor == 'N', ]
WS_LCR <- YesWS_LCR
YesNoWS <- YesWS_LCR$Value / NoWS_LCR$Value
WS_LCR$Value <- YesNoWS
WS_LCR$Water_Sensor <- NULL
view(WS_LCR)

#Graph Prep

LCRBase <- Losses_LAE_long

Loss_Profit_Ratio <- (Losses_LAE_long$Value) / (Premiums_long$Value)

LCRBase$Value <- Loss_Profit_Ratio
view(LCRBase)

#Loss Profit Ratio for Water Sensor

WaterSensorY_LCR <- LCRBase[LCRBase$Water_Sensor == 'Y', ]
WaterSensorN_LCR <- LCRBase[LCRBase$Water_Sensor == 'N', ]

WaterSensorYvN_LCR <- WaterSensorY_LCR

WaterSensorYvN <- WaterSensorY_LCR$Value / WaterSensorN_LCR$Value

WaterSensorYvN_LCR$Value <- WaterSensorYvN

WaterSensorYvN_LCR$Water_Sensor <- NULL

view(WaterSensorYvN_LCR)

WaterSensorN_Discount = 1
WaterSensorY_Discount = mean(WaterSensorYvN_LCR$Value)

print(WaterSensorN_Discount)
print(WaterSensorY_Discount)

Category = c('Wind/Hail', 'Wind/Hail', 
             'Water', 'Water',
             'Fire', 'Fire',
             'Theft', 'Theft',
             'Other', 'Other',
             'Liability', 'Liability',
             'Total', 'Total')
Water_Sensor = c('Water Sensor', 'No Water Sensor',
              'Water Sensor', 'No Water Sensor',
              'Water Sensor', 'No Water Sensor',
              'Water Sensor', 'No Water Sensor',
              'Water Sensor', 'No Water Sensor',
              'Water Sensor', 'No Water Sensor',
              'Water Sensor', 'No Water Sensor')
Discount = c(mean(WaterSensorYvN_LCR$Value[WaterSensorYvN_LCR$Category == 'Wind/Hail']), 1, 
             mean(WaterSensorYvN_LCR$Value[WaterSensorYvN_LCR$Category == 'Water']), 1,
             mean(WaterSensorYvN_LCR$Value[WaterSensorYvN_LCR$Category == 'Fire']), 1,
             mean(WaterSensorYvN_LCR$Value[WaterSensorYvN_LCR$Category == 'Theft']), 1,
             mean(WaterSensorYvN_LCR$Value[WaterSensorYvN_LCR$Category == 'Other']), 1,
             mean(WaterSensorYvN_LCR$Value[WaterSensorYvN_LCR$Category == 'Liability']), 1, 
             mean(WaterSensorYvN_LCR$Value), 1)

WaterSensorDiscountdf <- data.frame(Category, Water_Sensor, Discount)

view(WaterSensorDiscountdf)

# Create a grouped bar chart
WaterSensorDiscountPlot <- ggplot(WaterSensorDiscountdf, aes(x = Category, y = Discount, fill = factor(Water_Sensor))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d(option = "D") + # Use a viridis color palette
  labs(title = "Comparison of Water Sensor Availability per Category (None as Denominator)",
       x = "Category",
       y = "Loss/Profit Ratio of Water Sensor Type vs No Water Sensor",
       fill = "Water Sensor Availability") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 16, face = "bold"), 
        axis.title = element_text(size = 14), 
        legend.title = element_text(size = 12))

# Print the plot
print(WaterSensorDiscountPlot)

#Loss Claim Ratio for Roof Type

RoofType1_LCR <- LCRBase[LCRBase$Roof_Type == 1, ]
RoofType2_LCR <- LCRBase[LCRBase$Roof_Type == 2, ]
RoofType3_LCR <- LCRBase[LCRBase$Roof_Type == 3, ]

RoofType2v1_LCR <- RoofType1_LCR
RoofType3v1_LCR <- RoofType1_LCR

RoofType2v1 <- RoofType2_LCR$Value / RoofType1_LCR$Value
RoofType3v1 <- RoofType3_LCR$Value / RoofType1_LCR$Value

RoofType2v1_LCR$Value <- RoofType2v1
RoofType3v1_LCR$Value <- RoofType3v1

RoofType2v1_LCR$Roof_Type <- NULL
RoofType3v1_LCR$Roof_Type <- NULL

view(RoofType2v1_LCR)
view(RoofType3v1_LCR)

RoofType1_Discount = 1
RoofType2_Discount = mean(RoofType2v1_LCR$Value)
RoofType3_Discount = mean(RoofType3v1_LCR$Value)

print(RoofType1_Discount)
print(RoofType2_Discount)
print(RoofType3_Discount)

Category = c('Wind/Hail', 'Wind/Hail', 'Wind/Hail', 
             'Water', 'Water', 'Water',
             'Fire', 'Fire', 'Fire',
             'Theft', 'Theft', 'Theft',
             'Other', 'Other', 'Other',
             'Liability', 'Liability', 'Liability',
             'Total', 'Total', 'Total')
Roof_Type = c('Composite Shingle', 'Hail Resistant Shingle', 'Solar Panel over Shingle',
          'Composite Shingle', 'Hail Resistant Shingle', 'Solar Panel over Shingle',
          'Composite Shingle', 'Hail Resistant Shingle', 'Solar Panel over Shingle',
          'Composite Shingle', 'Hail Resistant Shingle', 'Solar Panel over Shingle',
          'Composite Shingle', 'Hail Resistant Shingle', 'Solar Panel over Shingle',
          'Composite Shingle', 'Hail Resistant Shingle', 'Solar Panel over Shingle',
          'Composite Shingle', 'Hail Resistant Shingle', 'Solar Panel over Shingle')
Discount = c(1, mean(RoofType2v1_LCR$Value[RoofType2v1_LCR$Category == 'Wind/Hail']), mean(RoofType3v1_LCR$Value[RoofType3v1_LCR$Category == 'Wind/Hail']), 
             1, mean(RoofType2v1_LCR$Value[RoofType2v1_LCR$Category == 'Water']), mean(RoofType3v1_LCR$Value[RoofType3v1_LCR$Category == 'Water']), 
             1, mean(RoofType2v1_LCR$Value[RoofType2v1_LCR$Category == 'Fire']), mean(RoofType3v1_LCR$Value[RoofType3v1_LCR$Category == 'Fire']), 
             1, mean(RoofType2v1_LCR$Value[RoofType2v1_LCR$Category == 'Theft']), mean(RoofType3v1_LCR$Value[RoofType3v1_LCR$Category == 'Theft']),  
             1, mean(RoofType2v1_LCR$Value[RoofType2v1_LCR$Category == 'Other']), mean(RoofType3v1_LCR$Value[RoofType3v1_LCR$Category == 'Other']), 
             1, mean(RoofType2v1_LCR$Value[RoofType2v1_LCR$Category == 'Liability']), mean(RoofType3v1_LCR$Value[RoofType3v1_LCR$Category == 'Liability']), 
             1, mean(RoofType2v1_LCR$Value), mean(RoofType3v1_LCR$Value))

RoofDiscountdf <- data.frame(Category, Roof_Type, Discount)

view(RoofDiscountdf)

# Create a grouped bar chart
RoofDiscountPlot <- ggplot(RoofDiscountdf, aes(x = Category, y = Discount, fill = factor(Roof_Type))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d(option = "D") + # Use a viridis color palette
  labs(title = "Comparison of Roof Types per Category (Composite as Denominator)",
       x = "Category",
       y = "Loss/Profit Ratio of Roof Type vs Composite Roof",
       fill = "Roof Type") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 16, face = "bold"), 
        axis.title = element_text(size = 14), 
        legend.title = element_text(size = 12))

# Print the plot
print(RoofDiscountPlot)


#Loss Claim Ratio for Alarm

Alarm1_LCR <- LCRBase[LCRBase$Alarm == 1, ]
Alarm2_LCR <- LCRBase[LCRBase$Alarm == 2, ]
Alarm3_LCR <- LCRBase[LCRBase$Alarm == 3, ]
Alarm4_LCR <- LCRBase[LCRBase$Alarm == 4, ]

Alarm2v1_LCR <- Alarm1_LCR
Alarm3v1_LCR <- Alarm1_LCR
Alarm4v1_LCR <- Alarm1_LCR

Alarm2v1 <- Alarm2_LCR$Value / Alarm1_LCR$Value
Alarm3v1 <- Alarm3_LCR$Value / Alarm1_LCR$Value
Alarm4v1 <- Alarm4_LCR$Value / Alarm1_LCR$Value

Alarm2v1_LCR$Value <- Alarm2v1
Alarm3v1_LCR$Value <- Alarm3v1
Alarm4v1_LCR$Value <- Alarm4v1

Alarm2v1_LCR$Alarm <- NULL
Alarm3v1_LCR$Alarm <- NULL
Alarm4v1_LCR$Alarm <- NULL

view(Alarm2v1_LCR)
view(Alarm3v1_LCR)
view(Alarm4v1_LCR)

Alarm1_Discount = 1
Alarm2_Discount = mean(Alarm2v1_LCR$Value)
Alarm3_Discount = mean(Alarm3v1_LCR$Value)
Alarm4_Discount = mean(Alarm4v1_LCR$Value)



print(Alarm1_Discount)
print(Alarm2_Discount)
print(Alarm3_Discount)
print(Alarm4_Discount)

Category = c('Wind/Hail', 'Wind/Hail', 'Wind/Hail', 'Wind/Hail', 
             'Water', 'Water', 'Water', 'Water', 
             'Fire', 'Fire', 'Fire', 'Fire', 
             'Theft', 'Theft', 'Theft', 'Theft', 
             'Other', 'Other', 'Other', 'Other', 
             'Liability', 'Liability', 'Liability', 'Liability', 
             'Total', 'Total', 'Total', 'Total')
Alarm = c('None', 'Local Alarm', 'High Tech Local Alarm', 'Monitored High Tech Alarm', 
          'None', 'Local Alarm', 'High Tech Local Alarm', 'Monitored High Tech Alarm', 
          'None', 'Local Alarm', 'High Tech Local Alarm', 'Monitored High Tech Alarm', 
          'None', 'Local Alarm', 'High Tech Local Alarm', 'Monitored High Tech Alarm', 
          'None', 'Local Alarm', 'High Tech Local Alarm', 'Monitored High Tech Alarm', 
          'None', 'Local Alarm', 'High Tech Local Alarm', 'Monitored High Tech Alarm', 
          'None', 'Local Alarm', 'High Tech Local Alarm', 'Monitored High Tech Alarm')
Discount = c(1, mean(Alarm2v1_LCR$Value[Alarm2v1_LCR$Category == 'Wind/Hail']), mean(Alarm3v1_LCR$Value[Alarm3v1_LCR$Category == 'Wind/Hail']), mean(Alarm4v1_LCR$Value[Alarm4v1_LCR$Category == 'Wind/Hail']),
             1, mean(Alarm2v1_LCR$Value[Alarm2v1_LCR$Category == 'Water']), mean(Alarm3v1_LCR$Value[Alarm3v1_LCR$Category == 'Water']), mean(Alarm4v1_LCR$Value[Alarm4v1_LCR$Category == 'Water']), 
             1, mean(Alarm2v1_LCR$Value[Alarm2v1_LCR$Category == 'Fire']), mean(Alarm3v1_LCR$Value[Alarm3v1_LCR$Category == 'Fire']), mean(Alarm4v1_LCR$Value[Alarm4v1_LCR$Category == 'Fire']), 
             1, mean(Alarm2v1_LCR$Value[Alarm2v1_LCR$Category == 'Theft']), mean(Alarm3v1_LCR$Value[Alarm3v1_LCR$Category == 'Theft']), mean(Alarm4v1_LCR$Value[Alarm4v1_LCR$Category == 'Theft']), 
             1, mean(Alarm2v1_LCR$Value[Alarm2v1_LCR$Category == 'Other']), mean(Alarm3v1_LCR$Value[Alarm3v1_LCR$Category == 'Other']), mean(Alarm4v1_LCR$Value[Alarm4v1_LCR$Category == 'Other']), 
             1, mean(Alarm2v1_LCR$Value[Alarm2v1_LCR$Category == 'Liability']), mean(Alarm3v1_LCR$Value[Alarm3v1_LCR$Category == 'Liability']), mean(Alarm4v1_LCR$Value[Alarm4v1_LCR$Category == 'Liability']),
             1, mean(Alarm2v1_LCR$Value), mean(Alarm3v1_LCR$Value), mean(Alarm4v1_LCR$Value))

AlarmDiscountdf <- data.frame(Category, Alarm, Discount)

view(AlarmDiscountdf)

# Create a grouped bar chart
AlarmDiscountPlot <- ggplot(AlarmDiscountdf, aes(x = Category, y = Discount, fill = factor(Alarm))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d(option = "D") + # Use a viridis color palette
  labs(title = "Comparison of Alarm Types per Category (None as Denominator)",
       x = "Category",
       y = "Loss/Profit Ratio of Alarms Type vs No Alarm",
       fill = "Alarm Type") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 16, face = "bold"), 
        axis.title = element_text(size = 14), 
        legend.title = element_text(size = 12))

# Print the plot
print(AlarmDiscountPlot)

#Official Discounts based on Data!

#Alarm Discounts
##Alarm 2
Alarm2Discount <- mean(mean(Alarm2v1_LCR$Value[Alarm2v1_LCR$Category == 'Fire']), mean(Alarm2v1_LCR$Value[Alarm2v1_LCR$Category == 'Water']), mean(Alarm2v1_LCR$Value[Alarm2v1_LCR$Category == 'Theft']))
##Alarm 3
Alarm3Discount <- mean(mean(Alarm3v1_LCR$Value[Alarm3v1_LCR$Category == 'Fire']), mean(Alarm3v1_LCR$Value[Alarm3v1_LCR$Category == 'Water']), mean(Alarm3v1_LCR$Value[Alarm3v1_LCR$Category == 'Theft']))
##Alarm 4
Alarm4Discount <- mean(mean(Alarm4v1_LCR$Value[Alarm4v1_LCR$Category == 'Fire']), mean(Alarm4v1_LCR$Value[Alarm4v1_LCR$Category == 'Water']), mean(Alarm4v1_LCR$Value[Alarm4v1_LCR$Category == 'Theft']))

AlarmLS <- sum(Premiums_long$Value[Premiums_long$Category == 'Fire' | Premiums_long$Category == 'Water' | Premiums_long$Category == 'Theft']) / 
  sum(Losses_LAE_long$Value[Losses_LAE_long$Category == 'Fire' | Losses_LAE_long$Category == 'Water' | Losses_LAE_long$Category == 'Theft'])
print(AlarmLS)

#Roof Discounts
##Roof 2
Roof2Discount <- mean(RoofType2v1_LCR$Value[RoofType2v1_LCR$Category == 'Wind/Hail'])
##Roof 3
Roof3Discount <- mean(RoofType3v1_LCR$Value[RoofType3v1_LCR$Category == 'Wind/Hail'])

print(sum(Premiums_long$Value)/200000)

NewPremiums <- Premiums_long

NewPremiums$Value[NewPremiums$Alarm == 2 & (NewPremiums$Category == 'Fire' | NewPremiums$Category == 'Water' | NewPremiums$Category == 'Theft')] = 
  NewPremiums$Value[NewPremiums$Alarm == 2 & (NewPremiums$Category == 'Fire' | NewPremiums$Category == 'Water' | NewPremiums$Category == 'Theft')] * Alarm2Discount
NewPremiums$Value[NewPremiums$Alarm == 3 & (NewPremiums$Category == 'Fire' | NewPremiums$Category == 'Water' | NewPremiums$Category == 'Theft')] = 
  NewPremiums$Value[NewPremiums$Alarm == 3 & (NewPremiums$Category == 'Fire' | NewPremiums$Category == 'Water' | NewPremiums$Category == 'Theft')] * Alarm3Discount
NewPremiums$Value[NewPremiums$Alarm == 4 & (NewPremiums$Category == 'Fire' | NewPremiums$Category == 'Water' | NewPremiums$Category == 'Theft')] = 
  NewPremiums$Value[NewPremiums$Alarm == 4 & (NewPremiums$Category == 'Fire' | NewPremiums$Category == 'Water' | NewPremiums$Category == 'Theft')] * Alarm4Discount

NewPremiums$Value[NewPremiums$Roof_Type == 2 & (NewPremiums$Category == 'Wind/Hail')] = 
  NewPremiums$Value[NewPremiums$Roof_Type == 2 & (NewPremiums$Category == 'Wind/Hail')] * Roof2Discount
NewPremiums$Value[NewPremiums$Roof_Type == 3 & (NewPremiums$Category == 'Wind/Hail')] = 
  NewPremiums$Value[NewPremiums$Roof_Type == 3 & (NewPremiums$Category == 'Wind/Hail')] * Roof3Discount

NewPremiums$Value[NewPremiums$Water_Sensor == 'Y' & (NewPremiums$Category == 'Water')] = 
  NewPremiums$Value[NewPremiums$Water_Sensor == 'Y' & (NewPremiums$Category == 'Water')] * WaterDiscount

print(sum(Premiums_long$Value) - sum(NewPremiums$Value))
print()



#Water Sensor Discounts
##Yes
WaterDiscount <- mean(WaterSensorYvN_LCR$Value[WaterSensorYvN_LCR$Category == 'Water'])

#Premium Claim Ratio for Water Sensor
#2

premiumClaimRatio <-Premiums_long$Value/Claim_Counts_long$Value
view(premiumClaimRatio)
PCRBase <- Premiums_long
PCRBase$Value <- premiumClaimRatio
view(PCRBase)
YesWS_PCR <- PCRBase[PCRBase$Water_Sensor == 'Y', ]
NoWS_PCR <- PCRBase[PCRBase$Water_Sensor == 'N', ]
WS_PCR <- YesWS_PCR
YesNoWS_P <- YesWS_LCR$Value / NoWS_LCR$Value
WS_PCR$Value <- YesNoWS_P
WS_PCR$Water_Sensor <- NULL
view(WS_PCR)

#1 graphs

ggplot(WS_LCR, aes(x = Alarm, y = Value/18)) +
  geom_bar(stat = "identity", width = 0.6, fill = "skyblue") +
  labs(x = "Alarm Level", y = "Percentage of Average Water Sensor Ratio (Y/N)", 
       title = "Distribution of Alarm Levels vs Water Sensor Ratio") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

ggplot(WS_LCR, aes(x = Roof_Type, y = Value/24)) +
  geom_bar(stat = "identity", width = 0.6, fill = "skyblue") +
  labs(x = "Roof Type", y = "Percentage of Average Water Sensor Ratio (Y/N)", 
       title = "Distribution of Roof Type vs Water Sensor Ratio") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

ggplot(WS_LCR, aes(x = Category, y = Value/12)) +
  geom_bar(stat = "identity", width = 0.6, fill = "skyblue") +
  labs(x = "Category", y = "Percentage of Average Water Sensor Ratio (Y/N)", 
       title = "Distribution of Category vs Water Sensor Ratio") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

#2 graphs

ggplot(WS_PCR, aes(x = Alarm, y = Value/18)) +
  geom_bar(stat = "identity", width = 0.6, fill = "skyblue") +
  labs(x = "Alarm Level", y = "Percentage of Average Water Sensor Ratio (Y/N)", 
       title = "Distribution of Alarm Levels vs Water Sensor Ratio") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

ggplot(WS_PCR, aes(x = Roof_Type, y = Value/24)) +
  geom_bar(stat = "identity", width = 0.6, fill = "skyblue") +
  labs(x = "Roof Type", y = "Percentage of Average Water Sensor Ratio (Y/N)", 
       title = "Distribution of Roof Type vs Water Sensor Ratio") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

ggplot(WS_PCR, aes(x = Category, y = Value/12)) +
  geom_bar(stat = "identity", width = 0.6, fill = "skyblue") +
  labs(x = "Category", y = "Percentage of Average Water Sensor Ratio (Y/N)", 
       title = "Distribution of Category vs Water Sensor Ratio") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

Y_WF_loss <- mean(LCRBase$Value[Losses_LAE_long$Water_Sensor == 'Y'])
print(Y_WF_loss)
N_WF_loss <- mean(LCRBase$Value[Losses_LAE_long$Water_Sensor == 'N'])
print(N_WF_loss)

WFLossRatio <- Y_WF_loss / N_WF_loss
print(WFLossRatio)

write.csv(Losses_LAE_long, "C:/Users/Vishnu Nambiar/Downloads/claimcountscsv/claimcsv.txt", row.names=TRUE)

#Linear Modeling Technique starts here

# Convert categorical variables to dummy variables
losses <- Losses_LAE_long
losses$Alarm <- as.factor(losses$Alarm)
losses$Roof_Type <- as.factor(losses$Roof_Type)
losses$Water_Sensor <- as.factor(losses$Water_Sensor)
losses$Category <- as.factor(losses$Category)

premiums <- Premiums_long
premiums$Alarm <- as.factor(premiums$Alarm)
premiums$Roof_Type <- as.factor(premiums$Roof_Type)
premiums$Water_Sensor <- as.factor(premiums$Water_Sensor)
premiums$Category <- as.factor(premiums$Category)

write.csv(AlarmDiscountdf, file = "C:/Users/Vishnu Nambiar/Desktop/alarmdiscountdf.csv", row.names = FALSE)

#Machine Learning Attempt:

data <- merge(losses, premiums, by = c("Water_Sensor", "Alarm", "Roof_Type", "Category"))

# Calculate loss ratios and differences
data$Loss_Ratio <- with(data, losses$Value / premium$Value)
data$Loss_Difference <- with(data, losses$Value - premium$Value)

# Convert categorical variables to factors
data$Water_Sensor <- as.factor(data$Water_Sensor)
data$Alarm <- as.factor(data$Alarm)
data$Roof_Type <- as.factor(data$Roof_Type)
data$Category <- as.factor(data$Category)

# Split the data
set.seed(123)  # for reproducibility
training_rows <- createDataPartition(data$Loss_Difference, p = 0.8, list = FALSE)
train_data <- data[training_rows, ]
test_data <- data[-training_rows, ]

# Train the model using XGBoost
xgb_data <- xgb.DMatrix(data = as.matrix(train_data[, -which(names(train_data) %in% c("Value_loss", "Value_premium", "Loss_Ratio", "Loss_Difference"))]),
                        label = train_data$Loss_Difference)
xgb_model <- xgb.train(data = xgb_data, 
                       nrounds = 100, 
                       objective = "reg:squarederror")

# Make predictions on the test set
test_data_matrix <- xgb.DMatrix(data = as.matrix(test_data[, -which(names(test_data) %in% c("Value_loss", "Value_premium", "Loss_Ratio", "Loss_Difference"))]))
predictions <- predict(xgb_model, test_data_matrix)

# Evaluate the model
test_data$Predicted_Loss_Difference <- predictions
RMSE <- sqrt(mean((test_data$Loss_Difference - test_data$Predicted_Loss_Difference)^2))
print(RMSE)





