# instal packages for cleaning  and visualizing data

library(janitor)
library(dplyr)
library(reshape2)

food_production <- read.csv('/Users/janetromero/Documents/DA-Projects/Data-Sets/Portfolio-Data/Food_Production.csv')

# Checking the data
View(food_production)

# Check for null and missing values
is.null(food_production)

is.na(food_production)
colSums(is.na(food_production))

#Replace NA values with 0
food_production[is.na(food_production)] <- 0

#Create a copy of DF
food_production_2 <- food_production

#  Create a subset to start analysis 
(food_prodsub <- food_production_2[, c(1:9,12,14,17,20,22)])
food_prodsub
str(food_prodsub)
summary(food_prodsub)

library(psych) # Statistics like: standar deviation
#mad(mean absolute deviation), skew (measure weather the
#data distribution is simetrical)
describe(food_prodsub)

# Let's plot some numbers
# Food product with highest total emmision

library(ggplot2)
plot1 <- food_prodsub %>%
  ggplot(aes(x = Food.product, y = Total_emissions)) +
  geom_point(aes(color = "Red")) +
  guides(size = FALSE) +
  labs(color = "Emissions", title = "Emissions by Food Product",
       x = "Product Name",
       y = "Total Emissions") +
  theme_classic()

plot1 + guides(color = FALSE, size = FALSE)  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
 
# Greenhouse gases per 100gr of food product
Plot2 <- food_prodsub %>%
  ggplot(aes( x = Food.product)) +
  geom_point(aes(y = Greenhouse.gas.emissions.per.100g.protein..kgCO.eq.per.100g.protein.,
                 color = "Emission")) +
  geom_line(aes(y = Greenhouse.gas.emissions.per.100g.protein..kgCO.eq.per.100g.protein., 
                color = "Emission")) +
    theme_minimal()+
  ggtitle("Greenhouses gases per 100 gr of Food Product") +
  labs(x = "Product Name", y = "tCO2")
  

Plot2 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5,
                                         hjust = 1)) 
Plot2v <- food_prodsub %>%
  ggplot(aes(x = Food.product)) +
  geom_point(aes(y = Greenhouse.gas.emissions.per.100g.protein..kgCO.eq.per.100g.protein.,
                 color = "Emission tCO2")) 
  
Plot2v + geom_line(aes(y = Greenhouse.gas.emissions.per.100g.protein..kgCO.eq.per.100g.protein.,
                       color = "Emission tCO2")) 
  
  
plotv2 <- food_prodsub %>%
  ggplot(aes(x = Food.product, y = Greenhouse.gas.emissions.per.100g.protein..kgCO.eq.per.100g.protein., group = 1)) +
  geom_line(color = "gray", linetype = "dashed") +
  geom_point()

plotv2 + theme_minimal()+ theme_classic() +
  ggtitle("Greenhouses Gases per 100 gr of Food Product") +
  labs(x = "Product Name", y = "tCO2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))  
  

# Water used per 100gr of protein  
Plot3 <- food_prodsub %>%
  ggplot(aes(x = Food.product, y = Freshwater.withdrawals.per.100g.protein..liters.per.100g.protein., group = 1)) +
  geom_line(color = "gray", linetype = "dashed") +
  geom_point()

Plot3 + theme_minimal() + theme_classic() +
  ggtitle("Water Used per 100gr Protein Produce") +
  labs(x = "Product Name", y = "Water Used") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 


#Euthrophying Emissions per each 100gr of protein  
Plot4 <- food_prodsub %>%
  ggplot(aes(x = Food.product, y = Eutrophying.emissions.per.100g.protein..gPO.eq.per.100.grams.protein., group = 1)) +
  geom_line(color = "gray", linetype = "dashed") +
  geom_point()
  
Plot4 + theme_minimal() + theme_classic() +
  ggtitle("Euthrophying Emission per 100gr Protein") +
  labs(x = "Product Name", y = "Euthrophying Emssions") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 
  


