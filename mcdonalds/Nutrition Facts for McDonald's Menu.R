#package required:

library(regclass)
library(tidyverse)
library(ggplot2)
library(heatmaply)


#read McDonald's menu dataset

menu <- read.csv("menu.csv",sep = ",",colClasses = c("character", "character","character", rep("numeric", 21)))


#seperate menu to food menu and drink menu
#drinkmenu<-menu[c(111:260),]
foodmenu<-menu[c(1:110),]


#Step 2 Data cleaning
#seperate the variable Serving Size into two,one is the number with oz unit,another one is the number with g unit
#Created three new numerical variables and added into the dataset (Cal_per_oz, Fat_per_oz, Cal_from_Fat_per_oz).
foodmenu<-foodmenu %>%
  separate("Serving.Size",c("Serving size in oz",'del1','Serving size in g','del2'), sep =" .")%>%
  transform(`Serving size in oz` = as.numeric(`Serving size in oz`), `Serving size in g` = as.numeric(`Serving size in g`)) %>%
  mutate(Cal_per_oz = Calories/Serving.size.in.oz,Cal_from_Fat_per_oz = Calories.from.Fat /Serving.size.in.oz,Fat_per_oz = Total.Fat/ Serving.size.in.oz) %>% 
  mutate(across(where(is.numeric), round, 2)) 

#delete the columns that I don't need
foodmenu<-select(foodmenu, -del1,-del2)


#drinkmenu<-drinkmenu %>%
#  separate("Serving.Size",c("Serving_size_in_fl_oz","del3","del4","del5"), sep =" .")%>%
#  mutate (Serving_size_in_fl_oz = as.numeric(Serving_size_in_fl_oz))%>%
#  mutate(Cal_per_fl_oz = Calories/Serving_size_in_fl_oz,Cal_from_Fat_per_fl_oz = Calories.from.Fat /Serving_size_in_fl_oz,Fat_per_fl_oz = Total.Fat/ Serving_size_in_fl_oz) %>% 
#  mutate(across(where(is.numeric), round, 2))       
#delete the columns that I don't need         
#drinkmenu<-select(drinkmenu, -del3,-del4,-del5)

#manually change the column with 1 carton to  8 fl oz
#drinkmenu[21:22,3]<-8.0

#have a look at data structure
str(foodmenu)
summary(foodmenu)

#have a look if there is missing value in dataset
sum(is.na(foodmenu))
#sum(is.na(drinkmenu))


#dataset only has nutrition variables    
allfood <- foodmenu %>% select(c("Category", "Item","Serving.size.in.oz","Calories","Calories.from.Fat","Total.Fat","Saturated.Fat","Trans.Fat","Cholesterol","Sodium","Carbohydrates","Dietary.Fiber","Sugars","Protein","Fat_per_oz","Cal_from_Fat_per_oz" ))
head(allfood)



#check if variables are numeric
sapply(allfood, is.numeric)
#check correlations with heat map
selected <- allfood[, c(3:16)]
heatmaply_cor(x = cor(selected),
              xlab = "Features",
              ylab = "Features",
              k_col = 2,
              k_row = 2)


all_correlations(allfood,interest="Total.Fat",sorted="magnitude")


#I decided to use calories as y and choose 4 variables that has strongest correlation with my y 
allfood <- foodmenu %>% select(c("Category", "Item","Calories","Total.Fat","Sodium","Carbohydrates","Protein" ))

head(allfood)



#Step 3



#associate
associate(Calories~Total.Fat,data=allfood)
associate(Calories~Sodium,data=allfood)
associate(Calories~Carbohydrates,data=allfood)
associate(Calories~Protein,data=allfood)
associate(Calories~Category,data=allfood)



#Step 4 regression
#total fat
F <- lm(Calories~Total.Fat,data=allfood)
plot(Calories~Total.Fat,data=allfood)
abline(F)
anova(F)
summary(F)
confint(F,level=0.95)
possible_regressions(F)


#sodium
S <- lm(Calories~Sodium,data=allfood)
plot(Calories~Sodium,data=allfood)
abline(S)
possible_regressions(S)
summary(S)
anova(S)
confint(S,level=0.95)

#Carbohydrates
Carb <- lm(Calories~Carbohydrates,data=allfood)
plot(Calories~Carbohydrates,data=allfood)
abline(Carb)
possible_regressions(Carb)
summary(Carb)
anova(Carb)
confint(Carb,level=0.95)

#Protein
P <- lm(Calories~Protein,data=allfood)
possible_regressions(P)
summary(P)
anova(P)
confint(P,level=0.95)






#Others
#box plot
ggplot(allfood, aes(x = Category, y = Calories, fill = Category)) + 
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point",shape = 20,size =5, color = "blue")

#The distribution of each Category is fairly same, and there are some outliers in each categories

# to show top 10 products with the most caloreies
calories <- head(arrange(menu, desc(Calories)),10)
ggplot(calories, aes(x=reorder(Item,Calories), y= Calories, fill= Category, label=Calories))+
  geom_bar(stat="identity")+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "top")+
  geom_text(hjust=1.3, colour = "blue")+
  ggtitle("Top 10 products with the most calories")