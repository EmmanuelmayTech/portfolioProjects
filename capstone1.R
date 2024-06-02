#install.packages("Hmisc")
#install.packages("data.table")
#install.packages("plyr")
#install.packages("RColorBrewer")
#install.packages("gridExtra")
#install.packages("recommenderlab")
#install.packages("recosystem")
#install.packages("rmarkdown")
#install.packages("knit")

install.packages("ggplot2")
install.packages("tidyverse")
install.packages("caret")

library(tidyverse)
#library(Hmisc)
#library(data.table)
#library(plyr)
#library(pander)
#library(RColorBrewer)
library(ggplot2)
#library(gridExtra)
library(caret)
#library(recommenderlab)
#library(recosystem)
#library(rmarkdown)
#library(knit)
getwd()
setwd("C:/Users/HP/Documents")

data.load <- function(data.size){
  if(data.size == '10mn'){
    dl <- file.path(getwd(),'Movielens10Mn.zip')
    download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
    
    ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                     col.names = c("userId", "movieId", "rating", "timestamp"))
    
    movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
    colnames(movies) <- c("movieId", "title", "genres")
    movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                               title = as.character(title),
                                               genres = as.character(genres))
  }else if(data.size == '10k'){
    dl <- file.path(getwd(),'Movielens10K.zip')
    download.file("http://files.grouplens.org/datasets/movielens/ml-latest-small.zip", dl)
    
    ratings <- read.csv(unzip(dl, "ml-latest-small/ratings.csv"))
    
    movies <- read.csv(unzip(dl, "ml-latest-small/movies.csv"), stringsAsFactors = F)
  }else{
    print('Use input 10k for smaller dataset and 10mn for larger dataset')
    return()
  }
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Checking for R Version and using set.seed function appropriately
  if(as.numeric(R.Version()$major)==3 & as.numeric(R.Version()$minor) >5) set.seed(1, sample.kind="Rounding") else set.seed(1)
  
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
  training.data <- movielens[-test_index,]
  temp <- movielens[test_index,]
  
  # Make sure userId and movieId in validation set are also in training.data set
  validation <- temp %>%
    semi_join(training.data, by = "movieId") %>%
    semi_join(training.data, by = "userId")
  
  # Add rows removed from validation set back into training.data set
  removed <- anti_join(temp, validation)
  training.data <- rbind(training.data, removed)
  
  rm(dl, ratings, movies, test_index, temp, movielens, removed)
  
  return(list(training.data=training.data,validation=validation))
}

train.and.test = data.load('10k')
train.dtS = train.and.test$training.data
validationS = train.and.test$validation
rm(train.and.test)

save.image('Part1.RData')



setwd("C:/Users/HP/Documents")
load('Part1.RData')

#DATA AUDIT validationS
View(validationS)
ncol(validationS)#6
nrow(validationS)#9708
str(validationS)
head(validationS)
tail(validationS)

unique(sapply(validationS, class))
apply(validationS,2, function(x) sum(is.na(x))) #no missing data
summary(validationS)
min(validationS$userId) #1
max(validationS$userId) #610

#install.packages("tidyverse")
library(tidyverse)


#The bar graph illustrates the number of rating in thousand for each scale rating from 1 to 5 where 5, as opposed to 1, is the best rating possible. By far the most popular movie rating were `4` with 2614 ratings followed by `3` at 1927. The `1.5` and `0.5` were far less popular at around 161 and 149 thousand respectively.
Rating<- validationS %>% group_by(rating) %>% summarise(n=n())
Rating<-ggplot(Rating, aes(x=reorder(rating,-n), y=n)) +
  geom_bar(stat="identity", fill = "dark red")+
  xlab("Rating") +
  ylab("Frequency Rating")
Rating + ggtitle("Number Rating")+ geom_text(aes(label= n), hjust=0.5, size=3, vjust = -0.5)

##ins# The data present outliers shown by the boxplot in triangle shape. However, It may not be meaningful to be treated

gg.val.box<-ggplot(validationS, aes(y= rating, fill= rating)) +
  geom_boxplot(color="black", fill="dark red", alpha=0.2, notch = TRUE, outlier.colour = "blue", outlier.shape = 2)
gg.val.box #we are not gonna treat outliers as those doesn't represent a significative count

#count distinct
dplyr::n_distinct (validationS$userId)#594
dplyr::n_distinct(validationS$movieId)#3360


#___________________________________________________________________
#DATA AUDIT train.dtS
View(train.dtS)
ncol(train.dtS) #6
nrow(train.dtS) #91128
str(train.dtS)
head(train.dtS)
tail(train.dtS)

unique(sapply(train.dtS, class))
apply(train.dtS,2, function(x) sum(is.na(x))) #no missing data
summary(train.dtS)
min(train.dtS$userId)#1
max(train.dtS$userId)#610


Rating2<- train.dtS %>% group_by(rating) %>% summarise(n=n())
Rating2<-ggplot(Rating2, aes(x=reorder(rating,-n), y=n)) +
  geom_bar(stat="identity", fill = "dark blue")+
  xlab("Rating") +
  ylab("Frequency Rating")
Rating2 + ggtitle("Number Rating")+ geom_text(aes(label= n), hjust=0.5, size=3, vjust = -0.5)

gg.Train.box<-ggplot(train.dtS, aes(y= rating, fill= rating)) +
  geom_boxplot(color="black", fill="dark blue", alpha=0.2, notch = TRUE, outlier.colour = "blue", outlier.shape = 2)
gg.Train.box


#count distinct
dplyr::n_distinct (train.dtS$userId)
dplyr::n_distinct(train.dtS$movieId)



##______________ _______________________ _____________________
#Data Cleaning validationS
#install.packages ('lubridate')
library(lubridate)


#Timestamp - convert data into date format in a new column
validationS$timestamp.2 <- validationS$timestamp
validationS$timestamp.2 <- as_datetime(validationS$timestamp)
validationS$timestamp.2


validationS$Year<-year(validationS$timestamp.2)
validationS$month<-month(validationS$timestamp.2)
validationS$day<-day(validationS$timestamp.2)
validationS$wday<-wday(validationS$timestamp.2)
validationS$hour<-hour(validationS$timestamp.2)

#install.packages('data.table')
library(data.table)
library(tidyr)


# separate all genre into different columns
gen.df <- as.data.frame(validationS$genres, stringsAsFactors=F)
gen.df.g <- as.data.frame(tstrsplit(gen.df[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(gen.df.g) <- paste0('genre',c(1:7))
apply(gen.df.g,2, function(x) sum(is.na(x)))

Genre.List<- gather(gen.df.g)
Genre.List<- Genre.List[-1]
Genre.List<-unique(Genre.List)
Genre.List


list_g <- c("Comedy","Mystery","Adventure", "Action", "Drama", "Animation", "Crime", "Thriller", "Children", "Horror",
            "Documentary", "Sci-Fi", "Fantasy", "(no genres listed)", "Western", "Film-Noir", "Romance", "Musical", "War",
            "IMAX")

library(stringr)
validationS2<-validationS

validationS2$Mystery <- str_detect(validationS2$genres, "Mystery")
validationS2$Comedy <- str_detect(validationS2$genres, "Comedy")
validationS2$Adventure <- str_detect(validationS2$genres, "Adventure")
validationS2$Action <- str_detect(validationS2$genres, "Action")
validationS2$Drama <- str_detect(validationS2$genres, "Drama")
validationS2$Animation <- str_detect(validationS2$genres, "Animation")
validationS2$Crime <- str_detect(validationS2$genres, "Crime")
validationS2$Thriller <- str_detect(validationS2$genres, "Thriller")
validationS2$Children <- str_detect(validationS2$genres, "Children")
validationS2$Horror <- str_detect(validationS2$genres, "Horror")
validationS2$Documentary <- str_detect(validationS2$genres, "Documentary")
validationS2$Fantasy <- str_detect(validationS2$genres, "Fantasy")
validationS2$Western <- str_detect(validationS2$genres, "Western")
validationS2$Romance <- str_detect(validationS2$genres, "Romance")
validationS2$Musical <- str_detect(validationS2$genres, "Musical")
validationS2$War <- str_detect(validationS2$genres, "War")
validationS2$IMAX <- str_detect(validationS2$genres, "IMAX")
validationS2$Film.Noir <- str_detect(validationS2$genres, "Film-Noir")
validationS2$Sci.Fi <- str_detect(validationS2$genres, "Sci-Fi")
validationS2$NGL <- str_detect(validationS2$genres, "(no genres listed)")

#binary replacing. converting as numeric
cols <- sapply(validationS2, is.logical)
validationS2[,cols] <- lapply(validationS2[,cols], as.numeric)
View(validationS2)


##======================================##==========================================

#Timestamp - convert data into date format in a new column
train.dtS$timestamp.2 <- train.dtS$timestamp
train.dtS$timestamp.2 <- as_datetime(train.dtS$timestamp)
train.dtS$timestamp.2

train.dtS$Year<-year(train.dtS$timestamp.2)
train.dtS$month<-month(train.dtS$timestamp.2)
train.dtS$day<-day(train.dtS$timestamp.2)
train.dtS$wday<-wday(train.dtS$timestamp.2)
train.dtS$hour<-hour(train.dtS$timestamp.2)

# separate all genre into different columns
dtS.gen.df <- as.data.frame(train.dtS$genres, stringsAsFactors=F)
dtS.gen.df.g <- as.data.frame(tstrsplit(dtS.gen.df[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(dtS.gen.df.g) <- paste0('genre',c(1:7))
apply(dtS.gen.df.g,2, function(x) sum(is.na(x)))

dtS.Genre.List<- gather(dtS.gen.df.g)
dtS.Genre.List<- dtS.Genre.List[-1]
dtS.Genre.List<-unique(dtS.Genre.List)


list_g <- c("Comedy","Mystery","Adventure", "Action", "Drama", "Animation", "Crime", "Thriller", "Children", "Horror",
            "Documentary", "Sci-Fi", "Fantasy", "(no genres listed)", "Western", "Film-Noir", "Romance", "Musical", "War",
            "IMAX")

train.dtS2<-train.dtS

train.dtS2$Mystery <- str_detect(train.dtS2$genres, "Mystery")
train.dtS2$Comedy <- str_detect(train.dtS2$genres, "Comedy")
train.dtS2$Adventure <- str_detect(train.dtS2$genres, "Adventure")
train.dtS2$Action <- str_detect(train.dtS2$genres, "Action")
train.dtS2$Drama <- str_detect(train.dtS2$genres, "Drama")
train.dtS2$Animation <- str_detect(train.dtS2$genres, "Animation")
train.dtS2$Crime <- str_detect(train.dtS2$genres, "Crime")
train.dtS2$Thriller <- str_detect(train.dtS2$genres, "Thriller")
train.dtS2$Children <- str_detect(train.dtS2$genres, "Children")
train.dtS2$Horror <- str_detect(train.dtS2$genres, "Horror")
train.dtS2$Documentary <- str_detect(train.dtS2$genres, "Documentary")
train.dtS2$Fantasy <- str_detect(train.dtS2$genres, "Fantasy")
train.dtS2$Western <- str_detect(train.dtS2$genres, "Western")
train.dtS2$Romance <- str_detect(train.dtS2$genres, "Romance")
train.dtS2$Musical <- str_detect(train.dtS2$genres, "Musical")
train.dtS2$War <- str_detect(train.dtS2$genres, "War")
train.dtS2$IMAX <- str_detect(train.dtS2$genres, "IMAX")
train.dtS2$Film.Noir <- str_detect(train.dtS2$genres, "Film-Noir")
train.dtS2$Sci.Fi <- str_detect(train.dtS2$genres, "Sci-Fi")
train.dtS2$NGL <- str_detect(train.dtS2$genres, "(no genres listed)")

#binary replacing. converting as numeric
dtS.cols <- sapply(train.dtS2, is.logical)
train.dtS2[,dtS.cols] <- lapply(train.dtS2[,dtS.cols], as.numeric)

save.image('Part2.RData')

setwd("C:/Users/HP/Documents")
load('Part2.RData')
#Data Exploration
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)



#1. Bar chart - x axis is userId & y axis is count of ratings or observations

train.dtS %>% group_by(userId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  scale_x_log10() +
  ggtitle("Count of ratings per Users",
          subtitle = "The distribution is almost symetric.") +
  xlab("Number of Ratings") +
  ylab("Number of Users")


#temp.u <- train.dtS %>% group_by(userId) %>% summarize(count.of.obs = n())
#gg.No.userId<-ggplot(temp.u, aes(x=count.of.obs)) +
# geom_bar()+
# ylab("No of ratings for a User") +
# xlab("Count of User") +
# scale_x_continuous(limits = c(15,200))
#gg.No.userId


#Bar chart - x axis is userId & y axis is percentage of ratings or observations

temp.u <- train.dtS %>% group_by(userId) %>% summarize(count.of.obs = n())
temp.u2 <- temp.u %>% group_by(count.of.obs) %>% summarize(Count.of.Users = n())
names(temp.u2) <- c('Count.of.ratings.for.User', 'Count.of.Users')
temp.u2 <- temp.u2 %>% mutate(perc.of.obs = round(100*Count.of.Users/610,0))

gg.perc.userId<-ggplot(temp.u2, aes(x=Count.of.ratings.for.User, y=perc.of.obs)) +
  geom_bar(stat="identity", fill = "dark red")+
  xlab("Count of ratings for a User") +
  ylab("Perc. of Users")+
  scale_x_continuous(limits = c(15,75))
gg.perc.userId + ggtitle("Freq. Percentage of ratings for User")



#2. Bar chart - x axis is movieId & y axis is count of ratings or observations


temp.m <- train.dtS %>% group_by(movieId) %>% summarize(count.of.obs = n())
gg.No.movieId<-ggplot(temp.m, aes(x=count.of.obs)) +
  geom_bar()+
  ylab("Number ratings per Movie") +
  xlab("Count of movies") +
  scale_x_continuous(limits = c(5,100))
gg.No.movieId


#Bar chart - x axis is movieId & y axis is percentage of ratings or observations
temp.m2 <- temp.m %>% group_by(count.of.obs) %>% summarize(count.of.movies = n())
names(temp.m2) <- c('Count.of.ratings.for.the.movie', 'Count.of.movies')
temp.m2 <- temp.m2 %>% mutate(perc.of.obs = round(100*Count.of.movies/9724,0))

gg.perc.movieId<-ggplot(temp.m2, aes(x=Count.of.ratings.for.the.movie, y=perc.of.obs)) +
  geom_bar(stat="identity", fill = "blue")+
  ylab("Perc. of ratings for a Movie") +
  xlab("Perc. of ratings for a Movie") +
  scale_x_continuous(limits = c(0,13))
gg.perc.movieId + ggtitle("Freq. Percentage of ratings for a Movie")+
  geom_text(aes(label= perc.of.obs), hjust=0.5, size=3, vjust = -0.5)





#3. plot - x axis id Year & y axis is avg ratings
train.dtS2<- train.dtS
Year.rating<- train.dtS2 %>% group_by(Year) %>% summarise(n=n())
Year.rating<-ggplot(Year.rating, aes(x=reorder(Year, -n), y=n)) +
  geom_bar(stat="identity", fill = "dark red")+
  xlab("Year") +
  ylab("Frequency Rating")
Year.rating + ggtitle("Average Rating per year") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_text(aes(label= n), hjust=0, size=2.5, vjust = -0.5, angle = 30)


# plot - x axis id month & y axis is avg ratings
month.rating<- train.dtS2 %>% group_by(month) %>% summarise(n=n())
month.rating<-ggplot(month.rating, aes(x=reorder(month, -n), y=n)) +
  geom_bar(stat="identity", fill = "dark blue")+
  xlab("month") +
  ylab("Frequency Rating")
month.rating + ggtitle("Average Rating per month")+ coord_cartesian(ylim = c(5000, 10000))+
  geom_text(aes(label= n), hjust=0.5, size=3, vjust = -0.5)


# plot - x axis id day & y axis is avg ratings
day.rating<- train.dtS2 %>% group_by(day) %>% summarise(n=n())
day.rating<-ggplot(day.rating, aes(x=reorder(day, -n), y=n)) +
  geom_bar(stat="identity", fill = "dark green")+
  xlab("day") +
  ylab("Frequency Rating")
day.rating<- day.rating + ggtitle("Average Rating per day")
day.rating + coord_cartesian(ylim = c(1000, 4000)) + theme(axis.text.x = element_text(angle = 90))+
  geom_text(aes(label= n), hjust=0, size=2.5, vjust = -0.5, angle = 45)



# plot - x axis id day of the week & y axis is avg ratings
wday.rating<- train.dtS2 %>% group_by(wday) %>% summarise(n=n())
wday.rating<-ggplot(wday.rating, aes(x=reorder(wday, -n), y=n)) +
  geom_bar(stat="identity", fill = "dark red")+
  xlab("wday") +
  ylab("Frequency Rating")
wday.rating + ggtitle("Average Rating per wday") + coord_cartesian(ylim = c(7500, 18000))+
  geom_text(aes(label= n), hjust=0.5, size=3, vjust = -0.5)



# plot - x axis id hour & y axis is avg ratings
hour.rating<- train.dtS2 %>% group_by(hour) %>% summarise(n=n())
hour.rating<-ggplot(hour.rating, aes(x=reorder(hour, -n), y=n)) +
  geom_bar(stat="identity", fill = "dark red")+
  xlab("hour") +
  ylab("Frequency Rating")
hour.rating + ggtitle("Average Rating per hour")+
  coord_cartesian(ylim = c(2000, 6000))+ geom_text(aes(label= n), hjust=0, size=2.5, vjust = -0.5, angle = 60)




#4. Bar chart - x axis - Genres & y - axis is avg rating

Avg.Gen <- train.dtS2 %>% separate_rows(genres, sep = "\\|") %>% group_by(genres) %>%
  summarise(Avg.Rating = round(mean(rating), 2)) #%>% arrange(desc(number))

Gg.Avg.Gen <- ggplot(data= Avg.Gen, aes(x = reorder (genres, -Avg.Rating), y= Avg.Rating)) +
  geom_bar(stat="identity",fill = "dark green") + labs(x="Genres", y="Average Ratings")+
  ggtitle("Average Rating per Genre")
Gg.Avg.Gen + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + coord_cartesian(ylim = c(3, 4))+
  geom_text(aes(label= Avg.Rating), hjust=-0.5, size=3, vjust = -0.3, angle = 90)



#5. Bar chart - x axis - Genres & y - axis is count of ratings
N.Gen <- train.dtS2 %>% separate_rows(genres, sep = "\\|") %>% group_by(genres) %>%
  summarise(number = n()) %>% arrange(desc(number))

N.Gen <- ggplot(data= N.Gen, aes(x = reorder (genres, -number), y= number)) +
  geom_bar(stat="identity",fill = "dark red") +
  ggtitle("Number Rating per Genre")
N.Gen + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_text(aes(label= number), hjust=0.4, size=2.5, vjust = -0.5)


#6. List of top 10 & bottom 10 movies basis avg ratings & also show their count of ratings

rating.MId<- train.dtS%>% group_by(movieId) %>% summarize(No.Movies = n(), Avg.Rating= mean(rating))

rating.MId.top<- rating.MId %>% arrange(desc(No.Movies,Avg.Rating))
rating.MId.top<- rating.MId.top[1:10, ]

rating.MId.bottom <- rating.MId %>% arrange(No.Movies,Avg.Rating)
rating.MId.bottom<- rating.MId.bottom[1:10, ]

Gg.popular <- ggplot(rating.MId.top, aes(Avg.Rating, No.Movies))
Gg.popular + geom_point(aes(colour = factor(Avg.Rating), size = No.Movies))

Gg.rating <- ggplot(rating.MId, aes(Avg.Rating, No.Movies))
Gg.rating + xlim(3,5) +ylim(100, 250) + geom_point(aes(colour = "red"))


save.image('Part4.RData')



setwd("C:/Users/HP/Documents")
load('Part4.RData')


##______________________________________##______________________________##__________________________________________________________________________
##MODEL BUILDING

validationS3<- subset(validationS2, select= c(1:3))
train.dtS3 <- subset(train.dtS2, select= c(1:3))

# MODEL 1: Random ratings for predictions
set.seed(123, sample.kind = "Rounding")
validationS3$pred <- sample(x = seq(0.5,5,0.5),size = nrow(validationS3), replace = T, prob = rep(0.1, 10))
rmse <- sqrt(mean((validationS3$rating -validationS3$pred)^2)) #1.921271

RMSE.Result <- tibble(ModelType = "Random Predictions", RMSE = rmse, Notes = NA)




# MODEL 2: Random percentage prediction

No.obs <- data.frame(table(train.dtS$rating))
perc.obs <- data.frame(rating= No.obs$Var1, percentage = No.obs$Freq/sum(No.obs$Freq))
set.seed(123, sample.kind = "Rounding")
validationS3$pred2 <- sample(x = seq(0.5,5,0.5),size = nrow(validationS3), replace = T, prob = perc.obs$percentage)
rmse2 <- sqrt(mean((validationS3$rating -validationS3$pred2)^2)) #1.48289

RMSE.Result <- bind_rows(RMSE.Result, tibble(ModelType = "Random percentage prediction", RMSE = rmse2, Notes = NA))




#MODEL 3: Average Rating prediction
Avg.rating<- mean(train.dtS3$rating)
rmse3 <- sqrt(mean((validationS3$rating - Avg.rating)^2))# 1.054978

RMSE.Result <- bind_rows(RMSE.Result, tibble(ModelType = "Average prediction", RMSE = rmse3, Notes = NA))

save.image('Part5.RData')



# MODEL 4: Linear model with Movie Effect

setwd("C:/Users/HP/Documents")
load('Part5.RData')
library(tidyverse)
library(ggplot2)
library(caret)

train.dtS3$Error<- (train.dtS3$rating - Avg.rating)
Movie.Effect<- aggregate( Error ~ movieId , data = train.dtS3, mean)
names(Movie.Effect)[2]<-"Movie.Effect"

validationS3<- merge(x = validationS3, y = Movie.Effect, by = 'movieId', all.x = T)
validationS3$pred4<- Avg.rating + validationS3$Movie.Effect
rmse4 <- sqrt(mean((validationS3$rating - validationS3$pred4)^2)) # 0.9732817

RMSE.Result <- bind_rows(RMSE.Result, tibble(ModelType = "Linear model with Movie Effect", RMSE = rmse4, Notes = NA))



# MODEL 5:Linear Model with Movie & User Effect
train.dtS3<- merge(x = train.dtS3, y = Movie.Effect, by = 'movieId', all.x = T)
train.dtS3$pred5<- Avg.rating + train.dtS3$Movie.Effect
train.dtS3$Error5<- train.dtS3$rating - train.dtS3$pred5

User.Effect<- aggregate( Error5 ~ userId , data = train.dtS3, mean)
names(User.Effect)[2]<-"User.Effect"

validationS3<- merge(x = validationS3, y = User.Effect, by= 'userId', all.x =T )
validationS3$Pred5 <- Avg.rating + validationS3$Movie.Effect+ validationS3$User.Effect
rmse5 <- sqrt(mean((validationS3$rating - validationS3$Pred5)^2))

RMSE.Result <- bind_rows(RMSE.Result, tibble(ModelType = "Linear Model with Movie & User Effect", RMSE = rmse5, Notes = NA))


save.image('Part6.RData')



setwd("C:/Users/HP/Documents")
load('Part6.RData')

# MODEL 6: Linear Model with Movie Effect Regularized
set.seed(1234, sample.kind = "Rounding") # @Jay sample.kind
train.dtS3$random<- sample(1:10, size = nrow(train.dtS3), replace = TRUE)
train<- train.dtS3 [ which( train.dtS3$random !=1) , ]
test<- train.dtS3 [ which( train.dtS3$random ==1) , ]

'%!in%' <- function(x,y)!('%in%'(x,y))
Dist.Train <- test[(test$movieId %!in% train$movieId),] #446

train<- rbind(train,Dist.Train) #82456 new train with data back
test <- test[!(rownames(test) %in% rownames(Dist.Train)),] #8647
n_distinct (setdiff(test$movieId, train$movieId)) #0


library(recommenderlab)
library(tidyverse)

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  mu_reg <- mean(train$rating)
  
  b_i_reg <- train %>%
    group_by(movieId) %>%
    summarize(b_i_reg = sum(rating - mu_reg)/(n()+l))
  
  predicted_ratings_b_i <-
    test %>%
    left_join(b_i_reg, by = "movieId") %>%
    mutate(pred = mu_reg + b_i_reg) %>%
    .$pred
  
  return(RMSE(test$rating,predicted_ratings_b_i))
})


qplot(lambdas, rmses)
lambda1 <- lambdas[which.min(rmses)]
lambda1 #2.5


Movie.Effect6 <- train.dtS3 %>% group_by(movieId) %>% dplyr::summarise(Sum.Error=sum(Error), n= n())
Movie.Effect6$Movie.Effect6 <- Movie.Effect6$Sum.Error / (Movie.Effect6$n+ lambda1)

validationS3<- merge(x = validationS3, y = Movie.Effect6,by = 'movieId', all.x = T)
validationS3$pred6<- Avg.rating + validationS3$Movie.Effect6
rmse6 <- sqrt(mean((validationS3$rating - validationS3$pred6)^2)) #0.9477642

RMSE.Result <- bind_rows(RMSE.Result, tibble(ModelType = "Linear Model with Movie Effect Regularized", RMSE = rmse6, Notes = "With min.lambda1= 2.5"))

# MODEL 7: Linear Model with Movie & User Effect Regularized

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  mu_reg <- mean(train$rating)
  
  b_i_reg <- train %>%
    group_by(movieId) %>%
    summarize(b_i_reg = sum(rating - mu_reg)/(n()+l))
  
  b_u_reg <- train %>%
    left_join(b_i_reg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u_reg = sum(rating - b_i_reg - mu_reg)/(n()+l))
  
  predicted_ratings_b_i_u <-
    test %>%
    left_join(b_i_reg, by = "movieId") %>%
    left_join(b_u_reg, by = "userId") %>%
    mutate(pred = mu_reg + b_i_reg + b_u_reg) %>%
    .$pred
  
  return(RMSE(test$rating,predicted_ratings_b_i_u))
})


qplot(lambdas, rmses)
lambda2 <- lambdas[which.min(rmses)]
lambda2 #3.25

train.dtS3<- merge(x = train.dtS3, y = Movie.Effect6, by= 'movieId', all.x =T )
train.dtS3$Error7<- train.dtS3$rating - train.dtS3$Movie.Effect6- Avg.rating

User.Effect7 <- train.dtS3 %>% group_by(userId) %>% dplyr::summarise(Sum.Error=sum(Error7), n= n())
User.Effect7$User.Effect7 <- User.Effect7$Sum.Error / (User.Effect7$n+ lambda2)

validationS3<- merge(x = validationS3, y = User.Effect7,by = 'userId', all.x = T)
validationS3$pred7<- Avg.rating + validationS3$Movie.Effect6+ validationS3$User.Effect7
rmse7<- sqrt(mean((validationS3$rating - validationS3$pred7)^2)) #0.8584231

RMSE.Result <- bind_rows(RMSE.Result, tibble(ModelType = "Linear Model with Movie & User Effect Regularized", RMSE = rmse7, Notes = "With min.lambda2= 3.25"))

save.image('Part7.RData')



#Model 8: Collaborative Filtering UBCF
library(dplyr)
setwd("C:/Users/HP/Documents")
load('Part7.RData')
all.data.S <- rbind(train.dtS[,1:3], validationS[,1:3]) #100836 obs, 3 Var

popular.all.data.S <- all.data.S %>% group_by(movieId) %>% filter(n()>=50) %>% ungroup() %>%
  group_by(userId) %>% filter(n()>=20) %>% ungroup() #38856 obs 3var
popular.all.data.U<- popular.all.data.S %>% group_by(userId) %>% summarise(n=n())
popular.all.data.M<- popular.all.data.S %>% group_by(movieId) %>% summarise(n=n())

install.packages("recommenderlab")
library(recommenderlab)
library(reshape2)


real.rating.popular.all.data.S <- dcast(popular.all.data.S, userId ~ movieId, value.var = "rating", na.rm=FALSE)
real.rating.popular.all.data.S <- as.matrix(real.rating.popular.all.data.S[,-1]) #remove userIds
real.rating.popular.all.data.S <- as(real.rating.popular.all.data.S, "realRatingMatrix")
real.rating.popular.all.data.S # 487 x 450 rating matrix of class 'realRatingMatrix' with 39954 ratings


if(as.numeric(R.Version()$major)==3 & as.numeric(R.Version()$minor) >5) set.seed(1, sample.kind="Rounding") else set.seed(1)

n_fold <- 10
items_to_keep <- 15
rating_threshold <- 3.5

eval_sets <- evaluationScheme(data = real.rating.popular.all.data.S, method = "cross-validation", k = n_fold, given = items_to_keep, goodRating = rating_threshold)
training.all.data.S <- getData(eval_sets, "train")
validation.known.all.data.S <- getData(eval_sets, "known") #45*450
validation.unknown.all.data.S <- getData(eval_sets, "unknown") #55*450

training.all.data.S # 432 x 450 rating matrix of class 'realRatingMatrix' with 34341 ratings.
validation.known.all.data.S # 55 x 450 rating matrix of class 'realRatingMatrix' with 825 ratings
validation.unknown.all.data.S # 55 x 450 rating matrix of class 'realRatingMatrix' with 4788 ratings

model.UBCF <- Recommender(data = training.all.data.S, method = "UBCF", parameter = list(method = "Cosine"))
items_to_recommend <- 10 # upto 10 unknown ratings will be predicted
prediction.UBCF <- predict(object = model.UBCF, newdata = validation.known.all.data.S, n = items_to_recommend, type = "ratings")

rmse.UBCF <- calcPredictionAccuracy(x = prediction.UBCF, data = validation.unknown.all.data.S, byUser = FALSE)[1]
rmse.UBCF # RMSE 1.114196

RMSE.Result <- bind_rows(RMSE.Result, tibble(ModelType = "Collaborative Filtering- UBCF", RMSE = rmse.UBCF, Notes = NA))


#Model 9: Collaborative Filtering IBCF
model.IBCF <- Recommender(data = training.all.data.S, method = "IBCF", parameter = list(method = "Cosine"))
items_to_recommend <- 10 # upto 10 unknown ratings will be predicted
prediction.IBCF <- predict(object = model.IBCF, newdata = validation.known.all.data.S, n = items_to_recommend, type = "ratings")

rmse.IBCF <- calcPredictionAccuracy(x = prediction.IBCF, data = validation.unknown.all.data.S, byUser = FALSE)[1]
rmse.IBCF # RMSE 1.30475

RMSE.Result <- bind_rows(RMSE.Result, tibble(ModelType = "Collaborative Filtering- IBCF", RMSE = rmse.IBCF, Notes = NA))

save.image('Part8.RData')
View(RMSE.Result)



# Model 12: Matrix Factorization with Stochastic Gradient Descent

# 1. Convert the data in to the format that can be accepted by recosystem package
setwd("C:/Users/HP/Documents")
load('Part8.RData')
install.packages("recosystem")
library(recosystem)
train_data <- with(train.dtS, data_memory(user_index = userId, item_index = movieId, rating = rating))
test_data <- with(validationS, data_memory(user_index = userId, item_index = movieId, rating = rating))

# 2. Build the model object
model.MF <- recosystem::Reco()

# 3. Fine tune the model for best model parameters.

# Checking for R Version and using set.seed function appropriately
if(as.numeric(R.Version()$major)==3 & as.numeric(R.Version()$minor) >5) set.seed(1, sample.kind="Rounding") else set.seed(1)

opts <- model.MF$tune(train_data, opts = list(dim = c(300),
                                              lrate = c(0.01),
                                              costp_l2 = c(0.01),
                                              costq_l2 = c(0.1),
                                              nthread = 4, niter = 10))
opts$min

# 4. Train the model
# Number of iterations plays a important role. If you increase it too much than the model will overfit on training data
model.MF$train(train_data, opts = c(opts$min, nthread = 4, niter = 100))

# 5. Run the model on test data to predict the ratings
install.packages("Metrics")
library(Metrics)
predicted.ratings.MF <- model.MF$predict(test_data, out_memory())
rmse.MF <- rmse(validationS$rating, predicted.ratings.MF)
rmse.MF #0.8536655
RMSE.Result <- bind_rows(RMSE.Result, tibble(ModelType = "Matrix Factorization with Stochastic Gradient Descent", RMSE = rmse.MF, Notes = "OP: dim = 300,lrate = 0.01,costp_l2 = 0.01,costq_l2 = 0.1,nthread = 4, niter = 10 "))


save.image('part9.RData')




#Big dataset
train.and.test = data.load('10mn')
train.dtB = train.and.test$training.data
validationB = train.and.test$validation
rm(train.and.test)



#Final model with 10 million data set

library(recosystem)
train_data <- with(train.dtB, data_memory(user_index = userId,
                                          item_index = movieId, rating = rating))
test_data <- with(validationB, data_memory(user_index = userId,
                                           item_index = movieId, rating = rating))

# 2. Build the model object
model.MF <- recosystem::Reco()

# 3. Fine tune the model for best model parameters.

# Checking for R Version and using set.seed function appropriately
if(as.numeric(R.Version()$major)==3 & as.numeric(R.Version()$minor) >5)
  set.seed(1, sample.kind="Rounding") else set.seed(1)

opts <- model.MF$tune(train_data, opts = list(dim = c(300),
                                              lrate = c(0.01),
                                              costp_l2 = c(0.01),
                                              costq_l2 = c(0.1),
                                              nthread = 4, niter = 10))


# 4. Train the model
# Number of iterations plays a important role. If you increase it too much than the model will overfit on training data
model.MF$train(train_data, opts = c(opts$min, nthread = 4, niter = 100, verbose = FALSE))

# 5. Run the model on test data to predict the ratings
#install.packages("Metrics")
library(Metrics)
predicted.ratings.MF <- model.MF$predict(test_data, out_memory())
rmse.Bd <- rmse(validationB$rating, predicted.ratings.MF)

RMSE.Result <- bind_rows(RMSE.Result,
                         tibble(ModelType = "Big Dataset Matrix Factorization with Stochastic
Gradient Descent ",
                                RMSE = rmse.Bd,
                                Notes = NA))





save.image('capstone1.RData')



save.image("Final_Image.Rdata")
load("Final_Image.Rdata")
