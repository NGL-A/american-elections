data_uncleaned <- read.csv("county_statistics.csv", sep = ",", header = TRUE)

data <- na.omit(data_uncleaned)

data$Party16 = factor(if_else(percentage16_Hillary_Clinton >= percentage16_Donald_Trump, "Democrat", "Republican"))

data$Party20 = factor(if_else(percentage20_Joe_Biden >= percentage20_Donald_Trump, "Democrat", "Republican"))

attach(data)

rownames(data) <- 1:nrow(data)
plot(percentage20_Donald_Trump, Hispanic)
hist(lat, freq = FALSE)
lines(density(na.omit(lat[votes20_Donald_Trump])))

library(tidyverse)
library(maps)
#install.packages("mapproj")
library(png)
library(grid)
library(cowplot)
library(statebins)
library(DT)


library(gRbase)
library(ISLR2)
library(leaps)

v<-c(data$percentage20_Donald_Trump<0.5)
regfit.full <- regsubsets(v~., data=data[,-1:-13], really.big = T,nvmax = 38)
reg.summary <- summary(regfit.full)
reg.summary$outmat
reg.summary$rsq
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted Rsq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)


# Mallow's Cp with its smallest value
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)

# BIC with its smallest value
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.summary$bic)
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

par(mfrow=c(1,1))


par(mfrow=c(2,2))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")

plot(regfit.full,scale="bic")
coef(regfit.full,6)
par(mfrow=c(1,1))















options(repr.plot.width = 17, repr.plot.height = 9)

prov.usa <- map_data("state")

prov.abb <- data.frame(state = state.name, Abb = state.abb)

prov.abb2<-prov.abb

prov.abb2$state<-tolower(prov.abb2$state)

prov.election <- data %>% 
  group_by(state) %>% 
  summarise(votes16_Hillary_Clinton = sum(votes16_Hillary_Clinton),
            votes16_Donald_Trump = sum(votes16_Donald_Trump)) %>% 
  ungroup() %>% 
  gather(Candidate, Votes, -state) %>% 
  mutate(Party = factor(if_else(Candidate == "votes16_Hillary_Clinton", "Democrat", "Republican"))) %>% 
  left_join(prov.abb2) %>% 
  rename("region" = state)

prov.election2 <- prov.election %>% 
  group_by(region, Abb) %>% 
  summarise(Votes = max(Votes)) %>% 
  ungroup() %>% 
  inner_join(prov.election)

prov.election2<-prov.election2[,-2]

colnames(prov.abb2)[1]<-"region"

colnames(prov.election2)[1]<-"Abb"

prov.election3<-left_join(prov.election2,prov.abb2)

prov.usa_election <- left_join(prov.usa, prov.election3) %>% 
  filter(!is.na(Party))

map.us.16<-ggplot(data = prov.usa_election, aes(x = long, y = lat),color = prov.usa_election$Party)+
  geom_polygon(aes(group = group, fill = Party),color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + scale_fill_manual(values = alpha(c("blue2", "red3"), 1))

prov.abb <- data.frame(state = state.name, Abb = state.abb)

prov.abb2<-prov.abb

prov.abb2$state<-tolower(prov.abb2$state)

prov.election2020 <- data %>% 
  group_by(state) %>% 
  summarise(votes20_Joe_Biden = sum(votes20_Joe_Biden),
            votes20_Donald_Trump = sum(votes20_Donald_Trump)) %>% 
  ungroup() %>% 
  gather(Candidate, Votes, -state) %>% 
  mutate(Party = factor(if_else(Candidate == "votes20_Joe_Biden", "Democrat", "Republican"))) %>% 
  left_join(prov.abb2) %>% 
  rename("region" = state)

prov.election20202 <- prov.election2020 %>% 
  group_by(region, Abb) %>% 
  summarise(Votes = max(Votes)) %>% 
  ungroup() %>% 
  inner_join(prov.election2020)

prov.election20202<-prov.election20202[,-2]

colnames(prov.abb2)[1]<-"region"

colnames(prov.election20202)[1]<-"Abb"

prov.election20203<-left_join(prov.election20202,prov.abb2)

prov.usa_election2020 <- left_join(prov.usa, prov.election20203) %>% 
  filter(!is.na(Party))

map.us.20<-ggplot(data = prov.usa_election2020, aes(x = long, y = lat),color = prov.usa_election2020$Party)+
  geom_polygon(aes(group = group, fill = Party),color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + scale_fill_manual(values = alpha(c("blue2", "red3"), 1))

prov.data2 <- data %>% 
  group_by(state) %>% 
  summarise(Black = sum(Black*TotalPop),
            Hispanic=sum(Hispanic*TotalPop),
            Asian=sum(Asian*TotalPop),
            votes16_Hillary_Clinton = sum(votes16_Hillary_Clinton),
            votes16_Donald_Trump = sum(votes16_Donald_Trump),
            TotalPop = sum(TotalPop)) %>% 
  ungroup() %>% 
  rename("region" = state)

prov.data2$Black<- prov.data2$Black/prov.data2$TotalPop

prov.data2$Hispanic<- prov.data2$Hispanic/prov.data2$TotalPop

prov.data2$Asian<- prov.data2$Asian/prov.data2$TotalPop

colnames(prov.abb2)[1]<-"Abb"

colnames(prov.abb2)[2]<-"region"

prov.data3<-left_join(prov.data2,prov.abb2)

colnames(prov.data3)[1]<-"Abb"

colnames(prov.data3)[8]<-"region"

prov.usa_data <- left_join(prov.usa, prov.data3) 

map.pop<-ggplot(data = prov.usa_data, aes(x = long, y = lat),color = prov.usa_data$TotalPop)+
  geom_polygon(aes(group = group, fill = TotalPop),color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +scale_fill_gradient(names<-"TotalPop", low="White", high = "deepskyblue4")

map.black<-ggplot(data = prov.usa_data, aes(x = long, y = lat),color = prov.usa_data$Black)+
  geom_polygon(aes(group = group, fill = Black),color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +scale_fill_gradient(names<-"Black", low="White", high = "Black")

map.hisp<-ggplot(data = prov.usa_data, aes(x = long, y = lat),color = prov.usa_data$Hispanic)+
  geom_polygon(aes(group = group, fill = Hispanic),color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +scale_fill_gradient(names<-"Hispanic", low="White", high = "coral3")

map.asian<-ggplot(data = prov.usa_data, aes(x = long, y = lat),color = prov.usa_data$Asian)+
  geom_polygon(aes(group = group, fill = Asian),color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +scale_fill_gradient(names<-"Asian", low="White", high = "darkgoldenrod")

map.us.16
map.us.20
map.pop
map.black
map.hisp
map.asian




plot(Hispanic)
hist(Income[percentage20_Donald_Trump>0.5])
hist(Income[percentage20_Donald_Trump<0.5])

q=c(percentage20_Joe_Biden>0.5)

ggplot(data,aes(x=data$Asian, group=q,fill=q))+
  geom_histogram(position="identity",alpha=0.5,binwidth=1)+theme_bw()


S<-cov(data[,4:51])
P<-cov2cor(S)


library(corrplot)

corrplot(P)


library(glmnet)

X <- model.matrix(percentage20_Joe_Biden~.,data)
X<-X[,-1]
y<-percentage20_Joe_Biden
grid<-10^seq(10,-1,length=100)
ridge.mod<-glmnet(X,y,alpha = 0,lambda = grid)
plot(ridge.mod, xvar="lambda")


train <- sample(1:nrow(X),nrow(X)/2)
test<-(-train)
y.test<-y[test]
ridge.pred<-predict(ridge.mod,s=4,newx = X[test,],type = "response")
mean((ridge.pred-y.test)^2)


lm(percentage20_Joe_Biden~.,data[,4:51],subset = train)
predict(ridge.mod,s=0,exact = TRUE,type = "coefficients",x=X[train,],y=y[train])[1:20,]


# use cross-validation to choose the value of lambda 

cv.out <- cv.glmnet(X[train, ], y[train], alpha = 0, nfold=10)

cv.out$lambda[1:10]
summary(cv.out$lambda)

plot(cv.out)
i.bestlam <- which.min(cv.out$cvm)
i.bestlam 
bestlam <- cv.out$lambda[i.bestlam]
bestlam
cv.out$cvm[i.bestlam]

min(cv.out$cvm)

bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod, s = bestlam,
                      newx = X[test, ])
mean((ridge.pred - y.test)^2)

