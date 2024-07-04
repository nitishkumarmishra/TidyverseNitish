#https://github.com/esogin/tidy/blob/master/tuesdays/nz_birds/nz_birds.Rmd
#https://github.com/esogin/tidy/tree/master/tuesdays/nz_birds
rm(list = ls())

library(ggplot2)
library(tidyverse)

nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")
head(nz_bird); dim(nz_bird)

#vote rank: 1 is highest, 5 is lowest

# Data cleaning
unique(nz_bird$bird_breed)

# remove NAs
nz<-nz_bird[complete.cases(nz_bird),]
#split the vote rank
nz<-data.frame(nz %>% separate(vote_rank,c('vote','rank'),sep='_'))
# turn date + hour into a postx object, I even specified the time zone right!
nz$date_time<-as.POSIXct(strptime(paste(nz$date, nz$hour), format="%Y-%m-%d %H", tz="NZ"))
# turn rank into numeric value
nz$rank<-as.numeric(nz$rank)
head(nz)

#get means, se, and sample size
nz.means<-nz %>% group_by(bird_breed) %>% summarize(m=mean(rank), se=sd(rank)/sqrt(length(rank)), n=length(rank))
nz.means$bird_breed<-as.factor(nz.means$bird_breed)
nz.means$bird_breed<-fct_reorder(nz.means$bird_breed, nz.means$m, .desc = T) #reorder factor based on mean value
top_ten<-nz.means[order(nz.means$m,decreasing = F),] #order data according to avearge value
top_ten<-top_ten[1:10,] # select top ten
ggplot(top_ten, aes(x=m, y=bird_breed, label=paste('n = ',n))) + geom_point(size=3) + 
  geom_errorbarh(aes(xmin=m-se, xmax=m+se),height=0.5) + 
  theme_classic() +  ylab('Type of NZ bird') + xlab('Average voter rank  ± s.e.') + 
  geom_text(mapping = aes(x=m+se+0.1)) + labs(title="NZ's favorite bird is the yellow-eyed penguine", subtitle='it also got far more votes then any  other bird') + theme(plot.title=element_text(face = 'bold'))
ggsave('nz_birds.jpeg',width=10,height=5)
