library(tidyverse)
library(readxl)


install.packages('Hmisc')
pl <- read.csv("EPL.csv")
pl <- read.csv("season-1819_csv.csv")
pl <- read_excel("EPL.xlsx")
#6위맨유
manu <- pl %>% 
  filter(HomeTeam == 'Man United' | AwayTeam == 'Man United')

manuH <- pl %>% 
  filter(HomeTeam == 'Man United')

mean(manuH$FTHG)
mean(manuH$FTAG)


manuA <- pl %>% 
  filter( AwayTeam == 'Man United')

mean(manuA$FTHG)
mean(manuA$FTAG)

#2위 리버풀
liv <- EPL %>% 
  filter(HomeTeam == 'Liverpool'|AwayTeam == 'Liverpool')

livH <- liv %>% 
  filter(HomeTeam == 'Liverpool')

 

mean(livH$FTHG)
mean(livH$FTAG)

livA <- liv %>% 
  filter(AwayTeam == 'Liverpool')

mean(livA$FTHG)
mean(livA$FTAG)


#1위 맨시티
manc <- pl %>% 
filter(HomeTeam == 'Man City'|AwayTeam == 'Man City')

mancH <- manc %>% 
  filter(HomeTeam == 'Man City')

mean(mancH$FTHG)
mean(mancH$FTAG)

mancA <- manc %>% 
  filter(AwayTeam == 'Man City')




mean(mancA$FTHG)
mean(mancA$FTAG)

#심판

plre <- EPL %>% 
  filter(Referee == 'M Oliver')

mean(plre$FTHG)
mean(plre$FTAG)

mean(plre$HY)
mean(plre$AY)

mean(EPL$FTHG)
mean(EPL$FTAG)

var(EPL$FTHG)
var(EPL$FTAG)

sd(EPL$FTHG)
sd(EPL$FTAG)

var(plre$FTHG)
sd(plre$FTHG)

ggplot(livH, aes(x=Date,
                 y=FTHG))+
  geom_point()

EPL <- pl %>% 
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR, Referee,
        HS, AS, HST, AST, HF, AF, HC, AC, HY, AY, HR, AR)

write.csv(EPL, file = "EPL.CSV")

