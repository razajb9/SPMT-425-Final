getwd()

setwd("D:/Fall 22/SPMT 425")

nba_no_d <- read.csv("2018-2019 NBA data dups removed.csv")

#changing character variable to factor variable

nba_no_d$Pos <- as.factor(nba_no_d$Pos)

### Finding the average value of a shot

## total 3 point shots + total 2 point shots
sum(nba_no_d$X3PA) + sum(nba_no_d$X2PA)
### = 219458
## 3 pointers made * 3  + 2 pointers made * 2
(sum(nba_no_d$X3P)*3) + (sum(nba_no_d$X2P)*2)
### = 230079

shot_val = ((sum(nba_no_d$X3P)*3) + (sum(nba_no_d$X2P)*2)) / (sum(nba_no_d$X3PA) + sum(nba_no_d$X2PA))


### Finding the average value of a field goal made
sum(nba_no_d$X3P)/(sum(nba_no_d$X3P) + sum(nba_no_d$X2P))
### percentage of field goals scored that are 3's
1-sum(nba_no_d$X3P)/(sum(nba_no_d$X3P) + sum(nba_no_d$X2P))
0.7233876 * 2
0.2766124 * 3
scored_shot_val = 1.446775 + 0.8298372


### finding the points given away by a personal foul
foul_val = sum(nba_no_d$FT) / sum(nba_no_d$PF)

install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("ggthemes")
library(ggthemes)

# Net Points Created = Points + Assists*Value of a Scored Shot + Offensive Rebounds * Value of a Shot + Steals * Value of a Shot 
#cont.# - Turnovers * Value of a Shot - Personal Fouls * Average Value of Points Scored from Free Throws
nba_NPC <- mutate(nba_no_d, NPC = round(PTS + (AST * scored_shot_val) + (ORB * shot_val) + (STL * shot_val) - (TOV * shot_val) - (PF * foul_val), 2))

# Afterwards I divided the NPC by Minutes Played
nba_NPC <- mutate(nba_NPC, NPCperMP = round((PTS + (AST * scored_shot_val) + (ORB * shot_val) + (STL * shot_val) - (TOV * shot_val) - (PF * foul_val))/MP, 4))

summary(nba_NPC$MP)
# I filtered out the bottom 25% players in Minutes Played
nba_NPC <- filter(nba_NPC, nba_NPC$MP > 323.5)

# Then I sorted and cut out the top 10 players in terms of NPC/MP and wrote their stats onto a csv
nba_NPC_ord <- head(nba_NPC[order(-nba_NPC$NPCperMP),], n=10)
write.csv(nba_NPC_ord, "D:/Fall 22/SPMT 425/nbaTOP10.csv", row.names=FALSE)

# And then I did the same thing for the bottom 10 players in terms of NPC/MP
nba_NPC_rev_ord <- tail(nba_NPC[order(-nba_NPC$NPCperMP),], n=10)
write.csv(nba_NPC_rev_ord, "D:/Fall 22/SPMT 425/nbaBOT10.csv", row.names=FALSE)

ggplot(nba_NPC)+
  aes(x=NPCperMP, y=Pos, fill= Pos)+
  geom_boxplot()+
  labs(title = "Distribution of NPC/MP across Roles",
       subtitle = "2018-19 NBA Players with over 323 MP",
       x="Net Points Created per Minute Played",
       y="Main Position")

nba_NPC <- filter(nba_NPC, nba_NPC$MP > 1000)
filter(head(nba_NPC[order(-nba_NPC$NPCperMP),], n=16))%>%
  ggplot()+ #plot of the top 15 pitchers based on NPCperMP
  aes(x=PTS,y=NPCperMP, label = Player)+ # we need to tell Rstudio what to make the label, here we want names
  geom_line()+ # we can use geom_point here and it will work the exact same way
  geom_text(position = "dodge")+
  labs(title = "Top 16 Players in NPC/MP",
       subtitle = "2018-19 NBA Players with over 1000 MP",
       x="Points Scored",
       y="NPC per MP")

### For this part of the code I just switched out the position as needed and adjusted the title as needed for each of the 5 positions
tophomies <- filter(nba_NPC, Pos == 'C')
filter(head(tophomies[order(-tophomies$NPCperMP),], n=10)) %>% #same scatterplot as before w/o the Line of Regression
  ggplot() +
  aes(x = PTS, y = NPCperMP, color = Player, label = Player) +
  geom_jitter()+
  geom_text(position = "dodge" )+
  labs(title = "Top 10 Centers",
       subtitle = "2018-19 NBA Players with over 1000 MP",
       x="Points Scored",
       y="NPC per MP")

write.csv(nba_NPC, "D:/Fall 22/SPMT 425/nba_NPC.csv", row.names=FALSE)
