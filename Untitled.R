bondv3 %>% 
  group_by(complete_cusip,rating_date,rating)

DataZscore <- q188f447abac283de %>% 
  mutate(Zscore = A+B+C+D+E) %>% 
  filter(Zscore != 'NA')

ZscoreGroupedbyFY <- DataZscore %>% 
  group_by(FYEAR) %>% 
  summarise(Zscore=mean(Zscore, na.rm = TRUE))

ggplot(ZscoreGroupedbyFY, aes(x = FYEAR, y = Zscore))+
  geom_line()+
  geom_point()+
  ggtitle('Avg Zscore by Year')


ZscoreGroupedbyFYMedian <- DataZscore %>% 
  group_by(FYEAR) %>% 
  summarise(Zscore=median(Zscore, na.rm = TRUE))

ggplot(ZscoreGroupedbyFYMedian, aes(x = FYEAR, y = Zscore))+
  geom_line()+
  geom_point()+
  ggtitle('Median Zscore by Year')


remove_missing(DataZscore,na.rm=TRUE)%>% 
  summary()