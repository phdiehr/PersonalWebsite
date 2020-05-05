plotthis <- ZScoreYearsSummaryV3 %>% 
  mutate(Year = substr(ZScoreYearsSummaryV3$DATADATE,1,4)) %>%
  group_by(Year) %>% 
  summarise(mean(Avg_ZScore))

plot(plotthis)



A = 1.2*(q188f447abac283de$WCAP/q188f447abac283de$AT)
B = 1.4*(q188f447abac283de$RE/q188f447abac283de$AT)
C = 3.3*(q188f447abac283de$EBIT/q188f447abac283de$LT)
D = 0.6*(q188f447abac283de$MKVALT/q188f447abac283de$LT)
E = 1.0*(q188f447abac283de$SALE/q188f447abac283de$AT)

DataZscore <- q188f447abac283de %>% 
  mutate(Zscore = A+B+C+D+E) %>% 
  filter(Zscore != 'NA', Zscore > -2.199, Zscore < 13)


ZscoreGroupedbyFY <- DataZscore %>% 
  group_by(FYEAR) %>% 
  summarise(Zscore=mean(Zscore))

ggplot(ZscoreGroupedbyFY, aes(x = FYEAR, y = Zscore))+
  geom_line()+
  geom_point()+
  ggtitle('Avg Zscore by Year')

plot(ZscoreGroupedbyFY)

blogdown::serve_site()
