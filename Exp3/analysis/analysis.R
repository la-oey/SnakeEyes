source('cleanData.R')

glimpse(df)




sender <- df %>%
  filter(roleCurrent == "bullshitter")

sender %>%
  ggplot(aes(x=trueRoll, y=reportedRoll)) +
  geom_jitter()

sender %>%
  ggplot(aes(x=reportedRoll)) +
  geom_bar() +
  ggtitle("Roll Counts")

sender %>%
  gather("type","roll",6:7) %>%
  select(subjID, type, roll) %>%
  ggplot(aes(x=roll, fill=type, colour=type)) +
  geom_bar(alpha=0.5, position="identity") +
  scale_x_continuous(breaks = c(0:10))

sender %>%
  mutate(truth = trueRoll == reportedRoll) %>%
  #group_by(trueRoll, reportedRoll, truth) %>%
  #summarise(n = n()) %>%
  #arrange(desc(n))
  ggplot(aes(x=reportedRoll, fill=truth)) +
  geom_bar() +
  scale_x_continuous("reported roll", expand=c(0,0), breaks=seq(0,10,2)) +
  scale_y_continuous(breaks=seq(0,400,400)) +
  scale_fill_manual(values=c("red","forestgreen")) +
  coord_flip() +
  guides(fill=F) +
  facet_wrap(~trueRoll, ncol=11) +
  theme_bw()
ggsave("img/true_vs_reported_bar.png", width=7, height=2.5)

sender %>%
  count(trueRoll, reportedRoll) %>%
  complete(trueRoll=0:10, reportedRoll=0:10, fill = list(n = 0)) %>%
  ggplot(aes(x=trueRoll, y=reportedRoll, fill=n)) +
  geom_tile() +
  scale_x_continuous("true roll", expand=c(0,0), breaks=seq(0,10,2)) +
  scale_y_continuous("reported roll", expand=c(0,0), breaks=seq(0,10,2)) +
  scale_fill_gradient("counts", low="white",high="navy")
ggsave("img/true_vs_reported_tile.png")




# computer's behavior

df %>%
  filter(roleCurrent == "bullshitter") %>%
  group_by(reportedRoll) %>%
  summarise(computerCallBS = sum(callBS)/n()) %>%
  ggplot(aes(x=reportedRoll, y=computerCallBS)) +
  geom_point() +
  geom_line()



