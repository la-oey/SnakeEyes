source('cleanData.R')

df %>%
  filter(roleCurrent == "bullshitter") %>%
  ggplot()


