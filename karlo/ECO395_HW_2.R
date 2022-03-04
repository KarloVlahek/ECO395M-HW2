library('tidyverse')
library('ggplot2')
library("modelr")
library("rsample")
library("ggthemes")
library('mosaic')

#1a
capmetro_UT$day_of_week = ordered(capmetro_UT$day_of_week, levels =c("Sun", "Mon", "Tue","Wed","Thu", "Fri","Sat"))
capmetro_UT[order(capmetro_UT$day_of_week), ]

weekdays_ = c(
  `Sun` = "Sunday",
  `Mon` = "Monday",
  `Tue` = "Tuesday",
  `Wed` = "Wednesday",
  `Thu` = "Thursday",
  `Fri` = "Friday",
  `Sat` = "Saturday"
)

avg_boarding = capmetro_UT %>% 
  group_by(hour_of_day, day_of_week, month) %>%
  summarise(avgboarding = mean(boarding)) %>% 
  arrange((day_of_week))

avg_plot = ggplot(avg_boarding) +
  geom_line(aes(y=avgboarding, x= hour_of_day, color=month), size =0.8, alpha = 0.9) +
  facet_wrap(~day_of_week, scales ='free', labeller = as_labeller(weekdays_))+
  labs(title = "2018 Capital Metro Daily Passenger Average",
       y = "Average # of Passengers",
       x = "Time (24-Hour Cycle)",
       col = 'Month',
       caption = 
       "The figure provides interesting takeaways on several mentions. Firstly, on weekdays, the peak number of average passengers board the metro around 3 PM to 5:30 PM.
   During the weekends, the magnitude of passengers is nowhere near that of weekdays, but traffic incrementally increases over time throughout the weekend. On average,
   Monday's were lower in September most likely due to holidays such as Labor Day. We could observe lower overall average passengers in November on Wednesdays, Thursdays, 
   and Fridays, because of Thanksgiving holiday. In 2022 and onward, I anticiaptee average November passengers to be lower throughout the whole week as Thanksgiving break has 
   been extended to a full week.") +
  scale_color_manual(labels= c("November", "October", "September"),
                  values = c(Nov = 'red',
                             Oct = 'green',
                             Sep = 'blue')) +
  theme(plot.caption = element_text(hjust = 0.5, face= "italic"),
        plot.title = element_text(hjust=0.5), 
        plot.subtitle =element_text(hjust=0.5))

avg_plot

#1b

timestamp_temp = capmetro_UT %>% 
  select(timestamp, boarding, temperature) %>% 
  group_by(timestamp)



hours_ = c(
  `6` = "6:00 AM",
  `7` = "7:00 AM",
  `8` = "8:00 AM",
  `9` = "9:00 AM",
  `10` = "10:00 AM",
  `11` = "11:00 AM",
  `12` = "12:00 PM",
  `13` = "1:00 PM",
  `14` = "2:00 PM",
  `15` = "3:00 PM",
  `16` = "4:00 PM",
  `17` = "5:00 PM",
  `18` = "6:00 PM",
  `19` = "7:00 PM",
  `20` = "8:00 PM",
  `21` = "9:00 PM"
)

temp_plot = ggplot(capmetro_UT) +
  geom_point(aes(y = boarding, x = temperature, color = weekend), size =0.9, alpha = 0.6)+
  facet_wrap(~hour_of_day, scales = 'free',labeller = as_labeller(hours_)) +
  theme_tufte() + theme(axis.line=element_line()) +
  labs(title = 'Capital Metro Passengers and Temperature Throughout the Day',
       x = 'Temperature (°F)',
       y = 'Number of Passengers',
       col = "",
       caption ="Holding both the hour of the day and weekend status constant, we see that temperature doesn't have a noticeable effect in the number of UT passengers.") +
  scale_color_manual(labels= c("Weekday","Weekend"),
                     values = c(weekday = 'red',
                                weekend = 'turquoise'))+
  theme(plot.caption = element_text(hjust = 0.5, face= "bold.italic"),
        plot.title = element_text(hjust=0.5), 
        plot.subtitle =element_text(hjust=0.5))

temp_plot




#2
#make sure to set seed
data(SaratogaHouses)




#3

#What is the probability that they default given each of the history categories (i.e. P(D|G), P(D|P), P(D|T))?

default_data = addmargins(table(german_credit$Default, german_credit$history))
default_data
#dd knitr table for ^ above margins
prob_good=default_data[2,1]/default_data[3,1]
prob_poor=default_data[2,2]/default_data[3,2]
prob_terrible=default_data[2,3]/default_data[3,3]

default_prob_plot = ggplot(german_credit, aes(history)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Probability of Default on Loan given Credit History")+
  scale_y_continuous(labels=scales::percent) +
  ylab("Relative Frequencies") +
  xlab("Credit History Classification")

default_prob_plot


#Now the model

model1 = glm(Default~duration + amount + installment + age + history + purpose + foreign, family = binomial, data = german_credit)
summary(model1)
default_odds = coef(model1) %>% lapply(exp)

#4











  

