#creating a cluster bar chart for confirmed and presumed cases 
ggplot(data = conf_pres_summary, mapping = aes(x = region, y = count, fill = `case type`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "presumed vs confirmed malaria cases by region", subtitle = "Ginbot, 2016 to Tikimt, 2017") +
  scale_fill_manual(values = c(presumed = "green", confirmed = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#stacked bar chart for the above
ggplot(data = conf_pres_summary, mapping = aes(x = region, y = count, fill = `case type`)) +
  geom_bar(stat = "identity") +
  labs(title = "presumed vs confirmed malaria cases by region", subtitle = "Ginbot, 2016 to Tikimt, 2017") +
  scale_fill_manual(values = c(presumed = "pink", confirmed = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "plots/presumed vs confirmed malaria cases by region.pdf", width = 7, height = 5)

#creating line graph to see the malaria trend in each regions 
ggplot(data = confirmed_cases, mapping = aes(x = month, y = Value)) +
  geom_line(aes(group = region), size = 1) +   # Add lines for each region
  geom_point(size = 2) +                        # Add points for better visibility
  labs(title = "Trend of Confirmed Malaria Cases",
       subtitle = "Ginbot, 2016 - Tikimt, 2017") +
  theme_classic() +
  facet_wrap(~region)

#Plotting total malaria cases by region
ggplot(data = malaria_case_summary, mapping = aes(x= reorder(region, - total_cases), 
                                                  y= total_cases)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Malaria Cases by region", subtitle = "Ginbot, 2016 to Tikimt, 2017",
       x = "Region",
       y = "Number of Cases")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "plots/total malaria cases by region.pdf", width = 7, height = 5)


ggsave(filename = "plots/malaria trend by region.pdf", width = 7, height = 5)