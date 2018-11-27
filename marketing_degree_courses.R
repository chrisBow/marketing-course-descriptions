# top 25 universities for marketing taken from https://www.thecompleteuniversityguide.co.uk/league-tables/rankings?s=Marketing


# import and prepare data

library(readr)
courses_df <- read_csv("degree_data.csv")

library(dplyr)
courses_df <- 
  courses_df %>%
  select(university, title, type, copy)

glimpse(courses_df)



# change university course title from lower case to title case

library(tools)

courses_df <-
  courses_df %>%
  mutate(title = toTitleCase(title)) %>%
  mutate(title = as.factor(title),
         type = as.factor(type))

glimpse(courses_df)



# set up theme for ggplot2 plots

theme_chris <- function (base_size = 12, base_family = "serif", ticks = TRUE) 
{
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(legend.background = element_blank(), legend.key = element_blank(), 
          panel.border = element_blank(), 
          strip.background = element_blank(), 
          panel.background = element_rect(fill = "#94B1C533", colour = NA),
          plot.background = element_rect(fill = "#ffffff"),
          axis.line = element_blank(), 
          panel.grid = element_blank(),
          axis.text.x = element_text(colour = "#2a3132"),
          axis.title.x = element_text(colour = "#2a3132"),
          axis.title.y = element_text(colour="#2a3132"),
          axis.text.y = element_text(colour="#2a3132"),
          axis.title = element_text(colour = "#2a3132"),
          plot.title = element_text(colour = "#2a3132", 
                                    margin = margin(0,0,10,0)),
          plot.subtitle = element_text(colour = "#2a3132"),
          plot.caption = element_text(colour = "#2a3132"),
          legend.title = element_text(colour = "#2a3132"),
          legend.text = element_text(colour = "#2a3132"))
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  ret
}



# overview of courses by name

library(ggplot2)
library(forcats)

courses_df %>%
  group_by(title) %>%
  summarise(number = n()) %>%
  ggplot(aes(x = fct_reorder(title, number),
             y = number)) +
  geom_col(width = 0.5, fill = "#336b87") +
  coord_flip() +
  ylim(0, 15) +
  labs(title = "Number of Courses by Name",
       x = "Course name",
       y = "Count of course names in top 25 universities") +
  theme_chris()


# overview of courses by degree type

courses_df %>%
  group_by(type) %>%
  summarise(number = n()) %>%
  ggplot(aes(x = fct_reorder(type, number),
             y = number)) +
  geom_col(width = 0.5, fill = "#336b87") +
  ylim(0,15) +
  labs(title = "Count of Courses by Degree Type",
       x = "Degree Type",
       y = "Count of marketing course types in top 25 universities") +
  theme_chris()


# top words used in course descriptions

library(tidytext)

courses_df %>%
  unnest_tokens(word, copy) %>%
  anti_join(stop_words) %>%
  anti_join(extra_stop_words) %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  top_n(25) %>%
  ggplot(aes(x = fct_reorder(word, count),
             y = count)) +
  geom_col(fill = "#336b87") +
  labs(title = "Top Words in Marketing Course Overview Pages",
       x = "Word",
       y = "Count") +
  coord_flip() +
  theme_chris()



# look for presence of key words

library(stringr)

data_words <- c("data",
           "analytics",
           "statistics")

queries <- paste(data_words, collapse = "|")

courses_df <- 
  courses_df %>%
  mutate(keyword = str_detect(copy, queries))


# plot courses that mention data keywords

ggplot(courses_df, aes(x = keyword)) +
  geom_bar(fill = "#336b87", width = 0.5) +
  labs(title = "Marketing Degree Course Overview Pages That \nMention 'Data', 'Analytics' or 'Statistics'",
       x = "Includes keyword",
       y = "Count of courses") +
  ylim(0, 25) +
  theme_chris()



# plot courses that mention data keywords by degree type

ggplot(courses_df, aes(x = type,
                       fill = keyword)) +
  geom_bar(position = "fill", width = 0.5) +
  scale_fill_manual(values = c("#336b87", "#2a3132")) +
  labs(title = "Marketing Degree Course Overview Pages That Mention \n'Data', 'Analytics' or 'Statistics' by Degree Type",
       caption = "Keywords: 'data', 'analytics', 'statistics'",
       x = "Degree type",
       y = "Proportion of courses",
       fill = "Includes keyword") +
  theme_chris()





