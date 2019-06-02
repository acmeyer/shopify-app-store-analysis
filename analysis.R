library(tidyverse)
library(lubridate)
library(gridExtra)
library(stringr)

# Setup theme for plots
shopify_theme <- theme_minimal() +
  theme(
    plot.title = element_text(color = "#212B36", face = "bold", margin = unit(c(8,0,8,0), "pt")),
    plot.background = element_rect(fill = "#DFE3E8", color = "#C4CDD5"),
    axis.title = element_text(face = "bold"),
    axis.line = element_line(color = "#919EAB"),
    axis.ticks = element_line(color = "#919EAB"),
    axis.text = element_text(color = "#454F5B"),
    plot.caption = element_text(color = "#919EAB", face = "italic"),
    plot.margin = unit(c(16,16,16,16),"pt"),
    panel.grid.major = element_line(color = "#C4CDD5"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(color = "#212B36", face = "bold")
  )

shopify_table_theme <- ttheme_default(
  colhead=list(fg_params=list(col="#FFFFFF"), bg_params = list(col=NA, fill = "#5C6AC4")),
  core=list(bg_params = list(col=NA, fill = "#DFE3E8")),
  rowhead=list(fg_param=list(col=NA))
)
  
theme_set(shopify_theme)
  
# Read CSVs and setup dataframes
# Listings
listings_data <- read.csv('listings.csv') %>%
  mutate(monetization = ifelse(pricing == 'Free', 'Free', 'Paid'))

# Reviews
reviews_data <- read.csv('reviews.csv') %>% 
  mutate(
    date = as.Date(date, format = "%m/%d/%y"),
    monetization = listings_data$monetization[match(app_id, listings_data$id)]
  )

# Key Phrases
key_phrases <- read.csv('key_phrases.csv') %>% 
  mutate(
    positive = rating > 3,
    negative = rating < 3,
    neutral = rating == 3,
    phrase = str_trim(str_to_lower(phrase))
  )

# Save listings with reviews
listings_with_rating_data <- listings_data %>%
  filter(rating > 0)

# Save paid listings with reviews
paid_listings_with_reviews <- listings_with_rating_data %>%
  filter(monetization == 'Paid')

# Category related dataframe
listings_cat_data <- listings_data %>%
  group_by(category_name) %>%
  summarize(
    median_rating = median(rating[rating > 0], na.rm = TRUE),
    avg_rating = mean(rating[rating > 0], na.rm = TRUE),
    num_of_apps = n(),
    num_of_devs = n_distinct(author),
    free_apps_count = sum(monetization == 'Free'),
    paid_apps_count = sum(monetization == 'Paid'),
    percent_paid = paid_apps_count / n()
  ) %>%
  arrange(avg_rating)

# Plots
# Create printable table
category_table <- listings_cat_data %>% 
  mutate(
    avg_rating = round(avg_rating, digits = 2),
    percent_paid = paste(round(percent_paid, digits = 3) * 100, "%")
  ) %>% 
  select(category_name, avg_rating, num_of_apps, num_of_devs, percent_paid) %>% 
  rename(Category = category_name, "Avg Rating" = avg_rating, "Total Apps" = num_of_apps, "Total Developers" = num_of_devs, "% Paid" = percent_paid) %>% 
  tableGrob(theme = shopify_table_theme, rows = NULL) %>% 
  grid.arrange()
ggsave("plots/category_table.png", category_table)

# Ratings by Category
ratings_by_category <- listings_cat_data %>%
  ggplot() +
    geom_bar(
      mapping = aes(
        x = reorder(category_name, avg_rating), 
        y = avg_rating
      ), stat = "identity", fill = "#9C6ADE"
    ) +
    labs(
      x = "",
      y = "Rating",
      title = "Average Rating by Category",
      caption = paste(
        "Source: Shopify App Store",
        "as of",
        format(now(), "%m/%d/%y")
      )
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45, hjust = 1
      )
    )
ggsave("plots/avg_rating_by_category.png", ratings_by_category)

# Distribution of ratings by category paid vs free
dist_ratings_by_category_paid_vs_free <- listings_with_rating_data %>%
  ggplot() +
    geom_density(aes(x = rating), color="#9C6ADE", fill="#9C6ADE", bw=0.05) +
    facet_wrap(category_name ~ monetization, ncol = 4) +
  labs(
    x = "Rating",
    y = "",
    title = "Distribution of Ratings by Category - Free vs Paid",
    caption = paste(
      "Source: Shopify App Store",
      "as of",
      format(now(), "%m/%d/%y")
    )
  ) +
  theme(
    axis.text.y = element_blank()
  )
ggsave("plots/dist_ratings_by_category_paid_vs_free.png", dist_ratings_by_category_paid_vs_free, width = 12)

# Distribution of ratings per category - Paid only
distribution_by_category_paid <- paid_listings_with_reviews %>%
  ggplot() +
    geom_density(aes(x = rating), color="#9C6ADE", fill="#9C6ADE", bw=0.1) +
    facet_wrap(~ category_name) +
  labs(
    x = "Rating",
    y = "",
    title = "Distribution of Ratings by Category - Paid Only",
    caption = paste(
      "Source: Shopify App Store",
      "as of",
      format(now(), "%m/%d/%y")
    )
  ) +
  theme(
    axis.text.y = element_blank()
  )
ggsave("plots/dist_by_category_paid.png", distribution_by_category_paid, width = 11)

# Plot the distribution of ratings over time by category
dist_ratings_over_time_by_category <- reviews_data %>% 
  mutate(year = year(date)) %>% 
  filter(
    category_name == "FINDING AND ADDING PRODUCTS" | category_name == "PLACES TO SELL",
    monetization == "Paid"
  ) %>% 
  ggplot() +
    geom_density(aes(x = rating), color="#9C6ADE", fill="#9C6ADE", bw=.1) +
    facet_wrap(category_name ~ year) +
  labs(
    x = "Rating",
    y = "",
    title = "Distribution of Ratings Over Time by Category - Paid Only",
    caption = paste(
      "Source: Shopify App Store",
      "as of",
      format(now(), "%m/%d/%y")
    )
  ) +
  theme(
    axis.text.y = element_blank()
  )
ggsave("plots/dist_ratings_over_time_by_category.png", dist_ratings_over_time_by_category)

# Number of reviews per category, paid only
reviews_data %>% 
  filter(monetization == "Paid") %>% 
  group_by(category_name) %>% 
  summarize(
    num_reviews = n()
  )

# Most frequent reviews - Places to Sell
phrases_ranking <- key_phrases %>% 
  filter(
    neutral != TRUE,
    score > 0.75,
    phrase != "this app",
    phrase != "the app"
  ) %>% 
  group_by(phrase, positive) %>% 
  summarize(
    freq = n(),
    helpfulness = sum(helpful_count)
  ) %>% 
  arrange(desc(freq))

phrase_ranking_table_by_freq <- phrases_ranking %>% 
  rename(Text = phrase, Positive = positive, Frequency = freq, Helpfulness = helpfulness) %>%
  head(20) %>%
  tableGrob(theme = shopify_table_theme, rows = NULL) %>%
  grid.arrange()
ggsave("plots/phrase_ranking_table_by_freq.png", phrase_ranking_table_by_freq)

phrase_ranking_table_by_help <- phrases_ranking %>% 
  arrange(desc(helpfulness)) %>% 
  rename(Text = phrase, Positive = positive, Frequency = freq, Helpfulness = helpfulness) %>%
  head(20) %>%
  tableGrob(theme = shopify_table_theme, rows = NULL) %>%
  grid.arrange()
ggsave("plots/phrase_ranking_table_by_help.png", phrase_ranking_table_by_help)


# Top reviewers
reviews_data %>% 
  filter(reviewer_name != "") %>% 
  group_by(reviewer_name) %>% 
  summarize(
    count = n()
  ) %>% 
  arrange(desc(count))
