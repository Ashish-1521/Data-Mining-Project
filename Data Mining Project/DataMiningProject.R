## Loading required libraries
library(corrplot)
library("tidyr")
library("ggplot2")
library("bnlearn")
library("Rgraphviz")
library("kohonen")
library("dplyr")
library(reshape2)
library(confintr)

## Video Game Trends from 2010-2021
## Loading data
VG_sales_data <- read.csv("C:/Users/ASHISH/Downloads/vgds.csv")
## Analysing data
head(VG_sales_data)
names(VG_sales_data)
str(VG_sales_data)
## Converting to required format
VG_sales_data$genre <- as.factor(VG_sales_data$genre)
VG_sales_data$console <- as.factor(VG_sales_data$console)
dim(VG_sales_data)
VG_sales_data <- VG_sales_data[VG_sales_data$release_year>=2000,]
## Checking for null values
sum(is.na(VG_sales_data))

## Checking for duplicate values
duplicated_rows <- VG_sales_data[duplicated(VG_test[, c("title", "console")]) | duplicated(VG_test[, c("title", "console")], fromLast = TRUE), ]

# Print duplicated rows
View(duplicated_rows)

## Removing duplicated rows
VG_sales_data <- distinct(VG_sales_data, title, console, .keep_all = TRUE)

# Print the resulting dataframe
print(VG_sales_data)

genre_freq <- table(VG_sales_data$genre)
genre_freq_df <- as.data.frame(genre_freq)

VG_sales_data$release_year <- as.factor(VG_sales_data$release_year)

names(genre_freq_df) <- c("genre", "frequency")
ggplot(data = genre_freq_df, aes(x = genre, y = frequency, fill = genre)) +
  geom_bar(stat = "identity") +
  xlab("Genre") +
  ylab("Frequency") +
  ggtitle("Frequency of Video Game Genres")

# Create a bar plot for the number of games released each year
ggplot(data = VG_sales_data, aes(x = release_year)) +
  geom_bar() +
  # Set axis labels and plot title
  labs(x = "Release Year", y = "Number of Games", title = "Number of Games Released Each Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

## Average global sales by Release year
mean_sales_by_year <- VG_sales_data %>%
  group_by(release_year) %>%
  summarise(mean_global_sales = mean(global_sales))

# Plot the Avg global sales by release year
ggplot(data = mean_sales_by_year, aes(x = release_year, y = mean_global_sales)) +
  geom_line() +
  labs(x = "Release Year", y = "Mean Global Sales", title = "Mean Global Sales by Release Year") +
  theme_minimal()

ggplot(data = VG_sales_data, aes(x = release_year, y = global_sales, fill = factor(release_year))) +
  # Add a geom_bar to create a bar plot
  geom_bar(stat = "identity") +
  # Add facets by genre
  facet_wrap(~genre, ncol = 2) +
  # Adjust the theme for better readability
  theme_minimal() +
  # Set axis labels and plot title
  labs(x = "Release Year", y = "Total Global Sales", title = "Genre Sales by Year") +
  # Rotate x-axis labels for better readability
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

publisher_genre_count <- aggregate(genre ~ publisher, data = VG_sales_data, FUN = function(x) length(unique(x)))

# Sort the data frame by count of genre games developed
publisher_genre_count <- publisher_genre_count[order(publisher_genre_count$genre, decreasing = TRUE), ]
head(publisher_genre_count)
dim(publisher_genre_count)

publisher_genre_count <- VG_sales_data %>%
  group_by(publisher) %>%
  summarise(game_count = n(),
            genre_count = n_distinct(genre))

publisher_genre_count <- publisher_genre_count %>%
  arrange(desc(genre_count))

# Print the resulting dataframe
head(as.data.frame(publisher_genre_count),10)

publisher_summary <- VG_sales_data %>%
  group_by(publisher) %>%
  summarise(
    game_count = n(),                           # Count of games
    genre_count = n_distinct(genre),            # Count of genres
    total_global_sales = sum(global_sales)      # Total global sales
  )
publisher_summary <- publisher_summary %>%
  arrange(desc(genre_count))

# View the resulting dataframe
print(head(as.data.frame(publisher_summary),10))

publisher_counts <- table(VG_sales_data$publisher)
publisher_counts_df <- data.frame(publisher = names(publisher_counts), count = as.numeric(publisher_counts))

# Sort the data frame by count in descending order
publisher_counts_df <- publisher_counts_df[order(-publisher_counts_df$count), ]
head(publisher_counts_df)

publisher_stats <- VG_sales_data %>%
  group_by(publisher) %>%
  summarise(
    games_developed = n(),
    overall_sales = sum(global_sales)
  ) %>%
  arrange(desc(overall_sales))  # Arrange the data by overall sales in descending order

# View the resulting dataframe
head(publisher_stats)

top_publishers <- publisher_stats %>%
  head(15)

ggplot(data = top_publishers, aes(x = reorder(publisher, -overall_sales), y = overall_sales, label = games_developed)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(size = 3, hjust = -0.1) +  # Add text labels
  coord_flip() +  # Flip the axes to make the plot horizontal
  # Set axis labels and plot title
  labs(x = "Publisher", y = "Overall Sales", title = "Top 15 Publishers by Overall Sales") +
  theme_minimal()  # Set a minimal theme for better readability

publisher_stats_genre <- VG_sales_data %>%
  group_by(publisher) %>%
  summarise(
    games_developed = n(),
    overall_sales = sum(global_sales),
    Adventure = sum(genre=="Adventure"),
    Board_games = sum(genre=="Board Game"),
    Fighting = sum(genre=="Fighting"),
    Puzzle = sum(genre=="Puzzle"),
    Racing = sum(genre=="Racing"),
    Shooter = sum(genre=="Shooter"),
    Simulation = sum(genre=="Simulation"),
    Sports = sum(genre=="Sports")
  ) %>%
  arrange(desc(overall_sales))  # Arrange the data by overall sales in descending order

# View the resulting dataframe
head(as.data.frame(publisher_stats_genre),15)

top_15 <- as.data.frame(publisher_stats_genre %>% head(15))

top_15_melted <- melt(top_15, id.vars = c("publisher", "games_developed", "overall_sales"))

# Plot
ggplot(data = top_15_melted, aes(x = reorder(publisher, -overall_sales), y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(duplicated(publisher), "", games_developed), y = games_developed + 0.05), size = 3, hjust = -0.1) +  # Add one label at the top of each bar
  coord_flip() +  # Flip the axes to make the plot horizontal
  # Set axis labels and plot title
  labs(x = "Publisher", y = "Count", title = "Top 15 Publishers by Overall Sales and Genre Count") +
  theme_minimal()  # Set a minimal theme for better readability

(as.data.frame(top_15))

board_games <- VG_sales_data %>% filter(genre == "Board Game")

# Group the filtered data by publisher and calculate the total sales for each publisher
board_game_publishers <- board_games %>%
  group_by(publisher) %>%
  summarise(total_sales = sum(global_sales),no_board_games = sum(genre=="Board Game")) %>%
  arrange(desc(total_sales))  # Arrange the data by total sales in descending order

# Print the companies with the highest board game sales
head(as.data.frame(board_game_publishers))

publisher_stats_areas <- VG_sales_data %>%
  group_by(publisher) %>%
  summarise(
    games_developed = n(),
    overall_sales = sum(global_sales),
    asian_sales = sum(aisan_sales),
    na_sales = sum(north_american_sales),
    japan_sales = sum(japan_sales),
    eu_sales = sum(european_sales)
  ) %>%
  arrange(desc(overall_sales))  # Arrange the data by overall sales in descending order

# View the resulting dataframe
head(as.data.frame(publisher_stats_areas),15)

top_publishers <- publisher_stats_genre %>%
  head(15)  # Assuming publisher_stats_genre contains the dataset with publisher statistics

console_counts <- VG_sales_data %>%
  filter(publisher %in% top_publishers$publisher) %>%
  group_by(publisher, console) %>%
  summarise(game_count = n())

total_console_counts <- console_counts %>%
  group_by(console) %>%
  summarise(total_count = sum(game_count))

console_percentage <- console_counts %>%
  left_join(total_console_counts, by = "console") %>%
  mutate(percentage = (game_count / total_count) * 100)

# Print the resulting dataset
print(head(as.data.frame(console_percentage)))

print(as.data.frame(console_percentage[console_percentage$publisher=="Ubisoft",]))

dc_games <- console_percentage %>%
  filter(console == "DS")

ggplot(console_percentage, aes(x = publisher, y = percentage)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = console)) +
  facet_wrap(~ console, scales = "free_y", nrow = 3) +
  labs(x = "Publisher", y = "Percentage", title = "Percentage of Games by Publisher and Console Type") +
  scale_fill_manual(values = c("DS" = "skyblue", "PC" = "orange", "PS" = "red", "PS2" = "green", "PS3" = "blue", "PS4" = "purple", "PSN" = "cyan")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

VG_sales_data$release_year <- as.numeric(as.character(VG_sales_data$release_year))

platform_stats <- VG_sales_data %>%
  group_by(console) %>%
  summarize(total_sales = sum(global_sales, na.rm = TRUE),
            avg_critic_score = mean(critic_score, na.rm = TRUE),
            avg_user_score = mean(user_score, na.rm = TRUE),
            avg_vg_score = mean(vg_score,na.rm=TRUE))

sorted_platform_stats <- platform_stats %>%
  arrange(desc(total_sales))

as.data.frame(sorted_platform_stats)
ggplot(platform_stats,aes(x = console, y = total_sales)) + geom_col(fill="skyblue")

top_publishers <- VG_sales_data %>%
  group_by(publisher) %>%
  summarize(total_sales = sum(global_sales, na.rm = TRUE)) %>%
  arrange(desc(total_sales)) %>%
  head(10)

genre_sales <- VG_sales_data %>%
  group_by(genre) %>%
  summarise(
    total_na_sales = sum(north_american_sales),
    total_eu_sales = sum(european_sales),
    total_jp_sales = sum(japan_sales),
    total_as_sales = sum(aisan_sales)
  )

# Reshape the data from wide to long format for ggplot
genre_sales_long <- tidyr::pivot_longer(genre_sales, cols = c(total_na_sales, total_eu_sales, total_jp_sales, total_as_sales),
                                        names_to = "region", values_to = "sales")

# Plot
ggplot(genre_sales_long, aes(x = genre, y = sales, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Genre", y = "Total Sales", title = "Game Genre Sales per Region") +
  scale_fill_manual(values = c("#FFA500", "#1E90FF", "#32CD32", "#FF69B4"), 
                    labels = c("NA", "EU", "JP", "AS")) +  # Custom colors for regions
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

platform_sales <- VG_sales_data %>%
  group_by(console) %>%
  summarise(
    total_na_sales = sum(north_american_sales),
    total_eu_sales = sum(european_sales),
    total_jp_sales = sum(japan_sales),
    total_as_sales = sum(aisan_sales)
  )

# Reshape the data from wide to long format for ggplot
platform_sales_long <- tidyr::pivot_longer(platform_sales, cols = c(total_na_sales, total_eu_sales, total_jp_sales, total_as_sales),
                                           names_to = "region", values_to = "sales")
# Plot
ggplot(platform_sales_long, aes(x = console, y = sales, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Platform", y = "Total Sales", title = "Platform Sales per Region") +
  # scale_fill_manual(values = c("#FFA500", "#1E90FF", "#32CD32", "#FF69B4"), 
  #                   labels = c("NA", "EU", "JP", "AS")) +  # Custom colors for regions
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

platform_sales <- VG_sales_data %>%
  group_by(console) %>%
  summarise(
    total_sales = sum(north_american_sales + european_sales + japan_sales + aisan_sales)
  )

# Plot
ggplot(platform_sales, aes(x = console, y = total_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Platform", y = "Global Sales", title = "Global Platform Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

avg_sales_per_region_per_year <- VG_sales_data %>%
  group_by(release_year) %>%
  summarise(avg_asian_sales = mean(aisan_sales, na.rm = TRUE),
            avg_north_american_sales = mean(north_american_sales, na.rm = TRUE),
            avg_japan_sales = mean(japan_sales, na.rm = TRUE),
            avg_european_sales = mean(european_sales, na.rm = TRUE))

# Reshape data for plotting
avg_sales_per_region_per_year <- tidyr::gather(avg_sales_per_region_per_year, key = "region", value = "avg_sales", -release_year)

# Plot average sales for each region per year
ggplot(avg_sales_per_region_per_year, aes(x = release_year, y = avg_sales, color = region)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Average Sales", title = "Average Sales per Region per Year") +
  # scale_color_manual(values = c("aisan_sales" = "blue", "north_american_sales" = "red", "japan_sales" = "green", "european_sales" = "purple")) +
  theme_minimal()

colnames(VG_sales_data) <- tolower(colnames(VG_sales_data))

top_sales_games <- VG_sales_data %>%
  group_by(title) %>%
  summarise(total_sales = sum(global_sales)) %>%
  top_n(10, total_sales) %>%
  arrange(desc(total_sales))

# Plot critic score and global sales across different consoles for top 10 games
global_sales_plot <- VG_sales_data %>%
  semi_join(top_sales_games, by = "title") %>%
  group_by(title, console) %>%
  summarise(global_sales = global_sales,
            vg_score = vg_score,
            top_region = case_when(
              which.max(c(aisan_sales, european_sales, japan_sales, north_american_sales)) == 1 ~ "AS",
              which.max(c(aisan_sales, european_sales, japan_sales, north_american_sales)) == 2 ~ "EU",
              which.max(c(aisan_sales, european_sales, japan_sales, north_american_sales)) == 3 ~ "JP",
              which.max(c(aisan_sales, european_sales, japan_sales, north_american_sales)) == 4 ~ "NA"
            )) %>%
  ggplot(aes(x = title, y = global_sales, fill = console)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_point(aes(y = vg_score, color = top_region), size = 3, position = position_dodge(width = 0.9)) +
  labs(x = "Game title", y = "Global Sales", title = "Global Sales and vg Score for Top 10 Games on Different Consoles") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name = "Console") +
  scale_color_manual(name = "Top Sales Region", values = c("AS" = "red", "NA" = "blue", "JP" = "green", "EU" = "purple"))

print(global_sales_plot)

console_genre_freq <- table(VG_sales_data$console, VG_sales_data$genre)

# Convert to data frame
console_genre_freq_df <- as.data.frame(console_genre_freq)
names(console_genre_freq_df) <- c("console", "genre", "frequency")

# Plot
ggplot(data = console_genre_freq_df, aes(x = console, y = frequency, fill = genre)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Console") +
  ylab("Frequency") +
  ggtitle("Frequency of Video Game Genres by Console") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sony_2010 <- subset(VG_sales_data, publisher == "Sony Computer Entertainment" & release_year == 2010)

# Calculate the count of games in each genre
genre_counts <- table(sony_2010$genre)

# Convert to data frame
genre_counts_df <- as.data.frame(genre_counts)
names(genre_counts_df) <- c("genre", "count")

# Plot
ggplot(data = genre_counts_df, aes(x = genre, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  xlab("Genre") +
  ylab("Number of Games") +
  ggtitle("Number of Games in Each Genre Released by Sony in 2010") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



sales_2010 <- VG_sales_data[VG_sales_data$release_year == 2010, ]

# Determine the top publishing companies
top_publishers <- aggregate(global_sales ~ publisher, data = sales_2010, FUN = sum)
top_publishers <- top_publishers[order(-top_publishers$global_sales), "publisher"][1:5]

# Filter data for the top publishing companies in 2010
sales_top_publishers_2010 <- sales_2010[sales_2010$publisher %in% top_publishers, ]

# Calculate the number of games in each genre released by each of the top companies
games_by_genre <- aggregate(release_year ~ genre + publisher, data = sales_top_publishers_2010, FUN = length)

# Plot
ggplot(data = games_by_genre, aes(x = genre, y = release_year, fill = publisher)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Genre") +
  ylab("Number of Games Released") +
  ggtitle("Number of Games in Each Genre Released by Top Companies in 2010") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

