---
title: "Tidy Tuesday"
subtitle: "Week 8 "
author: "Cristian T"
date: last-modified
format: 
   html:
     df-print: paged
     embed-resources: true
editor: 
  markdown: 
    wrap: 72
---

```{r}
# Option 1: tidytuesdayR package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2024-02-20')
## OR
#tuesdata <- tidytuesdayR::tt_load(2024, week = 8)

isc_grants <- tuesdata$isc_grants




```

```{r}
library(tidyverse)
library(janitor)
library(rlang)
library(rvest)
library(here)
library(dplyr)
library(tidytext)

working_dir <- here::here("data", "2024", "2024-02-20")

all_grants_url <- "https://www.r-consortium.org/all-projects/awarded-projects"
urls <-
  all_grants_url |> 
  rvest::read_html() |> 
  rvest::html_node(".main-content") |> 
  rvest::html_nodes("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_subset(stringr::fixed("/projects/awarded-projects/")) |> 
  stringr::str_remove("#.+$") |> 
  unique() |> 
  rvest::url_absolute(all_grants_url)

get_grant_data <- function(url) {
  year <- as.integer(stringr::str_extract(url, "\\d{4}"))
  group <- as.integer(stringr::str_extract(url, "\\d$"))
  project_description_html <- 
    rvest::read_html(url) |> 
    rvest::html_nodes(".project-description") 
  titles <- 
    project_description_html |> 
    rvest::html_nodes("h3") |> 
    rvest::html_text2()
  other_data <- 
    project_description_html |> 
    purrr::map(parse_project_description) |> 
    purrr::list_rbind()
  
  tibble::tibble(
    year = year,
    group = group,
    title = titles,
    other_data
  )
}

parse_project_description <- function(project_description_html) {
  pieces <- project_description_html |> 
    rvest::html_nodes("p") |> 
    rvest::html_text2() |> 
    stringr::str_split(":\n")
  
  pieces <- purrr::reduce(
    pieces,
    \(x, y) {
      if (!rlang::is_named(x) && length(x) == 2) {
        x <- rlang::set_names(x[[2]], x[[1]])
      }
      if (length(y) == 2) {
        y <- rlang::set_names(y[[2]], y[[1]])
      } else {
        x[[length(x)]] <- paste(x[[length(x)]], y, sep = "\n")
        y <- NULL
      }
      return(c(x, y))
    }
  )
  
  pieces |> 
    tibble::enframe() |> 
    tidyr::pivot_wider(names_from = name, values_from = value)
}

isc_grants <- 
  urls |> 
  purrr::map(get_grant_data) |> 
  purrr::list_rbind() |> 
  janitor::clean_names() |> 
  dplyr::mutate(
    funded = stringr::str_remove(funded, "\\$") |> 
      stringr::str_remove(",") |> 
      as.integer(),
    website = dplyr::na_if(website, "")
  )

# Set the target directory
target_dir <- "~/Downloads/Notre Dame"

# Create the target directory if it doesn't exist
dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

# Write the CSV files to the target directory
readr::write_csv(isc_grants, fs::path(target_dir, "isc_grants.csv"))

isc_grants <- read.csv("/Users/cristianthirteen/Downloads/Notre Dame/isc_grants.csv")

glimpse(isc_grants)
```

explore and visualize the data to gain insights

```{r}
summary(isc_grants)
str(isc_grants)

```

visualize the distribution of grants over the years

```{r}
# Create a bar plot
ggplot(isc_grants, aes(x = factor(year), y = funded)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Grant Distribution Over Years",
       x = "Year",
       y = "Funded Amount") +
  theme_minimal()

```



```{r}
# Summary statistics
summary_stats <- summary(isc_grants$funded)

# Create a histogram
ggplot(isc_grants, aes(x = funded)) +
  geom_histogram(binwidth = 5000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Funded Amounts",
       x = "Funded Amount",
       y = "Frequency") +
  theme_minimal()


summary_stats
```

# Text Analysis

```{r}
suppressMessages(library(mdsr))
suppressMessages(library(tidytext))

tidy_grants1 <-  isc_grants %>%
  unnest_tokens(word, summary) %>%
  count(word, sort = TRUE) %>% anti_join(stop_words)

# Filter for the top 20 words
top_words <- tidy_grants1 %>% top_n(10)

# Create a bar plot
ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 20 Words in Grant Summaries",
       x = "Words",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```

# top words by year and funding

```{r}
library(ggrepel)

tidy_grants <- isc_grants %>% unnest_tokens(word, summary) %>% count(year, funded, word, sort = TRUE) %>% anti_join(stop_words)

# Top 20 words
top_20_words_fund <- head(tidy_grants, 20)

# Create a scatter
ggplot(top_20_words_fund, aes(x = funded, y = n, label = word, color = factor(year))) +
  geom_point() +
  geom_text_repel(box.padding = 0.5) +
  labs(title = "Scatter Plot of Funding vs. Word Frequency (Top 20 Words)",
       x = "Funding Amount",
       y = "Word Frequency",
       color = "Year") +
  scale_color_discrete(name = "Year") +
  theme_minimal()
```


```{r}
# Top 50 words
top_50_words_fund <- head(tidy_grants, 50)

# Create a scatter
ggplot(top_50_words_fund, aes(x = funded, y = n, label = word, color = factor(year))) +
  geom_point() +
  geom_text_repel(box.padding = 0.5) +
  labs(title = "Scatter Plot of Funding vs. Word Frequency (Top 20 Words)",
       x = "Funding Amount",
       y = "Word Frequency",
       color = "Year") +
  scale_color_discrete(name = "Year") +
  theme_minimal()
```

```{r}

```
