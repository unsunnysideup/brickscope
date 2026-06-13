#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: packages
#| echo: FALSE
#| message: FALSE

knitr::opts_chunk$set(   
  message = FALSE)   

library(tidyverse)
library(purrr)
library(rvest)

#
#
#
#| label: wrangling data

sets <- read.csv("sets.csv", TRUE)
themes <- read.csv("themes.csv", TRUE)

themes <- themes |>
    left_join(themes |> select(c(parent_id = id, parent_theme = name))) |>
    mutate(parent_theme = ifelse(is.na(parent_id), name, parent_theme)) |>
    rename(theme = name) |>
    select(
        !parent_id
    )

data <- left_join(sets, themes, by = join_by(theme_id == id)) |>
    select(!theme_id) |> 
    # Removed lego-merchandise
    filter(parent_theme != "Gear", parent_theme != "Books")

#
#
#
#| label: Top Ten Lego Themes with Most Sets Released

data |>
    group_by(theme) |>
    summarize(count = n()) |>
    mutate(theme = fct_reorder(theme, count)) |>
    arrange(desc(count)) |>
    slice(1:10) |>
    ggplot(aes(x = theme, y = count )) +
    geom_col(color = "#DA291C") +
    coord_flip() +
    labs(
        title = "Top Ten Lego Themes with Most Sets Released", 
        x = "Count",
        y = "Themes"
    ) + 
    theme_grey()

#
#
#
#| label: Top 10 Themes with Most Parts on Average per Set
data |>
    group_by(theme) |>
    summarize(avg_part = mean(num_parts)) |>
    mutate(theme = fct_reorder(theme, avg_part)) |>
    arrange(desc(avg_part)) |>
    slice(1:10) |>
    ggplot(aes(x = theme, y = avg_part )) +
    geom_col(color = "#DA291C") +
    coord_flip() +
    labs(
        title = "Top Ten Lego Themes with Most Sets Released", 
        x = "Count",
        y = "Themes"
    ) + 
    theme_grey()

#
#
#
#| label: Top Ten Parent Themes with Most Themes
data |>
    group_by(parent_theme) |>
    summarize(count = n()) |>
    mutate(parent_theme = fct_reorder(parent_theme, count)) |>
    arrange(desc(parent_theme)) |>
    slice(1:10) |>
    ggplot(aes(x = parent_theme, y = count )) +
    geom_col(color = "#DA291C") +
    coord_flip() +
    labs(
        title = "Top Ten Parent Themes with Most Themes", 
        x = "Count",
        y = "Parent Themes"
    ) + 
    theme_grey()

#
#
#
#| label: Lego Sets Released from 1949 - 2026

data |>
    group_by(year) |>
    summarize(count = n()) |>
    ggplot(aes(x = year, y = count )) +
    geom_line() +
    labs(
        title = "Lego Sets Released from 1949 - 2026", 
        x = "Year",
        y = "Count"
    ) + 
    theme_grey()

#
#
#
#| label: Lego Sets Released Per Theme (Filters)

data |>
    group_by(year, theme) |>
    filter(theme == "Star Wars") |>
    summarize(count = n()) |>
    ggplot(aes(x = year, y = count)) +
    geom_line() +
    labs(
        title = "Lego Sets Released Per Theme", 
        x = "Year",
        y = "Count"
    ) + 
    theme_grey()

#
#
#
#| label: webscraping brickset
set_urls <- paste0("https://brickset.com/sets/", data$set_num[394:404])

webdata <- tibble(url = set_urls)

webdata$rating <- map_dbl(webdata$url, function(r) {
    tryCatch({
        read_html(r) |>
        html_element("span.rating") |>
        html_text() |> 
        str_extract("\\d+\\.?\\d*") |>
        as.numeric()
    }, error = function(e) {
        NA_real_
    })
})

webdata$price <- map_dbl(webdata$url, function(p) {
    tryCatch({
        vals <- read_html_live(p) |>
            html_elements(".featurebox.bricklink a.plain") |>
            html_text(trim = TRUE) |>
            str_extract("\\d+\\.?\\d*") |>
            na.omit() 

        if (!is.na(vals[1])) {
            print(as.numeric(vals[1]))
            as.numeric(vals[1])
        } else {
            NA_real_
        }
    }, error = function(e) {
        NA_real_
    })
})

for (url in set_urls) {
    Sys.sleep(3)
    vals <-  read_html_live(url) |>
     html_elements(".featurebox.bricklink a.plain") |>
            html_text(trim = TRUE) |>
            str_extract("\\d+\\.?\\d*") |>
            na.omit()
    if (!is.na(vals[1])) {
            print(as.numeric(vals[1]))
            as.numeric(vals[1])
        } else {
            print(NA_real_)
        }
}

page <- read_html_live("https://brickset.com/sets/10342-1")


t <- page |> html_elements(".featurebox.bricklink a.plain") |>
            html_text(trim = TRUE) |>
            str_extract("\\d+\\.?\\d*") |>
            na.omit()
 if (!is.na(t[1])) {
            as.numeric(t[1])
        } else {
            NA_real_
        }


page |> html_element(".featurebox.bricklink a.plain") 




#
#
#
