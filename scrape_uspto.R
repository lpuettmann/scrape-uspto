library(tidyverse)
library(rvest)
library(assertthat)
library(ggthemes)
library(readxl)

# Followed these instructions:
# http://blog.corynissen.com/2015/01/using-rvest-to-scrape-html-table.html

# Table 1
uspto_1 <- "https://www.uspto.gov/web/offices/ac/ido/oeip/taf/cbcby.htm" %>% 
  read_html() %>% 
  html_nodes(xpath = "/html/body/div[1]/div/div/center/table") %>% 
  html_table() 

# Table 2
uspto_2 <- "https://www.uspto.gov/web/offices/ac/ido/oeip/taf/cbcby.htm" %>% 
  read_html() %>% 
  html_nodes(xpath = "/html/body/div[2]/div/div/center/table") %>% 
  html_table()

raw <- uspto_1[[1]] %>% 
  as.tibble() %>% 
  rename(class = Class,
         class_title = `Class Title`) %>% 
  mutate(counts = "original") %>% 
  full_join(uspto_2[[1]] %>% 
              as.tibble() %>% 
              rename(class = Class,
                     class_title = `Class Title`) %>% 
              mutate(counts = "no_dupl"))


# Check that data adds up correctly ---------------------------------------
cln <- raw %>% 
  select(-Total) %>% 
  filter(class != "ALL") %>% 
  gather(yr, patents, -class, -class_title, -counts)

cl_stats <- cln %>% 
  group_by(class, class_title, counts) %>% 
  summarise(patents = sum(patents)) %>% 
  left_join(raw %>% 
              select(class, class_title, counts, expected = Total)) 
assert_that(all(cl_stats$patents == cl_stats$expected))

yr_stats <- cln %>% 
  group_by(yr, counts) %>% 
  summarise(patents = sum(patents)) %>% 
  left_join(raw %>% 
              select(-Total) %>% 
              filter(class == "ALL") %>% 
              select(-class, -class_title) %>% 
              gather(yr, expected, -counts))
assert_that(all(yr_stats$patents == yr_stats$expected))


# Clean up and provide patents with Hall-Jaffe-Trajtenberg (2001)
# categories.
df <- cln %>% 
  filter(yr != "PRE_1995") %>% 
  mutate(yr = as.numeric(yr),
         uspc = as.numeric(class),
         ctg = as.character(100 * ceiling(uspc / 100)),
         ctg = ifelse(is.na(ctg), "other", ctg)) %>% 
  left_join(read_xlsx("hjt_crosswalk_v1.xlsx")) %>% 
  mutate(hjt = case_when(
    substr(hjt_num, 1, 1) == "1" ~ "Chemical",
    substr(hjt_num, 1, 1) == "2" ~ "Computers & Communications",
    substr(hjt_num, 1, 1) == "3" ~ "Drugs & Medical",
    substr(hjt_num, 1, 1) == "4" ~ "Electrical & Electronic",
    substr(hjt_num, 1, 1) == "5" ~ "Mechanical",
    substr(hjt_num, 1, 1) == "6" ~ "Others"))

df %>% 
  filter(counts == "original") %>% 
  ggplot(aes(yr, patents, group = class_title)) +
  geom_line(alpha = 0.4) +
  facet_wrap(~ctg, scales = "free_y") +
  theme_tufte(base_family = "Helvetica")

df %>% 
  filter(counts == "original") %>% 
  ggplot(aes(yr, patents, group = class_title)) +
  geom_line(alpha = 0.4) +
  facet_wrap(~hjt, scales = "free_y", nrow = 2) +
  theme_tufte(base_family = "Helvetica")

ggsave("patents_hjt.jpg", width = 8, height = 4, dpi = 500)

# Compare the two different series they provide
cmp_tabs <- df %>% 
  group_by(yr, hjt, counts) %>% 
  summarise(patents = sum(patents)) %>% 
  spread(counts, patents) %>% 
  arrange(hjt, yr) %>% 
  mutate(t2_ratio = no_dupl / original)

ggplot(cmp_tabs, aes(original, no_dupl)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~hjt, scales = "free") +
  theme_tufte(base_family = "Helvetica") +
  geom_smooth(method = "lm", color = "#ca0020", fill = "#fddbc7", alpha = 0.8,
              size = 0.5) 

ggplot(cmp_tabs, aes(yr, t2_ratio)) +
  geom_hline(yintercept = 1, size = 0.3, color = "grey80") +
  geom_line() +
  geom_point() +
  facet_wrap(~hjt) +
  theme_tufte(base_family = "Helvetica") 

write_csv(df, "uspto_counts.csv")


