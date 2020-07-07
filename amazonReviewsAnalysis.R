library(rvest)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(textdata)

url <- "https://www.amazon.com/product-reviews/"
Product_Code <- c("1501175564")
Product_Code1 <- c("1476751447")
url2 <- "/ref=cm_cr_getr_d_paging_btm_2?ie=UTF&&reviewerType=all_reviews&sortBy=recent&pageNumber="
Page_Number <- 1:100
List<-NULL
List1<-NULL
for(i in Page_Number) {
  link<-read_html(paste0(url, Product_Code, url2, i))
  text <- cbind(link %>% html_nodes(".review-text") %>% html_text())
  List<-rbind(text, List)
  Reviews <- as.vector(List)
}

for(i in Page_Number) {
  link1<-read_html(paste0(url, Product_Code1, url2, i))
  text1 <- cbind(link1 %>% html_nodes(".review-text") %>% html_text())
  List1<-rbind(text1, List1)
  Reviews1 <- as.vector(List1)
}

data(stop_words)
tidy_Reviews <- data_frame(text = Reviews) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_Reviews1 <- data_frame(text = Reviews1) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_Reviews
tidy_Reviews1

tidy_Reviews %>% 
  count(word, sort=TRUE) %>%
  filter(n>100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()

tidy_Reviews1 %>% 
  count(word, sort=TRUE) %>%
  filter(n>100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()
get_sentiments("afinn")

afinn <- tidy_Reviews %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index=row_number() %/% 50) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method="AFINN") %>%
  mutate(Product_Code)

afinn1 <- tidy_Reviews1 %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index=row_number() %/% 50) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method="AFINN") %>%
  mutate(Product_Code1)

afinn
afinn1

get_sentiments("nrc")

nrc<-tidy_Reviews %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment %in% c("positive",
                                       "negative"))) %>%
  mutate(method="NRC") %>%
  count(method, index=row_number() %/% 50, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(product = Product_Code)

nrc1<-tidy_Reviews1 %>%
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment %in% c("positive",
                                       "negative"))) %>%
  mutate(method="NRC") %>%
  count(method, index=row_number() %/% 50, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(product = Product_Code1)

nrc
nrc1

get_sentiments("bing")

bing<-tidy_Reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(index=row_number() %/% 50, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(method="BING") %>%
  mutate(product = Product_Code)

bing1<-tidy_Reviews1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(index=row_number() %/% 50, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(method="BING") %>%
  mutate(product = Product_Code1)

bing
bing1

get_sentiments("loughran")

loughran <- tidy_Reviews %>%
  inner_join(get_sentiments("loughran") %>% 
               filter(sentiment %in% c("positive",
                                       "negative"))) %>%
  mutate(method="loughran") %>%
  count(method, index=row_number() %/% 50, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(product = Product_Code)

loughran1 <- tidy_Reviews1 %>%
  inner_join(get_sentiments("loughran") %>% 
               filter(sentiment %in% c("positive",
                                       "negative"))) %>%
  mutate(method="loughran") %>%
  count(method, index=row_number() %/% 50, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(product = Product_Code1)

loughran
loughran1

bind_rows(loughran, afinn, bing, nrc) %>% 
  ggplot(aes(index, sentiment, fill = method)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 2, scales = "free_y")

bind_rows(loughran1, afinn1, bing1, nrc1) %>% 
  ggplot(aes(index, sentiment, fill = method)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 2, scales = "free_y")


# TF-IDF analysis
tidy_Reviews <- tidy_Reviews %>%
  mutate(name = Product_Code)

tidy_Reviews1 <- tidy_Reviews1 %>%
  mutate(name = Product_Code1)

tidy_Reviews_Final <- rbind(tidy_Reviews, tidy_Reviews1)
tidy_Reviews_Final

review_words <- tidy_Reviews_Final %>%
  count(name, word, sort = TRUE) %>%
  ungroup()

total_reviews_words <- review_words %>%
  group_by(name) %>%
  summarise(total = sum(n))

review_words

total_reviews_words

final <- left_join(total_reviews_words, review_words)
final

final <- final %>%
  bind_tf_idf(word, name, n)

final %>%
  select(-total) %>%
  arrange(desc(tf_idf))

plot_review <- final %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels=rev(unique(word))))

plot_review %>%
  top_n(20) %>%
  ggplot(aes(word, tf_idf, fill=name)) +
  geom_col() +
  labs(x=NULL, y="tf-idf") +
  coord_flip()

plot_review %>% 
  group_by(name) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=name)) +
  geom_col(show.legend = FALSE)+
  labs(x=NULL, y="tf-idf") +
  facet_wrap(~name, ncol=2, scales = "free") +
  coord_flip()



