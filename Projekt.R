#' ---
#' title: "Projekt zaliczeniowy"
#' author: "Patrycja Rutkowska, Anastasiya Albatava, Szymon Kiczyński"
#' date: ""
#' output:
#'    html_document:
#'      df_print: paged
#'      theme: cerulean
#'      highlight: default
#'      toc: yes
#'      toc_depth: 3
#'      toc_float:
#'         collapsed: false
#'         smooth_scroll: true
#'      code_fold: show
#' ---


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)


#' # Wymagane pakiety
# Wymagane pakiety ----
  library(tm)
  library(tidytext)
  library(stringr)
  library(wordcloud)
  library(RColorBrewer)
  library(ggplot2)
  library(SnowballC)
  library(SentimentAnalysis)
  library(ggthemes)
  library(tidyverse)
  library(stringi)

  
#' # 0. Funkcja do przetwarzania tekstu z apostrofami, stemmingiem i stemCompletion
  # 0. Funkcja do przetwarzania tekstu z apostrofami, stemmingiem i stemCompletion ----
process_text <- function(file_path) {
  # Wczytanie tekstu z pliku
  text <- tolower(readLines(file_path, encoding = "UTF-8"))
  # Zamiana wszystkich rodzajów apostrofów na klasyczny '
  text <- gsub("[\u2019\u2018\u0060\u00B4]", "'", text)
  # Usunięcie cyfr
  text <- removeNumbers(text)
  # Podział tekstu na słowa
  words <- unlist(strsplit(text, "\\s+"))
  # Usunięcie pustych elementów
  words <- words[words != ""]
  # Usunięcie słów zawierających apostrof 
  words <- words[!str_detect(words, "'")]
  # Usunięcie interpunkcji
  words <- str_replace_all(words, "[[:punct:]]", "")
  words <- words[words != ""]
  # Usunięcie spacji z początka i końca słów
  words <- str_trim(words)
  

  # stopwords z pakietu tidytext i jednolite apostrofy
  tidy_stopwords <- tolower(stop_words$word)
  tidy_stopwords <- gsub("[\u2019\u2018\u0060\u00B4]", "'", tidy_stopwords)
  
  # stopwords z pakietu tm i jednolite apostrofy
  tm_stopwords <- tolower(stopwords("en"))
  tm_stopwords <- gsub("[\u2019\u2018\u0060\u00B4]", "'", tm_stopwords)
  
  words <- words[!(words %in% tidy_stopwords)]
  words <- words[!(words %in% tm_stopwords)]
  
  # Stemming + stem completion
  stemmed_doc <- stemDocument(words)
  completed_doc <- stemCompletion(stemmed_doc, dictionary=words, type="prevalent")
  
  # Usunięcie pustych elementów
  completed_doc <- completed_doc[completed_doc != ""]
  
  # Zwróć wynik końcowy (po stemCompletion)
  return(completed_doc)
}


#' # 0. Funkcja do obliczania częstości występowania słów
  # 0. Funkcja do obliczania częstości występowania słów ----
word_frequency <- function(words) {
  freq <- table(words)
  freq_df <- data.frame(word = names(freq), freq = as.numeric(freq))
  freq_df <- freq_df[order(-freq_df$freq), ]
  return(freq_df)
}


#' # 0. Funkcja do tworzenia chmury słów
  # 0. Funkcja do tworzenia chmury słów ----
plot_wordcloud <- function(freq_df, title, color_palette = "Dark2") {
  wordcloud(words = freq_df$word, freq = freq_df$freq, min.freq = 100,
            colors = brewer.pal(8, color_palette))
  title(title)
}


#' # ANALIZA TEXT MINING jednego pliku csv 
  # ANALIZA TEXT MINING jednego pliku csv ----


# Wczytanie i przetworzenie tekstu
# Ustaw Working Directory!
words <- read.csv("clean_chatgpt_reviews.csv", stringsAsFactors = FALSE)

# Tokenizowanie kolumny 'content' na słowa w kolumnie 'word'
tidy_reviews <- words %>%
  unnest_tokens(word, content) 

# Usunięcie liczb (jeśli występują jako osobne tokeny)
tidy_reviews <- tidy_reviews %>%
  filter(!str_detect(word, "^[0-9]+$"))


# Usunięcie standardowych stopwords
data("stop_words")
tidy_reviews <- tidy_reviews %>%
  anti_join(stop_words, by = "word")

# Usunięcie niestandardowych stopwords
custom_stopwords <- tibble(word = c("$", "inne_slowo")) 
tidy_reviews <- tidy_reviews %>%
  anti_join(custom_stopwords, by = "word")

# Stemming 
tidy_reviews <- tidy_reviews %>%
mutate(word_stem = wordStem(word, language="english")) 


# Częstość słów
freq_df <- tidy_reviews %>%
  count(word_stem, sort = TRUE) %>% 
  rename(freq = n)

# Chmura słów
plot_wordcloud(freq_df, "Chmura słów", "Dark2")


# Wyświetl top 10
print(head(freq_df, 10))



#' # Analiza sentymentu słowniki CSV
  # Analiza sentymentu słowniki CSV ----

#' # Wczytaj słowniki z plików csv
  # Wczytaj słowniki z plików csv ----
afinn <- read.csv("afinn.csv", stringsAsFactors = FALSE)
bing <- read.csv("bing.csv", stringsAsFactors = FALSE)
loughran <- read.csv("loughran.csv", stringsAsFactors = FALSE)
nrc <- read.csv("nrc.csv", stringsAsFactors = FALSE)


#' # Analiza sentymentu przy użyciu słownika Loughran
  # Analiza sentymentu przy użyciu słownika Loughran ----

# Użycie inner_join() 
sentiment_review_loughran <- tidy_reviews %>% 
  inner_join(loughran, by = c("word_stem" = "word"), relationship = "many-to-many") 

# Zliczanie sentymentu
sentiment_counts_loughran <- sentiment_review_loughran %>%
  count(sentiment, sort = TRUE)
print(sentiment_counts_loughran)

# Zliczanie słów dla sentymentu 
word_sentiment_counts_loughran <- sentiment_review_loughran %>%
  count(word_stem, sentiment, sort = TRUE)
print(head(word_sentiment_counts_loughran))

# Filtrowanie analizy sentymentu i pozostawienie tylko słów o sentymencie pozytywnym lub negatywnym
word_counts_plot_loughran <- sentiment_review_loughran %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(word_stem, sentiment) %>%
  group_by(sentiment) %>%
  top_n(15, n) %>% # Weź np. top 15
  ungroup() %>%
  mutate(word2 = reorder_within(word_stem, n, sentiment)) # Lepsze porządkowanie dla facet_wrap

# Wizualizacja sentymentu
ggplot(word_counts_plot_loughran, aes(x = word2, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") + # Użyj free_y
  coord_flip() +
  scale_x_reordered() + # Dla poprawnego wyświetlania etykiet z reorder_within
  labs(x = "Słowa", y = "Liczba", title = "Najczęstsze słowa wg sentymentu (Loughran)") +
  theme_gdocs() +
  scale_fill_manual(values = c("negative" = "firebrick", "positive" = "darkolivegreen4")) 


#' # Analiza sentymentu przy użyciu słownika NRC
  # Analiza sentymentu przy użyciu słownika NRC ----

# Użycie inner_join() 
sentiment_review_nrc <- tidy_reviews %>% 
  inner_join(nrc, by = c("word_stem" = "word"), relationship = "many-to-many") 

# Zliczanie sentymentu 
sentiment_counts_nrc<- sentiment_review_nrc %>%
  count(sentiment, sort = TRUE)
print(sentiment_counts_nrc)

# Zliczanie słów dla sentymentu 
word_sentiment_counts_nrc <- sentiment_review_nrc %>%
  count(word_stem, sentiment, sort = TRUE)
print(head(word_sentiment_counts_nrc))

# Filtrowanie analizy sentymentu i pozostawienie tylko słów o sentymencie pozytywnym lub negatywnym
word_counts_plot_nrc <- sentiment_review_nrc %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(word_stem, sentiment) %>%
  group_by(sentiment) %>%
  top_n(15, n) %>% # Weź np. top 15
  ungroup() %>%
  mutate(word2 = reorder_within(word_stem, n, sentiment)) # Lepsze porządkowanie dla facet_wrap

# Wizualizacja sentymentu
ggplot(word_counts_plot_nrc, aes(x = word2, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") + 
  coord_flip() +
  scale_x_reordered() + # Dla poprawnego wyświetlania etykiet z reorder_within
  labs(x = "Słowa", y = "Liczba", title = "Najczęstsze słowa wg sentymentu (NRC)") +
  theme_gdocs() +
  scale_fill_manual(values = c("negative" = "firebrick", "positive" = "darkolivegreen4")) 




#' # Analiza sentymentu przy użyciu słownika Bing
  # Analiza sentymentu przy użyciu słownika Bing ----

# Użycie inner_join() 
sentiment_review_bing <- tidy_reviews %>% 
  inner_join(bing, by = c("word_stem" = "word"), relationship = "many-to-many") 

# Zliczanie sentymentu 
sentiment_counts_bing <- sentiment_review_bing %>%
  count(sentiment, sort = TRUE)
print(sentiment_counts_bing)

# Zliczanie słów dla sentymentu 
word_sentiment_counts_bing <- sentiment_review_bing %>%
  count(word_stem, sentiment, sort = TRUE)
print(head(word_sentiment_counts_bing))

# Filtrowanie analizy sentymentu i pozostawienie tylko słów o sentymencie pozytywnym lub negatywnym
word_counts_plot_bing <- sentiment_review_bing %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(word_stem, sentiment) %>%
  group_by(sentiment) %>%
  top_n(15, n) %>% 
  ungroup() %>%
  mutate(word2 = reorder_within(word_stem, n, sentiment)) 

# Wizualizacja sentymentu
ggplot(word_counts_plot_bing, aes(x = word2, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") + 
  coord_flip() +
  scale_x_reordered() + # Dla poprawnego wyświetlania etykiet z reorder_within
  labs(x = "Słowa", y = "Liczba", title = "Najczęstsze słowa wg sentymentu (Bing)") +
  theme_gdocs() +
  scale_fill_manual(values = c("negative" = "firebrick", "positive" = "darkolivegreen4")) 



#' # Analiza sentymentu przy użyciu słownika Afinn
  # Analiza sentymentu przy użyciu słownika Afinn ----


# Użycie inner_join() 
sentiment_review_afinn <- tidy_reviews %>% 
  inner_join(afinn, by = c("word_stem" = "word"), relationship = "many-to-many") %>%
  # Tworzymy kolumny 'sentiment' na podstawie wartości 'value' z Afinn
  # Przypisujemy "positive" dla value > 0 i "negative" dla value < 0
  mutate(sentiment = case_when(
    value > 0 ~ "positive",
    value < 0 ~ "negative",
    TRUE ~ NA_character_ # Słowa z wartością 0 lub brakiem wartości nie będą miały sentymentu
  )) %>%
  # Usuwamy wiersze, dla których nie udało się przypisać sentymentu
  filter(!is.na(sentiment))

# Zliczanie sentymentu
sentiment_counts_afinn <- sentiment_review_afinn %>%
  count(sentiment, sort = TRUE) 
print(sentiment_counts_afinn)

# Zliczanie słów dla sentymentu 
word_sentiment_counts_afinn <- sentiment_review_afinn %>%
  
  count(word_stem, sentiment, sort = TRUE) 
print(head(word_sentiment_counts_afinn))

# Filtrowanie analizy sentymentu i pozostawienie tylko słów o sentymencie pozytywnym lub negatywnym
word_counts_plot_afinn <- sentiment_review_afinn %>%
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(word_stem, sentiment) %>% 
  group_by(sentiment) %>%
  top_n(15, n) %>%
  ungroup() %>%
  mutate(word2 = reorder_within(word_stem, n, sentiment))

# Wizualizacja sentymentu
ggplot(word_counts_plot_afinn, aes(x = word2, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") + 
  coord_flip() +
  scale_x_reordered() +
  labs(x = "Słowa", y = "Liczba", title = "Najczęstsze słowa wg sentymentu (Afinn)") +
  theme_gdocs() +
  
  # Upewnienie się, że kolory pasują do utworzonych kategorii sentymentu ("positive", "negative")
  scale_fill_manual(values = c("negative" = "firebrick", "positive" = "darkolivegreen4"))



#' # Analiza sentymentu w czasie o ustalonej długości linii
# Analiza sentymentu w czasie o ustalonej długości linii ----



# Połączenie wszystkich słów words w jeden ciąg znaków
full_text <- paste(words, collapse = " ")


# Funkcja do dzielenia tekstu na segmenty o określonej długości
split_text_into_chunks <- function(text, chunk_size) {
  start_positions <- seq(1, nchar(text), by = chunk_size)
  chunks <- substring(text, start_positions, start_positions + chunk_size - 1)
  return(chunks)
}


# Podzielenie tekstu na segmenty
#
# ustaw min_lentgh jako jednolitą długość jednego segmentu
set_length <- 1000
split_text_into_chunks <- function(text, chunk_size) {
  stringi::stri_sub(
    text,
    seq(1, stri_length(text), by = chunk_size),
    length = chunk_size
  )
}


# Wyświetlenie wynikowych segmentów
# print(text_chunks)


#' # Analiza sentymentu przy użyciu pakietu SentimentAnalysis
# Analiza sentymentu przy użyciu pakietu SentimentAnalysis ----
sentiment <- analyzeSentiment(stringi::stri_sub(
  full_text, 
  seq(1, stringi::stri_length(full_text), by = 5000),
  length = 5000
))



#' # Słownik GI (General Inquirer)
### Słownik GI (General Inquirer) ----
#
# Słownik ogólnego zastosowania
# zawiera listę słów pozytywnych i negatywnych
# zgodnych z psychologicznym słownikiem harwardzkim Harvard IV-4
# DictionaryGI


# Wczytaj słownik GI
# data(DictionaryGI)
# summary(DictionaryGI)


# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem GI
sentimentGI <- convertToDirection(sentiment$SentimentGI)


# Wykres skumulowanego sentymentu kierunkowego
#plot(sentimentGI)


# Ten sam wykres w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_GI <- data.frame(index = seq_along(sentimentGI), value = sentimentGI, Dictionary = "GI")

# Usunięcie wierszy, które zawierają NA
df_GI <- na.omit(df_GI)

ggplot(df_GI, aes(x = value)) +
  geom_bar(fill = "green", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (GI)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()




#' # Słownik HE (Henry’s Financial dictionary)
### Słownik HE (Henry’s Financial dictionary) ----
#
# zawiera listę słów pozytywnych i negatywnych
# zgodnych z finansowym słownikiem "Henry 2008"
# pierwszy, jaki powstał w wyniku analizy komunikatów prasowych 
# dotyczących zysków w branży telekomunikacyjnej i usług IT
# DictionaryHE


# Wczytaj słownik HE
# data(DictionaryHE)
# summary(DictionaryHE)


# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem HE
sentimentHE <- convertToDirection(sentiment$SentimentHE)


# Wykres skumulowanego sentymentu kierunkowego
# plot(sentimentHE)


# Ten sam wykres w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_HE <- data.frame(index = seq_along(sentimentHE), value = sentimentHE, Dictionary = "HE")

# Usunięcie wierszy, które zawierają NA
df_HE <- na.omit(df_HE)

ggplot(df_HE, aes(x = value)) +
  geom_bar(fill = "blue", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (HE)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()




#' # Słownik LM (Loughran-McDonald Financial dictionary)
### Słownik LM (Loughran-McDonald Financial dictionary) ----
#
# zawiera listę słów pozytywnych i negatywnych oraz związanych z niepewnością
# zgodnych z finansowym słownikiem Loughran-McDonald
# DictionaryLM


# Wczytaj słownik LM
# data(DictionaryLM)
# summary(DictionaryLM)


# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem LM
sentimentLM <- convertToDirection(sentiment$SentimentLM)


# Wykres skumulowanego sentymentu kierunkowego
# plot(sentimentLM)


# Ten sam wykres w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_LM <- data.frame(index = seq_along(sentimentLM), value = sentimentLM, Dictionary = "LM")

# Usunięcie wierszy, które zawierają NA
df_LM <- na.omit(df_LM)

ggplot(df_LM, aes(x = value)) +
  geom_bar(fill = "orange", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (LM)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()




#' # Słownik QDAP (Quantitative Discourse Analysis Package)
### Słownik QDAP (Quantitative Discourse Analysis Package) ----
#
# zawiera listę słów pozytywnych i negatywnych
# do analizy dyskursu


# Wczytaj słownik QDAP
qdap <- loadDictionaryQDAP()
# summary(qdap)


# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem QDAP
sentimentQDAP <- convertToDirection(sentiment$SentimentQDAP)


# Wykres skumulowanego sentymentu kierunkowego
# plot(sentimentQDAP)


# Ten sam wykres w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_QDAP <- data.frame(index = seq_along(sentimentQDAP), value = sentimentQDAP, Dictionary = "QDAP")

# Usunięcie wierszy, które zawierają NA
df_QDAP <- na.omit(df_QDAP)

ggplot(df_QDAP, aes(x = value)) +
  geom_bar(fill = "red", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (QDAP)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()



#' # Porównanie sentymentu na podstawie różnych słowników
# Porównanie sentymentu na podstawie różnych słowników ----

# Minimalistycznie
# plot(convertToDirection(sentiment$SentimentGI))
# plot(convertToDirection(sentiment$SentimentHE))
# plot(convertToDirection(sentiment$SentimentLM))
# plot(convertToDirection(sentiment$SentimentQDAP))


# Wizualnie lepsze w ggplot2
# Połączenie poszczególnych ramek w jedną ramkę
df_all <- bind_rows(df_GI, df_HE, df_LM, df_QDAP)

# Tworzenie wykresu z podziałem na słowniki
ggplot(df_all, aes(x = value, fill = Dictionary)) +
  geom_bar(alpha = 0.7) + 
  labs(title = "Skumulowany sentyment według słowników",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw() +
  facet_wrap(~Dictionary) +  # Podział na cztery osobne wykresy
  scale_fill_manual(values = c("GI" = "green", 
                               "HE" = "blue", 
                               "LM" = "orange",
                               "QDAP" = "red" ))




#' # Agregowanie sentymentu z różnych słowników w czasie
# Agregowanie sentymentu z różnych słowników w czasie ----


# Sprawdzenie ilości obserwacji
length(sentiment[,1])


# Utworzenie ramki danych
df_all <- data.frame(sentence=1:length(sentiment[,1]),
                     GI=sentiment$SentimentGI, 
                     HE=sentiment$SentimentHE, 
                     LM=sentiment$SentimentLM,
                     QDAP=sentiment$SentimentQDAP)



# USUNIĘCIE BRAKUJĄCYCH WARTOŚCI
# gdyż wartości NA (puste) uniemożliwiają generowanie wykresu w ggplot
#

# Usunięcie wartości NA
# Wybranie tylko niekompletnych przypadków:
puste <- df_all[!complete.cases(df_all), ]


# Usunięcie pustych obserwacji
# np. dla zmiennej QDAP (wszystkie mają NA)
df_all <- df_all[!is.na(df_all$QDAP), ]


# Sprawdzenie, czy wartości NA zostały usunięte
# wtedy puste2 ma 0 wierszy:
puste2 <- df_all[!complete.cases(df_all), ]
puste2




#' # Wykresy przedstawiające ewolucję sentymentu w czasie
# Wykresy przedstawiające ewolucję sentymentu w czasie ----



ggplot(df_all, aes(x=sentence, y=QDAP)) +
  geom_line(color="red", size=1) +
  geom_line(aes(x=sentence, y=GI), color="green", size=1) +
  geom_line(aes(x=sentence, y=HE), color="blue", size=1) +
  geom_line(aes(x=sentence, y=LM), color="orange", size=1) +
  labs(x = "Oś czasu zdań", y = "Sentyment") +
  theme_gdocs() + 
  ggtitle("Zmiana sentymentu w czasie")



ggplot(df_all, aes(x=sentence, y=QDAP)) + 
  geom_smooth(color="red") +
  geom_smooth(aes(x=sentence, y=GI), color="green") +
  geom_smooth(aes(x=sentence, y=HE), color="blue") +
  geom_smooth(aes(x=sentence, y=LM), color="orange") +
  labs(x = "Oś czasu zdań", y = "Sentyment") +
  theme_gdocs() + 
  ggtitle("Zmiana sentymentu w czasie")







