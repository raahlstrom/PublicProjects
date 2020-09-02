# LOAD PACKAGES AND DATA --------------------------------------------------

require(tidyverse)
require(tidytext)
require(rvest)
require(stringr)
require(udpipe)


udmodel_english <- udpipe_load_model(file = 'C:/Users/ahlst/OneDrive/Desktop/niche_report_skeleton/english-ewt-ud-2.4-190531.udpipe')

# SCRAPE FUNCTION ---------------------------------------------------------

scrapeConvention <- function(url) {
  convention_scrape <- read_html(url)
  convention_scrape_vct <- convention_scrape %>%
    html_node("#transcription") %>%
    html_nodes("p") %>%
    html_text() %>%
    unlist()

  tibble(scrape_content_raw = convention_scrape_vct) %>%
    mutate(
      content = scrape_content_raw %>% str_extract(regex("(?<=:\\d\\d\\) ).*")),
      speaker = scrape_content_raw %>% str_extract(regex(".*(?=\\(\\d\\d:)")) %>%
        str_remove_all(":\\s|:")
    )
}


# SCRAPE CONVENTION TRANSCRIPTS -------------------------------------------

DEMnc_urls <- c(
  "https://www.rev.com/blog/transcripts/democratic-national-convention-dnc-night-1-transcript",
  "https://www.rev.com/blog/transcripts/democratic-national-convention-dnc-2020-night-2-transcript",
  "https://www.rev.com/blog/transcripts/democratic-national-convention-dnc-night-3-transcript",
  "https://www.rev.com/blog/transcripts/2020-democratic-national-convention-dnc-night-4-transcript"
)

REPnc_urls <- c(
  "https://www.rev.com/blog/transcripts/2020-republican-national-convention-rnc-night-1-transcript",
  "https://www.rev.com/blog/transcripts/2020-republican-national-convention-rnc-night-2-transcript",
  "https://www.rev.com/blog/transcripts/2020-republican-national-convention-rnc-night-3-transcript"
)


DEMnc_scrape <- DEMnc_urls %>% 
  map_dfr(scrapeConvention) %>% 
  bind_cols("party" = "democrat")

REPnc_scrape <- REPnc_urls %>% 
  map_dfr(scrapeConvention) %>% 
  bind_cols("party" = "republican") 

convention_scrape <- bind_rows(DEMnc_scrape,REPnc_scrape)

annotated_convention_scrape <- udpipe_annotate(udmodel_english,
                                          convention_scrape$content,
                  doc_id = convention_scrape$content,
                  trace = TRUE) %>% 
  as_tibble()

convention_scrape_token <- annotated_convention_scrape %>% 
  left_join(convention_scrape, by = c("doc_id" = "content")) %>% 
  select(sentence,
         speaker,
         token,
         lemma,
         upos,
         party) %>% 
  mutate(education_ref = sentence %>% str_detect("[Cc]ollege|[Uu]niversit|[Hh]igher [Ee]d"))

convention_scrape_speaker <- convention_scrape %>% 
  select(content, speaker, party) %>% 
  mutate(education_ref = content %>% str_detect("[Cc]ollege|[Uu]niversit|[Hh]igher [Ee]d"))


# TOTAL HIGHERED REFERENCES -----------------------------------------------

color_1 <- c("#7A99AC","#00A9E0")

convention_scrape_speaker %>% 
  group_by(education_ref) %>% 
  summarize(count = n(), percent = n()/nrow(convention_scrape_speaker)) %>% 
  mutate(lab_ypos = cumsum(percent) - 0.5*percent) %>%  
  arrange((percent)) %>% 
  ggplot(aes(x = "", y = percent, fill = if_else(education_ref, "Yes", "No"))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab_ypos, label = count), color = "white", size = 7) +
  scale_fill_manual(values = color_1) +
  theme_void() +
  theme(legend.title = element_blank())


# HIGHERED REFERENCES BY PARTY --------------------------------------------

color_2 <- c("#000080","#CD0000")

convention_scrape_speaker %>% 
  filter(education_ref == TRUE) %>%
  group_by(party) %>% 
  summarize(count = n(), percent = n()/nrow(convention_scrape_speaker)) %>%
  arrange((percent)) %>% 
  mutate(lab_ypos = cumsum(percent) - (0.5*percent)) %>%  
    ggplot(aes(x = "", y = percent, fill = party)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text(aes(y = lab_ypos, label = count), color = "white", size = 7)+
    scale_fill_manual(values = color_2) +
    theme_void()


# HIGHERED WORD ASSOC. BY PARTY -------------------------------------------

convention_scrape_token %>% 
  filter(
    education_ref == TRUE & 
      upos == "ADJ" |
      upos == "NOUN") %>% 
  count(party, lemma) %>%
  bind_tf_idf(lemma, party, n) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(lemma, levels = rev(unique(lemma)))) %>% 
  group_by(party) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(x = reorder(lemma, tf_idf), y = tf_idf, fill = party)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~party, ncol = 2, scales = "free") +
  coord_flip()

# HIGHERED REFERENCE BY PERSON --------------------------------------------

convention_scrape_speaker %>% 
  mutate(highere_ref_count = content %>% str_count("[Cc]ollege|[Uu]niversit|[Hh]igher [Ee]d")) %>% 
  group_by(party, speaker) %>% 
  summarize(count = sum(highere_ref_count)) %>%
  filter(count >= 1) %>% 
  ggplot(aes(x = count, y = reorder(speaker, count), fill = party)) +
  geom_bar(stat = "identity") +
  facet_wrap(~party, ncol = 2, scales = "free")

# HIGHERED WORD ASSOC. BY SPEAKER -----------------------------------------

# convention_scrape_speaker %>% 
#   mutate(highere_ref_count = content %>% str_count("[Cc]ollege|[Uu]niversit|[Hh]igher [Ee]d")) %>% 
#   group_by(party, speaker) %>% 
#   summarize(count = sum(highere_ref_count)) %>%
