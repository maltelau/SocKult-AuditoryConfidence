





# TODO: spectral analysis 
# http://www.david-reitter.com/pub/xu2017spectral.pdf
# 1. get word frequencies from telephone corpus
# 2. calculate per-word entropy
# 3. compute power spectrum overlap





# also: think about what to do with
# https://github.com/zonination/perceptions/blob/master/README.md

library(tidyverse)
library(purrr)
library(lubridate)
library(stringr)
library(tidytext)
library(crqa)

library(ggthemes)
library(scales)

# cleaning the data
setwd("/home/maltelau/Documents/CogSci/Social and Cultural Dynamics in Cognition/exam code")
chatfile = "data/Chat messages (accessed 2017-04-26).csv"

conf_scheme <- read.delim("codingscheme.txt", header = F, stringsAsFactors = F)
names(conf_scheme) <- c("pattern", "name")
apply_conf_scheme <- function(word, scheme=conf_scheme) {
    scheme %>%
        filter(str_detect(word, pattern)) %>%
        select(name) %>%
        as.character()
}



group_ids <- read.csv("data/group.csv") %>%
    select(group, group_id) %>%
    distinct()

data = read_csv(chatfile) %>%
    mutate(participant = as.numeric(factor(participant__code)),
           group = as.numeric(str_extract(channel, "\\d*$")),
           timestamp = as_datetime(timestamp, tz="Europe/Copenhagen")) %>%
    select(participant, group, timestamp, body) %>%
    right_join(group_ids) %>%
    mutate(group = group_id) %>% select(-group_id)




words <- data %>%
    unnest_tokens(word, body, drop = F)%>%
    filter(complete.cases(participant))



tokens_types <- words %>%
    
    # calculate some per-participant stats
    group_by(participant, group) %>%
    summarise(tokens = n(),
              types = length(unique(word)),
              vowels = sum(word %in% c("e", "i", "o", "u"))) %>%
    
    # sum up to per-group stats
    group_by(group) %>%
    do({data_frame(tokens = sum(.$tokens),
                   types = sum(.$types),
                   vowels = sum(.$vowels),
                   token_diff = abs(diff(.$tokens)),
                   type_diff  = abs(diff(.$types)),
                   vowel_diff  = abs(diff(.$vowels)),
                   token_ratio = max(.$tokens) / min(.$tokens),
                   type_ratio  = max(.$types) / min(.$types),
                   vowel_ratio  = max(.$vowels) / min(.$vowels)
                   )})

with_scheme <- words %>%
    group_by(participant) %>%
    mutate(conf = purrr::map_chr(word, apply_conf_scheme)) %>%
    filter(!conf %in% "character(0)") %>%
    select(-word)


# local confidence alignment
with_alignment_1 <- data %>%
    inner_join(with_scheme) %>%
    group_by(group) %>%
    do({
        begin = min(.$timestamp)
        data = .

        arrange(., timestamp) %>%
            mutate(local_alignment_confidence = pmap_lgl(list(conf, participant, timestamp), function(x,y,z) {
                # ugly hack
                z <- as_datetime(z, tz="Europe/Copenhagen")
                
                dat = filter(data,
                             participant != y,
                             timestamp %within% interval(begin, z))
                
                return(ifelse(nrow(dat) > 0,
                       x== dat[nrow(dat),"conf"],
                       NA))
            }))}) %>%
    group_by(group) %>%
    summarise(local_alignment_confidence = mean(local_alignment_confidence, na.rm=T))
                                
            
# local aligment for all words
with_alignment_2 <- words %>%
    group_by(group) %>%
    do({
        begin = min(.$timestamp)
        data = .
        
        arrange(., timestamp) %>%
            mutate(local_alignment_all = pmap_lgl(list(word, participant, timestamp), function(x,y,z) {
                z <- as_datetime(z, tz="Europe/Copenhagen")
                
                dat = filter(data,
                             participant != y,
                             timestamp %within% interval(begin, z))
                
                if (nrow(dat) == 0) {return(NA)}
                dat <- dat %>%
                    filter(timestamp == dat[nrow(dat),"timestamp"][[1]])
                
                return(x %in% dat$word)
            }))
    }) %>%
    group_by(group) %>%
    summarise(local_alignment_all = mean(local_alignment_all, na.rm=T))


# global alignment for confidence expressions
with_alignment_3 <- with_scheme %>%
    group_by(group) %>%
    count(conf) %>%
    summarise(global_alignment_confidence = max(n) / sum(n))


# cosine distance between word sets
cosine_word_set <- words %>%
    group_by(group) %>%
    do({
        all_words = unique(.$word)
        group_by(., participant) %>%
            summarise(word_set = list(as.numeric(all_words %in% word)))
    }) %>%
    summarise(cosine_word_set = lsa::cosine(word_set[[1]], word_set[[2]]))




# RQA

# first, split to a one-letter-per-row data frame
letters <- words %>%
    mutate(letter = purrr::map(word, ~unlist(str_split(.x, "")))) %>%
    unnest() %>%
    # then transform each letter into a unique number
    mutate(letter = as.numeric(factor(letter)))



rqa_self <- letters %>%
    group_by(participant, group) %>%
    do({RQA <- crqa(.$letter, .$letter, delay=1, embed=4, radius=0, rescale=0, normalize=0, mindiagline=2, minvertline=2)
        data.frame(self_rr = RQA$RR, self_l = RQA$L, self_entr = RQA$ENTR)}) %>%
    group_by(group) %>%
    summarise_at(starts_with("self"), mean) %>%
    select(-participant)

rqa_alignment <- letters %>%
    group_by(group) %>%
    do({
        participants = unique(.$participant)
        RQA <- crqa(filter(., participant == participants[1])$letter, 
                    filter(., participant == participants[2])$letter,
                    delay=1, embed=4, radius=0, rescale=0, normalize=0, mindiagline=2, minvertline=2)
        data.frame(align_rr = RQA$RR, align_l = RQA$L, align_entr = RQA$ENTR)
    })


rqa_synergy <- letters %>%
    group_by(group) %>%
    do({
        RQA <- crqa(.$letter, .$letter,
                    delay=1, embed=4, radius=0, rescale=0, normalize=0, mindiagline=2, minvertline=2)
        data.frame(synergy_rr = RQA$RR, synergy_l = RQA$L, synergy_entr = RQA$ENTR)
    })







# combine all the text stuff
text_data <- with_alignment_1 %>%
    full_join(with_alignment_2) %>%
    full_join(with_alignment_3) %>%
    full_join(tokens_types) %>%
    full_join(cosine_word_set) %>%
    full_join(rqa_self)%>%
    full_join(rqa_alignment)%>%
    full_join(rqa_synergy)


write.csv(text_data, "data/text_data.csv", row.names = F)




# global aligment plots

with_scheme %>%
    group_by(group, participant) %>%
    count(conf, sort=T) %>%
    group_by(conf) %>%
    mutate(total_n = sum(n)) %>%
    group_by(group) %>%
    mutate(n = n / sum(n)) %>%
    ggplot(aes(reorder(conf, desc(total_n)), n, fill=factor(participant))) +
    geom_bar(stat="identity") +
    facet_wrap(~group, labeller = "label_both") +
    theme_classic() +
    guides(fill = "none") +
    theme(axis.text.x = element_text(angle = 90),
          strip.background = element_blank()) +
    labs(x = "Confidence expression (Type)",
         y = "Percentage Tokens",
         title = "Confidence Expressions",
         subtitle = "Distributions of confidence expressions as percentage of all confidence expressions by each group\nEach participant in a separate colour") +
    scale_y_continuous(labels = percent)



# correlations

text_data %>%
    select(local_alignment_all, local_alignment_confidence, global_alignment_confidence, cosine_word_set) %>%
    cor()

text_data %>%
    select(self_rr:synergy_entr) %>%
    cor()

