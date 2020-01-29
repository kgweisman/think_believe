# DATA PREPARATION

# demographic variables from larger packets -----
demo_p456 <- read_csv("../data/demo_p456.csv") %>%
  mutate(religion = factor(religion,
                           levels = c("Buddhist", "Christian", 
                                      "Other religious", "Not religious")))


# think believe 1 (forced choice) -----
# load raw data
d1_raw <- read_xlsx("../data/ThinkBelieve1_organized.xlsx", sheet = "V1&V2 no dupes") %>%
  # eliminate one duplicate
  group_by(thb1_subj) %>%
  top_n(1, thb1_batc) %>% 
  ungroup() %>%
  mutate(thb1_ctry = factor(thb1_ctry, levels = levels_country))

# make question key
key1 <- read_xlsx("../data/ThinkBelieve1_organized.xlsx", sheet = 1)[1,] %>% 
  t() %>% 
  data.frame() %>% 
  rownames_to_column("question") %>%
  rename(question_text = ".") %>%
  # get rid of white space
  mutate(question_text = gsub("\\s+", " ", question_text)) %>%
  # hand code question categories
  mutate(category = case_when(
    grepl("final paper", question_text) |
      grepl("highway into town", question_text) |
      grepl("grocery store", question_text) |
      grepl("chemistry book", question_text) |
      grepl("cooking noodles", question_text) ~ "life fact",
    grepl("praying to God", question_text) |
      grepl("angels deliver", question_text) |
      grepl("go to Heaven", question_text) |
      grepl("changed water", question_text) |
      grepl("human sins", question_text) ~ "Christian religious",
    grepl("cycle of death", question_text) |
      grepl("Buddha found spiritual", question_text) |
      grepl("lotus flower bloomed", question_text) |
      grepl("ghosts suffer", question_text) |
      grepl("burning incense", question_text) ~ "Buddhist religious",
    grepl("moon goes around", question_text) |
      grepl("Barack Obama", question_text) |
      grepl("using batteries", question_text) |
      grepl("Brazil", question_text) |
      grepl("ancient Roman", question_text) ~ "well-known fact",
    grepl("octopus", question_text) |
      grepl("John Brown", question_text) |
      grepl("taller mountain", question_text) |
      grepl("species of fish", question_text) |
      grepl("contains more copper", question_text) ~ "esoteric fact",
    TRUE ~ NA_character_)) %>%
  mutate(category = factor(category, 
                           levels = c("Christian religious", 
                                      "Buddhist religious", 
                                      "well-known fact", 
                                      "esoteric fact", 
                                      "life fact")),
         super_cat = case_when(grepl("fact", category) ~ "fact",
                               grepl("religious", category) ~ "religious",
                               TRUE ~ NA_character_),
         super_cat = factor(super_cat, levels = c("religious", "fact"))) %>%
  rownames_to_column("order") %>%
  mutate(order = as.numeric(order),
         question_text = gsub("‚Äô", "'", question_text),
         question_text_short = gsub("^.*that ", "...", question_text),
         var_name = names(d1_raw[names(d1_raw) != "thb1_version"]))

# clean up variables
d1 <- d1_raw %>%
  filter(thb1_ctry %in% levels_country) %>%
  mutate(thb1_ctry = factor(thb1_ctry, levels = levels_country),
         thb1_demo_sex = factor(thb1_demo_sex,
                                levels = c("Male", "Female", "Other")), 
         thb1_demo_age = as.numeric(as.character(thb1_demo_age))) %>%
  mutate_at(vars(thb1_demo_regp, thb1_demo_olang),
            funs(factor(., levels = c("NO", "YES")))) %>%
  mutate_at(vars(thb1_demo_rely, thb1_demo_impr, thb1_demo_imsn), 
            funs(factor(., levels = 1:7))) %>%
  mutate(thb1_demo_wors = factor(thb1_demo_wors, 
                                 levels = c("Never", 
                                            "Once a year or less",
                                            "A few times a year",
                                            "Once or twice a month",
                                            "Every week or more often")),
         thb1_demo_bgod = factor(thb1_demo_bgod,
                                 levels = c("Not at all believe",
                                            "Believe slightly",
                                            "Believe moderately",
                                            "Believe strongly")),
         thb1_demo_bbuh = factor(thb1_demo_bbuh,
                                 levels = c("Not at all believe",
                                            "Believe slightly",
                                            "Believe moderately",
                                            "Believe strongly")),
         thb1_demo_bosp = factor(thb1_demo_bosp,
                                 levels = c("Not at all believe",
                                            "Believe slightly",
                                            "Believe moderately",
                                            "Believe strongly")),
         thb1_demo_atsn = factor(thb1_demo_atsn,
                                 levels = c("There is no such thing as supernatural forces or beings",
                                            "We cannot know if there are supernatural forces and beings",
                                            "There might be supernatural forces and beings",
                                            "Supernatural forces and beings exist but we cannot know what they are like",
                                            "There definitely are supernatural forces and beings"))) %>%
  mutate_at(vars(thb1_demo_rely, thb1_demo_impr, thb1_demo_wors, thb1_demo_bgod, 
                 thb1_demo_bbuh, thb1_demo_bosp, thb1_demo_atsn, thb1_demo_imsn), 
            funs(num = as.numeric(.) - 1)) %>%
  mutate(thb1_religion = case_when(
    thb1_demo_regp_1_TEXT %in% c("Anglican", 
                                 "Anglican/SDA",
                                 "AoG",
                                 "A.O.G youth group", 
                                 "Anuchon Church", 
                                 "AOG: Youth", 
                                 "Apostolic", 
                                 "Bible Church & CMC Church",
                                 "Bible church and Presbyterian church",
                                 "Black Campus Ministry",
                                 "Campus crusade for Christ",
                                 "Catholic Christian",
                                 "Catholic Church",
                                 "cell group Anuchon",
                                 "CF, Youth Ministry etc.",
                                 "Chorus youth",
                                 "Christian", 
                                 "Christian house church",
                                 "Christian - Methodist",
                                 "Christian - Pentecost", 
                                 "Christian - Roman Catholic",
                                 "Christian (Catholic)", 
                                 "Christian (Pentecost)",
                                 "Christian (Potters hand)",
                                 "Christian (Roman)", 
                                 "Christian church in Sop Tia",
                                 "Christian Fellowship Group (CF)", 
                                 "Christianity", 
                                 "Christianity (overcomers and Congress Int. Church)",
                                 "Christianity (Roman Catholic)", 
                                 "Christianity (Roman)", 
                                 "Christians on Campus SJSU",
                                 "Church",
                                 "church",
                                 "Church Youth",
                                 "Church Youth and Religious Singing Group",
                                 "church youths", 
                                 "Church, Youth fellowship",
                                 "Church: Sunday School + Services",
                                 "Community Prayer group", 
                                 "Community Prayer Group",
                                 "D.R.E.A.M Campus Ministry",
                                 "Emalus religious group, student life association",
                                 "Epauto church (Port Vila)", 
                                 "Father's House", 
                                 "Fathers house",
                                 "Fellowship",
                                 "friend group",
                                 "Glorious church people",
                                 "Glourise Church going and pray for sick people",
                                 "Go to Catholic church every Sunday",
                                 "Go to church every Sunday", 
                                 "got to church",
                                 "go to church",
                                 "group of Youth",
                                 "House church", 
                                 "House church of Christian",
                                 "I am part or member of Potoroki youth",
                                 "ICOMB", 
                                 "Jehova's Witnesses",
                                 "Jehovah's Witnesses",
                                 "joy with Municipality group", 
                                 "Just Youth's",
                                 "Kids prayer warrior",
                                 "Korean Campus Crusade for Christ?",
                                 "KYB - Knowing Your Bible",
                                 "LDS",
                                 "LDS Mormon",
                                 "Leader",
                                 "Leader of the Youth",
                                 "Living water, Heram praise", 
                                 "Listen to sermon",
                                 "local churches",
                                 "Mae Ka Boo Church",
                                 "Member of the youth, children class teacher",
                                 "Missonettes",
                                 "Mormon helping hands",
                                 "NTM Youth", 
                                 "only P.R.C",
                                 "Pastor and Apostle",
                                 "Pathfinder, Ambassador, Youth",
                                 "Pathway Ministry", 
                                 "praise & worship, visitation, combine service, youth, choir practice",
                                 "Pray warrior",
                                 "Praying Sunday",
                                 "Presbertent", 
                                 "Presbyterian", 
                                 "Presbyterian Church",
                                 "Pulse", 
                                 "Roman Catholic",
                                 "Roman Catholic (technically)",
                                 "Scripture Union", 
                                 "SDA Youth",
                                 "singing",
                                 "Siyon Group/ like caregroup",
                                 "Student life (USP)", 
                                 "sunday school",
                                 "sunday school, youth etc",
                                 "Sunday School, Bible study and Youth",
                                 "Sunday School, Youth",
                                 "Teacher for Sabbth School",
                                 "Ward Choir, Elder's quorem", 
                                 "Watchnight service",
                                 "Watchnight service ordination",
                                 "Words Christian fellowship",
                                 "Yes kawariki S.D.A Church",
                                 "Yes, Prayer group, Church Instrumentalis",
                                 "Yes! Church youth (drumer)",
                                 "Yes! - youth group/ministry -children ministry",
                                 "Young single adults", 
                                 "youth",
                                 "Youth", 
                                 "Youth Activities",
                                 "Youth Fellowship", 
                                 "Youth group", 
                                 "Youth Group",
                                 "Youth member", 
                                 "Youth Member",
                                 "Youth Members",
                                 "Youth, Church Band and Women's Ministry",
                                 "Youth, Sunday School",
                                 "Youth, Sunday school", 
                                 "Youth, Sunday School, etc...",
                                 "Youth/Men's Fellowship",
                                 "Youths (Port Vila) Anglican",
                                 "(Youth)",
                                 "youths") ~ "Christian",
    thb1_demo_regp_1_TEXT %in% c("Buddhism", 
                                 "Buddhism club",
                                 "Buddhist", 
                                 "Buddhist camping",
                                 "Buddhist Camping",
                                 "Buddhist Club",
                                 "Buddhist Sunday",
                                 "Buddhist, make merit in temple, go to temple",
                                 "Chant / Merit / Listening to Buddhist's teaching",
                                 "facebook village temple",
                                 "Go to neighbor temple", 
                                 "Go to temple",
                                 "Going to temple", 
                                 "Guan Yin Citta (created by an Australian Chinese",
                                 "holy day",
                                 "Holy day",
                                 "make merit", 
                                 "make merit in holy day",
                                 "Make merit",
                                 "Meditation",
                                 "meditation camping",
                                 "Merit give food to monk", 
                                 "Merit, going to temple in holy day",
                                 "Merit group",
                                 "Monk",
                                 "Temple",
                                 "to make merit, listening to Buddhist is teaching",
                                 "to temple") ~ "Buddhist",
    is.na(thb1_demo_regp_1_TEXT) |
      thb1_demo_regp_1_TEXT %in% c("mdata", "Missing Data", "not trans", 
                                   "Not trans", "none") ~ NA_character_,
    TRUE ~ "Other"
  ))

# make longform dataframe
d1_long <- d1 %>%
  gather(question, response, thb1_ghostshunger:thb1_obama) %>%
  mutate(think = ifelse(grepl("think", response) |
                          grepl("thought", response), T, F),
         believe = ifelse(grepl("belie", response), T, F),
         response_cat = case_when(believe == T ~ "believe",
                                  think == T ~ "think",
                                  TRUE ~ NA_character_),
         response_cat = factor(response_cat, levels = c("think", "believe"))) %>%
  left_join(key1 %>% select(-question) %>% rename(question = var_name))

# implement exclusion criteria, rename country variable, and add demo variables
d1 <- d1 %>% 
  filter(thb1_ordr == "Yes", thb1_attn == "Pass") %>%
  rename(country = thb1_ctry) %>%
  left_join(demo_p456 %>% select(-country), by = c("thb1_subj" = "subj"))

d1_long <- d1_long %>% 
  filter(thb1_ordr == "Yes", thb1_attn == "Pass") %>%
  rename(country = thb1_ctry) %>%
  left_join(demo_p456 %>% select(-country), by = c("thb1_subj" = "subj"))

# set contrasts
contrasts(d1$country) = contrast_country
contrasts(d1_long$country) = contrast_country
# contrasts(d1_long$category) = contrast_category
contrasts(d1_long$category) = contrast_category_orth
contrasts(d1_long$super_cat) = contrast_super_cat

# make sample size df
sample_size_d1 <- d1 %>% 
  count(country) %>% 
  data.frame() %>%
  mutate(country_n = paste0(country, " (n=", n, ")"),
         country_n = reorder(country_n, as.numeric(country)))

