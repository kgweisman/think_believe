# DATA CREATION

# run scripts -----
source("../analyses/scripts/dependencies.R")
source("../analyses/scripts/custom_funs.R")
source("../analyses/scripts/var_recode_contrast.R")


# think believe 1 (forced choice) -----
# load raw data
d1_raw <- read_xlsx("../data_raw/ThinkBelieve1_organized.xlsx", sheet = "V1&V2 no dupes") %>%
  # eliminate one duplicate
  group_by(thb1_subj) %>%
  top_n(1, thb1_batc) %>% 
  ungroup() %>%
  filter(thb1_ctry %in% levels_country) %>%
  mutate(thb1_ctry = factor(thb1_ctry, levels = levels_country))

# make question key
key1 <- read_xlsx("../data_raw/ThinkBelieve1_organized.xlsx", sheet = 1)[1,] %>% 
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

# implement exclusion criteria, rename country variable, and add demo variables
d1 <- d1 %>% 
  filter(thb1_ordr == "Yes", thb1_attn == "Pass") %>%
  rename(country = thb1_ctry)


# think believe 2 (free response) -----

# load raw data
d2_raw <- read_xlsx("../data_raw/ThinkBelieve2_organized.xlsx", sheet = "V1&V2 no dupes") %>%
  # ensure no duplicates
  group_by(thb2_subj) %>%
  top_n(1, thb2_batc) %>% 
  ungroup() %>%
  filter(thb2_ctry %in% levels_country) %>%
  mutate(thb2_ctry = factor(thb2_ctry, levels = levels_country))

# make question key
key2 <- read_xlsx("../data_raw/ThinkBelieve2_organized.xlsx", sheet = 1)[1,] %>% 
  data.frame() %>%
  # get rid of extra qualtrics questions
  select(-c(StartDate:UserLanguage)) %>%
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
         var_name = names(d2_raw[names(d2_raw) != "thb2_version"]))

# clean up variables
d2 <- d2_raw %>%
  filter(thb2_ctry %in% levels_country) %>%
  mutate(thb2_ctry = factor(thb2_ctry, levels = levels_country),
         thb2_demo_sex = factor(thb2_demo_sex,
                                levels = c("Male", "Female", "Other")), 
         thb2_demo_age = as.numeric(as.character(thb2_demo_age))) %>%
  mutate_at(vars(thb2_demo_regp, thb2_demo_olang),
            funs(factor(., levels = c("NO", "YES")))) %>%
  mutate_at(vars(thb2_demo_rely, thb2_demo_impr, thb2_demo_imsn), 
            funs(factor(., levels = 1:7))) %>%
  mutate(thb2_demo_wors = factor(thb2_demo_wors, 
                                 levels = c("Never", 
                                            "Once a year or less",
                                            "A few times a year",
                                            "Once or twice a month",
                                            "Every week or more often")),
         thb2_demo_bgod = factor(thb2_demo_bgod,
                                 levels = c("Not at all believe",
                                            "Believe slightly",
                                            "Believe moderately",
                                            "Believe strongly")),
         thb2_demo_bbuh = factor(thb2_demo_bbuh,
                                 levels = c("Not at all believe",
                                            "Believe slightly",
                                            "Believe moderately",
                                            "Believe strongly")),
         thb2_demo_bosp = factor(thb2_demo_bosp,
                                 levels = c("Not at all believe",
                                            "Believe slightly",
                                            "Believe moderately",
                                            "Believe strongly")),
         thb2_demo_atsn = factor(thb2_demo_atsn,
                                 levels = c("There is no such thing as supernatural forces or beings",
                                            "We cannot know if there are supernatural forces and beings",
                                            "There might be supernatural forces and beings",
                                            "Supernatural forces and beings exist but we cannot know what they are like",
                                            "There definitely are supernatural forces and beings"))) %>%
  mutate_at(vars(thb2_demo_rely, thb2_demo_impr, thb2_demo_wors, thb2_demo_bgod, 
                 thb2_demo_bbuh, thb2_demo_bosp, thb2_demo_atsn, thb2_demo_imsn), 
            funs(num = as.numeric(.) - 1)) %>%
  mutate(thb2_religion = case_when(
    thb2_demo_regp_1_TEXT %in% c("Anglican", 
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
    thb2_demo_regp_1_TEXT %in% c("Buddhism", 
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
    is.na(thb2_demo_regp_1_TEXT) |
      thb2_demo_regp_1_TEXT %in% c("mdata", "Missing Data", "not trans", 
                                   "Not trans", "none") ~ NA_character_,
    TRUE ~ "Other"
  ))

# make longform dataframe and lemmatize
d2_long <- d2 %>%
  gather(question, response, thb2_ghostshunger:thb2_obama) %>%
  mutate(response_lemma = lemmatize_strings(tolower(response)),
         response_lemma = gsub("\\( ", "", response_lemma),
         response_lemma = gsub(" \\)", "", response_lemma)) %>%
  mutate(response_lemma2 = case_when(
    response_lemma %in% c("talem", "talemaot", "talm", "blo talem", 
                          "talemout", "bin talem") ~ "say",
    response_lemma %in% c("luk", "look", "lukim", "luk save", "luk vision",
                          "lukum", "bin ko luk", "lok") ~ "see",
    response_lemma %in% c("believe", "belief", 
                          "beliv", "belivs", "belived", 
                          "belivm", "belivem", "belivim",
                          "belif", "belifs", "belifed",
                          "belif", "belifem", "belifim",
                          "biliv", "bilivs", "bilived",
                          "bilivm", "bilivem", "bilivim",
                          "bilif", "bilifs", "bilifed",
                          "bilifm", "bilifem", "bilifim",
                          "belive", "belives", "belived",
                          "bilive", "bilives", "bilived",
                          "beleive", "beleives", "beleived",
                          "beleivm", "beleivem", "beleivim",
                          "biliviem", "beliviem",
                          "beiv", "beives", "beived", 
                          "beivm", "beivem", "beivim",
                          "beleif", "beleifs", "beleifed",
                          "beleifm", "beleifem", "beleifim", 
                          "believem", "wu belive") ~ "believe",
    response_lemma %in% c("biliv strong", "blindly believe", "deeply believe",
                          "firmly believe", "surely believe", 
                          "wrongly believe") ~ "[adverb] believe",
    response_lemma %in% c("does not believe", "doesn't believe",
                          "do not believe", "don't believe",
                          "didn't believe",
                          "no believem") ~ "do not believe",
    response_lemma %in% c("think", "tingting", "tink", "thnks", "tinks",
                          "because she think", "she think", "thing", "ting",
                          "think right that", "think that", "think think",
                          "thinkthink", "use to think") ~ "think",
    response_lemma %in% c("tingbaot", "tingboat", "ting bot", "tingbaut",
                          "tingbot", "tingibaot", "tinkbaot", "thingbaot", 
                          "thingbaut") ~ "think about/remember",
    response_lemma %in% c("think of") ~ "think of",
    response_lemma %in% c("does not think", "doesn't think", 
                          "do not think", "don't think", 
                          "didn't think", "never think", "not think") ~ "do not think",
    response_lemma %in% c("XX") ~ "[adverb] think",
    response_lemma %in% c("save", "saveh", "sae") ~ "know",
    response_lemma %in% c("no save", "nosave", "no sae", "nosae", 
                          "no bin save", 
                          "does not know", "doesn't know", 
                          "do not know", "don't know",
                          "didn't know") ~ "do not know",
    response_lemma %in% c("wantem", "wante", "wantm") ~ "want",
    response_lemma %in% c("harem", "stap harem") ~ "hear",
    response_lemma %in% c("tokbaot", "tok bot", "tokbaout", "talkboat", 
                          "takboat", "tokbaut", "tokabout") ~ "describe/discuss",
    response_lemma %in% c("ridim", "readim", "readm", "ridem", "readem", 
                          "redem") ~ "read",
    response_lemma %in% c("kiaman", "kieman") ~ "lie",
    response_lemma %in% c("lanem") ~ "learn",
    response_lemma %in% c("trastem", "trustem") ~ "trust",
    response_lemma %in% c("confemem", "confirmem") ~ "confirm",
    response_lemma %in% c("faenem", "fainem", "finem", "be fainem", "finiem") ~ "find",
    response_lemma %in% c("fraet", "fright") ~ "fear",
    response_lemma %in% c("laekem", "likem", "laikem") ~ "like",
    response_lemma %in% c("no likem") ~ "do not like",
    response_lemma %in% c("sek") ~ "be shaken",
    response_lemma %in% c("saprais", "sapras", "saprias") ~ "be surprised",
    response_lemma %in% c("tijim", "titjim") ~ "teach",
    response_lemma %in% c("wari") ~ "worry",
    response_lemma %in% c("dream", "drim", "droem") ~ "dream",
    response_lemma %in% c("enkarej") ~ "encourage",
    response_lemma %in% c("explenem") ~ "explain",
    response_lemma %in% c("faenemaot", "faenmaot", "fainem out", "fainmoat",
                          "fainmaot", "faenamaot", "faenemaut", 
                          "fainem aot", "fanemaut", "finemaot", 
                          "finemout", "faenem maot", "faenemaat", "faenemoat",
                          "faenemout", "finem aot") ~ "find out",
    response_lemma %in% c("fogatem", "forgetem", "fogetem", "fogatem", "foget",
                          "foketem", "forget", "foket", "foketom") ~ "forget", 
    response_lemma %in% c("from") ~ "because of",
    response_lemma %in% c("hop") ~ "hope",
    response_lemma %in% c("imagin", "imaginem") ~ "imagine",
    response_lemma %in% c("infomem", "informem", "informen") ~ "inform",
    response_lemma %in% c("kasem") ~ "receive",
    response_lemma %in% c("kros") ~ "angry",
    response_lemma %in% c("kwesten", "questenem") ~ "question",
    response_lemma %in% c("notisim") ~ "notice",
    response_lemma %in% c("riconizem", "riconizen") ~ "recognize",
    response_lemma %in% c("rimembarem", "rememba", "remember") ~ "remember",
    response_lemma %in% c("ripot", "reportam", "ribotem") ~ "report",
    response_lemma %in% c("singaot") ~ "call",
    response_lemma %in% c("sowem", "soem") ~ "show",
    response_lemma %in% c("withnessem") ~ "witness",
    response_lemma %in% c("kes") ~ "case",
    response_lemma %in% c("studi", "study", "stady", "stadi") ~ "study",
    response_lemma %in% c("havem") ~ "have",
    response_lemma %in% c("jes save") ~ "come to know",
    response_lemma %in% c("save finis") ~ "already know",
    response_lemma %in% c("understanem", "antastanem") ~ "understand",
    response_lemma %in% c("no understanem") ~ "do not understand",
    response_lemma %in% c("agri") ~ "agree",
    response_lemma %in% c("askem") ~ "ask",
    response_lemma %in% c("bin identifi") ~ "identify",
    response_lemma %in% c("discoverem", "diskararem") ~ "discover",
    response_lemma %in% c("folemap") ~ "follow up",
    response_lemma %in% c("rielaesm", "realaesem", "realise", 
                          "realizem") ~ "realize",
    response_lemma %in% c("no rielaesm") ~ "did not realize",
    response_lemma %in% c("raetem", "writem", "raitem", "ritem", 
                          "biritingbut") ~ "write",
    response_lemma %in% c("traem") ~ "try",
    response_lemma %in% c("watchem", "watchm", "wajem") ~ "watch",
    response_lemma %in% c("tingse", "tung se") ~ "express an opinion",
    response_lemma %in% c("rao") ~ "argue",
    response_lemma %in% c("ansa", "ansarem") ~ "answer",
    response_lemma %in% c("jekem") ~ "check",
    response_lemma %in% c("olsem") ~ "all-same",
    response_lemma %in% c("serchem") ~ "search",
    response_lemma %in% c("sua") ~ "be sure",
    response_lemma %in% c("promes") ~ "promise",
    response_lemma %in% c("wet") ~ "wait",
    response_lemma %in% c("krae") ~ "cry",
    response_lemma %in% c("prae") ~ "pray",
    response_lemma %in% c("ring") ~ "call",
    response_lemma %in% c("use to she fill this way") ~ "feel", 
    response_lemma == "mdata" ~ NA_character_,
    TRUE ~ response_lemma
  )) %>%
  mutate(think = ifelse(response_lemma2 %in% c("think", "thought"), T, F),
         thinkX = ifelse(grepl("think", response_lemma2) |
                           grepl("thought", response_lemma2), T, F)) %>%
  mutate(believe = ifelse(response_lemma2 %in% c("believe", "belief"), T, F),
         believeX = ifelse(grepl("belie", response_lemma2), T, F)) %>%
  mutate(know = ifelse(response_lemma2 %in% c("know", "knew", "knows"), T, F),
         knowX = ifelse(grepl("know", response_lemma2) |
                          grepl("knew", response_lemma2), T, F)) %>%
  mutate(response_cat4 = case_when(think == T ~ "think",
                                   believe == T ~ "believe", 
                                   know == T ~ "know",
                                   !is.na(response) ~ "other response", 
                                   TRUE ~ NA_character_),
         response_cat4 = factor(response_cat4,
                                levels = c("other response", "know", 
                                           "think", "believe"))) %>%
  mutate(responseX_cat4 = case_when(thinkX == T ~ "think*",
                                   believeX == T ~ "believe*", 
                                   knowX == T ~ "know*",
                                   !is.na(response) ~ "other response", 
                                   TRUE ~ NA_character_),
         responseX_cat4 = factor(responseX_cat4,
                                 levels = c("other response", "know*", 
                                            "think*", "believe*"))) %>%
  mutate(response_cat3 = case_when(think == T ~ "think",
                                   believe == T ~ "believe",
                                   !is.na(response) ~ "other response",
                                   TRUE ~ NA_character_),
         response_cat3 = factor(response_cat3, 
                                levels = c("other response", "think", "believe"))) %>%
  mutate(responseX_cat3 = case_when(thinkX == T ~ "think*",
                                    believeX == T ~ "believe*",
                                    !is.na(response) ~ "other response",
                                    TRUE ~ NA_character_),
         responseX_cat3 = factor(responseX_cat3,
                                 levels = c("other response", "think*", "believe*"))) %>%
  mutate(response_cat = recode_factor(as.character(believe), 
                                      "FALSE" = "other", "TRUE" = "believe")) %>%
  mutate(responseX_cat = recode_factor(as.character(believeX), 
                                       "FALSE" = "other", "TRUE" = "believeX")) %>%
  left_join(key2 %>% select(-question) %>% rename(question = var_name)) %>%
  mutate(category2 = case_when(
    grepl("fact", category) ~ as.character(category),
    grepl("Christian", category) & 
      thb2_ctry %in% c("US", "Ghana", "Vanuatu") ~ "local religious",
    grepl("Buddhist", category) & 
      thb2_ctry %in% c("Thailand", "China") ~ "local religious",
    grepl("Christian", category) &
      thb2_ctry %in% c("Thailand", "China") ~ "other religious",
    grepl("Buddhist", category) &
      thb2_ctry %in% c("US", "Ghana", "Vanuatu") ~ "other religious",
    TRUE ~ NA_character_),
    category2 = factor(category2,
                       levels = c("local religious", "other religious",
                                  "well-known fact", "esoteric fact", 
                                  "life fact")))

# implement exclusion criteria, rename country variable, and add demo variables
d2 <- d2 %>% 
  filter(thb2_ordr == "Yes", thb2_attn == "Pass") %>%
  rename(country = thb2_ctry)

d2_long <- d2_long %>% 
  filter(thb2_ordr == "Yes", thb2_attn == "Pass") %>%
  rename(country = thb2_ctry)


# think believe 3 (forced choice, controlled content) -----

# load raw data
d3_raw <- read_xlsx("../data_raw/ThinkBelieve3_organized_updated_02.21.2020.xlsx", sheet = "V1 & V2 no dupes") %>%
  # identify second sample from Ghana
  mutate(thb3_ctry = case_when(
    grepl("2020", as.character(thb3_2day)) ~ "Ghana (undergrads)",
    TRUE ~ thb3_ctry)) %>%
  # ensure no duplicates
  group_by(thb3_subj) %>%
  top_n(1, thb3_batc) %>% 
  ungroup() %>%
  filter(thb3_ctry %in% levels_country6) %>%
  mutate(thb3_ctry = factor(thb3_ctry, levels = levels_country6))

# make question key
key3 <- read_xlsx("../data_raw/ThinkBelieve3_organized.xlsx", sheet = 1)[1,] %>% 
  data.frame() %>%
  # get rid of extra qualtrics questions
  select(-c(StartDate:UserLanguage)) %>%
  t() %>% 
  data.frame() %>% 
  rownames_to_column("question") %>%
  rename(question_text = ".") %>%
  # get rid of white space
  mutate(question_text = gsub("\\s+", " ", question_text)) %>%
  # hand code question categories
  mutate(category = case_when(
    grepl("NASA", question_text) |
      grepl("medical school", question_text) |
      grepl("Astronomers", question_text) |
      grepl("reads history", question_text) |
      grepl("travels many places", question_text) ~ "scientific",
    grepl("Scientology", question_text) |
      grepl("God sent ten plagues", question_text) |
      grepl("holy man", question_text) |
      grepl("Mayan religion", question_text) |
      grepl("Church of Christ Scientist", question_text) ~ "religious",
    TRUE ~ NA_character_)) %>%
  mutate(category = factor(category, 
                           levels = c("religious", 
                                      "scientific")),
         super_cat = case_when(grepl("scientific", category) ~ "scientific",
                               grepl("religious", category) ~ "religious",
                               TRUE ~ NA_character_),
         super_cat = factor(super_cat, levels = c("religious", "scientific"))) %>%
  rownames_to_column("order") %>%
  mutate(order = as.numeric(order),
         question_text = gsub("‚Äô", "'", question_text),
         question_text_short = gsub("^.*that ", "...", question_text),
         var_name = names(d3_raw[names(d3_raw) != "thb3_version"]))

# clean up variables
d3 <- d3_raw %>%
  filter(thb3_ctry %in% levels_country6) %>%
  mutate(thb3_ctry = factor(thb3_ctry, levels = levels_country6),
         thb3_demo_sex = factor(thb3_demo_sex,
                                levels = c("Male", "Female", "Other")), 
         thb3_demo_age = as.numeric(as.character(thb3_demo_age))) %>%
  mutate_at(vars(thb3_demo_regp, thb3_demo_olang),
            funs(factor(., levels = c("NO", "YES")))) %>%
  mutate_at(vars(thb3_demo_rely, thb3_demo_impr, thb3_demo_imsn), 
            funs(factor(., levels = 1:7))) %>%
  mutate(thb3_demo_wors = factor(thb3_demo_wors, 
                                 levels = c("Never", 
                                            "Once a year or less",
                                            "A few times a year",
                                            "Once or twice a month",
                                            "Every week or more often")),
         thb3_demo_bgod = factor(thb3_demo_bgod,
                                 levels = c("Not at all believe",
                                            "Believe slightly",
                                            "Believe moderately",
                                            "Believe strongly")),
         thb3_demo_bbuh = factor(thb3_demo_bbuh,
                                 levels = c("Not at all believe",
                                            "Believe slightly",
                                            "Believe moderately",
                                            "Believe strongly")),
         thb3_demo_bosp = factor(thb3_demo_bosp,
                                 levels = c("Not at all believe",
                                            "Believe slightly",
                                            "Believe moderately",
                                            "Believe strongly")),
         thb3_demo_atsn = factor(thb3_demo_atsn,
                                 levels = c("There is no such thing as supernatural forces or beings",
                                            "We cannot know if there are supernatural forces and beings",
                                            "There might be supernatural forces and beings",
                                            "Supernatural forces and beings exist but we cannot know what they are like",
                                            "There definitely are supernatural forces and beings"))) %>%
  mutate_at(vars(thb3_demo_rely, thb3_demo_impr, thb3_demo_wors, thb3_demo_bgod, 
                 thb3_demo_bbuh, thb3_demo_bosp, thb3_demo_atsn, thb3_demo_imsn), 
            funs(num = as.numeric(.) - 1)) %>%
  mutate(thb3_religion = case_when(
    thb3_demo_regp_1_TEXT %in% c("Anglican", 
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
    thb3_demo_regp_1_TEXT %in% c("Buddhism", 
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
    is.na(thb3_demo_regp_1_TEXT) |
      thb3_demo_regp_1_TEXT %in% c("mdata", "Missing Data", "not trans", 
                                   "Not trans", "none") ~ NA_character_,
    TRUE ~ "Other"
  ))

# make longform dataframe
d3_long <- d3 %>%
  gather(question, response, thb3_aliens.nasa:thb3_dog.wellwater) %>%
  mutate(think = ifelse(grepl("think", response) |
                          grepl("thought", response), T, F),
         believe = ifelse(grepl("belie", response), T, F),
         response_cat = case_when(believe == T ~ "believe",
                                  think == T ~ "think",
                                  TRUE ~ NA_character_),
         response_cat = factor(response_cat, levels = c("think", "believe"))) %>%
  left_join(key3 %>% select(-question) %>% rename(question = var_name))

# implement exclusion criteria, rename country variable, and add demo variables
d3 <- d3 %>% 
  filter(thb3_ordr == "Yes", thb3_attn == "Pass") %>%
  rename(country = thb3_ctry)



# save csvs -----
write_csv(d1, "../data/d1.csv")
write_csv(key1, "../data/key1.csv")

write_csv(d2, "../data/d2.csv")
write_csv(key2, "../data/key2.csv")

write_csv(d3, "../data/d3.csv")
write_csv(key3, "../data/key3.csv")
