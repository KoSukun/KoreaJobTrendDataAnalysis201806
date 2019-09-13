install.packages('foreign')
install.packages("rJava")
install.packages("memoise")
install.packages("KoNLP")
install.packages("stringr")
install.packages("wordcloud")
install.packages("mapproj")
install.packages("stringi")
install.packages("devtools")

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav", to.data.frame = T)
welfare <- raw_welfare

str(welfare)
welfare <- rename(welfare,
                  birth = h10_g4,
                  business_category = h10_eco8,
                  code_region = h10_reg7)

summary(welfare$birth)
table(is.na(welfare$birth))
welfare$birth <- ifelse(((2018 - welfare$birth)<15 | (2018 - welfare$birth) > 60 ), NA, welfare$birth)

welfare$age <- 2018 - welfare$birth +1
summary(welfare$age)

welfare <- welfare %>%
  mutate(age_group = ifelse(age < 20, "10대",
                            ifelse(age < 30 , "20대",
                                   ifelse(age < 40, "30대",
                                          ifelse(age < 50, "40대", "50대~정년퇴임")))))

table(welfare$age_group)

class(welfare$business_category)
table(welfare$business_category)
table(is.na(welfare$business_category))

welfare$category_name <-
  ifelse(welfare$business_category <=3, "농림업",
         ifelse(welfare$business_category <= 8, "광업",
                ifelse(welfare$business_category <= 33, "제조업",
                       ifelse(welfare$business_category <= 36, "전기,가스, 증기 및 수도사업",
                              ifelse(welfare$business_category <= 39, "환경복원업",
                                     ifelse(welfare$business_category <= 42, "건설업",
                                            ifelse(welfare$business_category <= 47, "도매 및 소매업",
                                                   ifelse(welfare$business_category <= 52, "운수업",
                                                          ifelse(welfare$business_category <= 56, "숙박, 음식점 및 주점업",
                                                                 ifelse(welfare$business_category <= 63, "출판, 영상, 방송통신 및 정보서비스업",
                                                                        ifelse(welfare$business_category <= 66, "금융 및 보험업",
                                                                               ifelse(welfare$business_category <= 69, "부동산 및 임대업",
                                                                                      ifelse(welfare$business_category <= 73, "전문, 과학 및 기술 서비스업",
                                                                                             ifelse(welfare$business_category <= 75, "사업시설관리 및 사업자원 서비스업",
                                                                                                    ifelse(welfare$business_category <= 84, "공공행정, 국방 및 사회보장 행정업",
                                                                                                           ifelse(welfare$business_category <= 85, "교육 서비스업",
                                                                                                                  ifelse(welfare$business_category <= 87, "보건 및 사회복지 사업",
                                                                                                                         ifelse(welfare$business_category <= 91, "예술, 스포츠 및 여가관련 서비스업",
                                                                                                                                ifelse(welfare$business_category <= 96, "협회 및 단체, 수리 및 기타 개인 서비스업",
                                                                                                                                       ifelse(welfare$business_category <= 98, "기타 자가소비 생산활동",
                                                                                                                                              ifelse(welfare$business_category <= 99, "국제 및 외국기관", NA)))))))))))))))))))))

class(welfare$category_name)
table(welfare$category_name)

library(KoNLP)
library(dplyr)

useNIADic()

twen <- welfare %>%
  filter(age_group=="20대") %>% 
  select(category_name)
table(twen$category_name)
write.csv(twen, file="twen.csv")
twentxt <- readLines("twen.csv")

library(stringr)

twentxt <- str_replace_all(twentxt, "\\W", " ")

twennouns <- sapply(twentxt, extractNoun, USE.NAMES = F)
jobcount <- table(unlist(twennouns))
twen_job <- as.data.frame(jobcount, stringAsFactors = F)
twen_job <- rename(twen_job,
                   word = Var1,
                   freq = Freq)

twen_job$word <- as.character(twen_job$word)
twen_job <- filter(twen_job, nchar(word) >= 2)

summary(twen_job$freq)
boxplot(twen_job$freq)$stats

twen_job <- twen_job %>% 
  filter(freq > 1)

library(wordcloud)
library(RColorBrewer)

pal <- brewer.pal(8, "Dark2")

set.seed(1234)
wordcloud(words = twen_job$word,
          freq = twen_job$freq,
          min.freq = 3,
          max.words = 71,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.3),
          colors = pal)

twen_region <- welfare %>%
  filter(age_group=="20대") %>%  
  select(business_category, code_region)

table(is.na(twen_region))
twen_region <- twen_region %>% filter(!is.na(business_category))
table(is.na(twen_region))

#Seoul
seoul_business <-
  twen_region %>%
  filter(code_region==1) %>% 
  select(business_category)

seoul_service_job <-
  seoul_business %>%
  filter(business_category %in% c(58, 59, 60, 61, 62, 63, 70, 71, 72, 73, 74, 75, 85, 90, 91, 94, 95, 96)) %>% 
  summarise("인원"=n())

#Gyeonggi
gyeonggi_business <-
  twen_region %>%
  filter(code_region==2) %>% 
  select(business_category)

gyeonggi_service_job <-
  gyeonggi_business %>%
  filter(business_category %in% c(58, 59, 60, 61, 62, 63, 70, 71, 72, 73, 74, 75, 85, 90, 91, 94, 95, 96)) %>% 
  summarise("인원"=n())

#Gyeongsangnam
gyeongsangnam_business <-
  twen_region %>%
  filter(code_region==3) %>% 
  select(business_category)

gyeongsangnam_service_job <-
  gyeongsangnam_business %>%
  filter(business_category %in% c(58, 59, 60, 61, 62, 63, 70, 71, 72, 73, 74, 75, 85, 90, 91, 94, 95, 96)) %>% 
  summarise("인원"=n())

#Gyeongsangbuk
gyeongsangbuk_business <-
  twen_region %>%
  filter(code_region==4) %>% 
  select(business_category)

gyeongsangbuk_service_job <-
  gyeongsangbuk_business %>%
  filter(business_category %in% c(58, 59, 60, 61, 62, 63, 70, 71, 72, 73, 74, 75, 85, 90, 91, 94, 95, 96)) %>% 
  summarise("인원"=n())

#Chungchungnam
chungcheongnam_business <-
  twen_region %>%
  filter(code_region==5) %>% 
  select(business_category)

chungcheongnam_service_job <-
  chungcheongnam_business %>%
  filter(business_category %in% c(58, 59, 60, 61, 62, 63, 70, 71, 72, 73, 74, 75, 85, 90, 91, 94, 95, 96)) %>% 
  summarise("인원"=n())

#Chungcheongbuk
chungcheongbuk_gangwon_business <-
  twen_region %>%
  filter(code_region==6) %>% 
  select(business_category)

chungcheongbuk_gangwon_service_job <-
  chungcheongbuk_gangwon_business %>%
  filter(business_category %in% c(58, 59, 60, 61, 62, 63, 70, 71, 72, 73, 74, 75, 85, 90, 91, 94, 95, 96)) %>% 
  summarise("인원"=n())

#JeollaAndJeju
jeolla_jeju_business <-
  twen_region %>%
  filter(code_region==7) %>% 
  select(business_category)

jeolla_jeju_service_job <-
  jeolla_jeju_business %>%
  filter(business_category %in% c(58, 59, 60, 61, 62, 63, 70, 71, 72, 73, 74, 75, 85, 90, 91, 94, 95, 96)) %>% 
  summarise("인원"=n())

seoul_service_job <- seoul_service_job %>% 
  mutate("지역" = "서울")
gyeonggi_service_job <- gyeonggi_service_job %>% 
  mutate("지역" = "경기도")
gyeongsangnam_service_job <- gyeongsangnam_service_job %>% 
  mutate("지역" = "경상남도")
gyeongsangbuk_service_job <- gyeongsangbuk_service_job %>% 
  mutate("지역" = "경상북도")
chungcheongnam_service_job <- chungcheongnam_service_job %>% 
  mutate("지역" = "충청남도")
chungcheongbuk_gangwon_service_job <- chungcheongbuk_gangwon_service_job %>% 
  mutate("지역" = "충청북도/강원도")
jeolla_jeju_service_job <- jeolla_jeju_service_job %>% 
  mutate("지역" = "전라도/제주도")

service_job <- bind_rows(seoul_service_job, gyeonggi_service_job, gyeongsangnam_service_job, gyeongsangbuk_service_job, chungcheongnam_service_job, chungcheongbuk_gangwon_service_job, jeolla_jeju_service_job)

ggplot(data = service_job, aes(x = reorder(지역, 인원), y = 인원)) + geom_col() + coord_flip()

## Map
devtools::install_github("cardiomoon/kormaps2014")
devtools::install_github("cardiomoon/moonBook2")
library(kormaps2014)

korpop1 <- korpop1

name <- c("서울", "경남", "경북", "경기", "전라/제주", "충남", "경남", "충남", "경기", "충북/강원", "충북/강원", "충남", "전라/제주", "전라/제주", "경북", "경남", "전라/제주")

pop <- c(19, 10, 3, 22, 12, 8, 10, 8, 22, 5, 5, 8, 12, 12, 3, 10, 12)

code <- c("11", "21", "22", "23", "24", "25", "26", "29", "31", "32", "33", "34", "35", "36", "37", "38", "39")

korpop <- data.frame(name, pop, code)

ggplot(korpop,aes(fill = pop, map_id=code, tooltip = name), interactive = T) +          
  geom_map(map=kormap1,colour="black",size=0.1) +
  expand_limits(x=kormap1$long,y=kormap1$lat) +
  scale_fill_gradientn(colours=c('white','orange','red')) +
  ggtitle("2015년도 도별 서비스 종사 분포") +
  coord_map()
