# 美国数据科学家工作职位分析(usdata_scientist)

# 读取数据
usdata_scientist <- read.csv("E:/mine/R/usdata_scientist858/usdata_scientist.csv")
# 查看缺失值
library(VIM)
aggr(usdata_scientist,prop=T,numbers=T)
# 分隔列，存储为city和state，并保留原有列
library(tidyverse)
usdata_scientist_new <- separate(data = usdata_scientist, col = location, into = c("city", "state"), sep = ", ", remove = FALSE)
# 去掉state列的邮编数字
usdata_scientist_new$state <- gsub('[0-9]*|\\s','',usdata_scientist_new$state)
# 转换为因子类型
usdata_scientist_new$position <- factor(tolower(usdata_scientist_new$position))
usdata_scientist_new$state <- factor(usdata_scientist_new$state)
usdata_scientist_new$city <- factor(usdata_scientist_new$city)
# 转换description为字符型，用于文本挖掘
usdata_scientist_new$description <- as.character(usdata_scientist_new$description)
str(usdata_scientist_new)

# 数据分析
# 1 观察数据的总体描述统计信息
summary(usdata_scientist_new)

# 2 各个州拥有的职位数情况
# 对state计数
library(plyr)
state_freq <- count(usdata_scientist_new$state)
state_freq
library(ggplot2)
# 重命名列名
names(state_freq) <- c("State","Freq")
library(ggthemes)
ggplot(state_freq,aes(x = reorder(State,-Freq), y= Freq, fill = State))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=Freq),vjust=-0.2,size=3)+ # 添加数值标签
  xlab("State")+ # 修改x轴坐标文本
  theme_bw() #添加主题

# 3 职位在各个州的地理分布情况
library(maps)
library(mapdata)
par(mar=rep(0,4))
# 经纬度数据
dat = read.csv(text = "state-capital,jd,wd
               California-Sacramento,-121.4943996,38.5815719
               Colorado-Denver,-104.990251,39.7392358
               Washington DC,-77.0368707,38.9071923
               Georgia-Atlanta,-84.3879824,33.7489954
               Illinois-Springfield,-72.5898110,42.1014831
               Massachusetts-Boston,-71.0588801,42.3600825
               New Jersey-Trenton,-74.7597170,40.2205824
               New York-Albany,-73.7562317,42.6525793
               Texas-Austin,-97.7430608,30.2671530
               Washington-Olympia,-122.9006951,47.0378741")
# 地图
map("state", col = "lightblue", fill = TRUE, ylim = c(18, 54), panel.first = grid())
# 地图上显示的点
points(dat$jd, dat$wd, pch = 19, cex = 1.5, col = rgb(0,0, 0, 0.5))
# points(dat$jd, dat$wd, pch = 19, cex = 1.5, col = c("blue","red","green","blue","red","blue","blue","red","red","green"))
# 地图上显示的文本；pos：文本显示位置（1,2,3,4-下，左，上，右）
text(dat$jd, dat$wd, dat[, 1], cex = 0.7, col = rgb(0,0, 0, 0.7),
     pos = c(1,2,1,1,3,2,2,1,1,1))
# 四周的坐标轴
axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)

# 4 CA、MA、NY、WA中各城市的职位数
# 4.1 CA州各城市的职位数
# 选取CA的子集
CA_df <- usdata_scientist_new[which(usdata_scientist_new$state=="CA"),]
CA_city_freq <- count(CA_df$city)
CA_city_freq
ggplot(CA_city_freq,aes(x=x,y=freq,fill=x))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=freq),vjust=-0.2,size=3)+ # 添加数值标签
  theme(axis.text.x = element_text(angle = 90))+ # x轴文本旋转90度
  xlab("CA-city")+ # 修改x轴坐标文本
  guides(fill=FALSE) # 删除图例

# 4.2 MA州各城市的职位数
# 选取MA的子集
MA_df <- usdata_scientist_new[which(usdata_scientist_new$state=="MA"),]
MA_city_freq <- count(MA_df$city)
MA_city_freq
ggplot(MA_city_freq,aes(x=x,y=freq,fill=x))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=freq),vjust=-0.2,size=3)+ # 添加数值标签
  theme(axis.text.x = element_text(angle = 90))+ # x轴文本旋转90度
  xlab("MA-city")+ # 修改x轴坐标文本
  guides(fill=FALSE) # 删除图例

# 4.3 WA州各城市的职位数
# 选取WA的子集
WA_df <- usdata_scientist_new[which(usdata_scientist_new$state=="WA"),]
WA_city_freq <- count(WA_df$city)
WA_city_freq
ggplot(WA_city_freq,aes(x=x,y=freq,fill=x))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=freq),vjust=-0.2,size=3)+ # 添加数值标签
  theme(axis.text.x = element_text(angle = 90))+ # x轴文本旋转90度
  xlab("WA-city")+ # 修改x轴坐标文本
  guides(fill=FALSE) # 删除图例

# 4.4 NY州各城市的职位数
# 选取NY的子集
NY_df <- usdata_scientist_new[which(usdata_scientist_new$state=="NY"),]
NY_city_freq <- count(NY_df$city)
NY_city_freq
ggplot(NY_city_freq,aes(x=x,y=freq,fill=x))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=freq),vjust=-0.2,size=3)+ # 添加数值标签
  theme(axis.text.x = element_text(angle = 90))+ # x轴文本旋转90度
  xlab("NY-city")+ # 修改x轴坐标文本
  guides(fill=FALSE) # 删除图例

# 5 分析各公司对数据科学家岗位的需求
# 统计频数
company_pos <- count(usdata_scientist_new$company)
# 各公司发布的职位数降序
library(dplyr)
#排序
company_pos_desc <- arrange(company_pos, desc(freq))
# 取前15名
company_pos_top <- company_pos_desc[1:15,]
ggplot(company_pos_top,aes(x=reorder(x,-freq),y=freq,fill=x))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))+ #x轴文本旋转90度
  geom_text(aes(label=freq),vjust=-0.2,size=2.5)+
  xlab("company-top15")+
  guides(fill=FALSE)

# 6 各个州的数据科学家热度的总体情况
# 过滤掉reviews为NA的行
reviews_com <- usdata_scientist_new[complete.cases(usdata_scientist_new[,5]),]
ggplot(reviews_com,aes(x = state,y = reviews))+
  geom_boxplot(colour = rainbow(10))+ # 调色板
  ylim(0,max(reviews_com$reviews)) # 设置y轴精度
ggplot(reviews_com,aes(x = state,y = reviews))+
  geom_boxplot(colour = rainbow(10))+ # 调色板
  ylim(0,10000) # 设置y轴精度

# 7 数据科学家职位描述的关键词
# 文本挖掘
library(tidytext)
tidy_words <- usdata_scientist_new %>%
  unnest_tokens(word,description) %>% # 分词
  anti_join(stop_words) # 去停用词
head(tidy_words)
word_freq <- tidy_words%>%
  count(word,sort=TRUE) # 计算词频并排序
head(word_freq)

# 7.1词频统计图
library(ggplot2)
word_freq %>% # 统计词频
  filter(n > 5000) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,fill=word))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  guides(fill=FALSE)+ # 删除图例
  
# 7.2 词云图
library(wordcloud2)
wc_freq <- word_freq %>% filter(n > 3000) # 取词频大于3000的词画云图
wordcloud2(wc_freq,size=1,shape = 'star')

# 8 数据科学家职位对工具型技能的要求
seg <- usdata_scientist_new %>%
  unnest_tokens(word,description) # 分词
# 转换为大写
seg$word <- toupper(seg$word)
# 只保留工具型技能的词
tools_df <- seg[which(seg$word %in% c("SQL","R","PYTHON",
                                      "EXCEL","SAS","SPSS","HIVE",
                                      "PPT","HADOOP","MYSQL",
                                      "SPARK","ORACLE","BI",
                                      "KPI","JAVA")),]
# 转换为因子类型
tools_df$word <- factor(tools_df$word)
# 求解频数
tools_freq <- count(tools_df$word)
# 降序输出
library(dplyr)
arrange(tools_freq, desc(freq))
# 计算占比
tools_freq$freq <- tools_freq$freq/nrow(usdata_scientist_new)
ggplot(tools_freq) + 
  geom_bar(aes(x=reorder(x,-freq),y=freq),fill="lightblue",stat = "identity") + 
  labs(x="工具型技能",y="不同技能需求占总职位需求量的比率") +
  theme(axis.text.x = element_text(angle = 30,hjust = 1))+
  geom_text(aes(x=x,y=freq,label=paste(round(tools_freq$freq,3)*100,'%',sep = '')),vjust=-0.2,size=3.5)+
  scale_y_continuous(labels = scales::percent) +
  guides(fill=FALSE)

# 9 分析不同公司对数据科学家职位工具型技能的要求
# 工具型技能词汇集合
tools <- c("SQL","R","PYTHON",
           "EXCEL","SAS","SPSS","HIVE",
           "PPT","HADOOP","MYSQL",
           "SPARK","ORACLE","BI",
           "KPI","JAVA")
# 发布职位数前10的公司
company_10 <- c("Amazon.com","Ball Aerospace","Microsoft","Google",
                "NYU Langone Health","Fred Hutchinson Cancer Research Center",
                "KPMG","Lab126","Broad Institute","Facebook")
company_df <- usdata_scientist_new[which(usdata_scientist_new$company %in% company_10),]
seg_company <- company_df %>%
  unnest_tokens(word,description) # 分词
# 转换为大写
seg_company$word <- toupper(seg_company$word)
# 只保留工具型技能的词
tools_company <- seg_company[which(seg_company$word %in% tools),]
# 转换为因子类型
tools_company$word <- factor(tools_company$word)
head(tools_company)
# 求解频数并按company分面可视化
ggplot(tools_company,aes(x=word,fill=word)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  facet_wrap(~company) + #分面
  guides(fill=FALSE)

