library(gganimate)
library(gifski)
source('req.R')

dt <- fread('API_NY.GDP.PCAP.CD_DS2_en_csv_v2_821084.csv', skip = 4, header = T)
dt <- dt[,c(1:2,45:62)]
colnames(dt) <- c('COUNTRY','CODE',paste0('YEAR_',colnames(dt[,3:13])))

meta <- fread('Metadata_Country_API_NY.GDP.PCAP.CD_DS2_en_csv_v2_821084.csv', skip = 0, header = T)
meta <- meta[,1:3]
colnames(meta) <- c('CODE','REGION','INCOME_GROUP')

dt <- merge(meta,dt)
rm(meta)

dt[dt == ''] <- NA
dt <- dt[complete.cases(dt),]


dt <- melt(dt, id.vars = c('CODE','REGION','INCOME_GROUP','COUNTRY'), 
           variable.name = 'YEAR',
           variable.factor = F,
           value.name = 'GDP_CAP')

dt[,YEAR := as.numeric(gsub('YEAR_','',YEAR))]
dt[,COUNTRY := factor(COUNTRY, levels = dt[YEAR == 2011][order(GDP_CAP)]$COUNTRY)]
dt[,INCOME_GROUP := factor(INCOME_GROUP,levels = c('Low income','Lower middle income','Upper middle income','High income'))]
dt[,YEAR := ymd(paste0(YEAR,'-01-01'))]

europe <- dt[REGION == 'Europe & Central Asia']
europe <- europe[COUNTRY %in% c('France','Germany','Italy','Spain','Poland','United Kingdom')]
g <- ggplot(europe ,aes(x = YEAR, y = GDP_CAP, color = COUNTRY)) + 
  geom_line(size=1) +
  geom_point(size = 2) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  #scale_y_continuous(seq(0,60000,10000)) +
  scale_color_manual(values = pal) +
  xlab('') +
  ylab('GDP/Capita') +
  ylim(0,60000) +
  t + transition_reveal(YEAR) +
  ease_aes('sine-in-out')


animate(g, duration = 10, fps = 20, width = 900, height = 600, renderer = gifski_renderer())
anim_save('animation.gif')
