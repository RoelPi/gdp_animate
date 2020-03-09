library(gganimate)
source('req.R')

dt <- fread('API_NY.GDP.PCAP.CD_DS2_en_csv_v2_821084.csv', skip = 4, header = T)
dt <- dt[,c(1:2,52:62)]
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

ggplot(dt[YEAR == 2011,],aes(x = COUNTRY, y = GDP_CAP, fill = INCOME_GROUP)) + 
  geom_bar(stat = 'identity') + t


  facet_wrap(~REGION) +
  transition_states(YEAR,
                    transition_length = 2,
                    state_length = 1) +
  ease_aes('sine-in-out') +
  t
