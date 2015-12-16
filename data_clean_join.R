# load packages, set parameters ####
library(dplyr)
library(reshape2)
library(magrittr)
library(xlsx)
library(tidyr)

# clean environment
rm(list = ls())

## *** set directory for files *** ##
dir <- '~/Desktop/NYU/Econometrics/Project'
# open zip
unzip(file.path(dir, 'project_files.zip'), exdir = file.path(dir, 'project_files'))
dir <- file.path(dir, 'project_files')

# load clean merge fdi tables ####
load_clean = function(fdi, type) {
  # read csv
  fdi <- read.csv(fdi, sep = ',', head = TRUE, stringsAsFactors = FALSE, skip = 4)
  # strip junk first row
  fdi <- fdi[-1,]
  fdi %<>% mutate(YEAR = gsub('^\\s+', '', YEAR)) %>% # clear spaces from YEAR field
    melt(., id.vars = 'YEAR') %>% # get one row per YEAR per country
    mutate(variable = as.numeric(gsub('X', '', variable)), # remove X's from variable names
           value = as.numeric(value)) %>% # make value numeric
    setNames(., c('country', 'year', type)) # set column names
  return(fdi)
}

# execute above functions on fdi files
fdi_flow <- load_clean(file.path(dir, 'fdi_flows.csv'), 'fdi_flow')
fdi_stock <- load_clean(file.path(dir, 'fdi_stock.csv'), 'fdi_stock')

# join fdi files together
fdi <- inner_join(fdi_stock, fdi_flow) %>%
  # fix country names
  mutate(country = ifelse(country == "Korea, Dem. People's Rep. of", 'North Korea', country),
         country = ifelse(country == "Korea, Republic of", 'South Korea', country))
# drop original two tables
rm(fdi_flow, fdi_stock)

# load clean insutry table ####
industry <- read.csv(file.path(dir, 'FDI_FLOW_INDUSTRY_13112015175740102.csv'),
                     stringsAsFactors = FALSE)
# create lookup for industry codes
codes_industry <- industry %>% select(ECOACT, Industry) %>% unique

# clean up industry table
industry %<>% filter(CUR == 'USD') %>% # filter for records in USD
  # remove junk columns
  select(-YEA, -Reference.Period.Code, -Unit, -CUR,
         -Reference.Period, -PowerCode.Code, -Currency, -Unit.Code,
         -PowerCode, -Type.of.FDI, -Flags, -Flag.Codes, -Industry) %>%
  # rename columns
  rename(country = Reporting.country, year = Year) %>%
  # spread industry across the columns (1 row per country/year)
  spread(ECOACT, Value) %>%
  # fix country names
  mutate(country = ifelse(country == 'Korea', 'South Korea', country),
         country = ifelse(country == 'Slovak Republic', 'Slovakia', country))

# merge fdi and industry, Inflows data only
fdi <- industry %>% filter(FLOW == 'IN') %>%
  left_join(fdi, .) %>%
  # clean up country names
  mutate(country = gsub("\\([^\\)]+\\)","", country),
         country = gsub('\\s+', ' ', country),
         country = gsub('^\\s+|\\s+$', '', country),
         country = ifelse(grepl('Ivoire', country), 'Cote dIvoire', country))

# load, clean, join world bank indicators ####
wb_inds <- read.csv(file.path(dir, "wb_inds.csv"), stringsAsFactors=FALSE)
# drop junk cols, rename a couple fields
wb_inds %<>% select(-Time.Code) %>% rename(country = Country.Name, year = Time) %>%
  # remove junk countries/rollups
  filter(!(Country.Code %in% c('WLD', 'WBG', 'UMC', 'SSA', 'SSF',
                               'SAS', 'SST', 'PSS', 'OSS', 'OED',
                               'INX', 'NAC', 'MIC', 'MNA', 'MEA',
                               'LMC', 'LIC', 'LMY', 'LDC', 'LAC',
                               'LCN', 'OEC', 'HIC', 'HPC', 'FCS',
                               'EUU', 'ECA', 'ECS', 'EMU', 'EAP',
                               'EAS', 'CEB', 'CSS', 'ARB', 'NOC',
                               'PRI', 'MCO', 'KSV', 'LIE', 'CUW',
                               'CHI', 'IMY', 'MAF', 'VIR'))) %>%
  # clean up country names (whitespace, useless abbrevs, etc)
  mutate(country = gsub('The|Fed\\.|Sts\\.|[[:punct:]]', '', country),
         country = gsub("\\([^\\)]+\\)","", country),
         country = gsub('^\\s+|\\s+$', '', country),
         country = gsub(' Dutch part', '', country),
         country = gsub('^St ', 'Saint ', country)) %>%
  # rename countries to match other tables
  mutate(country = ifelse(country == 'Taiwan China', 'China, Taiwan Province of', country),
         country = ifelse(country == 'Tanzania', 'United Republic of Tanzania', country),
         country = ifelse(country == 'Venezuela RB', 'Venezuela', country),
         country = ifelse(country == 'Vietnam', 'Viet Nam', country),
         country = ifelse(country == 'Slovak Republic', 'Slovakia', country),
         country = ifelse(country == 'Moldova', 'Republic of Moldova', country),
         country = ifelse(country == 'Korea Rep', 'South Korea', country),
         country = ifelse(country == 'Korea Dem Rep', 'North Korea', country),
         country = ifelse(country == 'TimorLeste', 'Timor-Leste', country),
         country = ifelse(country == 'Macedonia FYR', 'TFYR of Macedonia', country),
         country = ifelse(country == 'Lao PDR', "Lao People's Dem. Rep.", country),
         country = ifelse(country == 'Macao SAR China', 'China, Macao SAR', country),
         country = ifelse(country == 'Kyrgyz Republic', 'Kyrgyzstan', country),
         country = ifelse(country == 'Congo Rep', 'Congo', country),
         country = ifelse(country == 'Congo Dem Rep', 'Dem. Rep. of the Congo', country),
         country = ifelse(country == 'Iran Islamic Rep', 'Iran', country),
         country = ifelse(country == 'Hong Kong SAR China', 'China, Hong Kong SAR', country),
         country = ifelse(country == 'Egypt Arab Rep', 'Egypt', country),
         country = ifelse(country == 'GuineaBissau', 'Guinea-Bissau', country),
         country = ifelse(country == 'Yemen Rep', 'Yemen Arab Republic', country))
# join wb_inds with other table, filter for 2000+ (well populated records)
fdi_full <- left_join(fdi, wb_inds) %>% filter(year > 1999)
# fix column names that are only numeric
colnames(fdi_full) <- paste0(ifelse(gsub('[[:digit:]]', '', colnames(fdi_full)) %in% c('', 'D'), 'X', ''), colnames(fdi_full))

# flag oecd countries
fdi_full$oecd_ind <- ifelse(fdi_full$country %in% unique(industry$country), 1, 0)

# load clean join IMF outlook ####
imf <- read.csv("~/Desktop/NYU/Econometrics/Project/project_files/IMF_outlook.csv", stringsAsFactors=FALSE)
meas_lkup <- data.frame(Subject.Descriptor = c("Gross domestic product, constant prices",
                                               "Inflation, average consumer prices",
                                               "Current account balance"),
                        measure = c('GDP', 'Inflation', 'CAB'),
                        stringsAsFactors = FALSE)
imf %<>% inner_join(., meas_lkup) %>%
  dplyr::select(-Subject.Descriptor, -Units, -Scale, -Country.Series.specific.Notes,
                -Estimates.Start.After) %>%
  melt(., c('Country', 'File', 'measure')) %>%
  mutate(measure = paste0(measure, variable)) %>%
  dplyr::select(-variable) %>%
  spread(measure, value)
imf[,-c(1:2)] <- sapply(imf[,-c(1:2)], function(x) as.numeric(x))
imf %<>% filter(complete.cases(.)) %>%
  rename(country = Country, year = File) %>%
  filter(country != 'Kosovo') %>%
  mutate(country = ifelse(country == 'Islamic Republic of Iran', 'Iran', country),
         country = gsub('^The ', '', country),
         country = ifelse(country == 'Vietnam', 'Viet Nam', country),
         country = ifelse(country == 'Russia', 'Russian Federation', country),
         country = ifelse(country == "C\xaate d'Ivoire", 'Cote dIvoire', country),
         country = ifelse(country == 'Cape Verde', 'Cabo Verde', country),
         country = ifelse(country == 'Democratic Republic of Congo', 'Dem. Rep. of the Congo', country),
         country = gsub('Islamic Republic of ', '', country),
         country = ifelse(country == 'Korea', 'South Korea', country),
         country = ifelse(country == 'Hong Kong SAR', 'China, Hong Kong SAR', country),
         country = ifelse(country == 'Kyrgyz Republic', 'Kyrgyzstan', country),
         country = ifelse(country == "Lao People's Democratic Republic", "Lao People's Dem. Rep.", country),
         country = ifelse(country == "Moldova", "Republic of Moldova", country),
         country = ifelse(country == "Republic of Congo", "Congo", country),
         country = ifelse(country == "Republic of Yemen", "Yemen, Arab Republic", country),
         country = ifelse(country == 'S\xdco Tom_ and Pr\xd5ncipe', 'Sao Tome and Principe', country),
         country = ifelse(country == 'Slovak Republic', 'Slovakia', country),
         country = ifelse(country == 'Tanzania', 'United Republic of Tanzania', country),
         country = ifelse(country == 'Taiwan Province of China', 'China, Taiwan Province of', country),
         country = gsub('^St. ', 'Saint ', country))
keep <- as.character(usdm::vifstep(imf[,-c(1:2)], th = 4)@results$Variables)
imf <- imf[,c('country', 'year', keep)]
fdi_full %<>% left_join(., imf)

# extract variables for model ####
extract <- fdi_full %>%
  select(country, year, fdi_stock, fdi_flow,
         fdi_over_gdp = BX.KLT.DINV.WD.GD.ZS,
         starts_with('GDP'),
         starts_with('Inflation'),
         warehouse = IC.WRH.PROC,
         corp_tax = IC.TAX.TOTL.CP.ZS,
         education = SE.PRM.GINT.ZS,
         electricity = EG.ELC.ACCS.ZS,
         internet = IT.NET.BBND.P2) %>%
  filter(year > 2006) %>%
  mutate(country = gsub('[^[:alpha:][:space:]]', '', toupper(country)))

# drop junk
rm(list = ls()[ls() != 'extract'])

# exchange rates ####
REER <- read.csv("~/Desktop/NYU/Econometrics/Project/project_files/REER.csv", stringsAsFactors = FALSE)
REER %<>% mutate(country = toupper(gsub('[^[:alpha:][:space:]]', '', Country)),
                 country = gsub('^ST ', 'SAINT ', country),
                 country = gsub(' RB$|NETHERLANDS ANTILLES | THE$| REP$| ARAB REP$| ISLAMIC REP$| MAINLAND$', '', country),
                 country = gsub('CAPE', 'CABO', country),
                 country = gsub('CONGO DEM', 'DEM REP OF THE CONGO', country),
                 country = gsub('^CTE ', 'COTE ', country),
                 country = gsub('HONG KONG CHINA', 'CHINA HONG KONG SAR', country),
                 country = gsub('PDR', 'PEOPLES DEM REP', country),
                 country = gsub('MOLDOVA', 'REPUBLIC OF MOLDOVA', country),
                 country = gsub('VIETNAM', 'VIET NAM', country),
                 country = gsub('KYRGYZ REPUBLIC', 'KYRGYZSTAN', country),
                 country = gsub('KOREA', 'SOUTH KOREA', country),
                 country = gsub('MACEDONIA FYR', 'TFYR OF MACEDONIA', country),
                 country = gsub('SLOVAK REPUBLIC', 'SLOVAKIA', country),
                 country = gsub('TAIWAN', 'CHINA TAIWAN PROVINCE OF', country)) %>%
  select(-Country) %>%
  melt(., 'country') %>%
  mutate(year = as.numeric(gsub('X', '', variable))) %>%
  select(country, year, exchange_rate = value)
extract %<>% left_join(., REER)
rm(REER)

# WGI ####
wgi <- read.csv("~/Desktop/NYU/Econometrics/Project/project_files/WGI_trade_unemployment.csv",
                                   stringsAsFactors=FALSE)
wgi %<>% mutate(country = toupper(gsub('[^[:alpha:][:space:]]', '', Country))) %>%
  select(-Country, -Country.Code) %>%
  filter(!(country %in% c('WEST BANK AND GAZA', 'PUERTO RICO'))) %>%
  mutate(country = gsub(' THE$| RB$| ARAB REP$', '', country),
         country = gsub('CONGO REP', 'CONGO', country),
         country = gsub('KYRGYZ REPUBLIC', 'KYRGYZSTAN', country),
         country = gsub('TANZANIA', 'UNITED REPUBLIC OF TANZANIA', country),
         country = gsub('SLOVAK REPUBLIC', 'SLOVAKIA', country),
         country = gsub(' PDR$', ' PEOPLES DEM REP', country),
         country = gsub('VIETNAM', 'VIET NAM', country),
         country = gsub('MACEDONIA FYR', 'TFYR OF MACEDONIA', country),
         country = gsub('MOLDOVA', 'REPUBLIC OF MOLDOVA', country),
         country = gsub('HONG KONG SAR CHINA', 'CHINA HONG KONG SAR', country),
         country = gsub('MACAO SAR CHINA', 'CHINA MACAO SAR', country),
         country = gsub('KOREA REP', 'SOUTH KOREA', country),
         country = gsub('CONGO DEM REP', 'DEM REP OF THE CONGO', country))
extract %<>% left_join(., wgi)
rm(wgi)

# development groups ####
country_groups <- read.csv("~/Desktop/NYU/Econometrics/Project/project_files/country_groups.csv",
                           stringsAsFactors=FALSE)
country_groups %<>%
  rename(country = Income) %>%
  mutate(country = toupper(gsub('[[:punct:]]', '', country)),
         country = gsub('VIETNAM', 'VIET NAM', country),
         country = gsub('RUSSIA', 'RUSSIAN FEDERATION', country),
         country = gsub('SYRIAN', 'SYRIAN ARAB REPUBLIC', country),
         country = gsub('TANZANIA', 'UNITED REPUBLIC OF TANZANIA', country),
         country = gsub('AFTICAN', 'AFRICAN', country),
         country = gsub('MACEDONIA', 'TFYR OF MACEDONIA', country),
         country = gsub('MOLDOVA', 'REPUBLIC OF MOLDOVA', country),
         country = gsub('REP OF CONGO', 'CONGO', country),
         country = gsub('LAO PDR', 'LAO PEOPLES DEM REP', country)) %>%
  select(country, group = Group)
extract %<>% left_join(., country_groups)
rm(country_groups)

write.table(extract, '~/Desktop/NYU/Econometrics/Project/project_files/fdi_full.csv', sep = ',', col.names = TRUE, row.names = FALSE, quote = FALSE)
