library("tidyverse")
library("psych")
library("ggthemes")
library("lubridate")
library("scales")
library("ggrepel")
theme_set(theme_few(base_size = 14))
setwd("~/MSBA/MOD5")

allinv <- read.csv("newdata/181920invoice.csv")
# invoice <- read.csv('newdata/Vast_Invoices_010118_123120.csv')



allinv$Line.Cost[is.na(allinv$Line.Cost)] = 0
allinv$Total.Line.Revenue[is.na(allinv$Total.Line.Revenue)] = 0
allinv$Revenue.Less.Cost[is.na(allinv$Revenue.Less.Cost)] = 0

allinv$CUST_CREATE_DATE <- lubridate::mdy(allinv$CUST_CREATE_DATE)
allinv$INVOICE_DATE <- lubridate::ymd(allinv$INVOICE_DATE)
allinv$InvoiceYear <- year(allinv$INVOICE_DATE)
allinv <- allinv %>% mutate(createYear =year(CUST_CREATE_DATE))

# allinv$INVOICE_DATE <- as.factor(allinv$INVOICE_DATE)
allinv$InvoiceYear <- as.factor(allinv$InvoiceYear)

marginByYr=allinv %>%  group_by(InvoiceYear) %>% 
  summarise(Rev=sum(Total.Line.Revenue), Profit = sum(Revenue.Less.Cost), Cost=sum(Line.Cost))

# marginByYr$margin = marginByYr$Profit/marginByYr$Rev
marginByYr$margin = round((marginByYr$Rev - marginByYr$Cost)/marginByYr$Rev,3)

ggplot(marginByYr, aes(x=InvoiceYear, y=Rev, fill=InvoiceYear)) + geom_col(alpha=.4) +
  geom_col(data=marginByYr, aes(x=InvoiceYear, y=Profit, fill=InvoiceYear)) +
  scale_fill_brewer(palette = "Dark2", labels = c("2018 Profit", "2019 Profit", "2020 Profit")) +
  scale_y_continuous(labels = label_comma(accuracy = NULL, scale = .000001, 
                                          prefix = "$", suffix = "mm",
                                          big.mark = ",")) +
  geom_text(data=marginByYr, 
             aes(x=InvoiceYear, y=Rev, label=paste0(round(margin*100,1),'%')), nudge_y = 2000000) +
  labs(x="",y="Revenue",fill="", title="Revenue / Profit by Year", caption = "label=margin%")


allinv <- allinv %>% 
  mutate(CLASS_Desc = case_when( CLASS == 1 ~ 'Goodyear Tires',
                            CLASS == 2 ~ 'Pirellie Tire',
                            CLASS == 3 ~ 'Hankook Tires',
                            CLASS == 4 ~ 'Dunlop Tires',
                            CLASS == 5 ~ 'Republic Tires',
                            CLASS == 6 ~ 'Kelly Tires',
                            CLASS == 7 ~ 'Michelin Tires',
                            CLASS == 8 ~ 'BF Goodrich Tires',
                            CLASS == 9 ~ 'Uniroyal Tires',
                            CLASS == 10 ~ 'Falken Tires',
                            CLASS == 11 ~ 'Nexon Tire',
                            CLASS == 12 ~ 'Bridgestone Tires',
                            CLASS == 13 ~ 'Firestone Tires',
                            CLASS == 14 ~ 'Continental Tires',
                            CLASS == 15 ~ 'General Tires',
                            CLASS == 16 ~ 'Toyo Tires',
                            CLASS == 17 ~ 'Cooper Tire',
                            CLASS == 18 ~ 'Fuzion Tires',
                            CLASS == 19 ~ 'Nitto Tires',
                            CLASS == 20 ~ 'Yoko Tires',
                            CLASS == 21 ~ 'Kumho tires',
                            CLASS == 24 ~ 'Other Tires',
                            CLASS == 26 ~ 'Tire Labor',
                            CLASS == 27 ~ 'Tire Disposal',
                            CLASS == 28 ~ 'Road Hazard Sales',
                            CLASS == 29 ~ 'Sport Wheels',
                            CLASS == 30 ~ 'Wheels Accessories',
                            CLASS == 31 ~ 'Alignments',
                            CLASS == 32 ~ 'LOF Services',
                            CLASS == 33 ~ 'Tune-Up/Filters',
                            CLASS == 34 ~ 'Belts/Cooling',
                            CLASS == 35 ~ 'Electrical',
                            CLASS == 36 ~ 'Batteries',
                            CLASS == 37 ~ 'Brakes',
                            CLASS == 38 ~ 'Fuel/Emissions Work',
                            CLASS == 39 ~ 'Heat/AC',
                            CLASS == 40 ~ 'Engine',
                            CLASS == 41 ~ 'Drivetrain',
                            CLASS == 42 ~ 'Suspension/Steering',
                            CLASS == 43 ~ 'Exhaust',
                            CLASS == 44 ~ 'Transmission',
                            CLASS == 45 ~ 'Safety Inspections',
                            CLASS == 46 ~ 'Emission Testing',
                            CLASS == 47 ~ 'Other Disposal Fees',
                            CLASS == 48 ~ 'Shop Supplies',
                            CLASS == 49 ~ 'NATL OIL FILTER DISPOSAL FEES',
                            CLASS == 50 ~ 'Miscellaneous',
                            CLASS == 52 ~ 'Coupons/Discounts',
                            CLASS == 53 ~ 'VA Tire Tax',
                            CLASS == 55 ~ 'Road Hazard Warranty',
                            CLASS == 56 ~ 'Federal Excise Tax',
                            CLASS == 57 ~ 'Tire Coupons',
                            CLASS == 59 ~ 'Service Interval',
                            TRUE ~ 'NA'))

allinv$CLASS <- as.factor(allinv$CLASS)
allinv$CLASS_Desc <- as.factor(allinv$CLASS_Desc)
# allinv$CUST_CREATE_DATE <- ymd(allinv$CUST_CREATE_DATE)

# Summarize rev, profit, cost by CLASS
marginByClass <- allinv %>% group_by(CLASS,CLASS_Desc)%>% 
  summarise(Rev=sum(Total.Line.Revenue), Profit = sum(Revenue.Less.Cost), Cost=sum(Line.Cost))

# Drop NAs, calculate margin
marginByClass <- marginByClass[!(marginByClass$CLASS_Desc=="NA"),]
marginByClass$margin = round((marginByClass$Rev - marginByClass$Cost)/marginByClass$Rev,3)
marginByClass <- marginByClass %>% arrange(desc(margin))

write.csv(marginByClass, 'marginbyclass.csv')

## TIRES
#------------------
tireMargins <- marginByClass %>%  filter(str_detect(CLASS_Desc,"Tire|tire"))
tireMargins <- tireMargins %>% select(CLASS_Desc, Rev, Profit, margin)
write.csv(tireMargins, 'tireMargins.csv')
tireMargins <- tireMargins %>% pivot_longer(c("Rev", "Profit"), names_to="Metric")
tireMarginsRev <- tireMargins %>% filter(Metric=="Rev")
tireMarginsRev
tireMarginsPro <- tireMargins %>% filter(Metric=="Profit")
tireMarginsPro$Metric[tireMarginsPro$value<0] = "Coupon"

ggplot(tireMarginsRev, aes(x=reorder(CLASS_Desc, margin), y=value, fill=Metric)) + geom_col() +
  geom_col(data=tireMarginsPro, aes(x=CLASS_Desc, y=value, fill=Metric), show.legend = FALSE) + coord_flip() +
  scale_fill_manual(values = c("red4","green4","grey76")) +
  scale_y_continuous(labels = label_comma(accuracy = NULL, scale = .000001, 
                                          prefix = "$", suffix = "mm",
                                          big.mark = ",")) +
  geom_text(data=tireMarginsRev,
            aes(x=CLASS_Desc, y=value, label=paste0(round(margin*100,1),'%')), nudge_y = 500000) +
  labs(x="",y="Revenue",fill="", title="Tire Sale Margins",
       legend="test") + theme(legend.box = 'horizontal')

## OTHER
#------------------
serviceMargins <- marginByClass %>%  filter( str_detect(CLASS_Desc,"Tire|tire", negate=TRUE))
serviceMargins <- serviceMargins %>% select(CLASS_Desc, Rev, Profit, margin)
write.csv(serviceMargins, 'serviceMargins.csv')
serviceMargins <- serviceMargins %>% pivot_longer(c("Rev", "Profit"), names_to="Metric")
serviceMarginsRev <- serviceMargins %>% filter(Metric=="Rev")
serviceMarginsRev
serviceMarginsPro <- serviceMargins %>% filter(Metric=="Profit")
serviceMarginsPro$Metric[serviceMarginsPro$value<0] = "Loss"

ggplot(serviceMarginsRev, aes(x=reorder(CLASS_Desc, margin), y=value, fill=Metric)) + geom_col() +
  geom_col(data=serviceMarginsPro, aes(x=CLASS_Desc, y=value, fill=Metric), show.legend = FALSE) + coord_flip() +
  scale_fill_manual(values = c("red4","green4","grey76")) +
  scale_y_continuous(labels = label_comma(accuracy = NULL, scale = .000001, 
                                          prefix = "$", suffix = "mm",
                                          big.mark = ",")) +
  geom_text(data=serviceMarginsRev,
            aes(x=CLASS_Desc, y=value, label=paste0(round(margin*100,1),'%')), nudge_y = 1000000) +
  labs(x="",y="Revenue",fill="", title="VTA Service Margins",
       legend="test") + theme(legend.box = 'horizontal')

##### CLV

clv <- allinv

clv <- clv %>% filter(CUST_CREATE_DATE >= "2018-01-01")
newCust2018 <- clv %>% filter(CUST_CREATE_DATE <= "2018-12-31")


# write.csv(newCust2018, "2018cust.csv")


newCust2018$createMonth <- as.factor(month(newCust2018$CUST_CREATE_DATE))


allinv %>% 
  ggplot(aes(x=InvoiceYear, y=Total.Line.Revenue, fill=CLASS_Desc)) + geom_col() +
    scale_y_continuous(labels = label_comma(accuracy = NULL, scale = .000001, 
                                            prefix = "$", suffix = "mm",
                                            big.mark = ",")) 







newCust2018 %>%  filter( str_detect(CLASS_Desc,"Tire|tire", negate=TRUE)) %>% 
  ggplot(aes(x=CLASS_Desc, y=Total.Line.Revenue, fill=CLASS_Desc)) + geom_col() +
  scale_y_continuous(labels = label_comma(accuracy = NULL, scale = .000001, 
                                          prefix = "$", suffix = "mm",
                                          big.mark = ",")) 














