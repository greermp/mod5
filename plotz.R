library("tidyverse")
library("psych")
library("ggthemes")
library("lubridate")
library("scales")
library("ggrepel")
theme_set(theme_tufte(base_size = 14))

setwd("~/MSBA/MOD5")
# source("VATA_Invoice.R")
invoice <- read.csv('data/invoice_clean.csv')

avgByBrand=invoice %>% group_by(BrandSum) %>% summarise(count=n(),AvgInvoice=mean(Amount)) %>% 
  arrange(desc(AvgInvoice))

avgByBrand %>% filter(count>=500) %>% slice_max(AvgInvoice,n=10) %>% 
  ggplot(aes(x=reorder(BrandSum, -AvgInvoice), y=AvgInvoice)) + geom_col() +
  theme(axis.text.x = element_text(angle=45,hjust=1, vjust=.9)) +
  labs(x="Make", y="Avg Invoice", title="Highest Average Invoice by Make",
       caption = "Only showing makes with > 500 invoices")
ggsave("plots/avgInvoice.png")

invoice %>% select(BrandSum,Amount) %>% 
  group_by(BrandSum) %>% mutate(avg=mean(Amount), num=n()) %>% 
  arrange(desc(avg)) %>% 
  filter(Amount>0 & Amount <2000 & ! is.na(BrandSum) & num>3000) %>% 
  ggplot(aes(x=BrandSum, y=Amount)) +
  geom_boxplot() +
  labs(x="", y="Invoice Amount", title="There are lots of outliers!", caption = "Only Brands w/ > 3000 invoices pictured")
ggsave("plots/boxplotCostByMake.png")


## Rev

invoice %>%  group_by(Date) %>% 
  mutate(Rev = sum(Amount)) %>% 
  ggplot(aes(x=Date, y=Rev)) + geom_smooth() + 
  facet_wrap(~year, ncol=1) +
  scale_x_date(breaks = seq(as.Date("2019-01-01"), as.Date("2021-12-30"), by="6 months"), date_labels = "%b\n%Y")

  scale_x_date(breaks = "3 months", minor_breaks = "1 month", labels=date_format("%B"))
  
invoice$MODEL.YEAR <- as.factor(invoice$MODEL.YEAR)


invoice %>% group_by(MODEL.YEAR) %>% 
  summarise(num=n(),AvgInvoice=mean(Amount)) %>% filter(num>250) %>% 
  ggplot(aes(x=as.factor(reorder(MODEL.YEAR,-num)), y=num)) + geom_col() +
  geom_text(aes(x = MODEL.YEAR, y=num, label = paste0('$',round(AvgInvoice,0))), nudge_y = 250) +
  theme(axis.text.x.bottom = element_text(angle=45, hjust = 1, vjust = .9)) +
  labs(x="Make", y="Invoices per Make", title="Most Common Vehicle Serviced, by Year",
       caption = "Label = Average Invoice")
ggsave("plots/VisitsBy_WithAvgInvoice.png")



invoice %>% group_by(Date) %>% mutate(Revenue = sum(Amount)) %>% 
  ggplot(aes(x=Date, y=Revenue, color=year)) + geom_line() +
  facet_wrap(~year, nrow = 3) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b")


# visits=invoice %>% group_by(VIN, Date) %>% mutate(visits = n()) %>% filter(visits>1)

invoice %>% group_by(BrandSum) %>% summarise(count=n(),AvgInvoice=mean(Amount)) %>%
  arrange(desc(count)) %>% slice_max(count,n=10) %>% 
  ggplot(aes(x=reorder(BrandSum, -count), y=count)) + geom_col() +
  theme(axis.text.x = element_text(angle=45,hjust=1, vjust=.9)) +
  geom_label(aes(y = count, x=BrandSum, label = paste0('$',round(AvgInvoice))), nudge_y = 1000) +
  labs(x="Make", y="Invoices per Make", title="Most Common Vehicle Serviced, by Make",
       caption = "Label = Average Invoice")
ggsave("plots/VisitsPerBrand_WithAvgInvoice.png")

invoice %>% group_by(BrandSum) %>% summarise(count=n(),AvgInvoice=mean(Amount)) %>%
  arrange(desc(AvgInvoice))%>% filter(count>=200) %>% slice_max(AvgInvoice,n=10) %>%
  ggplot(aes(x=reorder(BrandSum, -AvgInvoice), y=AvgInvoice)) + geom_col() +
  theme(axis.text.x = element_text(angle=45,hjust=1, vjust=.9)) +
  geom_label(aes(y = 360, x=BrandSum, label = paste0(count))) +
  labs(x="Make", y="Avg Invoice", title="Highest Average Invoice by Make",
       subtitle="Label = Number of Invoices")
ggsave("plots/BrandAvgInvoice.png")


byServ <- invoice %>% group_by(ServiceReason) %>%
  summarise(number=n(), avgRev=mean(Amount))

byServ <- byServ %>%
  arrange(desc(ServiceReason)) %>%
  mutate(prop = number / sum(byServ$number) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

ggplot(byServ, aes(x="", y=prop, fill=ServiceReason)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position="none") +
  geom_label(aes(y = ypos, x=1.7, label = ServiceReason), color = "black", size=3, fill="white") +
  scale_fill_brewer(palette="Set1") +
  labs(title="Reasons for VTA Customer Visits") +
  scale_fill_brewer(palette="Dark2")
ggsave("plots/ServicePie.jpg")

invoice %>% group_by(ServiceReason) %>%
  summarise(Revenue = sum(Amount)) %>%
  ggplot(aes(x=reorder(ServiceReason, -Revenue), y=Revenue, fill=ServiceReason)) + geom_col( show.legend = FALSE) +
  scale_fill_brewer(palette="Dark2") +
  scale_y_continuous(labels = scales::number_format(big.mark = ',', prefix = '$')) +
  labs(x="") + theme(axis.text.x = element_text(angle=45,hjust=1, vjust=.9)) +
  labs(title="Total Revenue By Service Reason", caption = paste(min(invoice$Date), " to ", max(invoice$Date)))  +
  scale_fill_brewer(palette="Dark2")
ggsave("plots/revPerService.jpg")

invoice %>% group_by(ServiceReason) %>%
  summarise(Revenue = mean(Amount)) %>%
  ggplot(aes(x=reorder(ServiceReason, -Revenue), y=Revenue, fill=ServiceReason)) + geom_col( show.legend = FALSE) +
  scale_fill_brewer(palette="Dark2") +
  scale_y_continuous(labels = scales::number_format(big.mark = ',', prefix = '$')) +
  labs(x="") + theme(axis.text.x = element_text(angle=45,hjust=1, vjust=.9)) +
  labs(title="Average Revenue By Service", caption = paste(min(invoice$Date), " to ", max(invoice$Date)), y="Avg Rev/Invoice") +
  geom_text(aes(x = ServiceReason, y=Revenue, label = paste0('$',round(Revenue,2))), nudge_y = 30) +
  scale_fill_brewer(palette="Dark2")
ggsave("plots/avgServiceAmount.jpg")

invoice %>% group_by(ServiceReason) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x=reorder(ServiceReason, -Count), y=Count, fill=ServiceReason)) + geom_col( show.legend = FALSE) +
  scale_fill_brewer(palette="Dark2") +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  labs(x="") + theme(axis.text.x = element_text(angle=45,hjust=1, vjust=.9)) +
  labs(title="Number of Service Occurances",caption = paste(min(invoice$Date), " to ", max(invoice$Date))) +
  scale_fill_brewer(palette="Dark2")
ggsave("plots/serviceOccurances.jpg")


visitsPerVin <- invoice %>% 
  group_by(Date, VIN) %>% summarise(Visits=n()) %>% 
  group_by(Visits) %>% summarise(NumCustomers=n())

# visitsPerVin$Visits <- as.factor(visitsPerVin$Visits)


visitsPerVin %>% 
ggplot(aes(x=Visits, y=NumCustomers)) + geom_area(fill="blue", alpha=.4) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  geom_text_repel(aes(x = Visits, y=NumCustomers, label = NumCustomers), nudge_y = 250, color='black') +
  labs(title="How Often are Customers Coming Back?", caption = paste(min(invoice$Date), " to ", max(invoice$Date)))
ggsave("plots/customerReturnByGroup.jpg")

  
invoice$discount <- invoice$Total.discount.Amount<0

visitsPerVin <- invoice %>% 
  group_by(Date, VIN) %>% mutate(Visits=n()) %>% ungroup() %>% group_by(discount) %>% mutate(numper=n()) %>% 
  group_by(discount,Visits,numper) %>% summarise(NumCustomers=n()) %>% ungroup() %>% 
  mutate(per=NumCustomers/numper)

visitsPerVin$discount <- as.factor(visitsPerVin$discount)
str(visitsPerVin)

pew <- c(
  "TRUE"="Discount Given",
  "FALSE"="No Discount"
)

ggplot(visitsPerVin,aes(x=Visits, y=NumCustomers)) + geom_area(fill="blue", alpha=.4) +
  facet_wrap(~discount, labeller = labeller(discount = pew)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  geom_text_repel(aes(x = Visits, y=NumCustomers, label = NumCustomers), nudge_y = 250, color='black') +
  labs(title="What Effect does Discount Have on Customer Return?", caption = paste(min(invoice$Date), " to ", max(invoice$Date)))
ggsave("plots/customerReturnByGroup_wDiscount.jpg")


ggplot(visitsPerVin,aes(x=Visits, y=NumCustomers)) + geom_area(fill="blue", alpha=.4) +
  facet_wrap(~discount, labeller = labeller(discount = pew)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  geom_text_repel(aes(x = Visits, y=NumCustomers, label = paste0(round(per*100,2),'%')), nudge_y = 250, color='black') +
  labs(title="What Effect does Discount Have on Customer Return?", caption = paste(min(invoice$Date), " to ", max(invoice$Date)))
ggsave("plots/customerReturnByGroup_wDiscountper.jpg")
