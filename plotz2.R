setwd("~/MSBA/MOD5")
source("preprocess.R")

# BY locations
invoice %>% group_by(Store.ID) %>% summarise(Revenue = sum(TOTAL_SALE_AMOUNT)) %>% 
  ggplot(aes(x=reorder(Store.ID, -Revenue), y=Revenue)) + geom_col() +
  scale_y_continuous(labels = scales::unit_format(scale=.000001,prefix = '$ ', unit = 'Million',accuracy = 1, breaks=1)) + labs(x="") +
  theme(axis.text.x = element_text(angle=45,hjust=1, vjust=.9, size = 16)) 
  

# GET Return visits

inv=invoice
inv$VIN_NUMBER[inv$VIN_NUMBER=='NULL'] = NA
inv <- inv %>% drop_na(CUST_CREATE_DATE, VIN_NUMBER)
inv %>% group_by(createYear) %>% summarise(Reveneue=sum(TOTAL_SALE_AMOUNT)) %>% 
  ggplot(aes(x=createYear, y=Reveneue)) + geom_col()



inv$createYear = as.factor(inv$createYear)

cust_vin_visits <- inv %>% group_by(createYear,CUSTOMER_NUMBER, VIN_NUMBER) %>% summarise(visits=n())
ggplot(cust_vin_visits, aes(x=visits,fill=createYear)) + geom_histogram(bins=1,stat="count", show.legend = FALSE) +
  facet_wrap(~createYear) +
  labs(title="Number of Visits per Cust/VIN by Customer Creation Year", x="Visits per Customer/VIN") +
  scale_y_continuous(labels = label_comma(accuracy = NULL, scale = 1, 
                                          suffix = "",
                                          big.mark = ",", decimal.mark = ".")) +
  scale_x_continuous(breaks=seq(0,20,2), limits = c(0,20))
ggsave("plots/VisitsByCreationYr.jpg", width = 4, height = 4)

cust_visits <- inv %>% group_by(createYear,CUSTOMER_NUMBER) %>% summarise(visits=n())
ggplot(cust_visits, aes(x=visits,fill=createYear)) + geom_histogram(bins=1,stat="count", show.legend = FALSE) +
  facet_wrap(~createYear) +
  labs(title="Number of Visits per Customer by Customer Creation Year", x="Visits per Customer/VIN") +
  scale_y_continuous(breaks=seq(0,20000,5000), limits=c(0,20000),
    labels = label_comma(accuracy = NULL, scale = 1, 
                                          suffix = "",
                                          big.mark = ",", decimal.mark = ".")) +
  scale_x_continuous(breaks=seq(0,20,2), limits = c(0,20))
ggsave("plots/VisitsByCreationYr.jpg", width = 4, height = 4)

vin_visits <- inv %>% group_by(createYear,VIN_NUMBER) %>% summarise(visits=n())
ggplot(vin_visits, aes(x=visits,fill=createYear)) + geom_histogram(bins=1,stat="count", show.legend = FALSE) +
  facet_wrap(~createYear) +
  labs(title="Number of Visits per VIN by Creation Year", x="Visits per Customer/VIN") +
  scale_y_continuous(breaks=seq(0,20000,5000),
                     labels = label_comma(accuracy = NULL, scale = 1, 
                                          suffix = "",
                                          big.mark = ",", decimal.mark = ".")) +
  scale_x_continuous(breaks=seq(0,20,2), limits = c(0,20))


# VINS and People new per year
nVins <- inv %>% group_by(createYear) %>% select(VIN_NUMBER) %>% unique() %>% summarise(VINS=n()) #%>% mutate(vinx=createYear-.25)
nCust <- inv %>% group_by(createYear) %>% select(CUSTOMER_NUMBER) %>% unique() %>% summarise(Customer=n())#%>% mutate(custx=createYear+.25)

VLV = cbind(nVins, nCust )
VLV=VLV %>% pivot_longer(cols=c("VINS", "Customer"), names_to="Type")


VLV %>% ggplot(aes(x=createYear, y=value, fill=Type)) + geom_col(position="dodge")+
  # geom_text(aes(x=createYear, y=value,label=value))+
  labs(title="New Customers / VINs per Year", x="Year", y="New Customers") +
  scale_x_continuous(breaks = seq(2009, 2020, 1)) +
  scale_y_continuous(breaks = seq(0, 40000, 5000))

# 
# VLV$yPos=0
# VLV$yPos[VLV$Type=="VINS"] = (VLV$createYear+.25)
# VLV$yPos[VLV$Type=="Customer"] = (VLV$createYear-.25)
# 
# VLV1 <- VLV %>% filter(Type=="VINS") %>%  rowwise() %>% 
#   mutate(yPos = sum( c_across(createYear))-.25)
# VLV2 <- VLV %>% filter(Type=="Customer") %>%  rowwise() %>% 
#   mutate(yPos = sum( c_across(createYear))+.25)
# VLV <- cbind(VLV1, VLV2)

  summarise(newCustomers=n(), Revenue = sum(TOTAL_SALE_AMOUNT)) %>% 
  ggplot(aes(x=createYear, y=Revenue, fill=newCustomers)) + geom_col() +
  scale_fill_viridis_c() +
  labs(title="Revenue by Customer Creation Date", x="Year of Customer Creation") +
  scale_y_continuous(labels = label_comma(accuracy = NULL, scale = .000001, 
                                          prefix = "$", suffix = "mm",
                                          big.mark = ",", decimal.mark = "."))+
  theme(axis.text.x = element_text(angle=45, hjust=1))



custVin <- invoice %>% 
  group_by(CUSTOMER_NUMBER, VIN_NUMBER) %>% 
  summarise(visits=n())

CustomerByVin <- invoice %>% 
  select(CUSTOMER_NUMBER, VIN_NUMBER) %>% 
  group_by(CUSTOMER_NUMBER) %>% summarise(numVins=n()) %>% 
  arrange(desc(numVins))

CustomerByVin$gt5 = "no"

CustomerByVin$gt5[CustomerByVin$numVins > 5] <- "yes"

ggplot(CustomerByVin, aes(x=gt5))+ geom_bar() +
  labs(x="Greater than 5 VINs", y="Number of Customers", title="Corporate Accounts?")
ggsave("plots/CorporateAcc.jpg")

CustomerByVin %>% group_by(numVins) %>% summarise(num=n()) %>% filter(num>5) %>% 
  ggplot(aes(x=numVins, y=num)) + geom_area()

CustomerByVin %>% group_by(numVins) %>% 
  ggplot(mapping=aes(x=numVins,label=..count..)) + geom_histogram(breaks = seq(0, 100, 5)) +
  geom_text(stat="bin", size=4,nudge_y=5000,breaks = seq(0, 100, 5)) +
  labs(title="Customers by # of Serviced VINs") +
  scale_x_continuous(breaks = seq(0, 100, 5))

invoice %>% group_by(CUSTOMER_NUMBER) %>% summarise(liftimeSales=sum(TOTAL_SALE_AMOUNT)) %>% 
  summarise(mean(liftimeSales))




custValue=invoice %>% group_by(CUSTOMER_NUMBER) %>% summarise(RevPerCustomer=sum(TOTAL_SALE_AMOUNT)) 

describe(custValue)
custValue %>% filter(RevPerCustomer<3000 & RevPerCustomer>0) %>% 
ggplot(aes(x="VTA Customers",y=RevPerCustomer)) + geom_violin()

custValue %>%
ggplot(aes(x=RevPerCustomer)) + geom_density(adjust=3) +
  scale_x_continuous( expand=c(0,0), limits=c(0,5000),breaks = seq(0, 5000, 500))

invoice <- invoice %>% group_by(CUSTOMER_NUMBER, INVOICE_DATE) %>% 
  mutate(distinctVisits=n())


avgByBrand=invoice %>% group_by(BrandSum) %>% summarise(count=n(), AvgInvoice=mean(TOTAL_SALE_AMOUNT)) %>% 
  arrange(desc(AvgInvoice))

summary(avgByBrand)

avgByBrand  %>% slice_max(AvgInvoice,n=10) %>% 
  ggplot(aes(x=reorder(BrandSum, -AvgInvoice), y=AvgInvoice, fill=count)) + geom_col() +
  theme(axis.text.x = element_text(size=12,angle=45,hjust=1, vjust=.9)) +
  labs(x="", y="Avg Invoice", title="Highest Average Invoice by Make",
       caption = "Only showing makes with > 500 invoices") + #scale_fill_viridis_c()
  scale_y_continuous(labels = label_comma(accuracy = NULL, scale = 1, 
                                          suffix = "", prefix = "$",
                                          big.mark = ",", decimal.mark = "."))



avgByBrand  %>% slice_max(count,n=10) %>% 
  ggplot(aes(x=reorder(BrandSum, -count), y=count, fill=AvgInvoice)) + geom_col() +
  theme(axis.text.x = element_text(size=12,angle=45,hjust=1, vjust=.9)) +
  labs(x="", y="Number of Invoices", title="Number of Invoices by Make") + #scale_fill_viridis_c()
  scale_y_continuous(labels = label_comma(accuracy = NULL, scale = 1, 
                                          suffix = "",
                                          big.mark = ",", decimal.mark = "."))
ggsave("plots/avgInvoice.png")

invoice %>% select(BrandSum,TOTAL_SALE_AMOUNT) %>% 
  group_by(BrandSum) %>% mutate(avg=mean(TOTAL_SALE_AMOUNT), num=n()) %>% 
  arrange(desc(avg)) %>% 
  filter(TOTAL_SALE_AMOUNT>0 & TOTAL_SALE_AMOUNT <1000 & ! is.na(BrandSum) & num>3000) %>% 
  ggplot(aes(x=BrandSum, y=TOTAL_SALE_AMOUNT,fill=BrandSum)) +
  geom_violin(show.legend = FALSE) +
  labs(x="", y="Invoice TOTAL_SALE_AMOUNT", title="There are lots of outliers!", caption = "Only Brands w/ > 3000 invoices, and invoices < 1000$ pictured") +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("plots/boxplotCostByMake.png")


## Rev over time
invoice$year = as.factor(year(invoice$INVOICE_DATE))

datePlot <- invoice %>%
  mutate(YearMonth = format(INVOICE_DATE, "%Y-%m"))
datePlot$YearMonth <- ym(datePlot$YearMonth)
datePlot <- datePlot %>%  group_by(YearMonth) %>% 
  mutate(Rev = sum(TOTAL_SALE_AMOUNT)) %>% ungroup()


ggplot(datePlot,aes(x=YearMonth, y=Rev, shape=year, color=year)) + geom_point(show.legend = FALSE) + geom_line(group=1, show.legend = FALSE) +
  facet_wrap(~year, ncol=1,scales = "free_x") +
  scale_x_date(labels = date_format("%b"), breaks = date_breaks("1 month")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)) + 
  scale_y_continuous(labels = label_comma(accuracy = NULL, scale = 1, 
                                          suffix = "", prefix = "$",
                                          big.mark = ",", decimal.mark = ".")) +
  labs(title="Revenue Over Time", y="Revenue", x="")

#VISITS

visits=invoice 

visits$VIN_NUMBER[visits$VIN_NUMBER=="NULL"] = NA

visitsPerVin <- visits %>% filter(! is.na(VIN_NUMBER)) %>% 
  group_by(INVOICE_DATE, VIN_NUMBER) %>% summarise(Visits=n()) %>% 
  group_by(Visits) %>% summarise(NumCustomers=n())

visitsPerCust <- visits %>% 
  group_by(INVOICE_DATE, CUSTOMER_NUMBER) %>% summarise(Visits=n()) %>% 
  group_by(Visits) %>% summarise(NumCustomers=n())

# visitsPerVin$Visits <- as.factor(visitsPerVin$Visits)


visitsPerVin %>% 
ggplot(aes(x=Visits, y=NumCustomers)) + geom_col(fill="blue", alpha=.4) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  geom_text(aes(x = Visits, y=NumCustomers, label = NumCustomers), nudge_y = 250, color='black') +
  labs(title="How Often are Customers Coming Back?", caption = paste(min(invoice$Date), " to ", max(invoice$Date)))


visitsPerCust %>% 
  ggplot(aes(x=Visits, y=NumCustomers)) + geom_col(fill="blue", alpha=.4) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::number_format(big.mark = ',')) +
  geom_text(aes(x = Visits, y=NumCustomers, label = NumCustomers), nudge_y = 250, color='black') +
  labs(title="How Often are Customers Coming Back?", caption = paste(min(invoice$Date), " to ", max(invoice$Date)))

ggsave("plots/customerReturnByGroup.jpg")

  
invoice$discount <- invoice$Total.discount.TOTAL_SALE_AMOUNT<0

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
