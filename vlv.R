setwd("~/MSBA/MOD5")
# source("preprocess.R")
invoice <- read.csv("newdata/InvoiceSummary.csv")
invoice$CUSTOMER_NUMBER <- as.factor(invoice$CUSTOMER_NUMBER)
# Drop NA VINs

invoice$InvoiceYear <- as.factor(invoice$InvoiceYear)
# invoice$visit <- as.factor(invoice$visit)
Revenue2018=invoice %>% filter(InvoiceYear ==2018) %>% summarise(sum(TOTAL_SALE_AMOUNT))

# Drop null VINS, create date, grab 2018 only
invoice$VIN_NUMBER[invoice$VIN_NUMBER=='NULL'] = NA
invoice <- invoice %>% drop_na(CUST_CREATE_DATE, VIN_NUMBER)
invoice=invoice %>% filter(CUST_CREATE_DATE >= '2018-01-01' & CUST_CREATE_DATE <= '2018-12-31')
#Set create month
invoice$createMonth <- month(invoice$CUST_CREATE_DATE)
invoice$createMonth= month(invoice$createMonth)

# Plot new users
# nVins <- invoice %>% group_by(createMonth, VIN_NUMBER) %>% unique() %>% group_by(createMonth) %>% summarise(VINS=n()) #%>% mutate(vinx=createYear-.25)
# nCust <- invoice %>% group_by(createMonth,CUSTOMER_NUMBER) %>% unique() %>%  group_by(createMonth) %>%  summarise(Customer=n())#%>% mutate(custx=createYear+.25)
nVins <- invoice %>% group_by(createMonth) %>% select(createMonth,VIN_NUMBER) %>% unique() %>% summarise(VINS=n())
nCust <- invoice %>% group_by(createMonth) %>% select(createMonth,CUSTOMER_NUMBER) %>% unique() %>% summarise(Customer=n())

newCustomers = nCust %>% summarise(sum(Customer))
newVins = nVins %>% summarise(sum(VINS))

VLV = cbind(nVins, nCust )
VLV=VLV %>% pivot_longer(cols=c("VINS", "Customer"), names_to="Type")

VLV$createMonth= lubridate::month(VLV$createMonth, label=TRUE)

VLV %>% ggplot(aes(x=createMonth, y=value, fill=Type)) + geom_col(position="dodge")+
  labs(title="New Customers / VINs in 2018", x="", y="New Customers") 

custVisits <- invoice %>% group_by(CUSTOMER_NUMBER) %>% summarise(visits=n(),totalRev=sum(TOTAL_SALE_AMOUNT)) 
vinVists <- invoice %>% group_by(VIN_NUMBER) %>% summarise(VINVists=n(), totalRev=sum(TOTAL_SALE_AMOUNT))
#Drop fake VINS
vinVists <- vinVists %>% filter(nchar(VIN_NUMBER) >= 11)

#---------------------------
# Analyze customer data
custVisits$ARPU_6month <- custVisits$totalRev/6

# gET RID OF 1 OUTLIER
custVisits %>% filter(visits < 400) %>% 
  ggplot(aes(x=visits)) + geom_histogram(binwidth=1,fill='darkblue', alpha=.5) +
  labs(title="2018 Customers # Visits from Jan 2018 to Dec 2020")

cvisits <- invoice %>% group_by(CUSTOMER_NUMBER) %>% mutate(maxVisits=max(as.numeric(visit), na.rm=T))

custVisits

summary(custVisits)

CLVSum <- custVisits %>% group_by(visits) %>% 
  summarise(numberCustomers=n(), combineRev=sum(totalRev), meanRev=mean(totalRev), 
            medianRev=median(totalRev), mean6MoARPU=mean(ARPU_6month), totalCustomers=nrow(custVisits), 
            perCust=numberCustomers/totalCustomers, perLabel=paste0(round( perCust*100 ,1),'%'))
newRev <- CLVSum %>% summarise(newCustomerRev=sum(combineRev ))

marketing=Revenue2018*.025

marketingPerCust=marketing/newCustomers

CLVSum$visits=as.factor(CLVSum$visits)


CLVSum %>% ggplot(aes(x=visits, y= numberCustomers, fill=perCust*100)) + geom_col(show.legend = FALSE) +
  geom_text(aes(x=visits, y= numberCustomers, label=perLabel), nudge_y = 400, size=4) +
  labs(y="Number of Customers") + scale_y_continuous(breaks=seq(0,15000,1000), labels=label_comma(big.mark = ","))









library(networkD3)
# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
  target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
  value=c(2,3, 2, 3, 1, 3)
)

links
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)
nodes
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p




