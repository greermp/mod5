library("tidyverse")
library("psych")
library("ggthemes")

theme_set(theme_tufte(base_size = 14))

setwd("~/MSBA/MOD5")


theme_tufte()

# cust = read.csv("data/VTA - Customer List.csv", stringsAsFactors = FALSE,
#                 na.strings=c(""," ","NA"))
cust = read.csv("data/custclean.csv", stringsAsFactors = FALSE,
                na.strings=c(""," ","NA"))
veh = read.csv("data/VTA - Vehicle Master List.csv")
# vata = read.csv("data/VTA - Customer List.csv")

# 
# desc(cust)
# 
# 
# drops <- c("Customer Type","Login Access", "Buying Time Frame / Estimated Ship Date",
#            "Make", "Model", "Lead Industry", "Scheduled Demo", "Trade In",
#            "Product Type", "BUYING REASON", "Percentage", "UID", "Date Of Birth",
#            "Category", "Primary Contact","Partner","Customer Type", "Customer.Refund.Entity.Bank.Format",
#            "Product.Type","Cust.Ref.File.Format","Customer.DD.Entity.Bank.Format","Direct.Debit.File.Format",
#            "Customer.DD.Entity.Bank.Subsidiary","Customer.DD.Entity.Bank.Subsidiary","Phone",
#            "Customer.Refund.Entity.Bank.Subsidiary","Date.Of.Birth",
#            "Buying.Time.Frame...Estimated.Ship.Date","BUYING.REASON","Lead.Industry",
#            "Login.Access", "Scheduled.Demo", "Trade.In", "Status"
#            )
# cust <- cust[ , !(names(cust) %in% drops)]
# 
# cust %>% count()
# 
# # Set numeric fields
# cust$ID <- as.numeric(as.character(cust$ID))
# # Remove repeat IDs
# cust <- cust[cust$ID != 703440000000, ]
# # Remove null customer IDs
# cust <- cust[! is.na(cust$ID), ]  


cust$Home.Store <- factor(cust$Home.Store)
cust$Primary.Subsidiary <- factor(cust$Primary.Subsidiary)
cust$Sales.Rep <- factor(cust$Sales.Rep)
cust$Primary.Subsidiary <- factor(cust$Primary.Subsidiary)



count(cust)
# write.csv(cust, "custclean.csv")
colSums(is.na(cust))

custnn <- cust %>% filter(!is.na(Home.Store) & Home.Store != "OSO") 
custnn <- custnn %>% group_by(Home.Store) %>% mutate(count = n())


p=ggplot(custnn, aes(x=reorder(Home.Store,-count))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle=45,hjust=1, vjust=.9)) +
  labs(x="Store", y="Registered Customers", title="Virginia Tire&Auto Customers/Store", 
       caption ="* Does not include OSO / Null values (18%)")
ggsave("cust/CustByStore.png")


custnn <- cust %>% filter(!is.na(Sales.Rep)& Home.Store != "OSO") 

x=custnn %>% group_by(Home.Store, Sales.Rep) %>% 
  summarise(count=n()) %>% arrange(desc(count)) 

x=x[!duplicated(x["Sales.Rep"]), ]

top10= x[1:10, ]

ggplot(top10, aes(x=reorder(Sales.Rep,-count), y=count, fill=Home.Store)) +
  geom_col() +
  theme(axis.text.x = element_text(angle=45,hjust=1, vjust=.9)) +
  labs(x="Sales Rep", y="# Customers", title="Top 10 Sales Reps/Customer Num")
ggsave("cust/SalesReps.png")





invoice$Brand <- factor(invoice$Brand)

# byBrand=invoice %>% group_by(Brand) %>% summarise(Invoices=n())


bb <-  byBrand %>% 
  mutate(BrandSummary = case_when(grepl("Chevrolet", byBrand$Brand) ~ "Chevrolet",
                                  grepl("BMW", byBrand$Brand) ~ "BMW",
                                  grepl("Buick", byBrand$Brand) ~ "Buick",
                                  grepl("Chrysler", byBrand$Brand) ~ "Chrysler",
                                  grepl("Cadillac", byBrand$Brand) ~ "Cadillac",
                                  grepl("Dodge", byBrand$Brand) ~ "Dodge",
                                  grepl("Ford", byBrand$Brand) ~ "Ford",
                                  grepl("GMC", byBrand$Brand) ~ "GMC",
                                  grepl("Hyundai ", byBrand$Brand) ~ "Hyundai ",
                                  grepl("Infiniti", byBrand$Brand) ~ "Infiniti",
                                  grepl("Jeep", byBrand$Brand) ~ "Jeep",
                                  grepl("Kia", byBrand$Brand) ~ "Kia",
                                  grepl("Lexus", byBrand$Brand) ~ "Lexus",
                                  grepl("Lincoln", byBrand$Brand) ~ "Lincoln",
                                  grepl("Mercedes", byBrand$Brand) ~ "Mercedes",
                                  grepl("Mazda", byBrand$Brand) ~ "Mazda",
                                  grepl("Cadillac", byBrand$Brand) ~ "Cadillac",
                                  grepl("Toyota", byBrand$Brand) ~ "Toyota",
                                  grepl("MERCEDES", byBrand$Brand) ~ "Mercedes",
                                  grepl("NISSAN", byBrand$Brand) ~ "Nissan",
                                  TRUE ~ as.character(byBrand$Brand)))

bb %>% group_by(BrandSummary) %>%summarise(invoices=sum(Invoices)) %>%  filter(invoices>=500) %>% 
  ggplot(aes(x=reorder(BrandSummary,-invoices), y=invoices, color=BrandSummary, fill=BrandSummary)) +geom_col()



# invoice$MODEL.YEAR <- factor(invoice$MODEL.YEAR)

invoice$MODEL.YEAR <- lubridate::ymd(invoice$Date)

invoice$MODEL.YEAR <- lubridate::dmy(invoice$Date)

invoice$MODEL.YEAR <- lubridate::guess_formats(invoice$Date, orders = "dmy", local = "English_United States.1252")


# invoice <-  invoice %>% ==
#   mutate(MODEL.YEAR = case_when(MODEL.YEAR=="" ~ "N/A",
#                                 TRUE ~ as.character(invoice$MODEL.YEAR)))

invoice %>% group_by(MODEL.YEAR)  %>%  summarise(Revenue=sum(Amount)) %>%  filter(Revenue>=500) %>% 
  ggplot(aes(x=reorder(MODEL.YEAR,-Revenue), y=Revenue, color=MODEL.YEAR, fill=MODEL.YEAR)) +geom_col()



invoice %>%  group_by(MODEL.YEAR)  %>%  summarise(Revenue=sum(Amount)) %>%
  ggplot(aes(x=MODEL.YEAR, y=Revenue)) + geom_line()


invoice$Datez <- lubridate::mdy(invoice$Date)
invoice$Datez
ggplot(invoice, aes(x=Datez, y=Amount)) + geom_line() + scale_x_date(date_breaks = "6 months", date_labels = "%m/%y")
