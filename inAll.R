setwd("~/MSBA/MOD5")


# read.csv()
# source("VATA_Invoice.R")
# invoice=read.csv("newdata/Vast_Invoices_010118_123120.csv")
invoice=read.csv("newdata/Vast_Invoices_010118_123120_WithVisit.csv")
# i1801=readxl::read_excel("newdata/VastInvoiceDetail_2018/VastSale_1801.xlsx")
# i1802=readxl::read_excel("newdata/VastInvoiceDetail_2018/VastSale_1802.xlsx")
# i1803=readxl::read_excel("newdata/VastInvoiceDetail_2018/VastSale_1803.xlsx")
# i1804=readxl::read_excel("newdata/VastInvoiceDetail_2018/VastSale_1804.xlsx")
# i1805=readxl::read_excel("newdata/VastInvoiceDetail_2018/VastSale_1805.xlsx")
# i1806=readxl::read_excel("newdata/VastInvoiceDetail_2018/VastSale_1806.xlsx")
# i1807=readxl::read_excel("newdata/VastInvoiceDetail_2018/VastSale_1807.xlsx")
# i1808=readxl::read_excel("newdata/VastInvoiceDetail_2018/VastSale_1808.xlsx")
# i1809=readxl::read_excel("newdata/VastInvoiceDetail_2018/VastSale_1809.xlsx")
# i1810=readxl::read_excel("newdata/VastInvoiceDetail_2018/VastSale_1810.xlsx")
# i1811=readxl::read_excel("newdata/VastInvoiceDetail_2018/VastSale_1811.xlsx")
# i1812=readxl::read_excel("newdata/VastInvoiceDetail_2018/VastSale_1812.xlsx")
# 
# i1901=readxl::read_excel("newdata/VastInvoiceDetail_2019/VastSale_1901.xlsx")
# i1902=readxl::read_excel("newdata/VastInvoiceDetail_2019/VastSale_1902.xlsx")
# i1903=readxl::read_excel("newdata/VastInvoiceDetail_2019/VastSale_1903.xlsx")
# i1904=readxl::read_excel("newdata/VastInvoiceDetail_2019/VastSale_1904.xlsx")
# i1905=readxl::read_excel("newdata/VastInvoiceDetail_2019/VastSale_1905.xlsx")
# i1906=readxl::read_excel("newdata/VastInvoiceDetail_2019/VastSale_1906.xlsx")
# i1907=readxl::read_excel("newdata/VastInvoiceDetail_2019/VastSale_1907.xlsx")
# i1908=readxl::read_excel("newdata/VastInvoiceDetail_2019/VastSale_1908.xlsx")
# i1909=readxl::read_excel("newdata/VastInvoiceDetail_2019/VastSale_1909.xlsx")
# i1910=readxl::read_excel("newdata/VastInvoiceDetail_2019/VastSale_1910.xlsx")
# i1911=readxl::read_excel("newdata/VastInvoiceDetail_2019/VastSale_1911.xlsx")
# i1912=readxl::read_excel("newdata/VastInvoiceDetail_2019/VastSale_1912.xlsx")
# 
# i2001=readxl::read_excel("newdata/VastInvoiceDetail_2020/VastSale_2001.xlsx")
# i2002=readxl::read_excel("newdata/VastInvoiceDetail_2020/VastSale_2002.xlsx")
# i2003=readxl::read_excel("newdata/VastInvoiceDetail_2020/VastSale_2003.xlsx")
# i2004=readxl::read_excel("newdata/VastInvoiceDetail_2020/VastSale_2004.xlsx")
# i2005=readxl::read_excel("newdata/VastInvoiceDetail_2020/VastSale_2005.xlsx")
# i2006=readxl::read_excel("newdata/VastInvoiceDetail_2020/VastSale_2006.xlsx")
# i2007=readxl::read_excel("newdata/VastInvoiceDetail_2020/VastSale_2007.xlsx")
# i2008=readxl::read_excel("newdata/VastInvoiceDetail_2020/VastSale_2008.xlsx")
# i2009=readxl::read_excel("newdata/VastInvoiceDetail_2020/VastSale_2009.xlsx")
# i2010=readxl::read_excel("newdata/VastInvoiceDetail_2020/VastSale_2010.xlsx")
# i2011=readxl::read_excel("newdata/VastInvoiceDetail_2020/VastSale_2011.xlsx")
# i2012=readxl::read_excel("newdata/VastInvoiceDetail_2020/VastSale_2012.xlsx")
# all <- rbind(i1801,i1802,i1803,i1804,i1805,i1806,i1807,i1808,i1809,i1810,i1811,i1812,
#              i1901,i1902,i1903,i1904,i1905,i1906,i1907,i1908,i1909,i1910,i1911,i1912,
#              i2001,i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012)
# 
# all <- rename(all, INVOICE_DATE = INV_DATE)
# write.csv(all,"allDetail.csv")

all <- read.csv("allDetail.csv")

# testall=all %>% slice_sample(prop=.01)
# testinv=invoice %>% slice_sample(prop=.01)
# 
# testall$INVOICE_DATE <- lubridate::ymd(testall$INVOICE_DATE)
testinv$INVOICE_DATE <- lubridate::ymd(testinv$INVOICE_DATE)


all$INVOICE_DATE <- lubridate::ymd(all$INVOICE_DATE)
invoice$INVOICE_DATE <- lubridate::ymd(invoice$INVOICE_DATE)


invoice_181920 <- left_join(invoice, all, by= c("INVOICE_NUMBER", "INVOICE_DATE"))



# write.csv(invoice_181920, "newdata/181920invoice.csv")
write.csv(invoice_181920, "newdata/181920invoiceWithVisitNum.csv")

