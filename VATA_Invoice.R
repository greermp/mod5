library("tidyverse")
library("psych")
library("ggthemes")
library("lubridate")

theme_set(theme_tufte(base_size = 14))

setwd("~/MSBA/MOD5")

invoice = read.csv("data/VTA - Invoice List.csv", stringsAsFactors = FALSE,
                   na.strings=c(""," ","NA"))

drops <- c("Created.From","AvidPay.Cleared.Date", "Avid.Approval.Status",
           "Recommended.Sales.Order", "Inventory.Type", "Engine.Name", "Avid.APN.Has.Cleared.Check.Images", "Avid.Is.Printed",
           "Service.Types.for.Repair.Jobs", "Print.PDF", "ork.Order..", "Parts.Requesition.SO", "Epicore.Track.Number",
           "Epicore.Track.Number", "Left.Rear.Tire.Pressure","Right.Front.Tire.pressure","Right.Rear.Tire.Pressure", "Account.Print.Name",
           "X03.RECORD...invoiceOMER.ACCOUNT.NUMBER","Bill.Date","Intercompany.Transaction","Merge.To",
           " Discount.Approval.Email","UCHP.Registered.Date","Drop.Ship.Sales.Order",
           "Preventive.Maintenance","Date.Of.Birth",
           "Buying.Time.Frame...Estimated.Ship.Date","Cannibalization.Link","Drop.Ship.invoiceomer",
           "invoiceomer", "Enquiry.Id", "Preventive.Maintenance", "Status", "Contact Email",
           "Remarks.For.Articles", "Fuel", "PO.Check.Number", "VEHICLE.CATEGORY", "PO.Check.Number",
           "REPAIR.TYPE", "MODEL", "VEHICLE.CATEGORY", "Vehicle.Repair.Order.Refrence",
           "Mechanic", "Model.Group*", "Unit.", "Stock.*", "Transmission", "Fuel.Type",
           "Stock..", "Component", "Preferred.Entity.Bank", "Vendor.Bank.Fees", "Bank.Fee",
           "PDI.Start.Date", "PDI.End.Date", "PDI.Start.Time", "PDI.End.Time", "Revised.Quote*",
           "Original.Quote.Link*", "Reason.Code", "Capitalization.Work.Order", "Capitalization.Vehicle.Link",
           "Fuel.Level", "RO.invoiceomer.Signature", "Ro.Image.1","Ro.Image.2","Ro.Image.3","Ro.Remarks",
           "Receipt.Sales.Rep", "Scratch.Vehicle.Image", "Additional.Information..Remarks.", "Contact.Name",
           "Contact.Email", "Appointment.Date", "Service.Type", "Discount.Approval.Email","Model.Group", "Revised.Quote", "Stock.", "Total.Sliding.Sale.Amount", "Work.Order..", "A.P.Account.Print.Name", "Left.Front.Tire.Pressure", "Come.Back.Sales.Order", "Original Quote Link", "Print", "Account",
           "Is.Closed",	"PDI",	"Original.Quote.Link",	"Capitalization",	"Capitalization.Posted",	"RO.Customer.Signature",
           "Replacement.Parts",	"Articles.Of.Value", "Plate..",	"Customer",	"Drop.Ship.Customer", "Clock.In.Started",	"Parking.Lot.No",
           "X03.RECORD...CUSTOMER.ACCOUNT.NUMBER", "L.F.TREAD.DEPTH",	"R.F.TREAD.DEPTH",	"LH.Rear.Tire.Depth",	"R.R.TREAD.DEPTH",
           "Repair.Type...Brake",	"Repair.Type...Fluid.Change",	"Repair.Type...Sublet",
           "First.Mile.AccountNumber", "First.Mile.EntryMethod", "First.Mile.TransactionID", "First.Mile.Auth.Code", "First.Mile.Amount", "IN.MILEAGE",	"OUT.MILEAGE", "Repair.Order.Status", "Promise.Time"
           )

invoice <- invoice[ , !(names(invoice) %in% drops)]

invoice <-  invoice %>%
  mutate(BrandSum = case_when(grepl("Chevrolet", invoice$Brand) ~ "Chevrolet",
                                  grepl("BMW", invoice$Brand) ~ "BMW",
                                  grepl("Buick", invoice$Brand) ~ "Buick",
                                  grepl("Chrysler", invoice$Brand) ~ "Chrysler",
                                  grepl("Cadillac", invoice$Brand) ~ "Cadillac",
                                  grepl("Dodge", invoice$Brand) ~ "Dodge",
                                  grepl("Ford", invoice$Brand) ~ "Ford",
                                  grepl("GMC", invoice$Brand) ~ "GMC",
                                  grepl("Hyundai ", invoice$Brand) ~ "Hyundai ",
                                  grepl("Infiniti", invoice$Brand) ~ "Infiniti",
                                  grepl("Jeep", invoice$Brand) ~ "Jeep",
                                  grepl("Kia", invoice$Brand) ~ "Kia",
                                  grepl("Lexus", invoice$Brand) ~ "Lexus",
                                  grepl("Lincoln", invoice$Brand) ~ "Lincoln",
                                  grepl("Mercedes", invoice$Brand) ~ "Mercedes",
                                  grepl("Mazda", invoice$Brand) ~ "Mazda",
                                  grepl("Cadillac", invoice$Brand) ~ "Cadillac",
                                  grepl("Toyota", invoice$Brand) ~ "Toyota",
                                  grepl("MERCEDES", invoice$Brand) ~ "Mercedes",
                                  grepl("NISSAN", invoice$Brand) ~ "Nissan",
                                  grepl("Mitsubishi", invoice$Brand) ~ "Mitsubishi",
                                  grepl("Honda", invoice$Brand) ~ "Honda",
                                  grepl("Acura", invoice$Brand) ~ "Acura",
                                  grepl("Suburu", invoice$Brand) ~ "Suburu",
                                  grepl("Isuzu", invoice$Brand) ~ "Isuzu",
                                  grepl("Suzuki", invoice$Brand) ~ "Suzuki",
                                  grepl("Gmc", invoice$Brand) ~ "Gmc",
                                  TRUE ~ as.character(invoice$Brand)))



invoice$Repair.Type.Inspection[invoice$Repair.Type.Inspection=="Yes"] =1
invoice$Repair.Type.Inspection[invoice$Repair.Type.Inspection=="No"] =0
invoice$Repair.Type.Inspection <- as.logical(as.numeric(invoice$Repair.Type.Inspection))
invoice$Repair.Type...General.Services[invoice$Repair.Type...General.Services=="Yes"] =1
invoice$Repair.Type...General.Services[invoice$Repair.Type...General.Services=="No"] =0
invoice$Repair.Type...General.Services <- as.logical(as.numeric(invoice$Repair.Type...General.Services))
invoice$Repair.Type...Tire[invoice$Repair.Type...Tire=="Yes"] =1
invoice$Repair.Type...Tire[invoice$Repair.Type...Tire=="No"] =0
invoice$Repair.Type...Tire <- as.logical(as.numeric(invoice$Repair.Type...Tire))
invoice$Repair.Type...Declined.Lines[invoice$Repair.Type...Declined.Lines=="Yes"] =1
invoice$Repair.Type...Declined.Lines[invoice$Repair.Type...Declined.Lines=="No"] =0
invoice$Repair.Type...Declined.Lines <- as.logical(as.numeric(invoice$Repair.Type...Declined.Lines))
invoice$Repair.Type...Repair[invoice$Repair.Type...Repair=="Yes"] =1
invoice$Repair.Type...Repair[invoice$Repair.Type...Repair=="No"] =0
invoice$Repair.Type...Repair <- as.logical(as.numeric(invoice$Repair.Type...Repair))

invoice$Repair.Type.Inspection <- as.logical(invoice$Repair.Type.Inspection)
invoice$Repair.Type...General.Services <- as.logical(invoice$Repair.Type...General.Services)
invoice$Repair.Type...Tire <- as.logical(invoice$Repair.Type...Tire)
invoice$Repair.Type...Declined.Lines <- as.logical(invoice$Repair.Type...Declined.Lines)
invoice$Repair.Type...Repair <- as.logical(invoice$Repair.Type...Repair)

invoice$ServiceReason = ""
invoice <-  invoice %>%
  mutate(ServiceReason = case_when(invoice$Repair.Type.Inspection == TRUE ~ "Inspection",
                          invoice$Repair.Type...General.Services == TRUE ~ "General Services",
                          invoice$Repair.Type...Tire == TRUE ~ "Tire",
                          invoice$Repair.Type...Declined.Lines == TRUE ~ "Declined Lines",
                          invoice$Repair.Type...Repair == TRUE ~ "Repair",
                          TRUE ~ "NA"))
# invoice$Repair.Type...Brake[invoice$Repair.Type...Brake=="Yes"] =1
# invoice$Repair.Type...Brake[invoice$Repair.Type...Brake=="No"] =0
# invoice$Repair.Type...Brake[is.na(invoice$Repair.Type...Brake)] =0
# invoice$Repair.Type...Brake <- as.logical(as.numeric(invoice$Repair.Type...Brake))
# invoice$Repair.Type...Fluid.Change[invoice$Repair.Type...Fluid.Change=="Yes"] =1
# invoice$Repair.Type...Fluid.Change[invoice$Repair.Type...Fluid.Change=="No"] =0
# invoice$Repair.Type...Fluid.Change <- as.logical(as.numeric(invoice$Repair.Type...Fluid.Change))
# invoice$Repair.Type...Sublet[invoice$Repair.Type...Sublet=="Yes"] =1
# invoice$Repair.Type...Sublet[invoice$Repair.Type...Sublet=="No"] =0
# invoice$Repair.Type...Sublet[is.na(invoice$Repair.Type...Sublet)] =0
# invoice$Repair.Type...Sublet <- as.logical(as.numeric(invoice$Repair.Type...Sublet))


invoice %>% count()
invoice <- invoice %>% separate(Name, into=c("ID", "Name"), sep = "\\s", extra = "merge")
invoice <- invoice %>% separate(Billing.Customer, into=c("Billing ID", "Billing Name"), sep = "\\s", extra = "merge")
invoice$Date<- lubridate::mdy(invoice$Date)
invoice <- invoice %>% mutate(month= month(Date), year=year(Date))
invoice$ID <- as.numeric(as.character(invoice$ID))
invoice$Brand <- factor(invoice$Brand)
invoice$BrandSum <- factor(invoice$BrandSum)
invoice$Model <- factor(invoice$Model)
invoice$Module.Name <- factor(invoice$Module.Name)
invoice$Web.Appointment <- factor(invoice$Web.Appointment)


test="Neal Hallett 103 west brill drive northeast  vienna VA 22180 United States" 
test="-1 -1 -1"
word(test,-3)
invoice$Pickup.Drop.Address[is.na(invoice$Pickup.Drop.Address)] <- "-1 -1 -1"
invoice$CustomerZip=""
invoice$CustomerZip= word(invoice$Pickup.Drop.Address, -2)

invoice$Pickup.Drop.Address

invoice$count=str_count(invoice$Pickup.Drop.Address, "\\S+")>3


invoice <-  invoice %>%
  mutate(CustomerZip = case_when(invoice$count==TRUE ~ word(invoice$Pickup.Drop.Address, -3),
                                 TRUE ~ "NA"))


# drops <- c()
# invoice <- invoice[ , !(names(invoice) %in% drops)]


invoice <-  invoice %>%
  mutate(CustomerZip = case_when(grepl("^[0-9]{5}(?:-[0-9]{4})?$", invoice$CustomerZip) ~ invoice$CustomerZip,
                                   TRUE ~ "NA"))






colSums(is.na(invoice))
write.csv(invoice,file="data/invoice_clean.csv")
#################################################################

write.csv(smInv, file="data/invoice_sm.csv")
