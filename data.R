rm(list = ls())
getwd()

company=read.csv("company.csv")
contact=read.csv("contact.csv")
device=read.csv("device.csv")
device_predicate=read.csv("device_predicate.csv")
device_product_code=read.csv("device_product_code.csv")
medical_specialty=read.csv("medical_specialty.csv")
premarket_preview=read.csv("premarket_preview.csv")
product_code=read.csv("product_code.csv")
product_code_premarket_preview=read.csv("product_code_premarket_preview.csv")



library(dplyr)

# construct device
a=device
b=device_predicate
b=b[,2:3]
colnames(b)=c("id","predicate_id")

# rawdata=left_join(a, b, by = "id")
# rawdata=right_join(a, b, by = "id")
rawdata=full_join(a, b, by = "id")
summary(rawdata$predicate_id)

rm(a)
rm(b)
rm(device_predicate)
device=rawdata
rm(rawdata)

summary(company)#id max 31972
summary(contact)#id max 39073
summary(device)

# merge company in company and two in device
# applicant_contact_id
# correspondent_contact_id

a=device
b=company
colnames(b)=c("applicant_id","company_name","company_is_manufacturer")
# rawdata=left_join(a, b, by = "applicant_id")
# rawdata=right_join(a, b, by = "applicant_id")
rawdata=full_join(a, b, by = "applicant_id")
device=rawdata
rm(rawdata)

a=device
b=company
colnames(b)=c("correspondent_id","correspondent_name","correspondent_is_manufacturer")

rawdata=full_join(a, b, by = "correspondent_id")
device=rawdata
rm(rawdata)
rm(a)
rm(b)
rm(company)

# merge contact from contact.csv and two in device:
# applicant_contact_id
# correspondent_contact_id
a=device
b=contact
colnames(b)=c("applicant_contact_id","applicant_contact_name","applicant_contact_is_verified")
rawdata=full_join(a, b, by = "applicant_contact_id")
device=rawdata
rm(rawdata)

a=device
b=contact
colnames(b)=c("correspondent_contact_id","correspondent_contact_name","correspondent_contact_is_verified")
b$correspondent_contact_id=as.factor(b$correspondent_contact_id)
rawdata=full_join(a, b, by = "correspondent_contact_id")
device=rawdata
rm(rawdata)
rm(a)
rm(b)
rm(contact)

a=device
b=medical_specialty
summary(b)
colnames(b)=c("advisory_committee","advisory_committee_name","regulation_number")
rawdata=full_join(a, b, by = "advisory_committee")
device=rawdata
rm(rawdata)
rm(a)
rm(b)

#construct product
a=product_code
b=product_code_premarket_preview
b=b[,2:3]
colnames(b)=c("id","premarket_preview_id")
rawdata=full_join(a, b, by = "id")
product=rawdata
rm(rawdata)
rm(a)
rm(b)
rm(product_code_premarket_preview)
rm(product_code)

a=product
b=premarket_preview
b=b[,2:3]
colnames(b)=c("premarket_preview_id","premarket_preview_name")
rawdata=full_join(a, b, by = "premarket_preview_id")
product=rawdata
rm(rawdata)
rm(a)
rm(b)
rm(premarket_preview)

a=product
b=medical_specialty
colnames(b)=c("review_panel","advisory_committee","regulation_number")
rawdata=full_join(a, b, by = "review_panel")
product=rawdata
rm(rawdata)
rm(a)
rm(b)
rm(medical_specialty)

primary=subset(device_product_code,device_product_code$is_primary==1)
primary=primary[,2:3]
rm(device_product_code)
write.csv(device,file = "[data]device_maerged.csv")
write.csv(product,file = "[data]product_maerged.csv")

device=data.frame(device$id,
             device$device_name,
             device$decision_date,
             device$date_receive,
             device$decision_code,
             device$clearance_type,
             device$third_party_flag,
             device$expedited_review_flag,
             device$show_indications,
             device$predicate_id,
             device$company_name,
             device$company_is_manufacturer,
             device$correspondent_name,
             device$correspondent_is_manufacturer,
             device$applicant_contact_name,
             device$applicant_contact_is_verified,
             device$correspondent_contact_name,
             device$correspondent_contact_is_verified,
             device$advisory_committee_name,
             device$regulation_number)

product=data.frame(product$id,
             product$device_name,
             product$device_class,
             product$gmp_exempt_flag,
             product$third_party_review_eligible,
             product$third_party_review_code,
             product$regulation_number.x,
             product$submission_type,
             product$implant_flag,
             product$life_sustain_support_flag,
             product$summary_malfunction_reporting,
             product$premarket_preview_name,
             product$advisory_committee,
             product$regulation_number.y)

write.csv(device,file = "[subset_data]device_maerged.csv")
write.csv(product,file = "[subset_data]product_maerged.csv")


a=device
b=primary
colnames(b)=c("device.id","product.id")
rawdata=full_join(a, b, by = "device.id")
device=rawdata
rm(rawdata)
rm(a)
rm(b)
rm(primary)

a=device
b=product
rawdata=full_join(a, b, by = "product.id")
alldata=rawdata
rm(rawdata)
rm(a)
rm(b)
rm(device)
rm(product)

write.csv(alldata,file = "[all_data]product&device.csv")
rm(alldata)
