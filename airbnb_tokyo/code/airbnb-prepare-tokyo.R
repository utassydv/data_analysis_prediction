#########################################################################################
# Prepared for David Utassy's DA3 assignment 1.
#
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
library(modelsummary)



#-------------------------------------------------------
# Import data
data_in <- "/Users/utassydv/Documents/workspaces/CEU/my_repos/data_analysis_prediction/airbnb_tokyo/data/raw/listings.csv"
data_out <- "/Users/utassydv/Documents/workspaces/CEU/my_repos/data_analysis_prediction/airbnb_tokyo/data/clean/airbnb_tokio_cleaned.csv"
data <- read_csv(data_in)


# keep if property type is Apartment, House or Townhouse
table(data$property_type)

# Collect houses, apartments, hotels
hotels <- c("Entire cabin", "Private room in hostel", "Shared room in cabin", "Room in bed and breakfast",
            "Shared room in hotel", "Private room in bed and breakfast", "Room in boutique hotel", "Private room in cabin", "Room in aparthotel", 
            "Shared room in aparthotel", "Room in hostel", "Entire hostel", "Room in hotel", "Shared room in bed and breakfast", "Shared room in hostel")

apartments <- c("Private room in apartment", "Private room in serviced apartment", "Room in serviced apartment", "Entire condominium", "Entire serviced apartment",
                "Private room in condominium", "Private room in hut", "Shared room in hut", "Entire apartment", "Shared room in apartment", "Hut")

houses <- c("Entire loft", "Entire guesthouse", "Private room in guesthouse", "Entire townhouse", "Private room in tiny house", "Shared room in house", "Entire house",
            "Entire villa", "Private room in house", "Private room in ryokan", "Shared room in ryokan", "Private room in townhouse", "Room in ryokan",
            "Shared room in guesthouse", "Entire guest suite", "Private room in villa", "Tiny house", "Entire bungalow", "Shared room in tiny house")

data <- data %>%
  mutate(
    property_type = ifelse(property_type %in% houses, 
                           "House", data$property_type))

data <- data %>%
  mutate(
    property_type = ifelse(property_type %in% apartments,
                           "Apartment", data$property_type))

data <- data %>%
  mutate(
    property_type = ifelse(property_type %in% hotels,
                           "Hotel", data$property_type),
    f_property_type = factor(property_type))


data <- data %>%
  filter(property_type %in% c("Apartment", "House", "Hotel"))

table(data$property_type)


#Room type as factor
table(data$room_type)

data <- data %>%
  mutate(
    room_type = ifelse(room_type == "Hotel room",
                       "Private room", data$room_type),
    f_room_type = factor(room_type))



# Rename 
data$f_room_type2 <- factor(ifelse(data$f_room_type== "Entire home/apt", "Entire/Apt",
                                   ifelse(data$f_room_type== "Private room", "Private",
                                          ifelse(data$f_room_type== "Shared room", "Shared", "."))))


# neighbourhood_cleansed as factors
table(data$neighbourhood_cleansed)
data <- data %>%
  mutate(f_neighbourhood_cleansed = factor(neighbourhood_cleansed))

#---------------------------------------------------------------------------------------------------------------------------
## Create Numerical variables
data <- data %>%
  mutate(
    price_day = price,
    p_host_response_rate = as.numeric(host_response_rate))

#-------------------------------------------------------------------

# add new numeric columns from certain columns
numericals <- c("accommodates","review_scores_rating","number_of_reviews",
                "reviews_per_month","minimum_nights","beds")
data <- data %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)

#create days since first review
data <- data %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))

#filter for relevant rooms

# We focus on normal apartments, 2<n<6
table(data$f_property_type)
data <- data %>% filter(f_property_type == "Apartment")
#data <- data %>% filter(f_room_type == "Entire home/apt")
data <- data %>% filter(n_accommodates <= 6 & n_accommodates >= 2)

# create dummy vars
#amenities
data$amenities<-gsub("\\[","",data$amenities)
data$amenities<-gsub("\\]","",data$amenities)
data$amenities<-gsub('\\"',"",data$amenities)
data$amenities<-gsub('\\u....',"",data$amenities)
data$amenities<-gsub("\\\\","",data$amenities)
data$amenities<-gsub(",\\s+",",",data$amenities)
data$amenities<-tolower(data$amenities) 
data$amenities<-as.list(strsplit(data$amenities, ","))


levs <- levels(factor(unlist(data$amenities)))
data<-cbind(data,as.data.frame(do.call(rbind, lapply(lapply(data$amenities, factor, levs), table))))

source("https://raw.githubusercontent.com/fasihatif/Data-Analysis-1-2-3/master/Data_Analysis_3/functions/aggregate_columns.R")

aggregate_columns("stove")
data <- data %>% rename("stove_gas_or_elect" = col_name)
aggregate_columns("tv")
data <- data %>% rename("tv" = col_name)
aggregate_columns("wifi")
data <- data %>% rename("wifi" = col_name)
aggregate_columns("children")
data <- data %>% rename("children" = col_name)
aggregate_columns("hostgreets")
data <- data %>% rename("hostgreets" = col_name)
aggregate_columns("paidparking")
data <- data %>% rename("paidparking" = col_name)

dummies <- names(data)[seq(88,363)]

data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))
# keep columns if contain d_, n_,f_, p_, usd_ and some others
data <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, price_day, id)


data$price_day<-gsub("\\$","",data$price_day)
data$price_day<-gsub("\\,","",data$price_day)
data$price_day<-as.numeric(data$price_day)

#####################
### look at price ###
#####################

data$usd_price_day <- data$price_day * 0.0096 #conversion to USD
summary(data$usd_price_day)
data <- data %>%
  mutate(ln_usd_price_day = log(usd_price_day))
data <- data %>%
  filter(usd_price_day <275)


# Squares and further values to create
data <- data %>%
  mutate(n_accommodates2=n_accommodates^2, 
         ln_accommodates=log(n_accommodates) ,
         ln_accommodates2=log(n_accommodates)^2,
         ln_beds = log(n_beds),
         ln_number_of_reviews = log(n_number_of_reviews+1)
  )

# Pool num of reviews to 3 categories: none, 1-51 and >51
data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))


# Pool and categorize the number of minimum nights: 1,2,3, 3+
data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F))

# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)


#------------------------------------------------------------------------------------------------
#TODO continue


# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# what to do with missing values? 
# 1. drop if no target
data <- data %>%
  drop_na(price)


# 2. imput when few, not that important
data <- data %>%
  mutate(
    #n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms), #assume at least 1 bath
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),0, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_accommodates),
  ) 

# 3. drop columns when many missing not imortant
to_drop <- c("usd_cleaning_fee", "p_host_response_rate")
data <- data %>%
  select(-one_of(to_drop))

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# 4. Replace missing variables re reviews with zero, when no review + add flags
data <- data %>%
  mutate(
    flag_days_since=ifelse(is.na(n_days_since),1, 0),
    n_days_since =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month),
    flag_n_number_of_reviews=ifelse(n_number_of_reviews==0,1, 0)
  )
table(data$flag_days_since)

# redo features
# Create variables, measuring the time since: squared, cubic, logs
data <- data %>%
  mutate(
    ln_days_since = log(n_days_since+1),
    ln_days_since2 = log(n_days_since+1)^2,
    ln_days_since3 = log(n_days_since+1)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3,
    ln_review_scores_rating = log(n_review_scores_rating),
    ln_days_since=ifelse(is.na(ln_days_since),0, ln_days_since),
    ln_days_since2=ifelse(is.na(ln_days_since2),0, ln_days_since2),
    ln_days_since3=ifelse(is.na(ln_days_since3),0, ln_days_since3),
  )

# Look at data
datasummary_skim(data$id)
datasummary_skim(data, 'categorical' )
#skimr::skim(data)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

#remove not frequent dummy variables
irrelevant_dummies <- which(colMeans(data %>% select(matches("^d_.*"))) < 0.05) %>% names()
data <- data[,-which(names(data) %in% irrelevant_dummies)]


# N=6072

write_csv(data, data_out)

str(data)


