library(tidyverse)


# ------- Upload the file from the Data file in R-D/R-D_HERD-analyses -------
University_RD <- read_csv("Data/2020-03-19_HERD_sort-by-field.csv")


# ------- Clean csv so that analysis can be done -------

# Remove the first 7 rows, which are just blank rows or unncessary text
University_RD <- University_RD[-c(1:7),]

# View tibble to check that the row removal worked properly
University_RD

# Rename the columns so that they have the right title and show years rather than
# X and numbers
University_RD <- University_RD %>%
  rename(
    state = 'Data Download from NCSES Interactive Data Tool',
    institution_name = X2,
    field = X3,
    source_of_funds = X4,
    year_2018 = X5,
    year_2017 = X6,
    year_2016 = X7,
    year_2015 = X8,
    year_2014 = X9,
    year_2013 = X10,
    year_2012 = X11,
    year_2011 = X12,
    year_2010 = X13
    )

# View tibble to check that the renames worked on the correct row
University_RD
  
# Convert "-" values into NA
University_RD <- University_RD %>%
  mutate_if(is.character, list(~na_if(.,"-")))

# View tibble to check that "-" converted into NA
University_RD

# Replace NA values with 0
University_RD <- University_RD %>%
  replace(is.na(.), 0)

# View tibble to check that NAs converted into 0s
University_RD

# Remove commas from numbers
University_RD$year_2018 <- gsub(',', '', University_RD$year_2018)
University_RD$year_2017 <- gsub(',', '', University_RD$year_2017)
University_RD$year_2016 <- gsub(',', '', University_RD$year_2016)
University_RD$year_2015 <- gsub(',', '', University_RD$year_2015)
University_RD$year_2014 <- gsub(',', '', University_RD$year_2014)
University_RD$year_2013 <- gsub(',', '', University_RD$year_2013)
University_RD$year_2012 <- gsub(',', '', University_RD$year_2012)
University_RD$year_2011 <- gsub(',', '', University_RD$year_2011)
University_RD$year_2010 <- gsub(',', '', University_RD$year_2010)

# View tibble to check that commas were removed
University_RD

# Convert year columns to numeric type
University_RD$year_2018 <- as.numeric(as.character(University_RD$year_2018))
University_RD$year_2017 <- as.numeric(as.character(University_RD$year_2017))
University_RD$year_2016 <- as.numeric(as.character(University_RD$year_2016))
University_RD$year_2015 <- as.numeric(as.character(University_RD$year_2015))
University_RD$year_2014 <- as.numeric(as.character(University_RD$year_2014))
University_RD$year_2013 <- as.numeric(as.character(University_RD$year_2013))
University_RD$year_2012 <- as.numeric(as.character(University_RD$year_2012))
University_RD$year_2011 <- as.numeric(as.character(University_RD$year_2011))
University_RD$year_2010 <- as.numeric(as.character(University_RD$year_2010))

# View tibble to check that number vectors are numeric
University_RD

# Remove last 6 rows, which have extraneous data
University_RD <- slice(University_RD, 1:(n()-6))

# View tibble to check that rows were removed
University_RD

# Reorder columns so earliest year is on left
University_RD <- University_RD %>%
  select(state:source_of_funds, year_2010, year_2011, year_2012, year_2013, 
         year_2014, year_2015, year_2016, year_2017, year_2018)

# View tibble to check that columns were reordered
University_RD


# ----------------------------------- Analyses -------------------------------------
# ----------------------------------------------------------------------------------

# -------- Analysis 1: Total higher ed R&D spend by state -------

# Show total R&D spend, all states plus DC
total_RD_all_states <- University_RD %>%
  group_by(state) %>%
  filter(!state %in% c("American Samoa", "Guam", "Puerto Rico", 
                       "Virgin Islands")) %>%
  filter(institution_name == "Total") %>%
  filter(source_of_funds == "Total") %>%
  summarize(sum2010 = sum(year_2010), sum2011 = sum(year_2011), 
            sum2012 = sum(year_2012), sum2013 = sum(year_2013),
            sum2014 = sum(year_2014), sum2015 = sum(year_2015),
            sum2016 = sum(year_2016), sum2017 = sum(year_2017),
            sum2018 = sum(year_2018))

# View tibble
total_RD_all_states


# -------- Analysis 2: Business-funded higher ed R&D spend by state -------

# Show business-funded R&D spend, all states plus DC
business_RD_all_states <- University_RD %>%
  group_by(state) %>%
  filter(!state %in% c("American Samoa", "Guam", "Puerto Rico", 
                       "Virgin Islands")) %>%
  filter(source_of_funds == "Business") %>%
  summarize(sum2010 = sum(year_2010), sum2011 = sum(year_2011), 
            sum2012 = sum(year_2012), sum2013 = sum(year_2013),
            sum2014 = sum(year_2014), sum2015 = sum(year_2015),
            sum2016 = sum(year_2016), sum2017 = sum(year_2017),
            sum2018 = sum(year_2018))

# View tibble
business_RD_all_states


# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------

# ------- Several test filters to leverage -------

# Filter research fields column to show only fields relevant to the advanced sector
University_RD %>%
  filter(field %in% c("Computer and information sciences",
         "Aerospace, aeronautical, and astronautical engineering",
         "Bioengineering and biomedical engineering",
         "Chemical engineering",
         "Civil engineering",
         "Electrical, electronic, and communications engineering",
         "Industrial and manufacturing engineering",
         "Mechanical engineering",
         "Metallurgical and materials engineering",
         "Other engineering",
         "Agricultural sciences",
         "Biological and biomedical sciences",
         "Health sciences",
         "Other life sciences",
         "Mathematics and statistics",
         "Astronomy and astrophysics",
         "Chemistry",
         "Materials science",
         "Physics",
         "Other physical sciences"))


# Filter research fields column to show only fields relevant to the advanced sector 
# and filter state to just Indiana
University_RD %>%
  filter(field %in% c("Computer and information sciences",
                      "Aerospace, aeronautical, and astronautical engineering",
                      "Bioengineering and biomedical engineering",
                      "Chemical engineering",
                      "Civil engineering",
                      "Electrical, electronic, and communications engineering",
                      "Industrial and manufacturing engineering",
                      "Mechanical engineering",
                      "Metallurgical and materials engineering",
                      "Other engineering",
                      "Agricultural sciences",
                      "Biological and biomedical sciences",
                      "Health sciences",
                      "Other life sciences",
                      "Mathematics and statistics",
                      "Astronomy and astrophysics",
                      "Chemistry",
                      "Materials science",
                      "Physics",
                      "Other physical sciences")) %>%
  filter(state == "Indiana")


# Filter research fields column to show only fields relevant to the advanced sector,
# filter state to just Indiana, and filter source of funds to just business
University_RD %>%
  filter(field %in% c("Computer and information sciences",
                      "Aerospace, aeronautical, and astronautical engineering",
                      "Bioengineering and biomedical engineering",
                      "Chemical engineering",
                      "Civil engineering",
                      "Electrical, electronic, and communications engineering",
                      "Industrial and manufacturing engineering",
                      "Mechanical engineering",
                      "Metallurgical and materials engineering",
                      "Other engineering",
                      "Agricultural sciences",
                      "Biological and biomedical sciences",
                      "Health sciences",
                      "Other life sciences",
                      "Mathematics and statistics",
                      "Astronomy and astrophysics",
                      "Chemistry",
                      "Materials science",
                      "Physics",
                      "Other physical sciences")) %>%
  filter(state == "Indiana") %>%
  filter(source_of_funds == "Business")


# Convert "-" values into NA
University_RD <- University_RD %>%
  mutate_if(is.character, list(~na_if(.,"-")))


# ------- Compare Indiana overall research in advanced sector-relevant fields -------
# ------- to other states --------

# Filter research fields column to show only fields relevant to the advanced sector
University_RD %>%
  filter(field %in% c("Computer and information sciences",
                      "Aerospace, aeronautical, and astronautical engineering",
                      "Bioengineering and biomedical engineering",
                      "Chemical engineering",
                      "Civil engineering",
                      "Electrical, electronic, and communications engineering",
                      "Industrial and manufacturing engineering",
                      "Mechanical engineering",
                      "Metallurgical and materials engineering",
                      "Other engineering",
                      "Agricultural sciences",
                      "Biological and biomedical sciences",
                      "Health sciences",
                      "Other life sciences",
                      "Mathematics and statistics",
                      "Astronomy and astrophysics",
                      "Chemistry",
                      "Materials science",
                      "Physics",
                      "Other physical sciences"))




University_RD %>%
  group_by(state, source_of_funds) %>%
  summarize(s2018 = sum(University_RD$'2018'), s2017 = sum(University_RD$'2017'))

sum(University_RD$'2018')


# --------------------------------- Extra content ----------------------------------

# Trying to do this: https://stackoverflow.com/questions/21607464/what-is-the-equivalent-of-the-sumif-function-in-r 

# Code format to show total R&D funding by state (but do not use this because there
# is a 'total' line included in the csv, so if you use this you will double count the
# total)
University_RD %>%
  group_by(state) %>%
  summarize(sum2018 = sum(year_2018), sum2017 = sum(year_2017), 
            sum2016 = sum(year_2016), sum2015 = sum(year_2015),
            sum2014 = sum(year_2014), sum2013 = sum(year_2013),
            sum2012 = sum(year_2012), sum2011 = sum(year_2011),
            sum2010 = sum(year_2010))


# Relatedly...
# This works
sum(University_RD$'2018')

# But this doesn't
University_RD %>%
  sum('2018')

