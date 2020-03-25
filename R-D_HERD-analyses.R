library(tidyverse)


# ------- Upload HERD R&D file from the Data file in R-D/R-D_HERD-analyses -------
University_RD <- read_csv("Data/2020-03-19_HERD_sort-by-field.csv")


# ------- Clean HERD csv so that analysis can be done -------

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


# ------- Upload state GDP file from the Data file in R-D/R-D_HERD-analyses -------
BEA_GDP <- read_csv("Data/BEA_GDP.csv")

# ------- Clean BEA GDP csv so that analysis can be done -------

# Remove the first 4 rows, which are just blank rows or unncessary text
BEA_GDP <- BEA_GDP[-c(1:5),]

# View tibble to check that the row removal worked properly
BEA_GDP

# Remove state code column
BEA_GDP <- select(BEA_GDP, -'SAGDP2N Gross domestic product (GDP) by state 1/')

# View tibble to check that the column removal worked properly
BEA_GDP

# Rename the columns so that they show years rather than X and a number
BEA_GDP <- BEA_GDP %>%
  rename(
    state = X2,
    gdp_2006 = X3,
    gdp_2007 = X4,
    gdp_2008 = X5,
    gdp_2009 = X6,
    gdp_2010 = X7,
    gdp_2011 = X8,
    gdp_2012 = X9,
    gdp_2013 = X10,
    gdp_2014 = X11,
    gdp_2015 = X12,
    gdp_2016 = X13,
    gdp_2017 = X14,
    gdp_2018 = X15
  )

# View tibble to check that the rows renamed properly
BEA_GDP

# Remove last 4 rows, which have extraneous data and unnecessary regional 
# aggregations
BEA_GDP <- slice(BEA_GDP, 1:(n()-12))

# View tibble to check that the rows were properly removed
BEA_GDP


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
  summarize(total_2010 = sum(year_2010), total_2011 = sum(year_2011), 
            total_2012 = sum(year_2012), total_2013 = sum(year_2013),
            total_2014 = sum(year_2014), total_2015 = sum(year_2015),
            total_2016 = sum(year_2016), total_2017 = sum(year_2017),
            total_2018 = sum(year_2018))

# View tibble
total_RD_all_states


# -------- Analysis 2: Business-funded higher ed R&D spend by state -------

# Show business-funded R&D spend, all states plus DC
business_RD_all_states <- University_RD %>%
  group_by(state) %>%
  filter(!state %in% c("American Samoa", "Guam", "Puerto Rico", 
                       "Virgin Islands")) %>%
  filter(source_of_funds == "Business") %>%
  summarize(biz_2010 = sum(year_2010), biz_2011 = sum(year_2011), 
            biz_2012 = sum(year_2012), biz_2013 = sum(year_2013),
            biz_2014 = sum(year_2014), biz_2015 = sum(year_2015),
            biz_2016 = sum(year_2016), biz_2017 = sum(year_2017),
            biz_2018 = sum(year_2018))

# View tibble
business_RD_all_states


# -------- Analysis 3: Total advanced sector R&D spend by state --------

# Show total advanced sector R&D spend, all states plus DC
total_adv_sector_RD_all_states <- University_RD %>%
  group_by(state) %>%
  filter(!state %in% c("American Samoa", "Guam", "Puerto Rico", 
                       "Virgin Islands")) %>%
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
  filter(source_of_funds == "Total") %>%
  summarize(t_adv_2010 = sum(year_2010), t_adv_2011 = sum(year_2011), 
            t_adv_2012 = sum(year_2012), t_adv_2013 = sum(year_2013),
            t_adv_2014 = sum(year_2014), t_adv_2015 = sum(year_2015),
            t_adv_2016 = sum(year_2016), t_adv_2017 = sum(year_2017),
            t_adv_2018 = sum(year_2018))

# View tibble
total_adv_sector_RD_all_states


# -------- Analysis 4: Business-funded advanced sector R&D spend by state --------

# Show total advanced sector R&D spend, all states plus DC
biz_adv_sector_RD_all_states <- University_RD %>%
  group_by(state) %>%
  filter(!state %in% c("American Samoa", "Guam", "Puerto Rico", 
                       "Virgin Islands")) %>%
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
  filter(source_of_funds == "Business") %>%
  summarize(b_adv_2010 = sum(year_2010), b_adv_2011 = sum(year_2011), 
            b_adv_2012 = sum(year_2012), b_adv_2013 = sum(year_2013),
            b_adv_2014 = sum(year_2014), b_adv_2015 = sum(year_2015),
            b_adv_2016 = sum(year_2016), b_adv_2017 = sum(year_2017),
            b_adv_2018 = sum(year_2018))

# View tibble
biz_adv_sector_RD_all_states


# -------- Analysis 5: Total higher ed R&D spend as a share of state GDP -------

# Join tibble showing total R&D spend by state to the tibble showing GDP by state so
# they are in the same file for calculations
tot_GDPshare <- left_join(x = total_RD_all_states, y = BEA_GDP, by = "state")

#View tibble to make sure join was successful
tot_GDPshare

# Multiply the GDP numbers by 1,000 (because they are in millions of dollars and the
# R&D numbers are in thousands) and divide the R&D spend by GDP spend to get higher
# ed GDP share
tot_GDPshare <- tot_GDPshare %>%
  mutate(
    tot_share_2010 = total_2010/(gdp_2010 * 1000),
    tot_share_2011 = total_2011/(gdp_2011 * 1000),
    tot_share_2012 = total_2012/(gdp_2012 * 1000),
    tot_share_2013 = total_2013/(gdp_2013 * 1000),
    tot_share_2014 = total_2014/(gdp_2014 * 1000),
    tot_share_2015 = total_2015/(gdp_2015 * 1000),
    tot_share_2016 = total_2016/(gdp_2016 * 1000),
    tot_share_2017 = total_2017/(gdp_2017 * 1000),
    tot_share_2018 = total_2018/(gdp_2018 *1000)
  )

# View tibble to make sure calculation went through properly
tot_GDPshare

# Remove columns from tibbles that were joined together to show just higer ed R&D
# share
tot_GDPshare <- select(tot_GDPshare, -(total_2010:gdp_2018))

# View tibble to make sure columns were removed
tot_GDPshare


# ------- Analysis 6: Advanced sector R&D as a share of state GDP -------

# Join tibble showing advanced sector R&D by state to tibble showing state GDP to
# get them in the same file
adv_GDPshare <- left_join(x = total_adv_sector_RD_all_states, y = BEA_GDP, 
                          by = "state")

#View tibble to make sure join was successful
adv_GDPshare

# Multiply the GDP numbers by 1,000 (because they are in millions of dollars and the
# R&D numbers are in thousands) and divide the R&D spend by GDP spend to get higher
# ed GDP share
adv_GDPshare <- adv_GDPshare %>%
  mutate(
    adv_share_2010 = t_adv_2010/(gdp_2010 * 1000),
    adv_share_2011 = t_adv_2011/(gdp_2011 * 1000),
    adv_share_2012 = t_adv_2012/(gdp_2012 * 1000),
    adv_share_2013 = t_adv_2013/(gdp_2013 * 1000),
    adv_share_2014 = t_adv_2014/(gdp_2014 * 1000),
    adv_share_2015 = t_adv_2015/(gdp_2015 * 1000),
    adv_share_2016 = t_adv_2016/(gdp_2016 * 1000),
    adv_share_2017 = t_adv_2017/(gdp_2017 * 1000),
    adv_share_2018 = t_adv_2018/(gdp_2018 * 1000)
  )

#View tibble to make sure calculation was successful
adv_GDPshare

# Remove columns from tibbles that were joined together to show just higer ed R&D
# share
adv_GDPshare <- select(adv_GDPshare, -(t_adv_2010:gdp_2018))

# View tibble to make sure columns were removed
tot_GDPshare


# ------- Analysis 7: Computer and information sciences (proxy for ICT only) research
# by state --------

# Show total ICT R&D spend, all states plus DC
ict_RD_all_states <- University_RD %>%
  group_by(state) %>%
  filter(!state %in% c("American Samoa", "Guam", "Puerto Rico", 
                       "Virgin Islands")) %>%
  filter(field == "Computer and information sciences") %>%
  filter(source_of_funds == "Total") %>%
  summarize(ict_2010 = sum(year_2010), ict_2011 = sum(year_2011), 
            ict_2012 = sum(year_2012), ict_2013 = sum(year_2013),
            ict_2014 = sum(year_2014), ict_2015 = sum(year_2015),
            ict_2016 = sum(year_2016), ict_2017 = sum(year_2017),
            ict_2018 = sum(year_2018))

# View tibble
ict_RD_all_states


# ------- Analysis 8: Computer and information sciences (proxy for ICT only) research
# as a share of stage GDP -------

# Join tibble showing ICT R&D by state to tibble showing state GDP to get them in the
# same file
ict_GDPshare <- left_join(x = ict_RD_all_states, y = BEA_GDP, by = "state")

#View tibble to make sure join was successful
ict_GDPshare

# Multiply the GDP numbers by 1,000 (because they are in millions of dollars and the
# R&D numbers are in thousands) and divide the R&D spend by GDP spend to get higher
# ed GDP share
ict_GDPshare <- ict_GDPshare %>%
  mutate(
    ict_share_2010 = ict_2010/(gdp_2010 * 1000),
    ict_share_2011 = ict_2011/(gdp_2011 * 1000),
    ict_share_2012 = ict_2012/(gdp_2012 * 1000),
    ict_share_2013 = ict_2013/(gdp_2013 * 1000),
    ict_share_2014 = ict_2014/(gdp_2014 * 1000),
    ict_share_2015 = ict_2015/(gdp_2015 * 1000),
    ict_share_2016 = ict_2016/(gdp_2016 * 1000),
    ict_share_2017 = ict_2017/(gdp_2017 * 1000),
    ict_share_2018 = ict_2018/(gdp_2018 * 1000)
  )

#View tibble to make sure calculation was successful
ict_GDPshare

# Remove columns from tibbles that were joined together to show just higer ed R&D
# share
ict_GDPshare <- select(ict_GDPshare, -(ict_2010:gdp_2018))

# View tibble to make sure columns were removed
ict_GDPshare



