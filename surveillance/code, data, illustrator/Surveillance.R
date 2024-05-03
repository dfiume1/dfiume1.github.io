require(tidyverse)
require(sf)
require(ggmap)
require(ggnewscale)

# NOTE: This code was not written with the intent on being published,
# and is thus pretty messy. I can't publish the DOJ data set, but you
# might be able to access it through other means through the linked cite.
# I'm not really expecting anyone to look at this, but if you do and have
# questions, feel free to email me! daniel_fiume@brown.edu   (add alumni
# if after August 2025)

# census data 
census_c <- read_csv("census.csv") %>%
  select(STNAME, CTYNAME, pop = POPESTIMATE2020) 
census_c

census_s <- census_c %>%
  filter(STNAME == CTYNAME)
census_s


# mapping: big atlas dataset does have entries for alsaka/hawaii: look to explore
# the fixes for that 
ignoreStates <- c("Alaska", "Hawaii", "Puerto Rico")
counties <- st_read("counties/cb_2021_us_county_20m.shp") %>%
  filter(!(STATE_NAME %in% ignoreStates)) %>%
  left_join(census_c,
            by = join_by(NAMELSAD == CTYNAME, STATE_NAME == STNAME))
  
states <- st_read("states/cb_2021_us_state_20m.shp") %>%
  left_join(census_s,
            by = join_by(NAME == STNAME))
counties
states



# Atlas of Surveillance: Main database of the project by the 
# Electronic Frontier Foundation
atlasRaw <- read_csv("Atlas of Surveillance-20240321.csv") %>%
  rename(Juris = `Type of Juris`)
atlasRaw
# NOTES:
# Shows technological capabilities of police departments across the country
# Lends itself well to mapping. 



# Types of technology
cameras <- c("Automated License Plate Readers", "Body-worn Cameras", "Face Recognition", "Drones")
alt <- c("Cell-site Simulator", "Predictive Policing", "Real-Time Crime Center", "Third-party Investigative Platforms")
analysis <- c("Camera Registry", "Gunshot Detection", "Video Analysis") 
bar <- c("Automated License Plate Readers", "Body-worn Cameras", "Face Recognition", "Drones"
         ,"Cell-site Simulator", "Predictive Policing", "Real-Time Crime Center", "Third-party Investigative Platforms", "Camera Registry")

### COUNTY LEVEL

# join with county geography
atlasCounty <- counties %>%
  left_join(atlasRaw,
            by = join_by(NAMELSAD == County, STUSPS == State)) %>%
  filter(Juris == "County" | Juris == "Parish") %>%
  filter(Technology %in% cameras) 
atlasCounty

# Looking at type of technology (county level juristiction)
camera <- ggplot() + 
  facet_wrap(~Technology, ncol=2) +
  geom_sf(data = atlasCounty, aes(fill = Technology), color = NA) +
  # Adding State Data
  geom_sf(data = states, fill = NA, color = "#BBBBBB") +
  #geom_sf_text(data = states, aes(label = STUSPS), size = 3) +
  coord_sf(crs = st_crs("EPSG:5070")) + 
  scale_fill_manual(values = c("#5a5b5d", "#005288", "#c0c2c4", "#0078ae")) +
  # remove x and y labels
  theme_void() + 
  labs(
    title = "Say Cheese!",
    subtitle = "Police departments with county-level juristiction, by type of camera technology use",
    caption = "Source: EFF"
  ) 
camera


# STATE LEVEL

# join with state geography
atlasState <- states %>%
  left_join(atlasRaw,
            by = join_by(STUSPS == State)) %>%
  filter(Juris == "State" | Juris == "Statewide") %>%
  filter(Technology %in% cameras) 
atlasState

camera <- ggplot() + 
  facet_wrap(~Technology, ncol=2) +
  #geom_sf(data = atlasCounty, aes(fill = Technology), color = NA) +
  # Adding State Data
  geom_sf(data = atlasState, aes(fill = Technology), color = "#BBBBBB") +
  scale_fill_manual(values = c("#F3EB9F", "#b8cfde", "#b8d9e8", "#e08493")) +
  ggnewscale::new_scale_fill() + 
  geom_sf(data = atlasCounty, aes(fill = Technology), color = NA) +
  
  scale_fill_manual(values = c("#CABB37", "#003e67", "#3d98c1", "#660919")) +
  
  geom_sf(data = states, fill = NA, color = "#333333") +
  coord_sf(crs = st_crs("EPSG:5070")) + 
  theme_void() + 
  labs(
    title = "Say Cheese!",
    subtitle = "Police departments by state and county level juristiction, by technology use",
    caption = "Source: EFF Survelliance Atlas"
  ) 
camera

# Bar chart for states

# This double groupby is to get rid of duplicate articles reporting the same
# technology for the same state
stateTech <- atlasState %>%
  group_by(NAME, Technology, pop) %>%
  summarize()
stateTech
stateTechTotals <- stateTech %>%
  group_by(Technology) %>%
  summarize(total = sum(pop), num = n()) %>%
  mutate(total = (total / 328571074) * 100) %>%
  arrange(-total)
stateTechTotals

# total 2020 lower 48: 328,571,074
  
bar <- ggplot(stateTechTotals, aes(x = total, y = reorder(Technology, total))) +
  geom_bar(stat = "identity", 
           fill = c("#660919", "#003e67", "#CABB37", "#3d98c1", "#c0c2c4", "#c0c2c4", "#c0c2c4", "#c0c2c4", "#c0c2c4")) +
  xlim(0, 100) + 
  geom_text(aes(label=round(total, 1))) + 
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line()
  ) + 
  labs(
    title = "Facial Recognition is All-American",
    subtitle = "Percent of Americans subject to statewide law-enforcement technologies",
    caption = "Sources: EFF Survelliance Atlas and the 2020 U.S.Census"
  ) 
bar




# LOCAL / MUNICIPALITY LEVEL
# TODO: Geolocate? For now just use 

# DOJ Census of State and Local Law Enforcement Agencies (2018)
# https://bjs.ojp.gov/library/publications/census-state-and-local-law-enforcement-agencies-2018-statistical-tables
# https://www.icpsr.umich.edu/web/NACJD/studies/38771
load("~/Desktop/CS/Data1500/Final Project/ICPSR_38771/DS0001/38771-0001-Data.rda")
DOJ <- da38771.0001 %>%
  select(AGENCYNAME:COUNTY, POPULATION, OPBUDGET) %>%
  #mutate(AGENCYNAME = toString(AGENCYNAME)) 
  mutate(AGENCYNAME = gsub("\\s+", "", AGENCYNAME)) %>%
  mutate(CITY = gsub("\\s+", "", CITY)) 
DOJ

notLocal = c("Federal", "County", "State", "Statewide", "State Agency",
             "Port", "Railroad", "Parish")

# join with county geography
atlasLocal <- counties %>%
  left_join(atlasRaw,
            by = join_by(NAMELSAD == County, STUSPS == State)) %>%
  filter(!Juris %in% notLocal) %>%
  filter(Technology %in% cameras) %>%
  select(Juris, STUSPS, NAMELSAD, Agency, Technology, pop, geometry, City)
atlasLocal

atlasLocal <- atlasLocal %>%
  #mutate(Agency = toString(Agency)) %>%
  mutate(Agency = gsub("`", "", Agency)) %>%
  mutate(Agency = gsub("\\s+", "", Agency)) %>%
  mutate(Agency = toupper(Agency)) %>%
  mutate(City = toupper(City)) %>%
  mutate(City = gsub("\\s+", "", City)) %>%
  left_join(DOJ, 
            by = join_by(Agency == AGENCYNAME, City == CITY)) %>%
  filter(!is.na(POPULATION)) %>%
  # give 10% population leeway due to 2018 versus 2020 - otherwise
  # give priority to the census 
  filter(POPULATION < ((11 * pop) / 10)) 
atlasLocal

pctLocal = atlasLocal %>%
  mutate(pctCounty = (POPULATION / pop) * 100) %>%
  group_by(Technology, NAMELSAD, STUSPS) %>%
  summarize(totalpct = sum(pctCounty)) %>%
  mutate(totalpct = if_else(totalpct > 100, 100, totalpct)) %>%
  filter(!is.na(totalpct)) %>%
  mutate(bins = cut(
    totalpct, 
    c(0, 25, 50, 75, 100),
    right = TRUE))
pctLocal



# Map for local: show counties that have local municpialites with the technology
# Highlight counties that have state county and local?


local <- ggplot() + 
  facet_wrap(~Technology, ncol=2) +
  #geom_sf(data = atlasCounty, aes(fill = Technology), color = NA) +
  # Adding State Data
  geom_sf(data = states, fill = NA, color = "#666666") +
  scale_fill_manual(values = c("#c0c2c4", "#666666", "#373737", "#000000")) +

  geom_sf(data = pctLocal, aes(fill = bins), color = NA) +
  
  #scale_fill_manual(values = c("#5a5b5d", "#005288", "#0078ae", "#660919")) +
  coord_sf(crs = st_crs("EPSG:5070")) + 
  theme_void() + 
  labs(
    title = "What the cops next door are using:",
    subtitle = "Percent of county population within a municipal law enforcement jurisdiction by technology use",
    caption = "Sources: EFF Survelliance Atlas, DOJ Census of State and Local Law Enforcement Agencies (2018)"
  ) 
local




# Notes: Literally just wanted to get an estimate on how many people they serve / 
# a total number of law enforcement agencies

budget = atlasLocal %>%
  # note, to get rid of data in cents versus dollars / people who reported 
  # officers rather than population, filter by max per capita spend = 2000
  filter(!is.na(OPBUDGET)) %>%
  filter((OPBUDGET / 2000) < POPULATION) %>%
  filter((OPBUDGET / POPULATION) > 1)
budget

budgetAll = DOJ %>%
  filter(!is.na(POPULATION)) %>%
  filter(!is.na(OPBUDGET)) %>%
  filter((OPBUDGET / 2000) < POPULATION) %>%
  filter((OPBUDGET / POPULATION) > 1)
budgetAll
  

scatterAll <- ggplot(budgetAll, aes(x = POPULATION, y = OPBUDGET)) +
  geom_point(color = "#e0e2e4", alpha = 1) +
  geom_point(data = budget, aes(x = POPULATION, y = OPBUDGET, color = Technology, alpha = 1)) + 
  scale_color_manual(values = c("#CABB37", "#003e67", "#3d98c1", "#660919")) + 
  xlim(0, 100000) + 
  ylim(0, 50000000) +
  geom_smooth(color = "#b0b2b4", se=FALSE) + 
  geom_smooth(data = budget, color = "#000000", aes(x = POPULATION, y = OPBUDGET), se=FALSE) + 
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank()
    #panel.grid.major.y = element_blank(),
    #panel.grid.major.x = element_line(linetype = "dotted", linewidth = 1)
  ) + 
  labs(
    title = "Local Law Enforcement using camera technologies spend more per capita",
    subtitle = "Municipal law enforcement district size versus their yearly budget",
    caption = "Sources:EFF Survelliance Atlas, DOJ Census of State and Local Law Enforcement Agencies (2018)"
  ) 
scatterAll

scatterFacet <- ggplot(budget, aes(x = POPULATION, y = OPBUDGET)) +
  facet_wrap(~Technology, ncol=2) +
  geom_point() +
  geom_smooth() + 
  scale_color_manual(values = c("#F3EB9F", "#b8cfde", "#b8d9e8","#e08493")) + 
  xlim(0, 500000) + 
  ylim(0, 100000000) + 
  theme(
    panel.grid.minor = element_blank(),
    #panel.grid.major.y = element_blank(),
    #panel.grid.major.x = element_line(linetype = "dotted", linewidth = 1)
  ) +
  labs(
    title = "",
    subtitle = "Percent of Americans subject to various statewide law-enforcement technologies",
    caption = "Source: DOJ Census of State and Local Law Enforcement Agencies (2018)"
  ) 
scatterFacet
  
























byStation <- atlasRaw %>%
  group_by(Agency, County, State, City) %>%
  summarize(count = n())
byStation

countyStation <- counties %>%
  left_join(byStation,
            by = join_by(NAMELSAD == County, STUSPS == State))
countyStation

# 17721 - 7725 = 9996
totalCount <- byStation %>%
  group_by(count) %>%
  summarise(total = n()) %>%
  # From DOJ
  rbind(list(0, 9996)) %>%
  arrange(-total)
totalCount


numTechs <- ggplot() + 
  geom_sf(data = states, fill = "#FFFFFF", color = "#BBBBBB") +
  geom_sf(data = countyStation, aes(fill = count), color = "#BBBBBB") +
  scale_fill_gradient(low = "#d6e3ec", high = "#002b47") + 
  coord_sf(crs = st_crs("EPSG:5070")) + 
  theme_void() + 
  labs(
    title = "Why have just one?",
    subtitle = "Number of distinct surveliiance technologies deployed, by county",
    caption = "Source: EFF Survelliance Atlas"
  ) 
numTechs

bar <- ggplot(totalCount, aes(x = count, y = total)) +
  geom_bar(stat = "identity", fill = c("#FFFFFF", "#d6e3ec", "#b8cfde", "#7aa5c1", "#3d7ca5", "#005288", "#003e67", "#002b47", "#001726", "#000305", "#000000")) +
  # theme_void() + 
  labs(
    title = "",
    subtitle = "Num law enforcement agencies by num publically reported survelliance technologies",
    caption = "Source: EFF Survelliance Atlas and DOJ Law Enforcement Census (2018)"
  ) 
bar

# TODO: Correlation of publicly reported technology to each other?

byTechnology <- atlasRaw %>%
  group_by(Technology) %>%
  summarize(count = n())
byTechnology

corr <- ggplot(friends, aes(y=speaker, x=name_spoken, fill=count)) + 
  geom_tile() +
  geom_text(aes(label = count)) +
  scale_fill_gradient(low = "#5a5b5d", high = "#005288") +
  theme_void() + 
  labs(
    title = "Bulk Survelliance: Do agencies use the same types of technologies together?",
    subtitle = "Correlation between ",
    caption = "Source: EFF Survelliance Atlas"
  ) 
corr
















# College Campus Surveillance database
collegeRaw <- read_csv("scholars_unders_surveillance_dataset_03-03-2021.csv")
collegeRaw
#NOTES: 
# Shows different technology university police departments use. 
# Very nice dataset, clean. Not sure if subset of survelliance atlas
# Could process the first column to get the university and then in combination with
# geolocating universites could get more accurate maps. 


# Who has your face database
faceRaw <- read_csv("who-has-your-face-agency-sharing-3-10-2020.csv")
faceRaw
#NOTES: 
# Shows whether different DMV's / agencies can use facial recognition data alongside
# accessing data from other departments. 
# Could be useful in combination with state pop / driver license data
# Otherwise probably not going to use this

replace_na(faceRaw, 0)
replace(faceRaw, "X", 1)

# TODO: Turn Column names into state, take the first row for facial recognition

 