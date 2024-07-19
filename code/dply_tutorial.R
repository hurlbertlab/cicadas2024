setwd("Z:/R Tutorials")
bbs = read.csv("NC_warblers.csv")
mass = read.csv("bodysize_abundance_data.csv")

# counting how many sites a species occurs at
occupancy = count(bbs, SpeciesName)
# above we are creating a dataframe called occupancy that,
# reads in bbs and then counts the sites each Species name is at

# renaming a column in a dataframe
names(occupancy)[2] = 'occ'
# above the names function renames the 2nd column in
# occupancy to occ

my_species1 = bbs[bbs$SpeciesName == 'Pine Warbler']
my_species1 = filter(bbs, SpeciesName == 'Pine Warbler')
# both functions above show how many routes pine warblers appear in
# the first is a traditional method while 2nd is dplyr
# both functions filter the bbs dataframe to show where 
# SpeciesName is Pine Warbler

species_richness = count(bbs, Route)
# creates a dataframe called species_richness that shows
# how many species occur on each route
species_richness2 = filter(bbs, Route == 23)
#creates a dataframe of the species seen only on route 23

# taking the mean of each species at the site at which they occur
bbs_by_species = group_by(bbs, SpeciesName)
# this organizes bbs by species name
pop = summarize(bbs_by_species, meanN = mean(Abundance))
#after grouping by species, summarize creates a dataframe
# with the species name and the mean abundance of each species across sites

#Your Turn: find the max abundance for each species observed
# we want to group by species abundance
# then we want to summarize for max abundance
bbs_by_species = group_by(bbs, SpeciesName)
max_abundance = summarize(bbs_by_species, maxN = max(Abundance))

# Your Turn: find max abundance for each route
bbs_by_route = group_by(bbs, Route)
max_route_abundance = summarize(bbs_by_route, maxN = max(Abundance))

#joining dataframes
#needs at least one column that contains the same information
birds1 = left_join(occupancy, pop, by = c('SpeciesName' = 'SpeciesName'))
# c() combines values into a vector or a list
#telling R that 'Species Name' in occupancy is the same as 'SpeciesName' in  pop

#Your Turn: create a dataframe joining data on body mass with birds1
birds2 = left_join(mass, birds1, by = c('Species' = 'SpeciesName'))

#pipe operators eliminate the need for intermediate objects (ex: bbs_by_species)
pop = bbs %>%
  group_by(SpeciesName) %>%
  summarize(meanN = mean(Abundance)) %>%

#Your Turn: use piping to join average pop density by species data with average
# body mass data by species
pop2 = bbs %>% 
