library(sf)

samplePrecinctPopulation <- function(dataDir,
                                     baseOutputDir,
                                     precinctOutputDir,
                                     samplePercent,
                                     locationFile,
                                     locationLayer,
                                     bufferDistance
                                     ) {
  
  # constants: apartment and other attached dwelling numbers, and people per dwelling
  #----------------------------------------------------------------------------
  # Calculated by samplePrecinctGetNumbers.R, which obtains numbers from 2016 Census Greater Melbourne GCCSA
  # Alternatively, other constants could be are used.  Note that:
  # - 'APT' and 'ATT' are numbers of apartments/other attached dwellings; they could be absolute numbers
  #   or' percentages (eg 53, 47); their proportions are applied to the people per respective dwelling type
  # - 'PERSON.PER.APT' an 'PERSON.PER.ATT' are average numbers of people per dwelling
  source("samplePrecinctGetNumbers.R", local = TRUE)
  dwelling.constants <- samplePrecinctGetNumbers()
  
  APT <- dwelling.constants[[1]]  # 231289 (number of apartments in Greater Melbourne GCCSA)
  ATT <- dwelling.constants[[2]]  # 264405 (number of other attached dwellings in Greater Melbourne GCCSA)
  PERSON.PER.APT <- dwelling.constants[[3]]  # 1.835937 (average people per apartment)
  PERSON.PER.ATT <- dwelling.constants[[4]]  # 2.315822 (average people per other attached dwewlling)
  
  PERSON.PER.DENSE.DWELLING <- ((APT * PERSON.PER.APT) + (ATT * PERSON.PER.ATT)) / (APT + ATT)  # 2.09191
  
  
  # setting up: meshblock dwellings and population, and SRL dwelling increases
  #----------------------------------------------------------------------------
  # read in base sample (used to ensure new precinct people don't appear in base sample) [not currently used]
  # baseSample <- read.csv(gzfile(paste0('../', baseOutputDir, "/2.sample/sample.csv.gz")))

  # read in meshblock dwelling and population counts
  meshblock.counts <- read.csv(gzfile(paste0(dataDir, "/2016 census mesh block counts.csv.gz"))) %>%
    replace(is.na(.), 0) %>% # where population/dwelling numbers not completed
    mutate(MB_CODE_2016 = as.numeric(MB_CODE_2016))  # ignore 'NAs by coercion' warning, triggered by incomplete rows at end 
  
  # read in mrshblock locations, calculate area and join to population and dwelling figures
  meshblocks <- st_read(paste0(dataDir, "/absRegionsReprojected.sqlite"),
                        layer = "mb_2016_aust") %>%
    mutate(orig_area = as.numeric(st_area(.))) %>%
    left_join(meshblock.counts, by = c("mb_code_2016" = "MB_CODE_2016"))
  
  # make table of SA1 codes and corresponding SA2s
  SA1Table <- meshblocks %>%
    st_drop_geometry() %>%
    distinct(sa1_maincode_2016, sa1_7digitcode_2016, sa2_maincode_2016)
  
  # make table of SA2 codes and corresopnding names
  SA2Table <- read.csv(gzfile(paste0(dataDir, "/sa1_2016_aust.csv.gz"))) %>%
    distinct(SA2_MAINCODE_2016, SA2_NAME_2016)
  
  # make table of SA2 population files, with names and codes
  SA2s <- data.frame(file = list.files(path = dataDir, 
                                       pattern = "\\persons.csv.gz$", 
                                       recursive = TRUE, 
                                       full.names = TRUE), 
                     stringsAsFactors=FALSE) %>%
    mutate(SA2_NAME = stringr::str_extract(string = file, pattern = "(?<=SA2/).*(?=/population)")) %>%
    left_join(., SA2Table, by = c("SA2_NAME" = "SA2_NAME_2016"))
  
  # read in SRL base case household projections from https://bigbuild.vic.gov.au/library/suburban-rail-loop/business-and-investment-case ,
  # appendix C1, table B-18.  Code assumes contains columns for 'precinct', 'basecase' and 'programcaseoptionA'
  SRL.household <- read.csv(paste0(dataDir, "/SRLBaseCase2056HouseholdProjections.csv")) %>%
    rename(precinct = 1)  # fixes read-in naming error in column 1
  
  
  # create precincts: based on buffers around point locations, without overlaps
  #----------------------------------------------------------------------------
  # read in precinct locations
  precinct.locations <- st_read(paste0(dataDir, "/", locationFile),
                                layer = locationLayer) %>%
    mutate(precinct_id = row_number())
  
  # buffer precincts to bufferDistance
  precinct.buffers <- st_buffer(precinct.locations, bufferDistance)
  
  # make voronoi tesselation for area containing precincts (used to eliminate overlaps)
  tesselation <- st_voronoi(st_union(precinct.locations), st_union(precinct.buffers)) %>%
    st_cast(.)  # splits geom collection into separate polygons
  
  # intersect buffers with tesselation
  intersection.areas <- st_intersection(precinct.buffers, tesselation)
  
  # precincts: keep only the intersection areas containing the original centroids (overlaps eliminated)
  precincts <- NULL
  for (i in 1:nrow(precinct.locations)) {
    selected.area <- intersection.areas %>%
      filter(precinct_id == i) %>%
      st_filter(., precinct.locations %>% filter(precinct_id == i), .predicate = st_contains)
    precincts <- rbind(precincts, selected.area)
  }
  
  # library(ggplot2)
  # library(ggspatial)
  # ggplot() +
  # annotation_map_tile(type="osmgrayscale",zoom=12, alpha=0.6) +
  #   geom_sf(data = precincts, color = "blue", fill = NA) + 
  #   geom_sf(data = precinct.locations, color = "red")
  # 
  
  # calculate current census dwellings and population of meshblocks in precincts,  
  # with dwellings and population for partial meshblocks apportioned on basis of area 
  #----------------------------------------------------------------------------
  # intersect precincts with meshblocks and calculate census dwellings and population of each intersection area
  precincts.intersected <- precincts %>%
    st_intersection(., meshblocks) %>%
    mutate(isec_area = as.numeric(st_area(.)),
           isec_dwell = (isec_area/orig_area) * Dwelling,
           isec_pop = (isec_area/orig_area) * Person) %>%
    st_drop_geometry()
  
  # join dwelling and pop data to precinct meshblocks, and calculate additional population required
  # for each meshblock, based on the base case uplift for the relevant precinct
  precinct.meshblocks.additional.pop <- precincts.intersected %>%
    left_join(SRL.household, by = c("stations" = "precinct")) %>%
    mutate(dwelling.uplift = (ProgramCaseOptionA - BaseCase) / BaseCase,  # the SRL BAse Case uplift for the precinct
           additional.dwellings = isec_dwell * dwelling.uplift,
           additional.pop = additional.dwellings * PERSON.PER.DENSE.DWELLING)
  
  # summarise additional meshblock population by precinct and SA1, to find number required for each SA1
  precinct.SA1s.additional.pop <- precinct.meshblocks.additional.pop %>%
    group_by(precinct_id, sa1_maincode_2016) %>%
    summarise(SA1.additional.pop = sum(additional.pop))
  

  # for each precinct's SA1, determine sample required, based on samplePercent
  #----------------------------------------------------------------------------
  precinct.SA1s.additional.pop <- precinct.SA1s.additional.pop %>%
    mutate(sample = SA1.additional.pop * samplePercent / 100)
  
  # where using small sample size, too many SA1s may be rounded to zero
  # function to keep largest remainders, resulting in total remaining correct, from
  # https://stackoverflow.com/questions/32544646/round-vector-of-numerics-to-integer-while-preserving-their-sum
  round_preserve_sum <- function(x, digits = 0) {
    up <- 10 ^ digits
    x <- x * up
    y <- floor(x)
    indices <- tail(order(x-y), round(sum(x)) - sum(y))
    y[indices] <- y[indices] + 1
    y / up
  } 
  
  # determine additional sample pop for each SA1 area in each precinct
  SA1.additional.pops <- NULL
  for (i in 1:nrow(precinct.locations)) {
    precinct.SA1s <- precinct.SA1s.additional.pop %>%
      filter(precinct_id == i)
    precinct.SA1s$sample <- round_preserve_sum(precinct.SA1s$sample)
    SA1.additional.pops <- rbind(SA1.additional.pops, precinct.SA1s)
  }
  
  # sum of additional population for each SA1 (some may be in 2 precincts), joined to SA2 codes and names
  SA1.additional.pops <- SA1.additional.pops %>%
    group_by(sa1_maincode_2016) %>%
    # total sample for each SA1 is the sum of the samples for each precinct it falls in 
    summarise(sample = sum(sample)) %>%
    # remove any with zero sample required
    filter(sample > 0) %>%
    # join SA2 codes and names
    left_join(., SA1Table, 
              by = c("sa1_maincode_2016")) %>%
    left_join(., SA2s %>% dplyr::select(SA2_NAME, SA2_MAINCODE_2016),
              by = c("sa2_maincode_2016" = "SA2_MAINCODE_2016")) %>%
    filter(!is.na(SA2_NAME)) # excludes any which are not in SA2 population files (eg Moorabbin Airport SA2, zero population)


  # obtain the sample, taking the required number from each SA1
  #----------------------------------------------------------------------------
  precinctSample <- NULL
  
  echo(paste0("Selecting a ", samplePercent, "% population sample of additional precinct residents from ", nrow(SA1.additional.pops), " SA1 areas (can take a while)\n"))
  for (i in 1:nrow(SA1.additional.pops)) {
    printProgress(i,".")
    
    # extract all people who live in the SA1 [and were not in base sample - not currently used]
    file <- SA2s %>%
      filter(SA2_NAME == as.character(SA1.additional.pops[i, "SA2_NAME"])) %>%
      .$file
    candidates <- read.csv(gzfile(file)) %>%
      filter(SA1_7DIGCODE == as.character(SA1.additional.pops[i, "sa1_7digitcode_2016"])) #%>%
      # filter(!AgentId %in% baseSample$AgentId)  # [not currently used
    
    # sample the relevant number of people from the candidate people
    sampleSet <- candidates[sample(nrow(candidates), 
                                   size = as.numeric(SA1.additional.pops[i, "sample"]),
                                   replace = TRUE), ]  #  sampling with replacement - ensure never run out of people
    
    # add SA1_MAINCODE (required for consistency with base sample)
    sampleSet <- sampleSet %>%
      mutate(SA1_MAINCODE_2016 = as.numeric(SA1.additional.pops[i, "sa1_maincode_2016"]))
    
    precinctSample <- rbind(precinctSample, sampleSet)
  }
  
  # add a suffix to AgentIDs, so that any duplicates will have unique IDs ("PR" stands for "precinct")
  precinctSample <- precinctSample %>%
    mutate(AgentId = paste0(AgentId, "PR", row_number()))
    
  cat("\n")
  echo(paste0("Extracted a sample of ", nrow(precinctSample), " people\n"))
  
  # write output
  write.csv(precinctSample, file=gzfile(paste0('../',precinctOutputDir,'/2.sample/sample.csv.gz')), quote=TRUE, row.names = FALSE)
  
}
