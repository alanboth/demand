library(sf)

samplePrecinctPopulation <- function(dataDir,
                                     baseOutputDir,
                                     precinctOutputDir,
                                     samplePercent,
                                     locationFile,
                                     locationLayer,
                                     bufferDistance,
                                     newPeople
                                     ) {
  
  # setting up: base sample and SA1 populations
  #----------------------------------------------------------------------------
  # read in base sample (used to ensure new precinct people don't appear in base sample)
  baseSample <- read.csv(gzfile(paste0('../', baseOutputDir, "/2.sample/sample.csv.gz")))
  
  # make table of SA2 names and codes
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
  
  # determine SA1 populations, from SA2 population files
  SA1.pops <- NULL
  
  echo(paste0("Extracting SA1 populations figures from ", nrow(SA2s), " Melbourne SA2 population files\n"))
  for(i in 1:nrow(SA2s)) {
    printProgress(i,".")
    
    SA1.pop <- read.csv(gzfile(SA2s$file[i])) %>%
      group_by(SA1_7DIGCODE) %>%
      summarise(census_pop = n()) 
    
    SA1.pops <- rbind(SA1.pops, SA1.pop)
  }
  cat("\n")
  echo(paste0("Extracted SA1 population figures for  ", nrow(SA1.pops), " SA1s\n"))
  
  # read in SA1 locations, calculate area and join to population figures
  SA1s <- st_read(paste0(dataDir, "/absRegionsReprojected.sqlite"),
                  layer = "sa1_2016_aust") %>%
    mutate(orig_area = as.numeric(st_area(.))) %>%
    left_join(SA1.pops, by = c("sa1_7digitcode_2016" = "SA1_7DIGCODE")) %>%
    # replace NAs with 0 (these are SA1s with zero population, eg parks)
    mutate(census_pop = ifelse(is.na(census_pop), 0, census_pop))
  
  
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
  
  # calculate current census population of SA1s in precincts, with population 
  # for partial SA1 apportioned on basis of area 
  #----------------------------------------------------------------------------
  # intersect precincts with SA1s and calculate census population of each intersection area
  precincts.intersected <- precincts %>%
    st_intersection(., SA1s) %>%
    mutate(isec_area = as.numeric(st_area(.)),
           isec_pop = (isec_area/orig_area) * census_pop) %>%
    st_drop_geometry()
  
  # sum of intersection census populations for each precinct
  precinct.pops <- precincts.intersected %>%
    group_by(precinct_id) %>%
    summarise(precinct_pop = sum(isec_pop))
  
  # apportioned additional population for each intersection area: 
  # additional population for precinct apportioned between intersection areas based on their census populations
  for (i in 1:nrow(precincts.intersected)) {
    precinct.pop <- precinct.pops[precinct.pops$precinct_id == precincts.intersected[i, "precinct_id"], "precinct_pop"]
    precincts.intersected[i, "additional_pop"] <- (precincts.intersected[i, "isec_pop"] / precinct.pop) * newPeople
  }
  
  
  # for each precinct's SA1, determine sample required, based on samplePercent
  #----------------------------------------------------------------------------
  precincts.intersected <- precincts.intersected %>%
    mutate(sample = additional_pop * samplePercent / 100)
  
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
    precinct.SA1s <- precincts.intersected %>%
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
    left_join(., SA1s %>% dplyr::select(sa1_maincode_2016, sa1_7digitcode_2016, sa2_maincode_2016), 
              by = c("sa1_maincode_2016")) %>%
    left_join(., SA2s %>% dplyr::select(SA2_NAME, SA2_MAINCODE_2016),
              by = c("sa2_maincode_2016" = "SA2_MAINCODE_2016")) %>%
    filter(!is.na(SA2_NAME)) %>%  # excludes any which are not in SA2 population files (eg Moorabbin Airport SA2, zero population)
    dplyr::select(-GEOMETRY)


  # obtain the sample, taking the required number from each SA1
  #----------------------------------------------------------------------------
  precinctSample <- NULL
  
  echo(paste0("Selecting a ", samplePercent, "% population sample of additional precinct residents from ", nrow(SA1.additional.pops), " SA1 areas (can take a while)\n"))
  for (i in 1:nrow(SA1.additional.pops)) {
    printProgress(i,".")
    
    # extract all people who live in the SA1 and were not in base sample
    file <- SA2s %>%
      filter(SA2_NAME == as.character(SA1.additional.pops[i, "SA2_NAME"])) %>%
      .$file
    candidates <- read.csv(gzfile(file)) %>%
      filter(SA1_7DIGCODE == as.character(SA1.additional.pops[i, "sa1_7digitcode_2016"])) %>%
      filter(!AgentId %in% baseSample$AgentId)
    
    # sample the relevant number of people from the candidate people
    sampleSet <- candidates[sample(nrow(candidates), 
                                   as.numeric(SA1.additional.pops[i, "sample"])),]
    
    # add SA1_MAINCODE (required for consistency with base sample)
    sampleSet <- sampleSet %>%
      mutate(SA1_MAINCODE_2016 = as.numeric(SA1.additional.pops[i, "sa1_maincode_2016"]))
    
    precinctSample <- rbind(precinctSample, sampleSet)
  }
  cat("\n")
  echo(paste0("Extracted a sample of ", nrow(precinctSample), " people\n"))
  
  # write output
  write.csv(precinctSample, file=gzfile(paste0('../',precinctOutputDir,'/2.sample/sample.csv.gz')), quote=TRUE, row.names = FALSE)
  
}
