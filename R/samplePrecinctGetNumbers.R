library(readxl)

samplePrecinctGetNumbers <- function() {
  
  # 1. Read in data - dwelling type and number of people
  # -----------------------------------------------------------------------------
  # Tablebuilder: SA1s in Melbourne GCCSA (rows) v STRD and NPRD (columns)
  # Note that 'total' row at end is whole of Melbourne GCCSA
  dwell.person <- as.data.frame(read_excel(paste0(dataDir, "/STRD and NPRD Melbourne GCCSA SA1s 2016.xlsx")))
  
  # compile and assign column names
  STRD <- unique(as.character(dwell.person[9, -1])) # each unique STRD (dwelling type)
  STRD <- STRD[!is.na(STRD)]
  NPRD <- unique(as.character(dwell.person[10, -1]))  # each unique NPRD (one person - 8+ persons, NA)
  geog <- unique(as.character(dwell.person[11, 1]))
  names <- c(geog)
  for (i in 1:length(STRD)) {
    for (j in 1:length(NPRD)) {
      names <- c(names, paste(STRD[i], "-", NPRD[j]))
    }
  }
  colnames(dwell.person) <- names
  
  # remove non-data start/end info rows
  dwell.person <- dwell.person[complete.cases(dwell.person), ]  # remove incomplete start/end info rows
  dwell.person <- dwell.person[-1,] # remove remaining intro row
  
  # filter to GCCSA ('total' row is figures for Greater Melb GCCSA)
  dwell.person.GMel <- dwell.person %>%
    filter(SA1 == "Total")
  
  
  # 2. Find dwelling numbers and people per dwelling for apartments and other attached dwellings for Greater Melbourne
  # -----------------------------------------------------------------------------
  # calculate total numbers of apartments ('apt') and other attached dwellings ('att'), and people for each
  dense.person <- dwell.person.GMel %>%
    mutate(apt = as.numeric(`Flat or apartment in a one or two storey block - One person`) +
             as.numeric(`Flat or apartment in a one or two storey block - Two persons`) +
             as.numeric(`Flat or apartment in a one or two storey block - Three persons`) +
             as.numeric(`Flat or apartment in a one or two storey block - Four persons`) +
             as.numeric(`Flat or apartment in a one or two storey block - Five persons`) +
             as.numeric(`Flat or apartment in a one or two storey block - Six persons`) +
             as.numeric(`Flat or apartment in a one or two storey block - Seven persons`) +
             as.numeric(`Flat or apartment in a one or two storey block - Eight or more persons`) +
             as.numeric(`Flat or apartment in a three storey block - One person`) +
             as.numeric(`Flat or apartment in a three storey block - Two persons`)  +
             as.numeric(`Flat or apartment in a three storey block - Three persons`) +
             as.numeric(`Flat or apartment in a three storey block - Four persons`) +
             as.numeric(`Flat or apartment in a three storey block - Five persons`) +
             as.numeric(`Flat or apartment in a three storey block - Six persons`) +
             as.numeric(`Flat or apartment in a three storey block - Seven persons`) +
             as.numeric(`Flat or apartment in a three storey block - Eight or more persons`) +
             as.numeric(`Flat or apartment in a four or more storey block - One person`) +
             as.numeric(`Flat or apartment in a four or more storey block - Two persons`) +
             as.numeric(`Flat or apartment in a four or more storey block - Three persons`) +
             as.numeric(`Flat or apartment in a four or more storey block - Four persons`) +
             as.numeric(`Flat or apartment in a four or more storey block - Five persons`) +
             as.numeric(`Flat or apartment in a four or more storey block - Six persons`) +
             as.numeric(`Flat or apartment in a four or more storey block - Seven persons`) +
             as.numeric(`Flat or apartment in a four or more storey block - Eight or more persons`) +
             as.numeric(`Flat or apartment attached to a house - One person`) +
             as.numeric(`Flat or apartment attached to a house - Two persons`) +
             as.numeric(`Flat or apartment attached to a house - Three persons`) +
             as.numeric(`Flat or apartment attached to a house - Four persons`) +
             as.numeric(`Flat or apartment attached to a house - Five persons`) +
             as.numeric(`Flat or apartment attached to a house - Six persons`) +
             as.numeric(`Flat or apartment attached to a house - Seven persons`) +
             as.numeric(`Flat or apartment attached to a house - Eight or more persons`),
           apt.person = as.numeric(`Flat or apartment in a one or two storey block - One person`) +
             as.numeric(`Flat or apartment in a one or two storey block - Two persons`) * 2 +
             as.numeric(`Flat or apartment in a one or two storey block - Three persons`) * 3 +
             as.numeric(`Flat or apartment in a one or two storey block - Four persons`) * 4 +
             as.numeric(`Flat or apartment in a one or two storey block - Five persons`) * 5 +
             as.numeric(`Flat or apartment in a one or two storey block - Six persons`) * 6 +
             as.numeric(`Flat or apartment in a one or two storey block - Seven persons`) * 7 +
             as.numeric(`Flat or apartment in a one or two storey block - Eight or more persons`) * 8 +
             as.numeric(`Flat or apartment in a three storey block - One person`) +
             as.numeric(`Flat or apartment in a three storey block - Two persons`) * 2 +
             as.numeric(`Flat or apartment in a three storey block - Three persons`) * 3 +
             as.numeric(`Flat or apartment in a three storey block - Four persons`) * 4 +
             as.numeric(`Flat or apartment in a three storey block - Five persons`) * 5 +
             as.numeric(`Flat or apartment in a three storey block - Six persons`) * 6 +
             as.numeric(`Flat or apartment in a three storey block - Seven persons`) * 7 +
             as.numeric(`Flat or apartment in a three storey block - Eight or more persons`) * 8 +
             as.numeric(`Flat or apartment in a four or more storey block - One person`) +
             as.numeric(`Flat or apartment in a four or more storey block - Two persons`) * 2 +
             as.numeric(`Flat or apartment in a four or more storey block - Three persons`) * 3 +
             as.numeric(`Flat or apartment in a four or more storey block - Four persons`) * 4 +
             as.numeric(`Flat or apartment in a four or more storey block - Five persons`) * 5 +
             as.numeric(`Flat or apartment in a four or more storey block - Six persons`) * 6 +
             as.numeric(`Flat or apartment in a four or more storey block - Seven persons`) * 7 +
             as.numeric(`Flat or apartment in a four or more storey block - Eight or more persons`) * 8 +
             as.numeric(`Flat or apartment attached to a house - One person`) +
             as.numeric(`Flat or apartment attached to a house - Two persons`) * 2 +
             as.numeric(`Flat or apartment attached to a house - Three persons`) * 3 +
             as.numeric(`Flat or apartment attached to a house - Four persons`) * 4 +
             as.numeric(`Flat or apartment attached to a house - Five persons`) * 5 +
             as.numeric(`Flat or apartment attached to a house - Six persons`) * 6 +
             as.numeric(`Flat or apartment attached to a house - Seven persons`) * 7 +
             as.numeric(`Flat or apartment attached to a house - Eight or more persons`) * 8,
           att = as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - One person`) +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - Two persons`) +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - Three persons`) +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - Four persons`)  +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - Five persons`) +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - Six persons`) +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - Seven persons`) +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - Eight or more persons`) +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - One person`) +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - Two persons`) +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - Three persons`) +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - Four persons`)  +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - Five persons`) +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - Six persons`) +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - Seven persons`) +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - Eight or more persons`),
           att.person = as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - One person`) +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - Two persons`) * 2 +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - Three persons`) * 3 +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - Four persons`) * 4 +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - Five persons`) * 5 +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - Six persons`) * 6 +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - Seven persons`) * 7 +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with one storey - Eight or more persons`) * 8 +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - One person`) +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - Two persons`) * 2 +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - Three persons`) * 3 +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - Four persons`) * 4 +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - Five persons`) * 5 +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - Six persons`) * 6 +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - Seven persons`) * 7 +
             as.numeric(`Semi-detached, row or terrace house, townhouse etc. with two or more storeys - Eight or more persons`) * 8) %>%
    dplyr::select(SA1, apt, apt.person, att, att.person) 
  
  # calculate dwelling numbers, and people per appartment or other attached dwelling
  apt <- dense.person[1, "apt"]
  att <- dense.person[1, "att"]
  person.per.apt <- dense.person[1, "apt.person"] / apt
  person.per.att <- dense.person[1, "att.person"] / att
  
  
  # 3. return dwelling numbers and people per dwelling
  # -----------------------------------------------------------------------------
  return(list(apt, att, person.per.apt, person.per.att))
  
}
