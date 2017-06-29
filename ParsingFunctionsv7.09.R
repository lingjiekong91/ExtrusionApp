#This file contains the functions necessary to PPS object and parse the text.


singleExtrusionParameters <- function(PPS, attribute_names){
  
  column_length <- ncol(PPS)
  row_length <- nrow(PPS)
  
  section_list <- splitPPS(PPS)
  first_section <- section_list$'first section'
  second_section <- section_list$'second section'
  third_section <- section_list$'third section'
  
  
  ## Tooling names ##
  die_names <- c("die")
  dieland_names <- c("die land", "die land length")
  tip_names <- c("mandrel", "tip", "tip/mandrel", "mandrel/tip")
  tipland_names <- c("mandrel land", "mandrel land length", "tip/mandrel land", 
                     "tip/mandrel land lengths", "mandrel/tip land", "mandrel/tip land lengths",
                     "tip land", "tip land length")
  
  ## Temperature names ##
  feedthroat_names <- c("feed", "feedthroat", "circulator")
  barrel_names <- c("barrel", "zone")
  clamp_names <- c("clamp")
  adapter_names <- c("adapter")
  
  
  ## Attribute names ##
  od_names <- c("Finished OD", "OD", "outer diameter")
  id_names <- c("Finished ID", "ID", "inner diameter")
  wall_names <- c("average wall", "wall", "wall thickness")
  oor_names <- c("OOR", "out of roundness", "out-of-roundness", "out of round", "ovality", 
                 "Wall Uniformity", "roundness")
  concentricity_names <- c("concentricity")
  length_names <- c("length")
  perpendicularity_names <- c("perpendicularity")
  
  output <- rep(list(-1000), length(attribute_names)) #will store the outputs into a list
  
  names(output) <- attribute_names
  
  
  ###output[["Extrusion Type"]] <- getExtrusionType(PPS)
  
  {#get tooling
    output[["Die Size (in)"]] <- getTooling(first_section, die_names)
    output[["Die Land Length (in)"]] <- getTooling(first_section, dieland_names)
    output[["Tip Size (in)"]] <- getTooling(first_section, tip_names)
    output[["Tip Land Length (in)"]] <- getTooling(first_section, tipland_names)
  } #end get Tooling
  
  {#get screw
    output[["Screw Print"]] <- getScrew(first_section)
  }
  
  
  {#get FCA
    output[["Feedthroat Temperature (F)"]] <- getFCATemperature(second_section, feedthroat_names)
    output[["Clamp Temperature (F)"]] <- getFCATemperature(second_section, clamp_names)
    output[["Adapter Temperature (F)"]] <- getFCATemperature(second_section, adapter_names)  
  }#end get FCA
  {#get barrel and die temperatures
    barrel_temperatures <- getBarrelTemperatures(second_section, barrel_names)
    
    output[["Barrel Zone 1 Temperature (F)"]] <- barrel_temperatures[1]
    output[["Barrel Zone 2 Temperature (F)"]] <- barrel_temperatures[2]
    output[["Barrel Zone 3 Temperature (F)"]] <- barrel_temperatures[3]
    
    die_temperatures <- getDieTemperatures(second_section, die_names)
    
    output[["Die 1 Temperature (F)"]] <- die_temperatures[1]
    output[["Die 2 Temperature (F)"]] <- die_temperatures[2]
  }#end barrel and die temperatures
  {#get attributes
    output[["Inner Diameter (in)"]] <- getAttribute(third_section, id_names, "Target")
    output[["Outer Diameter (in)"]] <- getAttribute(third_section, od_names, "Target")
    output[["Wall Thickness (in)"]] <- getAttribute(third_section, wall_names, "Target")
    output[["Out-of-Roundness (in)"]] <- getAttribute(third_section, oor_names, "USL")
    output[["Concentricity (in)"]] <- getAttribute(third_section, concentricity_names, "USL")
    output[["Length (in)"]] <- getAttribute(third_section, length_names, "Target")
    output[["Perpendicularity (in)"]] <- getAttribute(third_section, perpendicularity_names, "USL")
    
  }#end attributes
  
  return(output) #returns the list of outputs
  
} #end singleExtrusionParameters

taperedExtrusionParameters <- function(PPS, attribute_names){
  #this will get the parameters for tapered extrusion in a similar format to single extrusion
  
  column_length <- ncol(PPS)
  row_length <- nrow(PPS)
  
  section_list <- splitPPS(PPS)
  first_section <- section_list$'first section'
  second_section <- section_list$'second section'
  third_section <- section_list$'third section'
  
  
  #### Names to Search through the PPS ####
  
  ## Temperature names ##
  feedthroat_names <- c("feed", "feedthroat", "circulator")
  barrel_names <- c("barrel", "zone")
  clamp_names <- c("clamp")
  adapter_names <- c("adapter")
  die_names <- c("die")
  
  
  ## Attribute names ##
  prox_od_names <- c("Proximal Finished OD", "Proximal OD", "Proximal outer diameter",
                     "Prox Finished OD", "Prox OD", "Prox outer diameter",
                     "Finished OD, proximal", "Proximal OD, proximal", "Proximal outer diameter, proximal",
                     "Finished OD, prox", "Proximal OD, prox", "Proximal outer diameter, prox",
                     "OD @ A")
  prox_id_names <- c("Proximal Finished ID", "Proximal ID", "Proximal inner diameter",
                     "Prox Finished ID", "Prox ID", "Prox inner diameter",
                     "Finished ID, proximal", "ID, proximal", "inner diameter, proximal",
                     "Finished ID, prox", "ID, prox", "inner diameter, prox",
                     "ID @ A")
  prox_wall_names <- c("Proximal average wall", "Proximal wall", "Proximal wall thickness",
                       "Prox average wall", "Prox wall", "Prox wall thickness",
                       "average wall, proximal", "wall, proximal", "wall thickness, proximal",
                       "average wall, prox", "wall, prox", "wall thickness, prox")
  prox_oor_names <- c("Proximal OOR", "Proximal out of roundness", "Proximal out-of-roundness", 
                      "Proximal out of round", "Proximal ovality", "Proximal Wall Uniformity", 
                      "Proximal roundness",
                      "Prox OOR", "Prox out of roundness", "Prox out-of-roundness", 
                      "Prox out of round", "Prox ovality", "Prox Wall Uniformity", 
                      "Prox roundness",
                      "OOR, proximal", "out of roundness, proximal", "out-of-roundness, proximal", 
                      "out of round, proximal", "ovality, proximal", "Wall Uniformity, proximal", 
                      "roundness, proximal",
                      "OOR, prox", "out of roundness, prox", "out-of-roundness, prox", 
                      "out of round, prox", "ovality, prox", "Wall Uniformity, prox", 
                      "roundness, prox")
  prox_concentricity_names <- c("Proximal concentricity",
                                "Prox concentricity",
                                "concentricity, proximal",
                                "concentricity, prox")
  prox_length_names <- c("Proximal length",
                         "Prox length",
                         "length, proximal",
                         "length, prox",
                         "A to B Distance")
  prox_perpendicularity_names <- c("Proximal perpendicularity",
                                   "Prox perpendicularity",
                                   "perpendicularity, proximal",
                                   "perpendicularity, prox")
  
  dist_od_names <- c("Distal Finished OD", "Distal OD", "Distal outer diameter",
                     "Dist Finished OD", "Dist OD", "Dist outer diameter",
                     "Finished OD, distal", "OD, distal", "outer diameter, distal",
                     "Finished OD, dist", "OD, dist", "outer diameter, dist",
                     "OD @ D")
  dist_id_names <- c("Distal Finished ID", "Distal ID", "Distal inner diameter",
                     "Dist Finished ID", "Dist ID", "Dist inner diameter",
                     "Finished ID, distal", "ID, distal", "inner diameter, distal",
                     "Finished ID, dist", "ID, dist", "inner diameter, dist",
                     "ID @ D")
  dist_wall_names <- c("Distal average wall", "Distal wall", "Distal wall thickness",
                       "Dist average wall", "Dist wall", "Dist wall thickness",
                       "average wall, distal", "wall, distal", "wall thickness, distal",
                       "average wall, dist", "wall, dist", "wall thickness, dist")
  dist_oor_names <- c("Distal OOR", "Distal out of roundness", "Distal out-of-roundness", 
                      "Distal out of round", "Distal vality", "Distal Wall Uniformity", 
                      "Distal roundness",
                      "Dist OOR", "Dist out of roundness", "Dist out-of-roundness", 
                      "Dist out of round", "Dist ovality", "Dist Wall Uniformity", 
                      "Dist roundness",
                      "OOR, distal", "out of roundnes, distals", "out-of-roundness, distal", 
                      "out of round, distal", "ovality, distal", "Wall Uniformity, distal", 
                      "roundness, distal",
                      "OOR, dist", "out of roundness, dist", "out-of-roundness, dist", 
                      "out of round, dist", "ovality, dist", "Wall Uniformity, dist", 
                      "roundness, dist")
  dist_concentricity_names <- c("Distal concentricity",
                                "Dist concentricity",
                                "concentricity, distal",
                                "concentricity, dist")
  dist_length_names <- c("Distal length",
                         "Dist length",
                         "length, distal",
                         "length, dist")
  dist_perpendicularity_names <- c("Distal perpendicularity",
                                   "Dist perpendicularity",
                                   "perpendicularity, distal",
                                   "perpendicularity, dist")
  
  proxtransition_length_names <- c("Proximal + Transition Length", "A to C Distance",
                                   "Cut \"Y\" max","Cut  \"Y\"  max")
  
  transition_length_names <- c("Transition Length", "Taper Length", "Length, Transition", 
                               "Length, Taper", "Cut \"Z\" ref","Cut  \"Z\"  ref")
  
  length_names <- c("length", "total length", "overall length")
  
  
  #### Naming the Outputs ####
  
  output <- rep(list(-1000), length(attribute_names)) #will store the outputs into a list
  
  names(output) <- attribute_names
  
  ###output[["Extrusion Type"]] <- getExtrusionType(PPS)
  
  {#get FCA
    output[["Feedthroat Temperature (F)"]] <- getFCATemperature(second_section, feedthroat_names)
    output[["Clamp Temperature (F)"]] <- getFCATemperature(second_section, clamp_names)
    output[["Adapter Temperature (F)"]] <- getFCATemperature(second_section, adapter_names)  
  }#end get FCA
  {#get barrel and die temperatures
    barrel_temperatures <- getBarrelTemperatures(second_section, barrel_names)
    
    output[["Barrel Zone 1 Temperature (F)"]] <- barrel_temperatures[1]
    output[["Barrel Zone 2 Temperature (F)"]] <- barrel_temperatures[2]
    output[["Barrel Zone 3 Temperature (F)"]] <- barrel_temperatures[3]
    
    die_temperatures <- getDieTemperatures(second_section, die_names)
    
    output[["Die 1 Temperature (F)"]] <- die_temperatures[1]
    output[["Die 2 Temperature (F)"]] <- die_temperatures[2]
  }#end barrel and die temperatures
  {#get attributes
    #prox
    output[["Proximal Inner Diameter (in)"]] <- getAttribute(third_section, prox_id_names, "Target")
    output[["Proximal Outer Diameter (in)"]] <- getAttribute(third_section, prox_od_names, "Target")
    output[["Proximal Wall Thickness (in)"]] <- getAttribute(third_section, prox_wall_names, "Target")
    output[["Proximal Out-of-Roundness (in)"]] <- getAttribute(third_section, prox_oor_names, "USL")
    output[["Proximal Concentricity (in)"]] <- getAttribute(third_section, prox_concentricity_names, "USL")
    output[["Proximal Perpendicularity (in)"]] <- getAttribute(third_section, prox_perpendicularity_names, "USL")
    
    #dist
    output[["Distal Inner Diameter (in)"]] <- getAttribute(third_section, dist_id_names, "Target")
    output[["Distal Outer Diameter (in)"]] <- getAttribute(third_section, dist_od_names, "Target")
    output[["Distal Wall Thickness (in)"]] <- getAttribute(third_section, dist_wall_names, "Target")
    output[["Distal Out-of-Roundness (in)"]] <- getAttribute(third_section, dist_oor_names, "USL")
    output[["Distal Concentricity (in)"]] <- getAttribute(third_section, dist_concentricity_names, "USL")
    output[["Distal Perpendicularity (in)"]] <- getAttribute(third_section, dist_perpendicularity_names, "USL")
    
    #lengths
    output[["Total Length (in)"]] <- getAttribute(third_section, length_names, "Target")
    output[["Proximal Length (in)"]] <- getAttribute(third_section, prox_length_names, "Target")
    output[["Distal Length (in)"]] <- getAttribute(third_section, dist_length_names, "Target")
    output[["Transition Length (in)"]] <- getAttribute(third_section, transition_length_names, "Target")
    output[["Proximal and Transition Length (in)"]] <- getAttribute(third_section, proxtransition_length_names, "Target")
    
  }#end attributes
  
  return(output) #returns the list of outputs
  
} #end taperedExtrusionParameters

multiExtrusionParameters <- function(PPS, attribute_names){
  
  column_length <- ncol(PPS)
  row_length <- nrow(PPS)
  
  section_list <- splitPPS(PPS)
  first_section <- section_list$'first section'
  second_section <- section_list$'second section'
  third_section <- section_list$'third section'
  
  ## Temperature names ##
  feedthroat_names <- c("feed", "feedthroat", "circulator")
  barrel_names <- c("barrel", "zone")
  clamp_names <- c("clamp")
  adapter_names <- c("adapter")
  die_names <- c("die")
  
  
  ## Attribute names ##
  od_names <- c("Finished OD", "OD", "outer diameter")
  id_names <- c("Finished ID", "ID", "inner diameter")
  overall_wall_names <- c("total wall", "total wall thickness", "overall wall",
                          "overall wall thickness")
  inner_wall_names <- c("inner wall", "inner layer wall", "inner wall thickness", 
                        "inner layer wall thickness")
  middle_wall_names <- c("middle wall", "middle layer wall", "middle wall thickness", 
                         "middle layer wall thickness")
  outer_wall_names <- c("outer wall", "outer layer wall", "outer wall thickness", 
                        "outer layer wall thickness")
  oor_names <- c("OOR", "out of roundness", "out-of-roundness", "out of round", "ovality", 
                 "Wall Uniformity", "roundness")
  concentricity_names <- c("concentricity")
  length_names <- c("length")
  perpendicularity_names <- c("perpendicularity")    
  
  
  
  output <- rep(list(-1000), length(attribute_names)) #will store the outputs into a list
  
  names(output) <- attribute_names
  
  
  output[["Extrusion Type"]] <- getExtrusionType(PPS)
  
  {#get FCA
    output[["Feedthroat Temperature (F)"]] <- getFCATemperature(second_section, feedthroat_names)
    output[["Clamp Temperature (F)"]] <- getFCATemperature(second_section, clamp_names)
    output[["Adapter Temperature (F)"]] <- getFCATemperature(second_section, adapter_names)  
  }#end get FCA
  {#get barrel and die temperatures
    barrel_temperatures <- getBarrelTemperatures(second_section, barrel_names)
    
    output[["Barrel Zone 1 Temperature (F)"]] <- barrel_temperatures[1]
    output[["Barrel Zone 2 Temperature (F)"]] <- barrel_temperatures[2]
    output[["Barrel Zone 3 Temperature (F)"]] <- barrel_temperatures[3]
    
    die_temperatures <- getDieTemperatures(second_section, die_names)
    
    output[["Die 1 Temperature (F)"]] <- die_temperatures[1]
    output[["Die 2 Temperature (F)"]] <- die_temperatures[2]
  }#end barrel and die temperatures
  {#get attributes
    #prox
    output[["Proximal Inner Diameter (in)"]] <- getAttribute(third_section, prox_id_names, "Target")
    output[["Proximal Outer Diameter (in)"]] <- getAttribute(third_section, prox_od_names, "Target")
    output[["Proximal Wall Thickness (in)"]] <- getAttribute(third_section, prox_wall_names, "Target")
    output[["Proximal Out-of-Roundness (in)"]] <- getAttribute(third_section, prox_oor_names, "USL")
    output[["Proximal Concentricity (in)"]] <- getAttribute(third_section, prox_concentricity_names, "USL")
    output[["Proximal Perpendicularity (in)"]] <- getAttribute(third_section, prox_perpendicularity_names, "USL")
    
    #dist
    output[["Distal Inner Diameter (in)"]] <- getAttribute(third_section, dist_id_names, "Target")
    output[["Distal Outer Diameter (in)"]] <- getAttribute(third_section, dist_od_names, "Target")
    output[["Distal Wall Thickness (in)"]] <- getAttribute(third_section, dist_wall_names, "Target")
    output[["Distal Out-of-Roundness (in)"]] <- getAttribute(third_section, dist_oor_names, "USL")
    output[["Distal Concentricity (in)"]] <- getAttribute(third_section, dist_concentricity_names, "USL")
    output[["Distal Perpendicularity (in)"]] <- getAttribute(third_section, dist_perpendicularity_names, "USL")
    
    #lengths
    output[["Total Length (in)"]] <- getAttribute(third_section, length_names, "Target")
    output[["Proximal Length (in)"]] <- getAttribute(third_section, prox_length_names, "Target")
    output[["Distal Length (in)"]] <- getAttribute(third_section, dist_length_names, "Target")
    output[["Transition Length (in)"]] <- getAttribute(third_section, transition_length_names, "Target")
    output[["Proximal and Transition Length (in)"]] <- getAttribute(third_section, proxtransition_length_names, "Target")
    
  }#end attributes
  
} #end multiExtrusionParameters

multiAndTaperedExtrusionParameters <- function(PPS, attribute_names){
  column_length <- ncol(PPS)
  row_length <- nrow(PPS)
  
  section_list <- splitPPS(PPS)
  first_section <- section_list$'first section'
  second_section <- section_list$'second section'
  third_section <- section_list$'third section'
  
  ## Temperature names ##
  feedthroat_names <- c("feed", "feedthroat", "circulator")
  barrel_names <- c("barrel", "zone")
  clamp_names <- c("clamp")
  adapter_names <- c("adapter")
  die_names <- c("die")
  
  
  #### Attribute names ####
  prox_od_names <- c("Proximal Finished OD", "Proximal OD", "Proximal outer diameter",
                     "Prox Finished OD", "Prox OD", "Prox outer diameter",
                     "Finished OD, proximal", "Proximal OD, proximal", "Proximal outer diameter, proximal",
                     "Finished OD, prox", "Proximal OD, prox", "Proximal outer diameter, prox",
                     "OD @ A")
  prox_id_names <- c("Proximal Finished ID", "Proximal ID", "Proximal inner diameter",
                     "Prox Finished ID", "Prox ID", "Prox inner diameter",
                     "Finished ID, proximal", "ID, proximal", "inner diameter, proximal",
                     "Finished ID, prox", "ID, prox", "inner diameter, prox",
                     "ID @ A")
  prox_wall_names <- c("Proximal average wall", "Proximal wall", "Proximal wall thickness",
                       "Prox average wall", "Prox wall", "Prox wall thickness",
                       "average wall, proximal", "wall, proximal", "wall thickness, proximal",
                       "average wall, prox", "wall, prox", "wall thickness, prox")
  prox_oor_names <- c("Proximal OOR", "Proximal out of roundness", "Proximal out-of-roundness", 
                      "Proximal out of round", "Proximal ovality", "Proximal Wall Uniformity", 
                      "Proximal roundness",
                      "Prox OOR", "Prox out of roundness", "Prox out-of-roundness", 
                      "Prox out of round", "Prox ovality", "Prox Wall Uniformity", 
                      "Prox roundness",
                      "OOR, proximal", "out of roundness, proximal", "out-of-roundness, proximal", 
                      "out of round, proximal", "ovality, proximal", "Wall Uniformity, proximal", 
                      "roundness, proximal",
                      "OOR, prox", "out of roundness, prox", "out-of-roundness, prox", 
                      "out of round, prox", "ovality, prox", "Wall Uniformity, prox", 
                      "roundness, prox")
  prox_concentricity_names <- c("Proximal concentricity",
                                "Prox concentricity",
                                "concentricity, proximal",
                                "concentricity, prox")
  prox_length_names <- c("Proximal length",
                         "Prox length",
                         "length, proximal",
                         "length, prox",
                         "A to B Distance")
  prox_perpendicularity_names <- c("Proximal perpendicularity",
                                   "Prox perpendicularity",
                                   "perpendicularity, proximal",
                                   "perpendicularity, prox")
  
  dist_od_names <- c("Distal Finished OD", "Distal OD", "Distal outer diameter",
                     "Dist Finished OD", "Dist OD", "Dist outer diameter",
                     "Finished OD, distal", "OD, distal", "outer diameter, distal",
                     "Finished OD, dist", "OD, dist", "outer diameter, dist",
                     "OD @ D")
  dist_id_names <- c("Distal Finished ID", "Distal ID", "Distal inner diameter",
                     "Dist Finished ID", "Dist ID", "Dist inner diameter",
                     "Finished ID, distal", "ID, distal", "inner diameter, distal",
                     "Finished ID, dist", "ID, dist", "inner diameter, dist",
                     "ID @ D")
  dist_wall_names <- c("Distal average wall", "Distal wall", "Distal wall thickness",
                       "Dist average wall", "Dist wall", "Dist wall thickness",
                       "average wall, distal", "wall, distal", "wall thickness, distal",
                       "average wall, dist", "wall, dist", "wall thickness, dist")
  dist_oor_names <- c("Distal OOR", "Distal out of roundness", "Distal out-of-roundness", 
                      "Distal out of round", "Distal vality", "Distal Wall Uniformity", 
                      "Distal roundness",
                      "Dist OOR", "Dist out of roundness", "Dist out-of-roundness", 
                      "Dist out of round", "Dist ovality", "Dist Wall Uniformity", 
                      "Dist roundness",
                      "OOR, distal", "out of roundnes, distals", "out-of-roundness, distal", 
                      "out of round, distal", "ovality, distal", "Wall Uniformity, distal", 
                      "roundness, distal",
                      "OOR, dist", "out of roundness, dist", "out-of-roundness, dist", 
                      "out of round, dist", "ovality, dist", "Wall Uniformity, dist", 
                      "roundness, dist")
  dist_concentricity_names <- c("Distal concentricity",
                                "Dist concentricity",
                                "concentricity, distal",
                                "concentricity, dist")
  dist_length_names <- c("Distal length",
                         "Dist length",
                         "length, distal",
                         "length, dist")
  dist_perpendicularity_names <- c("Distal perpendicularity",
                                   "Dist perpendicularity",
                                   "perpendicularity, distal",
                                   "perpendicularity, dist")
  
  proxtransition_length_names <- c("Proximal + Transition Length", "A to C Distance",
                                   "Cut \"Y\" max","Cut  \"Y\"  max")
  
  transition_length_names <- c("Transition Length", "Taper Length", "Length, Transition", 
                               "Length, Taper", "Cut \"Z\" ref","Cut  \"Z\"  ref")
  
  length_names <- c("length", "total length", "overall length")
  
  
  #### Naming the data ####
  output <- rep(list(-1000), length(attribute_names)) #will store the outputs into a list
  
  names(output) <- attribute_names
  
  
  output[["Extrusion Type"]] <- getExtrusionType(PPS)
  
  {#get FCA
    output[["Feedthroat Temperature (F)"]] <- getFCATemperature(second_section, feedthroat_names)
    output[["Clamp Temperature (F)"]] <- getFCATemperature(second_section, clamp_names)
    output[["Adapter Temperature (F)"]] <- getFCATemperature(second_section, adapter_names)  
  }#end get FCA
  {#get barrel and die temperatures
    barrel_temperatures <- getBarrelTemperatures(second_section, barrel_names)
    
    output[["Barrel Zone 1 Temperature (F)"]] <- barrel_temperatures[1]
    output[["Barrel Zone 2 Temperature (F)"]] <- barrel_temperatures[2]
    output[["Barrel Zone 3 Temperature (F)"]] <- barrel_temperatures[3]
    
    die_temperatures <- getDieTemperatures(second_section, die_names)
    
    output[["Die 1 Temperature (F)"]] <- die_temperatures[1]
    output[["Die 2 Temperature (F)"]] <- die_temperatures[2]
  }#end barrel and die temperatures
  {#get attributes
    #prox
    output[["Proximal Inner Diameter (in)"]] <- getAttribute(third_section, prox_id_names, "Target")
    output[["Proximal Outer Diameter (in)"]] <- getAttribute(third_section, prox_od_names, "Target")
    output[["Proximal Wall Thickness (in)"]] <- getAttribute(third_section, prox_wall_names, "Target")
    output[["Proximal Out-of-Roundness (in)"]] <- getAttribute(third_section, prox_oor_names, "USL")
    output[["Proximal Concentricity (in)"]] <- getAttribute(third_section, prox_concentricity_names, "USL")
    output[["Proximal Perpendicularity (in)"]] <- getAttribute(third_section, prox_perpendicularity_names, "USL")
    
    #dist
    output[["Distal Inner Diameter (in)"]] <- getAttribute(third_section, dist_id_names, "Target")
    output[["Distal Outer Diameter (in)"]] <- getAttribute(third_section, dist_od_names, "Target")
    output[["Distal Wall Thickness (in)"]] <- getAttribute(third_section, dist_wall_names, "Target")
    output[["Distal Out-of-Roundness (in)"]] <- getAttribute(third_section, dist_oor_names, "USL")
    output[["Distal Concentricity (in)"]] <- getAttribute(third_section, dist_concentricity_names, "USL")
    output[["Distal Perpendicularity (in)"]] <- getAttribute(third_section, dist_perpendicularity_names, "USL")
    
    #lengths
    output[["Total Length (in)"]] <- getAttribute(third_section, length_names, "Target")
    output[["Proximal Length (in)"]] <- getAttribute(third_section, prox_length_names, "Target")
    output[["Distal Length (in)"]] <- getAttribute(third_section, dist_length_names, "Target")
    output[["Transition Length (in)"]] <- getAttribute(third_section, transition_length_names, "Target")
    output[["Proximal and Transition Length (in)"]] <- getAttribute(third_section, proxtransition_length_names, "Target")
    
  }#end attributes
} #end multiAndTaperedExtrusionParameters

splitPPS <- function(PPS){
  #this function splits the PPS into three sections (four if there is an annealing section)
  #it then returns a list with each section
  
  section_list <- list()
  
  column_length <- ncol(PPS) #gets the number of columns
  row_length <- nrow(PPS) #gets the numbers of rows
  
  
  first_section_indice <- grep("Material & Tooling Requirements", PPS[,1], ignore.case = TRUE)
  
  if(length(first_section_indice) > 1){
    first_section_indice <- first_section_indice[1]
  }
  
  if (length(first_section_indice) == 0){
    # incase the first section is not labeled 'Material & Tooling Requirements' then I will check for 
    #the string 'PPS Barcode Verification' and then 'Equipment & Tooling Requirements'
    first_indice_count <- 0
    while (first_indice_count < column_length){
      #runs through the columns of PPS to look for 'PPS Barcode Verification'
      first_section_indice <- grep("PPS Barcode Verification", PPS[,first_indice_count], ignore.case = TRUE)
      
      if(length(first_section_indice) > 1){
        first_section_indice <- first_section_indice[1]
        break
      }
      
      if (length(first_section_indice) != 0){
        break;
      }# end if
      
      first_section_indice <- grep("Equipment & Tooling Requirements", PPS[,first_indice_count], 
                                   ignore.case = TRUE)
      
      if(length(first_section_indice) > 1){
        first_section_indice <- first_section_indice[1]
        break
      }
      
      if (length(first_section_indice) != 0){
        break;
      }# end if
      
      first_indice_count <- first_indice_count + 1
    } #end while
    
    if (length(first_section_indice) == 0){
      # if the while loop did not match a pattern to grep
      first_section_indice <- 1 #assign indice of 1
    }# end if
  }# end if
  
  second_section_indice <- grep("Processing Parameters", PPS[,1], ignore.case = TRUE)
  
  if(length(second_section_indice) > 1){
    second_section_indice <- second_section_indice[1]
  }
  else if (length(second_section_indice) == 0){
    #this will check Process Parameters after checking for Processing Parameters
    second_section_indice <- grep("Process Parameters", PPS[,1], ignore.case = TRUE)
    
    if(length(second_section_indice) > 1){
      second_section_indice <- second_section_indice[1]
    }
    
  }
  
  if (length(second_section_indice) == 0){
    # incase the section is not labeled 'Processing Parameters' then I will check for a subsection
    #of the section 'Extrusion Parameters' and then 'Water' for waterbath and assign the section 
    #the indice above
    second_indice_count <- 0
    while (second_indice_count < column_length){
      #runs through the columns of PPS to look for 'Extrusion Parameters' first and then 'Water'
      
      second_section_indice <- grep("Processing Parameters", PPS[,second_indice_count], ignore.case = TRUE)
      
      if(length(second_section_indice) > 1){
        second_section_indice <- second_section_indice[1]
        break
      }
      
      if (length(second_section_indice) != 0){
        break;
      }# end if
      
      
      second_section_indice <- grep("Extrusion Parameter", PPS[,second_indice_count], ignore.case = TRUE)
      
      if(length(second_section_indice) > 1){
        second_section_indice <- second_section_indice[1]
        break
      }
      
      if (length(second_section_indice) != 0){
        break;
      }# end if
      
      second_section_indice <- grep("Waterbath", PPS[,second_indice_count], ignore.case = TRUE)
      
      if(length(second_section_indice) > 1){
        second_section_indice <- second_section_indice[1] - 3
        break
      }
      
      if (length(second_section_indice) != 0){
        second_section_indice <- second_section_indice - 3
        break;
      }# end if
      
      second_indice_count <- second_indice_count + 1
    } #end while
    
    if (length(second_section_indice) == 0 || second_section_indice < 8){
      # if the while loop did not match a pattern to grep
      # or if the second_section_indice found a bad match and is less than 8
      second_section_indice <- 8 #assign indice of 8
    }# end if
  }# end if
  
  third_section_indice <- grep("Product Attributes & Testing", PPS[,1], ignore.case = TRUE)
  
  if(length(third_section_indice) > 1){
    third_section_indice <- third_section_indice[1]
  }
  
  if (length(third_section_indice) == 0){
    # incase the section is not labeled 'Product Testing & Attributes' then I will check for a 
    #subsection of the section 'Attribute' and assign the section the indice two above
    third_indice_count <- 0
    while (third_indice_count < column_length){
      #runs through the columns of PPS to look for matches
      
      third_section_indice <- grep("Product Attributes & Testing", PPS[,third_indice_count], 
                                   ignore.case = TRUE)
      
      if(length(third_section_indice) > 1){
        third_section_indice <- third_section_indice[1] - 1
        break;
      }
      
      if (length(third_section_indice) != 0){
        third_section_indice <- third_section_indice - 1
        break;
      }# end if
      
      third_section_indice <- grep("Attribute", PPS[,third_indice_count], ignore.case = TRUE)
      
      if(length(third_section_indice) > 1){
        third_section_indice <- third_section_indice[1] - 1
        break;
      }
      
      if (length(third_section_indice) != 0){
        third_section_indice <- third_section_indice - 1
        break;
      }# end if
      third_indice_count <- third_indice_count + 1
    } #end while
    
    if (length(third_section_indice) == 0){
      # if the while loop did not match a pattern to grep
      third_section_indice <- 20 #assign indice of 20
    }# end if
  }# end if
  
  #cut off the third section before we get to annealing parameters
  anneal_count <- second_section_indice #start from the second section
  before_anneal_indice <- 0
  while (anneal_count < row_length){
    placeholder <- grep("anneal", PPS[anneal_count,], ignore.case = TRUE)
    if (length(placeholder) != 0){
      before_anneal_indice <- anneal_count
      break
    }
    anneal_count <- anneal_count + 1
  }
  
  
  #Splits the PPS into these three sections that are present in each one.
  first_section <- PPS[first_section_indice:(second_section_indice - 1),]
  second_section <- PPS[second_section_indice:row_length,]
  #' the section section continue until row_length instead of third_section_indice becuase if the 
  #' indice is lower than 21, the second_section may be too short and not fully encompass all the
  #' temperature. Some of the temps may be in indice 21 or 22.
  if (before_anneal_indice != 0){
    third_section <- PPS[third_section_indice:before_anneal_indice-1,]
  }
  else{
    third_section <- PPS[third_section_indice:row_length,]
  }
  
  section_list$'first section' <- first_section
  section_list$'second section' <- second_section
  section_list$'third section' <- third_section
  
  return(section_list)
}#end splitPPS

getTooling <- function(data, names_to_match){
  #this function will parse the PPS to get the tooling sizes
  #it can get the barrel, die, die land length, tip/mandrel, and tip/mandrel land length from
  #the first section
  
  data_count <- 1 #will run through the while loop but it will also be the index of the column that
  #that contains the desired element since the while loop breaks
  
  row_index <- -1 #initialize the index of the row
  column_index <- -1 #initialize the index of the column
  names_length <- length(names_to_match)
  column_length <- ncol(data)
  
  while(data_count < column_length + 1){
    # this while loop runs through the second section and searches for the column that contains
    #either string in 'names_to_match'. If it finds the element, is saves the indices and then
    #breaks out of the while loop
    
    name_count <- 1 #runs through the vector of names_to_match
    
    while (name_count < names_length + 1){
      # this while loop checks if any of the names match anywhere in the column
      placeholder <- grep(names_to_match[name_count], data[,data_count], ignore.case = TRUE)
      
      if (length(placeholder) != 0){
        placeholder_count <- 1
        
        while (placeholder_count < length(placeholder) + 1){
          #goes through placeholder searching for a correct match for the tip
          
          
          if (nchar(data[placeholder[placeholder_count],data_count]) < 50){
            #nchar ensures a note was not selected
            row_index <- placeholder[placeholder_count]
            column_index <- data_count
            
            
            if (grepl(paste0("\\(", names_to_match[name_count], "\\)"), data[row_index, column_index], 
                      ignore.case = TRUE) == TRUE){
              #this makes sure that matches are not found for "(mandrel)", "(tip)", etc.
              row_index = -1
              column_index = -1
            }
            else if (grepl("die-cut", data[row_index, column_index], ignore.case = TRUE) == TRUE){
              #this make sure a match is not found when the code notices die-cut
              #has been found to occur for PPS 90057309 and 90057311
              row_index = -1
              column_index = -1
            }
            else if (grepl("core mandrel", data[row_index, column_index], ignore.case = TRUE) == TRUE ||
                     grepl("tip retainer", data[row_index, column_index], ignore.case = TRUE) == TRUE ||
                     grepl("die spacer", data[row_index, column_index], ignore.case = TRUE) == TRUE ||
                     grepl("slicking die", data[row_index, column_index], ignore.case = TRUE) == TRUE ||
                     grepl("sparker die", data[row_index, column_index], ignore.case = TRUE) == TRUE ||
                     grepl("distance die", data[row_index, column_index], ignore.case = TRUE) == TRUE
            ){
              #this makes sure a match is not found for the core mandrel. An example is PPS 90951412
              #this makes sure is a match not found for the tip retainer. An example is PPS 90971813
              #this makes sure is a match not found for the die spacer. An example is PPS 90971813
              #this makes sure is a match not found for the slicking die. An example is PPS 90971813
              #this makes sure is a match not found for the sparker die. An example is PPS 90971813
              #this makes sure a match is not found for the distance die. Examples are PPS documents
              # 2-TD0346-003, -004, -006, -012, -013, -015, -016, and -017
              row_index = -1
              column_index = -1
            }
            else if (row_index < 3){
              #this prevents a match occuring with the name of the part. Such as if the part name has
              #tip or mandrel in it.
              row_index = -1
              column_index = -1
            }
            else{
              #if the match does not meet any of the above conditions, it breaks out of the  
              #placeholder match while loop
              break
            }
          } #end if to check if the placeholder is a note
          
          
          placeholder_count <- placeholder_count + 1
        } #end while
        
        if (row_index != 1){
          # breaks out of the placeholder while loop to stop searching the other matches if
          # an approprioate match was already found
          break
        }# end if to break out of the name match while loop
        
      } # end if
      else if(length(placeholder) == 1){
        row_index <- placeholder
        column_index <- data_count
        
        if (grepl(paste0("\\(", names_to_match[name_count], "\\)"), data[row_index, column_index], 
                  ignore.case = TRUE) == TRUE){
          #this makes sure that matches are not found for "(mandrel)", "(tip)", etc.
          row_index = -1
          column_index = -1
        }
        else if (grepl("die-cut", data[row_index, column_index], ignore.case = TRUE) == TRUE){
          #this make sure a match is not found when the code notices die-cut
          #has been found to occur for PPS 90057309 and 90057311
          row_index = -1
          column_index = -1
        }
        else if (grepl("core mandrel", data[row_index, column_index], ignore.case = TRUE) == TRUE){
          #this makes sure is not found for the core mandrel
          row_index = -1
          column_index = -1
        }
        else if (row_index < 3){
          #this prevents a match occuring with the name of the part. Such as if the part name has
          #tip or mandrel in it.
          row_index = -1
          column_index = -1
        }
        else{
          #if the match does not meet any of the above conditions, it breaks out of the name match 
          #while loop
          break
        }
        
      } #end else if
      
      name_count <- name_count + 1 #update name_count
    } # end while
    
    if (row_index != -1){
      # break out of the outer while loop if a match was found
      break
    } # end if
    
    data_count <- data_count + 1 # update data_count
  } # end while
  
  
  if (row_index != -1){
    
    if (length(data[row_index, column_index + 1]) == 0){
      # makes sure the replacement length is not zero
      desired_data <- "Replacement Length Was Zero"
      
      if (nchar(data[row_index, column_index]) > 50){
        # ensure that a note is not picked up instead. A note will be longer than 50 characters
        desired_data <- "Replacement Length Was Zero and A note was read"
      }
    }
    else{
      desired_data <- data[row_index, column_index + 1]
    }
    
    if (nchar(data[row_index, column_index]) > 50){
      # ensure that a note is not picked up instead. A note will be longer than 50 characters
      desired_data <- "A note was read"
    }
    #the next two ensure that if there is a space between the spec name and value then it checks
    #the next columns
    
    
    desired_count <- 2
    while (desired_count < (ncol(data) + 1 - column_index)){
      
      #this split the desired data into its characters and removes spaces
      split_characters <- strsplit(as.character(desired_data), "")
      #as.character was added to prevent strsplitting on a non-character argument
      split_characters_nospace <- split_characters[[1]][split_characters[[1]] != ""]
      
      if (is.na(desired_data) || desired_data == ""){
        #sometimes there will be a space between the spec and the output
        desired_data <- data[row_index, column_index + desired_count]
      }
      else if (split_characters_nospace[1] == "9" && !grepl("e", desired_data, ignore.case = TRUE)){
        #if the first character of the index value is a 9, it is a print, and it will not pass
        #the value of the print
        #the grepl ensure that a match is not found for the scientific notation of a tooling size
        # for example "9.4E-2" should not be matched as a part number
        desired_data <- data[row_index, column_index + desired_count]
      }
      
      desired_count <- desired_count + 1
    }
    
    tooling_spec <- splitToolingString(desired_data)
    
    return (tooling_spec)
  }
  else{
    return ("Tooling was not found")
  }
  
} #end getTooling

getScrew <- function(data){
  #this function will the the print numbers of the screw
  #' it will find the column of the screw and the row, it will then find the column with 'p/n' or
  #' 'print number' and find the intersection between the two.
  
  column_count <- 1
  data_columns <- ncol(data) #number of columns in the data frame
  screw_row <- 0 #row where screw is found
  print_column <- 0 #column where the print is located
  
  while (column_count < data_columns + 1){
    #this searches through the columns of the data frames to find matches of the screw
    
    placeholder <- grep("screw", data[, column_count], ignore.case = TRUE)
    mismatch <- grep("screw speed", data[,column_count], ignore.case = TRUE) #ensure no mismatchess
    
    correct_match <- FALSE #this is the boolean condition if a proper match was found
    
    if (length(placeholder > 0) && length(mismatch) == 0){
      #if at least one match was found and no mismatch
      correct_match = TRUE
    }
    else if(length(placeholder > 0) && length(mismatch) > 0){
      #a mismatch was found
      screw_match <- match(placeholder, mismatch) #checks if there are any matches
      mismatch_boolean <- (screw_match[!is.na(screw_match)] != 0) #if there are matches,
      #there will be non-NA values and thus the length will not equal zero
      correct_match <- !mismatch_boolean
    }
    
    if (correct_match == TRUE){
      #a match was found with no mismatches, then the row index will be saved and the while loop
      #will be broken out of
      
      screw_row <- placeholder[1] #takes the first index because that will probably be 'screw' if there
      # are multiple matches
      column_count <- column_count + 1 #updates by one so the next while loop does not research this
      #column.
      break; #breaks out of the while loop
    }
    
    column_count <- column_count + 1
  } #end while of searching through columns for screw
  
  part_strings <- c("print", "p/n", "part")
  
  while (column_count < data_columns + 1){
    #this will now search for 'p/n' or 'print number' or 'part number'
    #' it seaches for it after finding screw, becuase this column name will have to come after
    #' that column that contains the word 'screw'
    
    part_string_count <- 1
    while (part_string_count < 4){
      search_string <- part_strings[part_string_count]
      placeholder <- grep(search_string, data[,column_count], ignore.case = TRUE)
      
      if (length(placeholder > 0)){
        #checks for a match
        if (placeholder[1] < screw_row){
          #the column name should be in an earlier row than the screw name. Prevents finding a match 
          #in the notes or other sections
          
          print_column <- column_count
          break;
          
        } #end if for row check
      } #end if for placeholder
      
      part_string_count <- part_string_count + 1
    } #end while for part_strings searching
    
    if (print_column == column_count){
      #print_column was updated to column_count because  match was found: break out of loop
      break;
    }
    
    column_count <- column_count + 1
  } #end while seacrhing for part number
  
  screw_print <- data[screw_row, print_column]
  
  if (length(screw_print) == 0 || is.na(screw_print)){
    #if the element is NA because the print is one or two columns over
    screw_print <- data[screw_row, print_column + 1]
  }
  if (length(screw_print) == 0 || is.na(screw_print)){
    #if the element is NA because the print is one or two columns over
    screw_print <- data[screw_row, print_column + 2]
  }
  if (length(screw_print) == 0 || is.na(screw_print) || nchar(screw_print) < 8){
    #is NA or does not have a print number because too few characters, returns that the print was
    #not found
    return ("Screw Print not Found")
  }
  
  return (screw_print) #if everything worked
  
}#end getScrew

getFCATemperature <- function(data, names_to_match){
  #Gets the temperature data of the feedthroat, clamp, or adapter
  
  data_count <- 1 #will run through the while loop but it will also be the index of the column that
  #that contains the desired element since the while loop breaks 
  #if true
  
  
  row_index <- -1 #initialize the index of the row
  column_index <- -1 #initialize the index of the column
  names_length <- length(names_to_match)
  column_length <- ncol(data)
  
  while(data_count < column_length + 1){
    # this while loop runs through the second section and searches for the column that contains
    #either string in 'names_to_match'. If it finds the element, is saves the indices and then
    #breaks out of the while loop
    
    name_count <- 1 #runs through the vector of names_to_match
    
    while (name_count < names_length + 1){
      # this while loop checks if any of the names match anywhere in the column
      # if it does, it assigns the index to that name in second column of df and assigns TRUE to 
      # the third column
      placeholder <- grep(names_to_match[name_count], data[,data_count], ignore.case = TRUE)
      
      if (length(placeholder) != 0 && nchar(data[placeholder[1],data_count]) < 50){
        #in case of multiple matches, take the first
        #nchar ensures a note was not selected
        row_index <- placeholder[1]
        column_index <- data_count
        break
      } # end if
      else if(length(placeholder) == 1){
        row_index <- placeholder
        column_index <- data_count
        break
      } #end else if
      
      name_count <- name_count + 1 #update name_count
    } # end while
    
    data_count <- data_count + 1 # update data_count
  } # end while
  
  
  if (row_index != -1){
    
    if (length(data[row_index, column_index + 1]) == 0){
      # makes sure the replacement length is not zero
      desired_data <- "Replacement Length Was Zero"
      
      if (nchar(data[row_index, column_index]) > 50){
        # ensure that a note is not picked up instead. A note will be longer than 50 characters
        temps[intersect_count] <- "Replacement Length Was Zero and A note was read"
      }
    }
    else{
      desired_data <- data[row_index, column_index + 1]
    }
    
    if (nchar(data[row_index, column_index]) > 50){
      # ensure that a note is not picked up instead. A note will be longer than 50 characters
      desired_data <- "A note was read"
    }
    #the next two ensure that if there is a space between the spec name and value then it checks
    #the next columns
    if (is.na(desired_data) || desired_data == ""){
      #sometimes there will be a space between the spec and the output
      desired_data <- data[row_index, column_index + 2]
    }
    if (is.na(desired_data) || desired_data == ""){
      #sometimes there will be a space between the spec and the output
      desired_data <- data[row_index, column_index + 3]
    }
    
    temperature <- splitTemperatureString(desired_data)
    
    return (temperature)
  }
  else{
    return ("Temperature was not found")
  }
  
} #end getFCATemperature

getBarrelTemperatures <- function(data, names_to_match){
  #Gets the temperature data of the barrel
  
  temps <- c(-999, -999, -999) # store the three barrel temperatures
  intersect_indices <- c(-999, -999, -999) # stores indices of intersect of the names
  #and corresponding numbers
  
  data_count <- 1 #will run through the while loop but it will also be the index of the column that
  #that contains the desired element since the while loop breaks if true
  
  temp_strings <- matrix(c("1", "2", "3", "one", "two", "three"), nrow = 3, ncol = 2)
  row_index <- -1 #initialize the index of the row
  column_index <- -1 #initialize the index of the column
  column_length <- ncol(data)
  names_length <- length(names_to_match)
  temps_filled <- FALSE #boolean check if temps vectors has been filled
  
  while(data_count < column_length + 1){
    # this while loop runs through the second section and searches for the column that contains
    #either string in names_to_match'. If it finds the element, is saves the indices and then
    #breaks out of the while loop
    
    name_count <- 1 #runs through the vector of names_to_match
    
    while (name_count < names_length + 1){
      # this while loop checks if any of the names match anywhere in the column
      placeholder <- grep(names_to_match[name_count], data[,data_count], ignore.case = TRUE)
      
      
      if (length(placeholder) != 0 && nchar(data[placeholder[1],data_count]) < 50){
        # have to check to make sure grep does not return integer(0)
        #the nchar ensures that a note was not picked up
        
        barrelnumber_count <- 1
        #two iterations to check for 1 and one, 2 and two, 3 and three
        while (barrelnumber_count < 2){
          #check if 1, 2, 3 appears in the columns
          barrel_one <- grep(temp_strings[1,barrelnumber_count], data[,data_count], ignore.case = TRUE)
          barrel_two <- grep(temp_strings[2,barrelnumber_count], data[,data_count], ignore.case = TRUE)
          barrel_three <- grep(temp_strings[3,barrelnumber_count], data[,data_count], ignore.case = TRUE)
          
          if (length(barrel_one) != 0 || length(barrel_two) != 0 || length(barrel_three) != 0){
            #maybe there a renot three zones.
            break # breaks out of the while loop if one is found
          } # end if
          barrelnumber_count <- barrelnumber_count + 1 # update counter
        } # end while
        
        
        #the the first element
        if (length(barrel_one) != 0){
          intersect_indices[1] <- intersect(placeholder, barrel_one)[1]
        } #end if
        if (length(barrel_two) != 0){
          intersect_indices[2] <- intersect(placeholder, barrel_two)[1]
        }#end if
        if (length(barrel_three) != 0){
          intersect_indices[3] <- intersect(placeholder, barrel_three)[1]
        }#end if
        
        break # breaks out of the while loop since a match was found
      } # end if
      name_count <- name_count + 1 #update name_count
    } # end while
    
    intersect_count <- 1
    while (intersect_count < 4){
      
      
      if (length(data[intersect_indices[intersect_count], data_count + 1]) == 0){
        # makes sure the replacement length is not zero. Prevents intersect indices form being NA
        temps[intersect_count] <- "Replacement Length Was Zero"
        if (!is.na(intersect_indices[intersect_count]) && 
            !is.na(data[intersect_indices[intersect_count], data_count]) &&
            nchar(data[intersect_indices[intersect_count], data_count]) > 50){
          # ensure that a note is not picked up instead. A note will be longer than 50 characters
          temps[intersect_count] <- "Replacement Length Was Zero and A note was read"
        }
      }
      else if (!is.na(intersect_indices[intersect_count]) && intersect_indices[intersect_count] != -999){
        #checks to see if the matrix was filled and breaks out of the while loop. Checks if intersect
        #had a match, essentially.
        
        temps[intersect_count] <- data[intersect_indices[intersect_count], data_count + 1]
        temps[intersect_count] <- splitTemperatureString(temps[intersect_count])
        
        if (!is.na(intersect_indices[intersect_count]) && 
            !is.na(data[intersect_indices[intersect_count], data_count]) &&
            nchar(data[intersect_indices[intersect_count], data_count]) > 50){
          # ensure that a note is not picked up instead. A note will be longer than 50 characters
          temps[intersect_count] <- "A note was read"
        }
        if (is.na(temps[intersect_count]) || temps[intersect_count] == ""){
          #sometimes there will be a space between the spec and the output
          temps[intersect_count] <- data[intersect_indices[intersect_count], data_count + 2]
          temps[intersect_count] <- splitTemperatureString(temps[intersect_count])
        }
        if (is.na(temps[intersect_count]) || temps[intersect_count] == ""){
          #sometimes there will be a space between the spec and the output
          temps[intersect_count] <- data[intersect_indices[intersect_count], data_count + 3]
          temps[intersect_count] <- splitTemperatureString(temps[intersect_count])
        }
        
        
        temps_filled = TRUE # a barrel temperature has been found
      } # end if
      else {
        temps[intersect_count] <- "Barrel temperature is NA or ''"
      }
      intersect_count <- intersect_count + 1
    } #end while
    
    if (temps_filled == TRUE){
      break
    } # end if
    
    data_count <- data_count + 1 # update data_count
  } # end while
  
  return(temps)
  
} #end getBarrelTemepratures

getDieTemperatures <- function(data, names_to_match){
  #Gets the temperature data of the barrel
  
  temps <- c(-999, -999) # store the two die temperatures
  intersect_indices <- c(-999, -999) # stores indices of intersect of the names
  #and corresponding numbers
  
  
  data_count <- 1 #will run through the while loop but it will also be the index of the column that
  #that contains the desired element since the while loop breaks if true
  
  temp_strings <- matrix(c("1", "2", "one", "two"), nrow = 2, ncol = 2)
  row_index <- -1 #initialize the index of the row
  column_index <- -1 #initialize the index of the column
  names_length <- length(names_to_match)
  column_length <- ncol(data)
  temps_filled <- FALSE
  
  while(data_count < column_length + 1){
    # this while loop runs through the second section and searches for the column that contains
    #either string in names_to_match'. If it finds the element, is saves the indices and then
    #breaks out of the while loop
    
    name_count <- 1 #runs through the vector of names_to_match
    
    while (name_count < names_length + 1){
      # this while loop checks if any of the names match anywhere in the column
      placeholder <- grep(names_to_match[name_count], data[,data_count], ignore.case = TRUE)
      
      if (length(placeholder) != 0 && nchar(data[placeholder[1],data_count]) < 50){
        # have to check to make sure grep does not return integer(0)
        #nchar ensures a note is not read
        
        dienumber_count <- 1
        #two iterations to check for 1 and one, 2 and two
        while (dienumber_count < 3){
          #check if 1, 2 appears in the columns
          die_one <- grep(temp_strings[1,dienumber_count], data[,data_count], ignore.case = TRUE)
          die_two <- grep(temp_strings[2,dienumber_count], data[,data_count], ignore.case = TRUE)
          if (length(die_one) != 0 || length(die_two) != 0){
            break
          } # end if
          dienumber_count <- dienumber_count + 1 # update counter
        } # end while
        
        
        if (length(intersect(placeholder, die_one)) == 0){
          #checks if there is a Die 1 in the PPS
          intersect_indices[1] <- "Die 1 Intersect in PPS not found"
        }
        else if ((length(intersect(placeholder, die_one)) > 1)){
          intersect_indices[1] <- intersect(placeholder, die_one)[1]
        }
        else{
          intersect_indices[1] <- intersect(placeholder, die_one)
        }
        
        if (length(intersect(placeholder, die_two)) == 0){
          #checks if there is a Die 2 in the PPS
          intersect_indices[2] <- "Die 2 Intersect in PPS not found"
        }
        else if ((length(intersect(placeholder, die_two)) > 1)){
          intersect_indices[2] <- intersect(placeholder, die_two)[1]
        }
        else{
          intersect_indices[2] <- intersect(placeholder, die_two)
        }
        
        intersect_count <- 1 # intialize intersect_count
        
        
        while (intersect_count < 3){
          
          if (length(data[intersect_indices[intersect_count], data_count + 1]) == 0){
            # makes sure the replacement length is not zero
            temps[intersect_count] <- "Replacement Length Was Zero"
            if (!is.na(intersect_indices[intersect_count]) && 
                !is.na(data[intersect_indices[intersect_count], data_count]) &&
                nchar(data[intersect_indices[intersect_count], data_count]) > 50){
              # ensure that a note is not picked up instead. A note will be longer than 50 characters
              temps[intersect_count] <- "Replacement Length Was Zero and A note was read"
            }
          }
          else if (!is.na(intersect_indices[intersect_count]) && intersect_indices[intersect_count] != -999){
            #checks to see if the matrix was filled and breaks out of the while loop. Checks if intersect
            #had a match, essentially.
            temps[intersect_count] <- data[intersect_indices[intersect_count], data_count + 1]
            temps[intersect_count] <- splitTemperatureString(temps[intersect_count])
            
            if (!is.na(intersect_indices[intersect_count]) && 
                !is.na(data[intersect_indices[intersect_count], data_count]) &&
                nchar(data[intersect_indices[intersect_count], data_count]) > 50){
              # ensure that a note is not picked up instead. A note will be longer than 50 characters
              temps[intersect_count] <- "A note was read"
            }
            if (is.na(temps[intersect_count]) == TRUE || temps[intersect_count] == ""){
              #sometimes there will be a space between the spec and the output
              temps[intersect_count] <- data[intersect_indices[intersect_count], data_count + 2]
              temps[intersect_count] <- splitTemperatureString(temps[intersect_count])
            }
            if (is.na(temps[intersect_count]) == TRUE || temps[intersect_count] == ""){
              #sometimes there will be a space between the spec and the output
              temps[intersect_count] <- data[intersect_indices[intersect_count], data_count + 3]
              temps[intersect_count] <- splitTemperatureString(temps[intersect_count])
            }
            
            temps_filled <- TRUE
          } # end if
          else{
            temps[intersect_count] <- "Die temp was NA or ''"
          } #end else
          
          intersect_count <- intersect_count + 1
        } # end while
        
        break # breaks out of the while loop since a match was found
        
      } # end if
      
      name_count <- name_count + 1 #update name_count
    } # end while
    
    if (temps_filled == TRUE){
      break
    }
    data_count <- data_count + 1 # update data_count
  } # end while
  
  return(temps)
  
} #end getDieTemperatures

getAttribute <- function(data, names_to_match, desired_tolerance){
  #Gets the attribute data of the OD, ID, wall thickness, OOR, concentricity, length, and 
  #perpendicularity
  
  desired_data <- c(-999, -999, -999, -999, -999)
  
  desired_value <- -999 #the value of the attribute that is wanted (either USL, UCL, Target, LCL, or LSL)
  
  data_count <- 1 #will run through the while loop but it will also be the index of the column that
  #that contains the desired element since the while loop breaks 
  #if true
  
  
  row_index <- -1 #initialize the index of the row
  column_index <- -1 #initialize the index of the column
  
  while(data_count < ncol(data) + 1){
    # this while loop runs through the second section and searches for the column that contains
    #either string in names_to_match'. If it finds the element, is saves the indices and then
    #breaks out of the while loop
    
    name_count <- 1 #runs through the vector of names_to_match
    
    while (name_count < length(names_to_match) + 1){
      # this while loop checks if any of the names match anywhere in the column
      # if it does, it assigns the index to that name in second column of df and assigns TRUE to 
      # the third column
      placeholder <- grep(paste0("^",names_to_match[name_count]), data[,data_count], ignore.case = TRUE)
      #the paste0 of '^' says that the string must start with the name. It prevents words like
      #'product' from appearing when we search 'OD'
      
      if (length(placeholder) > 1){
        placeholder <- placeholder[1]
      }
      else if (length(placeholder) == 0){
        #in case it did not match '^OD' because there is a space
        placeholder <- grep(paste0(" ",names_to_match[name_count]), data[,data_count], ignore.case = TRUE)
        if (length(placeholder) > 1){
          placeholder <- placeholder[1]
        }
        
        if (length(grep("wall uniformity", placeholder, ignore.case = TRUE))){
          #' sometimes when searching for 'wall thickness', 'wall uniformity' will be matched instead.
          #' This if statement is included to prevent it.
          placeholder <-integer(0)
        }
        
      }#end else if
      
      #Trying to prevent any matches to 'Scrap Proximal Length'
      scrap_match <- grep("scrap", data[,data_count], ignore.case = TRUE)
      scrap_intersect <- intersect(placeholder,scrap_match)
      if (length(scrap_intersect) != 0){
        placeholder <- setdiff(placeholder, scrap_intersect)
      }#end if
      
      #Trying to prevent people who mistype scrap
      scap_match <- grep("scap", data[,data_count], ignore.case = TRUE)
      scap_intersect <- intersect(placeholder,scap_match)
      if (length(scap_intersect) != 0){
        placeholder <- setdiff(placeholder, scap_intersect)
      }#end if
      
      
      # have to check to make sure grep does not return integer(0)
      if (length(placeholder) != 0){
        break # breaks out of the while loop since a match was found
      } # end if
      name_count <- name_count + 1 #update name_count
    } # end while
    
    
    if (name_count < length(names_to_match) + 1){
      #if the previous while loop went inside the if statement, then name_count must be less than 
      # names_to match as it would have never had the final update
      row_index <- placeholder #gets index from previous while loop
      column_index <- data_count
      
      break;
    } # end if
    
    data_count <- data_count + 1 # update data_count
  } # end while
  
  
  spec_count <- 1 #initializes a spec count
  
  spec_names <- c("LSL", "LCL", "Target", "UCL", "USL")
  spec_indices <- matrix (nrow = 5, ncol = 1) #stores incidices for LSL, LCL, Target, UCL, and USL
  spec_values <- matrix(nrow = 5, ncol = 1) # stores the values of the attributes
  
  while(spec_count < length(spec_names) + 1){
    # this while loop runs through the data and searches for the column that contains
    #either spec_names. If it finds the elements, is saves the indices and then
    #breaks out of the while loop. It runs through the names first, and then through the column
    
    spec <- spec_names[spec_count]
    attributename_index <- -998 # the column index at which the attribute name is found
    #data_count is not used instead so -998 can be passed if the attribute name was never found
    
    data_count <- 1 #reinitialize data
    
    while (data_count < ncol(data) + 1){
      # this while loop checks if the attribute is found in the matrix
      placeholder <- grep(paste0("^",spec), data[,data_count], ignore.case = TRUE)
      #paste0 makes sure the string starts with the name. Prevents substring finds
      
      if (length(placeholder) == 0){# if it did not match
        placeholder <- grep(spec, data[,data_count], ignore.case = TRUE)
        if (length(placeholder) != 0){
          possible_match <- gsub(" ", "", data[placeholder[1],data_count])
          possible_match_verify <- grepl(paste0("^",spec), possible_match, ignore.case = TRUE)
          if (possible_match_verify == FALSE){
            placeholder <- integer(0)
          }
        }
      } #end if
      
      
      # have to check to make sure grep does not return integer(0)
      if (length(placeholder) != 0){
        attributename_index <- data_count
        
        break # breaks out of the while loop since a match was found
      } # end if
      
      data_count <- data_count + 1 # update data_count
    } # end while
    
    spec_indices[spec_count,1] <- attributename_index
    
    spec_count <- spec_count + 1 #update name_count
  } # end while
  
  if (row_index != -1){
    #checks for non-negative spec indice
    
    if (length(spec_indices[spec_indices == -998]) > 0){
      #checks for any missing specs (assigned -998)
      replace_count <- 1
      while (replace_count < 6){
        #runs through each index of desired_data and spec_indices
        if (spec_indices[replace_count] != -998){
          #replace if it does not equal -998. If it does, the value for the desired_data remains -999
          desired_data[replace_count] <- data[row_index, spec_indices[replace_count]]
        }
        replace_count <- replace_count + 1
      }
    }
    else {
      desired_data <- data[row_index, spec_indices]
    }
    
    
    #cleans up the values
    no_ft <- gsub("ft", "", desired_data, ignore.case = TRUE)
    no_quotes <- gsub("\"", "", no_ft, ignore.case = TRUE)
    desired_data <- gsub(" ", "", no_quotes, ignore.case = TRUE)
    
    names(desired_data) <- spec_names
  } #end if
  
  #### The next lines of code decide which value to pass #####
  ####
  placeholder <- desired_data[desired_tolerance] #assigns placeholder to the value located in the
  #desired tolerance
  desired_value <- desired_data[desired_tolerance]
  placeholder_target <- (999)
  placeholder_UCL <- (999)
  placeholder_USL <- (999)
  change_count <- 0 #counts how many times it went into the if statements
  
  
  if (is.na(placeholder) ||
      grepl("n/a", placeholder, ignore.case = TRUE) || 
      placeholder < "0.00000000001" || #sometimes zero does not equal zero due to computer arithmetic
      #brackets prevent coercion
      placeholder == ""
  ){
    
    placeholder_target <- desired_data["Target"]
    desired_value <- placeholder_target
    change_count <- change_count + 1
  }#end if
  
  if (is.na(placeholder_target) ||
      grepl("n/a", placeholder_target, ignore.case = TRUE) || 
      placeholder_target < "0.00000000001" || #sometimes zero does not equal zero due to computer arithmetic
      #brackets prevent coercion
      placeholder_target == ""
  ){
    placeholder_UCL <- desired_data["UCL"]
    desired_value <- placeholder_UCL
    change_count <- change_count + 1
  }#end if
  
  if (is.na(placeholder_UCL) ||
      grepl("n/a", placeholder_UCL, ignore.case = TRUE) || 
      placeholder_UCL < "0.00000000001" || #sometimes zero does not equal zero due to computer arithmetic
      #brackets prevent coercion
      placeholder_UCL == ""
  ){
    placeholder_USL <- desired_data["USL"]
    desired_value <- placeholder_USL
    change_count <- change_count + 1
  }#end if
  
  if (change_count == 3 && 
      (is.na(placeholder_USL) ||
       grepl("n/a", placeholder_USL, ignore.case = TRUE) || 
       placeholder_USL < "0.00000000001" || #sometimes zero does not equal zero due to computer arithmetic
       placeholder_USL == "")
  ){
    #this if statements check if all three tolerances gave a value of zero or n/a. If every tolerance
    #had such a value, then the code will simple return the vlaue of the desired_tolerance.
    desired_value <- desired_data[desired_tolerance]
  }#end if
  
  #cleaning
  desired_value <- gsub("<", "", desired_value)
  desired_value <- gsub("\\(ref\\)", "", desired_value, ignore.case = TRUE)
  
  first_char <- strsplit(desired_value, "")[[1]][1]
  
  if (!is.na(first_char) && first_char == "."){
    #adds a leading zero if there is none in front of a decimal
    desired_value <- paste0("0",desired_value)
  } #end if
  
  
  
  if (!is.na(desired_value)){
    #checks to make sure the value is not NA
    #check if it is an integer
    value_list <- strsplit(desired_value, "")[[1]]
    first_char_numeric <- !is.na(as.numeric(value_list[1])) #checks if it is a number
    last_char_numeric <-!is.na(as.numeric(value_list[length(value_list)]))
    
    if (first_char_numeric == TRUE &&
        last_char_numeric == TRUE && 
        desired_value > 0){
      # if the value is zero or negative (-999), does not round
      desired_value <- as.double(desired_value)
      desired_value <- round(desired_value, digits = 4)
      desired_value <- format(desired_value, scientific = FALSE) #removes scientific notation
    }
  }
  
  return(desired_value)
  
} #end getAttribute

splitTemperatureString <- function(temperature_string){
  #gets the temperature from the number in the temperature
  
  if (length(temperature_string) == 0){
    return (temperature_string)
  }
  else if(is.na(temperature_string)){
    return (temperature_string)
  }
  else if(temperature_string == ""){
    return (temperature_string)
  }
  else if(length(grep("NA", temperature_string, ignore.case = TRUE)) > 0){
    return (temperature_string)
  }
  
  remove_target <- gsub(pattern = "target", replacement = "", temperature_string, ignore.case = TRUE)
  remove_equals <- gsub(pattern = "=", replacement = "", remove_target, ignore.case = TRUE)
  split_space <- strsplit(remove_equals, " ")
  no_blanks <- unlist(split_space)[!split_space[[1]] == ""]
  first_temp <- no_blanks[1]
  split_error <- strsplit(first_temp, "Â±")[[1]][1]
  split_degree <- strsplit(split_error, "Â°")[[1]][1]
  split_farenheit <- strsplit(split_degree, "F")[[1]][1]
  split_celcius <- strsplit(split_farenheit, "C")[[1]][1]
  split_broken <- strsplit(split_celcius, "Ã")[[1]][1]
  split_degree_second <- strsplit(split_broken, "Â°")[[1]][1]
  split_underscore <- strsplit(split_degree_second, "_")[[1]][1]
  split_minus <- strsplit(split_underscore, "-")[[1]][1]
  before_final <- gsub(pattern = "°", replacement = "", split_minus, ignore.case = TRUE)
  before_final2 <- gsub(pattern = "\\(ref\\)", replacement = "", before_final, ignore.case = TRUE)
  final <- gsub(pattern = "*", replacement = "", before_final2, ignore.case = TRUE)
  
  
  if (!is.na(final)){
    #checks to make sure the value is not NA
    #check if it is an integer
    temperature_list <- strsplit(final, "")[[1]]
    first_char_numeric <- !is.na(as.numeric(temperature_list[1])) #checks if it is a number
    last_char_numeric <-!is.na(as.numeric(temperature_list[length(temperature_list)]))
    
    if (first_char_numeric == TRUE &&
        last_char_numeric == TRUE && 
        final > 0){
      # if the value is zero or negative (-999), does not round
      final <- as.double(final)
      final <- round(final, digits = 0)
      final <- format(final, scientific = FALSE) #removes scientific notation
    }
  }
  
  return(final)
}#end splitTemperatureString

splitToolingString <- function(tooling_string){
  #splits string found from the tooling
  first_sub <- gsub("\"", "", tooling_string)
  second_sub <- gsub("\\(", "", first_sub)
  third_sub <- gsub("*", "", second_sub)
  fourth_sub <- gsub(",", "", third_sub)
  split_spaces <- strsplit(fourth_sub, " ")
  no_spaces <- split_spaces[[1]][split_spaces[[1]] != ""]
  first_nospaces <- no_spaces[1]
  
  no_by <- strsplit(first_nospaces, "x")[[1]][1]
  no_capsby <- strsplit(no_by, "X")[[1]][1]
  desired_value <- gsub("\"", "", no_capsby)
  
  first_char <- strsplit(desired_value, "")[[1]] #gets the first charcaters of the string
  
  if (!is.na(desired_value) && first_char == "."){
    #adds a leading zero if there is none in front of a decimal
    desired_value <- paste0("0",desired_value)
  } #end if
  
  
  
  if (!is.na(desired_value)){
    #checks to make sure the value is not NA
    #check if it is an integer. If it is, then it rounds the values.
    value_list <- strsplit(desired_value, "")[[1]]
    first_char_numeric <- !is.na(as.numeric(value_list[1])) #checks if it is a number
    last_char_numeric <-!is.na(as.numeric(value_list[length(value_list)]))
    
    if (first_char_numeric == TRUE &&
        last_char_numeric == TRUE && 
        desired_value > 0){
      # if the value is zero or negative (-999), does not round
      desired_value <- as.double(desired_value)
      desired_value <- round(desired_value, digits = 4)
      desired_value <- format(desired_value, scientific = FALSE) #removes scientific notation
    }
  }
  
  return(desired_value)
  
}#end splitToolingString

getExtrusionType <- function(data){
  #'this function determines whether the PPS is for an extruded component that is a single extrusion,
  #'multi-extrusion, or tapered extrusion. It cannot determine ILC extrusions.
  #'The code does this by parsing through the PPS data and searching for keywords. It returns "single"
  #'for single extrusion, "multi" for multi-extrusion, and "tapered" for tapered extrusion.
  
  ############### Multi-section  ###############
  
  matches <- 0 #reassign 0 to matches to check for multi-extrusion
  numberofrows <- nrow(data)
  row_count <- 1
  
  while (row_count < numberofrows + 1){
    #this while loops goes through each row in data to search for 'Extruder 1', 'Extruder 2',
    #'Extruder A', 'Extruder B', etc.
    
    
    ####### Regular Extruder Match ####### 
    
    extruder_match <- grep("Extruder", data[row_count,], ignore.case = TRUE)
    
    if (length(extruder_match) > 0){
      ##Checking for #1, #2, #3
      one_match <- grep("#1", data[row_count,], ignore.case = TRUE)
      two_match <- grep("#2", data[row_count,], ignore.case = TRUE)
      three_match <- grep("#3", data[row_count,], ignore.case = TRUE)
      
      one_intersect <- intersect(extruder_match, one_match)
      two_intersect <- intersect(extruder_match, two_match)
      three_intersect <- intersect(extruder_match, three_match)
      
      ##Dealing with overlaps (incase something like "extruder #1#2" is picked up)
      onetwo_intersect <- intersect(one_intersect, two_intersect)
      onethree_intersect <- intersect(one_intersect, three_intersect)
      twothree_intersect <- intersect(two_intersect, three_intersect)
      if (length(onetwo_intersect) != 0){
        two_intersect <- setdiff(two_intersect, onetwo_intersect)
      }
      if (length(onethree_intersect) != 0){
        three_intersect <- setdiff(three_intersect,onethree_intersect)
      }
      if (length(twothree_intersect) != 0){
        three_intersect <- setdiff(three_intersect,twothree_intersect)
      }
      
      
      first_total_matches <- length(one_intersect) + length(two_intersect) + length(three_intersect)
      
      if (first_total_matches == 2){
        return ("multi")
      }
      else if (first_total_matches == 3){
        return ("multi")
      }
      
      
      ##Checking for A, B
      A_match <- grep("A", data[row_count,], ignore.case = FALSE)
      B_match <- grep("B", data[row_count,], ignore.case = FALSE)
      
      A_intersect <- intersect(extruder_match, A_match)
      B_intersect <- intersect(extruder_match, B_match)
      
      ##Dealing with overlaps
      AB_intersect <- intersect(A_intersect, B_intersect)
      if (length(AB_intersect) != 0){
        B_intersect <- setdiff(B_intersect, AB_intersect)
      }
      
      second_total_matches <- length(A_intersect) + length(B_intersect)
      
      if (second_total_matches == 2){
        return("multi")
      }
      
      
      
      ##Checking for 1, 2, 3
      oneblank_match <- grep("1", data[row_count,], ignore.case = TRUE)
      twoblank_match <- grep("2", data[row_count,], ignore.case = TRUE)
      threeblank_match <- grep("3", data[row_count,], ignore.case = TRUE)
      
      oneblank_intersect <- intersect(extruder_match, oneblank_match)
      twoblank_intersect <- intersect(extruder_match, twoblank_match)
      threeblank_intersect <- intersect(extruder_match, threeblank_match)
      
      ##Dealing with overlaps (incase something like "extruder 12" is picked up)
      onetwoblank_intersect <- intersect(oneblank_intersect, twoblank_intersect)
      onethreeblank_intersect <- intersect(oneblank_intersect, threeblank_intersect)
      twothreeblank_intersect <- intersect(twoblank_intersect, threeblank_intersect)
      
      if (length(onetwoblank_intersect) != 0){
        twoblank_intersect <- setdiff(twoblank_intersect,onetwoblank_intersect)
      }
      if (length(onethreeblank_intersect) != 0){
        threeblank_intersect <- setdiff(threeblank_intersect,onethreeblank_intersect)
      }
      if (length(twothreeblank_intersect) != 0){
        threeblank_intersect <- setdiff(threeblank_intersect, twothreeblank_intersect)
      }
      
      third_total_matches <- length(oneblank_intersect) + length(twoblank_intersect) + 
        length(threeblank_intersect)
      
      if (third_total_matches == 2){
        return ("multi")
      }
      else if (third_total_matches == 3){
        return ("multi")
      }
      
    }
    
    row_count <- row_count + 1
    
    
    ####### Special Extruder Match #######
    
    extA_indices <- grep("ext A", data[row_count,], ignore.case = TRUE)
    extB_indices <- grep("ext B", data[row_count,], ignore.case = TRUE)
    
    extAandB_match <- length(extA_indices) + length(extB_indices)
    if (length(extA_indices) > 0 &&
        length(extB_indices) > 0 &&
        extAandB_match > 1
    ){
      return("multi")
    }#end if
    
    
    oneinch_indices <- grep("1\" DS", data[row_count,], ignore.case = TRUE)
    oneandhalfinch_indices <- grep("1.5\" DS", data[row_count,], ignore.case = TRUE)
    
    oneandoneandhalfinch_match <- length(oneinch_indices) + length(oneandhalfinch_indices)
    if (length(oneinch_indices) > 0 &&
        length(oneandhalfinch_indices) > 0 &&
        oneandoneandhalfinch_match > 1
    ){
      return("multi")
    }#end if
    
    
    alayer_indices <- grep("A \\(inner layer\\)", data[row_count,], ignore.case = TRUE)
    blayer_indices <- grep("B \\(outer layer\\)", data[row_count,], ignore.case = TRUE)
    
    aandblayer_match <- length(alayer_indices) + length(blayer_indices)
    if (length(alayer_indices) > 0 &&
        length(blayer_indices) > 0 &&
        aandblayer_match > 1
    ){
      return("multi")
    }#end if
    
    
  }#end while
  
  
  ############### Tapered Section ###############
  
  numberofcolumns <- ncol(data)
  column_count <- 1
  tapered_strings <- c("Prox", "Dist") #grep will match Prox to Proximal and Dist to Distal
  prox_matches <- 0
  dist_matches <- 0
  bumppuller_matches <- 0
  
  while (column_count < numberofcolumns + 1){
    #this while loop goes through each column in data and searches for matches for 'Prox' and 'Dist'
    prox_indices <- grep(tapered_strings[1], data[,column_count], ignore.case = TRUE)
    dist_indices <- grep(tapered_strings[2], data[,column_count], ignore.case = TRUE)
    bumppuller_indicies <- grep("Bump Puller", data[,column_count], ignore.case = TRUE)
    
    #checks if prox and dist matched because of the words approx (approximately) and distance
    approx_indices <- grep("approx", data[,column_count], ignore.case = TRUE)
    distance_indices <- grep("distance", data[,column_count], ignore.case = TRUE)
    
    #if there were false matches, it finds which indices were false
    prox_intersect <- intersect(prox_indices, approx_indices)
    dist_intersect <- intersect(dist_indices, distance_indices)
    
    #gets the value that did not have intersection with false positives
    if (length(prox_intersect) != 0){
      #if the length is zero, the code below will return a length zero even if prox_indices is filled
      actual_prox <- prox_indices[prox_indices != prox_intersect]
    }
    else{
      actual_prox <- prox_indices
    }
    if (length(dist_intersect) != 0){
      actual_dist <- dist_indices[dist_indices != dist_intersect]
    }
    else{
      actual_dist <- dist_indices
    }
    
    prox_matches <- prox_matches + length(actual_prox)
    dist_matches <- dist_matches + length(actual_dist)
    bumppuller_matches <- bumppuller_matches + length(bumppuller_indicies)
    
    column_count <- column_count + 1
  }#end while
  
  if (prox_matches > 0 &&
      dist_matches > 0 &&
      (prox_matches + dist_matches) > 2){
    #if there are at least three matches and one of each
    return("tapered")
  }
  if (bumppuller_matches > 0){
    return("tapered")
  }
  
  
  
  ############### Single Section ###############
  
  return("single") #returns single if non of the others were found
  
}#end getExtrusionType






