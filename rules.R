### FM VALIDATION RULES (based on https://cran.r-project.org/web/packages/validate/vignettes/cookbook.html)

## Variable checks

# Missingness

!is.na(geographic_area)

!is.na(OC2)

!is.na(OC3)

!is.na(working_time)

!is.na(sex)

!is.na(year)

# Field length

field_length(year, n = 4)

field_length(flag, min = 0, max = 1) | is.na(flag)

# General field format (make sure employment numbers are integers)

grepl("^\\d+$", value) | is.na(value)

# Numeric ranges

value >= 0  | is.na(value)

in_range(year, min = 1950, max = format(Sys.Date(), "%Y"))

# Acceptable values

geographic_area %in% country_codes$Name_En

OC3 %in% c( "Aquaculture",
            "Inland Waters Fishing",
            "Marine Fishing, nei",
            "Marine Deep-Sea Fishing",
            "Processing",
            "Subsistence",
            "Marine Coastal Fishing",
            "Unspecified")

working_time %in% c("Full time",
                    "Occasional",
                    "Part time",
                    "Status Unspecified")

sex %in% c( "M",
            "F",
            "U")

flag %in% c("B",
            "E",
            "F",
            "I",
            "O",
            "M",
            "Q",
            "N",
            "P",
            "R",
            "S",
            "T",
            "X",
            NA)

## Availability and uniqueness

# Uniqueness

is_unique(geographic_area, OC3, working_time, sex, year, value, flag) # uniqueness
is_unique(geographic_area, OC3, working_time, sex, year) # partial uniqueness

## Multivariate checks

# Completeness of records

is_complete(geographic_area, OC3, working_time, sex, year)

# Conditional restrictions (mostly based on https://unstats.un.org/unsd/methodology/m49/#changes, Wikipedia)

if(is.na(value)) flag %in% c("O", "M", "Q")
if(!is.na(value)) flag %in% c(NA, "B", "E", "F", "I", "N", "P", "R", "S", "T", "X")

if(landlocked == TRUE) !(OC3 %in% c("Marine Fishing, nei", 
                                     "Marine Deep-Sea Fishing", 
                                     "Marine Coastal Fishing"))

year >= Start_Year & year <= End_Year

# if(flag == "E") method_flag %in% c("c", "f", "h", "i", "s", "t")
# if(flag == "I") method_flag %in% c("b", "c", "e", "i", "s", "t")
# if(flag == "M") method_flag %in% c("c", "i", "q", "p")
# if(flag == "X") method_flag %in% c("c", "h", "i", "p", "q", "s")
# if(flag == "O") method_flag %in% c("c", "i", "q", "s", "u")
# if(flag == "Q") method_flag %in% c("c", "h", "p")
# if(flag == "T") method_flag %in% c("c", "h", "i", "p", "s")
# if(is.na(flag)) method_flag %in% c("c", "h", "i", "p", "q", "s")
