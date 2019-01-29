
library(dplyr)
library(magrittr)

get_session <- function(csv){
    
    ################
    # Segmentation #
    ################
    
    ## Header
    header <- csv[2, ]
    names(header) <- csv[1, ]
    
    descript.has.comma <- !any(is.na(header))
    if(descript.has.comma){
        idx.desc <- which(names(header) == "MEPs") - 1
        description <- paste0(header[3:idx.desc], collapse = ", ")
        header[3] <- description
        header    <- header[-seq(4, idx.desc, 1)]
        
        df.names <- names(header)[-length(header)]
        header <- header[-4]
        names(header) <- df.names
        names(header)[3] <- "description"
    } else {
        names(header)[3] <- "description"
        header <- header[-c(4, 26)]
    }
    
    idx.numeric <- suppressWarnings(vapply(header, as.numeric, numeric(1))) %>% 
        is.na %>% not %>% which
    
    header[idx.numeric] <- lapply(header[idx.numeric], as.numeric)
    
    
    ## Heart Rate
    idx.hr.rows <- 3 + seq(1, header$Duration, 1)
    hr <- csv[idx.hr.rows, c(1, 2)]
    names(hr) <- c("Time", "hr")
    
    
    
    ############
    # Clean-up #
    ############
    
    # Convert types
    hr$hr   <- as.numeric(hr$hr)
    hr$Time <- as.POSIXlt(hr$Time)
    
    
    # Convert Start and End times to 24 hour format
    start <- min(hr$Time)
    end   <- max(hr$Time)
    header$Start <- paste(start$hour, start$min, sep = ":")
    header$End   <- paste(end$hour, end$min, sep = ":")
    
    header$Start <- formatC(header$Start, width = 5) %>% gsub("^\\s", "0", .)
    header$End   <- formatC(header$End, width = 5)   %>% gsub("^\\s", "0", .)
    
    
    # Convert average effort from a character string, showing percentage,
    # to a decimal fraction.
    header[["Average Effort"]] <- header[["Average Effort"]] %>% 
        gsub("\\%", "", .) %>% 
        as.numeric %>% 
        divide_by(100)
    
    
    return(list(header = header, hr = hr))
}



MyZone_data_import <- function(file){
    
    # Read data for all sessions into R
    csv <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)
    
    
    # Identify the first and last rows of each session
    idx.start <- which(csv[[1]] == "Start")
    idx.end   <- c(idx.start[-1] - 1, nrow(csv))
    
    
    # Segment spreadsheet into data for each session
    sessionList <- Map(function(i, j) get_session(csv[i:j, ]), idx.start, idx.end)
    
    
    # Combine summary statistics for all workouts
    meta <- lapply(sessionList, function(x) x$header) %>% 
        do.call("rbind", .)
    
    
    # Combine heart-rate records for all workouts
    hr <- lapply(sessionList, function(x) x$hr) %>% 
        do.call("rbind", .)
    
    
    # Some workouts were split due to discrepancies with the time-
    # stamps, which are applied by whichever device the MyZone is
    # paired with.
    meta <- group_by(meta, Date) %>% 
        summarise(
            Start = min(Start),
            End = max(End), 
            description = paste(description, collapse = " "), 
            `Average Effort` = sum(`Average Effort` * Duration) / sum(Duration),
            `Average Heart Rate` = sum(`Average Heart Rate` * Duration) / sum(Duration),
            MEPs = sum(MEPs), 
            Duration = sum(Duration), 
            `Time in Zone` = sum(`Time in Zone`), 
            Calories = sum(Calories), 
            `Peak Heart Rate` = max(`Peak Heart Rate`), 
            `Zone 0 Mins` = sum(`Zone 0 Mins`),
            `Zone 1 Mins` = sum(`Zone 1 Mins`),
            `Zone 2 Mins` = sum(`Zone 2 Mins`),
            `Zone 3 Mins` = sum(`Zone 3 Mins`),
            `Zone 4 Mins` = sum(`Zone 4 Mins`),
            `Zone 5 Mins` = sum(`Zone 5 Mins`),
            `Zone 0 MEPs` = sum(`Zone 0 MEPs`),
            `Zone 1 MEPs` = sum(`Zone 1 MEPs`),
            `Zone 2 MEPs` = sum(`Zone 2 MEPs`),
            `Zone 3 MEPs` = sum(`Zone 3 MEPs`),
            `Zone 4 MEPs` = sum(`Zone 4 MEPs`),
            `Zone 5 MEPs` = sum(`Zone 5 MEPs`)
        ) %>% ungroup
    
    
    return(list(workouts = meta, heat.rate = hr))
}




workout_variables <- function(workouts){
    
    # Create dummy variables based on description
    pt    <- grepl("Jesse", workouts$description, ignore.case = TRUE) 
    blitz <- grepl("Blitz", workouts$description, ignore.case = TRUE) 
    walk  <- grepl("Walk", workouts$description, ignore.case = TRUE) 
    self  <- grepl("(Gym|Workout|Cardio)", workouts$description, ignore.case = TRUE) 
    
    mutate(workouts, PT = pt, Blitz = blitz, Walk = walk, Self = self)
}



