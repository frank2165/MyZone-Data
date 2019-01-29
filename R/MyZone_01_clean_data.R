
get_session <- function(csv){
    
    ################
    # Segmentation #
    ################
    
    ## Header
    header <- csv[2, ]
    names(header) <- csv[1, ]
    
    descript.has.comma <- !any(is.na(header))
    if(descript.has.comma){
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
    
    return(list(workouts = meta, heat.rate = hr))
}




workout_variables <- function(workouts){
    
    # Create dummy variables based on description
    pt    <- grepl("Jesse", workouts$description, ignore.case = TRUE)
    blitz <- grepl("Blitz", workouts$description, ignore.case = TRUE)
    walk  <- grepl("Walk", workouts$description, ignore.case = TRUE)
    gym   <- grepl("(Gym|Workout|Cardio)", workouts$description, ignore.case = TRUE)
    
    mutate(workouts, PT = pt, Blitz = blitz, Walk = walk, Gym = gym)
}



