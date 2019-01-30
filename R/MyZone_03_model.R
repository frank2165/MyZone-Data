calculate_meps <- function(duration, type, model){
    meps <- sum(fixef(model) * c(1, duration)) + ranef(model)$Type[type, ]
    round(meps)
}
