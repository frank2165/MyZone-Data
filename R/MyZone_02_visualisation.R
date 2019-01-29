
library(rlang)
library(ggplot2)
library(magrittr)


MyZone_histogram <- function(x, data, fill.col = "red", alpha = 0.6, bins = NULL){
    
    x <- enquo(x)
    
    if(is.null(bins)){
        bins <- nrow(data) %>% sqrt %>% ceiling %>% c(20) %>% min
    }
    
    ggplot(data, aes(x = !!x)) + 
        geom_histogram(fill = fill.col, colour = "white", alpha = alpha, bins = bins) + 
        theme_classic() + 
        labs(x = quo_text(x))
}






