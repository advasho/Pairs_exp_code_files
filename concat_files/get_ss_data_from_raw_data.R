library(stringr)

get_ss_data_from_raw_data = function(raw_data, key) {
    regex_pattern = paste0(key, "\\s+.")
    regex_pattern = paste0("(?<=", key, ").*?(?=[,])")
    #(?<=gender).*?(?=[,])
    
    # Clean up the raw data
    raw_data = str_replace_all(raw_data, "[^A-Za-z0-9א-ת,]", " ")
    # Add a comma at the end for easier extraction
    raw_data = paste0(raw_data, ",")
    value = str_extract(raw_data, pattern = regex_pattern)
    value = str_trim(value)
    
    return(value)
}