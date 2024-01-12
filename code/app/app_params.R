#DDH PARAMS-----
# Boolean to set whether private (ddh.com) or public (ddh.org)
privateMode <- Sys.getenv("DDH_PRIVATE", "N") == "Y" 

# Boolean to set whether test data or all data
testMode <- FALSE

##############################################################################################
## DO NOT EDIT MANUALLY! This path is automatically generated based on "testMode" parameter ##
##############################################################################################
app_data_dir <- ifelse(testMode, here::here("tests", "data"), here::here("data"))
