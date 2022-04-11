# Load packages ----------------------------------------------------------------
source(list.files("./R", full.names = T))

ipak("tidyverse")


# Movebank ---------------------
ipak("move")

curl_login <- movebankLogin(username = "username",
                            password = "password")
