#' fetch publications for dashboard
# 1. get ugoe org data
library(tidyverse)
library(ropenaire)
h2020_org <- readr::read_delim("data/cordis-h2020organizations.csv", delim = ";",
                               locale = locale(decimal_mark = ",")) %>%
  mutate_if(is.integer, as.character) %>%
  filter(id == "999845640")
# 2. get project info
h2020_projects <- readr::read_delim("data/cordis-h2020projects.csv", delim = ";",
                                    locale = locale(decimal_mark = ",")) %>%
  mutate_if(is.integer, as.character) %>%
  inner_join(h2020_org, by = c("id" = "projectID")) %>%
  mutate(start_year = lubridate::year(startDate)) %>%
  separate(fundingScheme, "funding_scheme") -> ugoe_all
#' basic metadata
#' no fair info in tsv 
#' 
library(xml2)
pub_fetch_oaire <- function(project_id) {
  my_pubs <- ropenaire::roa_pubs(project_id = project_id, format = "xml")
  doc  <- xml_find_all(my_pubs, "//results//result//metadata")
  id <-  xml_find_all(my_pubs, "//results//result//header//dri:objIdentifier") %>%
  xml_text()
doc  <- xml_find_all(my_pubs, "//results//result//metadata")
pubs <- map_df(doc, function(x) {
  #id <- xml_find_first(x, "./header/dri:objIdentifier") %>% 
  #  xml_text()
  title <- xml_find_first(x, "./oaf:entity//oaf:result/title") %>% 
    xml_text()
  embargoenddate <-  xml_find_first(x, "./oaf:entity//oaf:result/embargoenddate") %>% 
    xml_text()
  dateofacceptance <-  xml_find_first(x, "./oaf:entity//oaf:result/dateofacceptance") %>% 
    xml_text()
  originalId <- list(xml_find_all(x, "./oaf:entity//oaf:result/originalId") %>%
    xml_text())
  collectedfrom <- list(xml_find_all(x, "./oaf:entity//oaf:result/collectedfrom") %>%
    xml_attr("name"))
  pid_source <-  list(xml_find_all(x, "./oaf:entity//oaf:result/pid") %>%
    xml_attr("classid"))
  pid <- list(xml_find_all(x, "./oaf:entity//oaf:result/pid") %>%
    xml_text())
  access <- xml_find_first(x, "./oaf:entity//oaf:result/bestaccessright") %>%
    xml_attr("classname")
  data_frame(title, embargoenddate, dateofacceptance, originalId, collectedfrom, pid, pid_source, access)
})
pubs %>% 
  mutate(openaire_id = id) %>%
  mutate(openaire_project_id = project_id)
}
pubs_ugoe <- purrr::map(ugoe_all$id, .f = purrr::safely(pub_fetch_oaire))
purrr::map_df(pubs_ugoe, "result") -> pubs_ugoe_df
jsonlite::stream_out(pubs_ugoe_df, file("data/pubs_ugoe.json"))
