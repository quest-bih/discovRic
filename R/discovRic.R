library(tidyverse)
library(here)
library(openalexR)
library(glue)
library(rcrossref)
library(easyRPubMed)

if (!file.exists(here("data", "raw", "intovalue.csv"))) {
  # Function to download file to `dir` within "data" directory, if not already downloaded
  download_file <- function(file_url, file_name, dir = "raw"){
    dir_path <- fs::dir_create(here::here("data", dir))
    file_path <- fs::path(dir_path, file_name)
    if (!fs::file_exists(file_path)) {download.file(file_url, file_path)}
  }
  
  # Download intovalue
  download_file(
    "https://github.com/maia-sh/intovalue-data/blob/main/data/processed/trials.csv?raw=true",
    "intovalue.csv"
  )
} 

iv_dois <- read_csv(here("data", "raw", "intovalue.csv")) |> 
  filter(!is.na(doi)) |> 
  distinct(doi) |> 
  pull(doi)

dois <- sample(iv_dois, 200)

res <- oa_fetch(doi = iv_dois, entity = "works", mailto = Sys.getenv("EMAIL"))



oaids <- res$id

# unclear what upper bound on identifiers is (or dois). Works supposedly faster with oaids
# so step 1 convert doi to oaid, step two chunk oaids to right size, step 3 map function over all oaids to get citing oaids
# step 4 fetch dois from oaids



sb_res <- oa_snowball(identifier = oaids[1:100], id_type = "original", cited_by_params = list(from_publication_date = "2025-01-01"),
                    mailto = Sys.getenv("EMAIL"))


sb_df <- snowball2df(sb_res)

cited_id <- "10.1016/j.bbmt.2011.12.041"

get_citing_dois_from_oa <- function(cited_id, sleep = 1, mailto = Sys.getenv("EMAIL")) {
  
  if (stringr::str_detect(cited_id, "^10\\.")) {
    oaid <- openalexR::oa_fetch(doi = cited_id, entity = "works", mailto = mailto)
    if (rlang::is_null(oaid)) {
      warning(glue::glue("No record for id {cited_id} found on Open Alex!"))
      return(NULL)
    }
    cited_doi <- cited_id
    oaid <- oaid |> 
      dplyr::pull(id)
    
  } else {
    cited_doi <- NA
    oaid <- cited_id
  }
  Sys.sleep(stats::rgamma(1, sleep))
  
  records <- openalexR::oa_fetch(cites = oaid, entity = "works",
           mailto = mailto) 
  
  if (rlang::is_null(records)) {
   return(tibble::tibble(cited_by_doi = "no citations found", cited_by = "no citations found",
                 cited_work = oaid, cited_work_doi = cited_doi))
  } else {
    return(records |> 
             dplyr::select(cited_by_doi = doi, cited_by = id) |> 
             dplyr::mutate(cited_work = oaid,
                           cited_work_doi = cited_doi))
  }
}


iv_cites <- iv_dois |> # takes ca. 4-5 hours!!!
  map(get_citing_dois_from_oa, .progress = TRUE)


iv_citations <- iv_cites |> 
  list_rbind()

iv_citations |> 
  saveRDS(here("data", "processed", "citations.rds"))

iv_citations <- readRDS(here("data", "processed", "citations.rds"))


# Cochrange systematic Reviews have a doi schema:
cochrane_cd_regex <- "10\\.1002\\/14651858\\.cd.*pub" 

# consider deversioning the dois or not?

cochrane_reviews <- iv_citations |> 
  filter(str_detect(cited_by_doi, cochrane_cd_regex))

has_cochrane <- iv_citations |> 
  group_by(cited_work_doi) |> 
  summarise(has_cochrane_review = any(str_detect(cited_by_doi, cochrane_cd_regex), na.rm =  TRUE))

has_cochrane |> 
  count(has_cochrane_review)

