library(tidyverse)
library(here)
library(openalexR)
library(glue)
library(rcrossref)
library(easyRPubMed)

# We want to build a process that allows to identify "impact citations" for diffeent sets of DOIs.
# With "impact citations" we mean systematic reviews, clinical practice guidelines and maybe others. 
# Am member of my team (e.g. Delwen, Merle) might take over tasks in this regard but we need support
# in the initial development of code for a) getting citing DOIs via Pubmed-API or Crossref-API and
# b) from all citing DOIs identifying those that are e.g. Cochrane Reviews (via Cochrane library API or Pubmed-Mesh terms.)
# Vladi knows better what a reasonable timeline is. He suggested 2-3 days. Once we have relevant 
# datasets based on this code other members fro mmy team and from partners in Basel will do manual checks to test precision etc.
# We believe that this process for analyses of "impact citations" can be useful for several projects
# including the Charité dashboard, etc. 


# Schritt 1: Wie aufwendig wäre es die DOIs der gefundenen Publikationen aus unserem IntoValue Datensatz (der als CSV vorliegt mit einer Spalte für DOI Nummern oder?) in einem separaten Code zu verwenden?
# Man müsste entweder diese DOIs in eine eigenständige CSV bringen ODER man schreibt ein skript,
# dass die DOIs automatisch aus dem Datensatz zieht, richtig?
# Schritt 2: Pubmed und CrossRef haben APIs. Wie aufwendig wäre es, ein Skript zu schreiben, welches für die DOIs aus Schritt 1 Zitationen (also zitierende DOIs) über diese APIs zu erfassen und zu dokumentieren (entweder in die Eingangs-CSV Datei oder in eine eigenständige CSV Datei)?
# Schritt 3: Cochrane Library hat auch eine API. Wie aufwendig wäre es, in einem dritten Schritt, die gefundenen zitierenden DOIs mit der Cochrane API darauf hin zu testen, welche der zitierenden DOIs Cochrane Reviews sind?

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


# Cochrange systematic Reviews have a doi schema:
cochrane_cd_regex <- "10.1002/14651858.cd" 

cites <- iv_dois[1:800] |> # takes ca. 5 hours!!!
  map(get_citing_dois_from_oa, .progress = TRUE)

cites_batch <- cites |> 
  list_rbind()


cites2 <- iv_dois[801:2500] |> # takes ca. 5 hours!!!
  map(get_citing_dois_from_oa, .progress = TRUE)

cites_batch2 <- cites2 |> 
  list_rbind()
test <- get_citing_dois_from_oa(iv_dois[801:1700][27])

cites_all <- cites_batch1 |> 
  bind_rows(cites_batch2)



# consider deversioning the dois or not?

cochrane_reviews <- cites |> 
  filter(str_detect(cited_by_doi, cochrane_cd_regex))


doi_817 <- openalexR::oa_fetch(doi = iv_dois[817], entity = "works", mailto = Sys.getenv("EMAIL"))
