library(tidyverse)
library(here)
library(openalexR)
library(glue)
library(rcrossref)
library(easyRPubMed)
library(rentrez)
library(ggVennDiagram)
library(janitor)

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


iv_data <- read_csv(here("data", "raw", "intovalue.csv"))

iv_ids <- iv_data |>
  filter(!is.na(doi) | !is.na(pmid)) |> 
  distinct(doi, pmid) |> 
  select(cited_work_doi = doi, cited_work = pmid)

iv_dois <- iv_data |> 
  filter(!is.na(doi)) |> 
  distinct(doi) |> 
  pull(doi)

iv_pmids <- iv_data |> 
  filter(!is.na(pmid)) |> 
  distinct(pmid) |> 
  pull(pmid)

# dois <- sample(iv_dois, 200)

res <- oa_fetch(doi = iv_dois, entity = "works", mailto = Sys.getenv("EMAIL"))

oaids <- res$id

res2 <- oa_fetch(pmid = only_in_pm_no_doi$cited_work, entity = "works", mailto = Sys.getenv("EMAIL"))

oaids2 <- res2$id

sb_res <- oa_snowball(identifier = oaids2, id_type = "original", cited_by_params = list(from_publication_date = "2025-01-01"),
                                          mailto = Sys.getenv("EMAIL"))

sb_df <- snowball2df(sb_res)
# potential alternative pipeline
# unclear what upper bound on identifiers is (or dois). Works supposedly faster with oaids
# so step 1 convert doi to oaid, step two chunk oaids to right size, step 3 map function over all oaids to get citing oaids
# step 4 fetch dois from oaids

# sb_res <- oa_snowball(identifier = oaids[1:100], id_type = "original", cited_by_params = list(from_publication_date = "2025-01-01"),
#                     mailto = Sys.getenv("EMAIL"))
# 
# 
# sb_df <- snowball2df(sb_res)

# cited_id <- "10.1016/j.bbmt.2011.12.041"
cited_id <- iv_pmids_only[1]
get_citing_dois_from_oa <- function(cited_id, sleep = 1, mailto = Sys.getenv("EMAIL")) {
  
  if (stringr::str_detect(cited_id, "^10\\.")) {
    oaid <- openalexR::oa_fetch(doi = cited_id, entity = "works", mailto = mailto)
    if (rlang::is_null(oaid)) {
      warning(glue::glue("No record for id {cited_id} found on Open Alex!"))
      return(NULL)
    }
    cited_doi <- cited_id
    cited_pmid <- NA
    oaid <- oaid |> 
      dplyr::pull(id)
    
  } else if (stringr::str_detect(cited_id, "\\d{8,}")) {
    oaid <- openalexR::oa_fetch(pmid = cited_id, entity = "works", mailto = mailto)
    if (rlang::is_null(oaid)) {
      warning(glue::glue("No record for id {cited_id} found on Open Alex!"))
      return(NULL)
    }
    cited_doi <- NA
    cited_pmid <- cited_id
    oaid <- oaid |> 
      dplyr::pull(id)
  } else {
    cited_doi <- NA
    cited_pmid <- NA
    oaid <- cited_id
  }
  Sys.sleep(stats::rgamma(1, sleep))
  
  records <- openalexR::oa_fetch(cites = oaid, entity = "works",
           mailto = mailto) 
  
  if (rlang::is_null(records)) {
   return(tibble::tibble(cited_by_doi = "no citations found", cited_by = "no citations found",
                 cited_work = oaid, cited_work_doi = cited_doi, cited_work_pmid = cited_pmid))
  } else {
    return(records |> 
             dplyr::select(cited_by_doi = doi, cited_by = id) |> 
             dplyr::mutate(cited_work = oaid,
                           cited_work_doi = cited_doi,
                           cited_work_pmid = cited_pmid))
  }
}


iv_cites <- iv_dois |> # takes ca. 4-5 hours!!!
  map(get_citing_dois_from_oa, .progress = TRUE)

iv_pmids_only <- iv_data |> 
  filter(has_pubmed == TRUE, has_doi == FALSE) |> 
  pull(pmid)

iv_cites_pmids <- iv_pmids_only |> 
  map(get_citing_dois_from_oa, .progress = TRUE)

iv_citations <- iv_cites |> 
  list_rbind()

iv_citations_pmids <- iv_cites_pmids |> 
  list_rbind()

iv_citations |> 
  saveRDS(here("data", "processed", "citations.rds"))

iv_citations <- readRDS(here("data", "processed", "citations.rds"))

iv_citations <- iv_citations |> 
  mutate(cited_work_pmid = NA) |> 
  bind_rows(iv_citations_pmids)

# Cochrange systematic Reviews have a doi schema:
# cochrane_cd_regex <- "10\\.1002\\/14651858\\.cd.*pub" 
cochrane_cd_regex <- "10\\.1002\\/14651858\\.cd."
# consider deversioning the dois or not?

cochrane_reviews <- iv_citations |> 
  filter(str_detect(cited_by_doi, cochrane_cd_regex))

has_cochrane <- iv_citations |> 
  group_by(cited_work_doi) |> 
  summarise(has_cochrane_review = any(str_detect(cited_by_doi, cochrane_cd_regex), na.rm =  TRUE))

has_cochrane |> 
  count(has_cochrane_review)



dois <- iv_dois[1:50]

# cited_doi <- "10.3389/fnagi.2015.00152"

# Find citing articles
citing_articles <- entrez_link(dbfrom="pubmed", id=pmid, db="pubmed")
# Get the PubMed IDs of citing articles
citing_pmids <- citing_articles$links$pubmed_pubmed_citedin


dois_citing <- get_metadata(tibble(pmid = citing_pmids), pmid, api_key = Sys.getenv("NCBI_KEY"))

#from pmids much better!

summaries <- entrez_summary(db="pubmed", id=citing_pmids)


# usethis::edit_r_environ()


# cited_id <- "10.3389/fnagi.2015.00152"


get_citing_pmids_from_pubmed <- function(cited_id, sleep = 1, api_key = Sys.getenv("NCBI_KEY")) {
  
  if (stringr::str_detect(cited_id, "^10\\.")) {
    
    pmid <- id_to_meta(cited_id, api_key = Sys.getenv("NCBI_KEY")) |> 
      pull(pmid)
    # oaid <- openalexR::oa_fetch(doi = cited_id, entity = "works", mailto = mailto)
    if (rlang::is_null(pmid)) {
      warning(glue::glue("No record for id {cited_id} found on PubMed!"))
      return(NULL)
    }
    cited_doi <- cited_id
    
  } else {
    cited_doi <- NA
    pmid <- cited_id
  }
  Sys.sleep(stats::rgamma(1, sleep))
  
  records <- rentrez::entrez_link(dbfrom="pubmed", id=pmid, db="pubmed")
  
  if (rlang::is_null(records)) {
    return(tibble::tibble(cited_by_doi = "no citations found", cited_by = "no citations found",
                          cited_work = pmid, cited_work_doi = cited_doi))
  } else {
    return(
      # Get the PubMed IDs of citing articles
      
      tibble::tibble(cited_by = records$links$pubmed_pubmed_citedin,
                     cited_work = pmid, cited_work_doi = cited_doi)
    )
  }
}



iv_cites_pm <- iv_pmids[1:50] |> # takes ca. 1 hour
  map(get_citing_pmids_from_pubmed, .progress = TRUE)

iv_citations_pm <- iv_cites_pm |> 
  list_rbind()

write_excel_csv2(iv_citations_pm, here("data", "pmid_citations_no_doi.csv"))

iv_citations_pm <- read_csv2(here("data", "pmid_citations_no_doi.csv"))

iv_citations_pm_deduped <- iv_citations_pm |> 
  distinct(cited_by)


iv_cites_pm_dois <- get_metadata(iv_citations_pm_deduped, cited_by, api_key = Sys.getenv("NCBI_KEY"))

iv_citations_pm_dois <- iv_cites_pm_dois |> 
  list_rbind()

iv_citations_pm_dois <- iv_citations_pm_dois |> 
  mutate(cited_by_doi = tolower(doi)) |> 
  select(cited_by = pmid, cited_by_doi, journal, pubtype)

ivcpm <- iv_citations_pm |> 
  mutate(cited_by = as.double(cited_by)) |> 
  left_join(iv_citations_pm_dois, by = "cited_by")

## add dois of cited articles from original data.frame
ivcpm <- ivcpm |> 
  select(-cited_work_doi) |> 
  left_join(iv_ids, by = "cited_work") |> 
  select(contains("cited_work"), contains("cited_by"), everything())
#   
# ivcpm |> 
#   write_excel_csv2(here("data", "pmid_citations.csv"))
# 
# ivcpm <- read_csv2(here("data", "pmid_citations.csv"))

ivcpm |> 
  saveRDS(here("data", "processed", "citations_pmid.rds"))

ivcpm <- readRDS(here("data", "processed", "citations_pmid.rds"))

crsr <- ivcpm |> 
  filter(str_detect(journal, "Cochrane"),
         !str_detect(pubtype, "(R|r)eview")
  )


has_cochrane_pmid <- ivcpm |> 
  group_by(cited_work_doi) |> 
  summarise(has_cochrane_review = any(str_detect(journal, "Cochrane") , na.rm = TRUE))
  # summarise(has_cochrane_review = any(str_detect(journal, "Cochrane") & str_detect(pubtype, "(R|r)eview") , na.rm = TRUE))


has_cochrane_pmid |> 
  count(has_cochrane_review)

#what was missed by whom?

# api_key <- Sys.getenv("NCBI_KEY")
# doi <- "10.1002/14651858.cd005343.pub6"
# searchQuery <- paste0(doi, "[DOI]")
# search_by_id <- get_pubmed_ids(searchQuery, api_key = api_key)
# 
# ls_doi <- articles_to_list(fetch_pubmed_data(search_by_id))
# ls_doi
# # some works don't get cited?
# nas <- ivcpm |> 
#   filter(is.na(cited_by))
# 
# # pmids without dois
# short_dois <- ivcpm |> 
#   filter(str_length(cited_by_doi) < 2)
# 
# id_to_meta(doi)

doi_results_pm <- ivcpm |> 
  mutate(doi_or_pmid = case_when(
    str_length(cited_by_doi) < 5 | is.na(cited_by_doi) ~ as.character(cited_by),
    .default = cited_by_doi
  )) |> 
  distinct(doi_or_pmid, .keep_all = TRUE)
  
doi_results_oa <- iv_citations |> 
  # filter(!str_detect(cited_by_doi, "")) |> 
  distinct(cited_by_doi) |> 
  mutate(cited_by_doi = str_extract(cited_by_doi, "10\\..*"))

# setdiff(doi_results_pm$cited_by_doi, doi_results_oa$cited_by_doi)
only_in_oa <- anti_join(doi_results_oa, doi_results_pm)
only_in_pm <- anti_join(doi_results_pm, doi_results_oa)


# citing publications found
dois <- list(PM = doi_results_pm$cited_by_doi,
             oA = doi_results_oa$cited_by_doi)

ggVennDiagram(dois, label_size = 5) +
  scale_fill_viridis_c(option = "cividis", guide = NULL) +
  scale_color_viridis_d(option = "cividis", guide = NULL) +
  ggtitle("Citing publications with DOIs found")

# citing publications found
pubs <- list(PM = doi_results_pm$doi_or_pmid,
             oA = doi_results_oa$cited_by_doi)

ggVennDiagram(pubs, label_size = 5) +
  scale_fill_viridis_c(option = "cividis", guide = NULL) +
  scale_color_viridis_d(option = "cividis", guide = NULL) +
  ggtitle("Citing publications\n (DOIs and PMIDs) found")

only_coch_pm <- has_cochrane_pmid |> 
  filter(has_cochrane_review == TRUE)
only_coch_oa <- has_cochrane |> 
  filter(has_cochrane_review == TRUE)

cochrane <- list(PM = only_coch_pm$cited_work_doi,
             oA = only_coch_oa$cited_work_doi)

ggVennDiagram(cochrane) +
  scale_fill_viridis_c(option = "cividis", guide = NULL) +
  scale_color_viridis_d(option = "cividis", guide = NULL) +
  ggtitle("IntoValue publications cited in\n a Cochrane Review (or Protocol)")



#search for:

pmids <- c("38768286", "38657097")

doi <- "10.23970/AHRQEPCCER267"

cites_pmids <- pmids |> 
  map(get_citing_dois_from_oa, .progress = TRUE)

cites_doi <- doi |> 
  map(get_citing_dois_from_oa, .progress = TRUE)

resp <- oa_fetch(
  entity = "works",
  doi = doi,
  # options = list(select = c("doi", "id", "cited_by_count", "type")),
  verbose = TRUE
)

#not on openAlex, no citing articles found on PubMed

cites_pm <- pmids |> 
  map(get_citing_pmids_from_pubmed, .progress = TRUE) |> 
  list_rbind()

res <- tibble(pmid = pmids) |> 
  get_metadata(idcol = pmid, api_key = Sys.getenv("NCBI_KEY"))

res <- id_to_meta(pmids[1], api_key = Sys.getenv("NCBI_KEY"))


ids <- pmids

if (stringr::str_detect(ids[1], "^10\\.")) {
  id_type = "[DOI]"
} else {
  id_type = "[UID]"
}
searchQuery <- purrr::reduce(paste0(ids, id_type), paste, 
                             sep = " OR ")
search_by_id <- get_pubmed_ids(searchQuery, api_key = api_key)

res <- fetch_pubmed_data(search_by_id)

res2 <- article_to_df(res)


# 126 has_doi, but here 129
pub_dois <- publications |> 
  filter(has_doi == TRUE, has_pubmed == FALSE) |> 
  distinct(doi) |> 
  pull(doi)

publications |> 
  filter(has_doi == TRUE, has_pubmed == TRUE) |> 
  distinct(id)

id_dupes <- publications |> 
  get_dupes(id)

pmid_dupes <- publications |> 
  get_dupes(pmid)


doi_dupes <- publications |> 
  get_dupes(doi)

# n = 2374 dois and pmids together for both pubs and cites, so why other numbers in 
has_both_ids_pubs <- publications |> 
  filter(has_doi == TRUE, has_pubmed == TRUE) |> 
  distinct(doi, pmid, .keep_all = TRUE)

has_cochrane |> 
  filter(has_doi == TRUE, has_pubmed == FALSE)

has_both_ids_cites <- has_cochrane |> 
  filter(has_doi == TRUE, has_pubmed == TRUE) |> 
  distinct(cited_work_doi, cited_work_pmid, .keep_all = TRUE)

dupes <- has_cochrane |> 
  filter(has_doi == TRUE, has_pubmed == TRUE) |> 
  get_dupes(cited_work_doi, cited_work_pmid)



cochr_dupes <- has_cochrane |> 
  get_dupes(cited_work)

cochr_id_dupes <- has_cochrane |> 
  get_dupes(id)


### These 10 are not on openAlex
dois_not_in_oa <- publications |> 
  filter(!doi %in% has_cochrane$cited_work_doi)



iv_missed_doi_cites <- dois_not_in_final$doi |> # takes ca. 4-5 hours!!!
  map(get_citing_dois_from_oa, .progress = TRUE)

iv_missed_citations <- iv_missed_doi_cites |> 
  list_rbind()

iv_citations <- iv_citations |> 
  bind_rows(iv_missed_citations)

neither <- publications |> 
  filter(is.na(doi) & is.na(pmid)) |> 
  select(id, url, publication_date, everything())

neither |> 
  write_excel_csv2(here("data", "processed", "missing_pmids_dois.csv"))

publications |> 
  # distinct(id, .keep_all = TRUE) |>
  count(is.na(doi), has_doi, is.na(pmid), has_pubmed)


