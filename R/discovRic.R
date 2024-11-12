library(tidyverse)
library(here)
library(openalexR)
library(glue)
library(rcrossref)
library(easyRPubMed)
library(rentrez)
library(ggVennDiagram)
library(janitor)
library(readxl)
library(pdfRetrieve)

### TODO add manually retrieved pmids and dois and perform all citation searchers, etc.

# cttd <- read_csv(here("data", "raw", "cttd.csv"))

cttd <- read_xlsx(here("data", "raw", "Dataset_Dashboard.xlsx")) |> 
  filter(is.na(exclude_include_sgs) | exclude_include_sgs == "include") |> 
  mutate(doi = coalesce(doi, doi_new_sgs),
         pmid = coalesce(pmid, pmid_new_sgs))

doi_changed <- cttd |> 
  filter(!is.na(doi_new_sgs) | !is.na(pmid_new_sgs)) |> 
  select(contains(c("doi", "pmid")), everything())

# 
# iv_data_manual <- read_csv(here("data", "raw", "14_iv-checks-missing-info - sa_extraction.csv"))  |> 
#   filter(!is.na(doi) | !is.na(pmid))

iv_ids <- cttd |>
  filter(!is.na(doi) | !is.na(pmid)) |> 
  distinct(doi, pmid) |> 
  select(cited_work_doi = doi, cited_work = pmid)

iv_dois <- cttd |> 
  filter(!is.na(doi)) |> 
  distinct(doi) |> 
  pull(doi)

iv_pmids <- cttd |> 
  filter(!is.na(pmid)) |> 
  distinct(pmid) |> 
  pull(pmid)

only_in_pm_no_doi <- iv_ids |> 
  filter(is.na(cited_work_doi))

# dois <- sample(iv_dois, 200)

res <- oa_fetch(doi = iv_dois, entity = "works", mailto = Sys.getenv("EMAIL"))

oaids <- res$id

res2 <- oa_fetch(pmid = only_in_pm_no_doi$cited_work, entity = "works", mailto = Sys.getenv("EMAIL"))

oaids2 <- res2$id


sb_res <- oa_snowball(identifier = c(oaids, oaids2), id_type = "original",
                      # cited_by_params = list(from_publication_date = "2025-01-01"),
                                          mailto = Sys.getenv("EMAIL"))

sb_df <- snowball2df(sb_res)

sb_df |> 
  saveRDS(here("data", "processed", "oa_sb_results.rds"))

sb_df <- readRDS(here("data", "processed", "oa_sb_results.rds"))

dois_sb <- sb_df |> 
  transmute(doi = doi_full2stripped(doi),
            title, id, cited_by, cited_by_api_url, cited_by_count)

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

oaid <- cochrane_reviews |> 
  distinct(cited_work) |> 
  # slice(1:20) |>
  pull(cited_work)

oa_to_ids <- function(oaid, mailto = Sys.getenv("EMAIL")) {
  # if (stringr::str_detect(oaid, "^10\\.")) {
    oaid_results <- openalexR::oa_fetch(id = oaid, entity = "works", mailto = mailto)

    oaid_results |> 
      # pull(pmid)
      dplyr::mutate(pmid = purrr::map_chr(ids, \(id) purrr::pluck(id, "pmid") |> 
                                            dplyr::coalesce(NA)),
                    doi = purrr::map_chr(ids, \(id) purrr::pluck(id, "doi") |> 
                                           dplyr::coalesce(NA))) |> 
      dplyr::select(id, pmid, doi)

}


pmids_from_oaid <- oa_to_ids(oaid)

pmids_from_oaid <- pmids_from_oaid |> 
  mutate(pmid = str_remove(pmid, ".*\\/"))

pmids_from_oaid |> 
  write_excel_csv(here("data", "processed", "pmids_from_oaid.csv"))

get_citing_dois_from_oa <- function(cited_id, sleep = 0.5, mailto = Sys.getenv("EMAIL")) {
  
  if (stringr::str_detect(cited_id, "^10\\.")) {
    oaid <- openalexR::oa_fetch(doi = cited_id, entity = "works", mailto = mailto)
    if (rlang::is_null(oaid)) {
      warning(glue::glue("No record for id {cited_id} found on Open Alex!"))
      return(NULL)
    }
    cited_doi <- cited_id
    cited_pmid <- oaid |> 
      dplyr::pull(pmid)
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
  filter(str_detect(cited_by_doi, cochrane_cd_regex)) |> 
  mutate(cochrane_doi = pdfRetrieve::doi_full2stripped(cited_by_doi))

has_cochrane <- iv_citations |> 
  group_by(cited_work_doi) |> 
  summarise(has_cochrane_review = any(str_detect(cited_by_doi, cochrane_cd_regex), na.rm =  TRUE))

has_cochrane |> 
  count(has_cochrane_review)

pmids_from_oaid <- read_csv(here("data", "processed", "pmids_from_oaid.csv")) |> 
  distinct(id, .keep_all = TRUE)

####### TODO use rentrez history as in tutorial

cochrane_reviews_types <- cochrane_reviews |>
  left_join(pmids_from_oaid, by = c("cited_work" = "id")) |> 
  mutate(cited_work_pmid = coalesce(cited_work_pmid, pmid)) |> 
  select(-pmid, -doi)
 
cochrane_pmids_prior <- ivcpm |>
  filter(str_detect(cited_by_doi, cochrane_cd_regex)) |>
  distinct(cited_by, cited_by_doi) |>
  rename(cochrane_pmid = cited_by,
         cochrane_doi = cited_by_doi)

cochrane_pmids <- cochrane_reviews_types |>
  distinct(cited_by) |>
  pull(cited_by) |>
  oa_to_ids()

cochrane_pmids <- cochrane_pmids |> 
  transmute(cochrane_id = id,
            cochrane_pmid = str_remove(pmid, ".*\\/"),
            cochrane_doi = str_extract(doi, "10\\..*"))

cochrane_pmids_missing <- cochrane_pmids |> 
  filter(is.na(cochrane_pmid)) |> 
  select(-cochrane_pmid) |> 
  left_join(cochrane_pmids_prior, by = "cochrane_doi")

cochrane_metadata <- cochrane_pmids_missing |> 
  filter(is.na(cochrane_pmid),
         str_detect(cochrane_doi, "pub")) |> 
  get_metadata(cochrane_doi, chunksize = 50, api_key = Sys.getenv("NCBI_KEY"))

cochrane_metadata <- cochrane_metadata |> 
  # list_rbind() |> 
  select(cochrane_doi = doi,
         cochrane_pmid = pmid) |> 
  mutate(cochrane_doi = tolower(cochrane_doi))

cochrane_pmids_missing <- cochrane_pmids_missing |> 
  rows_update(cochrane_metadata, by = "cochrane_doi") |> 
  mutate(cochrane_pmid = as.character(cochrane_pmid))

cochrane_pmids <- cochrane_pmids |> 
  rows_update(cochrane_pmids_missing, by = "cochrane_doi") |> 
  filter(!is.na(cochrane_pmid))

cochrane_reviews_types |>
  left_join(cochrane_pmids, by = "cochrane_doi") |> 
  slice(459) |> 
  t()

rate <- rate_delay(0.2)
cochrane_reviews_types <- cochrane_reviews_types |>
  left_join(cochrane_pmids, by = "cochrane_doi") |> 
  # select(-oaid) |>
  filter(!is.na(cochrane_pmid)) |> 
  mutate(cited_work_id = ifelse(!is.na(cited_work_pmid), cited_work_pmid, cited_work_doi)) |> 
  mutate(cochrane_citation_type = map2_chr(cited_work_id, cochrane_pmid, \(x, y) slow_get_cochrane_citation_type(cited_id = x,
                                                             cochrane_id = y,
                                                             key = Sys.getenv("ENTREZ_KEY"),
                                                             rate = rate),
                                           .progress = TRUE))

get_cochrane_citation_type(cited_id = "26738812",
                           cochrane_id = "35028933",
                           key = Sys.getenv("ENTREZ_KEY"))


cochrane_reviews_types <- cochrane_reviews_types |>
  group_by(cited_work, cited_work_doi, cited_work_pmid) |>
  mutate(work_has_cochrane_included = any(cochrane_citation_type == "included"),
         work_has_cochrane_excluded = any(cochrane_citation_type == "excluded"),
         work_has_cochrane_not_found = any(cochrane_citation_type == "not_found")) |>
  ungroup()



cochrane_reviews_types |>
  filter(work_has_cochrane_included == FALSE,
         work_has_cochrane_not_found == TRUE) |>
  count(cited_work)

cochrane_reviews_types |> 
  select(-cited_by, -cited_by_doi) |> 
  write_excel_csv2(here("data", "processed", "cochrane_reviews_types.csv"))

cochrane_reviews_types <- read_csv(here("data", "processed", "cochrane_reviews_types.csv"))

cochrane_summaries <- cochrane_reviews_types |> 
  group_by(cited_work, cited_work_doi, cited_work_pmid) |> 
  summarise(has_cochrane_included = any(cochrane_citation_type == "included"),
            # has_no_cochrane_included = !any(cochrane_citation_type == "included"),
            has_cochrane_excluded = any(cochrane_citation_type == "excluded"),
            has_cochrane_not_found = any(cochrane_citation_type == "not_found")) |> 
  ungroup()

cochrane_summaries |> 
  count(has_cochrane_included, has_no_cochrane_included)

cochrane_summaries |> 
  count(has_cochrane_included)

cochrane_summaries |> 
  count(!has_cochrane_included, has_cochrane_not_found)

cochrane_summaries |> 
  filter()

cochrane_reviews_types2 |>
  slice(528) |> 
  t()

get_cochrane_citation_type(cited_id = "21474646",
                           cochrane_id = "23450552",
                           key = Sys.getenv("ENTREZ_KEY"))

dois <- iv_dois[1:50]

# cited_doi <- "10.3389/fnagi.2015.00152"
pmid <- "20818904"
doi <- "10.1002/14651858.CD005343.pub3"
cited_id <- pmid
cochrane_id <- "24203004"
# Find citing articles
citing_articles <- entrez_link(dbfrom="pubmed", id=pmid, db="pubmed")
# Get the PubMed IDs of citing articles
citing_pmids <- citing_articles$links$pubmed_pubmed_citedin


dois_citing <- get_metadata(tibble(pmid = citing_pmids), pmid, api_key = Sys.getenv("NCBI_KEY"))

 
# summaries <- entrez_summary(db="pubmed", id=citing_pmids)
# 
# summaries <- entrez_summary(db="pubmed", id=pmid)
# usethis::edit_r_environ()
# refs <- entrez_fetch(db="pubmed", id = pmid, rettype = "xml") #, parsed = TRUE)


# rr <- refs |> xml2::read_xml()
# 
# # cited_id <- "10.3389/fnagi.2015.00152"
# cr_res <- cr_works(dois=doi, .progress = "text")
# cr_refs <- cr_res$data$reference[[1]]

# cited_id <- "10.1302/2633-1462.36.BJO2022-0036"
# citing_id <- pmid

##### only works with pmids! not with dois!!!!!

get_cochrane_citation_type <- function(cited_id, cochrane_id, key) {
  
  rentrez::set_entrez_key(key = key)
  
  # if (stringr::str_detect(cited_id, "^10\\.")) {
  #   cited_id_pmid <- easyRPubMed::id_to_meta(cited_id, api_key = Sys.getenv("NCBI_KEY")) 
  # 
  #   cited_id_pmid |> 
  #     xml2::xml_find_first(xpath = "//ArticleId")
  #     
  #   easyRPubMed::id_to_meta("10.1016/s0168-8278(11)60030-5", api_key = Sys.getenv("NCBI_KEY")) 
  # } else {
  #   cited_id_pmid <- cited_id
  # }
  
  cited_id <- tolower(cited_id)
  
  refs <- rentrez::entrez_fetch(db="pubmed", id = cochrane_id, rettype = "xml") |>
    xml2::read_xml() |>
    xml2::xml_find_all(xpath = "//PubmedData/ReferenceList")
  
  ref_titles <- refs |> 
    xml2::xml_find_all(xpath = "./Title") |>
    xml2::xml_text()
  
  refs_included <- refs[stringr::str_detect(ref_titles, "included")] |> 
    xml2::xml_find_all(xpath = ".//ArticleId") |>
    xml2::xml_text() |> 
    tolower()
  
  refs_excluded <- refs[stringr::str_detect(ref_titles, "excluded")] |> 
    xml2::xml_find_all(xpath = ".//ArticleId") |>
    xml2::xml_text() |> 
    tolower()
  
  refs_other <- refs[!stringr::str_detect(ref_titles, "cluded")] |> 
    xml2::xml_find_all(xpath = ".//ArticleId") |>
    xml2::xml_text() |> 
    tolower()
  
  return(
    dplyr::case_when(
      cited_id %in% refs_included ~ "included",
      cited_id %in% refs_excluded ~ "excluded",
      cited_id %in% refs_other ~ "other",
      .default = "not_found"
    )
  )
  
}

rate <- rate_delay(0.1)
slow_get_cochrane_citation_type <- slowly(\(cited_id, cochrane_id, key, rate) get_cochrane_citation_type(cited_id, cochrane_id, key), rate = rate, quiet = FALSE)

get_cochrane_citation_type(cited_id = "1593914", cochrane_id = pmid, key = Sys.getenv("ENTREZ_KEY"))

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


# 
# iv_cites_pm <- iv_pmids |> # takes ca. 1 hour
#   map(get_citing_pmids_from_pubmed, .progress = TRUE)
# 
# iv_citations_pm <- iv_cites_pm |> 
#   list_rbind()

# write_excel_csv2(iv_citations_pm, here("data", "pmid_citations_no_doi.csv"))

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


