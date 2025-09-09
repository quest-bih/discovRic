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
library(progressr)

# cttd <- read_csv(here("data", "raw", "cttd.csv"))

# cttd_old <- read_xlsx(here("data", "raw", "Dataset_Dashboard.xlsx")) |> 
#   filter(is.na(exclude_include_sgs) | exclude_include_sgs == "include") |> 
#   mutate(doi = coalesce(doi, doi_new_sgs) |> tolower(),
#          pmid = coalesce(pmid, pmid_new_sgs))

cttd <- read_xlsx(here("data", "raw", "combined_dataset_dashboard.xlsx")) |>
  filter(is.na(exclude_include_sgs) | exclude_include_sgs == "include") |> 
  mutate(doi = case_when(!is.na(doi_new_sgs) ~ doi_new_sgs,
                         .default = doi) |> tolower(),
           
           # coalesce(doi, doi_new_sgs) |> tolower(),
         pmid = coalesce(pmid, pmid_new_sgs))
cttd_unique <- cttd |> 
  distinct(id, doi, pmid) |> 
  get_dupes(pmid, doi) |> 
  filter(!is.na(doi)) |> 
  write_excel_csv(here("data", "processed", "dupe_pubs.csv"))
all_oaids <- read_csv(here("data", "raw", "oaids.csv"))

all_oaids |> 
  count(is.na(pmid), is.na(doi))

oaids_from_doi <- all_oaids |> 
  filter(!is.na(doi)) |>
  select(oaid = id, doi)

oaids_from_pmid <- all_oaids |> 
  filter(!id %in% oaids_from_doi$oaid, !is.na(pmid)) |> 
  select(oaid = id, pmid)

cttd <- cttd |> 
  left_join(oaids_from_doi, by = "doi") |> 
  rows_update(oaids_from_pmid, by = "pmid", unmatched = "ignore")


dupes_oaids <- cttd |> 
  get_dupes(oaid) |> 
  select(oaid, dupe_count, doi, pmid, everything())

qa_ids <- cttd |> 
  distinct(doi, pmid, oaid)

# all_oaids <- oaids |> 
#   bind_rows(all_oaids) |> 
#   bind_rows(iv_oaids_from_doi) |> 
#   distinct(id, doi, pmid)

missing_dois <- setdiff(tolower(qa_ids$doi), all_oaids$doi)

false_dois <- setdiff(tolower(all_oaids$doi), qa_ids$doi) # these came via
# previously included articles that were since excluded

missing_pmids <- setdiff(tolower(qa_ids$pmid), all_oaids$pmid) 

# doi_changed <- cttd |> 
#   filter(!is.na(doi_new_sgs) | !is.na(pmid_new_sgs)) |> 
#   select(contains(c("doi", "pmid")), everything())

# 
# iv_data_manual <- read_csv(here("data", "raw", "14_iv-checks-missing-info - sa_extraction.csv"))  |> 
#   filter(!is.na(doi) | !is.na(pmid))

iv_ids <- cttd |>
  filter(!is.na(doi) | !is.na(pmid) | !is.na(oaid)) |> 
  distinct(doi, pmid, oaid) |> 
  select(cited_work_doi = doi, cited_work_pmid = pmid, cited_work_oaid = oaid)

iv_dois <- cttd |> 
  filter(!is.na(doi)) |> 
  distinct(doi) |> 
  pull(doi)

iv_pmids <- cttd |> 
  filter(!is.na(pmid)) |> 
  distinct(pmid) |> 
  pull(pmid)

iv_oaids <- cttd |> 
  filter(!is.na(oaid)) |> 
  distinct(oaid) |> 
  pull(oaid)

only_in_pm_no_doi <- iv_ids |> 
  filter(is.na(cited_work_doi))

# missing_pmid_oaids <- setdiff(iv_pmids, oaids$pmid)

extract_ids <- function(oatib, .keep_all = FALSE) {
  
  extracted <- oatib |> 
    dplyr::mutate(pmid = purrr::map_dbl(ids, \(id) purrr::pluck(id, "pmid") |> 
                                          dplyr::coalesce(NA) |> 
                                          stringr::str_remove_all(".*\\/") |> 
                                          as.numeric()),
                  doi = purrr::map_chr(ids, \(id) purrr::pluck(id, "doi") |> 
                                         dplyr::coalesce(NA)) |> 
                    doi_full2stripped())
  
  if (.keep_all == FALSE) {
    return(
      extracted |> 
        dplyr::select(id, pmid, doi)
    )
  } else {
    return(extracted)
  }
    
}

# 
# iv_oaids_from_pmid <- openalexR::oa_fetch(pmid = missing_pmids,
#                                           entity = "works", mailto = Sys.getenv("EMAIL"))
# 
# iv_oaids_from_pmid <- iv_oaids_from_pmid |> 
#   extract_ids()
# 
# iv_oaids_from_pmid
# 
# iv_oaids_from_pmid <- openalexR::oa_fetch(pmid = cited_id, entity = "works", mailto = mailto)
# 
# iv_oaids_from_pmid |> 
#   count(is.na(pmid))
# 
# iv_oaids_from_pmid <- iv_oaids_from_pmid |> 
#   mutate(pmid = str_remove_all(pmid, ".*\\/") |> 
#            as.numeric())
# 
# oaids <- pmids_from_oaid |> 
#   bind_rows(iv_oaids_from_pmid) |> 
#   distinct(id, pmid, doi)

# dupes <- c("https://openalex.org/W2407792209",
#            "https://openalex.org/W3185400799",
#            "https://openalex.org/W3194736407")
# 
# oaids <- oaids |> 
#   filter(!id %in% dupes)

iv_ids_missing <- iv_ids |> 
  filter(!cited_work_pmid %in% all_oaids$pmid &
           !cited_work_doi %in% all_oaids$doi |
           is.na(cited_work_doi) |
           is.na(cited_work_pmid))

iv_missing_dois <- iv_ids_missing |> 
  filter(!is.na(cited_work_doi),
         is.na(cited_work_oaid)) |> 
  pull(cited_work_doi)


# sb_res <- oa_snowball(identifier = c(oaids, oaids2), id_type = "original",
#                       cited_by_params = list(from_publication_date = "2025-01-01"),
#                                           mailto = Sys.getenv("EMAIL"))
# 
# sb_df <- snowball2df(sb_res)
# 
# sb_df |> 
#   saveRDS(here("data", "processed", "oa_sb_results.rds"))
# 
# sb_df <- readRDS(here("data", "processed", "oa_sb_results.rds"))

# dois_sb <- sb_df |> 
#   transmute(doi = doi_full2stripped(doi),
#             title, id, cited_by, cited_by_api_url, cited_by_count)

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
# cited_id <- iv_pmids_only[1]

# oaid <- cochrane_reviews |> 
#   distinct(cited_work) |> 
#   # slice(1:20) |>
#   pull(cited_work)

oa_to_ids <- function(oaid, mailto = Sys.getenv("EMAIL")) {
  # if (stringr::str_detect(oaid, "^10\\.")) {
    oaid_results <- openalexR::oa_fetch(id = oaid, entity = "works", mailto = mailto)

    oaid_results |> 
      # pull(pmid)
      extract_ids()

}


# cited_id <- "https://openalex.org/W2106767659"

# cited_id <- "10.1182/blood-2012-09-456988"

get_citing_dois_from_oa <- function(cited_id, sleep = 0.5, mailto = Sys.getenv("EMAIL")) {
  
  if (stringr::str_detect(cited_id, "^10\\.")) {
    oaid <- openalexR::oa_fetch(doi = cited_id, entity = "works", mailto = mailto)
    if (rlang::is_null(oaid)) {
      warning(glue::glue("No record for id {cited_id} found on Open Alex!"))
      return(NULL)
    }
    cited_doi <- cited_id
    cited_pmid <- oaid |>
      extract_ids() |> 
      dplyr::pull(pmid)
    oaid <- oaid |> 
      dplyr::pull(id)
    
  } else if (stringr::str_detect(cited_id, "^\\d{8,}")) {
    oaid <- openalexR::oa_fetch(pmid = cited_id, entity = "works", mailto = mailto)
    if (rlang::is_null(oaid)) {
      warning(glue::glue("No record for id {cited_id} found on Open Alex!"))
      return(NULL)
    }
    cited_doi <- oaid |>
      extract_ids() |> 
      dplyr::pull(doi)
    cited_pmid <- cited_id
    oaid <- oaid |> 
      dplyr::pull(id)
  } else {
    oaid <- openalexR::oa_fetch(id = cited_id, entity = "works", mailto = mailto)
    ids <- oaid |>
      extract_ids()
    cited_doi <- dplyr::pull(ids, doi)
    cited_pmid <- dplyr::pull(ids, pmid)
    oaid <- cited_id
  }
  Sys.sleep(stats::rgamma(1, sleep))
  
  records <- openalexR::oa_fetch(cites = oaid, entity = "works",
           mailto = mailto) 
  
  if (rlang::is_null(records)) {
   return(tibble::tibble(cited_by_doi = "no citations found", cited_by_oaid = "no citations found",
                 cited_work_oaid = oaid, cited_work_doi = cited_doi, cited_work_pmid = cited_pmid))
  } else {
    return(records |> 
             dplyr::select(cited_by_doi = doi, cited_by_oaid = id) |> 
             dplyr::mutate(cited_work_oaid = oaid,
                           cited_work_doi = cited_doi,
                           cited_work_pmid = cited_pmid))
  }
}

handlers(global = TRUE)
# p <- progressr::progressor(along = iv_oaids)
iv_cites <- iv_oaids |> # takes ca. 4-5 hours from dois and 2 hours from oaids!
  map(get_citing_dois_from_oa, .progress = TRUE)

iv_citations <- iv_cites |> 
  list_rbind()

iv_citations |> 
  saveRDS(here("data", "processed", "citations.rds"))

iv_citations <- readRDS(here("data", "processed", "citations.rds"))

# Cochrange systematic Reviews have a doi schema:
# cochrane_cd_regex <- "10\\.1002\\/14651858\\.cd.*pub" # fpr every publication
# after the first, which is usually a protocol
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


progressr::with_progress({
  cochrane_meta <- cochrane_reviews |> 
    distinct(cochrane_doi) |> 
    get_metadata(cochrane_doi, chunksize = 50, api_key = Sys.getenv("NCBI_KEY"))
})

# two of the .pub2 results are actually protocols

cochrane_pmids <- cochrane_meta |>
  list_rbind() |> 
  mutate(doi = tolower(doi))

cochrane_pmids |>
  write_excel_csv(here("data", "processed", "cochrane_pmids.csv"))

cochrane_pmids <- read_csv(here("data", "processed", "cochrane_pmids.csv"))

cochrane_reviews <- cochrane_reviews |> 
  left_join(cochrane_pmids |> select(cochrane_doi = doi, cochrane_pmid = pmid)) 

get_cochrane_citation_type <- function(cited_id, cochrane_id, key) {
  
  rentrez::set_entrez_key(key = key)
  
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


rate <- rate_delay(20) # smaller rates like 1 resulted in timeouts, unless chuncked by about 200 at a time

slow_get_cochrane_citation_type <- slowly(\(cited_id, cochrane_id, key, rate) get_cochrane_citation_type(cited_id, cochrane_id, key), rate = rate, quiet = FALSE)

key <- Sys.getenv("ENTREZ_KEY")
cochrane_reviews_types_resp <- cochrane_reviews |>
  filter(!is.na(cochrane_pmid)) |>
  mutate(cited_work_id = ifelse(!is.na(cited_work_pmid), cited_work_pmid, cited_work_doi)) |> 
  # slice(401:771) |> 
  mutate(cochrane_citation_type = map2_chr(cited_work_id, cochrane_pmid, \(x, y) slow_get_cochrane_citation_type(cited_id = x,
                                                             cochrane_id = y,
                                                             key = key,
                                                             rate = rate),
                                           .progress = TRUE))

cochrane_reviews_types_resp |>
  write_csv(here("data", "processed", "cochrane_citation_types.csv"))
# 
# get_cochrane_citation_type(cited_id = "23594786",
#                            cochrane_id = "27432490",
#                            key = Sys.getenv("ENTREZ_KEY"))



cochrane_reviews_types <- cochrane_reviews_types_resp |>
  group_by(cited_work_oaid, cited_work_doi, cited_work_pmid) |>
  mutate(work_has_cochrane_included = any(cochrane_citation_type == "included"),
         work_has_cochrane_excluded = any(cochrane_citation_type == "excluded"),
         work_has_cochrane_not_found = any(cochrane_citation_type == "not_found")) |>
  ungroup()

# cochrane_reviews_types |>
#   filter(work_has_cochrane_included == FALSE,
#          work_has_cochrane_not_found == TRUE) |>
#   count(cited_work_oaid)

cochrane_reviews_types |> 
  rename(cochrane_oaid = cited_by_oaid) |> 
  select(contains("cited_work"), contains("cochrane")) |> 
  write_excel_csv(here("data", "processed", "cochrane_reviews_types.csv"))

cochrane_reviews_types <- read_csv(here("data", "processed", "cochrane_reviews_types.csv"))

cochrane_summaries <- cochrane_reviews_types |> 
  group_by(cited_work_oaid, cited_work_doi, cited_work_pmid) |> 
  summarise(has_cochrane_included = any(cochrane_citation_type == "included"),
            # has_no_cochrane_included = !any(cochrane_citation_type == "included"),
            has_cochrane_excluded = any(cochrane_citation_type == "excluded"),
            has_cochrane_not_found = any(cochrane_citation_type == "not_found")) |> 
  ungroup()

cochrane_summaries |> 
  count(has_cochrane_included)

cochrane_summaries |> 
  count(!has_cochrane_included, has_cochrane_not_found)

# cited_id <- iv_pmids[1]

get_citing_pmids_from_pubmed <- function(cited_id, sleep = 5, api_key = Sys.getenv("NCBI_KEY")) {
  
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
  Sys.sleep(sleep)
  message(paste("Querying ID", pmid))
  records <- rentrez::entrez_link(dbfrom="pubmed", id=pmid, db="pubmed")
  
  if (rlang::is_null(records)) {
    return(tibble::tibble(cited_by_doi = "no citations found", cited_by_pmid = "no citations found",
                          cited_work_pmid = pmid, cited_work_doi = cited_doi))
  } else {
    return(
      # Get the PubMed IDs of citing articles
      
      tibble::tibble(cited_by_pmid = records$links$pubmed_pubmed_citedin,
                     cited_work_pmid = pmid, cited_work_doi = cited_doi)
    )
  }
}

# 3 per second = 180 per minute = 10800 per hour = 1800 per 10 minutes

### Above function unrealistic 
# Users are expected to know a little bit about the EUtils API,
# which is well documented: https://www.ncbi.nlm.nih.gov/books/NBK25500/
  
# The NCBI will ban IPs that don't use EUtils within their user guidelines.
# In particular /enumerated /item Don't send more than three request per second
# (rentrez enforces this limit) /item If you plan on sending a sequence of more
# than ~100 requests, do so outside of peak times for the US /item For large requests
# use the web history method (see examples for entrez_search or use entrez_post to upload IDs)


### TODO: continue from here tomorrow

iv_cites_pm <- iv_pmids |> # takes ca. 1 hour
  map(get_citing_pmids_from_pubmed, .progress = TRUE)

buffered_query <- function(source_vec, buffer_file_path, name_col, .f_query, .n_checkpoint = 2) {
  if (!file.exists(buffer_file_path)) {
    output_data <- NULL
    unprocessed_vec <- source_vec
  } else {
    output_data <- readRDS(buffer_file_path)
    last_processed <- output_data[[length(output_data)]] |> 
      dplyr::slice(1) |> 
      dplyr::pull({{name_col}}) 
    message(paste("Starting after:", last_processed))
    unprocessed_vec <- source_vec[(which(iv_pmids == last_processed) + 1):
                                    length(source_vec)]
    unprocessed_vec
  }
  
  unprocessed_tib <- tibble(source = unprocessed_vec) |> 
    dplyr::mutate(chunk = 1 + cumsum(dplyr::if_else(
      dplyr::row_number() %% .n_checkpoint == 0, 1, 0)))
  
  n_chunks <- ceiling(length(unprocessed_tib) / .n_checkpoint)
  
  ls_ids <- split(unprocessed_tib, f = unprocessed_tib$chunk) |> 
    purrr::map(\(ch) dplyr::pull(ch, source))
  p <- progressr::progressor(along = ls_ids)
  
  for (i in 1:length(ls_ids)) {
    chunked_output <- ls_ids[[i]] |> 
      purrr::map(\(chunk) .f_query(chunk))
    output_data <- append(output_data, chunked_output)
    saveRDS(output_data, buffer_file_path)
    p()
  }
  return(output_data)
}

handlers(global = TRUE)

ret <- buffered_query(source_vec = iv_pmids,
                      here("data", "processed", "buffered_pmid_ffcs.rds"), cited_work_pmid,
               get_citing_pmids_from_pubmed)

iv_citations_pm <- readRDS(here("data", "processed", "buffered_pmid_ffcs.rds")) |>
  list_rbind()
# 
# write_excel_csv(iv_citations_pm, here("data", "pmid_citations_no_doi.csv"))
# 
# iv_citations_pm <- read_csv(here("data", "pmid_citations_no_doi.csv"))

iv_citations_pm_deduped <- iv_citations_pm |> 
  distinct(cited_by_pmid)

iv_cites_pm_dois <- get_metadata(iv_citations_pm_deduped, cited_by_pmid, api_key = Sys.getenv("NCBI_KEY"))

iv_citations_pm_dois <- iv_cites_pm_dois |> 
  list_rbind()

iv_citations_pm_dois <- iv_citations_pm_dois |> 
  mutate(cited_by_doi = tolower(doi)) |> 
  select(cited_by_pmid = pmid, cited_by_doi, journal, pubtype)

ivcpm <- iv_citations_pm |> 
  mutate(cited_by_pmid = as.double(cited_by_pmid)) |> 
  left_join(iv_citations_pm_dois, by = "cited_by_pmid")

## add dois of cited articles from original data.frame
ivcpm <- ivcpm |> 
  select(-cited_work_doi) |> 
  left_join(iv_ids, by = "cited_work_pmid") |> 
  select(contains("cited_work"), contains("cited_by"), everything())

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
    str_length(cited_by_doi) < 5 | is.na(cited_by_doi) ~ as.character(cited_by_pmid),
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

# neither <- publications |> 
#   filter(is.na(doi) & is.na(pmid)) |> 
#   select(id, url, publication_date, everything())
# 
# neither |> 
#   write_excel_csv2(here("data", "processed", "missing_pmids_dois.csv"))

publications |> 
  # distinct(id, .keep_all = TRUE) |>
  count(is.na(doi), has_doi, is.na(pmid), has_pubmed)


