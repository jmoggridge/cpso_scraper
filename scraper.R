## CPSO website scraper; configured for 'endocrinologist' specialty but could be generalized to collect all physicians & possibly served via webapi...

# let selenium select both options under "CPSO Registration Status (required):"
# [x] Search doctors currently registered with the CPSO
# [x] Search doctors no longer registered with the CPSO

# need to select 'Type of Doctor' --> 'Specialist'
# need to select 'Specialization' --> 'Endocrinology and Metabolism'

## For each physician page,
'https://doctors.cpso.on.ca/DoctorDetails/{first}-{last}/{cspo_number}/....'
## extract:
# physician names
# practice address
# year of becoming an independent member
# gender

library(tidyverse)
library(RSelenium)
library(netstat)

execute_search <- function(remote) {
  # add inactive doctors
  remote$executeScript(
    "document.getElementById('p_lt_ctl01_pageplaceholder_p_lt_ctl02_CPSO_AllDoctorsSearch_chkInactiveDoctors').click()"
  )

  # Select "Type of Doctor" -> "Specialist"
  remote$executeScript(
    "document.getElementById('rdoDocTypeSpecialist').click()"
  )

  # select "Specialization" -> "Endocrinology and Metabolism"
  spec_option <- remote$findElement(
    using = 'xpath',
    value = "//*/option[@value = '152']"
  )
  spec_option$clickElement()

  submit <- remote$findElement(using = 'name',
                               value = 'p$lt$ctl01$pageplaceholder$p$lt$ctl02$CPSO_AllDoctorsSearch$btnSubmit1')
  submit$clickElement()
  return(remote)
}

# get up to 10 doctors' links from one results page
links_from_results_page <- function(remote){
  remote$findElements(using = 'tag name', value = 'a') |>
    map(~.x$getElementAttribute(attrName = 'href')) |>
    keep(~length(.) == 1) |>
    unlist() |>
    keep(~str_detect(., 'https://doctors.cpso.on.ca/DoctorDetails/'))
}

# from search results page, find last page number
get_total_pages <- function(remote){
  pages <- remote$findElement(
    using = 'xpath',
    value = '//*[@id="p_lt_ctl01_pageplaceholder_p_lt_ctl03_CPSO_DoctorSearchResults_lnbLastPage"]'
  )$getElementText() |>
    pluck(1) |>
    as.numeric()
}


# follow links to five results pages
click_through_five_pages <- function(remote){

  # scrape links from one results page
  goto_results_page_get_links <- function(xpath){
    remote$findElement('xpath', xpath)$clickElement()
    links_from_results_page(remote)
  }

  # handle errors with results page
  possibly_get_links <- possibly(goto_results_page_get_links,
                                 otherwise = NULL)

  # iterate over the five page links in the pagination section
  lst(int = str_pad(0:4, 2, pad = '0')) |>
    glue::glue_data('//*[@id="p_lt_ctl01_pageplaceholder_p_lt_ctl03_CPSO_DoctorSearchResults_rptPages_ctl{int}_lnbPage"]') |>
    map(~possibly_get_links(.))
}

# navigate to next pagination of five pages
next_five_pages <- function(remote){
  remote$findElement('xpath', '//*[@id="p_lt_ctl01_pageplaceholder_p_lt_ctl03_CPSO_DoctorSearchResults_lnbNextGroup"]')$clickElement()
}

# iterate over all search results pages
get_links_from_results <- function(remote){
  map(
    .x = seq(ceiling(get_total_pages(remote)/5)),
    .f = ~{
      dat <- click_through_five_pages(remote)
      next_five_pages(remote)
      return(dat)
    },
    .progress = 'Collecting links'
  )
}

# test for no duplicates
all_distinct <- function(x) length(unique(x)) == length(x)


# scrape links from search for ''
scrape_doctor_urls <- function(remote){

  remote$navigate("https://doctors.cpso.on.ca/?search=general")

  # fill in form and submit, proceed to results[1,2,3,...]
  remote |> execute_search()

  # go through paginated results and get urls
  links <- get_links_from_results(remote) |>
    enframe(name = 'group', value = 'link') |>
    unnest(link) |>
    unnest(link) |>
    select(link)

  stopifnot(all_distinct(links$link))
  return(links)
}

# get text from an element and simplify to vector
get_element_text <- function(xpath, remote){
  remote$findElement('xpath', xpath)$getElementText() |> unlist()
}

# tidies up text data
str_cleanup <- function(x){
  x |> str_remove(',') |> str_trim()
}

## get the table of specialties of the current doctor page
scrape_specialties_table <- function(remote){
  # find the right table
  tbl <- remote$findElement('id', 'specialties')
  # find the body of that table
  tbl_body <- tbl$findChildElement(using = 'tag name', 'tbody')
  # get all the rows of that table
  tbl_rows <- tbl_body$findChildElements(using = 'tag name', 'tr') |>
    enframe(name = 'row', value = 'element')
  # get td cells from each row and parse the 3 values, return table
  tbl_rows |>
    mutate(cells = map(
      element,
      ~.$findChildElements('tag name', 'td') |>
        map(~.$getElementText()) |>
        set_names('specialty', 'specialty_issued_date', 'specialty_type')
    )
    ) |>
    unnest_wider(cells) |>
    select(specialty, specialty_issued_date, specialty_type) |>
    mutate(across(everything(), ~unlist(.) |> paste0()))
}


## get data for a single doctor from their cpso page
scrape_doctor_page <- function(link, remote){
  remote$navigate(link)

  spec_tbl <- scrape_specialties_table(remote)

  lst(
    name = '//*[@id="docTitle"]',
    cpso_id = '/html/body/form/section/div/div/div[2]/div[3]/div[1]/h3',
    address = '/html/body/form/section/div/div/div[2]/div[4]/section[2]/div/div[2]',
    member_status = '/html/body/form/section/div/div/div[2]/div[3]/div[2]/div[2]/strong',
    curr_or_past_cpso_reg_class = '/html/body/form/section/div/div/div[2]/div[3]/div[3]/div[2]',
    info =  '/html/body/form/section/div/div/div[2]/div[4]/section[1]/div[2]'
  ) |>
    map(get_element_text, remote = remote) |>
    as_tibble_row() |>
    mutate(spec_tbl = list(spec_tbl))
}

## Main ----

# setup browser
driver <- RSelenium::rsDriver(browser = "firefox",
                              chromever = NULL,
                              verbose = F,
                              port = netstat::free_port())
remote <- driver$client

# get all the links from the search for endocrinology + metabolism
links <- scrape_doctor_urls(remote)

# collect data from each link
endocrinologists <-
  links |>
  mutate(data = map(link, scrape_doctor_page,
                    remote = remote,
                    .progress = 'Collecting data')) |>
  unnest_wider(data) |>
  mutate(
    cpso_id = str_remove(cpso_id, '^CPSO#: '),
    gender = info |>
      str_extract('(Gender:.*?)\n') |>
      str_remove_all('Gender: |\n')
  )

endocrinologists <- endocrinologists |>
  transmute(
    cpso_id = str_remove(cpso_id, '^CPSO#: '),
    lastname = str_extract(name, '^.*?,') |> str_cleanup(),
    firstname = str_extract(name, ',.*?$') |> str_cleanup(),
    address,
    gender,
    member_status_date = member_status |>
      str_remove('.*? as of ')  |>
      lubridate::parse_date_time(orders = '%d %b %Y'),
    member_status_class = member_status |>
      str_remove('as of.*?$') |>
      str_trim(),
    cpso_reg_class = curr_or_past_cpso_reg_class |>
      str_extract('.*?as of') |>
      str_remove('as of') |>
      str_cleanup(),
    cpso_reg_date = curr_or_past_cpso_reg_class |>
      str_extract('[0-9].*?$') |>
      lubridate::parse_date_time(orders = '%d %b %Y'),
    more_raw_info = info,
    link,
    spec_tbl
  ) |>
  glimpse()


endocrinologist_specialites <-
  endocrinologists |>
  select(cpso_id, spec_tbl) |>
  unnest(spec_tbl) |>
  unnest(spec_tbl) |>
  mutate(
    specialty_issued_date = specialty_issued_date |>
      str_remove('Effective:') |>
      parse_date(format = '%d %b %Y')
    )

stopifnot(
  # check that each doctor is represented in specialties table
  endocrinologist_specialites |> distinct(cpso_id) |> nrow() ==
    nrow(endocrinologists)
)

fs::dir_create('output')
write_csv(endocrinologists |> select(-spec_tbl),
          'output/endocrinologists.csv')
write_csv(endocrinologist_specialites,
          'output/endocrinologists_specialties.csv')
driver$server$stop()

beepr::beep()
cli::cli_alert_success('Finished')

