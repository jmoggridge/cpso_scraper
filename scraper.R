

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

links_from_results_page <- function(remote){
  remote$findElements(using = 'tag name', value = 'a') |>
    map(~.x$getElementAttribute(attrName = 'href')) |>
    keep(~length(.) == 1) |>
    unlist() |>
    keep(~str_detect(., 'https://doctors.cpso.on.ca/DoctorDetails/'))
}


get_total_pages <- function(remote){
  # from search results page, find last page number
  pages <- remote$findElement(
    using = 'xpath',
    value = '//*[@id="p_lt_ctl01_pageplaceholder_p_lt_ctl03_CPSO_DoctorSearchResults_lnbLastPage"]'
  )$getElementText() |>
    pluck(1) |>
    as.numeric()
}


click_through_five_pages <- function(remote){

  goto_results_page_get_links <- function(xpath){
    remote$findElement('xpath', xpath)$clickElement()
    links_from_results_page(remote)
  }

  possibly_get_links <- possibly(goto_results_page_get_links,
                                 otherwise = NULL)

  lst(int = str_pad(0:4, 2, pad = '0')) |>
    glue::glue_data('//*[@id="p_lt_ctl01_pageplaceholder_p_lt_ctl03_CPSO_DoctorSearchResults_rptPages_ctl{int}_lnbPage"]') |>
    map(~possibly_get_links(.))
}

next_five_pages <- function(remote){
  remote$findElement('xpath', '//*[@id="p_lt_ctl01_pageplaceholder_p_lt_ctl03_CPSO_DoctorSearchResults_lnbNextGroup"]')$clickElement()
}


get_links_from_results <- function(remote){
  map(
    .x = seq(ceiling(pages/5)),
    .f = ~{
      dat <- click_through_five_pages(remote)
      next_five_pages(remote)
      return(dat)
    },
    .progress = 'Iterating over all pages'
  )
}

all_distinct <- function(x) length(unique(x)) == length(x)


scrape_doctor_urls <- function(){
  # setup
  driver <- RSelenium::rsDriver(browser = "firefox",
                                chromever = NULL,
                                port = netstat::free_port())
  remote <- driver$client
  remote$navigate("https://doctors.cpso.on.ca/?search=general")

  # fill in form and submit, proceed to results[1,2,3,...]
  remote |> execute_search()

  # go through paginated results and get urls
  pages <- get_total_pages(remote)
  links <- get_links_from_results(remote) |>
    enframe(name = 'group', value = 'link') |>
    unnest(link) |>
    unnest(link)

  stopifnot(all_distinct(links$link))
  driver$server$stop()
  return(links)
}

