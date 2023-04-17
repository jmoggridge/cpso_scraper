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

# get data for a single doctor from their cpso page
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


# setup browser
driver <- RSelenium::rsDriver(browser = "firefox",
                              chromever = NULL,
                              verbose = F,
                              port = netstat::free_port())
remote <- driver$client

# fill in form and submit, proceed to results[1,2,3,...]
remote$navigate("https://doctors.cpso.on.ca/?search=general")
remote |> execute_search()





scrape_doctor_page('https://doctors.cpso.on.ca/DoctorDetails/Hussein-Abdurrahman-Abujrad/0229777-84138', remote) |>
  unnest(spec_tbl)
scrape_doctor_page('', remote)
scrape_doctor_page('', remote)
scrape_doctor_page('', remote)