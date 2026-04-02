#' Get each team Transfermarkt player market values 
#'
#' Returns data frame of player valuations (in Euros) from transfermarkt.com 
#' for an individual team
#'
#' @param each_team_url the url of the required team
#' @param time_pause the wait time (in seconds) between page loads
#'
#' @return returns a dataframe of player valuations for a team
#'
#' @importFrom magrittr |>
#' @importFrom rlang .data
#'
#' @export
#'

tm_each_team_player_market_val <- function(each_team_url, time_pause = 15) {
  
  main_url <- "https://www.transfermarkt.com"
  
  # there now appears to be an errorneous URL so will remove that manually:
  if(any(grepl("com#", each_team_url))) {
    each_team_url <- each_team_url[-grep(".com#", each_team_url)]
  }
  
  each_team_url <- gsub("startseite", "kader", each_team_url) |>
    paste0(., "/plus/1")
  
  Sys.sleep(time_pause)
  
  team_page <- xml2::read_html(each_team_url)
  
  comp_name <- team_page |> rvest::html_elements(".data-header__club a") |> rvest::html_text() |> trimws()
  #country <- team_page |> rvest::html_elements(".data-header__content a img") |> rvest::html_attr("title")
  season_start_year <- gsub(".*saison_id/", "", each_team_url) |> gsub("/plus/1", "", x=_) |> as.numeric()
  
  team_data <- team_page |> rvest::html_nodes("#yw1") |> rvest::html_nodes(".items") |> rvest::html_node("tbody")
  
  tab_head_names <- team_page |> rvest::html_nodes("#yw1") |> rvest::html_nodes(".items") |> rvest::html_nodes("th") |> rvest::html_text()
  
  # team name
  squad <- team_page |> rvest::html_node(".data-header__headline-wrapper--oswald") |> rvest::html_text() |> stringr::str_squish()
  # numbers
  player_num <- team_data |> rvest::html_nodes(".rn_nummer") |> rvest::html_text()
  if(length(player_num) == 0) {
    player_num <- NA_character_
  }
  # player names
  player_name <- team_data |> rvest::html_nodes(".inline-table a") |> rvest::html_text() |> stringr::str_squish()
  if(length(player_name) == 0) {
    player_name <- NA_character_
  }
  # player_url
  player_url <- team_data |> rvest::html_nodes(".inline-table a") |> rvest::html_attr("href") |>
    paste0(main_url, .)
  if(length(player_url) == 0) {
    player_url <- NA_character_
  }
  # player position
  player_position <- team_data |> rvest::html_nodes(".inline-table tr+ tr td") |> rvest::html_text() |> stringr::str_squish()
  if(length(player_position) == 0) {
    player_position <- NA_character_
  }
  # birthdate
  player_birthday <- team_data |> rvest::html_nodes("td:nth-child(3)") |> rvest::html_text()
  if(length(player_birthday) == 0) {
    player_birthday <- NA_character_
  }
  # player_nationality
  player_nationality <- c()
  player_nat <- team_data |> rvest::html_nodes(".flaggenrahmen:nth-child(1)")
  if(length(player_nat) == 0) {
    player_nationality <- NA_character_
  } else {
    for(i in 1:length(player_nat)) {
      player_nationality <- c(player_nationality, xml2::xml_attrs(player_nat[[i]])[["title"]])
    }
  }
  # current club - only for previous seasons, not current:
  current_club_idx <- grep("Current club", tab_head_names)
  if(length(current_club_idx) == 0) {
    current_club <- NA_character_
  } else {
    c_club <- team_data |> rvest::html_nodes(paste0("td:nth-child(", current_club_idx,")"))
    current_club <- c()
    for(cc in c_club) {
      each_current <- cc |> rvest::html_nodes("a") |> rvest::html_nodes("img") |> rvest::html_attr("alt")
      if(length(each_current) == 0) {
        each_current <- NA_character_
      }
      current_club <- c(current_club, each_current)
    }
  }
  # player height
  height_idx <- grep("Height", tab_head_names)
  if(length(height_idx) == 0) {
    player_height_mtrs <- NA_character_
  } else {
    suppressWarnings(player_height_mtrs <- team_data |> rvest::html_nodes(paste0("td:nth-child(", height_idx, ")")) |> rvest::html_text() |>
                       gsub(",", "\\.", .) |> gsub("m", "", .) |> stringr::str_squish() |> as.numeric())
  }
  # player_foot
  foot_idx <- grep("Foot", tab_head_names)
  if(length(foot_idx) == 0) {
    player_foot <- NA_character_
  } else {
    player_foot <- team_data |> rvest::html_nodes(paste0("td:nth-child(", foot_idx,")")) |> rvest::html_text()
  }
  # date joined club
  joined_idx <- grep("Joined", tab_head_names)
  if(length(joined_idx) == 0) {
    date_joined <- NA_character_
  } else {
    date_joined <- team_data |> rvest::html_nodes(paste0("td:nth-child(", joined_idx, ")")) |> rvest::html_text()
  }
  # joined from
  from_idx <- grep("Signed from", tab_head_names)
  if(length(from_idx) == 0) {
    joined_from <- NA_character_
  } else {
    p_club <- tryCatch(team_data |> rvest::html_nodes(paste0("td:nth-child(", from_idx, ")")), error = function(e) NA)
    joined_from <- c()
    for(pc in p_club) {
      each_past <- pc |> rvest::html_nodes("a") |> rvest::html_nodes("img") |> rvest::html_attr("alt")
      if(length(each_past) == 0) {
        each_past <- NA_character_
      }
      joined_from <- c(joined_from, each_past)
    }
  }
  # contract expiry
  contract_idx <- grep("Contract", tab_head_names)
  if(length(contract_idx) == 0) {
    contract_expiry <- NA_character_
  } else {
    contract_expiry <- team_data |> rvest::html_nodes(paste0("td:nth-child(", contract_idx, ")")) |> rvest::html_text()
  }
  # value
  player_market_value <- team_data |> rvest::html_nodes(".rechts.hauptlink") |> rvest::html_text()
  if(length(player_market_value) == 0) {
    player_market_value <- NA_character_
  }
  
  suppressWarnings(team_df <- cbind(squad, player_num, player_name, player_url, player_position, player_birthday, player_nationality, current_club,
                                    player_height_mtrs, player_foot, date_joined, joined_from, contract_expiry, player_market_value) |> data.frame())
  
  team_df <- team_df |> 
    dplyr::mutate(comp_name = comp_name,
                  season_start_year = season_start_year
                  # country = "country"
    ) |> 
    dplyr::mutate(player_market_value_euro = mapply(.convert_value_to_numeric, player_market_value)) |>
    dplyr::mutate(date_joined = .tm_fix_dates(.data[["date_joined"]]),
                  contract_expiry = .tm_fix_dates(.data[["contract_expiry"]])) |>
    tidyr::separate(., player_birthday, into = c("Month", "Day", "Year"), sep = "\\.", remove = F) |> suppressWarnings() |> 
    dplyr::mutate(player_age = gsub(".*\\(", "", .data[["player_birthday"]]) |> gsub("\\)", "", .),
                  player_birthday = gsub("\\s*\\([^)]*\\)", "", .data[["player_birthday"]]),
                  player_dob = suppressWarnings(lubridate::dmy(.data[["player_birthday"]]))) |>
    dplyr::mutate(player_age = as.numeric(gsub("\\D", "", .data[["player_age"]]))) |>
    dplyr::select(.data[["comp_name"]], .data[["season_start_year"]], .data[["squad"]], .data[["player_num"]], .data[["player_name"]], .data[["player_position"]], .data[["player_dob"]], .data[["player_age"]], .data[["player_nationality"]], .data[["current_club"]],
                  .data[["player_height_mtrs"]], .data[["player_foot"]], .data[["date_joined"]], .data[["joined_from"]], .data[["contract_expiry"]], .data[["player_market_value_euro"]], .data[["player_url"]])
  
  
  return(team_df)
  
}