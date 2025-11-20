# FUNCTIONS TO RUN THE NATURE TOOL ----

#' get_company_data
#' Matches ISINs in the input portfolio to ISINs in the database and calculates a coverage rate
#'
#' @param input_data the table provided by ISIN2LEI with issuers information
#'
#' @return a table with all the input ISINs, the related company (ID, Name and LEI when available), sector, country and invested amount,
#' as well as the share of portfolio covered in the database
#' @export
#'

get_company_data <- function(input_data) {
  portfolio_data <- input_data %>%
    left_join(isin2lei_mapping_final, by = "ISIN")

  total_amount <- sum(as.numeric(input_data$Amount), na.rm = TRUE)
  covered_amount <- sum(as.numeric((portfolio_data %>% filter(!is.na(ID)))$Amount), na.rm = TRUE)
  share_portfolio_covered <- covered_amount / total_amount

  return(list(portfolio_data = portfolio_data,
              share_portfolio_covered = share_portfolio_covered))
  }


#' get_subsidiaries
#' Finds all the subsidiaries (when existing) for each company in the input portfolio
#'
#' @param portfolio_data the input portfolio joined with sector, country information. It is the "portfolio_complete_data" output of get_company_data function
#' @param isin2lei_mapping_final the data from ISIN2LEI combined with all existing ISINs (open source), with all issuers and their subsidiaries information
#'
#' @return an extension of the portfolio table with all the subsidiaries associated with issuers in the initial portfolio (when they exist)
#' @export
#'

get_subsidiaries <- function(portfolio_data, isin2lei_mapping_final) {

  # Create a column "Amount_attributed"
  portfolio_data <- portfolio_data %>%
    mutate(Amount_attributed = Amount)

  # Check if lines with Type == "Issuer" exist
  if (!any(portfolio_data$Type == "Issuer")) {
    message("No entity with subsidiaries in the portfolio.")
    return(portfolio_data)
  }

  # Filter on Type = "Issuer"
  issuer_rows <- portfolio_data %>% filter(Type == "Issuer")

  # Retreive all the subsidiaries for each ID
  subsidiary_rows <- issuer_rows %>%
    rowwise() %>%
    do({
      id_val <- .$ID
      portfolio_val <- .$Portfolio_Name
      amount_val <- .$Amount
      currency_val <- .$Currency
      isin_val <- .$ISIN  # Issuer's initial ISIN

      subs <- isin2lei_mapping_final %>%
        filter(IssuerID == id_val, Type == "Subsidiary") %>%
        distinct(LEI, ID, Name, Country, NACE, IssuerID, SubsidiaryID_old, Type, .keep_all = TRUE)

      nb_subs <- nrow(subs)

      if (nb_subs == 0) {
        # No subsidiary: keep the initial line (already present in the portfolio)
        return(NULL)
      }

      subs <- subs %>%
        mutate(
          Portfolio_Name = portfolio_val,
          ISIN = isin_val,
          Amount = amount_val,
          Currency = currency_val,
          Amount_attributed = amount_val / nb_subs
        )

      subs
    }) %>%
    ungroup()

  # For issuers with subsidiaries, set the attributer amount to 0 (the Amount is split across subsidiaries only)
  issuers_with_subs <- unique(subsidiary_rows$IssuerID)
  portfolio_data <- portfolio_data %>%
    mutate(Amount_attributed = ifelse(ID %in% issuers_with_subs & Type == "Issuer", 0, Amount_attributed))

  # Combine with the initial portfolio
  portfolio_extended <- bind_rows(portfolio_data, subsidiary_rows)

  # Order columns
  col_order <- c("Portfolio_Name", "ISIN", "Amount", "Amount_attributed", "Currency",
                 "LEI", "ID", "Name", "Country", "NACE", "IssuerID", "SubsidiaryID_old", "Type")

  portfolio_extended <- portfolio_extended %>%
    select(all_of(col_order))

  return(portfolio_extended)
}

#' get_impacts
#' Selects impacts from the impacts database (impacts of all ISINs in the universe) for the ISINs in the portfolio
#' It returns:
#' 1. Line by line impacts (for the issuers and subsidiaries if they exist and for each Exiobase industry associated with one issuer or subsidiary) of the portfolio
#' 2. Aggregated impacts for the issuers and subsidiaries if they exist of the portfolio
#' 3. Aggregated impacts for each ISIN (entity) in the portfolio
#'
#' @param portfolio_complete_data portfolio data linked with ISIN2LEI information to link each ISIN to the related entity and subsidiaries if they exist
#' @param all_impacts dataset with impacts for all the entities of the universe (issuers and subsidiaries), broken down by Exiobase industry
#'
#'@return portfolio_all_impacts: Line by line impacts (for each ISIN in the input file, impacts across of all subsidiaries and all associated Exiobase industries)
#' portfolio_impacts_by_isin: Aggregated impacts by Scope, Pressure, Realm and Accounting category for each ISIN of the input file
#' portfolio_impacts_by_subsidiary: Aggregated impacts by Scope, Pressure, Realm and Accounting category for across all subsidiaries (no Exiobase industry breakdown)
#' @export
#'

get_impacts <- function(portfolio_complete_data, all_impacts, impacts_by_entity) {

  # ==== No aggregation ====

  # Line by line impacts: for each ISIN in the input file, impacts across of all subsidiaries and all associated Exiobase industries)
  portfolio_all_impacts <- portfolio_complete_data %>%
    left_join(all_impacts, by = c("ID" = "Name_business")) %>%
    mutate(Footprint_MSAkm2_attributed = Footprint_MSAkm2 * Turnover_attributed_EUR * 10^-6) %>% # [MSA.km²] = [MSA.km²/MEUR] x [EUR] x 10^-6
    select(-SubsidiaryID_old, -Group_1, -Currency_to_EUR_rate_2024, -rotation_ratio_country, -rotation_ratio, -Footprint_MSAkm2, -Footprint_MSAppb)

  # ==== Aggregation level 1 ====

  # Aggregated impacts by Scope, Pressure, Realm and Accounting category for across all subsidiaries (no Exiobase industry breakdown)
  portfolio_impacts_by_subsidiary <- portfolio_complete_data %>%
    left_join(impacts_by_entity, by = c("ID" = "Name_business")) %>%
    mutate(Footprint_MSAkm2_attributed = Footprint_MSAkm2 * Turnover_attributed_EUR * 10^-6) %>% # [MSA.km²] = [MSA.km²/MEUR] x [EUR] x 10^-6
    select(-SubsidiaryID_old, -Group_1, -Currency_to_EUR_rate_2024, -rotation_ratio_country, -rotation_ratio, -Footprint_MSAkm2, -Footprint_MSAppb)

  # ==== Aggregation level 2 ====
  # Aggregated impacts by Scope, Pressure, Realm and Accounting category for each ISIN of the input file

  # Footprint aggregation
  footprint_agg <- portfolio_impacts_by_subsidiary %>%
    group_by(Portfolio_Name, ISIN, Amount, Amount_EUR, Currency, Scope, Pressure, realm, accounting_category) %>%
    summarise(Footprint_MSAkm2_attributed = sum(Footprint_MSAkm2_attributed, na.rm = TRUE), .groups = "drop")

  # Turnover aggregation
  turnover_agg <- portfolio_impacts_by_subsidiary %>%
    group_by(Portfolio_Name, ISIN, Amount, Currency, Amount_EUR, Amount_attributed, Amount_attributed_EUR, Turnover_attributed_EUR,
             ID, LEI, Name, NACE, Country, IssuerID, Type) %>%
    summarise(.groups = "drop") %>%  # garde les lignes uniques
    group_by(Portfolio_Name, ISIN, Amount, Currency, Amount_EUR) %>%
    summarise(Turnover_EUR = sum(Turnover_attributed_EUR, na.rm = TRUE), .groups = "drop")

  # Final aggregation
  portfolio_impacts_by_isin <- footprint_agg %>%
    left_join(turnover_agg, by = c("Portfolio_Name", "ISIN", "Amount", "Currency", "Amount_EUR"))

  # ==== Return results ====
  return(list(portfolio_all_impacts = portfolio_all_impacts,
              portfolio_impacts_by_subsidiary =  portfolio_impacts_by_subsidiary,
              portfolio_impacts_by_isin = portfolio_impacts_by_isin))
}

#' get_dependencies
#' Selects dependencies from the dependencies database (dependencies of all entities in the universe) for the ISINs in the portfolio
#' Returns line by line dependencies (for the issuers and subsidiaries if they exist) of the portfolio
#' As well as aggregated dependencies for each ISIN (entity) in the portfolio
#'
#' @param portfolio_complete_data portfolio data linked with ISIN2LEI information to link each ISIN to the related entity and subsidiaries if they exist
#' @param dependencies_by_entity dataset with dependencies (average or critical) for all the entities of the universe
#'
#' @return portfolio_dependencies: line by line dependencies (for each ISIN in the input file, dependencies across of all subsidiaries
#' as well as  portfolio_dependencies_by_isin: Aggregated dependencies for each ISIN (entity) of the input file (no subsidiaries breakdown)
#' @export
#'

get_dependencies <- function(portfolio_complete_data, dependencies_by_entity) {

  # ==== Aggregation level 1 ====

  # Aggregated dependencies across of all subsidiaries (no Exiobase industry breakdown)
  portfolio_dependencies_by_subsidiary <- portfolio_complete_data %>%
    left_join(dependencies_by_entity, by = c("ID" = "Name_business")) %>%
    select(-SubsidiaryID_old, -Group, -Currency_to_EUR_rate_2024, -rotation_ratio_country, -rotation_ratio)

  # ==== Aggregation level 2 ====

  # Aggregated dependencies for each ISIN of the input file
  portfolio_dependencies_by_isin <- portfolio_dependencies_by_subsidiary %>%
    group_by(Portfolio_Name, ISIN, Amount, Currency,Amount_EUR, Scope) %>%
    mutate(Turnover_EUR = sum(Turnover_attributed_EUR, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(share_turnover = Turnover_attributed_EUR / Turnover_EUR) %>%
    mutate(across(starts_with("dependency_"), ~ .x * share_turnover, .names = "{.col}")) %>%
    group_by(Portfolio_Name, ISIN, Amount, Currency, Amount_EUR, Turnover_EUR, Scope) %>%
    summarise(across(starts_with("dependency_"), ~ sum(.x, na.rm = TRUE), .names = "{.col}"))

  # ==== Return results ====
  return(list(portfolio_dependencies_by_subsidiary = portfolio_dependencies_by_subsidiary,
              portfolio_dependencies_by_isin = portfolio_dependencies_by_isin))
}
