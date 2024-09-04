#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })
    
    output$ui_tree <- renderUI({
      
      req(input$sel_taxa)
      
      # flds_taxa_id <- c(glue('{setdiff(flds_taxa, "scientificName")}_id') |> as.vector(), "taxonID")
      # flds_taxa_id <- c(setdiff(flds_taxa, "scientificName"), "taxonID")
      # input <- list(sel_taxa = 82696)
      
      taxa_str <- paste(input$sel_taxa, collapse = "|")
      d0 <- d <- dbGetQuery(
        con, glue("
          SELECT tsn::INT, scientific_name, taxon_rank, path::TEXT 
            FROM taxa_hierarchy 
            WHERE path ~ '*.{taxa_str}.*'")) |> 
          tibble()
      d <- dbGetQuery(
        con, 
        # https://stackoverflow.com/questions/43510596/collect-all-leaf-nodes-with-postgres-ltree
        glue("WITH a AS (
             -- get all nodes containing taxa_str in their path
             SELECT tsn::INT, scientific_name, taxon_rank, path  -- path::TEXT 
               FROM taxa_hierarchy 
               WHERE path ~ '*.{taxa_str}.*')
            -- subset to all leaf end nodes
             SELECT * 
               FROM a AS a1
               WHERE NOT EXISTS (
                 SELECT 1 
                   FROM a AS a2 
                   WHERE 
                      a1.path <> a2.path AND 
                      a1.path @> a2.path )")) |> 
        tibble()
      # browser()
      
      # TODO: rename path to path_itis
      # TODO: add drop-down of taxonomic authority: ITIS, WoRMS, GBIF, COL, ...; add path_* to species table
      
      if (nrow(d) == 1) {
        
        d_tr <- d |> 
          select(tsn, taxon_rank, scientific_name) |> 
          mutate(
            level = glue("{scientific_name} ({taxon_rank}; {tsn})"))
        
        # browser()
        
        tsn_selected <- d$tsn
        tr_taxa <- create_tree(
          d_tr,
          levels    = "level",
          levels_id = "tsn")
      } else {
        
        str_tsns <- paste(d$tsn, collapse = ",")
        # get least common ancestor (lca)
        root <- dbGetQuery(
          con, 
          glue("SELECT lca(array_agg(path))::TEXT AS lca FROM taxa_hierarchy WHERE tsn::INT IN ({str_tsns})")) |> 
          pull(lca)
        # lca() misses one extra level, per [Ltree.lca has confusing behavior wrt to the lca function provided by the ltree extension in postgres · Issue #468 · kvesteri/sqlalchemy-utils](https://github.com/kvesteri/sqlalchemy-utils/issues/468)
        # append one level deeper
        # root      = "202423.914154.914156.158852.331030.914179.161061.161105.166082.167640.172249"
        # d$path[1] = "202423.914154.914156.158852.331030.914179.161061.161105.166082.167640.172249.172250"
        # root <- as.character(d$path[1]) |> str_replace(glue("({root}\\.[0-9]+).*"), "\\1")
        
        ranks_keep <- c("kingdom", "phylum", "class", "order", "family", "genus", "species") # , "species")
        
        z <- d |>
          mutate(
            path = str_replace(path, glue("({root})\\.(.*)"), "\\2")) |> 
          select(tsn_target = tsn, tsn = path) |>
          separate_longer_delim(tsn, delim = ".") |>  # 66 × 2
          distinct(tsn_target, tsn) |> 
          mutate(
            tsn = as.integer(tsn)) |>
          left_join(
            d_taxa |> 
              select(tsn = id, name, rank) |> 
              mutate(rank = tolower(rank)), 
            by = "tsn") |> 
          filter(
            rank %in% ranks_keep | 
              tsn_target == tsn) |> 
          filter(
            !is.na(name),
            !is.na(rank)) |> # rm 98056; why name/rank == NA
          mutate(
            tsn_target = as.character(tsn_target),
            tsn        = as.character(tsn) )
          
        ranks_tr <- intersect(ranks_keep, unique(z$rank)) # orde
        w <- z |> 
          mutate(
            name = glue("{name} ({rank}; {tsn})")) |> 
          pivot_wider(
            names_from  = rank, 
            values_from = c(name, tsn),
            names_glue  = "{rank}_{.value}") |> 
          select(-tsn_target)
        
        # i <- which(is.na(w$genus_tsn))
        # w$genus_tsn[i] <- glue("NAgenus{i}") 
        # i <- which(is.na(w$species_tsn))
        # w$species_tsn[i] <- glue("NAspecies{i}") 
        
        # browser()
        tr_taxa <- create_tree(
          w,
          levels    = glue("{ranks_tr}_name"), 
          levels_id = glue("{ranks_tr}_tsn") )
        # listviewer::jsonedit(tr)
        
        # View(z)
        # d_tr <- z |>
        #   arrange(!!! flds_tr) |>
        #   mutate(across(is.character, ~replace_na(., "")))
        
        get_n <- function(x){length(unique(x))}
        n_d_tsn        <- get_n(d$tsn)
        n_z_tsn_target <- get_n(z$tsn_target)
        n_z_tsn        <- get_n(z$tsn)
        n_w_tsn_target <- get_n(w$tsn_target)
        message(glue("
          n_d_tsn:        {n_d_tsn}
          n_z_tsn_target: {n_d_tsn}
          n_z_tsn:        {n_z_tsn}
          n_w_tsn_target: {n_w_tsn_target}"))
        
        # Galatheidae (family)
        # 97964
        # 97964 %in% unique(z$tsn)
        # children are NA
        # z |> filter(tsn_target == 98056)
        #   tsn_target   tsn name         rank  
        #        <int> <dbl> <chr>        <chr> 
        # 1      98056 89787 Malacostraca class 
        # 2      98056 95599 Decapoda     order 
        # 3      98056 97964 Galatheidae  family
        # 4      98056 98056 NA           NA 
        # https://itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=98056
        # Pleuroncodes planipes (98056; invalid) -> Grimothea planipes (1202386; valid)
        # d_taxa |> filter(id == 98056)
        # d_taxa |> filter(status != "valid")
        
        tsn_selected <- unique(z$tsn_target)
        message(glue("selected: {paste(tsn_selected,collapse = ', ')}"))
        
      }
      
      treeInput(
        "sel_tree",
        label       = "Taxa",
        choices     = tr_taxa,
        selected    = tsn_selected,
        returnValue = "id",
        closeDepth  = 2 )
      
    })
    
    # observe changes in selected taxa and send message to console
    observeEvent(input$sel_tree, {
      message(glue("sel_tree: {paste(input$sel_tree, collapse = ', ')}"))
    })

}
