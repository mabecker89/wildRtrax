#' Conduct multivariate analysis on recordings processed by multiple users to assess dissimilarity between species detections
#'
#'
#'

wt_carrefour <- function() {
  input <- dlgInput("Enter a WildTrax ARU project name: ", Sys.info()["user"])$res
  multi <- as_tibble(dbGetQuery(conn = con, paste0('select * from aru.report_v where "project" ~* \'',input,'\'')))
  multi <- multi %>%
    mutate(species_individual_name = case_when(
             abundance == "TMTC" ~ 4,
             abundance == "CI 1" ~ 1,
             abundance == "CI 2" ~ 5,
             abundance == "CI 3" ~ 20,
             TRUE ~ as.numeric(species_individual_name)))
  multi2 <- multi %>%
    select(location, transcriber, species_code, species_individual_name) %>%
    distinct() %>%
    filter(!species_code %in% abiotic_codes) %>%
    group_by(location, transcriber, species_code, species_individual_name) %>%
    summarise(count = n()) %>%
    mutate(count2 = sum(count)) %>%
    ungroup() %>%
    group_by(location, species_code) %>%
    add_count(name = "rich") %>%
    ungroup() %>%
    select(-species_individual_name, -count) %>%
    group_by(species_code, location, transcriber, rich, count2) %>%
    distinct() %>%
    pivot_wider(names_from = species_code, values_from = count2, values_fill = 0) %>%
    as_tibble() %>%
    mutate_if(is.integer, as.numeric)
  multi_type <- multi2 %>%
    select(location, transcriber, rich) %>%
    distinct()
  t3 <- rda(multi2[,-c(1:3)] ~ transcriber + Condition(location, rich), data = multi_type)
  t3 <- ordistep(t3)
  t3scores <- scores(t3, display = "sites") %>%
    as.data.frame() %>%
    rownames_to_column("site") %>%
    bind_cols(., multi_type)
  t3vect <- scores(t3, display = "species") %>%
    as.data.frame()
  plot_PCA <- ggplot() +
    geom_point(data = t3scores, aes(x = PC1, y = PC2, color = transcriber), alpha = 0.6) +
    scale_colour_viridis_d() +
    geom_vline(xintercept = c(0), color = "#A19E99", linetype = 2) +
    geom_hline(yintercept = c(0), color = "#A19E99", linetype = 2) +
    geom_segment(data = t3vect, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm"))) +
    geom_text(data = t3vect, aes(x = PC1, y = PC2, label = rownames(t3vect))) +
    blanktheme +
    labs(x = paste0("PC1 ", round(t3$CA$eig[[1]],2), '%'),
         y = paste0("PC2 ", round(t3$CA$eig[[2]],2), '%'),
         title = paste0("Principal Components Analysis for WildTrax Project: ", input)) +
    stat_ellipse(type = "t", level = 0.67)
  return(plot_PCA)
}


