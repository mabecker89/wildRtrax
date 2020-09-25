#' Conduct multivariate analysis on recordings processed by multiple users to assess dissimilarity between species detections
#'
#'
#'

wt_carrefour <- function() {
  input <- dlgInput("Enter a WildTrax ARU project name: ", Sys.info()["user"])$res
  multi <- as_tibble(dbGetQuery(conn = con, paste0('select * from aru.report_v where "project" ~* \'',input,'\'')))
  multi2 <- multi %>%
    select(location, transcriber, species_code, species_individual_name) %>%
    distinct() %>%
    filter(!species_code %in% abiotic_codes) %>%
    group_by(location, transcriber, species_code, species_individual_name) %>%
    summarise(count = n()) %>%
    mutate(count2 = sum(count)) %>%
    ungroup() %>%
    select(-species_individual_name, -count) %>%
    group_by(species_code, location, transcriber, count2) %>%
    distinct() %>%
    pivot_wider(names_from = species_code, values_from = count2, values_fill = 0) %>%
    as_tibble() %>%
    mutate_if(is.integer, as.numeric)
  multi_type <- multi2 %>%
    select(location, transcriber) %>%
    distinct()
  t3 <- rda(multi2[,-c(1:2)])
  t3scores <- scores(t3, display = "sites") %>%
    as.data.frame() %>%
    rownames_to_column("site") %>%
    bind_cols(., multi_type)
  t3vect <- scores(t3, display = "species") %>%
    as.data.frame()
  m_loc<-adonis(multi2[,-c(1:2)] ~ location, data = multi_type)
  m_obs<-adonis(multi2[,-c(1:2)] ~ transcriber, data = multi_type)
  m_add<-adonis(multi2[,-c(1:2)] ~ location + transcriber, data = multi_type)
  m_int<-adonis(multi2[,-c(1:2)] ~ location * transcriber, data = multi_type)
  model.list <- list(m_loc, m_obs, m_add, m_int)
  plot_PCA <- ggplot() +
    geom_point(data = t3scores, aes(x = PC1, y = PC2, color = transcriber)) +
    scale_colour_viridis_d() +
    geom_vline(xintercept = c(0), color = "#A19E99", linetype = 2) +
    geom_hline(yintercept = c(0), color = "#A19E99", linetype = 2) +
    geom_segment(data = t3vect, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm"))) +
    geom_text(data = t3vect, aes(x = PC1, y = PC2, label = rownames(t3vect))) +
    blanktheme +
    labs(x = paste0("PC1 ", round(t3$CA$eig[[1]],2), '%'),
         y = paste0("PC2 ", round(t3$CA$eig[[2]],2), '%'),
         title = paste0("Principal Components Analysis for WildTrax Project: ", input))
  return(list(plot_PCA, model.list))
}

wt_carrefour()

