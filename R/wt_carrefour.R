#' Conduct multivariate analysis on recordings processed by multiple users to assess dissimilarity between species detections
#'
#'
#'
library(vegan)
library(svDialogs)

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
    unite("variable", location:transcriber, sep = "_", remove = T, na.rm = T) %>%
    as_tibble() %>%
    mutate_if(is.integer, as.numeric) %>%
    column_to_rownames("variable")
  multi_type <- multi %>%
    select(location, transcriber)
  t3 <- rda(multi2)
  t3scores <- scores(t3, display = "sites") %>%
    as.data.frame() %>%
    rownames_to_column("site") %>%
    full_join(multi_type, by = c("site" = "location"))
  t3vect <- scores(t3, display = "species") %>%
    as.data.frame()
  plot_PCA <- ggplot() +
    geom_point(data = t3scores, aes(x = PC1, y = PC2)) +
    geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
    geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
    geom_segment(data = t3vect, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm"))) +
    geom_text(data = t3vect, aes(x = PC1, y = PC2, label = rownames(t3vect))) +
    blanktheme +
    labs(x = paste0("PC1 ", round(t3$CA$eig[[1]],2), '%'),
         y = paste0("PC2 ", round(t3$CA$eig[[2]],2), '%'),
         title = "Principal Components Analysis")
  return(plot_PCA)
}

wt_carrefour()
