## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>",
warning = FALSE,
cache = FALSE,
message = FALSE,
eval = TRUE,
fig.width = 9
)

## ----setup--------------------------------------------------------------------
# library(spatialplanr)
devtools::load_all()

## -----------------------------------------------------------------------------
Region <- "Coral Sea" # "Australia"
Type <- "Oceans" # "EEZ"

## ----eval=FALSE---------------------------------------------------------------
#  Region <- c(xmin = 150, xmax = 160, ymin = -40, ymax = -30)

## -----------------------------------------------------------------------------
cCRS <- "ESRI:54009"

## -----------------------------------------------------------------------------
PU_size <- 107460 # m

## -----------------------------------------------------------------------------
Bndry <- splnr_get_boundary(Limits = Region, Type = Type, cCRS = cCRS)

## -----------------------------------------------------------------------------
landmass <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  sf::st_transform(cCRS)

## -----------------------------------------------------------------------------
PUs <- spatialgridr::get_grid(boundary = Bndry, 
                              crs = cCRS,
                              output = "sf_hex", 
                              resolution = PU_size)


## -----------------------------------------------------------------------------
(ggPU <- splnr_plot(df = PUs) +
   ggplot2::theme_bw()) # Plot Planning Units

## -----------------------------------------------------------------------------
(ggPU <- splnr_plot(df = PUs) +
   splnr_gg_add(
     Bndry = Bndry, overlay = landmass,
     cropOverlay = PUs, ggtheme = "Default"
   ))

## -----------------------------------------------------------------------------
splnr_theme <- list(
  ggplot2::theme_bw(),
  ggplot2::theme(
    legend.position = "right",
    legend.direction = "vertical",
    text = ggplot2::element_text(size = 9, colour = "black"),
    axis.text = ggplot2::element_text(size = 9, colour = "black"),
    plot.title = ggplot2::element_text(size = 9),
    axis.title = ggplot2::element_blank()
  )
)

(ggPU <- splnr_plot(PUs) +
    splnr_gg_add(
      Bndry = Bndry, overlay = landmass,
      cropOverlay = PUs, ggtheme = splnr_theme
    ))

## -----------------------------------------------------------------------------
Dict <- tibble::tribble(
  ~nameCommon, ~nameVariable, ~category, 
  "Green sea turtle", "Chelonia_mydas", "Reptiles", 
  "Loggerhead sea turtle", "Caretta_caretta", "Reptiles", 
  "Hawksbill sea turtle", "Eretmochelys_imbricata", "Reptiles", 
  "Olive ridley sea turtle", "Lepidochelys_olivacea", "Reptiles", 
  "Saltwater crocodile", "Crocodylus_porosus", "Reptiles", 
  "Humpback whale", "Megaptera_novaeangliae", "Mammals", 
  "Common Minke whale", "Balaenoptera_acutorostrata", "Mammals", 
  "Dugong", "Dugong_dugon", "Mammals", 
  "Grey nurse shark", "Carcharias_taurus", "Sharks and rays", 
  "Tiger shark", "Galeocerdo_cuvier", "Sharks and rays", 
  "Great hammerhead shark", "Sphyrna_mokarran", "Sharks and rays", 
  "Giant oceanic manta ray", "Mobula_birostris", "Sharks and rays", 
  "Reef manta ray", "Mobula_alfredi", "Sharks and rays", 
  "Whitetip reef shark", "Triaenodon_obesus", "Sharks and rays",
  "Red-footed booby", "Sula_sula", "Birds"
)

## -----------------------------------------------------------------------------
datEx_species_bin <- dat_species_prob %>%
  splnr_apply_cutoffs(Cutoffs = 0.5)

