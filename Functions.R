#######################################################################
#Functions for Indices calculation
#Function for calculating NDVI (Normalized Difference Vegetation Index)
ndvi <- function(img, r, nir){
  br <- img[[r]]
  bnir <- img [[nir]]
  ndvi <- (bnir - br) / (bnir + br)
  return(ndvi)
}

#Function for calculating NDSI (Normalized Difference Salinity Index)
ndsi <- function(img, r, nir){
  br <- img[[r]]
  bnir <- img [[nir]]
  ndsi <- (br - bnir) / (br + bnir)
  return(ndsi)
}

#Function for calculating NDDI (Normalized Difference Infrared Index)
ndii <- function(img, nir, swir1){
  bnir <- img [[nir]]
  bswir1 <- img [[swir1]]
  ndii <- (bnir - bswir1) / (bnir + bswir1)
  return(ndii)
}

#Function for calculating CSRI (Combined Spectral Response Index)
csri <- function(img, b, g, r, nir, ndvi){
  br <- img[[r]]
  bg <- img[[g]]
  bb <- img[[b]]
  bnir <- img [[nir]]
  bndvi <- img[[ndvi]]
  csri <- (bb + bg) / (br + bnir) * bndvi
  return(csri)
}

#Function for calculating Salinity Index
si_T <- function(img, r, nir){
  br <- img[[r]]
  bnir <- img [[nir]]
  si_T <- (br / bnir) * 100
  return(si_T)
}

#Function for calculating Salinity Index
si_1 <- function(img, nir, swir1){
  bnir <- img [[nir]]
  bswir1 <- img [[swir1]]
  si_1 <- (bnir / bswir1)
  return(si_1)
}

#Function for calculating Salinity Index
si_2 <- function(img, b, r){
  bb <- img [[b]]
  br <- img [[r]]
  si_2 <- (bb - br) / (bb + br)
  return(si_2)
}

#Function for calculating Salinity Index 
si_3 <- function(img, b, g, r){
  bb <- img [[b]]
  bg <- img [[g]]
  br <- img [[r]]
  si_3 <- (bb * br) / bg
  return(si_3)
}

#Function for calculating VSSI (Vegetation Soil Salinity Index)
vssi <- function(img, g, r, nir){
  bg <- img [[g]]
  br <- img [[r]]
  bnir <- img [[nir]]
  vssi <- 2 * bg - 5 * (br+bnir)
  return(vssi)
}

#Add Water
ndwi <- function(img, g, swir1){
  bg <- img [[g]]
  bswir <- img [[swir1]]
  ndwi <- (bg - bswir) / (bg + bswir)
  return(ndwi)
}
#add watermask
watermask <- function(img, ndvi, ndwi) {
  bndvi <- img [[ndvi]]
  bndwi <- img[[ndwi]]
  watermask <- bndvi <= 0 & bndwi >= 0
}
