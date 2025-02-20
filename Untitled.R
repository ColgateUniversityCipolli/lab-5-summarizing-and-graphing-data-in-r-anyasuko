########################################################################
#        Lab 05 Code file
########################################################################
########################################################################


########################################################################
#step 1: Write a function that uses tidyverse to determine whether 
# allentown is out of range, unusual, or within the range for each band
# start w? overall_loudness before trying to apply to whole data set
########################################################################

install.packages("tidyverse")
library(tidyverse)

allentown.data <- read.csv("data/essentia.data.allentown.csv")
essentia.data <- read.csv("data/essentia.data.csv")
  
allen.loud <- allentown.data[["overall_loudness"]]




for (col = 4:length(esenta.data)){
  feature = essentia.data[col]
  allen.feature = allentown.data[col]
  description = func(feature, allen.feature)
}


func <- function(feature, allen.feature){
  essentia.data |>
    group_by(artist) |>
    summarize(
      min = min(feature),
      q1 = quantile(feature, .25),
      q3 = quantile(feature, .75),
      qr = q3-q1,
      LF = q1 - (1.51*qr),
      UF = q3 + (1.51*qr),
      max = max(feature)
    ) |>
    mutate(out.of.range = (min > allen.feature)|(max< allen.feature),
           unusual = (LF>allen.feature)|(max<allen.feature),
    ) |>
    mutate(
      description = case_when(out.of.range==TRUE~ "Out of Range",
                              unusual == TRUE~ "Outlying",
                              TRUE ~ "Within Range")
    )
}

