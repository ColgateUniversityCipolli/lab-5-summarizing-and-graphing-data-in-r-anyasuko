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
  

#function that takes the current feature, and generates a comparison result for allentown vs. the three artists, and returns the comparisions
compare <- function(feature, allen.feature){
  essentia.data |>
    group_by(artist) |>
    summarize(
      min = min(get(feature)),
      q1 = quantile(get(feature), .25),
      q3 = quantile(get(feature), .75),
      qr = q3-q1,
      LF = q1 - (1.51*qr),
      UF = q3 + (1.51*qr),
      max = max(get(feature))
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

# goes through all different features for both the essentia data and the allentown line of data
# and calls func() for each feature to determine if allentown is in/out/close to the range for the current feature
num.of.cols <- ncol(essentia.data)
for (i in 4:num.of.cols){
  feature = colnames(essentia.data)[i] 
  allen.feature = allentown.data[[feature]]
  description = compare(feature, allen.feature)
} 



