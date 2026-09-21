import pandas as pd

df_counts = r.airway_counts
df_meta = r.airway_meta


# 1. Force the SummarizedExperiment metadata to a true base R data frame
meta_df <- as.data.frame(colData(your_se_object))

# 2. Export directly to CSV from R
write.csv(meta_df, "airway_meta.csv", row.names = FALSE)
