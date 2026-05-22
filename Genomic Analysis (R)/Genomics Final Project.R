library(parathyroidSE)
library(tidyverse)
library(Biobase)
library(SummarizedExperiment)
library(ggplot2)
library(DESeq2)
library(tidyr)
library(org.Hs.eg.db)
library(clusterProfiler)
library(enrichplot)

col_control <- "royalblue4"
col_DPN <- "green4"
col_OHT <- "tan4"
col_accent <- "#E76F51"


######################
### Analyze Object ###
######################
ptgse <- parathyroidGenesSE

class(ptgse)
dim(ptgse)


## coldata to view possible covariates

colptgse <- colData(ptgse)


ucol1 <- unique(colptgse$run); ucol1; length(ucol1)

# evaluate levels of each column data variable
unique(colptgse$experiment)
unique(colptgse$patient)
unique(colptgse$treatment)
unique(colptgse$time)
unique(colptgse$submission)
unique(colptgse$study)
unique(colptgse$sample)

# visualize the distributions of counts
names(assays(ptgse))
assays(ptgse)$log2 <- log2(assays(ptgse)$counts + 1)

hist(assays(ptgse)$log2)


# create shortname for later use
sample_info <- as.data.frame(colData(ptgse))
sample_info$shortname <- paste0("S", 1:27)



###
### DESeq workflow
##


# Create DESeqDataSet object
dds <- DESeqDataSet(ptgse, design = ~ patient + treatment + time + patient*time)

# Filter low expressed genes
keep <- rowSums(counts(dds) >= 10) >= 3
dds <- dds[keep, ]
cat("After filtering:", nrow(dds), "genes\n")

# Estimate size factors (normalization)
dds <- estimateSizeFactors(dds)

# Variance stabilizing transformation (for visualizations)
vsd <- vst(dds, blind = FALSE)

# Run DE analysis using DESeq2
dds <- DESeq(dds)

res <- results(dds, contrast= c('treatment', 'DPN', 'Control'), alpha = 0.05)

res.2 <- results(dds, contrast = c('treatment', 'OHT', 'Control'), alpha = 0.05)

# Results
cat("DE genes (padj < 0.05):", sum(res$padj < 0.05, na.rm = TRUE), "\n")
cat("  Up:", sum(res$padj < 0.05 & res$log2FoldChange > 0, na.rm = TRUE), "\n")
cat("  Down:", sum(res$padj < 0.05 & res$log2FoldChange < 0, na.rm = TRUE), "\n")



desq.matrix <- model.matrix(design(dds), colData(dds));desq.matrix

res_df <- as.data.frame(res)

res_df <- res_df %>% arrange(padj)
res_df[1:10, ]


###################
#### QC PLOTS #####
###################


########
# count dist raw vs. normal
#######

raw_log <- log2(counts(dds, normalized = FALSE) + 1)
norm_log <- log2(counts(dds, normalized = TRUE) + 1)

box_raw <- tidyr::pivot_longer(
  as.data.frame(raw_log) |> dplyr::mutate(gene = rownames(raw_log)),
  cols = -gene, names_to = "sample_id", values_to = "log2count"
)
box_raw$stage <- "Raw counts"

box_norm <- tidyr::pivot_longer(
  as.data.frame(norm_log) |> dplyr::mutate(gene = rownames(norm_log)),
  cols = -gene, names_to = "sample_id", values_to = "log2count"
)
box_norm$stage <- "DESeq2 normalized"

box_df <- rbind(box_raw, box_norm)
box_df$stage <- factor(box_df$stage, levels = c("Raw counts", "DESeq2 normalized"))
box_df$sample_id <- sub("^V", "", box_df$sample_id)
box_df$sample_id <- factor(box_df$sample_id,
                        levels = as.character(1:length(box_df$sample_id)))

# Map sample IDs to short names
id_map <- setNames(sample_info$shortname, rownames(sample_info))
box_df$sample <- id_map[box_df$sample_id]
box_df$treatment <- sample_info$treatment[match(box_df$sample_id, rownames(sample_info))]

ggplot(box_df, aes(x = sample_id, y = log2count, fill = treatment)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.2) +
  facet_wrap(~ stage) +
  scale_fill_manual(values = c("Control" = col_control, "DPN" = col_DPN, "OHT" = col_OHT)) +
  labs(title = "Count distributions before and after normalization",
       subtitle = "log2(count + 1)",
       x = "Sample", y = "log2(count + 1)", fill = "treatment") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))


#########
### PCA
#########

set.seed(123)
pca <- prcomp(t(assay(ptgse)))
pct_var <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)

pca_df <- data.frame(
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  treatment = sample_info$treatment,
  patient = sample_info$patient,
  sample = sample_info$shortname
)

ggplot(pca_df, aes(x = PC1, y = PC2, color = treatment, shape = patient)) +
  geom_point(size = 5) +
  geom_text_repel(aes(label = sample), size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c("Control" = col_untrt, "DPN" = col_DPN, "OHT"= col_OHT)) +
  labs(title = "PCA of variance-stabilized counts",
       subtitle = "Visualizing dominant sources of sample-level variation",
       x = paste0("PC1 (", pct_var[1], "% variance)"),
       y = paste0("PC2 (", pct_var[2], "% variance)"),
       color = "treatment", shape = "patient") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))




################
### DE PLOTS ###
################


###
### MA PLots
###

res_shrunk_DPN <- lfcShrink(dds, coef = "treatment_DPN_vs_Control", type = "apeglm")
res_shrunk_OHT <- lfcShrink(dds, coef = "treatment_OHT_vs_Control", type = "apeglm")
  
par(mfrow = c(1, 3))
DESeq2::plotMA(res, main = "Before LFC shrinkage", ylim = c(-6, 6))
DESeq2::plotMA(res_shrunk_DPN, main = "After LFC shrinkage DPN", ylim = c(-6, 6))
DESeq2::plotMA(res_shrunk_OHT, main = "After LFC shrinkage OHT", ylim = c(-6, 6))
par(mfrow = c(1, 1))

###
### volcano plots
###


res_df.DPN <- as.data.frame(res)
res_df.DPN$padj[is.na(res_df.DPN$padj)] <- 1
res_df.DPN$sig <- "NS"
res_df.DPN$sig[res_df.DPN$padj < 0.05 & res_df.DPN$log2FoldChange > 1] <- "Up"
res_df.DPN$sig[res_df.DPN$padj < 0.05 & res_df.DPN$log2FoldChange < -1] <- "Down"

ggplot(res_df.DPN, aes(x = log2FoldChange, y = -log10(padj), color = sig)) +
  geom_point(size = 0.5, alpha = 0.5) +
  scale_color_manual(values = c("Up" = "#C0392B", "Down" = "#2471A3", "NS" = "grey70")) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
  labs(x = expression(log[2]~fold~change),
       y = expression(-log[10]~(adjusted~p-value)),
       title = "Volcano Plot - DESeq2 DPN",
       color = "Significance") +
  theme_bw(base_size = 13)

res_df.OHT <- as.data.frame(res.2)
res_df.OHT$padj[is.na(res_df.OHT$padj)] <- 1
res_df.OHT$sig <- "NS"
res_df.OHT$sig[res_df.OHT$padj < 0.05 & res_df.OHT$log2FoldChange > 1] <- "Up"
res_df.DPN$sig[res_df.OHT$padj < 0.05 & res_df.OHT$log2FoldChange < -1] <- "Down"

ggplot(res_df.DPN, aes(x = log2FoldChange, y = -log10(padj), color = sig)) +
  geom_point(size = 0.5, alpha = 0.5) +
  scale_color_manual(values = c("Up" = "#C0392B", "Down" = "#2471A3", "NS" = "grey70")) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
  labs(x = expression(log[2]~fold~change),
       y = expression(-log[10]~(adjusted~p-value)),
       title = "Volcano Plot - DESeq2 DPN",
       color = "Significance") +
  theme_bw(base_size = 13)

ggplot(res_df.OHT, aes(x = log2FoldChange, y = -log10(padj), color = sig)) +
  geom_point(size = 0.5, alpha = 0.5) +
  scale_color_manual(values = c("Up" = "#C0392B", "Down" = "#2471A3", "NS" = "grey70")) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
  labs(x = expression(log[2]~fold~change),
       y = expression(-log[10]~(adjusted~p-value)),
       title = "Volcano Plot - DESeq2 OHT",
       color = "Significance") +
  theme_bw(base_size = 13)



###
### p-value distribution
###

hist(res$pvalue, breaks = 50, col = "steelblue", border = "white",
     main = "P-value Distribution — DESeq2",
     xlab = "P-value", ylab = "Frequency")
abline(h = sum(!is.na(res$pvalue)) / 50, col = "red", lty = 2, lwd = 2)
legend("topright", expression("Expected under H"[0]), col = "red", lty = 2, lwd = 2)



##################
### ENRICHMENT ###
##################

###
### ORA
###
gene_map <- AnnotationDbi::mapIds(
  org.Hs.eg.db,
  keys = rownames(res),
  keytype = "ENSEMBL",
  column = "ENTREZID",
  multiVals = "first"
)

# how many mapped?
table(!is.na(gene_map))

# also map to gene symbols (for fgsea / msigdbr)
symbol_map <- AnnotationDbi::mapIds(
  org.Hs.eg.db,
  keys = rownames(res),
  keytype = "ENSEMBL",
  column = "SYMBOL",
  multiVals = "first"
)

# add to results
res$entrez <- gene_map[rownames(res)]
res$symbol <- symbol_map[rownames(res)]

head(res)


# ----- Define DE gene list and background -----

# DE genes: FDR < 0.05 and |log2FC| > 1
de_idx <- which(res$padj < 0.05 & abs(res$log2FoldChange) > 1)
de_genes <- res$entrez[de_idx]
de_genes <- de_genes[!is.na(de_genes)]  # remove NAs
length(de_genes)

# background: all genes that were tested (important!)
bg_genes <- res$entrez[!is.na(res$entrez)]
length(bg_genes)

# ----- GO enrichment (Biological Process) -----

ego_bp <-   enrichGO(
  gene = de_genes,
  universe = bg_genes,
  OrgDb = org.Hs.eg.db,
  ont = "BP",
  pAdjustMethod = "BH",
  pvalueCutoff = 0.05,
  qvalueCutoff = 0.2,
  readable = TRUE  # convert Entrez IDs to gene symbols in output
)

# view top results
head(ego_bp)
dim(ego_bp)

set.seed(123)
dotplot(ego_bp, showCategory = 15, title = "GO Biological Process (ORA)")






###
### GSEA
###


res_complete <- res[!is.na(res$stat) & !is.na(res$entrez), ]

# create named vector: names = Entrez IDs, values = stat
ranks_entrez <- res_complete$stat
names(ranks_entrez) <- res_complete$entrez

# deduplicate Entrez IDs
# (multiple Ensembl IDs can map to the same Entrez ID)
# keep the entry with the largest absolute statistic
ranks_entrez <- ranks_entrez[order(abs(ranks_entrez), decreasing = TRUE)]
ranks_entrez <- ranks_entrez[!duplicated(names(ranks_entrez))]

ranks_entrez <- sort(ranks_entrez, decreasing = TRUE)

# check: positive values = upregulated in treated; negative = downregulated
head(ranks_entrez)
tail(ranks_entrez)

# also create a symbol-named version (for fgsea with msigdbr gene sets)
res_complete_sym <- res[!is.na(res$stat) & !is.na(res$symbol), ]
ranks_symbol <- res_complete_sym$stat
names(ranks_symbol) <- res_complete_sym$symbol
# handle duplicate symbols: keep the one with largest absolute stat
ranks_symbol <- ranks_symbol[order(abs(ranks_symbol), decreasing = TRUE)]
ranks_symbol <- ranks_symbol[!duplicated(names(ranks_symbol))]
ranks_symbol <- sort(ranks_symbol, decreasing = TRUE)

head(ranks_symbol)
tail(ranks_symbol)

# ----- GSEA with GO Biological Process -----

gsea_bp <- gseGO(
  geneList = ranks_entrez,
  OrgDb = org.Hs.eg.db,
  ont = "BP",
  minGSSize = 15,
  maxGSSize = 500,
  pvalueCutoff = 0.05,
  verbose = FALSE
)

head(gsea_bp)
dim(gsea_bp)


dotplot(gsea_bp, showCategory = 15, title = "GO BP (GSEA)")

gseaplot2(gsea_bp, geneSetID = 1, title = gsea_bp$Description[1])



#############################
### UNSUPERVISED ANALYSIS ### seed set
#############################

set.seed(123)
pca <- prcomp(t(assay(ptgse)))
pct_var <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)

pca_df <- data.frame(
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  treatment = sample_info$treatment,
  patient = sample_info$patient,
  sample = sample_info$shortname
)

ggplot(pca_df, aes(x = PC1, y = PC2, color = treatment, shape = patient)) +
  geom_point(size = 5) +
  geom_text_repel(aes(label = sample), size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c("Control" = col_untrt, "DPN" = col_DPN, "OHT"= col_OHT)) +
  labs(title = "PCA of variance-stabilized counts",
       subtitle = "Visualizing dominant sources of sample-level variation",
       x = paste0("PC1 (", pct_var[1], "% variance)"),
       y = paste0("PC2 (", pct_var[2], "% variance)"),
       color = "treatment", shape = "patient") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))