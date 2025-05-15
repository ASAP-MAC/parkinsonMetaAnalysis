# plot 1 constipation BluePoo 
library(tidyverse)
theme_set(theme_light())
# this is metaphalan 3
bluePoo_C4.vs.C1 <- openxlsx::read.xlsx("~/Downloads/gutjnl-2020-323877supp005.xlsx", sheet = 1) %>%
  transmute(
    Species = gsub("s__", "", str_extract(Taxonomy, "s__.*")),
    Beta.bluePoo = -Effect.size,
         FDR.bluePoo = pvalue.MWU.FDR)
  

# this is metaphlan 4
PD_meta_analysis_GA <- read_tsv("~/Documents/git_repos/parkinsonMetaAnalysis/results/asin_sqrt_PD_only.tsv") %>% 
  transmute(
    Species = gsub("s__", "", FeatureID),
    Beta.metaAnalysis = RE,
    FDR.metaAnalysis = FDR_Qvalue
  )

# DoD constipation in healthy controls
# this is metaphlan 3
haydeh_constipation_healthy <- read_tsv("~/Downloads/constipation_controls_BirminghamAL.tsv") %>% 
  filter(grepl("Constipation", metadata)) %>% 
  transmute(
    Species = gsub("s__", "", feature),
    Beta.NHC_constipated = coef,
    FDR.NHC_constipated = qval
  )

datasets_together.list <- list(
  bluePoo = bluePoo_C4.vs.C1,
  metaAnalysis = PD_meta_analysis_GA,
  NHC_constipated = haydeh_constipation_healthy
)

venn_plot_species_tested <- ggVennDiagram::ggVennDiagram(lapply(datasets_together.list, "[[", "Species"))

datasets_together.df <- reduce(datasets_together.list, full_join, by = "Species")
rownames(datasets_together.df) <- datasets_together.df$Species

scatterplot_betas_metaAnalysis.vs.bluePoo <- ggplot(datasets_together.df, aes(x = Beta.metaAnalysis, y = Beta.bluePoo)) + 
  geom_point(color = "darkgray") + 
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_hline(yintercept = 0, color = "black", lty = "dashed") + 
  geom_vline(xintercept = 0, color = "black", lty = "dashed") + 
  ggpubr::stat_cor(method = "pearson") +
  labs(
    caption= "Correlation: Pearson"
  )

scatterplot_betas_bluePoo.vs.NHC_healthy <- ggplot(datasets_together.df, aes(x = Beta.bluePoo, y = Beta.NHC_constipated)) + 
  geom_point(color = "darkgray") + 
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_hline(yintercept = 0, color = "black", lty = "dashed") + 
  geom_vline(xintercept = 0, color = "black", lty = "dashed") + 
  ggpubr::stat_cor(method = "pearson") +
  labs(
    caption= "Correlation: Pearson"
  )

scatterplot_betas_metaAnalysis.vs.NHC_healthy <- ggplot(datasets_together.df, aes(x = Beta.metaAnalysis, y = Beta.NHC_constipated)) + 
  geom_point(color = "darkgray") + 
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_hline(yintercept = 0, color = "black", lty = "dashed") + 
  geom_vline(xintercept = 0, color = "black", lty = "dashed") + 
  ggpubr::stat_cor(method = "pearson") +
  labs(
    caption= "Correlation: Pearson"
  )


plotPanel <- ggarrange(venn_plot_species_tested, scatterplot_betas_metaAnalysis.vs.bluePoo, scatterplot_betas_bluePoo.vs.NHC_healthy, scatterplot_betas_metaAnalysis.vs.NHC_healthy, labels = "AUTO")

ggsave(plot = plotPanel, filename = "~/Desktop/Constipation_vs_PD.pdf", width = 10, height = 10, units = "in")
plotPanel


#-------------------------------------------------------------------------------
# Construct null distribution to test for non-correlated values
datasets_together_testable.df <- datasets_together.df[!is.na(datasets_together.df$Beta.bluePoo) & !is.na(datasets_together.df$Beta.metaAnalysis),]

lm_bluePoo_vs_MetaAnalysis <- lm(Beta.metaAnalysis ~ Beta.bluePoo, data = datasets_together_testable.df)

summary(residuals(lm_bluePoo_vs_MetaAnalysis))


diff_beta_minus_residuals <- datasets_together_testable.df$Beta.bluePoo - residuals(lm_bluePoo_vs_MetaAnalysis)

plot(residuals(lm_bluePoo_vs_MetaAnalysis), diff_beta_minus_residuals)


plot(lm_bluePoo_vs_MetaAnalysis)


scatterplot_betas_metaAnalysis.vs.bluePoo <- ggplot(datasets_together.df, aes(x = Beta.metaAnalysis, y = Beta.bluePoo, key = Species)) + 
  geom_point(color = "darkgray") + 
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_hline(yintercept = 0, color = "black", lty = "dashed") + 
  geom_vline(xintercept = 0, color = "black", lty = "dashed") + 
  ggpubr::stat_cor(method = "pearson") +
  labs(
    caption= "Correlation: Pearson"
  )
plotly::ggplotly(scatterplot_betas_metaAnalysis.vs.bluePoo)
