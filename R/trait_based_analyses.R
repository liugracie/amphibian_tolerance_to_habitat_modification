# Modelling script

# Load packages -----

library(tidyverse)
library(arm)
library(corrplot)
library(ape)
library(phylobase)
library(phylosignal)
library(ggtree)
library(broom)
library(MuMIn)
library(phylolm)
library(car)

# Load data -----

analysis_dat <- 
  read_csv(file = "Data/analysis_dat.csv")

## Remove Heleioporus eyrei, 
## the only species in the semi-terrestrial foamy clutch category,
## before modelling.
analysis_dat <- 
  analysis_dat %>% 
  filter(species != "Heleioporus eyrei")

## Select variables and filter data to complete cases.

### first, rescale the predictors 
data_rescaled <- 
  analysis_dat %>% 
  transmute(species,
            z.log_body_size = log10(male_body_size_maximum.mm), 
            clutch_category,
            clutch_size_category,
            ecological_group,
            male_calling_position.vegetation,
            z.log_range_size = log10(range_size.km2),
            no_of_tadpole_positions, 
            no_of_climate_zones,
            no_of_habitats,
            no_of_calling_positions,
            z.log_dfreq = log10(dfreq)
  ) %>% 
  filter(complete.cases(.)) %>%   
  mutate_if(is.numeric, rescale) #rescale numeric (and binary) variables

### then, add on the (unscaled) response variables 
data <- 
  data_rescaled %>% 
  left_join(analysis_dat %>% 
              dplyr::select(species, 
                            response_medianghm, response_relativeghm, 
                            response_medianviirs, response_relativeviirs,
                            N), 
            by = "species") %>%
  mutate(weights = ifelse(N>1000, 1000, N))

# Look at the corrplot -----

data_numeric <- 
  data %>% 
  select_if(is.numeric) %>% 
  dplyr::select(-response_medianghm, -response_relativeghm,
                -response_medianviirs, -response_relativeviirs) 

detach("package:arm", unload = TRUE) #corrplot doesn't work with arm package loaded

res1 <- cor.mtest(data_numeric)

corrplot(cor(data_numeric), 
         p.mat = res1$p, 
         insig = "blank", sig.level = 0.05,
         addCoef.col = T, number.cex = 0.5, addgrid.col = T,
         tl.srt = 45, tl.cex = 0.5, tl.col = "black",
         method = "color", type = "lower")

## Corrplot of morphological traits (supplementary material).

morph_matrix <- 
  analysis_dat %>% 
  transmute(SVL_m = male_body_size_maximum.mm, 
            SVL_f = female_body_size_maximum.mm, 
            head_l = Head_l,
            head_w = Head_width,
            eye_naris = Eye_naris,
            interorbital = Interorbital,
            internarial = Internarial,
            naris_snout = Naris_Snout,
            eye_l = Eye_l,
            mouth_w = Mouth_width,
            humerus_l = Humerus_l,
            forearm_l = Forearm_l, 
            wrist_w = Wrist_width,
            hand_l = Hand_l,
            thumb = Thumb, 
            finger_4 = Finger_4,
            femur_l = Femur_l,
            femur_w = Femur_width,
            tibial_l = Tibial_l,
            tibial_w = Tibial_width,
            foot_l_toe1 = Foot_l_toe_1,
            foot_l_total = Foot_l_total, 
            toe_1 = Toe_1, 
            toe_5 = Toe_5) %>%
  filter(complete.cases(.))

corrplot(cor(morph_matrix), 
         p.mat = cor.mtest(morph_matrix)$p, 
         insig = "blank", sig.level = 0.05,
         addCoef.col = "white", addCoefasPercent = T, 
         number.cex = 0.7, addgrid.col = T,
         tl.srt = 45, tl.cex = 0.9, tl.col = "black",
         method = "color", type = "lower")

# =======================================================
# Models (relative GHM is the response variable) ####
# =======================================================
# Load and update phylogenetic tree. -----

## Make a list of species in our dataset.
tree <- ape::read.tree(file="Data/aus_phylo_tree.tre")

our_species <- analysis_dat %>%
  mutate(species=gsub(" ", "_", .$species)) %>%
  .$species

our_sp_list <- data.frame(species = our_species)

## Make a list of species in the phylo tree.
spp_in_tree <- data.frame(species = tree[["tip.label"]])

## Filter the tree list to species not in our dataset. 
no_data_sp <- tree$tip.label[!tree$tip.label %in% our_species]

## Drop the species in the tree that are not in our dataset so that 
## you're left with species in the tree that are also in our list.
our_tree <- drop.tip(tree, no_data_sp)
our_tree_sp <- data.frame(species = our_tree[["tip.label"]]) %>% arrange(.$species)

## View species that are in our list that are not in our tree. 
View(anti_join(our_sp_list, our_tree_sp)) 

## NOTE that there are mismatches between the tree and our taxonomy:
## Litoria verreauxii is labelled as Cyclorana verreauxii in the tree.
## Litoria nudidigitus is labelled as Litoria_nudidigita in the tree.

## Rename labels of tree tips.
treetips <- 
  data.frame(tree=as.character(tree$tip.label)) %>% 
  mutate(tree=as.character(as.factor(tree)))

treetips2 <- 
  treetips %>% 
  mutate(tree2=ifelse(tree=="Cyclorana_verreauxii", "Litoria_verreauxii", tree)) %>%
  mutate(tree3=ifelse(tree=="Litoria_nudidigita", "Litoria_nudidigitus", tree2)) %>%
  mutate(tree4=ifelse(tree=="Litoria_verrucosa", "Cyclorana_verrucosa", tree3))

tree$tip.label <- treetips2$tree4

tree$tip.label

names(tree)

# Phylogenetic signal analysis. -----

our_species <- data %>%
  mutate(species = gsub(" ", "_", .$species)) %>% 
  .$species

no_data_sp <- tree$tip.label[!tree$tip.label %in% our_species]

our_tree <- drop.tip(tree, no_data_sp)

data2 <- data %>%
  mutate(species2 = gsub(" ", "_", .$species)) %>%   
  column_to_rownames(var="species2")

data3 <- data2 %>%
  dplyr::select(response_relativeghm)

p4d <- phylo4d(our_tree, data3) #create phylobase object

ps <- phyloSignal(p4d, reps = 9999) #run calculation

stats <- ps$stat %>%
  rownames_to_column(var="term") %>%
  mutate(value="Statistic")

p_values <- ps$pvalue %>%
  rownames_to_column(var="term") %>%
  mutate(value="P value")

phy_signal_GHMrelative <- bind_rows(stats, p_values) %>% 
  mutate_at(2:6, round, 3)

## Looks like 'mixed' results for phylogenetic signal, so we will perform both non-phylo
## and phylo models.

## Plot on a tree.
ggtree(p4d, layout='circular', aes(color=response_relativeghm), 
       ladderize = FALSE, size=1) +
  scale_colour_viridis_c(option="B", begin = 0, end=0.9, name="Modification index") + 
  geom_tiplab(aes(angle=angle), size=3) +
  theme(legend.position = c(.05, .85)) + xlim(0,250)


# Non-phylogenetic model. -----

## Fit a linear global model.

lm.mod_relativeGHM <- lm(response_relativeghm ~ 
                           z.log_body_size +
                           clutch_category +
                           clutch_size_category +
                           ecological_group +
                           male_calling_position.vegetation +
                           z.log_range_size +
                           no_of_tadpole_positions +
                           no_of_climate_zones +
                           no_of_habitats +
                           no_of_calling_positions +
                           z.log_dfreq,
                         data = data, weights=weights, na.action = "na.fail")

summary(lm.mod_relativeGHM)
Anova(lm.mod_relativeGHM)

## Summarise results of global linear model.
result_GHMrelative <- tidy(lm.mod_relativeGHM) %>%
  mutate(lwr_95_confint=confint(lm.mod_relativeGHM)[,1]) %>%
  mutate(upr_95_confint=confint(lm.mod_relativeGHM)[,2]) %>%
  mutate(significance=ifelse(p.value <=0.05, "Significant", "Non-significant")) %>%
  mutate(trend=ifelse(.$estimate >0, "positive", "negative")) %>%
  mutate(model_type="linear_global_model") %>% 
  mutate_at(2:7, round, 3)

## Get a dataframe of variance inflation factors.
vif_GHMrelative <- as.data.frame(vif(lm.mod_relativeGHM)) %>%
  rownames_to_column(var="term") %>%
  rename(adjusted_GVIF=4) %>%
  mutate(adjusted_GVIF=round(adjusted_GVIF, digits=2)) %>% 
  mutate_at(2, round, 2)

## Do some model averaging.

dredged_model_lm <- dredge(lm.mod_relativeGHM)

### select all models with deltaAic < 4
top.models_lm <- get.models(dredged_model_lm, subset=delta<4) 

### how many top models?
length(top.models_lm)

### ranks these models based on AICc
my.models_lm <- model.sel(top.models_lm, rank="AICc") 

### do the model averaging part of the analysis
averaged_models_lm <- model.avg(my.models_lm)

summary(averaged_models_lm)

## 'Best' model.
summary(get.models(dredged_model_lm, 1)[[1]])

## Summarize results of model averaging results.
model_results_lm <- as.data.frame(summary(averaged_models_lm)$coefmat.subset) %>%
  cbind(confint(averaged_models_lm, full=TRUE)) %>%
  rownames_to_column(var="term") %>%
  full_join(., data.frame(importance=averaged_models_lm$sw) %>%
              rownames_to_column(var="term"), by="term") %>%
  rename(estimate=Estimate) %>%
  rename(std.error=`Std. Error`) %>%
  rename(z_value=`z value`) %>%
  rename(p.value=`Pr(>|z|)`) %>%
  rename(lwr_95_confint=`2.5 %`) %>%
  rename(upr_95_confint=`97.5 %`) %>%
  mutate(significance=ifelse(p.value <=0.05, "Significant", "Non-significant")) %>%
  mutate(trend=ifelse(.$estimate >0, "positive", "negative")) %>%
  dplyr::select(-`Adjusted SE`)

## Get sum of weights.
summary(averaged_models_lm)$sw 


# Phylogenetic model. -----

phy_mod_relativeGHM <- 
  phylolm(response_relativeghm ~ 
            z.log_body_size +
            clutch_category +
            clutch_size_category +
            ecological_group +
            male_calling_position.vegetation +
            z.log_range_size +
            no_of_tadpole_positions + 
            no_of_climate_zones +
            no_of_habitats +
            no_of_calling_positions +
            z.log_dfreq,
          data=data2, phy=our_tree, weights=weights, na.action="na.fail")

summary(phy_mod_relativeGHM)

result_phy_GHMrelative <- 
  as.data.frame(summary(phy_mod_relativeGHM)$coefficients) %>%
  cbind(confint(phy_mod_relativeGHM)) %>%
  rownames_to_column(var="term") %>%
  rename(estimate=Estimate) %>%
  rename(std.error=StdErr) %>%
  rename(lwr_95_confint=`2.5 %`) %>%
  rename(upr_95_confint=`97.5 %`) %>%
  mutate(significance=ifelse(p.value <=0.05, "Significant", "Non-significant")) %>%
  mutate(trend=ifelse(.$estimate >0, "positive", "negative")) %>%
  mutate(model_type="phylo_global_mod") %>% 
  mutate_at(2:7, round, 3)

## Do some model averaging.
dredged_model_phy <- dredge(phy_mod_relativeGHM)

### selects all models with deltaAic < 4
top.models_phy <- get.models(dredged_model_phy, subset=delta<4) 

### how many top models?
length(top.models_phy)

### ranks these models based on AICc
my.models_phy <- model.sel(top.models_phy, rank="AICc") 

### do the model averaging part of the analysis
averaged_models_phy <- model.avg(my.models_phy)

summary(averaged_models_phy)

## 'Best' model.
summary(get.models(dredged_model_phy, 1)[[1]])

## Summarize results of model averaging results.
model_results_phy <- as.data.frame(summary(averaged_models_phy)$coefmat.subset) %>%
  cbind(confint(averaged_models_phy, full=TRUE)) %>%
  rownames_to_column(var="term") %>%
  full_join(., data.frame(importance=averaged_models_phy$sw) %>%
              rownames_to_column(var="term"), by="term") %>%
  rename(estimate=Estimate) %>%
  rename(std.error=`Std. Error`) %>%
  rename(z_value=`z value`) %>%
  rename(p.value=`Pr(>|z|)`) %>%
  rename(lwr_95_confint=`2.5 %`) %>%
  rename(upr_95_confint=`97.5 %`) %>%
  mutate(significance=ifelse(p.value <=0.05, "Significant", "Non-significant")) %>%
  mutate(trend=ifelse(.$estimate >0, "positive", "negative"))

summary(averaged_models_phy)$sw 

# Now do some individual linear model tests. -----

## Get a list of predictors.
mypredictors <- list("z.log_body_size", 
                     "clutch_category", 
                     "clutch_size_category", 
                     "ecological_group",
                     "male_calling_position.vegetation", 
                     "z.log_range_size", 
                     "no_of_tadpole_positions", 
                     "no_of_climate_zones", 
                     "no_of_habitats", 
                     "no_of_calling_positions", 
                     "z.log_dfreq")

## Function for lms. 
lm_result <- 
  function(trait) {
    formula <- as.formula(paste("response_relativeghm ~ ", paste(trait)))
    lm(formula, 
       data = data, weights=weights)
  }

## Function for phylolms.
phylolm_result <- 
  function(trait) {
    formula <- as.formula(paste("response_relativeghm ~ ", paste(trait)))
    phylolm(formula,
            data = data2, phy=our_tree, weights=weights)
  }

## Get all coefficients (but not the intercept term).
get_coefficients <- 
  function(mymodel) {
    as.data.frame(summary(mymodel)$coefficients) %>% 
      rownames_to_column(var="term") %>% 
      dplyr::filter(term != "(Intercept)")
  }

## Summarise results in a table.
all_lm_results <- 
  mypredictors %>% 
  map(lm_result) %>% 
  map(get_coefficients) %>% 
  bind_rows()

all_phylolm_results <- 
  mypredictors %>% 
  map(phylolm_result) %>% 
  map(get_coefficients) %>% 
  bind_rows()

indiv_results <- 
  all_lm_results %>% 
  left_join(all_phylolm_results, by="term")

# Join all results in a table. -----

round_2decimals <- function(x){
  round(x = x, digits=2)
}
round_3decimals <- function(x){
  round(x = x, digits=3)
}

edit_predictor_names <- 
  function(data) {
    data$term %>%  
      gsub("ecological_groupT", "Ecological group: terrestrial", .) %>% 
      gsub("ecological_groupS", "Ecological group: stream-associated", .) %>% 
      gsub("ecological_groupE/P", "Ecological group: ephemeral/permanent water", .) %>% 
      gsub("ecological_groupP", "Ecological group: permanent water", .) %>% 
      gsub("no_of_habitats", "Habitat generalism", .) %>% 
      gsub("male_calling_position.vegetation", "Calls from vegetation", .) %>% 
      gsub("no_of_climate_zones", "Climate generalism", .) %>% 
      gsub("no_of_tadpole_positions", "Tadpole generalism", .) %>% 
      gsub("no_of_calling_positions", "Number of calling positions", .) %>% 
      gsub("z.log_body_size", "log(Body size)", .) %>% 
      gsub("z.log_dfreq", "log(Dominant frequency)", .) %>% 
      gsub("z.log_range_size", "log(Range size)", .) %>% 
      gsub("clutch_size_category", "Clutch size: ", .) %>%
      gsub("clutch_category", "Clutch type: ", .)
  }

result_table <- 
  model_results_lm %>% 
  dplyr::select(term, estimate:p.value, importance) %>% 
  filter(term != "(Intercept)") %>% 
  left_join(model_results_phy %>% dplyr::select(term, estimate:p.value, importance), 
            by="term") %>%  
  left_join(indiv_results, by="term") %>% 
  mutate_if(is.numeric, round_3decimals) %>% 
  mutate_at(vars(-"term", -"p.value.x", -"p.value.y", -`Pr(>|t|)`, -"p.value"), 
            round_2decimals) %>% #round everything but p-values to 2 d.p.
  arrange(match(term, c("z.log_body_size",
                        "clutch_categoryaquatic non-foamy",
                        "clutch_categoryterrestrial non-foamy", 
                        "clutch_size_categorysmall",  
                        "clutch_size_categorymedium", 
                        "ecological_groupE/P",
                        "ecological_groupP",
                        "ecological_groupS",
                        "ecological_groupT",
                        "male_calling_position.vegetation", 
                        "z.log_range_size", 
                        "no_of_tadpole_positions", 
                        "no_of_climate_zones", 
                        "no_of_habitats", 
                        "no_of_calling_positions",	
                        "z.log_dfreq"))) %>% 
  mutate(term = edit_predictor_names(.))


# Plot parameter estimates (FIGURE 5). -----

model_results_phy %>%
  mutate(mod="phy") %>%
  bind_rows(model_results_lm %>%
              mutate(mod="lm")) %>%
  dplyr::select(-importance) %>% 
  filter(!term %in% c("(Intercept)", "clutch_size_category", 
                      "ecological_group", "clutch_category")) %>% 
  mutate(term = edit_predictor_names(.), #relabel the term names
         term = fct_reorder(term, estimate)) %>% #order by parameter estimates
  ggplot(., aes(x=term, y=estimate, color=mod, shape=significance))+
  geom_hline(yintercept=0, linetype="dotted") +  
  geom_point(position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), 
                position = position_dodge(width=1), 
                width = 0.5) +
  scale_color_manual(name="Model", 
                     labels = c("Non-phylo model-averaged", "Phylo model-averaged"), 
                     values = c("grey10", "royalblue")) +
  scale_shape_manual(values = c(16,8)) +
  scale_y_continuous(breaks = seq(-0.5,0.3,0.1)) + 
  scale_x_discrete(expand = c(0,0)) +
  labs(x = NULL, y = "Parameter estimate") +
  theme_bw(base_size = 11) +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(color = "black"), 
        legend.position = c(0.3,0.85), 
        legend.background = element_rect(color = "black", size=0.3), 
        legend.title.align = 0.5) +
  coord_flip() +
  geom_vline(xintercept=seq(0.5,16.5,1), colour="grey80", alpha=0.5) + 
  guides(shape = FALSE) 
