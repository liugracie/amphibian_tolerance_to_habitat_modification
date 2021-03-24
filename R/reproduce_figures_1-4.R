# Script to reproduce Figures 1-4.

library(tidyverse)
library(cowplot)

# Load file with each species' modification scores.
species_mod_scores <- 
  read.csv("Data/species_summarised_modification_scores.csv")

# FIGURE 1. Density plots. -----

## Load and filter datasets to a subset of 6 species.
species_mod_scores_subset <- 
  species_mod_scores %>% 
  filter(species == "Limnodynastes peronii" | 
           species == "Crinia signifera" | 
           species == "Litoria peronii" | 
           species == "Uperoleia fusca" | 
           species == "Litoria rothii" | 
           species == "Crinia remota") %>% 
  droplevels() # drop unused levels

species_dat_to_plot <- 
  read.csv("Data/species_obs_modification_scores_raw_fig1.csv")

range_dat_to_plot <- 
  read.csv("Data/in_range_obs_fig1.csv") %>% 
  rename(species = which_species_range)

## Make the density plot.

### specify some visuals
paneldesign <- 
  theme(strip.background = element_rect(fill="grey90"), 
        panel.border = element_rect(colour="black", fill=NA), 
        panel.spacing = unit(0.5, "lines"))

### override the legend position in theme_classic()
theme_new <- 
  function () {
    theme_classic() %+replace% 
      theme(legend.position = "bottom", 
            legend.direction = "vertical")
  }

species_mod_scores_subset %>% 
  mutate(species = fct_reorder(species, species_mod_scores_subset$median.ghm.1km)) %>%
  ggplot() + 
  geom_density(data = range_dat_to_plot,
               mapping = aes(x = zhuman_mod.1km, y = stat(density)), 
               colour = "grey", fill="NA", alpha=0.2) +     
  geom_density(data = species_dat_to_plot, 
               mapping = aes(x = species_dat_to_plot$zhuman_mod.1km, y = stat(density)), 
               colour = "mediumblue", fill = "mediumblue", alpha=0.2) + 
  geom_vline(data = species_mod_scores_subset, 
             aes(xintercept = species_mod_scores_subset$median.ghm.1km, 
                 colour = "Species modification score", 
                 linetype = "Species modification score")) + 
  geom_vline(data = species_mod_scores_subset, 
             aes(xintercept = species_mod_scores_subset$overall.median.ghm.1km, 
                 colour = "Overall modification score", 
                 linetype = "Overall modification score")) + 
  geom_vline(data = species_mod_scores_subset, 
             aes(xintercept = species_mod_scores_subset$range.median.ghm.1km, 
                 colour = "Range modification score", 
                 linetype = "Range modification score")) + 
  scale_colour_manual(name = "Modification score", 
                      values = c("Species modification score"="red", 
                                 "Range modification score"="gold", 
                                 "Overall modification score"="black")) + 
  scale_linetype_manual(name = "Modification score", 
                        values = c("Species modification score"="solid", 
                                   "Range modification score"="solid", 
                                   "Overall modification score"="dotted")) +
  facet_wrap(~species) + 
  theme_classic() + 
  guides(colour = guide_legend(reverse=TRUE), 
         linetype = guide_legend(reverse=TRUE)) +
  paneldesign +
  labs(x="Global human modification index", y="Density") +
  scale_x_continuous(expand=c(0,0), breaks = seq(0.2,1,0.2), limits=c(0,1)) +
  scale_y_continuous(expand=c(0,0), breaks = seq(0,5,1), limits=c(0,5)) + 
  theme(strip.text = element_text(face="italic")) +
  theme(legend.position = "none")


# FIGURE 2. Histogram of modification index (i.e. relative GHM). -----

species_mod_scores %>% 
  ggplot() + 
  geom_histogram(mapping=aes(relative.ghm.1km, y = stat(count)),
                 alpha=0.4, colour="mediumblue", fill="mediumblue",
                 binwidth=0.025) + 
  theme_classic() + 
  labs(x="Modification index",y="Number of species") +
  geom_vline(aes(xintercept=median(species_mod_scores$relative.ghm.1km)),
             colour="black", linetype="dotted") +
  scale_y_continuous(limits=c(0,18), breaks=seq(0,18,2), expand=c(0,0)) +
  scale_x_continuous(limits=c(-0.7,0.3), breaks=seq(-0.8,0.4,0.1)) +
  annotate("segment", 
           x=c(-6.506317e-01, -3.752524e-01, -1.492802e-01, 9.449154e-02, 2.281123e-01), 
           #	P. guentheri, A. fryi, L. verreauxii, L.caerulea, Lim.peronii, 
           xend=c(-6.506317e-01, -3.752524e-01, -1.492802e-01, 9.449154e-02, 2.281123e-01), 
           y = c(11.5), 
           yend = c(9), 
           colour = "black", 
           size=0.5, arrow=arrow(angle=25, type="closed",length=unit(0.2,"cm"))) +
  annotate("text", 
           x = c(-6.506317e-01, -3.8e-01, -1.72e-01, 0.055, 2.5e-01), 
           y = 17, 
           label = c(expression(italic(P.~guentheri)), 
                     expression(italic(A.~fryi)), 
                     expression(italic(L.~verreauxii)), 
                     expression(italic(L.~caerulea)), 
                     expression(italic(Lim.~peronii))),
           color="black", size=3.5) +
  theme(panel.border = element_rect(colour="black", fill=NA))


# FIGURE 3. Plot modification index (i.e. relative GHM) ordered by species. -----

theme_classic_edits <- 
  function() {
    theme_classic() %+replace% 
      theme(
        panel.grid.major.y = element_line(colour="grey90"),
        panel.border = element_rect(fill=NA, colour="black"),
        axis.title.y = element_blank(),
        axis.text.y.left = element_text(face = "italic"),
      )}

relativeghm <- 
  species_mod_scores %>% 
  mutate(species = fct_reorder(species, relative.ghm.1km)) %>% 
  ggplot(mapping=aes(y=species, x=relative.ghm.1km)) + 
  geom_point(alpha=0.5, colour="mediumblue", size=1.5) +
  geom_vline(aes(xintercept=0), linetype="dotted") +
  labs(x = "Modification index",
       y = " ") +
  scale_x_continuous(breaks=seq(-0.8,0.3,0.1), limits=c(-0.7,0.3)) +
  theme_classic_edits() 

relativeghm


# FIGURE 4. Multipanel plot -----

analysis_dat <- 
  read_csv("Data/analysis_dat.csv")

## Plot body size, freq, clutch-size category, ecological group, clutch category.

response_vs_size <- 
  ggplot(analysis_dat, 
         aes(x = male_body_size_maximum.mm, y = response_relativeghm)) + 
  geom_point() + 
  geom_smooth(method = "lm") +  
  scale_x_log10(breaks=c(15,25,40,65,100)) + 
  scale_y_continuous(limits = c(-0.75,0.25), breaks = seq(-0.75,0.25,0.25)) +
  labs(x = "Body size (mm)", y = "Modification index") +
  theme_classic(base_size = 11) +
  theme(plot.margin = margin(10, 10, 0, 10),
        panel.border = element_rect(fill=NA))

response_vs_dfreq <- 
  ggplot(analysis_dat, aes(x = dfreq, y = response_relativeghm)) + 
  geom_point() + 
  geom_smooth(method = "lm") +  
  scale_x_log10(breaks=c(500,1000,2500,6000)) + 
  scale_y_continuous(limits=c(-0.75,0.25), breaks=seq(-0.75,0.25,0.25)) +
  labs(x = "Dominant frequency (Hz)", y = "Modification index") +
  theme_classic(base_size = 11) +
  theme(plot.margin = margin(10, 10, 0, 10),
        panel.border = element_rect(fill=NA))

response_vs_clutchsizecategory <-
  analysis_dat %>% 
  group_by(clutch_size_category) %>% 
  summarise(mean = mean(response_relativeghm)) %>% 
  ggplot() + 
  geom_boxplot(data = analysis_dat, 
               mapping=aes(x = fct_relevel(clutch_size_category, "small", "medium", "large"), 
                           y = response_relativeghm, 
                           fill = clutch_size_category)) + 
  geom_point(aes(x = clutch_size_category, y = mean), 
             colour = "red", shape = 18, size = 2) +
  scale_y_continuous(limits = c(-0.75,0.25), breaks = seq(-0.75,0.25,0.25)) +
  labs(x = "Clutch size category", y = "Modification index") +
  theme_classic(base_size = 11) + 
  theme(plot.margin = margin(0, 10, 10, 10), 
        legend.position = "none",
        panel.border = element_rect(fill=NA)) + 
  scale_fill_manual(values=c("royalblue3","slategray2","aliceblue"))

response_vs_ecolgroup <-
  analysis_dat %>% 
  group_by(ecological_group) %>% 
  summarise(mean = mean(response_relativeghm)) %>% 
  ggplot() + 
  geom_boxplot(data = analysis_dat, 
               mapping = aes(x = fct_relevel(ecological_group, "P","E/P","E","S","T"), 
                             y = response_relativeghm, 
                             fill = ecological_group)) + 
  geom_point(aes(x = ecological_group, y = mean), 
             colour="red", shape=18, size=2) +
  scale_y_continuous(limits = c(-0.75,0.25), breaks = seq(-0.75,0.25,0.25)) +
  labs(x = "Ecological group", y = "Modification index") +
  theme_classic(base_size = 11) + 
  theme(plot.margin = margin(0, 10, 10, 10), 
        legend.position = "none",
        panel.border = element_rect(fill=NA)) + 
  scale_fill_manual(values = c("skyblue3","slategray2",
                               "aliceblue","cornflowerblue","royalblue3"))

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

response_vs_clutchcategory <-
  analysis_dat %>% 
  group_by(clutch_category) %>% 
  summarise(mean = mean(response_relativeghm)) %>% 
  ggplot() + 
  geom_boxplot(data = analysis_dat, 
               mapping=aes(x = clutch_category, 
                           y = response_relativeghm, 
                           fill = clutch_category)) + 
  geom_point(aes(x = clutch_category, y = mean), 
             colour = "red", shape = 18, size = 2) +
  scale_y_continuous(limits = c(-0.75,0.25), breaks = seq(-0.75,0.25,0.25)) +
  scale_x_discrete(labels = addline_format(c("aquatic foamy", 
                                             "aquatic non-foamy", 
                                             "semi- terrestrial foamy", 
                                             "terrestrial non-foamy"))) +
  labs(x = "Clutch type", y = "Modification index") +
  theme_classic(base_size = 11) + 
  theme(plot.margin = margin(0, 10, 10, 10), 
        legend.position = "none",
        panel.border = element_rect(fill=NA)) + 
  scale_fill_manual(values = c("aliceblue","slategray2","cornflowerblue","royalblue3"))


## Assemble panels.
plot_grid(response_vs_clutchsizecategory, 
          response_vs_clutchcategory,           
          response_vs_ecolgroup, 
          response_vs_size, 
          response_vs_dfreq, 
          labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
          label_size = 14,
          align = "vh", 
          hjust = -0.35)
