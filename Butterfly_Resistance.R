# load dependencies -------------------------------------------------------

# Install necessary packages
library(tidyverse)
library(scales)

# Specify path to data
path_butterflies <- "resistance_butterflies.csv"
path_studyarea <- "resistance_studyarea.csv"

# Load data
butterflies <- read_csv(path_butterflies)
studyarea <- read.csv(path_studyarea, header = F)

# Clean data --------------------------------------------------------------

# Clean butterfly data
butterflies_clean <- butterflies %>% 
  # add type indicator
  mutate(type="Butterfly Occurrences")

# Clean study area data
studyarea_clean <- studyarea %>% 
  
  # rearrange to long format
  pivot_longer(everything(),
               values_to = "resistance") %>% 
  # exclude NoData values
  filter(resistance > -9999) %>% 
  
  # keep required column
  select(resistance) %>% 
  
  # add type indicator
  mutate(type="Study Area")

# join datasets together for plotting
joint_data <- butterflies_clean %>% 
  bind_rows(studyarea_clean)

# histograms --------------------------------------------------------------

# plot histogram of butterfly and study area resitance densities on single plot
joint_data %>% 
  ggplot(aes(x = resistance,
             fill=type))+
  geom_density(alpha=0.5)+
  scale_fill_manual(values=c("darkblue", "turquoise"))+
  scale_x_continuous(breaks = seq(0,500,100))+
  scale_y_continuous(labels = label_percent(),
                     breaks = seq(0,0.012,0.002))+
  guides(fill=guide_legend(position = "top"))+
  labs(title = "Resistance Value Distribution Across Study Area and Butterfly Occurrences",
       x="Resistance Value",
       y="Proportion of Area",
       fill="")+
  theme_minimal()+
  theme(text = element_text(size = 14),
        panel.grid.minor = element_blank())

ggsave("butterfly_area_density.png",
       dpi = 600,
       width = 23,height = 16,units="cm")


