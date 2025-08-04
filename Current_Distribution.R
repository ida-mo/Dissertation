# load dependencies -------------------------------------------------------


# Install necessary packages
library(tidyverse)
library(ggrepel)
library(scales)
library(dplyr)
library(ggplot2)

# Specify data path
data_path <- "cumulative_currents.csv"

# Load data
currents <- read.csv(data_path, header = F)

# Prepare data for charts
clean_data <- currents %>% 
  
  # Rearrange to long format
  pivot_longer(everything(),
               names_to = "Col",
               values_to = "Amount") %>% 
  
  # Exclude NoData values
  filter(Amount > -9999)

# histograms --------------------------------------------------------------

# Create bins with 10-point intervals and a separate "100+" bin
clean_data <- clean_data %>%
  mutate(
    Amount_bin = case_when(
      Amount <= 10 ~ "0-10",
      Amount <= 20 ~ "11-20",
      Amount <= 30 ~ "21-30",
      Amount <= 40 ~ "31-40",
      Amount <= 50 ~ "41-50",
      Amount <= 60 ~ "51-60",
      Amount <= 70 ~ "61-70",
      Amount <= 80 ~ "71-80",
      Amount <= 90 ~ "81-90",
      Amount <= 100 ~ "91-100",
      Amount > 100  ~ "100+"
    ),
    # Set as factors for plotting
    Amount_bin = factor(Amount_bin,
                        levels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", 
                                   "61-70", "71-80", "81-90", "91-100", "100+"))
  )


# Count and calculate percentages
bin_counts <- clean_data %>%
  count(Amount_bin) %>%
  mutate(percentage = n / sum(n))

# Plot as percentage bar chart with labels
ggplot(bin_counts, aes(x = Amount_bin, y = percentage)) +
  geom_col(fill = "turquoise", color = "black", alpha = 0.5) +
  geom_text(aes(label = percent(percentage, accuracy = 0.1)),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
  labs(title = "Distribution of Cumulative Currents Across Study Area",
       x = "Cumulative Current Value",
       y = "Percentage of Total") +
  theme_minimal()+
  theme(text = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave("current_distribution.png",
       dpi = 600,
       width = 23,height = 16,units="cm")

# CDF ---------------------------------------------------------------------

# Extract quantiles for layering
quantile(clean_data$Amount,c(0.25, 0.5, 0.75, 0.85, 0.95))

# Plot empirical CDF and highlight selected quantiles
clean_data %>% 
  ggplot(aes(x=Amount))+
  geom_line(stat = "ecdf")+
  geom_vline(xintercept = quantile(clean_data$Amount,c(0.25,0.5,0.75,0.85,0.95)),
             color=c("red", "red", "darkblue", "darkblue", "darkblue"),
             linewidth = c(1, 1, 1.2, 1.2, 1.2),
             linetype = c(2, 2, 1, 1, 1))+
  geom_text(x=quantile(clean_data$Amount,0.75) + 3,label="75%",
            y=0.1, colour = "darkblue")+
  geom_text(x=quantile(clean_data$Amount,0.85) + 3,label="85%",
            y=0.1, colour = "darkblue")+
  geom_text(x=quantile(clean_data$Amount,0.95) + 3,label="95%",
            y=0.1, colour = "darkblue")+
  geom_text(x=quantile(clean_data$Amount,0.5) - 5,label="Median",
            y=0.1, colour = "red")+
  geom_text(x=quantile(clean_data$Amount,0.25) - 2,label="25%",
            y=0.1, colour = "red")+
  
  # Cut axis to focus on key chart area
  scale_x_continuous(breaks = seq(0,100,10),
                     limits = c(0, 100))+
  scale_y_continuous(labels = label_percent(),
                     breaks = seq(0,1,0.2))+
  labs(title = "Distribution of Cumulative Currents Across Study Area",
       x = "Cumulative Current Value",
       y = "Percentage of Total") +
  theme_minimal()+
  theme(text = element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave("current_cdf.png",
       dpi = 600,
       width = 23,height = 16,units="cm")


