

if (!require("USgas")) {
  install.packages("USgas")
}

library(USgas)
library(fpp3)

gas <- tail(aus_production, 5*4) %>% select(Gas)


gas %>% autoplot(Gas) + labs(y="Gas production in petajoules",
                             title = "Quarterly production of in Australia")

sprintf("Considering the last 5 years of Australian gas production we observe major seasonal fluctuations and an upward trend.")


# a. Using classical_decomposition with type=multiplicative to calculate the trend-cycle and seasonal indices

gas %>% model(classical_decomposition(Gas, type = "mult")) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical multiplicative decomposition of Australian gas production")

sprintf("c ) These components confirm our earlier observations regarding an upward trend and clear seasonality.")

# b. Compute and plot the seasonally adjusted data

# STL decomposition
dcmp <- gas %>%
  model(stl = STL(Gas))

#Compute and plot the seasonally adjusted data
components(dcmp) %>%
  as_tsibble() %>%
  autoplot(Gas, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = 'blue') +
  labs(y = "Gas production",
       title = "Australian Gas Production")

sprintf("The gray line in the plot above shows our original autoplot() output while the blue line represents seasonally-adjusted data. Seasonal ajustment blunts the crests and troughs.")

# c. Outlier Effect Analysis: Change one observation to be an outlier (e.g., add 300 to one observation), and recompute the seasonally adjusted data

#change one observation to be an outlier
gas2 <- gas
gas2$Gas[10] <- gas2$Gas[10] + 300

#recompute the seasonally adjusted data

# STL decomposition
dcmp <- gas2 %>%
  model(stl = STL(Gas))

#Compute and plot the seasonally adjusted data
components(dcmp) %>%
  as_tsibble() %>%
  autoplot(Gas, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "blue") +
  labs(y = "Gas production",
       title = "Australian Gas Production")

sprintf("The outlier was very influential. It changed the level and shape of the seasonally adjusted data plot. The trend is changed just by the addition of one outlier and the seasonal adjusted plot will be altered. ")

# d. Does it make any difference if the outlier is near the end rather than in the middle of the time series?

gas3 <- gas
gas3$Gas[17] <- gas3$Gas[17] + 300

#recompute the seasonally adjusted data

# STL decomposition
dcmp <- gas3 %>%
  model(stl = STL(Gas))

#Compute and plot the seasonally adjusted data
components(dcmp) %>%
  as_tsibble() %>%
  autoplot(Gas, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "blue") +
  labs(y = "Gas production",
       title = "Australian Gas Production")

sprintf("Irrespecive of where the outlier is, the seasonal adjusted data is altered and the presence of the outlier is significant.")
