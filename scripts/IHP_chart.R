
# plot ts
hpiChart <- ggplot(data = dfCCJoint, aes(x = date)) + 
  geom_line(mapping = aes(y = obs, color = cc)) +
  geom_ribbon(data = dfCC, aes(ymin = quant1, ymax = quant3, fill = "Country sample"), alpha = 0.2) + #, color = "blue"
  labs(title = "Nominal housing price index", x = "Date", y = "Index") +
  scale_color_manual(name = "Country/region", values = c("US" = "red", "Aggregate_fix" = "black", "Aggregate_dyn" = "grey")) + 
  scale_fill_manual(name = "Interquartile range", values = c("blue")) + 
  theme_light() 
print(hpiChart)

# plot ts yoy%
hpiChartYoY <- ggplot(data = dfCCJoint, aes(x = date)) + 
  geom_line(mapping = aes(y = yoy, color = cc)) +
  labs(title = "Nominal housing price index (year-over-year %)", x = "Date", y = "Index") +
  scale_color_manual(name = "Country/region", values = c("US" = "red", "Aggregate_fix" = "black", "Aggregate_dyn" = "grey")) + 
  theme_light() 
print(hpiChartYoY)

# plot ts qoq%
hpiChartQoQ <- ggplot(data = dfCCJoint, aes(x = date)) + 
  geom_line(mapping = aes(y = qoq, color = cc)) +
  labs(title = "Nominal housing price index (quarter-over-quarter %)", x = "Date", y = "Index") +
  scale_color_manual(name = "Country/region", values = c("US" = "red", "Aggregate_fix" = "black", "Aggregate_dyn" = "grey")) + 
  theme_light() 
print(hpiChartQoQ)


#===================================================================================================================================

# save plots
png(paste0(WD, "/charts/", "LineChart_HPI.png"), width = 12, height = 3, units = 'in', res = 300) 
print(hpiChart)
dev.off()

png(paste0(WD, "/charts/", "LineChart_yoy_HPI.png"), width = 12, height = 3, units = 'in', res = 300) 
print(hpiChartYoY)
dev.off()

png(paste0(WD, "/charts/", "LineChart_qoq_HPI.png"), width = 12, height = 3, units = 'in', res = 300) 
print(hpiChartQoQ)
dev.off()

png(paste0(WD, "/charts/", "LineChart_modelfitted_HPI.png"), width = 8, height = 6, units = 'in', res = 300) 
start <- length(dfCC$date):length(dfCC$date)-100  # last 20 years of data
end <- length(dfCC$date)
plot(dfCC$date[start:end], dfCC$qoq[start:end], type = "n", #xlim = c(1, 10), ylim = c(0, 10) 
     xlab="quarters", ylab = "qoq%", main = "US house prices")
lines(dfCC$date[start:end], dfCC$qoq[start:end], lwd=2, col = "black")
lines(dfCC$date[start:end], dfCC$ar_fitted[start:end], lwd=2, col = "blue")
legend("topright", legend = c("Actual", "Predicted"), 
       col = c("black", "blue"), lty=1)
dev.off()

