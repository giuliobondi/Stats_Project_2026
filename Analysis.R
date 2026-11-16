#This is the Analyisis for the Statistics 2 project by Daniel Puglisi 3355544, Giulio Bondi (giulio metti la tua matricola) and Matteo D'Anna 3344390.
# A.1 - Descriptive analysis

hotels <- read.csv("30457_hotel _2026_2027.csv")

# Check the data
nrow(hotels)
head(hotels)
str(hotels)
colSums(is.na(hotels))

# "hotel" contains the hotel names, so we use it as an identifier.


# Continent

continent_counts <- table(hotels$continent)
continent_counts

round(prop.table(continent_counts) * 100, 2)

barplot(continent_counts,
        main = "Hotels by continent",
        ylab = "Number of hotels",
        las = 2,
        cex.names = 0.7)


# International chain

chain_counts <- table(hotels$international_chain)
chain_counts

round(prop.table(chain_counts) * 100, 2)

barplot(chain_counts,
        main = "International chain",
        xlab = "Belongs to an international chain",
        ylab = "Number of hotels")


# Stars

stars_counts <- table(hotels$stars)
stars_counts

round(prop.table(stars_counts) * 100, 2)

# Quartiles and median
quantile(hotels$stars, probs = c(0.25, 0.50, 0.75), type = 1)

barplot(stars_counts,
        main = "Hotel star ratings",
        xlab = "Stars",
        ylab = "Number of hotels")


# Rooms

# Summary gives the minimum, quartiles, median, mean and maximum.
# sd gives the standard deviation.
summary(hotels$rooms)
sd(hotels$rooms)

hist(hotels$rooms,
     breaks = 16,
     main = "Number of rooms",
     xlab = "Rooms")

boxplot(hotels$rooms,
        main = "Number of rooms",
        ylab = "Rooms")


# Restaurants

restaurants_counts <- table(hotels$restaurants)
restaurants_counts

round(prop.table(restaurants_counts) * 100, 2)

summary(hotels$restaurants)
sd(hotels$restaurants)

barplot(restaurants_counts,
        main = "Restaurants per hotel",
        xlab = "Number of restaurants",
        ylab = "Number of hotels")


# Swimming pool

pool_counts <- table(hotels$pool)
pool_counts

round(prop.table(pool_counts) * 100, 2)

barplot(pool_counts,
        main = "Swimming pool",
        xlab = "Has a swimming pool",
        ylab = "Number of hotels")


# Spa

spa_counts <- table(hotels$spa)
spa_counts

round(prop.table(spa_counts) * 100, 2)

barplot(spa_counts,
        main = "Spa",
        xlab = "Has a spa",
        ylab = "Number of hotels")


# Annual revenue

summary(hotels$annual_revenue_USD)
sd(hotels$annual_revenue_USD)

hist(hotels$annual_revenue_USD,
     breaks = 30,
     main = "Annual revenue",
     xlab = "Revenue (USD)")

boxplot(hotels$annual_revenue_USD,
        main = "Annual revenue",
        ylab = "Revenue (USD)")


# Profitability

summary(hotels$profitability_percent)
sd(hotels$profitability_percent)

hist(hotels$profitability_percent,
     breaks = 30,
     main = "Hotel profitability",
     xlab = "Profitability (%)")

boxplot(hotels$profitability_percent,
        main = "Hotel profitability",
        ylab = "Profitability (%)")


# Employees

summary(hotels$employees)
sd(hotels$employees)

hist(hotels$employees,
     breaks = 30,
     main = "Number of employees",
     xlab = "Employees")

boxplot(hotels$employees,
        main = "Number of employees",
        ylab = "Employees")

#Extra: check pools and spas by star rating

# Calculate the percentage with each amenity within each star category.
# tapply groups hotels by stars.
# Taking the mean of TRUE/FALSE gives the proportion of "Yes".

pool_by_stars <- tapply(hotels$pool == "Yes", hotels$stars, mean) * 100
spa_by_stars <- tapply(hotels$spa == "Yes", hotels$stars, mean) * 100

# Summary table
amenities_by_stars <- data.frame(
  Stars = names(pool_by_stars),
  Hotels = as.vector(table(hotels$stars)),
  Pool_percent = round(as.vector(pool_by_stars), 2),
  Spa_percent = round(as.vector(spa_by_stars), 2)
)

print(amenities_by_stars)

# Put the percentages together for the chart
amenities_plot <- rbind(pool_by_stars, spa_by_stars)

# beside = TRUE places the two bars next to each other
barplot(amenities_plot,
        beside = TRUE,
        names.arg = names(pool_by_stars),
        col = c("steelblue", "tan2"),
        ylim = c(0, 100),
        main = "Swimming pools and spas by star rating",
        xlab = "Stars",
        ylab = "Hotels within each star category (%)",
        legend.text = c("Pool", "Spa"),
        args.legend = list(x = "topleft", bty = "n"))

# Both pools and spas become more common as star ratings increase.
# Pools increase from 16.67% for one-star hotels to 66.16% for five-star hotels.
# Spas increase from 21.43% to 53.03%.
# These results show an association in our sample, not proof of causation.