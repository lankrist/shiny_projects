#Data simulated for commodity tool interface
n = 10000
rows = 50000
days = 200
quantity = 100


commodity_order = 
data.frame(
  RO. = round(runif(rows, 1, n)),
  Destination.Country = sample(levels(countries$ADMIN), rows, replace = T),
  Product.Category = sample(c("flying", "talking", "shy"), rows, replace = T),
  Item.Description = sample(c("cat", "dog", "fish", "parrot", "alligator"), rows, replace = T),
  Status = sample(c("dead", "alive"), rows, replace = T),
  Shipped.Quantity = round(runif(rows, 1, quantity)),
  Unit.Cost = runif(rows, 1, n),
  Agreed.Delivery.Date = sample((seq(as.Date("2012-1-1"), Sys.Date(), "day")), rows, replace = T)
  )
commodity_order$Actual.Delivery.Date = commodity_order$Agreed.Delivery.Date + (round(runif(rows, -days, days)))

tier = round(runif(rows/25, 1, n))
commodity_order$Actual.Delivery.Date[tier] = NA
commodity_order$Agreed.Delivery.Date[sample(tier, rows/80)] = NA
