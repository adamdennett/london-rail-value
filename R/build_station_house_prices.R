# build_station_house_prices.R ------------------------------------------------
# Median house price per square metre near every GB station with a London
# fare, from the property-level "house price per square metre" dataset
# (Land Registry price paid x EPC floor area; London Datastore, hpm_la_2024,
# https://data.london.gov.uk/dataset/house-price-per-square-metre-in-england-and-wales-epo9w)
# geocoded via the ONS Postcode Directory.
#
# For each station: all residential transactions within 2 km, in two windows:
#   1618 = 2016-2018  (consistent with the May 2018 RJFAF851 fares epoch)
#   2224 = 2022-2024  (the market today)
# Rural stations with fewer than `min_sales` 2016-18 sales within 2 km have
# the radius widened in 1 km steps up to `max_radius_km`; the radius used is
# recorded in `radius_km`.
# Writes data/station_house_prices.csv keyed on CRS.
# Coverage is England & Wales only (the source dataset excludes Scotland).
# ------------------------------------------------------------------------------

library(data.table)

panel_csv <- "E:/rail/data/london_fare_panel.csv"
onspd_csv <- "E:/rail/ONSPD_MAY_2025/Data/ONSPD_MAY_2025_UK.csv"
hpm_dir   <- "E:/rail/hpm_la_2024/hpm_la_2024"
out_csv   <- "E:/rail/data/station_house_prices.csv"
radius_km     <- 2
max_radius_km <- 5
min_sales     <- 50

std_pc <- function(x) sub("([0-9][A-Z]{2})$", " \\1", gsub(" ", "", toupper(x)))

## 1 - stations ----------------------------------------------------------------
st <- fread(panel_csv)[!is.na(lat), .(crs, station, slat = lat, slon = lon)]

## 2 - postcode coordinates (terminated postcodes retained - old sales use them)
onspd <- fread(onspd_csv, select = c("pcds", "lat", "long"))
setnames(onspd, c("pc", "plat", "plon"))
onspd <- onspd[!is.na(plat) & plat < 90]          # 99.999999 = no grid ref
onspd[, pc := std_pc(pc)]
onspd <- unique(onspd, by = "pc")

## 3 - transactions in the two windows ------------------------------------------
files <- list.files(hpm_dir, pattern = "\\.csv$", full.names = TRUE)
hp <- rbindlist(lapply(files, function(f)
  fread(f, select = c("priceper", "year", "price", "postcode"),
        showProgress = FALSE)[year %in% c(2016:2018, 2022:2024)]))
hp <- hp[!is.na(priceper) & priceper > 0 & postcode != ""]
hp[, price := as.numeric(price)]   # keep median() type-stable across groups
hp[, pc := std_pc(postcode)]
hp <- merge(hp, onspd, by = "pc")
cat("geocoded transactions:", nrow(hp), "\n")

## 4 - transactions near each station --------------------------------------------
# equirectangular km coordinates + grid cells of side r; a station collects
# the 3x3 neighbourhood of cells then filters to the exact radius
kx <- 111.32 * cos(52.5 * pi / 180); ky <- 110.57
hp[, `:=`(x = plon * kx, y = plat * ky)]
st[, `:=`(sx = slon * kx, sy = slat * ky)]

pairs_within <- function(stations, r) {
  h <- hp[, .(cx = floor(x / r), cy = floor(y / r), x, y, year, priceper, price)]
  nb <- stations[rep(seq_len(.N), each = 9), .(crs, sx, sy)]
  nb[, `:=`(cx = floor(sx / r) + rep(rep(-1:1, each = 3), len = .N),
            cy = floor(sy / r) + rep(rep(-1:1, times = 3), len = .N))]
  m <- merge(nb, h, by = c("cx", "cy"), allow.cartesian = TRUE)
  m[, d := sqrt((x - sx)^2 + (y - sy)^2)][d <= r]
}

summarise <- function(m) m[, .(
  ppm2_1618  = median(priceper[year <= 2018]),
  price_1618 = median(price[year <= 2018]),
  n_1618     = sum(year <= 2018),
  ppm2_2224  = median(priceper[year >= 2022]),
  price_2224 = median(price[year >= 2022]),
  n_2224     = sum(year >= 2022)
), by = crs]

res <- summarise(pairs_within(st, radius_km))[, radius_km := radius_km]

# Thin (mostly rural) stations: widen 1 km at a time until min_sales is met.
# Done as a second pass over just those stations, because a 5 km pass over
# every station would put each inner-London sale in dozens of catchments.
thin <- st[!crs %chin% res[n_1618 >= min_sales, crs]]
if (nrow(thin)) {
  wide <- pairs_within(thin, max_radius_km)
  pick <- rbindlist(lapply((radius_km + 1):max_radius_km, function(r)
    wide[d <= r, .(n = sum(year <= 2018), r = r), by = crs]))
  pick <- pick[order(crs, r)][, .SD[n >= min_sales | r == max_radius_km][1], by = crs]
  wide <- merge(wide, pick[, .(crs, r)], by = "crs")[d <= r]
  res <- rbind(res[!crs %chin% pick$crs],
               merge(summarise(wide), pick[, .(crs, radius_km = r)], by = "crs"))
}
cat("radius used:\n"); print(res[, .N, by = radius_km][order(radius_km)])

fwrite(res[order(crs)], out_csv)
cat("Wrote", out_csv, ":", nrow(res), "stations with nearby sales.\n")
print(res[crs %chin% c("BTN", "LEI", "RDG", "ERI", "HEV", "CWN", "GLY", "WRB")])
