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
# Writes data/station_house_prices.csv keyed on CRS.
# Coverage is England & Wales only (the source dataset excludes Scotland).
# ------------------------------------------------------------------------------

library(data.table)

panel_csv <- "E:/rail/data/london_fare_panel.csv"
onspd_csv <- "E:/rail/ONSPD_MAY_2025/Data/ONSPD_MAY_2025_UK.csv"
hpm_dir   <- "E:/rail/hpm_la_2024/hpm_la_2024"
out_csv   <- "E:/rail/data/station_house_prices.csv"
radius_km <- 2

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

## 4 - transactions within 2 km of each station ----------------------------------
# equirectangular km coordinates + 2 km grid cells; a station collects the
# 3x3 neighbourhood of cells then filters to the exact radius
kx <- 111.32 * cos(52.5 * pi / 180); ky <- 110.57
hp[, `:=`(x = plon * kx, y = plat * ky)]
hp[, `:=`(cx = floor(x / radius_km), cy = floor(y / radius_km))]
st[, `:=`(sx = slon * kx, sy = slat * ky)]

stnb <- st[rep(seq_len(.N), each = 9)]
stnb[, `:=`(cx = floor(sx / radius_km) + rep(rep(-1:1, each = 3), len = .N),
            cy = floor(sy / radius_km) + rep(rep(-1:1, times = 3), len = .N))]

m <- merge(stnb[, .(crs, sx, sy, cx, cy)],
           hp[, .(cx, cy, x, y, year, priceper, price)],
           by = c("cx", "cy"), allow.cartesian = TRUE)
m <- m[sqrt((x - sx)^2 + (y - sy)^2) <= radius_km]

res <- m[, .(
  ppm2_1618  = median(priceper[year <= 2018]),
  price_1618 = median(price[year <= 2018]),
  n_1618     = sum(year <= 2018),
  ppm2_2224  = median(priceper[year >= 2022]),
  price_2224 = median(price[year >= 2022]),
  n_2224     = sum(year >= 2022)
), by = crs]

fwrite(res[order(crs)], out_csv)
cat("Wrote", out_csv, ":", nrow(res), "stations with nearby sales.\n")
print(res[crs %chin% c("BTN", "LEI", "RDG", "WOK", "SEV", "PBO")])
