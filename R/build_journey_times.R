# build_journey_times.R ------------------------------------------------------
# Fastest scheduled DIRECT journey from every GB station to a central London
# terminal, computed straight from a CIF timetable file, written to
# data/journey_times.csv (crs, minutes, terminal). london-rail-value.qmd
# picks that file up automatically in place of its distance-based estimates.
#
# Timetable used here: "May18 Published Archive CIF.gz" from the NWR
# Historical Timetable Data product on the Rail Data Marketplace
# (https://raildata.org.uk/dataProducts) - valid 20/05/2018-08/12/2018,
# matching the RJFAF851 fares feed. Any CIF full extract works, e.g. the
# RDG "Timetable - Full Refresh" product for current data.
# (Alternative route not used here: UK2GTFS::nr2gtfs()/atoc2gtfs() converts
# these files to GTFS - https://itsleeds.github.io/UK2GTFS/articles/ATOC.html)
#
# CIF fixed-width offsets per Network Rail CIF / RSPS5046:
#   TA/TI location: tiploc 3-9, CRS 54-56
#   BS schedule: runs_from 10-15 (yymmdd), runs_to 16-21, days-run 22-28
#                (Mon..Sun), STP indicator 80 (C = cancellation overlay)
#   LO origin:   tiploc 3-9, public dep 16-19
#   LI interm.:  tiploc 3-9, public arr 26-29, public dep 30-33
#   LT terminus: tiploc 3-9, public arr 16-19
# Public times are hhmm; "0000" means no public call.
# ------------------------------------------------------------------------------

library(data.table)

cif_file    <- "E:/rail/timetable/May18.cif"   # gunzip the .gz first
out_csv     <- "E:/rail/data/journey_times.csv"
travel_date <- as.Date("2018-05-22")           # a plain Tuesday in the window
arrive_from <- 6.5 * 60                        # arrive London 06:30...
arrive_to   <- 10.5 * 60                       # ...to 10:30 (minutes)

# CRS codes of the London Terminals fare group (NLC 1072), from the fares feed
london_crs <- c("EUS","MYB","STP","PAD","BFR","CTK","CST","CHX","LBG","WAE",
                "VIC","VXH","WAT","OLD","MOG","KGX","LST","FST")

## 1 - read the raw CIF -------------------------------------------------------
lines <- fread(cif_file, sep = NULL, header = FALSE, quote = "",
               strip.white = FALSE, col.names = "line")[, rec := substr(line, 1, 2)]

## 2 - TIPLOC -> CRS map from the location records ------------------------------
tip <- lines[rec %chin% c("TA", "TI"),
             .(tiploc = substr(line, 3, 9), crs = trimws(substr(line, 54, 56)),
               stanox = substr(line, 45, 49))]
tip <- unique(tip, by = "tiploc")
# platform-group TIPLOCs (e.g. VICTRIC/VICTRIE at London Victoria) carry a
# blank CRS; recover it from a CRS-bearing TIPLOC sharing the same STANOX
sx <- unique(tip[crs != "" & stanox != "00000", .(stanox, crs)], by = "stanox")
tip[sx, on = "stanox", crs_fill := i.crs]
tip[crs == "" & !is.na(crs_fill), crs := crs_fill]
tip <- tip[!is.na(crs) & crs != "", .(tiploc, crs)]

## 3 - schedules valid on the chosen date ---------------------------------------
# number every schedule, and stamp its id onto the calling-point rows below it
lines[, sched := cumsum(rec == "BS")]

ymd  <- as.integer(format(travel_date, "%y%m%d"))
dow  <- as.integer(format(travel_date, "%u"))          # 1 = Monday
bs <- lines[rec == "BS",
            .(sched, from = as.integer(substr(line, 10, 15)),
              to   = as.integer(substr(line, 16, 21)),
              runs = substr(line, 21 + dow, 21 + dow),
              stp  = substr(line, 80, 80))]
valid <- bs[from <= ymd & to >= ymd & runs == "1" & stp != "C", sched]

## 4 - calling points of valid schedules ----------------------------------------
pts <- lines[rec %chin% c("LO", "LI", "LT") & sched %in% valid]
pts[, `:=`(
  tiploc = substr(line, 3, 9),
  dep = fcase(rec == "LO", substr(line, 16, 19),
              rec == "LI", substr(line, 30, 33),
              default = "0000"),
  arr = fcase(rec == "LI", substr(line, 26, 29),
              rec == "LT", substr(line, 16, 19),
              default = "0000"))]
hhmm <- function(x) fifelse(x == "0000", NA_integer_,
                            as.integer(substr(x, 1, 2)) * 60L +
                            as.integer(substr(x, 3, 4)))
pts[, `:=`(dep_min = hhmm(dep), arr_min = hhmm(arr))]
pts <- merge(pts[, .(sched, seq = rowid(sched), tiploc, dep_min, arr_min)],
             tip, by = "tiploc")

## 5 - fastest direct journey to any London terminal ----------------------------
arr_ldn <- pts[crs %chin% london_crs & arr_min %between% c(arrive_from, arrive_to),
               .(sched, ldn_seq = seq, ldn_arr = arr_min, terminal = crs)]
legs <- merge(pts[!is.na(dep_min) & !crs %chin% london_crs,
                  .(sched, seq, dep_min, crs)],
              arr_ldn, by = "sched", allow.cartesian = TRUE)[seq < ldn_seq]
legs[, minutes := ldn_arr - dep_min]
legs <- legs[minutes > 0]                       # drops overnight/sleeper wraps

best <- legs[, .(minutes = min(minutes)), by = .(crs, terminal)]
best <- best[, .(minutes = min(minutes),
                 terminal = terminal[which.min(minutes)]), by = crs]

dir.create(dirname(out_csv), showWarnings = FALSE)
fwrite(best[order(crs)], out_csv)
cat("Wrote", out_csv, ":", nrow(best),
    "stations with a direct AM-peak service to a London terminal.\n")
print(best[crs %chin% c("BTN","LEI","RDG","WOK","CBG","MKC","SEV","PBO")])
