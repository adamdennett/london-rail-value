# The price of getting into London

Station-level analysis of GB rail fares into London, weighed against the
distance and journey time to a central London terminal: where is it cheap or
expensive to commute from, for what you actually get?

The analysis lives in [`london-rail-value.qmd`](london-rail-value.qmd)
(rendered: `london-rail-value.html`). It:

- parses the RDG/ATOC national fares feed (fixed-width `RJFAF` format) —
  flows, fares, ticket types, station clusters, routes and non-derivable
  overrides — and builds a one-row-per-station panel of walk-up fares to the
  London Terminals group: anytime/off-peak singles and returns, weekly and
  annual seasons, with the cheapest route identified
  ([`data/london_fare_panel.csv`](data/london_fare_panel.csv));
- computes each station's fastest **direct AM-peak journey time** to a London
  terminal straight from the national CIF timetable
  ([`R/build_journey_times.R`](R/build_journey_times.R) →
  [`data/journey_times.csv`](data/journey_times.csv));
- analyses cost × time × distance: value-for-money models, per-distance-band
  Pareto frontiers, budget leaderboards, value-of-time re-rankings,
  searchable interactive scatters and cost/speed maps.

Headline finding: **season prices scale with distance (elasticity ≈ 0.86)
while journey time is essentially unpriced (≈ −0.09)** — which is why
Brighton (62 min, £3,968 p.a. in May 2018) and Leicester (67 min, £9,320)
can coexist.

## Reproducing

The raw industry feeds are **not committed** (size and licence); download
them and drop them in place:

| What | Where to get it | Where to put it |
|---|---|---|
| Fares feed (`RJFAFxxx.ZIP`) | [Rail Data Marketplace](https://raildata.org.uk/dataProducts) → *Fares* (Rail Delivery Group) | unzip to `RJFAF851/` (or update `params$feed_dir` in the qmd) |
| Timetable (CIF) | RDM → *NWR Historical Timetable Data* (May 2018 matches this fares feed) or *Timetable – Full Refresh* for current | `timetable/`, then run `R/build_journey_times.R` |
| Fares feed spec (`SP0035`) | RDM documentation links (ASSIST) | optional reference |

This copy of the analysis is pinned to the **May 2018** fares round
(RJFAF851, generated 25/05/2018) with the matching May 2018 timetable, so
fares and times are temporally consistent. A newer feed pair drops straight
in — the file formats are unchanged.

Render with:

```sh
quarto render london-rail-value.qmd
```

R ≥ 4.x with data.table, tidyverse, geodist, plotly, crosstalk, leaflet,
DT, kableExtra, ggrepel.

## Data attribution

Fares, timetable and station data are derived from feeds published by the
Rail Delivery Group and Network Rail via the
[Rail Data Marketplace](https://raildata.org.uk) under their respective open
licences (restrictions may apply); this repository redistributes only small
derived aggregates. Station coordinates via `UK stations.csv` (NLC/CRS/lat-lon
reference data).
