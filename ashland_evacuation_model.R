# =============================================================================
# Ashland, OR Wildfire Evacuation Traffic Model
# =============================================================================
# Macroscopic zone-based flow model with BPR queuing at exit bottlenecks.
# Outputs: clearance time curves, bottleneck analysis, sensitivity sweeps.
#
# Study area: Ashland, OR (~22,300 pop.) — exits via I-5 N, I-5 S, OR-66 E
# =============================================================================

# --- 0. PACKAGES -------------------------------------------------------------
pkgs <- c("osmdata", "sf", "tidyverse", "ggplot2", "patchwork", "viridis")
new  <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

set.seed(42)

# =============================================================================
# 1. ROAD NETWORK (OpenStreetMap)
# =============================================================================
message("Downloading Ashland road network from OSM...")

ashland_bb <- c(left = -122.78, bottom = 42.14, right = -122.60, top = 42.26)

roads_osm <- tryCatch(
  opq(bbox = ashland_bb) |>
    add_osm_feature(
      key   = "highway",
      value = c("motorway", "trunk", "primary", "secondary", "tertiary",
                "residential", "motorway_link", "trunk_link", "primary_link")
    ) |>
    osmdata_sf(),
  error = function(e) {
    stop("OSM download failed. Check internet connection.\n", e$message)
  }
)

roads <- roads_osm$osm_lines |>
  select(osm_id, name, highway, maxspeed, lanes, geometry) |>
  st_transform(crs = 32610)   # UTM zone 10N — metres

message(sprintf("  Downloaded %d road segments.", nrow(roads)))

# Capacity and free-flow speed by road class (vehicles/hour/lane, mph)
road_params <- tribble(
  ~highway,        ~cap_vphpl, ~ffs_mph, ~lanes_def,
  "motorway",          2200,      65,       2,
  "motorway_link",     1200,      45,       1,
  "trunk",             1800,      55,       2,
  "trunk_link",        1000,      35,       1,
  "primary",           1500,      45,       2,
  "primary_link",       800,      35,       1,
  "secondary",         1000,      35,       2,
  "tertiary",           700,      30,       1,
  "residential",        400,      25,       1
)

roads <- roads |>
  left_join(road_params, by = "highway") |>
  mutate(
    lanes_n    = coalesce(as.integer(lanes), lanes_def),
    cap_vph    = cap_vphpl * lanes_n,
    length_km  = as.numeric(st_length(geometry)) / 1000,
    ffs_kmh    = ffs_mph * 1.60934,
    ff_time_h  = length_km / ffs_kmh
  ) |>
  filter(!is.na(cap_vph))

# =============================================================================
# 2. EVACUATION ZONES
# =============================================================================
# Six zones approximating Ashland's neighbourhoods.
# Populations estimated from 2020 Census tract data (total ~22,300).
zones <- tribble(
  ~zone_id, ~zone_name,           ~lon,      ~lat,   ~pop,
  1,        "Downtown/Plaza",     -122.709,  42.194,  4200,
  2,        "Railroad District",  -122.697,  42.200,  3800,
  3,        "Quiet Village/E",    -122.680,  42.191,  3500,
  4,        "East Ashland",       -122.664,  42.189,  3200,
  5,        "Siskiyou Heights",   -122.720,  42.184,  2600,
  6,        "North Ashland",      -122.718,  42.210,  5000
)
stopifnot(sum(zones$pop) == 22300)

zones_sf <- st_as_sf(zones, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs = 32610)

# =============================================================================
# 3. EXIT NODES
# =============================================================================
# Three exit corridors out of the Ashland valley.
# I-5 is the dominant capacity route; OR-66 is mountainous (lower cap).
exits <- tribble(
  ~exit_id, ~exit_name,              ~lon,      ~lat,   ~cap_vph,
  1,        "I-5 North (Medford)",   -122.711,  42.232,    3600,
  2,        "I-5 South (CA)",        -122.640,  42.155,    2400,
  3,        "OR-66 East (Klamath)",  -122.609,  42.188,     800
)
# total_cap = 6,800 vph outbound

exits_sf <- st_as_sf(exits, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs = 32610)

# Zone → exit split fractions (distance + road-type weighted, sum to 1 per zone)
zone_exit_split <- tribble(
  ~zone_id, ~exit_id, ~fraction,
  1,  1,  0.60,
  1,  2,  0.40,
  2,  1,  0.55,
  2,  2,  0.45,
  3,  1,  0.45,
  3,  2,  0.40,
  3,  3,  0.15,
  4,  2,  0.50,
  4,  3,  0.50,
  5,  2,  0.80,
  5,  1,  0.20,
  6,  1,  0.90,
  6,  2,  0.10
)

# =============================================================================
# 4. MODEL PARAMETERS (base case)
# =============================================================================
base_params <- list(
  total_pop        = 22300L,
  car_occupancy    = 2.2,        # persons/vehicle
  compliance_rate  = 0.85,       # fraction heeding order
  warning_time_min = 30,         # min from order → first departures
  departure_window = 120,        # min over which departures are spread
  sim_duration     = 360,        # total simulation window (min)
  dt               = 1,          # time step (min)
  bpr_alpha        = 0.15,       # BPR travel-time alpha
  bpr_beta         = 4.0,        # BPR travel-time beta
  contraflow       = FALSE       # double outbound cap on I-5 N?
)

# =============================================================================
# 5. HELPERS
# =============================================================================

# Logistic S-curve: cumulative departure fraction at time t (minutes)
cum_departure <- function(t, warning_min, window_min) {
  t_adj <- t - warning_min
  ifelse(t_adj <= 0, 0,
         plogis(t_adj, location = window_min / 2, scale = window_min / 12))
}

# BPR delay multiplier (dimensionless): 1 + alpha*(v/c)^beta
bpr_multiplier <- function(vol, cap, alpha = 0.15, beta = 4) {
  1 + alpha * (vol / cap)^beta
}

# =============================================================================
# 6. SIMULATION ENGINE
# =============================================================================
run_simulation <- function(params      = base_params,
                           zone_exit   = zone_exit_split,
                           closed_exits = integer(0)) {

  p <- params

  # --- vehicle counts -------------------------------------------------------
  total_veh  <- p$total_pop * p$compliance_rate / p$car_occupancy
  zone_veh   <- zones |>
    mutate(vehicles = round(pop / p$total_pop * total_veh))

  time_steps <- seq(0, p$sim_duration, by = p$dt)
  n_t        <- length(time_steps)

  # departure rate (fraction per minute)
  cum   <- cum_departure(time_steps, p$warning_time_min, p$departure_window)
  d_rate <- c(0, diff(cum))     # length n_t

  # --- adjust exits for contraflow / closures --------------------------------
  exit_cap <- exits$cap_vph
  if (p$contraflow)         exit_cap[1] <- exit_cap[1] * 1.5
  if (length(closed_exits)) exit_cap[closed_exits] <- 0

  # re-normalise zone->exit fractions if exits are closed
  ze <- zone_exit
  if (length(closed_exits)) {
    ze <- ze |>
      filter(!exit_id %in% closed_exits) |>
      group_by(zone_id) |>
      mutate(fraction = fraction / sum(fraction)) |>
      ungroup()
  }

  # --- pre-compute per-exit inflow per time-step ----------------------------
  n_exits <- nrow(exits)
  inflow_mat <- matrix(0, nrow = n_t, ncol = n_exits)   # veh/step

  for (z in seq_len(nrow(zone_veh))) {
    zid <- zone_veh$zone_id[z]
    veh <- zone_veh$vehicles[z]
    alloc <- ze |> filter(zone_id == zid)
    for (r in seq_len(nrow(alloc))) {
      eid  <- alloc$exit_id[r]
      inflow_mat[, eid] <- inflow_mat[, eid] + veh * alloc$fraction[r] * d_rate
    }
  }

  # --- time-stepped queuing at exits ----------------------------------------
  queue   <- numeric(n_exits)   # vehicles waiting at each exit bottleneck
  cleared <- numeric(n_exits)

  res <- tibble(
    time          = time_steps,
    queued        = 0,
    cleared_total = 0,
    inflow_total  = 0
  )
  exit_cleared_mat <- matrix(0, nrow = n_t, ncol = n_exits,
                              dimnames = list(NULL, exits$exit_name))

  for (i in seq_len(n_t)) {
    # Arrivals
    queue <- queue + inflow_mat[i, ]

    # Discharge: capacity per minute, BPR delay applied to access road
    cap_pm   <- exit_cap / 60
    vol_ratio <- pmax(0, queue / pmax(1, exit_cap))     # avoid /0
    delay_m   <- bpr_multiplier(vol_ratio, 1,
                                p$bpr_alpha, p$bpr_beta)
    eff_cap   <- cap_pm / delay_m                        # reduced by congestion
    discharged <- pmin(queue, eff_cap)

    queue   <- queue - discharged
    cleared <- cleared + discharged

    res$queued[i]        <- sum(queue)
    res$cleared_total[i] <- sum(cleared)
    res$inflow_total[i]  <- sum(inflow_mat[i, ])
    exit_cleared_mat[i, ] <- cleared
  }

  # --- clearance metrics ----------------------------------------------------
  tv <- sum(zone_veh$vehicles)

  t95 <- time_steps[which(res$cleared_total >= 0.95 * tv)[1]]
  t99 <- time_steps[which(res$cleared_total >= 0.99 * tv)[1]]

  list(
    results       = res,
    exit_cleared  = as_tibble(exit_cleared_mat) |> mutate(time = time_steps),
    clearance_95  = t95,
    clearance_99  = t99,
    total_vehicles = tv,
    final_queue   = queue,
    params        = p
  )
}

# =============================================================================
# 7. BASE CASE
# =============================================================================
message("\nRunning base-case simulation...")
base <- run_simulation()

cat("\n=== BASE CASE RESULTS ===\n")
cat(sprintf("  Evacuating vehicles : %d\n",  round(base$total_vehicles)))
cat(sprintf("  95%% clearance time  : %d min  (%.1f h)\n",
            base$clearance_95, base$clearance_95 / 60))
cat(sprintf("  99%% clearance time  : %d min  (%.1f h)\n",
            base$clearance_99, base$clearance_99 / 60))
cat(sprintf("  Residual queue      : %.0f vehicles still in network\n",
            sum(base$final_queue)))

# =============================================================================
# 8. SENSITIVITY ANALYSIS
# =============================================================================
message("\nRunning sensitivity sweeps...")

# 8a. Compliance rate sweep
compliance_sweep <- tibble(compliance = seq(0.50, 1.00, by = 0.05)) |>
  mutate(
    sim = map(compliance, \(cr) {
      p <- base_params; p$compliance_rate <- cr
      run_simulation(params = p)
    }),
    clearance_95 = map_dbl(sim, "clearance_95"),
    clearance_99 = map_dbl(sim, "clearance_99"),
    total_veh    = map_dbl(sim, "total_vehicles")
  ) |>
  select(-sim)

# 8b. Warning lead-time sweep
warning_sweep <- tibble(warning_min = c(0, 15, 30, 45, 60, 90, 120)) |>
  mutate(
    sim = map(warning_min, \(wt) {
      p <- base_params; p$warning_time_min <- wt
      run_simulation(params = p)
    }),
    clearance_95 = map_dbl(sim, "clearance_95"),
    clearance_99 = map_dbl(sim, "clearance_99")
  ) |>
  select(-sim)

# 8c. Contraflow on I-5 North
cf_on  <- run_simulation(params = modifyList(base_params, list(contraflow = TRUE)))

# 8d. Route closures (fire cuts off an exit)
no_i5s  <- run_simulation(closed_exits = 2L)   # I-5 South blocked by fire
no_or66 <- run_simulation(closed_exits = 3L)   # OR-66 blocked

cat("\n=== SENSITIVITY SUMMARY ===\n")
cat(sprintf("  Contraflow I-5 N:  %d → %d min (95%%)\n",
            base$clearance_95, cf_on$clearance_95))
cat(sprintf("  I-5 South closed:  %d min (95%%)\n", no_i5s$clearance_95))
cat(sprintf("  OR-66 closed:      %d min (95%%)\n", no_or66$clearance_95))

# =============================================================================
# 9. PLOTS
# =============================================================================
message("\nGenerating plots...")

clr_blue <- "#2c7bb6"
clr_red  <- "#d7191c"
clr_grn  <- "#1a9641"

# -- Plot A: Clearance curve (base case) --------------------------------------
exit_long <- base$exit_cleared |>
  pivot_longer(-time, names_to = "exit", values_to = "cleared")

pA <- ggplot(base$results, aes(x = time)) +
  geom_ribbon(aes(ymin = cleared_total - queued,
                  ymax = cleared_total + queued,
                  fill = "In network / queued"), alpha = 0.3) +
  geom_line(aes(y = cleared_total, colour = "Cleared"), linewidth = 1.2) +
  geom_hline(yintercept = 0.95 * base$total_vehicles,
             linetype = "dotted", colour = clr_red, linewidth = 0.8) +
  geom_vline(xintercept = base$clearance_95,
             linetype = "dashed", colour = clr_red, linewidth = 0.8) +
  annotate("text",
           x     = base$clearance_95 + 6,
           y     = base$total_vehicles * 0.35,
           label = sprintf("95%% clearance\n%d min (%.1f h)",
                           base$clearance_95, base$clearance_95 / 60),
           hjust = 0, size = 3.5, colour = clr_red) +
  scale_colour_manual(values = c("Cleared" = clr_blue)) +
  scale_fill_manual(values   = c("In network / queued" = clr_red)) +
  scale_x_continuous(
    name       = "Time from Evacuation Order (minutes)",
    sec.axis   = sec_axis(~ . / 60, name = "Hours")
  ) +
  labs(
    title  = "Vehicle Clearance Over Time — Base Case",
    y      = "Cumulative Vehicles",
    colour = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

# -- Plot B: Per-exit clearance -----------------------------------------------
pB <- exit_long |>
  ggplot(aes(x = time, y = cleared, colour = exit)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = base$clearance_95,
             linetype = "dashed", colour = "grey50") +
  scale_colour_viridis_d(option = "D", end = 0.85) +
  scale_x_continuous(
    name     = "Time from Evacuation Order (minutes)",
    sec.axis = sec_axis(~ . / 60, name = "Hours")
  ) +
  labs(
    title  = "Vehicles Cleared by Exit Corridor",
    y      = "Cumulative Vehicles",
    colour = "Exit"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

# -- Plot C: Sensitivity — compliance rate ------------------------------------
pC <- compliance_sweep |>
  pivot_longer(c(clearance_95, clearance_99),
               names_to = "metric", values_to = "minutes") |>
  mutate(metric = recode(metric,
                         clearance_95 = "95% clearance",
                         clearance_99 = "99% clearance")) |>
  ggplot(aes(x = compliance * 100, y = minutes, colour = metric)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("95% clearance" = clr_blue,
                                 "99% clearance" = clr_red)) +
  labs(
    title  = "Sensitivity: Compliance Rate",
    x      = "Compliance Rate (%)",
    y      = "Clearance Time (min)",
    colour = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

# -- Plot D: Sensitivity — warning lead time ----------------------------------
pD <- warning_sweep |>
  pivot_longer(c(clearance_95, clearance_99),
               names_to = "metric", values_to = "minutes") |>
  mutate(metric = recode(metric,
                         clearance_95 = "95% clearance",
                         clearance_99 = "99% clearance")) |>
  ggplot(aes(x = warning_min, y = minutes, colour = metric)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("95% clearance" = clr_blue,
                                 "99% clearance" = clr_red)) +
  labs(
    title  = "Sensitivity: Warning Lead Time",
    x      = "Warning Time (minutes)",
    y      = "Clearance Time (min)",
    colour = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

# -- Plot E: Route closure & contraflow bar chart -----------------------------
scenario_df <- tibble(
  scenario = factor(
    c("All open", "Contraflow\nI-5 N", "I-5 S\nclosed", "OR-66\nclosed"),
    levels = c("All open", "Contraflow\nI-5 N", "I-5 S\nclosed", "OR-66\nclosed")
  ),
  t95 = c(base$clearance_95, cf_on$clearance_95,
           no_i5s$clearance_95, no_or66$clearance_95),
  t99 = c(base$clearance_99, cf_on$clearance_99,
           no_i5s$clearance_99, no_or66$clearance_99)
) |>
  pivot_longer(c(t95, t99), names_to = "pct", values_to = "minutes") |>
  mutate(pct = recode(pct, t95 = "95% clearance", t99 = "99% clearance"))

pE <- ggplot(scenario_df, aes(x = scenario, y = minutes, fill = pct)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = round(minutes)),
            position = position_dodge(0.8), vjust = -0.4, size = 3) +
  scale_fill_manual(values = c("95% clearance" = clr_blue,
                                "99% clearance" = clr_red)) +
  labs(
    title  = "Clearance Time by Scenario",
    x      = NULL,
    y      = "Clearance Time (min)",
    fill   = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

# -- Plot F: OSM road network map ---------------------------------------------
roads_plot <- roads |>
  filter(highway %in% c("motorway", "trunk", "primary", "secondary")) |>
  mutate(road_class = case_when(
    highway %in% c("motorway", "trunk") ~ "Motorway / Trunk",
    highway == "primary"                ~ "Primary",
    highway == "secondary"              ~ "Secondary",
    TRUE                                ~ "Other"
  ))

pF <- ggplot() +
  geom_sf(data = roads_plot, aes(colour = road_class),
          linewidth = 0.6, alpha = 0.8) +
  geom_sf(data = exits_sf, shape = 17, size = 4, colour = clr_red) +
  geom_sf(data = zones_sf, shape = 16, size = 3, colour = clr_blue) +
  scale_colour_viridis_d(option = "C", end = 0.85) +
  labs(
    title   = "Ashland Road Network & Model Zones",
    colour  = "Road Class",
    caption = "Red triangles = exits  |  Blue circles = evacuation zones"
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), legend.position = "bottom")

# -- Combine all panels -------------------------------------------------------
layout <- "
AABB
CCDD
EEFF
"

combined <- pA + pB + pC + pD + pE + pF +
  plot_layout(design = layout) +
  plot_annotation(
    title    = "Ashland, OR — Wildfire Evacuation Traffic Model",
    subtitle = sprintf(
      "Population: %s  |  Compliance: %.0f%%  |  Warning: %d min  |  Exits: I-5 N, I-5 S, OR-66 E",
      format(base_params$total_pop, big.mark = ","),
      base_params$compliance_rate * 100,
      base_params$warning_time_min
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 10, colour = "grey40")
    )
  )

out_file <- "ashland_evacuation_results.png"
ggsave(out_file, combined, width = 16, height = 14, dpi = 150)
message(sprintf("\nPlot saved to: %s", out_file))

# =============================================================================
# 10. SUMMARY TABLE
# =============================================================================
cat("\n=== FULL SENSITIVITY TABLE ===\n")
sens_table <- bind_rows(
  tibble(parameter   = "Compliance rate",
         value_label = sprintf("%.0f%%", compliance_sweep$compliance * 100),
         t95         = compliance_sweep$clearance_95,
         t99         = compliance_sweep$clearance_99),
  tibble(parameter   = "Warning lead time",
         value_label = sprintf("%d min", warning_sweep$warning_min),
         t95         = warning_sweep$clearance_95,
         t99         = warning_sweep$clearance_99)
)
print(sens_table, n = Inf)
cat("\n")
message("Model complete.")
