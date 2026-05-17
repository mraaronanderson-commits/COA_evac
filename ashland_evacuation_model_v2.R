# =============================================================================
# Ashland, OR Wildfire Evacuation Traffic Model  —  Version 2
# =============================================================================
# Enhanced macroscopic model informed by:
#   KLD Engineering, P.C. (2021). "City of Ashland Evacuation Time Estimate
#   Study." KLD TR-1217, April 2021 Final Report.
#
# Key improvements over v1:
#   - 10 Emergency Management Zones (EMZs) with KLD-derived populations
#   - KLD household vehicle rates (1.43 veh/HH, 2.23 persons/HH)
#   - Multi-population departure curves (residents C/D, tourists/employees A)
#   - Shadow region (6% voluntary compliance, ~388 vehicles)
#   - HCM 2016 road capacities with capacity-drop factor R = 0.90
#   - I-5 on-ramp metering explicitly modelled as dominant bottleneck
#   - Fire-direction scenarios: bidirectional | forced NB | forced SB
#   - Scenario matrix: summer weekday vs. fall weekday (school in session)
#   - Infrastructure sensitivity: E Nevada bridge, N Mountain ramps,
#     OR-99 NB widening, 1-vehicle/HH public-messaging campaign
#
# The original v1 model is preserved in ashland_evacuation_model.R
# =============================================================================

# --- 0. PACKAGES -------------------------------------------------------------
pkgs <- c("osmdata", "sf", "tidyverse", "ggplot2", "patchwork", "viridis")
new  <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

set.seed(42)

# =============================================================================
# 1. POPULATION & VEHICLE GENERATION  (KLD TR-1217 Tables 3-1 through 3-6)
# =============================================================================

# HCM / KLD household parameters (survey-derived)
HH_SIZE     <- 2.23   # persons per household
VEH_PER_HH  <- 1.43   # evacuating vehicles per household (survey)
VEH_PER_HH_REDUCED <- 1.00  # 1-vehicle/HH sensitivity case

# --- 1a. Emergency Management Zone populations (2020 projected) --------------
# EMZ populations from KLD Table 3-1; total EMZ residents = 21,449
# Approximate spatial centroids (lon/lat WGS84) from KLD Figure 2-1 description
emz <- tribble(
  ~emz_id, ~emz_name,              ~lon,      ~lat,   ~pop_res,
  1,  "Southern Ridgeline",        -122.720,  42.175,   1800,
  2,  "Siskiyou Heights/SW",       -122.728,  42.184,   1850,
  3,  "Quiet Village/SE",          -122.680,  42.183,   2100,
  4,  "East Ashland",              -122.660,  42.190,   2800,
  5,  "Shakespeare/OSF",           -122.709,  42.192,   1750,
  6,  "Downtown/Plaza",            -122.710,  42.196,   2400,
  7,  "Railroad District",         -122.700,  42.204,   2300,
  8,  "North Mountain",            -122.718,  42.208,   2650,
  9,  "Oak St/NE Access-Impaired", -122.688,  42.205,   1950,
  10, "Walker Ave/SOU Campus",     -122.703,  42.185,   1849
)
stopifnot(sum(emz$pop_res) == 21449)

# Derived residential vehicle demand
emz <- emz |>
  mutate(
    households  = round(pop_res / HH_SIZE),
    veh_res_base = round(households * VEH_PER_HH)
  )

# --- 1b. Shadow region -------------------------------------------------------
# KLD Table 3-2: surrounding region (Talent + rural) = 10,099 persons
# 6% voluntary evacuation compliance (demographic survey finding)
shadow_pop        <- 10099
shadow_compliance <- 0.06
shadow_veh        <- round((shadow_pop / HH_SIZE) * VEH_PER_HH * shadow_compliance)
# shadow_veh ≈ 388

# --- 1c. Scenario-dependent special populations ------------------------------
# KLD Tables 3-3 to 3-5; values vary by scenario
# Scenario 1 = summer midweek midday (highest tourist load)
# Scenario 4 = fall midweek midday, schools in session (highest overall demand)

scenarios <- tribble(
  ~scenario_id, ~label,
  1, "Summer weekday (high tourism)",
  4, "Fall weekday (schools in session)"
)

# Tourists (Oregon Shakespeare Festival, lodging, hiking, golf)
# KLD Section 3.3: total 5,150 tourists, avg 2.29 persons/vehicle
# Scenario 1: 70% daytime occupancy; Scenario 4: 55% daytime occupancy
tourist_total <- 5150; tourist_occ <- 2.29
tourist_veh <- c(
  `1` = round(tourist_total * 0.70 / tourist_occ),   # 1,575
  `4` = round(tourist_total * 0.55 / tourist_occ)    # 1,237
)

# Employees / commuters (from outside EMZ), KLD Table 3-4
# 2,302 employees, 1.06 persons/vehicle (commuter rate)
employee_veh <- round(2302 / 1.06)   # 2,172 vehicles

# Southern Oregon University (SOU) — Scenario 4 only
# Off-campus students: 4,425 vehicles; on-campus: 2 buses (modelled as ~10 cars equiv.)
sou_veh <- c(`1` = 0, `4` = 4435)

# Schools (K-12): 8,330 students; 75 buses → each bus equivalent to
# ~40 car-trip-equivalents for flow purposes; varies by scenario
school_bus_veh_equiv <- c(`1` = 0, `4` = round(75 * 40))   # 3,000 car-equiv.
# Note: school buses travel in convoy; converted to pcu for network loading

# External / I-5 pass-through traffic (KLD Section 3.5)
# These add to network congestion but are not evacuees.
# Modelled as background demand that dissipates over the first 120 minutes.
external_veh_total <- 8412  # ODOT AADT, K=0.107, D=0.50

# =============================================================================
# 2. ROAD NETWORK CORRIDORS  (KLD Section 4 + HCM 2016)
# =============================================================================

# Capacity parameters per HCM 2016 (pc/h/lane)
# Capacity-drop factor R = 0.90 (Zhang & Levinson 2004 calibration in KLD)
R_DROP   <- 0.90
BDN_FRAC <- 0.90   # breakdown occurs at 90% of capacity

# Five evacuation corridor chains modelled at their governing bottleneck
# Each chain: local feeder → arterial bottleneck → I-5 on-ramp → I-5 mainline
# Capacity = min-capacity link in chain (on-ramp or arterial, whichever binds)
corridors <- tribble(
  ~corr_id, ~name,                          ~direction, ~cap_base_vph, ~lanes,
  1, "OR-99 NB → I-5 North",               "north",    1700,           1,  # Single-lane NB bottleneck (Helman–Jackson)
  2, "OR-99 SB / I-5 SB on-ramp",          "south",    2000,           1,  # I-5 SB on-ramp from OR-99 (metered)
  3, "OR-66 EB → I-5 N/S interchange",     "both",     1900,           2,  # Multi-lane, OR-66 approaches I-5
  4, "OR-66 East (Klamath Falls)",          "east",     1700,           1,  # Rural 2-lane, exits study area eastbound
  5, "S Valley View / local SB feed",      "south",    1200,           1   # Minor southern collector
) |>
  mutate(
    cap_vph        = cap_base_vph * lanes,                  # total vph
    cap_drop_vph   = cap_vph * R_DROP,                      # post-breakdown throughput
    bdn_thresh_vph = cap_vph * BDN_FRAC                     # flow that triggers breakdown
  )

# Fire-direction routing: which corridors remain open
# All open (bidirectional), Fire from North (only southbound), Fire from South (only northbound)
fire_closures <- list(
  bidirectional = integer(0),
  fire_north    = c(1L, 3L),   # I-5 North and OR-66 EB/N interchange blocked
  fire_south    = c(2L, 4L, 5L) # I-5 South, OR-66 East, and S Valley View blocked
)

# --- EMZ → corridor routing split -------------------------------------------
# Based on EMZ geography relative to I-5 interchanges and OR-66/OR-99
# Rows sum to 1.0 per EMZ (under bidirectional routing)
emz_corr_split <- tribble(
  ~emz_id, ~corr_id, ~fraction,
  1,  2,  0.60,   # Southern ridgeline — mostly I-5 South
  1,  5,  0.25,
  1,  4,  0.15,
  2,  2,  0.55,
  2,  3,  0.30,
  2,  5,  0.15,
  3,  3,  0.45,
  3,  4,  0.35,
  3,  2,  0.20,
  4,  4,  0.50,
  4,  3,  0.30,
  4,  2,  0.20,
  5,  1,  0.40,
  5,  2,  0.35,
  5,  3,  0.25,
  6,  1,  0.50,
  6,  2,  0.35,
  6,  3,  0.15,
  7,  1,  0.60,
  7,  2,  0.25,
  7,  3,  0.15,
  8,  1,  0.75,
  8,  2,  0.15,
  8,  3,  0.10,
  9,  1,  0.80,   # Oak St / NE — access-impaired, funnels to OR-99 NB
  9,  2,  0.20,
  10, 2,  0.50,   # Walker Ave / SOU — near OR-99 / I-5 SB
  10, 1,  0.30,
  10, 3,  0.20
)

# =============================================================================
# 3. DEPARTURE DISTRIBUTION FUNCTIONS  (KLD Section 5, Appendix D)
# =============================================================================
# Three distributions derived from the KLD notification × mobilization convolution:
#
#   Distribution A — tourists & employees (fast):
#     Direct departure from activity location; 90th pct ≈ 45 min
#
#   Distribution D — residents without returning commuter (86% of HHs):
#     Notify → prepare home; 90th pct ≈ 135 min
#
#   Distribution C — residents awaiting returning commuter (14% of HHs):
#     Notify → commuter returns → prepare home; 90th pct ≈ 195 min
#
# Logistic curves fitted to KLD cumulative trip-generation tables.

dep_curve_A <- function(t) plogis(t, location = 30,  scale = 10)   # tourists/employees
dep_curve_D <- function(t) plogis(t, location = 75,  scale = 28)   # residents, no wait
dep_curve_C <- function(t) plogis(t, location = 135, scale = 38)   # residents, wait commuter

# Weighted resident departure (86% Distribution D, 14% Distribution C)
dep_curve_resident <- function(t) 0.86 * dep_curve_D(t) + 0.14 * dep_curve_C(t)

# =============================================================================
# 4. SIMULATION ENGINE
# =============================================================================

run_simulation_v2 <- function(
    scenario_id      = 1,
    fire_dir         = "bidirectional",
    veh_per_hh       = VEH_PER_HH,
    shadow_comp      = shadow_compliance,
    include_external = TRUE,
    infra            = list(nevada_bridge = FALSE,  # E Nevada St bridge over Bear Creek
                             mountain_ramps = FALSE, # N Mountain Ave I-5 ramps
                             or99_widening  = FALSE) # OR-99 NB extra lane
) {

  scen <- as.character(scenario_id)
  sim_dur  <- 360   # minutes total
  dt       <- 1     # minute time steps
  time_v   <- seq(0, sim_dur, by = dt)
  n_t      <- length(time_v)

  # --- Vehicle demand by population type -----------------------------------
  veh_res     <- sum(round(emz$households * veh_per_hh))
  veh_shadow  <- round((shadow_pop / HH_SIZE) * veh_per_hh * shadow_comp)
  veh_tourist <- tourist_veh[[scen]]
  veh_emp     <- employee_veh
  veh_sou     <- sou_veh[[scen]]
  veh_school  <- school_bus_veh_equiv[[scen]]

  total_evac_veh <- veh_res + veh_shadow + veh_tourist + veh_emp + veh_sou + veh_school

  # --- Departure-rate vectors per population type --------------------------
  # (fraction of that group departing in each 1-minute step)
  rate_res     <- c(0, diff(dep_curve_resident(time_v)))
  rate_tourist <- c(0, diff(dep_curve_A(time_v)))
  rate_emp     <- c(0, diff(dep_curve_A(time_v)))
  rate_sou     <- c(0, diff(dep_curve_A(time_v)))   # students leave quickly
  rate_school  <- c(0, diff(dep_curve_D(time_v)))   # staged bus departures

  # Network-wide arrival rate (vehicles/minute at corridors)
  veh_inflow_t <- veh_res     * rate_res     +
                  veh_shadow  * rate_res     +   # shadow mirrors resident behaviour
                  veh_tourist * rate_tourist +
                  veh_emp     * rate_emp     +
                  veh_sou     * rate_sou     +
                  veh_school  * rate_school

  # --- Corridor capacity adjustments ---------------------------------------
  corr_cap <- corridors$cap_vph
  corr_bdn <- corridors$bdn_thresh_vph
  corr_drop_cap <- corridors$cap_drop_vph

  # Infrastructure improvements
  if (infra$or99_widening) {
    corr_cap[1]      <- corr_cap[1]      + 1700   # add NB lane to OR-99 corridor
    corr_bdn[1]      <- corr_cap[1]      * BDN_FRAC
    corr_drop_cap[1] <- corr_cap[1]      * R_DROP
  }
  if (infra$mountain_ramps) {
    # New N Mountain Ave ramps add ~10% capacity to northbound I-5 feed
    corr_cap[1]      <- corr_cap[1]      * 1.10
    corr_bdn[1]      <- corr_cap[1]      * BDN_FRAC
    corr_drop_cap[1] <- corr_cap[1]      * R_DROP
  }
  if (infra$nevada_bridge) {
    # E Nevada St bridge reconnects eastern EMZs to OR-99; improves corr 3 & 4 feed
    corr_cap[3]      <- corr_cap[3]      * 1.08
    corr_bdn[3]      <- corr_cap[3]      * BDN_FRAC
    corr_drop_cap[3] <- corr_cap[3]      * R_DROP
  }

  # --- Fire direction: close affected corridors ----------------------------
  closed <- fire_closures[[fire_dir]]
  if (length(closed)) corr_cap[closed] <- 0

  # --- EMZ → corridor routing (re-normalise if corridors closed) -----------
  ze <- emz_corr_split
  if (length(closed)) {
    ze <- ze |>
      filter(!corr_id %in% closed) |>
      group_by(emz_id) |>
      mutate(fraction = fraction / sum(fraction)) |>
      ungroup()
  }

  n_corr <- nrow(corridors)

  # Pre-compute per-corridor inflow by time step
  # Each EMZ's inflow is proportional to its share of total residential vehicles
  inflow_mat <- matrix(0, nrow = n_t, ncol = n_corr)
  emz_veh_v  <- round(emz$households * veh_per_hh)

  for (z in seq_len(nrow(emz))) {
    zid  <- emz$emz_id[z]
    zveh <- emz_veh_v[z]
    alloc <- ze |> filter(emz_id == zid)
    for (r in seq_len(nrow(alloc))) {
      cid <- alloc$corr_id[r]
      inflow_mat[, cid] <- inflow_mat[, cid] + zveh * alloc$fraction[r] * rate_res
    }
  }

  # Non-EMZ populations distributed proportionally by fire-direction availability
  open_corr <- setdiff(seq_len(n_corr), closed)
  open_caps  <- corr_cap[open_corr]
  open_share <- open_caps / sum(open_caps)
  for (i in seq_along(open_corr)) {
    cid <- open_corr[i]
    inflow_mat[, cid] <- inflow_mat[, cid] +
      (veh_tourist * rate_tourist +
       veh_emp     * rate_emp     +
       veh_sou     * rate_sou     +
       veh_school  * rate_school) * open_share[i]
  }
  # Shadow region — same EMZ routing proportions (simplified)
  for (z in seq_len(nrow(emz))) {
    zid  <- emz$emz_id[z]
    zveh_sh <- round(emz_veh_v[z] / sum(emz_veh_v) * veh_shadow)
    alloc <- ze |> filter(emz_id == zid)
    for (r in seq_len(nrow(alloc))) {
      cid <- alloc$corr_id[r]
      inflow_mat[, cid] <- inflow_mat[, cid] + zveh_sh * alloc$fraction[r] * rate_res
    }
  }

  # External/pass-through vehicles (add to corridor congestion for first 120 min)
  if (include_external && length(open_corr) > 0) {
    ext_rate <- rep(0, n_t)
    ext_rate[time_v <= 120] <- external_veh_total / 120 / 60  # veh/min spread over 2 h
    for (i in seq_along(open_corr)) {
      cid <- open_corr[i]
      inflow_mat[, cid] <- inflow_mat[, cid] + ext_rate * open_share[i]
    }
  }

  # --- Time-stepped queuing with capacity drop -----------------------------
  breakdown    <- rep(FALSE, n_corr)
  queue        <- rep(0, n_corr)
  cleared      <- rep(0, n_corr)

  res <- tibble(
    time          = time_v,
    queued        = 0,
    cleared_total = 0,
    inflow_total  = 0
  )
  corr_cleared_mat <- matrix(0, nrow = n_t, ncol = n_corr,
                              dimnames = list(NULL, corridors$name))
  corr_queue_mat   <- matrix(0, nrow = n_t, ncol = n_corr,
                              dimnames = list(NULL, corridors$name))

  for (i in seq_len(n_t)) {
    queue <- queue + inflow_mat[i, ]

    # Determine effective capacity (with capacity drop)
    # Breakdown triggered when cumulative inflow exceeds breakdown threshold in a step
    vol_rate_vph <- inflow_mat[i, ] * 60   # convert veh/min → veh/h for comparison
    breakdown <- ifelse(vol_rate_vph > 0,
                        breakdown | (queue * 60 / sim_dur > corr_bdn),
                        breakdown)
    eff_cap_pm <- ifelse(breakdown, corr_drop_cap, corr_cap) / 60

    # BPR delay (applied as additional friction on top of capacity)
    vol_ratio  <- pmax(0, queue) / pmax(1, ifelse(breakdown, corr_drop_cap, corr_cap))
    bpr_factor <- 1 + 0.15 * vol_ratio^4
    adj_cap_pm <- eff_cap_pm / bpr_factor

    discharged <- pmin(queue, pmax(0, adj_cap_pm))
    discharged[closed] <- 0

    queue   <- pmax(0, queue - discharged)
    cleared <- cleared + discharged

    res$queued[i]        <- sum(queue)
    res$cleared_total[i] <- sum(cleared)
    res$inflow_total[i]  <- sum(inflow_mat[i, ])
    corr_cleared_mat[i, ] <- cleared
    corr_queue_mat[i, ]   <- queue
  }

  tv <- total_evac_veh
  t90 <- time_v[which(res$cleared_total >= 0.90 * tv)[1]]
  t95 <- time_v[which(res$cleared_total >= 0.95 * tv)[1]]
  t99 <- time_v[which(res$cleared_total >= 0.99 * tv)[1]]

  list(
    results          = res,
    corr_cleared     = as_tibble(corr_cleared_mat) |> mutate(time = time_v),
    corr_queue       = as_tibble(corr_queue_mat)   |> mutate(time = time_v),
    clearance_90     = t90,
    clearance_95     = t95,
    clearance_99     = t99,
    total_vehicles   = tv,
    veh_by_type      = c(residents = veh_res, shadow = veh_shadow,
                         tourists  = veh_tourist, employees = veh_emp,
                         sou       = veh_sou, school = veh_school),
    params           = list(scenario_id = scenario_id, fire_dir = fire_dir,
                            veh_per_hh = veh_per_hh, infra = infra)
  )
}

# =============================================================================
# 5. BASE CASES
# =============================================================================
message("Running base cases (Scenarios 1 & 4, bidirectional)...")

s1 <- run_simulation_v2(scenario_id = 1, fire_dir = "bidirectional")
s4 <- run_simulation_v2(scenario_id = 4, fire_dir = "bidirectional")

cat("\n=== BASE CASE — Scenario 1 (Summer Weekday) ===\n")
cat(sprintf("  Total evacuating vehicles  : %d\n", round(s1$total_vehicles)))
cat(sprintf("  Vehicle breakdown by type  :\n"))
invisible(Map(function(n, v) cat(sprintf("    %-12s : %d\n", n, round(v))),
              names(s1$veh_by_type), s1$veh_by_type))
cat(sprintf("  90th pct clearance  : %d min  (%.1f h)\n", s1$clearance_90, s1$clearance_90/60))
cat(sprintf("  95th pct clearance  : %d min  (%.1f h)\n", s1$clearance_95, s1$clearance_95/60))
cat(sprintf("  99th pct clearance  : %d min  (%.1f h)\n", s1$clearance_99, s1$clearance_99/60))

cat("\n=== BASE CASE — Scenario 4 (Fall Weekday, Schools In) ===\n")
cat(sprintf("  Total evacuating vehicles  : %d\n", round(s4$total_vehicles)))
cat(sprintf("  Vehicle breakdown by type  :\n"))
invisible(Map(function(n, v) cat(sprintf("    %-12s : %d\n", n, round(v))),
              names(s4$veh_by_type), s4$veh_by_type))
cat(sprintf("  90th pct clearance  : %d min  (%.1f h)\n", s4$clearance_90, s4$clearance_90/60))
cat(sprintf("  95th pct clearance  : %d min  (%.1f h)\n", s4$clearance_95, s4$clearance_95/60))
cat(sprintf("  99th pct clearance  : %d min  (%.1f h)\n", s4$clearance_99, s4$clearance_99/60))

# =============================================================================
# 6. FIRE DIRECTION SCENARIOS  (KLD Appendix J.4)
# =============================================================================
message("\nRunning fire-direction scenarios...")

fire_scenarios <- tibble(
  fire_dir = c("bidirectional", "fire_north", "fire_south"),
  label    = c("All routes open\n(bidirectional)",
               "Fire from North\n(only southbound routes)",
               "Fire from South\n(only northbound routes)")
) |>
  mutate(
    s1_sim = map(fire_dir, \(fd) run_simulation_v2(scenario_id = 1, fire_dir = fd)),
    s4_sim = map(fire_dir, \(fd) run_simulation_v2(scenario_id = 4, fire_dir = fd)),
    s1_t90 = map_dbl(s1_sim, "clearance_90"),
    s1_t95 = map_dbl(s1_sim, "clearance_95"),
    s4_t90 = map_dbl(s4_sim, "clearance_90"),
    s4_t95 = map_dbl(s4_sim, "clearance_95")
  )

cat("\n=== FIRE DIRECTION RESULTS (Scenario 4, 90th pct) ===\n")
cat(sprintf("  Bidirectional      : %d min  (%.1f h)\n",
            filter(fire_scenarios, fire_dir == "bidirectional")$s4_t90,
            filter(fire_scenarios, fire_dir == "bidirectional")$s4_t90 / 60))
cat(sprintf("  Fire from North    : %d min  (%.1f h)  [forced SB only]\n",
            filter(fire_scenarios, fire_dir == "fire_north")$s4_t90,
            filter(fire_scenarios, fire_dir == "fire_north")$s4_t90 / 60))
cat(sprintf("  Fire from South    : %d min  (%.1f h)  [forced NB only]\n",
            filter(fire_scenarios, fire_dir == "fire_south")$s4_t90,
            filter(fire_scenarios, fire_dir == "fire_south")$s4_t90 / 60))

# =============================================================================
# 7. SENSITIVITY ANALYSES
# =============================================================================
message("\nRunning sensitivity sweeps (Scenario 4)...")

# 7a. Vehicles per household (public messaging campaign)
veh_sweep <- tibble(vph = c(1.00, 1.10, 1.20, 1.30, 1.43)) |>
  mutate(
    sim = map(vph, \(v) run_simulation_v2(scenario_id = 4, veh_per_hh = v)),
    t90 = map_dbl(sim, "clearance_90"),
    t95 = map_dbl(sim, "clearance_95"),
    total_veh = map_dbl(sim, "total_vehicles")
  ) |> select(-sim)

# 7b. Shadow evacuation rate
shadow_sweep <- tibble(shadow_rate = c(0, 0.03, 0.06, 0.15, 0.30, 1.00)) |>
  mutate(
    sim = map(shadow_rate, \(sr)
              run_simulation_v2(scenario_id = 4, shadow_comp = sr)),
    t90 = map_dbl(sim, "clearance_90"),
    t95 = map_dbl(sim, "clearance_95")
  ) |> select(-sim)

# 7c. Infrastructure improvements  (KLD Appendix J.5)
infra_scenarios <- list(
  base              = list(nevada_bridge=FALSE, mountain_ramps=FALSE, or99_widening=FALSE),
  nevada_bridge     = list(nevada_bridge=TRUE,  mountain_ramps=FALSE, or99_widening=FALSE),
  mountain_ramps    = list(nevada_bridge=FALSE, mountain_ramps=TRUE,  or99_widening=FALSE),
  both_nb_mr        = list(nevada_bridge=TRUE,  mountain_ramps=TRUE,  or99_widening=FALSE),
  or99_widening     = list(nevada_bridge=FALSE, mountain_ramps=FALSE, or99_widening=TRUE),
  all_improvements  = list(nevada_bridge=TRUE,  mountain_ramps=TRUE,  or99_widening=TRUE)
)
infra_labels <- c("Base", "E Nevada Bridge", "N Mountain Ramps",
                  "Bridge + Ramps", "OR-99 NB Widening", "All Improvements")

infra_results <- tibble(
  label = infra_labels,
  infra = infra_scenarios
) |>
  mutate(
    sim = map(infra, \(inf) run_simulation_v2(scenario_id = 4, infra = inf)),
    t90 = map_dbl(sim, "clearance_90"),
    t95 = map_dbl(sim, "clearance_95")
  ) |> select(-sim, -infra)

cat("\n=== INFRASTRUCTURE SENSITIVITY (Scenario 4, 90th pct) ===\n")
print(infra_results, n = Inf)

# =============================================================================
# 8. PLOTS
# =============================================================================
message("\nGenerating plots...")

clr_blue  <- "#2c7bb6"
clr_red   <- "#d7191c"
clr_grn   <- "#1a9641"
clr_amber <- "#f7a600"

# -- Plot A: Clearance curves — Scenario 1 vs 4, bidirectional ---------------
clearance_comparison <- bind_rows(
  s1$results |> mutate(scenario = "Scenario 1\n(Summer weekday)"),
  s4$results |> mutate(scenario = "Scenario 4\n(Fall weekday, schools)")
)

pA <- ggplot(clearance_comparison, aes(x = time, y = cleared_total,
                                        colour = scenario)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0.90 * s4$total_vehicles, linetype = "dotted",
             colour = "grey50", linewidth = 0.7) +
  annotate("text", x = 5, y = 0.90 * s4$total_vehicles + 300,
           label = "90th pct threshold", hjust = 0, size = 3, colour = "grey40") +
  geom_vline(xintercept = s1$clearance_90, linetype = "dashed",
             colour = clr_blue, linewidth = 0.8) +
  geom_vline(xintercept = s4$clearance_90, linetype = "dashed",
             colour = clr_red, linewidth = 0.8) +
  scale_colour_manual(values = c(clr_blue, clr_red)) +
  scale_x_continuous(name = "Minutes from Evacuation Order",
                     sec.axis = sec_axis(~ . / 60, name = "Hours")) +
  labs(title  = "Vehicle Clearance — Summer vs. Fall Scenarios",
       y      = "Cumulative Vehicles Cleared",
       colour = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")

# -- Plot B: Per-corridor congestion (Scenario 4, bidirectional) -------------
queue_long <- s4$corr_queue |>
  pivot_longer(-time, names_to = "corridor", values_to = "queued")

pB <- ggplot(queue_long, aes(x = time, y = queued, fill = corridor)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_viridis_d(option = "D", end = 0.9) +
  scale_x_continuous(name = "Minutes from Evacuation Order",
                     sec.axis = sec_axis(~ . / 60, name = "Hours")) +
  labs(title  = "Queue Build-Up by Corridor\n(Scenario 4, Bidirectional)",
       y      = "Vehicles Queued",
       fill   = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "right",
                                         legend.text = element_text(size = 7))

# -- Plot C: Fire direction — clearance curves --------------------------------
fire_curves <- bind_rows(
  fire_scenarios$s4_sim[[1]]$results |> mutate(dir = fire_scenarios$label[1]),
  fire_scenarios$s4_sim[[2]]$results |> mutate(dir = fire_scenarios$label[2]),
  fire_scenarios$s4_sim[[3]]$results |> mutate(dir = fire_scenarios$label[3])
)

pC <- ggplot(fire_curves, aes(x = time, y = cleared_total, colour = dir)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c(clr_blue, clr_red, clr_amber)) +
  scale_x_continuous(name = "Minutes from Evacuation Order",
                     sec.axis = sec_axis(~ . / 60, name = "Hours")) +
  labs(title  = "Impact of Fire Direction on Clearance\n(Scenario 4)",
       y      = "Cumulative Vehicles Cleared",
       colour = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom",
                                         legend.text = element_text(size = 8))

# -- Plot D: Vehicles per household sensitivity --------------------------------
pD <- ggplot(veh_sweep, aes(x = vph)) +
  geom_line(aes(y = t90, colour = "90th pct"), linewidth = 1.1) +
  geom_line(aes(y = t95, colour = "95th pct"), linewidth = 1.1, linetype = "dashed") +
  geom_point(aes(y = t90, colour = "90th pct"), size = 3) +
  geom_point(aes(y = t95, colour = "95th pct"), size = 3) +
  geom_vline(xintercept = VEH_PER_HH, linetype = "dotted", colour = "grey50") +
  annotate("text", x = VEH_PER_HH + 0.01, y = max(veh_sweep$t95) * 0.98,
           label = "Survey avg\n(1.43)", hjust = 0, size = 3, colour = "grey40") +
  scale_colour_manual(values = c("90th pct" = clr_blue, "95th pct" = clr_red)) +
  labs(title  = "Sensitivity: Vehicles per Household\n(Public Messaging Campaign)",
       x      = "Evacuating Vehicles per Household",
       y      = "Clearance Time (min)",
       colour = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")

# -- Plot E: Shadow evacuation rate sensitivity --------------------------------
pE <- ggplot(shadow_sweep, aes(x = shadow_rate * 100)) +
  geom_line(aes(y = t90, colour = "90th pct"), linewidth = 1.1) +
  geom_line(aes(y = t95, colour = "95th pct"), linewidth = 1.1, linetype = "dashed") +
  geom_point(aes(y = t90, colour = "90th pct"), size = 3) +
  geom_point(aes(y = t95, colour = "95th pct"), size = 3) +
  geom_vline(xintercept = 6, linetype = "dotted", colour = "grey50") +
  annotate("text", x = 7, y = max(shadow_sweep$t95) * 0.98,
           label = "Survey base\n(6%)", hjust = 0, size = 3, colour = "grey40") +
  scale_colour_manual(values = c("90th pct" = clr_blue, "95th pct" = clr_red)) +
  labs(title  = "Sensitivity: Shadow Evacuation Rate",
       x      = "Shadow Region Compliance (%)",
       y      = "Clearance Time (min)",
       colour = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")

# -- Plot F: Infrastructure what-if -------------------------------------------
infra_plot_df <- infra_results |>
  pivot_longer(c(t90, t95), names_to = "pct", values_to = "minutes") |>
  mutate(
    pct   = recode(pct, t90 = "90th pct", t95 = "95th pct"),
    label = factor(label, levels = infra_labels),
    saving_90 = infra_results$t90[1] - infra_results$t90[match(label, infra_labels)]
  )

pF <- ggplot(infra_plot_df, aes(x = label, y = minutes, fill = pct)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = round(minutes)),
            position = position_dodge(0.8), vjust = -0.4, size = 3) +
  scale_fill_manual(values = c("90th pct" = clr_blue, "95th pct" = clr_red)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  labs(title  = "Infrastructure Improvements — Clearance Time\n(Scenario 4)",
       x      = NULL,
       y      = "Clearance Time (min)",
       fill   = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 8))

# -- Combine ------------------------------------------------------------------
layout <- "
AABC
DDEF
"

combined <- pA + pB + pC + pD + pE + pF +
  plot_layout(design = layout) +
  plot_annotation(
    title    = "Ashland, OR — Wildfire Evacuation Model  v2",
    subtitle = paste0(
      "KLD-informed: 10 EMZs | Multi-population departure curves | ",
      "HCM 2016 capacities with R=0.90 drop | Fire-direction scenarios"
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 9,  colour = "grey40")
    )
  )

out_file <- "ashland_evacuation_results_v2.png"
ggsave(out_file, combined, width = 18, height = 13, dpi = 150)
message(sprintf("\nPlot saved to: %s", out_file))

# =============================================================================
# 9. SUMMARY TABLE
# =============================================================================
cat("\n=== COMPLETE SCENARIO MATRIX (90th / 95th pct clearance, minutes) ===\n")
scenario_matrix <- fire_scenarios |>
  select(label, s1_t90, s1_t95, s4_t90, s4_t95) |>
  rename(
    `Fire direction`        = label,
    `S1 90th pct (min)`     = s1_t90,
    `S1 95th pct (min)`     = s1_t95,
    `S4 90th pct (min)`     = s4_t90,
    `S4 95th pct (min)`     = s4_t95
  )
print(scenario_matrix)

message("\nModel v2 complete.")
