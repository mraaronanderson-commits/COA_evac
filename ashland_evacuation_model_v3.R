# =============================================================================
# Ashland, OR Wildfire Evacuation Traffic Model  —  Version 3
# =============================================================================
# Builds directly on the KLD TR-1217 framework established in v2.
#
# Parameter changes from v2 (two real-world updates):
#
#   1. SOUTHERN OREGON UNIVERSITY (SOU) enrollment decline
#      SOU has contracted significantly since the 2021 KLD study.
#      Fall enrollment now approximately half the value used in KLD Table 3-5.
#      v2: 4,435 student/staff vehicles (Scenario 4)
#      v3: 2,218 student/staff vehicles (Scenario 4, -50%)
#
#   2. ASANTE ASHLAND COMMUNITY HOSPITAL closure to small ER/outpatient
#      The full-service hospital is closing; a small emergency room and
#      outpatient facility will remain. This reduces:
#        a) Hospital employees contributing to the evacuation workforce
#           (~200 FTE reduction, embedded in KLD's 2,302 employee count)
#        b) Medical transit-dependent patients (ambulances, wheelchair vans)
#           — captured here by the hospital employee count reduction.
#      v2: 2,172 employee vehicles  (2,302 commuters / 1.06 persons per veh)
#      v3: 1,981 employee vehicles  (2,102 commuters / 1.06 persons per veh)
#
# Simulation engine: uses the corrected capacity-limited discharge model
# (no BPR penalty on queue discharge) with fallback routing for EMZs whose
# preferred corridors are fire-blocked. These fixes were introduced in the v2
# visualization script (ashland_evac_v2_plots.R); v3 adopts them as canonical.
#
# Version history:
#   v1: ashland_evacuation_model.R    — 6 zones, 3 exits, simple BPR queuing
#   v2: ashland_evacuation_model_v2.R — 10 EMZs, KLD parameters, multi-pop
#   v3: ashland_evacuation_model_v3.R — SOU -50%, hospital closure (this file)
# =============================================================================

# --- 0. PACKAGES -------------------------------------------------------------
pkgs <- c("tidyverse", "ggplot2", "patchwork", "viridis")
new  <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

set.seed(42)

# =============================================================================
# 1. CONSTANTS (shared between v2 and v3 for comparison)
# =============================================================================
HH_SIZE    <- 2.23    # persons per household (KLD survey)
VEH_PER_HH <- 1.43    # evacuating vehicles per household
R_DROP     <- 0.90    # capacity-drop factor at breakdown (Zhang & Levinson 2004)
BDN_FRAC   <- 0.90    # fraction of capacity at which breakdown triggers

# External / pass-through I-5 traffic (ODOT AADT, K=0.107, D=0.50)
EXTERNAL_VEH <- 8412

# Shadow region (surrounding Talent + rural areas)
SHADOW_POP <- 10099

# =============================================================================
# 2. ROAD NETWORK  (identical to v2 — no infrastructure changes in v3)
# =============================================================================
corridors <- tribble(
  ~corr_id, ~name,                         ~cap_base_vph, ~lanes,
  1, "OR-99 NB -> I-5 North",              1700, 1,
  2, "OR-99 SB / I-5 SB on-ramp",         2000, 1,
  3, "OR-66 EB -> I-5 interchange",        1900, 2,
  4, "OR-66 East (Klamath Falls)",         1700, 1,
  5, "S Valley View SB",                   1200, 1
) |> mutate(
  cap_vph      = cap_base_vph * lanes,
  cap_drop_vph = cap_vph * R_DROP,
  bdn_thresh   = cap_vph * BDN_FRAC
)

fire_closures <- list(
  bidirectional = integer(0),
  fire_north    = c(1L, 3L),
  fire_south    = c(2L, 4L, 5L)
)

# =============================================================================
# 3. EMERGENCY MANAGEMENT ZONES  (identical to v2)
# =============================================================================
emz <- tribble(
  ~emz_id, ~emz_name,                   ~pop_res,
  1,  "EMZ 1 — S Ridgeline",             1800,
  2,  "EMZ 2 — Siskiyou Hts",            1850,
  3,  "EMZ 3 — Quiet Village/SE",        2100,
  4,  "EMZ 4 — East Ashland",            2800,
  5,  "EMZ 5 — Shakespeare/OSF",         1750,
  6,  "EMZ 6 — Downtown/Plaza",          2400,
  7,  "EMZ 7 — Railroad District",       2300,
  8,  "EMZ 8 — North Mountain",          2650,
  9,  "EMZ 9 — Oak St/NE",               1950,
  10, "EMZ 10 — Walker Ave/SOU",         1849
) |> mutate(households = round(pop_res / HH_SIZE),
            veh_base   = round(households * VEH_PER_HH))

stopifnot(sum(emz$pop_res) == 21449)

# EMZ → corridor routing splits (identical to v2)
emz_cs <- tribble(
  ~emz_id, ~corr_id, ~fraction,
  1,2,.60, 1,5,.25, 1,4,.15,
  2,2,.55, 2,3,.30, 2,5,.15,
  3,3,.45, 3,4,.35, 3,2,.20,
  4,4,.50, 4,3,.30, 4,2,.20,
  5,1,.40, 5,2,.35, 5,3,.25,
  6,1,.50, 6,2,.35, 6,3,.15,
  7,1,.60, 7,2,.25, 7,3,.15,
  8,1,.75, 8,2,.15, 8,3,.10,
  9,1,.80, 9,2,.20,
  10,2,.50, 10,1,.30, 10,3,.20
)

# =============================================================================
# 4. SPECIAL POPULATIONS  (v2 baseline + v3 changes)
# =============================================================================

# -- v2 BASELINE values (preserved for comparison) ---------------------------
sou_veh_v2      <- c("1" = 0L, "4" = 4435L)   # KLD Table 3-5
employee_veh_v2 <- round(2302 / 1.06)           # 2,172 vehicles

# -- v3 UPDATED values --------------------------------------------------------
# SOU: ~50% enrollment reduction from KLD survey period
sou_veh_v3      <- c("1" = 0L, "4" = round(4435 * 0.50))  # 2,218 vehicles

# Hospital: full-service → small ER/outpatient; ~200 fewer employee commuters
# Asante Ashland Community Hospital employed roughly 200 FTE (nursing + admin +
# support) beyond what a small ER would retain. Those commuter trips are removed.
employee_veh_v3 <- round((2302 - 200) / 1.06)              # 1,981 vehicles

cat(sprintf(
  "\n=== V2 → V3 PARAMETER CHANGES ===\n  SOU vehicles (S4)   : %d  →  %d  (Δ = %+d)\n  Employee vehicles   : %d  →  %d  (Δ = %+d)\n",
  sou_veh_v2[["4"]], sou_veh_v3[["4"]],
  sou_veh_v3[["4"]] - sou_veh_v2[["4"]],
  employee_veh_v2, employee_veh_v3,
  employee_veh_v3 - employee_veh_v2
))

# Shared special populations (unchanged from v2)
tourist_veh  <- c("1" = round(5150 * .70 / 2.29), "4" = round(5150 * .55 / 2.29))
school_veh   <- c("1" = 0L, "4" = round(75 * 40))

# =============================================================================
# 5. DEPARTURE DISTRIBUTIONS  (KLD Section 5 / Appendix D, identical to v2)
# =============================================================================
dep_A   <- function(t) plogis(t, location = 30,  scale = 10)
dep_D   <- function(t) plogis(t, location = 75,  scale = 28)
dep_C   <- function(t) plogis(t, location = 135, scale = 38)
dep_res <- function(t) 0.86 * dep_D(t) + 0.14 * dep_C(t)

# =============================================================================
# 6. SIMULATION ENGINE  (corrected capacity-limited discharge)
# =============================================================================
# Uses the fixed engine from ashland_evac_v2_plots.R:
#   - Discharge rate capped at corridor capacity (no BPR penalty on discharge)
#   - Fallback routing assigns EMZs with all routes blocked to remaining open corridors
#   - Breakdown flag triggered by instantaneous inflow rate exceeding threshold

run_sim_v3 <- function(
    scenario_id  = 1,
    fire_dir     = "bidirectional",
    veh_per_hh   = VEH_PER_HH,
    shadow_comp  = 0.06,
    dur          = 600,
    sou_veh      = sou_veh_v3,
    employee_veh = employee_veh_v3,
    infra        = list(nevada_bridge  = FALSE,
                        mountain_ramps = FALSE,
                        or99_widening  = FALSE)
) {
  scen   <- as.character(scenario_id)
  time_v <- seq(0, dur); n_t <- length(time_v)

  # Demand by population type
  veh_res    <- sum(round(emz$households * veh_per_hh))
  veh_shadow <- round((SHADOW_POP / HH_SIZE) * veh_per_hh * shadow_comp)
  veh_t      <- tourist_veh[[scen]]
  veh_e      <- employee_veh
  veh_s      <- sou_veh[[scen]]
  veh_sc     <- school_veh[[scen]]
  total_veh  <- veh_res + veh_shadow + veh_t + veh_e + veh_s + veh_sc

  # Departure rate vectors (fraction per minute)
  rate_res <- c(0, diff(dep_res(time_v)))
  rate_A   <- c(0, diff(dep_A(time_v)))
  rate_sc  <- c(0, diff(dep_D(time_v)))

  # Corridor capacities (mutable)
  corr_cap <- corridors$cap_vph
  corr_bdn <- corridors$bdn_thresh
  corr_dc  <- corridors$cap_drop_vph

  # Infrastructure adjustments
  if (infra$or99_widening) {
    corr_cap[1] <- corr_cap[1] + 1700
    corr_bdn[1] <- corr_cap[1] * BDN_FRAC
    corr_dc[1]  <- corr_cap[1] * R_DROP
  }
  if (infra$mountain_ramps) {
    corr_cap[1] <- corr_cap[1] * 1.10
    corr_bdn[1] <- corr_cap[1] * BDN_FRAC
    corr_dc[1]  <- corr_cap[1] * R_DROP
  }
  if (infra$nevada_bridge) {
    corr_cap[3] <- corr_cap[3] * 1.08
    corr_bdn[3] <- corr_cap[3] * BDN_FRAC
    corr_dc[3]  <- corr_cap[3] * R_DROP
  }

  # Fire-direction closures
  closed    <- fire_closures[[fire_dir]]
  if (length(closed)) corr_cap[closed] <- 0
  open_corr  <- setdiff(seq_len(nrow(corridors)), closed)
  open_share <- corr_cap[open_corr] / sum(corr_cap[open_corr])

  # Route table — re-normalise after closures
  ze <- emz_cs
  if (length(closed)) {
    ze <- ze |>
      filter(!corr_id %in% closed) |>
      group_by(emz_id) |>
      mutate(fraction = fraction / sum(fraction)) |>
      ungroup()
  }

  # Fallback: EMZs with all preferred routes blocked → distribute to open corridors
  unrouted <- setdiff(emz$emz_id, unique(ze$emz_id))
  if (length(unrouted) > 0 && length(open_corr) > 0) {
    fallback <- tibble(
      emz_id   = rep(unrouted, each = length(open_corr)),
      corr_id  = rep(open_corr, times = length(unrouted)),
      fraction = rep(open_share, times = length(unrouted))
    )
    ze <- bind_rows(ze, fallback)
  }

  # Per-corridor inflow matrix
  n_c        <- nrow(corridors)
  inflow_mat <- matrix(0, nrow = n_t, ncol = n_c)
  emz_veh_v  <- round(emz$households * veh_per_hh)
  shadow_v   <- round(emz_veh_v / sum(emz_veh_v) * veh_shadow)

  for (z in seq_len(nrow(emz))) {
    alloc <- ze |> filter(emz_id == emz$emz_id[z])
    for (r in seq_len(nrow(alloc))) {
      cid <- alloc$corr_id[r]
      inflow_mat[, cid] <- inflow_mat[, cid] +
        (emz_veh_v[z] + shadow_v[z]) * alloc$fraction[r] * rate_res
    }
  }

  for (i in seq_along(open_corr)) {
    cid <- open_corr[i]
    inflow_mat[, cid] <- inflow_mat[, cid] +
      (veh_t * rate_A + veh_e * rate_A + veh_s * rate_A + veh_sc * rate_sc) *
      open_share[i]
  }

  # External I-5 pass-through (first 2 hours)
  ext_rate <- rep(0, n_t)
  ext_rate[time_v <= 120] <- EXTERNAL_VEH / 120 / 60
  for (i in seq_along(open_corr))
    inflow_mat[, open_corr[i]] <- inflow_mat[, open_corr[i]] + ext_rate * open_share[i]

  # Time-stepped queuing — capacity-limited discharge with R-drop
  bdn      <- rep(FALSE, n_c)
  queue    <- rep(0,     n_c)
  cleared  <- rep(0,     n_c)
  queued_v <- cleared_v <- inflow_v <- rep(0, n_t)
  cq_mat   <- cc_mat <- matrix(0, n_t, n_c,
                                dimnames = list(NULL, corridors$name))

  for (i in seq_len(n_t)) {
    queue    <- queue + inflow_mat[i, ]
    flow_vph <- inflow_mat[i, ] * 60
    bdn      <- bdn | (flow_vph > corr_bdn)

    eff_cap_pm  <- ifelse(bdn, corr_dc, corr_cap) / 60
    dis         <- pmin(queue, pmax(0, eff_cap_pm))
    dis[closed] <- 0

    queue   <- pmax(0, queue - dis)
    cleared <- cleared + dis

    queued_v[i]  <- sum(queue)
    cleared_v[i] <- sum(cleared)
    inflow_v[i]  <- sum(inflow_mat[i, ])
    cq_mat[i, ]  <- queue
    cc_mat[i, ]  <- cleared
  }

  t90 <- time_v[which(cleared_v >= 0.90 * total_veh)[1]]
  t95 <- time_v[which(cleared_v >= 0.95 * total_veh)[1]]
  t99 <- time_v[which(cleared_v >= 0.99 * total_veh)[1]]

  list(
    cleared    = cleared_v, queued = queued_v, inflow = inflow_v,
    cq_mat     = cq_mat,    cc_mat = cc_mat,   time   = time_v,
    t90 = t90, t95 = t95,   t99 = t99,
    total_veh  = total_veh,
    veh_by_type = c(residents = veh_res, shadow = veh_shadow,
                    tourists  = veh_t,   employees = veh_e,
                    sou       = veh_s,   school = veh_sc)
  )
}

# Helper: run same scenario with v2 parameters (for comparison)
run_sim_v2_compat <- function(scenario_id = 1, fire_dir = "bidirectional", dur = 600) {
  run_sim_v3(scenario_id = scenario_id, fire_dir = fire_dir, dur = dur,
             sou_veh = sou_veh_v2, employee_veh = employee_veh_v2)
}

# =============================================================================
# 7. BASE CASES
# =============================================================================
message("\nRunning v3 base cases...")

s1b <- run_sim_v3(1, "bidirectional")
s4b <- run_sim_v3(4, "bidirectional")

for (lbl in c("Scenario 1 — Summer weekday", "Scenario 4 — Fall weekday (schools)")) {
  sim <- if (grepl("1", lbl)) s1b else s4b
  cat(sprintf("\n=== V3 BASE — %s ===\n", lbl))
  cat(sprintf("  Total vehicles  : %d\n", sim$total_veh))
  cat(sprintf("  90th pct        : %d min  (%.1f h)\n", sim$t90, sim$t90 / 60))
  cat(sprintf("  95th pct        : %d min  (%.1f h)\n", sim$t95, sim$t95 / 60))
  cat(sprintf("  99th pct        : %d min  (%.1f h)\n", sim$t99, sim$t99 / 60))
  cat("  Vehicle types:\n")
  invisible(Map(function(n, v) cat(sprintf("    %-12s : %d\n", n, round(v))),
                names(sim$veh_by_type), sim$veh_by_type))
}

# =============================================================================
# 8. V2 vs V3 COMPARISON
# =============================================================================
message("\nRunning v2 comparison sims...")

s1b_v2 <- run_sim_v2_compat(1)
s4b_v2 <- run_sim_v2_compat(4)

cat("\n=== V2 vs V3 COMPARISON (base cases, bidirectional) ===\n")
cat(sprintf("%-30s  %6s  %6s  %6s\n", "Scenario", "V2 t90", "V3 t90", "Delta"))
cat(strrep("-", 56), "\n")
for (r in list(
  list("Scenario 1 Summer bidir",    s1b_v2$t90, s1b$t90),
  list("Scenario 4 Fall bidir",      s4b_v2$t90, s4b$t90)
)) {
  cat(sprintf("%-30s  %6d  %6d  %+6d min\n", r[[1]], r[[2]], r[[3]], r[[3]] - r[[2]]))
}

# =============================================================================
# 9. FIRE DIRECTION SCENARIOS
# =============================================================================
message("\nRunning fire-direction scenarios (v3)...")

fire_dirs <- c("bidirectional", "fire_north", "fire_south")
fire_lbls <- c("Bidirectional", "Fire from North\n(forced SB)", "Fire from South\n(forced NB)")

fire_res <- tibble(
  fire_dir = fire_dirs, label = fire_lbls
) |>
  mutate(
    s1_v3 = map(fire_dir, \(fd) run_sim_v3(1, fd)),
    s4_v3 = map(fire_dir, \(fd) run_sim_v3(4, fd)),
    s1_v2 = map(fire_dir, \(fd) run_sim_v2_compat(1, fd)),
    s4_v2 = map(fire_dir, \(fd) run_sim_v2_compat(4, fd)),
    s1_t90_v3 = map_dbl(s1_v3, "t90"), s4_t90_v3 = map_dbl(s4_v3, "t90"),
    s1_t95_v3 = map_dbl(s1_v3, "t95"), s4_t95_v3 = map_dbl(s4_v3, "t95"),
    s1_t90_v2 = map_dbl(s1_v2, "t90"), s4_t90_v2 = map_dbl(s4_v2, "t90")
  )

cat("\n=== FIRE DIRECTION — V3 RESULTS (Scenario 4, 90th pct) ===\n")
fire_res |>
  select(label, s4_t90_v2, s4_t90_v3) |>
  mutate(delta = s4_t90_v3 - s4_t90_v2) |>
  print()

# =============================================================================
# 10. SENSITIVITY ANALYSES
# =============================================================================
message("\nRunning sensitivity sweeps (v3, Scenario 4)...")

veh_sw <- tibble(vph = c(1.00, 1.10, 1.20, 1.30, 1.43)) |>
  mutate(s = map(vph, \(v) run_sim_v3(4, veh_per_hh = v, dur = 800)),
         t90 = map_dbl(s, "t90"), t95 = map_dbl(s, "t95"),
         total = map_dbl(s, "total_veh")) |> select(-s)

shad_sw <- tibble(rate = c(0, .03, .06, .15, .30, 1.00)) |>
  mutate(s = map(rate, \(r) run_sim_v3(4, shadow_comp = r, dur = 800)),
         t90 = map_dbl(s, "t90"), t95 = map_dbl(s, "t95")) |> select(-s)

infra_list <- list(
  list(nevada_bridge = FALSE, mountain_ramps = FALSE, or99_widening = FALSE),
  list(nevada_bridge = TRUE,  mountain_ramps = FALSE, or99_widening = FALSE),
  list(nevada_bridge = FALSE, mountain_ramps = TRUE,  or99_widening = FALSE),
  list(nevada_bridge = TRUE,  mountain_ramps = TRUE,  or99_widening = FALSE),
  list(nevada_bridge = FALSE, mountain_ramps = FALSE, or99_widening = TRUE),
  list(nevada_bridge = TRUE,  mountain_ramps = TRUE,  or99_widening = TRUE)
)
infra_labels <- c("Base", "E Nevada\nBridge", "N Mountain\nRamps",
                  "Bridge +\nRamps", "OR-99 NB\nWiden", "All\nImprovements")

infra_sw <- tibble(label = infra_labels, infra = infra_list) |>
  mutate(s = map(infra, \(inf) run_sim_v3(4, infra = inf)),
         t90 = map_dbl(s, "t90"), t95 = map_dbl(s, "t95")) |>
  select(-s, -infra) |>
  mutate(saving_90 = t90[1] - t90, label = factor(label, levels = infra_labels))

cat("\n=== INFRASTRUCTURE SENSITIVITY (v3, Scenario 4, 90th pct) ===\n")
print(infra_sw, n = Inf)

# =============================================================================
# 11. SCENARIO MATRIX
# =============================================================================
s1n <- run_sim_v3(1, "fire_north"); s4n <- run_sim_v3(4, "fire_north")
s1s <- run_sim_v3(1, "fire_south"); s4s <- run_sim_v3(4, "fire_south")

cat("\n=== V3 SCENARIO MATRIX (90th pct clearance, minutes) ===\n")
tibble(
  fire   = fire_lbls,
  S1_t90 = c(s1b$t90, s1n$t90, s1s$t90),
  S4_t90 = c(s4b$t90, s4n$t90, s4s$t90),
  S1_t95 = c(s1b$t95, s1n$t95, s1s$t95),
  S4_t95 = c(s4b$t95, s4n$t95, s4s$t95)
) |> print()

message("\nModel v3 complete. Run ashland_evac_v3_plots.R for comprehensive visualization.")
