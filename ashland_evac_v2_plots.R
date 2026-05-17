# =============================================================================
# Ashland, OR Evacuation Model v2 — Comprehensive Visualization
# =============================================================================
# Standalone script: re-implements the simulation engine with two bug fixes:
#   1. Fallback routing for EMZs whose preferred corridors are fire-blocked
#   2. Capacity-limited (not BPR-penalised) discharge rate
# Then builds a 10-panel analytical figure.
# =============================================================================

library(tidyverse)
library(ggplot2)
library(patchwork)
library(viridis)

# =============================================================================
# 1. PARAMETERS & POPULATION
# =============================================================================
HH_SIZE    <- 2.23
VEH_PER_HH <- 1.43
R_DROP     <- 0.90
BDN_FRAC   <- 0.90

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
  10, "EMZ 10 — Walker Ave/SOU"  ,       1849
) |> mutate(households = round(pop_res / HH_SIZE),
            veh_base   = round(households * VEH_PER_HH))

corridors <- tribble(
  ~corr_id, ~name,                         ~cap_base_vph, ~lanes,
  1, "OR-99 NB → I-5 North",               1700, 1,
  2, "OR-99 SB / I-5 SB on-ramp",          2000, 1,
  3, "OR-66 EB → I-5 interchange",         1900, 2,
  4, "OR-66 East (Klamath Falls)",          1700, 1,
  5, "S Valley View SB"  ,                 1200, 1
) |> mutate(
  cap_vph      = cap_base_vph * lanes,
  cap_drop_vph = cap_vph * R_DROP,
  bdn_thresh   = cap_vph * BDN_FRAC
)

fire_closures <- list(
  bidirectional = integer(0),
  fire_north    = c(1L, 3L),      # fire from N: northbound exits blocked
  fire_south    = c(2L, 4L, 5L)  # fire from S: southbound exits blocked
)

tourist_veh  <- c("1" = round(5150 * .70 / 2.29), "4" = round(5150 * .55 / 2.29))
sou_veh      <- c("1" = 0, "4" = 4435)
school_veh   <- c("1" = 0, "4" = round(75 * 40))
employee_veh <- round(2302 / 1.06)

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

# KLD trip-generation distributions (Section 5 / Appendix D)
dep_A   <- function(t) plogis(t, location = 30,  scale = 10)   # tourists/employees
dep_D   <- function(t) plogis(t, location = 75,  scale = 28)   # residents, no wait
dep_C   <- function(t) plogis(t, location = 135, scale = 38)   # residents, wait commuter
dep_res <- function(t) 0.86 * dep_D(t) + 0.14 * dep_C(t)      # weighted resident

# =============================================================================
# 2. SIMULATION ENGINE
# =============================================================================
run_sim <- function(
    scenario_id  = 1,
    fire_dir     = "bidirectional",
    veh_per_hh   = VEH_PER_HH,
    shadow_comp  = 0.06,
    dur          = 600,
    infra        = list(nevada_bridge = FALSE,
                        mountain_ramps = FALSE,
                        or99_widening  = FALSE)
) {
  scen   <- as.character(scenario_id)
  time_v <- seq(0, dur); n_t <- length(time_v)

  # Demand
  veh_res    <- sum(round(emz$households * veh_per_hh))
  veh_shadow <- round((10099 / HH_SIZE) * veh_per_hh * shadow_comp)
  veh_t  <- tourist_veh[[scen]]
  veh_e  <- employee_veh
  veh_s  <- sou_veh[[scen]]
  veh_sc <- school_veh[[scen]]
  total_veh <- veh_res + veh_shadow + veh_t + veh_e + veh_s + veh_sc

  # Departure rates (fraction per minute step)
  rate_res <- c(0, diff(dep_res(time_v)))
  rate_A   <- c(0, diff(dep_A(time_v)))
  rate_sc  <- c(0, diff(dep_D(time_v)))

  # Capacities (mutable copies)
  corr_cap <- corridors$cap_vph
  corr_bdn <- corridors$bdn_thresh
  corr_dc  <- corridors$cap_drop_vph

  # Infrastructure improvements
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

  # Close fire-blocked corridors
  closed <- fire_closures[[fire_dir]]
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

  # Fallback routing: EMZs with ALL preferred routes blocked → use open corridors
  unrouted <- setdiff(emz$emz_id, unique(ze$emz_id))
  if (length(unrouted) > 0 && length(open_corr) > 0) {
    fallback <- tibble(
      emz_id   = rep(unrouted, each = length(open_corr)),
      corr_id  = rep(open_corr, times = length(unrouted)),
      fraction = rep(open_share, times = length(unrouted))
    )
    ze <- bind_rows(ze, fallback)
  }

  # Build per-corridor inflow matrix
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

  # Non-EMZ populations distributed by open-corridor capacity share
  for (i in seq_along(open_corr)) {
    cid <- open_corr[i]
    inflow_mat[, cid] <- inflow_mat[, cid] +
      (veh_t * rate_A + veh_e * rate_A + veh_s * rate_A + veh_sc * rate_sc) *
      open_share[i]
  }

  # External / pass-through I-5 traffic (dissipates over first 2 h)
  ext_rate <- rep(0, n_t)
  ext_rate[time_v <= 120] <- 8412 / 120 / 60
  for (i in seq_along(open_corr))
    inflow_mat[, open_corr[i]] <- inflow_mat[, open_corr[i]] + ext_rate * open_share[i]

  # Time-stepped queuing — capacity-limited discharge with R-drop at breakdown
  bdn      <- rep(FALSE, n_c)
  queue    <- rep(0,     n_c)
  cleared  <- rep(0,     n_c)
  queued_v  <- cleared_v <- inflow_v <- rep(0, n_t)
  cq_mat   <- cc_mat <- matrix(0, n_t, n_c,
                                dimnames = list(NULL, corridors$name))

  for (i in seq_len(n_t)) {
    queue <- queue + inflow_mat[i, ]

    # Breakdown: triggered when instantaneous inflow rate (vph) exceeds threshold
    flow_vph <- inflow_mat[i, ] * 60
    bdn      <- bdn | (flow_vph > corr_bdn)

    # Discharge: limited by capacity (or capacity-drop if in breakdown)
    eff_cap_pm <- ifelse(bdn, corr_dc, corr_cap) / 60
    dis        <- pmin(queue, pmax(0, eff_cap_pm))
    dis[closed] <- 0

    queue   <- pmax(0, queue  - dis)
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

# =============================================================================
# 3. RUN ALL SCENARIOS
# =============================================================================
message("Running simulations...")
s1b <- run_sim(1, "bidirectional")
s4b <- run_sim(4, "bidirectional")
s1n <- run_sim(1, "fire_north")
s4n <- run_sim(4, "fire_north")
s1s <- run_sim(1, "fire_south")
s4s <- run_sim(4, "fire_south")

# Sensitivity sweeps (Scenario 4)
veh_sw <- tibble(vph = c(1.00, 1.10, 1.20, 1.30, 1.43)) |>
  mutate(s = map(vph, \(v) run_sim(4, veh_per_hh = v)),
         t90 = map_dbl(s, "t90"), t95 = map_dbl(s, "t95"),
         total = map_dbl(s, "total_veh")) |> select(-s)

shad_sw <- tibble(rate = c(0, .03, .06, .15, .30, 1.00)) |>
  mutate(s = map(rate, \(r) run_sim(4, shadow_comp = r)),
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
  mutate(s = map(infra, \(inf) run_sim(4, infra = inf)),
         t90 = map_dbl(s, "t90"), t95 = map_dbl(s, "t95")) |>
  select(-s, -infra) |>
  mutate(saving_90 = t90[1] - t90, label = factor(label, levels = infra_labels))

message("Simulations complete.")
cat(sprintf("  S4 bidir 90th: %d min  |  fire_north: %d min  |  fire_south: %d min\n",
            s4b$t90, s4n$t90, s4s$t90))

# =============================================================================
# 4. BUILD PLOTS
# =============================================================================
clr_blu <- "#2c7bb6"; clr_red <- "#d7191c"; clr_grn <- "#1a9641"
clr_amb <- "#f59c00"; clr_pur <- "#7b3fb5"; clr_tl  <- "#00827f"

fire_pal <- c(
  "Bidirectional"         = clr_blu,
  "Fire from North\n(forced SB)" = clr_red,
  "Fire from South\n(forced NB)" = clr_amb
)
scen_lty <- c("Scenario 1\n(Summer)" = "solid", "Scenario 4\n(Fall, schools)" = "dashed")

# ── Panel A: All clearance curves ──────────────────────────────────────────
curve_df <- bind_rows(
  tibble(time = s1b$time, cleared = s1b$cleared, pct = s1b$cleared / s1b$total_veh,
         scenario = "Scenario 1\n(Summer)", fire = "Bidirectional"),
  tibble(time = s4b$time, cleared = s4b$cleared, pct = s4b$cleared / s4b$total_veh,
         scenario = "Scenario 4\n(Fall, schools)", fire = "Bidirectional"),
  tibble(time = s1n$time, cleared = s1n$cleared, pct = s1n$cleared / s1n$total_veh,
         scenario = "Scenario 1\n(Summer)", fire = "Fire from North\n(forced SB)"),
  tibble(time = s4n$time, cleared = s4n$cleared, pct = s4n$cleared / s4n$total_veh,
         scenario = "Scenario 4\n(Fall, schools)", fire = "Fire from North\n(forced SB)"),
  tibble(time = s1s$time, cleared = s1s$cleared, pct = s1s$cleared / s1s$total_veh,
         scenario = "Scenario 1\n(Summer)", fire = "Fire from South\n(forced NB)"),
  tibble(time = s4s$time, cleared = s4s$cleared, pct = s4s$cleared / s4s$total_veh,
         scenario = "Scenario 4\n(Fall, schools)", fire = "Fire from South\n(forced NB)")
)

milestones <- tibble(
  label    = c("S1 bidir", "S4 bidir", "S1 N", "S4 N", "S1 S", "S4 S"),
  time     = c(s1b$t90, s4b$t90, s1n$t90, s4n$t90, s1s$t90, s4s$t90),
  fire     = c("Bidirectional","Bidirectional",
               "Fire from North\n(forced SB)","Fire from North\n(forced SB)",
               "Fire from South\n(forced NB)","Fire from South\n(forced NB)"),
  pct_val  = 0.90
) |> filter(!is.na(time))

pA <- ggplot(curve_df, aes(x = time, y = pct * 100,
                            colour = fire, linetype = scenario)) +
  geom_line(linewidth = 1.1, alpha = .9) +
  geom_hline(yintercept = 90, linetype = "dotted", colour = "grey50") +
  geom_point(data = milestones,
             aes(x = time, y = pct_val * 100, colour = fire),
             shape = 18, size = 4, show.legend = FALSE) +
  geom_text(data = milestones,
            aes(x = time, y = pct_val * 100 - 5, label = paste0(time, " min")),
            size = 2.8, show.legend = FALSE, colour = "grey20") +
  annotate("text", x = 3, y = 91.5, label = "90th pct threshold",
           hjust = 0, size = 3, colour = "grey40") +
  scale_colour_manual(values = fire_pal) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_continuous(name = "Minutes from Evacuation Order",
                     sec.axis = sec_axis(~ . / 60, name = "Hours"),
                     breaks = seq(0, 600, 60)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Cumulative Evacuation Progress — All Scenarios",
       y     = "% Vehicles Cleared",
       colour = "Fire Direction", linetype = "Scenario") +
  theme_minimal(base_size = 10.5) +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 8),
        plot.title = element_text(face = "bold"))

# ── Panel B: KLD Departure Rate Profiles ──────────────────────────────────
t_seq <- seq(0, 300)
dep_df <- bind_rows(
  tibble(t = t_seq, rate = c(0, diff(dep_A(t_seq)))   * 1000, dist = "Dist A — Tourists/\nEmployees (fast)"),
  tibble(t = t_seq, rate = c(0, diff(dep_D(t_seq)))   * 1000, dist = "Dist D — Residents,\nno wait (86%)"),
  tibble(t = t_seq, rate = c(0, diff(dep_C(t_seq)))   * 1000, dist = "Dist C — Residents,\nawait commuter (14%)"),
  tibble(t = t_seq, rate = c(0, diff(dep_res(t_seq))) * 1000, dist = "Weighted Resident\nComposite")
)
dep_pal <- c("Dist A — Tourists/\nEmployees (fast)"  = clr_grn,
             "Dist D — Residents,\nno wait (86%)"    = clr_blu,
             "Dist C — Residents,\nawait commuter (14%)" = clr_red,
             "Weighted Resident\nComposite"           = clr_pur)

pB <- ggplot(dep_df, aes(x = t, y = rate, colour = dist)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = dep_pal) +
  scale_x_continuous(breaks = seq(0, 300, 60)) +
  labs(title = "KLD Trip-Generation Distributions",
       subtitle = "Per-thousand vehicles departing per minute",
       x = "Minutes from Evacuation Order", y = "Departure Rate\n(veh / 1,000 total / min)",
       colour = NULL) +
  theme_minimal(base_size = 10.5) +
  theme(legend.position = "bottom", legend.text = element_text(size = 7.5),
        plot.title = element_text(face = "bold"))

# ── Panel C: Vehicle Composition — S1 vs S4 ────────────────────────────────
comp_df <- bind_rows(
  enframe(s1b$veh_by_type, name = "type", value = "vehicles") |> mutate(scenario = "Scenario 1\nSummer"),
  enframe(s4b$veh_by_type, name = "type", value = "vehicles") |> mutate(scenario = "Scenario 4\nFall")
) |>
  mutate(type = factor(type,
                        levels = c("school","sou","tourists","employees","shadow","residents"),
                        labels = c("School buses","SOU students","Tourists",
                                   "Employees","Shadow region","Residents")))

pC <- ggplot(comp_df, aes(x = scenario, y = vehicles, fill = type)) +
  geom_col(width = .6) +
  geom_text(aes(label = ifelse(vehicles > 100, format(round(vehicles), big.mark = ","), "")),
            position = position_stack(vjust = 0.5), size = 2.8, colour = "white") +
  scale_fill_manual(values = c("Residents"     = clr_blu, "Shadow region" = "#4da6d5",
                                "Employees"     = clr_grn, "Tourists"     = "#8bc34a",
                                "SOU students"  = clr_pur, "School buses" = clr_amb)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Evacuating Vehicle Composition",
       x = NULL, y = "Vehicles", fill = NULL) +
  theme_minimal(base_size = 10.5) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8),
        plot.title = element_text(face = "bold"))

# ── Panel D: Corridor Queue Heatmap — S4 bidirectional ─────────────────────
hmap_df <- as_tibble(s4b$cq_mat) |>
  mutate(time = s4b$time) |>
  pivot_longer(-time, names_to = "corridor", values_to = "queue") |>
  # shorten corridor labels
  mutate(corridor = str_replace(corridor, "→", "->"),
         corridor = factor(corridor, levels = rev(corridors$name |> str_replace("→","->"))))

pD <- ggplot(hmap_df |> filter(time <= 360),
             aes(x = time, y = corridor, fill = queue)) +
  geom_tile() +
  scale_fill_viridis_c(option = "inferno", name = "Vehicles\nqueued",
                       labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, 360, 60),
                     sec.axis = sec_axis(~ . / 60, name = "Hours")) +
  labs(title = "Corridor Congestion Heatmap",
       subtitle = "Scenario 4 — Bidirectional (first 6 hours)",
       x = "Minutes", y = NULL) +
  theme_minimal(base_size = 10.5) +
  theme(axis.text.y  = element_text(size = 8),
        legend.position = "right",
        plot.title   = element_text(face = "bold"))

# ── Panel E: Fire Direction Impact — Bar Chart ──────────────────────────────
fire_bar_df <- tibble(
  scenario  = rep(c("S1\nSummer", "S4\nFall"), each = 3),
  fire      = rep(c("Bidirectional","Fire North","Fire South"), 2),
  t90       = c(s1b$t90, s1n$t90, s1s$t90,
                s4b$t90, s4n$t90, s4s$t90),
  t95       = c(s1b$t95, s1n$t95, s1s$t95,
                s4b$t95, s4n$t95, s4s$t95)
) |>
  pivot_longer(c(t90, t95), names_to = "pct", values_to = "minutes") |>
  mutate(pct  = recode(pct, t90 = "90th pct", t95 = "95th pct"),
         fire = factor(fire, levels = c("Bidirectional","Fire North","Fire South")),
         hours = round(minutes / 60, 1))

pE <- ggplot(fire_bar_df, aes(x = fire, y = minutes, fill = pct)) +
  geom_col(position = position_dodge(.8), width = .7) +
  geom_text(aes(label = paste0(round(minutes / 60, 1), "h")),
            position = position_dodge(.8), vjust = -.4, size = 2.8) +
  facet_wrap(~ scenario, scales = "free_y") +
  scale_fill_manual(values = c("90th pct" = clr_blu, "95th pct" = clr_red)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 8)) +
  labs(title = "Clearance Time by Fire Direction",
       x = NULL, y = "Minutes", fill = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

# ── Panel F: Per-Corridor Cleared — S4 bidirectional (stacked area) ─────────
corr_clr_df <- as_tibble(s4b$cc_mat) |>
  mutate(time = s4b$time) |>
  pivot_longer(-time, names_to = "corridor", values_to = "cleared") |>
  mutate(corridor = str_replace(corridor, "→", "->"),
         corridor = factor(corridor, levels = corridors$name |> str_replace("→","->")))

pF <- ggplot(corr_clr_df |> filter(time <= 480),
             aes(x = time, y = cleared, fill = corridor)) +
  geom_area(alpha = .85, colour = "white", linewidth = .3) +
  geom_hline(yintercept = 0.90 * s4b$total_veh, linetype = "dashed",
             colour = "grey20", linewidth = .7) +
  annotate("text", x = 5, y = 0.90 * s4b$total_veh + 300,
           label = "90th pct total", hjust = 0, size = 3, colour = "grey30") +
  geom_vline(xintercept = s4b$t90, linetype = "dashed",
             colour = "grey20", linewidth = .7) +
  annotate("text", x = s4b$t90 + 5, y = s4b$total_veh * 0.30,
           label = paste0("t90 = ", s4b$t90, " min"), hjust = 0, size = 3) +
  scale_fill_viridis_d(option = "D", end = .9) +
  scale_x_continuous(name = "Minutes from Evacuation Order",
                     sec.axis = sec_axis(~ . / 60, name = "Hours"),
                     breaks = seq(0, 480, 60)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Vehicles Cleared by Corridor — Scenario 4 Bidirectional",
       y = "Cumulative Vehicles Cleared", fill = NULL) +
  theme_minimal(base_size = 10.5) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8),
        plot.title = element_text(face = "bold"))

# ── Panel G: Infrastructure Sensitivity ────────────────────────────────────
pG <- ggplot(infra_sw |>
               pivot_longer(c(t90, t95), names_to = "pct", values_to = "min") |>
               mutate(pct = recode(pct, t90 = "90th pct", t95 = "95th pct")),
             aes(x = label, y = min, fill = pct)) +
  geom_col(position = position_dodge(.8), width = .7) +
  geom_text(aes(label = paste0(round(min / 60, 1), "h")),
            position = position_dodge(.8), vjust = -.4, size = 2.8) +
  scale_fill_manual(values = c("90th pct" = clr_blu, "95th pct" = clr_red)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  labs(title = "Infrastructure Improvements",
       subtitle = "KLD Appendix J.5 scenarios — Scenario 4",
       x = NULL, y = "Clearance Time (min)", fill = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", axis.text.x = element_text(size = 8),
        plot.title = element_text(face = "bold"))

# ── Panel H: Vehicles per Household Sensitivity ────────────────────────────
pH <- ggplot(veh_sw, aes(x = vph)) +
  geom_ribbon(aes(ymin = t90, ymax = t95), fill = clr_blu, alpha = .15) +
  geom_line(aes(y = t90, colour = "90th pct"), linewidth = 1.2) +
  geom_line(aes(y = t95, colour = "95th pct"), linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = t90, colour = "90th pct"), size = 3) +
  geom_point(aes(y = t95, colour = "95th pct"), size = 3) +
  geom_vline(xintercept = VEH_PER_HH, linetype = "dotted", colour = "grey40") +
  annotate("text", x = VEH_PER_HH + .01, y = max(veh_sw$t95, na.rm=T) * .98,
           label = "Survey\n1.43", hjust = 0, size = 2.8, colour = "grey40") +
  scale_colour_manual(values = c("90th pct" = clr_blu, "95th pct" = clr_red)) +
  labs(title = "Sensitivity:\nVehicles per Household",
       subtitle = "Public messaging impact",
       x = "Veh / Household", y = "Clearance (min)", colour = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# ── Panel I: Shadow Compliance Sensitivity ──────────────────────────────────
pI <- ggplot(shad_sw, aes(x = rate * 100)) +
  geom_ribbon(aes(ymin = t90, ymax = t95), fill = clr_amb, alpha = .15) +
  geom_line(aes(y = t90, colour = "90th pct"), linewidth = 1.2) +
  geom_line(aes(y = t95, colour = "95th pct"), linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = t90, colour = "90th pct"), size = 3) +
  geom_point(aes(y = t95, colour = "95th pct"), size = 3) +
  geom_vline(xintercept = 6, linetype = "dotted", colour = "grey40") +
  annotate("text", x = 7, y = max(shad_sw$t95, na.rm=T) * .98,
           label = "Survey\n6%", hjust = 0, size = 2.8, colour = "grey40") +
  scale_colour_manual(values = c("90th pct" = clr_blu, "95th pct" = clr_red)) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Sensitivity:\nShadow Evacuation Rate",
       subtitle = "Surrounding area voluntary compliance",
       x = "Shadow Compliance (%)", y = "Clearance (min)", colour = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# ── Panel J: Scenario Matrix Heatmap ──────────────────────────────────────
mat_df <- tibble(
  scenario = rep(c("S1 Summer", "S4 Fall"), each = 3),
  fire     = rep(c("Bidirectional", "Fire from\nNorth", "Fire from\nSouth"), 2),
  t90      = c(s1b$t90, s1n$t90, s1s$t90,
               s4b$t90, s4n$t90, s4s$t90)
) |>
  mutate(fire     = factor(fire,     levels = c("Bidirectional","Fire from\nNorth","Fire from\nSouth")),
         scenario = factor(scenario, levels = c("S1 Summer","S4 Fall")),
         label    = paste0(round(t90 / 60, 1), "h\n(", t90, " min)"))

pJ <- ggplot(mat_df, aes(x = fire, y = scenario, fill = t90)) +
  geom_tile(colour = "white", linewidth = 1.5) +
  geom_text(aes(label = label), size = 3.2, fontface = "bold", colour = "white") +
  scale_fill_gradient2(low = clr_grn, mid = clr_amb, high = clr_red,
                       midpoint = median(mat_df$t90, na.rm = TRUE),
                       name = "90th pct\n(minutes)",
                       na.value = "grey80") +
  labs(title = "Scenario × Fire Direction Matrix",
       subtitle = "90th percentile clearance time",
       x = "Fire Direction", y = NULL) +
  theme_minimal(base_size = 10.5) +
  theme(legend.position = "right", plot.title = element_text(face = "bold"),
        axis.text = element_text(size = 9))

# =============================================================================
# 5. COMPOSE & SAVE
# =============================================================================
layout <- "
AAAABBCC
AAAADDEE
FFFFGGHH
FFFFIJJJ
"

combined <- pA + pB + pC + pD + pE + pF + pG + pH + pI + pJ +
  plot_layout(design = layout) +
  plot_annotation(
    title    = "City of Ashland, OR — Wildfire Evacuation Model v2  |  Comprehensive Analysis",
    subtitle = paste0(
      "KLD TR-1217 informed  •  10 EMZs  •  HCM 2016 capacities (R = 0.90 drop)  •  ",
      "Multi-population departure curves  •  Scenario 4 peak demand: ",
      format(s4b$total_veh, big.mark = ","), " vehicles"
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size  = 9, colour = "grey40"),
      plot.background = element_rect(fill = "grey97", colour = NA)
    )
  )

ggsave("ashland_evacuation_results_v2.png", combined,
       width = 20, height = 15, dpi = 150)
message("Saved: ashland_evacuation_results_v2.png")
