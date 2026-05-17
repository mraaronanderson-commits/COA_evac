# =============================================================================
# Ashland Evacuation Model — v2 vs v3 Single-Page Comparison
# =============================================================================
# Produces:  ashland_v2_v3_comparison.png
#
# Four-panel layout on one page:
#   A (top-left)    — Parameter changes summary table
#   B (top-right)   — Vehicle composition: what was removed and why
#   C (bottom-left) — Clearance time: v2 vs v3 across all scenarios
#   D (bottom-right)— Clearance curves: S4 bidirectional v2 vs v3 overlay
# =============================================================================

library(tidyverse)
library(ggplot2)
library(patchwork)

# =============================================================================
# 1. SHARED CONSTANTS
# =============================================================================
HH_SIZE    <- 2.23
VEH_PER_HH <- 1.43
R_DROP     <- 0.90
BDN_FRAC   <- 0.90

emz <- tribble(
  ~emz_id, ~pop_res,
  1,1800, 2,1850, 3,2100, 4,2800, 5,1750,
  6,2400, 7,2300, 8,2650, 9,1950, 10,1849
) |> mutate(households = round(pop_res / HH_SIZE))

corridors <- tribble(
  ~corr_id, ~name,                         ~cap_base_vph, ~lanes,
  1, "OR-99 NB -> I-5 North",              1700, 1,
  2, "OR-99 SB / I-5 SB on-ramp",         2000, 1,
  3, "OR-66 EB -> I-5 interchange",        1900, 2,
  4, "OR-66 East (Klamath Falls)",         1700, 1,
  5, "S Valley View SB",                   1200, 1
) |> mutate(cap_vph = cap_base_vph * lanes,
            cap_drop_vph = cap_vph * R_DROP,
            bdn_thresh   = cap_vph * BDN_FRAC)

fire_closures <- list(
  bidirectional = integer(0),
  fire_north    = c(1L, 3L),
  fire_south    = c(2L, 4L, 5L)
)

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

tourist_veh <- c("1" = round(5150 * .70 / 2.29), "4" = round(5150 * .55 / 2.29))
school_veh  <- c("1" = 0L, "4" = round(75 * 40))

dep_A   <- function(t) plogis(t, location = 30,  scale = 10)
dep_D   <- function(t) plogis(t, location = 75,  scale = 28)
dep_C   <- function(t) plogis(t, location = 135, scale = 38)
dep_res <- function(t) 0.86 * dep_D(t) + 0.14 * dep_C(t)

# --- Version-specific parameters --------------------------------------------
params <- list(
  v2 = list(sou = c("1" = 0L, "4" = 4435L),         emp = round(2302 / 1.06)),
  v3 = list(sou = c("1" = 0L, "4" = round(4435 * 0.50)), emp = round((2302 - 200) / 1.06))
)

# =============================================================================
# 2. SIMULATION ENGINE
# =============================================================================
run_sim <- function(scenario_id = 1, fire_dir = "bidirectional",
                    veh_per_hh = VEH_PER_HH, shadow_comp = 0.06,
                    dur = 600, sou_veh, employee_veh) {
  scen   <- as.character(scenario_id)
  time_v <- seq(0, dur); n_t <- length(time_v)

  veh_res    <- sum(round(emz$households * veh_per_hh))
  veh_shadow <- round((10099 / HH_SIZE) * veh_per_hh * shadow_comp)
  veh_t  <- tourist_veh[[scen]]; veh_e <- employee_veh
  veh_s  <- sou_veh[[scen]];     veh_sc <- school_veh[[scen]]
  total_veh <- veh_res + veh_shadow + veh_t + veh_e + veh_s + veh_sc

  rate_res <- c(0, diff(dep_res(time_v)))
  rate_A   <- c(0, diff(dep_A(time_v)))
  rate_sc  <- c(0, diff(dep_D(time_v)))

  corr_cap <- corridors$cap_vph
  corr_bdn <- corridors$bdn_thresh
  corr_dc  <- corridors$cap_drop_vph

  closed    <- fire_closures[[fire_dir]]
  if (length(closed)) corr_cap[closed] <- 0
  open_corr  <- setdiff(seq_len(nrow(corridors)), closed)
  open_share <- corr_cap[open_corr] / sum(corr_cap[open_corr])

  ze <- emz_cs
  if (length(closed)) {
    ze <- ze |> filter(!corr_id %in% closed) |>
      group_by(emz_id) |> mutate(fraction = fraction / sum(fraction)) |> ungroup()
  }
  unrouted <- setdiff(emz$emz_id, unique(ze$emz_id))
  if (length(unrouted) > 0 && length(open_corr) > 0) {
    ze <- bind_rows(ze, tibble(
      emz_id   = rep(unrouted, each = length(open_corr)),
      corr_id  = rep(open_corr, times = length(unrouted)),
      fraction = rep(open_share, times = length(unrouted))
    ))
  }

  n_c        <- nrow(corridors)
  inflow_mat <- matrix(0, n_t, n_c)
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
      (veh_t * rate_A + veh_e * rate_A + veh_s * rate_A + veh_sc * rate_sc) * open_share[i]
  }
  ext_rate <- rep(0, n_t); ext_rate[time_v <= 120] <- 8412 / 120 / 60
  for (i in seq_along(open_corr))
    inflow_mat[, open_corr[i]] <- inflow_mat[, open_corr[i]] + ext_rate * open_share[i]

  bdn <- rep(FALSE, n_c); queue <- cleared <- rep(0, n_c)
  cleared_v <- rep(0, n_t)

  for (i in seq_len(n_t)) {
    queue    <- queue + inflow_mat[i, ]
    bdn      <- bdn | (inflow_mat[i, ] * 60 > corr_bdn)
    dis      <- pmin(queue, pmax(0, ifelse(bdn, corr_dc, corr_cap) / 60))
    dis[closed] <- 0
    queue    <- pmax(0, queue - dis)
    cleared  <- cleared + dis
    cleared_v[i] <- sum(cleared)
  }

  t90 <- time_v[which(cleared_v >= 0.90 * total_veh)[1]]
  t95 <- time_v[which(cleared_v >= 0.95 * total_veh)[1]]
  list(cleared = cleared_v, time = time_v, t90 = t90, t95 = t95,
       total_veh = total_veh,
       veh_by_type = c(residents = veh_res, shadow = veh_shadow,
                       tourists  = veh_t,   employees = veh_e,
                       sou       = veh_s,   school = veh_sc))
}

# =============================================================================
# 3. RUN ALL NEEDED SIMULATIONS
# =============================================================================
message("Running simulations...")
fire_dirs <- c("bidirectional", "fire_north", "fire_south")

sims <- list()
for (ver in c("v2", "v3")) {
  for (scen in c(1, 4)) {
    for (fd in fire_dirs) {
      key <- paste(ver, scen, fd, sep = "_")
      sims[[key]] <- run_sim(
        scenario_id  = scen,
        fire_dir     = fd,
        sou_veh      = params[[ver]]$sou,
        employee_veh = params[[ver]]$emp
      )
    }
  }
}
message("Done.")

# =============================================================================
# 4. COLOURS & SHARED THEME
# =============================================================================
clr_v2  <- "#888888"   # grey  — v2 baseline
clr_v3  <- "#2c7bb6"   # blue  — v3 updated
clr_red <- "#d7191c"
clr_grn <- "#1a9641"
clr_amb <- "#f59c00"

base_theme <- theme_minimal(base_size = 11) +
  theme(plot.title   = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9, colour = "grey40"))

# =============================================================================
# 5. PANEL A — Parameter Changes Table
# =============================================================================
tbl_data <- tibble(
  Parameter     = c("SOU student vehicles (S4)",
                    "Employee / commuter vehicles",
                    "Total peak vehicles (S4, bidir)",
                    "Total peak vehicles (S1, bidir)"),
  v2_val        = c(
    format(params$v2$sou[["4"]], big.mark = ","),
    format(params$v2$emp,       big.mark = ","),
    format(sims$v2_4_bidirectional$total_veh, big.mark = ","),
    format(sims$v2_1_bidirectional$total_veh, big.mark = ",")
  ),
  v3_val        = c(
    format(params$v3$sou[["4"]], big.mark = ","),
    format(params$v3$emp,       big.mark = ","),
    format(sims$v3_4_bidirectional$total_veh, big.mark = ","),
    format(sims$v3_1_bidirectional$total_veh, big.mark = ",")
  ),
  delta_raw     = c(
    params$v3$sou[["4"]] - params$v2$sou[["4"]],
    params$v3$emp        - params$v2$emp,
    sims$v3_4_bidirectional$total_veh - sims$v2_4_bidirectional$total_veh,
    sims$v3_1_bidirectional$total_veh - sims$v2_1_bidirectional$total_veh
  )
) |>
  mutate(
    delta_str = paste0(ifelse(delta_raw > 0, "+", ""), format(delta_raw, big.mark = ",")),
    row_id    = seq_len(n())
  )

reason_data <- tibble(
  row_id = c(1, 2),
  reason = c(
    "SOU enrollment has declined ~50% since the 2021 KLD study",
    "Asante Ashland Community Hospital closing to small ER/outpatient;\n~200 fewer employee commuters"
  )
)

# Build table as ggplot using geom_text on a blank canvas
col_x <- c(0.01, 0.38, 0.53, 0.63)   # Parameter | V2 | V3 | Delta
col_names <- c("Parameter", "v2", "v3", "Change")
header_y  <- 4.7
row_ys    <- c(3.9, 3.0, 2.1, 1.35)
reason_ys <- c(3.55, 2.65)

pA <- ggplot() +
  # Column headers
  annotate("text", x = col_x, y = header_y,
           label = col_names,
           hjust = c(0, 0.5, 0.5, 0.5), fontface = "bold", size = 3.4,
           colour = "grey20") +
  annotate("segment", x = 0.01, xend = 0.99, y = header_y - 0.18,
           yend = header_y - 0.18, colour = "grey50", linewidth = 0.6) +
  # Data rows
  geom_text(data = tbl_data,
            aes(x = col_x[1], y = row_ys[row_id], label = Parameter),
            hjust = 0, size = 3.1, colour = "grey15", inherit.aes = FALSE) +
  geom_text(data = tbl_data,
            aes(x = col_x[2], y = row_ys[row_id], label = v2_val),
            hjust = 0.5, size = 3.1, colour = "grey40", inherit.aes = FALSE) +
  geom_text(data = tbl_data,
            aes(x = col_x[3], y = row_ys[row_id], label = v3_val),
            hjust = 0.5, size = 3.1, colour = clr_v3,
            fontface = "bold", inherit.aes = FALSE) +
  geom_text(data = tbl_data,
            aes(x = col_x[4], y = row_ys[row_id], label = delta_str,
                colour = delta_raw < 0),
            hjust = 0.5, size = 3.1, fontface = "bold", inherit.aes = FALSE) +
  scale_colour_manual(values = c(`TRUE` = clr_grn, `FALSE` = clr_red), guide = "none") +
  # Reason annotations for rows 1 & 2
  geom_text(data = reason_data,
            aes(x = col_x[1] + 0.02, y = reason_ys[row_id], label = reason),
            hjust = 0, size = 2.6, colour = "grey50",
            lineheight = 0.85, inherit.aes = FALSE) +
  # Thin separator between rows
  annotate("segment", x = 0.01, xend = 0.99,
           y = c(row_ys - 0.38), yend = c(row_ys - 0.38),
           colour = "grey88", linewidth = 0.3) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0.8, 5.1)) +
  labs(title    = "What Changed: v2 → v3",
       subtitle = "Green = reduction (fewer vehicles, faster clearance)") +
  theme_void(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", size = 11, margin = margin(b = 4)),
        plot.subtitle = element_text(size = 9, colour = "grey45"),
        plot.margin   = margin(8, 8, 4, 8))

# =============================================================================
# 6. PANEL B — Vehicle Composition Shift
# =============================================================================
comp_df <- bind_rows(
  enframe(sims$v2_4_bidirectional$veh_by_type, name = "type", value = "vehicles") |>
    mutate(version = "v2", scenario = "Fall (S4)"),
  enframe(sims$v3_4_bidirectional$veh_by_type, name = "type", value = "vehicles") |>
    mutate(version = "v3", scenario = "Fall (S4)"),
  enframe(sims$v2_1_bidirectional$veh_by_type, name = "type", value = "vehicles") |>
    mutate(version = "v2", scenario = "Summer (S1)"),
  enframe(sims$v3_1_bidirectional$veh_by_type, name = "type", value = "vehicles") |>
    mutate(version = "v3", scenario = "Summer (S1)")
) |>
  mutate(
    type = factor(type,
                  levels = c("school","sou","tourists","employees","shadow","residents"),
                  labels = c("School buses","SOU students","Tourists",
                             "Employees","Shadow region","Residents")),
    group = paste0(scenario, "\n", version),
    group = factor(group, levels = c("Summer (S1)\nv2","Summer (S1)\nv3",
                                     "Fall (S4)\nv2",  "Fall (S4)\nv3"))
  )

type_pal <- c("Residents"     = "#2c7bb6", "Shadow region" = "#4da6d5",
              "Employees"     = "#1a9641", "Tourists"      = "#8bc34a",
              "SOU students"  = "#7b3fb5", "School buses"  = "#f59c00")

# Total label on top of each bar
totals_df <- comp_df |>
  group_by(group) |>
  summarise(total = sum(vehicles), .groups = "drop")

pB <- ggplot(comp_df, aes(x = group, y = vehicles, fill = type)) +
  geom_col(width = .72, colour = NA) +
  geom_text(aes(label = ifelse(vehicles > 150,
                               format(round(vehicles), big.mark = ","), "")),
            position = position_stack(vjust = 0.5),
            size = 2.6, colour = "white", fontface = "bold") +
  geom_text(data = totals_df,
            aes(x = group, y = total + 300, label = format(total, big.mark = ",")),
            inherit.aes = FALSE, size = 2.9, colour = "grey20", fontface = "bold") +
  geom_vline(xintercept = 2.5, linetype = "dashed", colour = "grey60", linewidth = .5) +
  scale_fill_manual(values = type_pal) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, .1))) +
  labs(title    = "Vehicle Composition by Version",
       subtitle = "Numbers inside bars = vehicles of that type",
       x = NULL, y = "Total Evacuating Vehicles", fill = NULL) +
  base_theme +
  theme(legend.position = "bottom",
        legend.key.size  = unit(3.5, "mm"),
        legend.text      = element_text(size = 7.5),
        axis.text.x      = element_text(size = 9))

# =============================================================================
# 7. PANEL C — Clearance Time Comparison (all scenarios)
# =============================================================================
fire_order <- c("Bidirectional", "Fire North\n(forced SB)", "Fire South\n(forced NB)")
fire_keys  <- c("bidirectional", "fire_north", "fire_south")

clr_df <- bind_rows(
  lapply(seq_along(fire_keys), function(fi) {
    bind_rows(
      lapply(c("1","4"), function(sc) {
        key_v2 <- paste0("v2_", sc, "_", fire_keys[fi])
        key_v3 <- paste0("v3_", sc, "_", fire_keys[fi])
        tibble(
          fire     = fire_order[fi],
          scenario = ifelse(sc == "1", "S1 Summer", "S4 Fall"),
          v2_t90   = sims[[key_v2]]$t90,
          v3_t90   = sims[[key_v3]]$t90
        )
      })
    )
  })
) |>
  pivot_longer(c(v2_t90, v3_t90), names_to = "version", values_to = "t90") |>
  mutate(
    version  = recode(version, v2_t90 = "v2  (KLD baseline)", v3_t90 = "v3  (SOU -50%, hospital closure)"),
    fire     = factor(fire, levels = fire_order),
    scenario = factor(scenario, levels = c("S1 Summer", "S4 Fall")),
    label    = paste0(round(t90 / 60, 1), "h")
  )

delta_c <- clr_df |>
  pivot_wider(names_from = version, values_from = c(t90, label)) |>
  rename_with(~ gsub("t90_", "", .x)) |>
  mutate(
    delta      = `v3  (SOU -50%, hospital closure)` - `v2  (KLD baseline)`,
    y_ann      = pmax(`v2  (KLD baseline)`, `v3  (SOU -50%, hospital closure)`, na.rm = TRUE) + 14,
    delta_lbl  = paste0(ifelse(delta < 0, "−", "+"), abs(delta), " min")
  )

pC <- ggplot(clr_df, aes(x = fire, y = t90,
                          fill  = version,
                          group = interaction(fire, version))) +
  geom_col(position = position_dodge(.75), width = .65) +
  geom_text(aes(label = label, y = t90 + 8),
            position = position_dodge(.75), size = 2.6, colour = "grey20") +
  geom_text(data = delta_c,
            aes(x = fire, y = y_ann + 10, label = delta_lbl,
                colour = delta < 0),
            inherit.aes = FALSE, size = 2.6, fontface = "bold") +
  scale_colour_manual(values = c(`TRUE` = clr_grn, `FALSE` = clr_red), guide = "none") +
  facet_wrap(~ scenario, ncol = 2) +
  scale_fill_manual(values = c("v2  (KLD baseline)"               = clr_v2,
                                "v3  (SOU -50%, hospital closure)" = clr_v3)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  scale_y_continuous(
    name     = "90th Pct Clearance (min)",
    sec.axis = sec_axis(~ . / 60, name = "Hours"),
    expand   = expansion(mult = c(0, .15))
  ) +
  labs(title    = "90th Percentile Clearance Time: v2 vs v3",
       subtitle = "Delta labels: green = v3 faster, red = v3 slower",
       x = NULL, fill = NULL) +
  base_theme +
  theme(legend.position  = "bottom",
        legend.key.size  = unit(3.5, "mm"),
        strip.text       = element_text(face = "bold", size = 10))

# =============================================================================
# 8. PANEL D — Clearance Curve Overlay (S4 bidirectional)
# =============================================================================
curve_df <- bind_rows(
  tibble(time    = sims$v2_4_bidirectional$time,
         cleared = sims$v2_4_bidirectional$cleared,
         pct     = sims$v2_4_bidirectional$cleared / sims$v2_4_bidirectional$total_veh,
         version = "v2"),
  tibble(time    = sims$v3_4_bidirectional$time,
         cleared = sims$v3_4_bidirectional$cleared,
         pct     = sims$v3_4_bidirectional$cleared / sims$v3_4_bidirectional$total_veh,
         version = "v3")
)

t90_v2 <- sims$v2_4_bidirectional$t90
t90_v3 <- sims$v3_4_bidirectional$t90
t95_v2 <- sims$v2_4_bidirectional$t95
t95_v3 <- sims$v3_4_bidirectional$t95
gain_90 <- t90_v2 - t90_v3  # positive = v3 is faster

pD <- ggplot(curve_df, aes(x = time, y = pct * 100, colour = version)) +
  geom_line(linewidth = 1.4) +
  # 90th pct threshold
  geom_hline(yintercept = 90, linetype = "dotted", colour = "grey50") +
  annotate("text", x = 3, y = 91.5, label = "90th pct",
           hjust = 0, size = 2.9, colour = "grey40") +
  # 95th pct threshold
  geom_hline(yintercept = 95, linetype = "dotted", colour = "grey50") +
  annotate("text", x = 3, y = 96.4, label = "95th pct",
           hjust = 0, size = 2.9, colour = "grey40") +
  # v2 milestone verticals
  geom_vline(xintercept = t90_v2, linetype = "dashed", colour = clr_v2, linewidth = .8) +
  geom_vline(xintercept = t90_v3, linetype = "dashed", colour = clr_v3, linewidth = .8) +
  # Horizontal arrow / bracket showing time saving at 90th pct
  annotate("segment", x = t90_v3, xend = t90_v2, y = 88, yend = 88,
           arrow = arrow(ends = "both", length = unit(3, "pt"), type = "open"),
           colour = clr_grn, linewidth = 1) +
  annotate("text",
           x     = (t90_v2 + t90_v3) / 2,
           y     = 86.2,
           label = paste0("−", gain_90, " min\nat 90th pct"),
           size  = 2.9, colour = clr_grn, fontface = "bold") +
  # Point labels
  annotate("text", x = t90_v2 + 4, y = 83,
           label = paste0("v2: ", t90_v2, " min\n(", round(t90_v2/60,1), "h)"),
           hjust = 0, size = 2.7, colour = clr_v2) +
  annotate("text", x = t90_v3 - 4, y = 76,
           label = paste0("v3: ", t90_v3, " min\n(", round(t90_v3/60,1), "h)"),
           hjust = 1, size = 2.7, colour = clr_v3, fontface = "bold") +
  scale_colour_manual(values = c(v2 = clr_v2, v3 = clr_v3),
                      labels = c(v2 = "v2  (KLD baseline)",
                                 v3 = "v3  (SOU −50%, hospital)")) +
  scale_x_continuous(name   = "Minutes from Evacuation Order",
                     sec.axis = sec_axis(~ . / 60, name = "Hours"),
                     breaks = seq(0, 600, 60)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 100)) +
  labs(title    = "Clearance Curve Overlay — Scenario 4 (Fall), Bidirectional",
       subtitle = paste0("v3 total vehicles: ",
                         format(sims$v3_4_bidirectional$total_veh, big.mark = ","),
                         "  vs  v2: ",
                         format(sims$v2_4_bidirectional$total_veh, big.mark = ","),
                         "  (Δ ",
                         format(sims$v3_4_bidirectional$total_veh -
                                  sims$v2_4_bidirectional$total_veh, big.mark = ","),
                         " vehicles)"),
       y = "% Vehicles Cleared", colour = NULL) +
  base_theme +
  theme(legend.position  = "bottom",
        legend.key.size  = unit(4, "mm"))

# =============================================================================
# 9. COMPOSE & SAVE
# =============================================================================
combined <- (pA | pB) / (pC | pD) +
  plot_layout(heights = c(1, 1.1)) +
  plot_annotation(
    title    = "City of Ashland, OR — Wildfire Evacuation Model: v2 vs v3 Comparison",
    subtitle = paste0(
      "v3 reflects two 2024–2025 real-world changes: ",
      "SOU enrollment decline (~50%) and Asante hospital closure to small ER/outpatient  |  ",
      "All other parameters, road network, and departure distributions unchanged from v2 (KLD TR-1217)"
    ),
    caption  = "Model: macroscopic queuing, HCM 2016 capacities, R=0.90 capacity drop, KLD-calibrated departure distributions",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 14),
      plot.subtitle   = element_text(size  = 9, colour = "grey35", margin = margin(b = 4)),
      plot.caption    = element_text(size  = 7.5, colour = "grey55", hjust = 0),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin     = margin(10, 10, 6, 10)
    )
  )

out <- "ashland_v2_v3_comparison.png"
ggsave(out, combined, width = 16, height = 10, dpi = 180, bg = "white")
message("Saved: ", out)

# =============================================================================
# 10. CONSOLE SUMMARY
# =============================================================================
cat("\n")
cat("══════════════════════════════════════════════════════════════════\n")
cat("  v2 → v3 PARAMETER DELTA\n")
cat("══════════════════════════════════════════════════════════════════\n")
cat(sprintf("  SOU vehicles (S4)      : %4d  →  %4d  (%+d, %.0f%%)\n",
            params$v2$sou[["4"]], params$v3$sou[["4"]],
            params$v3$sou[["4"]] - params$v2$sou[["4"]],
            100*(params$v3$sou[["4"]] - params$v2$sou[["4"]])/params$v2$sou[["4"]]))
cat(sprintf("  Employee vehicles      : %4d  →  %4d  (%+d)\n",
            params$v2$emp, params$v3$emp, params$v3$emp - params$v2$emp))
cat(sprintf("  Total S4 peak demand   : %4d  →  %4d  (%+d vehicles)\n",
            sims$v2_4_bidirectional$total_veh,
            sims$v3_4_bidirectional$total_veh,
            sims$v3_4_bidirectional$total_veh - sims$v2_4_bidirectional$total_veh))
cat("\n")
cat("  90th pct clearance (bidirectional):\n")
cat(sprintf("    Scenario 1 Summer : %d min (%.1fh)  →  %d min (%.1fh)  [%+d min]\n",
            t90_v2 <- sims$v2_1_bidirectional$t90, t90_v2/60,
            t90_v3 <- sims$v3_1_bidirectional$t90, t90_v3/60, t90_v3 - t90_v2))
cat(sprintf("    Scenario 4 Fall   : %d min (%.1fh)  →  %d min (%.1fh)  [%+d min]\n",
            t90_v2 <- sims$v2_4_bidirectional$t90, t90_v2/60,
            t90_v3 <- sims$v3_4_bidirectional$t90, t90_v3/60, t90_v3 - t90_v2))
cat("══════════════════════════════════════════════════════════════════\n")
