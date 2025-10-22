
library(tidyverse)
# -------------------------------------------------------------------
# Example: two trajectories up to tau = 24 months
# States: 0 = remission, 1 = relapse, 2 = death (absorbing)
# -------------------------------------------------------------------
tau <- 24

# y-positions for the two groups (rows in the plot)
ypos <- tibble(group = c("Control", "Treatment"),
               y     = c(1, 2))

# Follow-up lines: end at death time (Control) or tau (Treatment)
lines_df <- tibble(
  group = c("Control", "Treatment"),
  start = c(0, 0),
  end   = c(12, 24)  # Control dies at 12; Treatment alive through tau
) %>%
  left_join(ypos, by = "group")

# Event times and types (shapes will encode event type)
events_df <- tribble(
  ~group,      ~time, ~event,     ~state,
  "Control",        10, "Relapse",     1,
  "Control",       12, "Death",       2,
  "Treatment",     6, "Relapse",     1
) %>%
  left_join(ypos, by = "group")

# Win/Loss shading "between events", per patient row
# Intervals where Treatment is in a more favorable state than Control:
# [6,10): Trt 0 vs Ctl 1  => Win for Treatment, Loss for Control
# [12,24]: Trt 1 vs Ctl 2 => Win for Treatment, Loss for Control
winloss_df <- tribble(
  ~group,      ~xmin, ~xmax, ~outcome,
  "Treatment",     6,    10,  "Loss",
  "Control",       6,    10,  "Win",
  "Treatment",    12,    24,  "Win",
  "Control",      12,    24,  "Loss"
) %>%
  left_join(ypos, by = "group") %>%
  mutate(ymin = y - 0.25,
         ymax = y + 0.25)

# Build plot
p <- ggplot() +
  # Shaded win/loss bands per patient row
  geom_rect(data = winloss_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = outcome),
            alpha = 0.3, color = NA) +
  # Patient follow-up lines (terminate at death)
  geom_segment(data = lines_df,
               aes(x = start, xend = end, y = y, yend = y),
               linewidth = 0.6, lineend = "round") +
  # Event points: relapse vs death (different shapes)
  geom_point(data = events_df,
             aes(x = time, y = y, shape = event),
             size = 3, stroke = 1.1) +
  # Optional vertical guides at event boundaries
  geom_vline(xintercept = c(6, 10, 12, tau), linetype = "dotted", linewidth = 0.3) +
  # Axes and scales
  scale_x_continuous("Time (months)",
                     limits = c(0, tau),
                     breaks = seq(0, tau, by = 6),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(NULL, breaks = 1:2, limits = c(0.5, 2.5), 
                     labels = c(expression(Y[0](t)), expression(Y[1](t)))) +
  # filled square for nonfatal, dot for death
  scale_shape_manual(values = c(Relapse = 15, Death = 16)) +  # 16 = filled circle,
  scale_fill_manual(values = c(Win = "#66BB6A", Loss = "#EF5350")) + # green vs red
  guides(linetype = "none") +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    # plot.margin = margin(10, 20, 10, 40),
    axis.text.y = element_text(size = 10),
    axis.line.y = element_line(linewidth = 0.8)
  ) 

p

# If you want to save:
ggsave("images/comp_rmtif_lp.eps", p, width = 8, height = 3.0, device = cairo_ps)
ggsave("images/comp_rmtif_lp.png", p, width = 8, height = 3.0, dpi = 300)

## Win ratio illustration

line_df <- tibble(
  subj = fct(rep(c("A", "B"), 3)),
  xstart = rep(c(0, 0), 3),
  xend   = c(12, 9, 14, 11, 14, 9),
  case = fct(rep(c("Win on death", "Win on hospitalization", "Tie"), each = 2))
)

line_cut_df <- line_df |> 
  group_by(
    case
  ) |> 
  summarize(
    min_end = min(xend)
  )

dots_df <- tibble(
  subj = fct(c(rep("A", 2), rep("B", 2), 
               rep("A", 2), rep("B", 2), 
               rep("A", 2), "B")),
  time = c(3, 12, 5, 9, 
           7, 14, 5, 11,
           12, 14, 9),
  event= c("Hospitalization", "Censoring", "Hospitalization", "Death",
           "Hospitalization", "Death", "Hospitalization", "Censoring",
           "Hospitalization", "Death", "Censoring"),
  events = fct(event, levels = c("Censoring", "Hospitalization", "Death")),
  case = fct(c(rep("Win on death", 4),
               rep("Win on hospitalization", 4),
               rep("Tie", 3))
  )
)

p <- ggplot() +
  geom_segment(data = line_df, aes(x = xstart, xend = xend, y = subj, yend = subj),
               linewidth = 0.6, lineend = "round") +
  # Optional vertical guides at event boundaries
  geom_vline(data = line_cut_df, aes(xintercept = min_end), linetype = "dotted", linewidth = 0.3) +
  geom_point(data = dots_df, aes(x = time, y = subj, shape = event), size = 3, stroke = 1.1, fill = "white") +
  facet_wrap(~case, ncol = 1) +
  # Axes and scales
  scale_x_continuous(NULL, breaks = NULL, expand = expansion(c(0, 0.1))) +
  scale_y_discrete(NULL, limits = c("B", "A")) +
  # filled square for nonfatal, dot for death
  scale_shape_manual(values = c(Hospitalization = 15, Death = 16, Censoring = 23)) +  # 16 = filled circle,
  coord_cartesian(clip = "off") +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    strip.background = element_blank(),
    strip.text = element_text(size = 11, hjust = 0, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.line.y = element_line(linewidth = 0.8),
    axis.line.x = element_blank(),
    axis.title.x = element_blank()
  ) 

p

# Save (vector + raster)
ggsave("images/comp_wr_pc.eps", p, width = 8, height = 5, device = cairo_ps)
ggsave("images/comp_wr_pc.png", p, width = 8, height = 5)


# ------------------------------------------------------------------
# Helper maps
# ------------------------------------------------------------------
subj_levels <- c("B", "A")  # show B on bottom, A on top in plots
subj_y      <- c(A = 1, B = 0)

# convenience constructors
make_segment <- function(scn, subj, end_time) {
  tibble(
    scenario = scn,
    subj     = fct(subj, levels = c("A", "B")),
    y        = subj_y[as.character(subj)],
    xstart   = 0,
    xend     = end_time
  )
}
make_events <- function(scn, subj, times, type) {
  tibble(
    scenario = scn,
    subj     = fct(subj, levels = c("A", "B")),
    y        = subj_y[as.character(subj)],
    time     = times,
    event    = type
  )
}

# ------------------------------------------------------------------
# Scenarios (times chosen to emulate the EPS layout)
# S1 TL: PWR B wins (later first), RWR B wins (fewer cumulative)
# S2 TR: PWR A wins (later first), RWR B wins (fewer cumulative)
# S3 BL: PWR A wins (later first), RWR A wins (later last occurrence)
# S4 BR: PWR B wins (later first), RWR A wins (later last occurrence)
# ------------------------------------------------------------------

# S1
s1_seg <- bind_rows(
  make_segment("(a)", "A", 12.5),
  make_segment("(a)", "B", 10.0)
)

s1_evt <- bind_rows(
  make_events("(a)", "A", c(2.0, 4.3, 6.2, 8.0, 9.2), "Hospitalization"),
  make_events("(a)", "A", 12.5, "Death"),
  make_events("(a)", "B", c(2.5, 5.0, 7.5), "Hospitalization"),
  make_events("(a)", "B", 10.0, "Censoring")
)

s1_cut <- tibble(scenario = "(a)", xcut = 10.0)

# S2
s2_seg <- bind_rows(
  make_segment("(b)", "A", 12.5),
  make_segment("(b)", "B", 10.0)
)
s2_evt <- bind_rows(
  make_events("(b)", "A", c(3.0, 5.0, 7.2, 8.5), "Hospitalization"),
  make_events("(b)", "A", 12.5, "Death"),
  make_events("(b)", "B", c(2.0, 6.8, 9.0), "Hospitalization"),
  make_events("(b)", "B", 10.0, "Censoring")
)
s2_cut <- tibble(scenario = "(b)", xcut = 10.0)

# S3
s3_seg <- bind_rows(
  make_segment("(c)", "A", 12.5),
  make_segment("(c)", "B", 10)
)
s3_evt <- bind_rows(
  make_events("(c)", "A", c(3.2, 7.0, 9.0), "Hospitalization"),
  make_events("(c)", "A", 12.5, "Death"),
  make_events("(c)", "B", c(2.0, 6.0, 8.0), "Hospitalization"),
  make_events("(c)", "B", 10, "Censoring")
)
s3_cut <- tibble(scenario = "(c)", xcut = 10)

# S4
s4_seg <- bind_rows(
  make_segment("(d)", "A", 12.5),
  make_segment("(d)", "B", 10)
)
s4_evt <- bind_rows(
  make_events("(d)", "A", c(3.0, 6.0, 9.0), "Hospitalization"),
  make_events("(d)", "A", 12.5, "Death"),
  make_events("(d)", "B", c(5.0, 7.0, 8.0), "Hospitalization"),
  make_events("(d)", "B", 10, "Censoring")
)
s4_cut <- tibble(scenario = "(d)", xcut = 10)

# bind all
line_df <- bind_rows(s1_seg, s2_seg, s3_seg, s4_seg) |>
  mutate(scenario = fct(scenario, levels = c("(a)", "(b)", "(c)", "(d)")))
dots_df <- bind_rows(s1_evt, s2_evt, s3_evt, s4_evt) |>
  mutate(
    event = fct(event, levels = c("Censoring", "Hospitalization", "Death")),
    scenario = fct(scenario, levels = c("(a)", "(b)", "(c)", "(d)"))
  )
line_cut_df <- bind_rows(s1_cut, s2_cut, s3_cut, s4_cut) |>
  mutate(scenario = fct(scenario, levels = c("(a)", "(b)", "(c)", "(d)")))

# optional start-of-time guide per panel (x = 0)
t0_df <- distinct(line_df, scenario) |>
  mutate(x = 0)

# y-axis labels "A" and "B" at left margin of each panel
ylab_df <- expand_grid(
  scenario = levels(line_df$scenario),
  subj = c("A", "B")
) |>
  mutate(
    y = subj_y[subj],
    x = -0.1,
    lab = subj,
    scenario = fct(scenario, levels = c("(a)", "(b)", "(c)", "(d)"))
  )

# plot
p <- ggplot() +
  # vertical dotted line at time 0
  geom_vline(
    xintercept = 0,
    linewidth = 0.8
  ) +
  # vertical dotted line at earlier follow-up (pairwise comparison time)
  geom_vline(
    data = line_cut_df,
    aes(xintercept = xcut),
    linetype = "dotted", linewidth = 0.35
  ) +
  # follow-up lines
  geom_segment(
    data = line_df,
    aes(x = xstart, xend = xend, y = y, yend = y),
    linewidth = 0.6, lineend = "round"
  ) +
  # events
  geom_point(
    data = dots_df,
    aes(x = time, y = y, shape = event),
    size = 3, stroke = 1.1, fill = "white"
  ) +
  # left-side A/B
  geom_text(
    data = ylab_df,
    aes(x = x, y = y, label = lab),
    hjust = 1, vjust = 0.5, size = 3.8
  ) +
  facet_wrap(~scenario, ncol = 1) +
  scale_y_continuous(NULL, breaks = c(0, 1), labels = subj_levels, limits = c(-0.5, 1.5)) +
  scale_x_continuous(NULL, breaks = NULL, expand = expansion(c(0, 0.1))) +
  scale_shape_manual(
    values = c(Censoring = 23, Hospitalization = 15, Death = 16)  # 23 diamond, 15 filled square, 16 filled circle
  ) +
  coord_cartesian(xlim = c(-0.8, 13.5), clip = "off") +
  theme_classic(base_size = 11) +
  theme(
    legend.position    = "top",
    legend.title       = element_blank(),
    legend.text = element_text(size = 10),
    strip.background   = element_blank(),
    strip.text         = element_text(size = 11, hjust = 0, face = "bold"),    
    axis.text.y        = element_blank(),    # we add A/B by geom_text
    axis.ticks         = element_blank(),
    axis.line.y        = element_blank(),
    axis.line.x        = element_blank()
    # plot.margin        = margin(8, 14, 8, 38)
  )
# 
p

# Save (vector + raster)
ggsave("images/comp_wr_rwr.eps", p, width = 8, height = 6, device = cairo_ps)
ggsave("images/comp_wr_rwr.png", p, width = 8, height = 6)


# residual processes ------------------------------------------------------

n <- 100
sd <- 2

set.seed(12345)

t <- seq(0, 1, length.out = n)
mean_fun <- 16 * t * (1 - t) * (1 - t)

sd_fun <- 1 * t * (1 - t) * (1 - t)

resid_a <- rnorm(n, 0, 3 * sd_fun)
resid_b <- rnorm(n, 0.9 * mean_fun, 2* sd_fun)
resid_c <- rnorm(n, - 0.9 * mean_fun, 2* sd_fun)


dat <- tibble(
  t = rep(t, 3),
  resid = c(resid_a, resid_b, resid_c),
  panel = rep(c("a", "b", "c"), each = n)
)

panel_label <- c(
  "a" = "(a) Proportional - good", 
  "b" = "(b) Win ratio\U2193 - no good", 
  "c" = "(c) Win ratio\U2191 - no good")



# LW %down% bgroup


res_p <- 
  dat |> 
  ggplot(aes(x = t, y = resid)) +
  geom_line(lwd = 1) +
  facet_wrap( ~ panel, labeller = as_labeller(panel_label)) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 1), labels = c(0, expression(infinity)),
                     minor_breaks = NULL) +
  scale_y_continuous(name = "Standardized residuals", breaks = c(-2, 0, 2),
                     minor_breaks = NULL) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 10),
    axis.line.x = element_line(),
    # axis.line.y = element_line(),
    axis.title.y = element_text(size = 10)
  ) +
  geom_hline(
    yintercept = c(-2, 0, 2), lty = 2
  ) 

ggsave("images/comp_pw_res.png", res_p, width = 8, height = 3.5)
ggsave("images/comp_pw_res.eps", res_p, width = 8, height = 3.5, device = cairo_ps)



library(tidyverse)

# install.packages("ggimage")  # if needed
library(ggimage)


# ---- 1) Specify the 4x4 outcomes matrix (rows: Treatment 1..4, cols: Control 1..4) 
# Allowed entries: "Win", "Loss", or "Tie" 
outcomes <- matrix( c("Win","Tie","Win","Win", 
                      "Loss", "Win","Tie","Tie", 
                      "Win","Loss","Tie","Win", 
                      "Win","Loss","Win","Tie" ), 
                    nrow = 4, ncol = 4, 
                    byrow = TRUE, 
                    dimnames = list(paste0("T", 1:4), paste0("C", 1:4)) ) 

# ---- 2) Layout: 4 treatment nodes (left) and 4 control nodes (right) 
nodes_t <- tibble(group = "Treatment", 
                  id = paste0("T", 1:4), x = 0, y = 4:1) 

nodes_c <- tibble(group = "Control", id = paste0("C", 1:4), x = 1, y = 4:1) 
nodes <- bind_rows(nodes_t, nodes_c)


# ---- 3) Build the 16 edges from the outcomes matrix 
edges <- as_tibble(outcomes, rownames = "t_id") |> 
  pivot_longer(-t_id, names_to = "c_id", values_to = "type") |> 
  left_join(nodes_t |> select(id, x, y), by = c("t_id" = "id")) |> 
  rename(x_start = x, y_start = y) |> 
  left_join(nodes_c |> select(id, x, y), by = c("c_id" = "id")) |> 
  rename(x_end = x, y_end = y) |> 
  mutate(type = factor(type, levels = c("Win","Loss","Tie"))) 


stopifnot(nrow(edges) == 16) # sanity check

# 0) point to your PNG (transparent background recommended)
icon_path <- "sj_images/human.png"  # e.g., "sj_images/node.png"

# 1) attach image path to each node
nodes <- nodes |>
  dplyr::mutate(img = normalizePath(icon_path, mustWork = FALSE))

# ---- 5) Colors and line sizing 
cols <- c(Win = "#2E7D32", Loss = "#F57C00", Tie = "#B3B3B3")

theme_clean <- theme_minimal(base_size = 14) + 
  theme( 
    axis.title = element_blank(), 
    axis.text = element_blank(), 
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", hjust = 0.5), 
    legend.position = "bottom", 
    legend.key.width = unit(1.5, "cm"),
    legend.text = element_text(size = 14),
    # legend.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

# 2) plot (unchanged layers omitted for brevity)
p <- ggplot() +
  # edges
  geom_segment(
    data = edges,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = type),
    linewidth = 1.3, lineend = "round"
  ) +
  # nodes as images (replace the old geom_point layer with this)
  ggimage::geom_image(
    data = nodes,
    aes(x = x, y = y, image = img),
    size = 0.14,           # ~14% of panel width; adjust 0.10–0.18 as you like
    asp  = 1               # keep your PNG square
  ) +
  annotate("text", x = 0, y = 4.5, label = "Treatment", fontface = "bold", size = 5, hjust = 0.5) +
  annotate("text", x = 1, y = 4.5, label = "Control",   fontface = "bold", size = 5, hjust = 0.5) +
  scale_color_manual(values = cols, breaks = c("Win","Loss","Tie")) +
  coord_cartesian(xlim = c(-0.15, 1.15), ylim = c(0.7, 4.7), expand = FALSE) +
  labs(color = "Treatment vs Control:") +
  theme_clean

print(p)

# ---- 8) Save editable files (SVG is ideal for Illustrator/InkScape/PowerPoint)
ggsave("sj_images/gpc_pairwise.png", p, width = 8, height = 5)



########## Stratification ####################


# ---- Packages ----
library(tidyverse)
library(ggimage)
library(patchwork)
library(glue)

# ---- Helper to build one panel with straight connections ----
make_panel <- function(outcomes,
                       title,
                       icon_path = "sj_images/human.png",
                       left_lab  = "Treatment",
                       right_lab = "Control",
                       cols = c(Win = "#2E7D32", Loss = "#F57C00", Tie = "#B3B3B3")) {
  
  n_t <- nrow(outcomes)
  n_c <- ncol(outcomes)
  
  # Nodes (treatment on left, control on right)
  nodes_t <- tibble(group = "Treatment",
                    id = paste0("T", seq_len(n_t)),
                    x = 0,
                    y = n_t:1)
  nodes_c <- tibble(group = "Control",
                    id = paste0("C", seq_len(n_c)),
                    x = 1,
                    y = n_c:1)
  nodes <- bind_rows(nodes_t, nodes_c) |>
    mutate(img = normalizePath(icon_path, mustWork = FALSE))
  
  # Edges (all pairs; no curvature)
  edges <- as_tibble(outcomes, rownames = "t_id") |>
    pivot_longer(-t_id, names_to = "c_id", values_to = "type") |>
    left_join(nodes_t |> select(id, x, y), by = c("t_id" = "id")) |>
    rename(x_start = x, y_start = y) |>
    left_join(nodes_c |> select(id, x, y), by = c("c_id" = "id")) |>
    rename(x_end = x, y_end = y) |>
    mutate(type = factor(type, levels = c("Win","Loss","Tie")))
  
  # Subtitle counts
  ct <- count(edges, type) |>
    complete(type = factor(c("Win","Loss","Tie"),
                           levels = c("Win","Loss","Tie")),
             fill = list(n = 0))
  nwins = ct$n[ct$type=='Win']
  nlosses = ct$n[ct$type=='Loss']
  n = nrow(edges)
  wins = paste0(round(100 * nwins / n,), "%")
  losses = paste0(round(100 * nlosses / n), "%")
  subtitle <- glue("Wins: {nwins} / {n} = {wins}; Losses: {nlosses} / {n} = {losses}")
  
  # Theme
  theme_clean <- theme_minimal(base_size = 13) +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom",
      # legend.title = element_blank(),
      legend.key.width = grid::unit(1.4, "cm"),
      legend.text = element_text(size = 14),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # Plot (straight lines)
  ggplot() +
    geom_segment(
      data = edges,
      aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = type),
      linewidth = 1.2, lineend = "round"
    ) +
    ggimage::geom_image(
      data = nodes,
      aes(x = x, y = y, image = img),
      size = 0.14, asp = 1
    ) +
    annotate("text", x = 0, y = n_t + 0.6, label = left_lab,
             fontface = "bold", size = 4.5, hjust = 0.5) +
    annotate("text", x = 1, y = n_c + 0.6, label = right_lab,
             fontface = "bold", size = 4.5, hjust = 0.5) +
    scale_color_manual(values = cols, breaks = c("Win","Loss","Tie")) +
    coord_cartesian(xlim = c(-0.18, 1.18),
                    ylim = c(0.5, max(n_t, n_c) + 0.8),
                    expand = FALSE) +
    labs(title = title, subtitle = subtitle,
         color = "Treatment vs Control:") +
    theme_clean
}

# ---- Panel (1): EF ≤ 40% — 4×4, 8 Wins, 6 Losses, 2 Ties ----
outcomes_ef_le40 <- matrix(
  c(
    "Win","Win","Loss","Tie",   # 2W,1L,1T
    "Win","Loss","Win","Loss",  # 2W,2L
    "Loss","Win","Tie","Win",   # 2W,1L,1T
    "Win","Loss","Win","Loss"   # 2W,2L
  ),
  nrow = 4, ncol = 4, byrow = TRUE,
  dimnames = list(paste0("T", 1:4), paste0("C", 1:4))
)
p1 <- make_panel(outcomes_ef_le40, title = "EF ≤ 40% (N = 8)")

# ---- Panel (2): EF > 40% — 3×3, 3 Wins, 2 Losses, 4 Ties ----
outcomes_ef_gt40 <- matrix(
  c(
    "Win","Tie","Tie",   # 1W,0L,2T
    "Tie","Loss","Win",  # 1W,1L,1T
    "Tie","Loss","Win"   # 1W,1L,1T
  ),
  nrow = 3, ncol = 3, byrow = TRUE,
  dimnames = list(paste0("T", 1:3), paste0("C", 1:3))
)

p2 <- make_panel(outcomes_ef_gt40, title = "EF > 40% (N = 6)")

# ---- Side-by-side (patchwork) ----
p_final <- p1 + p2 + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Render
p_final

# ---- (Optional) Save high-res files ----
# ggsave("stratified_winloss.svg", p_final, width = 14, height = 6)
ggsave("sj_images/str_winloss.png", p_final, width = 12, height = 4)


