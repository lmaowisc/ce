# Run from the repository root. Plot sources for Chapter 1.
library(rmt)
library(dplyr)
library(survival)
library(ggsurvfit)
library(patchwork)
library(Wcompo)
data("hfaction", package = "rmt")
hf <- hfaction
hf_death <- hf |> filter(status %in% c(0, 2)) |> mutate(death = status == 2)
hf_first <- hf |> arrange(patid, time, desc(status)) |>
  group_by(patid) |> slice_head(n = 1) |> ungroup() |>
  mutate(event = status != 0)

# Shared appearance; muted red = usual care, blue = training.
book_theme <- theme_minimal(base_size = 12, base_family = "Georgia") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#ded5da", linewidth = 0.3),
        text = element_text(color = "#332d34"),
        axis.text = element_text(size = 12, color = "#332d34"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.background = element_rect(fill = "#fffefd", color = NA),
        legend.position = "top", legend.title = element_blank())
# Mortality follow-up continues after hospitalization.
p_death <- survfit2(Surv(time, death) ~ trt_ab, data = hf_death) |>
  ggsurvfit(linewidth = 0.7) + scale_ggsurvfit() +
  scale_color_manual(values = c("#a85d60", "#50758a"),
                     labels = c("Usual care", "Training")) +
  scale_x_continuous("Time (years)", breaks = 0:4) +
  coord_cartesian(xlim = c(0, 4)) +
  labs(y = "Overall survival")
# Here the first hospitalization or death ends event-free follow-up.
p_first <- survfit2(Surv(time, event) ~ trt_ab, data = hf_first) |>
  ggsurvfit(linewidth = 0.7) + scale_ggsurvfit() +
  scale_color_manual(values = c("#a85d60", "#50758a"),
                     labels = c("Usual care", "Training")) +
  scale_x_continuous("Time (years)", breaks = 0:4) +
  coord_cartesian(xlim = c(0, 4)) +
  labs(y = "Hospitalization-free survival")
# Stack the two endpoints and share one treatment legend.
survival_plot <- (p_death / p_first + plot_layout(guides = "collect")) & book_theme
ggsave("website/figures/hf-survival.svg", survival_plot,
       device = svglite::svglite, width = 20/3, height = 5.4, bg = "#fffefd")

hf_weighted <- hf |> mutate(status_w = case_when(
  status == 0 ~ 0L, status == 2 ~ 1L, status == 1 ~ 2L))
fit_weighted <- CompoML(id = hf_weighted$patid, time = hf_weighted$time,
  status = hf_weighted$status_w, Z = as.matrix(hf_weighted["trt_ab"]), w = c(2, 1))
svglite::svglite("website/figures/hf-means.svg", width = 20/3, height = 3.6,
                 pointsize = 12, bg = "#fffefd")
# Match the book typography; these settings do not change estimates.
par(family = "Georgia", mar = c(3.6, 3.8, 0.8, 0.6),
    cex.axis = 1, cex.lab = 1,
    fg = "#332d34", col.axis = "#332d34", col.lab = "#332d34", las = 1,
    mgp = c(2.5, 0.7, 0), tcl = -0.25, bty = "l")
# z is treatment assignment: 0 = usual care, 1 = training.
plot(fit_weighted, z = 0, col = "#a85d60", lwd = 2,
     ylim = c(0, 5), xlim = c(0, 4),
     xlab = "Years since randomization", ylab = "Mean cumulative weighted count")
plot(fit_weighted, z = 1, add = TRUE, col = "#50758a", lwd = 2)
legend("topleft", c("Usual care", "Exercise training"),
       col = c("#a85d60", "#50758a"), lwd = 2, bty = "n")
dev.off()
