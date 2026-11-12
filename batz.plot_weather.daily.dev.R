# =============================================================================
# batz.plot_weather.daily.dev.R
# -----------------------------------------------------------------------------
# Dev / test script for batz.plot_weather.daily() (Batz package).
# Test data: weather.csv (Weather Underground readings for two Bristol, NH
# stations, 2026-06-25 to 2026-07-05) and MAMM_IN25092_2suntimes.csv.
# Set dir.wd to the folder holding the two files, then run the whole script.
# =============================================================================

library(Batz)

dir.wd   <- "C:/Users/josh.guilbert/OneDrive - Biodiversity Research Institute/Documents/ActiveProjects/MAMM-Batz Package/8reference tables"
weather  <- read.csv(file.path(dir.wd, "weather.csv"))
suntimes <- read.csv(file.path(dir.wd, "MAMM_IN25092_2suntimes.csv"))
dir.out  <- file.path(tempdir(), "weather_daily_test"); dir.create(dir.out, showWarnings = FALSE)

## 1. Defaults: nights with temperature > 10 C and wind < 4 m/s in the window
##    (-30 min sunset to +5 h), allowing 10 min outside each; axes 0-25 C and
##    0-10 m/s with 10 C level with 4 m/s; 8 nights per PNG
r1 <- batz.plot_weather.daily(weather, suntimes, dir.save = dir.out)
print(r1$nights)                              # window, min temperature, max wind, minutes outside, pass.auto, plotted
print(r1$axes)                                # expect 0-25 C and 0-10 m/s
basename(r1$files)                            # no project.name -> files start with "weather.daily"

## 2. temp.range changed -> wind axis matched: -5 C level with 0 m/s, 10 C with 4 m/s
r2 <- batz.plot_weather.daily(weather, suntimes, nights.auto = FALSE,
                              temp.range = c(-5, 30), dir.save = dir.out, project.name = "IN25092")
print(r2$axes)                                # expect temperature -5 to 32.5 C, wind/precipitation 0-10

## 3. wind.range changed -> temperature axis matched; precipitation max = wind max
r3 <- batz.plot_weather.daily(weather, suntimes, nights.auto = FALSE,
                              wind.range = c(0, 8), dir.save = dir.out, project.name = "IN25092")
print(r3$axes)                                # expect temperature 0-20 C, wind/precipitation 0-8

## 3b. scale.auto = FALSE: ranges used exactly as given, two cut-off lines (solid 10 C, dotted 4 m/s)
r3b <- batz.plot_weather.daily(weather, suntimes, nights.auto = FALSE, scale.auto = FALSE,
                               temp.range = c(0, 30), wind.range = c(0, 8), dir.save = dir.out)
print(r3b$axes)                               # expect 0-30 C and 0-8 m/s

## 4. Temperature and wind on the same axis: solid line at 10, dotted line at 4
r4 <- batz.plot_weather.daily(weather, suntimes, nights.auto = FALSE,
                              stack.y = c("Temperature", "Wind"), stack.y2 = "Precipitation",
                              panels = 4, dir.save = dir.out, project.name = "IN25092")
print(r4$plots[[1]][[1]])

## 5. Chosen nights, clock-time window, 1-hour X labels
r5 <- batz.plot_weather.daily(weather, suntimes, nights.auto = FALSE,
                              nights.manual = "c(2026-06-25, 2026-06-29)",
                              time.start_monitoring = "21:00", time.end_monitoring = "01:00",
                              time.interval = 60, dir.save = dir.out)
print(unique(r5$data$night))                 # expect 2026-06-25, 2026-06-29

## 6. "AND": passes the cut-offs AND is listed; window ends 30 min after sunrise
r6 <- batz.plot_weather.daily(weather, suntimes, nights.auto = "AND",
                              nights.manual = c("2026-06-26", "2026-06-27", "2026-06-28"),
                              time.end_monitoring = "+30 Sunrise", dir.save = dir.out)
print(r6$nights[r6$nights$plotted, c("station", "night")])

## 7. No cut-offs: no black lines, nights.auto keeps every night
r7 <- batz.plot_weather.daily(weather, suntimes, wind.ms = "", temperature = "", dir.save = dir.out)
print(table(r7$nights$pass.auto))             # all TRUE

## 8. Wind cut-off only (0.5 m/s): only wind decides the nights
r8 <- batz.plot_weather.daily(weather, suntimes, temperature = "", wind.ms = 0.5, dir.save = dir.out)
print(table(r8$nights$station, r8$nights$pass.auto))

## 9. Buffers: temperature 18 C with no time allowed below it, then 120 minutes allowed
r9a <- batz.plot_weather.daily(weather, suntimes, temperature = 18, temperature.c_buffer = 0, dir.save = dir.out)
r9b <- batz.plot_weather.daily(weather, suntimes, temperature = 18, temperature.c_buffer = 120, dir.save = dir.out)
print(r9a$nights[, c("station", "night", "minutes.temperature.out", "pass.auto")])
cat("nights passing: buffer 0 =", sum(r9a$nights$pass.auto), " buffer 120 =", sum(r9b$nights$pass.auto), "\n")

## 10. Zero wind / zero precipitation: show or hide
for (wa in c("show", "hide")) for (pa in c("show", "hide")) {
  r10 <- batz.plot_weather.daily(weather, suntimes, nights.auto = FALSE,
                                 nights.manual = c("2026-06-25", "2026-06-30", "2026-07-02", "2026-07-03"),
                                 wind.absent = wa, precipitation.absent = pa,
                                 project.name = paste0("wind-", wa, "_precip-", pa), dir.save = dir.out)
  cat("wind.absent =", wa, " precipitation.absent =", pa, " ->", basename(r10$files[1]), "\n")
}
# both "show": the blue zero-precipitation line runs along 0 and zero wind is drawn just above it

## 11. wind.type: "speed" (default) uses the wind speed column, "gust" the wind gust column
r11s <- batz.plot_weather.daily(weather, suntimes, wind.type = "speed", project.name = "wind-speed", dir.save = dir.out)
r11g <- batz.plot_weather.daily(weather, suntimes, wind.type = "gust",  project.name = "wind-gust",  dir.save = dir.out)
print(cbind(r11s$nights[, c("station", "night")],
            speed.max = round(r11s$nights$wind.max, 2), speed.pass = r11s$nights$pass.auto,
            gust.max  = round(r11g$nights$wind.max, 2), gust.pass  = r11g$nights$pass.auto))
stopifnot("wind.speed.ms" %in% names(r11s$data), "wind.gust.ms" %in% names(r11g$data))   # $data header follows wind.type
# the right-hand axis title reads "Wind gust (m/s) and precipitation (mm)" on the gust plots

## ---- aes.plotopts / aes.style (2026-11-10): settings from a plotopts file ----------
## order: function call > aes.plotopts > built-in defaults; a value equal to the default changes nothing
batz.generate_plotopts("batz.plot_weather.daily", project.name = "Bristol NH", dir.save = tempdir())
po <- read.csv(file.path(tempdir(), "Bristol NH_plotopts_batz.plot_weather.daily.csv"), check.names = FALSE)
po$`Bristol NH`[po$parameter == "panels"] <- "4"                   # 4 nights per PNG
po$`Bristol NH`[po$parameter == "temp.range"] <- "5;30"
r12 <- batz.plot_weather.daily(weather, suntimes = suntimes, aes.plotopts = po, aes.style = "Bristol NH",
                               dir.save = file.path(tempdir(), "wx_plotopts"))   # NOTE: settings from aes.plotopts: panels, temp.range
r13 <- batz.plot_weather.daily(weather, suntimes = suntimes, aes.plotopts = po, aes.style = "Bristol NH", panels = 6,
                               dir.save = file.path(tempdir(), "wx_plotopts_call"))   # panels from the call (6), temp.range from po
