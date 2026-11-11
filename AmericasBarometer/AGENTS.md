# AmericasBarometer project

- Save all rendered/saved images (PNG etc.) to the `export/` folder, never the project root.
- Input .dta files are in `import/`.
- Plots: French text, BBC-style theme (`bbc_style()` in `trust_china_us.R`), China `#E63115` (dashed, triangles), United States `#232551` (solid, circles).
- Preferred style going forward: `hrbrthemes::theme_ipsum_archivo()` (needs hrbrthemes >= 0.9.5 from https://hrbrmstr.r-universe.dev, and Archivo SemiCondensed fonts installed in `~/Library/Fonts`). Keep existing BBC versions and add `_ipsum` variations alongside. Pattern (see `plot_trust_direct_ipsum()` and `age_plot_ipsum` in `trust_china_us.R`): take the built plot, set `font_ar` as family on GeomText layers, add `theme_ipsum_archivo(grid = FALSE or "X", ...)` with sizes 16/14/10/14/12, then `theme(legend.position = "none", axis.title.y = element_blank(), plot.title = ggtext::element_markdown(...), plot.title.position = "plot", plot.caption.position = "plot")`. Colour-coded-title variants use blue `#0072B2` for the United States. Save PNG at 600 dpi plus cairo PDF, 8 x 4.5 in, to `export/` with `_ipsum` suffix.
