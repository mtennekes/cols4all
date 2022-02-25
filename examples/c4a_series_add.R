cbsnl_cols = list(
	cool = c("#00a1cd", "#0058b8", "#afcb05", "#53a31d",
		"#f39200", "#af0e80", "#ffcc00", "#e94c0a"),
	warm = c("#e94c0a", "#ffcc00", "#af0e80", "#f39200",
		"#53a31d", "#afcb05", "#0058b8", "#00a1cd"),
	blues = c("#c0e7ff", "#77cbe5", "#3d95d4", "#2256a0", "#143564"),
	reds =  c("#ffc597", "#f89e6b", "#e74d15", "#c01f26", "#82001e"),
	greens = c("#edf0c7", "#c9de85", "#85bc22", "#348a3a", "#0f5f34"),
	purples = c("#f8c1d9", "#e38cbf", "#be3e8d", "#8b176f", "#460042")
)

cbsnl_types = c("cat", "cat", "seq", "seq", "seq", "seq")

c4a_series_add(cbsnl_cols, xNA = "grey88", types = cbsnl_types, series = "cbsnl")

\dontrun{
c4a_gui(series = "cbsnl", n = 8)
}
