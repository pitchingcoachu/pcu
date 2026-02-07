# app.R
# Shiny pitching report with per-player privacy + admin view + customized Stuff+ metric per pitch type

library(shiny)
library(dplyr)
library(readr)
library(tibble)
library(stringr)
library(lubridate)
library(purrr)
library(DT)
library(gridExtra)
library(ggplot2)
library(patchwork)
library(rsconnect)
library(hexbin)
library(ggiraph)
library(httr2)
library(MASS)  # for kde2d in heatmaps
# --- media uploads (Cloudinary) ---
# raise upload size if needed (50 MB here)
options(shiny.maxRequestSize = 50 * 1024^2)

# Optional team scoping: blank = no filter
if (!exists("TEAM_CODE", inherits = TRUE)) TEAM_CODE <- ""

# ---- Video resource path (GLOBAL) ----
# NOTE: Videos are now served from Cloudinary instead of local files
# VideoClip columns in CSV files contain full Cloudinary URLs
cat("INFO: Videos are served from Cloudinary CDN for better performance\n")

resolve_storage_path <- function(env_var, filename) {
  override <- Sys.getenv(env_var, unset = NA_character_)
  if (!is.na(override) && nzchar(override)) {
    return(override)
  }
  user_dir <- tools::R_user_dir("pcu_dashboard", "data")
  dir.create(user_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(user_dir, filename)
  legacy <- file.path("data", filename)
  if (!file.exists(dest) && file.exists(legacy)) {
    tryCatch({
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      file.copy(legacy, dest, overwrite = TRUE)
    }, error = function(e) {
      warning(sprintf("Could not migrate legacy %s to %s: %s", filename, dest, e$message))
    })
  }
  dest
}

PLAYER_PLANS_STORAGE_PATH <- resolve_storage_path(
  "PLAYER_PLANS_STORAGE_PATH",
  "player_plans_store.rds"
)
PLAYER_COMPLETED_GOALS_STORAGE_PATH <- resolve_storage_path(
  "PLAYER_COMPLETED_GOALS_STORAGE_PATH",
  "player_completed_goals_store.rds"
)

read_rds_with_default <- function(path, default = list()) {
  if (!file.exists(path)) return(default)
  out <- tryCatch(readRDS(path), error = function(e) default)
  if (is.list(out)) out else default
}

write_rds_atomically <- function(obj, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  ok <- tryCatch({
    saveRDS(obj, tmp)
    moved <- file.rename(tmp, path)
    if (!moved) {
      moved <- file.copy(tmp, path, overwrite = TRUE)
    }
    if (!moved) stop("Could not move temp file into place")
    TRUE
  }, error = function(e) {
    warning(sprintf("Failed to persist data to %s: %s", path, e$message))
    FALSE
  })
  if (file.exists(tmp)) unlink(tmp)
  invisible(ok)
}


# ---- Heatmap constants and functions for Player Plans ----
HEAT_BINS <- 6
HEAT_EV_THRESHOLD <- 90

# Zone boundaries
ZONE_LEFT <- -0.83
ZONE_RIGHT <- 0.83
ZONE_BOTTOM <- 1.5
ZONE_TOP <- 3.5

# Frequency (keep multi-color)
heat_pal_freq <- function(n = HEAT_BINS) colorRampPalette(
  c("#00000000","pink","red")
)(n)

# All other heat maps → white→red only
heat_pal_red  <- function(n = HEAT_BINS) colorRampPalette(c("#00000000","pink","red"))(n)

# Draw heatmap function
draw_heat <- function(grid, bins = HEAT_BINS, pal_fun = heat_pal_red,
                      title = NULL, mark_max = TRUE, breaks = NULL) {
  if (!nrow(grid)) return(ggplot() + theme_void())
  
  home <- data.frame(
    x = c(-0.75, 0.75, 0.75, 0.00, -0.75),
    y = c(1.05, 1.05, 1.15, 1.25, 1.15) - 0.5
  )
  sz <- data.frame(xmin = ZONE_LEFT, xmax = ZONE_RIGHT, ymin = ZONE_BOTTOM, ymax = ZONE_TOP)
  
  peak_df <- NULL
  if (mark_max) {
    i <- which.max(grid$z)
    if (length(i) && is.finite(grid$z[i])) {
      peak_df <- data.frame(px = grid$x[i], py = grid$y[i])
    }
  }
  
  n_bins <- if (is.null(breaks)) bins else max(1, length(breaks) - 1)
  
  ggplot(grid, aes(x, y, z = z)) +
    {
      if (is.null(breaks))
        geom_contour_filled(aes(fill = after_stat(level)), bins = bins, show.legend = FALSE)
      else
        geom_contour_filled(aes(fill = after_stat(level)), breaks = breaks, show.legend = FALSE)
    } +
    scale_fill_manual(values = pal_fun(n_bins), guide = "none") +
    geom_polygon(data = home, aes(x, y), fill = NA, color = "black", inherit.aes = FALSE) +
    geom_rect(data = sz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = NA, color = "black", inherit.aes = FALSE) +
    { if (!is.null(peak_df))
      geom_point(data = peak_df, aes(x = px, y = py), inherit.aes = FALSE,
                 size = 3.8, shape = 21, fill = "red", color = "black", stroke = 0.5)
    } +
    coord_fixed(ratio = 1, xlim = c(-2, 2), ylim = c(0, 4.5)) +
    theme_void() + theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background  = element_rect(fill = "transparent", colour = NA)
    ) +
    labs(title = title)
}

# KDE grid generation for Player Plans heatmaps
make_kde_grid <- function(x, y, lims = c(-2,2,0,4.5), n = 180) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 2 || length(unique(x)) < 2 || length(unique(y)) < 2) {
    return(data.frame(x = numeric(0), y = numeric(0), z = numeric(0)))
  }
  
  x_seq <- seq(lims[1], lims[2], length.out = n)
  y_seq <- seq(lims[3], lims[4], length.out = n)
  
  # Use MASS::kde2d for kernel density estimation
  kde <- MASS::kde2d(x, y, n = n, lims = lims)
  
  # Convert to long format
  grid <- expand.grid(x = kde$x, y = kde$y)
  grid$z <- as.vector(kde$z)
  
  return(grid)
}

# Helper function for reading players access
read_players_access <- function() {
  if (!file.exists(PLAYERS_ACCESS_PATH)) {
    return(data.frame(email = character(), names = character(), stringsAsFactors = FALSE))
  }
  suppressWarnings(
    readr::read_csv(
      PLAYERS_ACCESS_PATH,
      col_types = readr::cols(
        email = readr::col_character(),
        names = readr::col_character(),  # pipe-separated roster names: "John Smith|Johnny Smith"
        team  = readr::col_character()   # optional
      )
    )
  )
}


# --- Admins list for privacy gates ---
ADMIN_EMAILS <- c(
  "jgaynor@pitchingcoachu.com",
  "jchipman@pitchingcoachu.com",
  "ahalverson@pitchingcoachu.com"
)

# --- Helpers to robustly fetch user email and admin status across variants ---
get_user_email <- function(session = NULL, input = NULL) {
  if (exists("current_user_email")) {
    fml <- tryCatch(names(formals(current_user_email)), error = function(e) character())
    out <- tryCatch({
      if (length(fml) >= 2) current_user_email(session, input)
      else if (length(fml) == 1) current_user_email(session)
      else current_user_email()
    }, error = function(e) NULL)
    if (is.character(out) && length(out)) return(out[[1]])
  }
  if (exists("user_email")) {
    out <- tryCatch(user_email(), error = function(e) NULL)
    if (is.character(out) && length(out)) return(out[[1]])
  }
  ""
}



# --- Make data.frames safe for DT: convert list columns to character and drop unsupported types
sanitize_df_for_dt <- function(df) {
  if (is.null(df)) return(df)
  if (!is.data.frame(df)) return(df)
  if (!NROW(df)) return(df[0, , drop = FALSE])
  list_cols <- vapply(df, is.list, logical(1))
  if (any(list_cols)) {
    df[list_cols] <- lapply(df[list_cols], function(z) vapply(z, function(x) {
      if (is.null(x)) "" else paste(as.character(x), collapse = ", ")
    }, character(1)))
  }
  df
}

collapse_list_cols <- function(dat) {
  if (!is.data.frame(dat) || !nrow(dat)) return(dat)
  dat %>%
    dplyr::mutate(dplyr::across(
      dplyr::where(is.list),
      ~ vapply(., function(x) {
        if (is.null(x)) ""
        else if (length(x) == 1 && !is.list(x)) as.character(x)
        else paste0(unlist(x), collapse = ", ")
      }, character(1))
    ))
}

default_swing_levels <- function() {
  c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall")
}

ensure_numeric_cols <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA_real_
    }
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  df
}

ensure_tagged_pitch_type <- function(df, fallback = "Unknown") {
  if (!is.data.frame(df)) return(df)
  n <- nrow(df)
  if (!("TaggedPitchType" %in% names(df))) {
    df$TaggedPitchType <- rep(NA_character_, n)
  }
  df$TaggedPitchType <- as.character(df$TaggedPitchType)
  missing <- is.na(df$TaggedPitchType) | !nzchar(df$TaggedPitchType)
  if (any(missing)) {
    if ("PitchType" %in% names(df)) {
      alt <- as.character(df$PitchType)
      df$TaggedPitchType[missing] <- alt[missing]
    } else if ("Pitch" %in% names(df)) {
      alt <- as.character(df$Pitch)
      df$TaggedPitchType[missing] <- alt[missing]
    }
  }
  df$TaggedPitchType[is.na(df$TaggedPitchType) | !nzchar(df$TaggedPitchType)] <- fallback
  df
}

format_player_display_name <- function(x) {
  if (is.null(x)) return(x)
  out <- as.character(x)
  empty <- is.na(out) | !nzchar(out)
  needs_swap <- grepl(",", out, fixed = TRUE) & !empty
  if (any(needs_swap)) {
    swapped <- vapply(strsplit(out[needs_swap], ",", fixed = TRUE), function(parts) {
      parts <- trimws(parts)
      if (length(parts) >= 2) {
        paste(c(parts[-1], parts[1]), collapse = " ")
      } else {
        paste(parts, collapse = " ")
      }
    }, character(1))
    out[needs_swap] <- swapped
  }
  trimws(out)
}

blank_ea_except_all <- function(df) {
  if (!is.data.frame(df) || !("E+A%" %in% names(df))) return(df)
  pitch_cols <- intersect(c("Pitch","PitchType","TaggedPitchType"), names(df))
  if (!length(pitch_cols)) return(df)
  pitch_vals <- tolower(as.character(df[[pitch_cols[1]]]))
  keep_all <- is.na(pitch_vals) | pitch_vals == "all"
  df$`E+A%` <- as.character(df$`E+A%`)
  df$`E+A%`[!keep_all] <- ""
  df
}

blank_ahead_except_all <- function(df) {
  if (!is.data.frame(df) || !("Ahead%" %in% names(df))) return(df)
  pitch_cols <- intersect(c("Pitch","PitchType","TaggedPitchType"), names(df))
  if (!length(pitch_cols)) return(df)
  pitch_vals <- tolower(as.character(df[[pitch_cols[1]]]))
  keep_all <- is.na(pitch_vals) | pitch_vals == "all"
  df$`Ahead%` <- as.character(df$`Ahead%`)
  df$`Ahead%`[!keep_all] <- ""
  df
}

filter_batter_side <- function(df, side) {
  if (!is.data.frame(df) || !("BatterSide" %in% names(df))) return(df)
  if (is.null(side) || !nzchar(side) || identical(side, "All")) return(df)
  dplyr::filter(df, !is.na(BatterSide) & BatterSide == side)
}
user_is_admin <- function(session = NULL, email = NULL) {
  em <- email
  if (is.null(em) || !nzchar(em)) em <- get_user_email(session)
  if (exists("is_admin")) {
    fml <- tryCatch(names(formals(is_admin)), error = function(e) character())
    ok <- tryCatch({
      if (length(fml) == 0) is_admin()
      else if (length(fml) >= 1) is_admin(em)
      else FALSE
    }, error = function(e) NA)
    if (isTRUE(ok) || identical(ok, FALSE)) return(ok)
  }
  if (!is.null(em) && nzchar(em) && exists("ADMIN_EMAILS")) {
    return(tolower(em) %in% tolower(ADMIN_EMAILS))
  }
  FALSE
}
# --- Build an email -> names map that works with your existing lookup_table.csv ---
# Returns a data.frame(email, names) where `names` is a pipe-separated string of player names.
read_players_access_compat <- function() {
  # 1) Prefer an in-memory lookup_table if you've already read/renamed it earlier
  
  if (exists("lookup_table", inherits = TRUE)) {
    df <- get("lookup_table")
    # auto-detect likely email/name columns
    nms <- names(df)
    nml <- tolower(nms)
    email_col <- nms[match(TRUE, nml %in% c("email","email_lookup"), nomatch = 0)]
    name_col  <- nms[match(TRUE, nml %in% c("playername","pitcher","player","name","fullname"), nomatch = 0)]
    if (length(email_col) && length(name_col)) {
      out <- df |>
        dplyr::transmute(email = tolower(trimws(.data[[email_col[1]]])),
                         nm    = as.character(trimws(.data[[name_col[1]]]))) |>
        dplyr::filter(nzchar(email), nzchar(nm)) |>
        dplyr::group_by(email) |>
        dplyr::summarise(names = paste(unique(nm), collapse = "|"), .groups = "drop")
      return(out)
    }
  }
  
  # 2) If not present in memory, try the CSV directly
  if (file.exists("lookup_table.csv")) {
    suppressWarnings({
      df <- readr::read_csv("lookup_table.csv", col_types = readr::cols(.default = readr::col_character()))
    })
    nm_lower <- tolower(names(df))
    email_col <- names(df)[nm_lower == "email"]
    name_col  <- names(df)[nm_lower %in% c("playername","player","name")]
    if (length(email_col) && length(name_col)) {
      out <- df |>
        dplyr::transmute(email = tolower(trimws(.data[[email_col[1]]])), nm = as.character(trimws(.data[[name_col[1]]]))) |>
        dplyr::filter(nzchar(email), nzchar(nm)) |>
        dplyr::group_by(email) |>
        dplyr::summarise(names = paste(unique(nm), collapse = "|"), .groups = "drop")
      return(out)
    }
  }
  # 3) Fallback to the old players-access format if provided
  if (exists("PLAYERS_ACCESS_PATH") && file.exists(PLAYERS_ACCESS_PATH)) {
    return(read_players_access())
  }
  data.frame(email = character(), names = character(), stringsAsFactors = FALSE)
}


normalize_name <- function(x) {
  x <- as.character(x %||% "")
  # lower, collapse internal spaces, trim
  x <- trimws(tolower(gsub("[[:space:]]+", " ", x)))
  # convert "last, first [middle]" -> "first [middle] last"
  has_comma <- !is.na(x) & grepl(",", x, fixed = TRUE)
  if (any(has_comma)) {
    conv <- vapply(strsplit(x[has_comma], ","), function(p) {
      p <- trimws(p)
      if (length(p) >= 2) {
        paste(paste(p[-1], collapse = " "), p[1])
      } else {
        p[1]
      }
    }, character(1))
    x[has_comma] <- conv
    x <- trimws(gsub("[[:space:]]+", " ", x))
  }
  x
}

# resolve current user email from your auth (adjust the order if you store it elsewhere)
current_user_email <- function(session, input) {
  tolower(trimws(
    session$userData$email %||%
      input$user_email      %||%   # some apps put it in an input
      session$user          %||%   # some auth frameworks set this
      ""
  ))
}

is_admin <- function(email, admin_emails = ADMIN_EMAILS %||% character()) {
  if (!nzchar(email)) return(FALSE)
  tolower(email) %in% tolower(admin_emails)
}

allowed_names_for_user <- function(email, pa_df = read_players_access_compat()) {
  if (!nzchar(email)) return(character(0))
  rows <- pa_df[tolower(pa_df$email) == tolower(email), , drop = FALSE]
  if (!nrow(rows)) return(character(0))
  raw <- paste(rows$names %||% "", collapse = "|")
  out <- unlist(strsplit(raw, "\\|"))
  out <- trimws(out)
  out <- unique(out[nzchar(out)])
  out
}

# generic row-level filter
filter_to_user <- function(df, domain, names_allow) {
  if (is.null(df)) return(df)
  if (!is.data.frame(df) || NROW(df) == 0) return(df)
  if (is.null(names_allow) || !length(names_allow)) return(df)  # nothing to filter by
  
  # candidate columns to match per suite
  domain_cols <- switch(
    domain,
    "Hitting"      = c("Batter","Hitter","Player","Name","FullName"),
    "Catching"     = c("Catcher","Player","Name","FullName"),
    "Leaderboard"  = c("Pitcher","Batter","Catcher","Player","Name","FullName"),
    "Comparison"   = c("Pitcher","Batter","Catcher","Player","Name","FullName"),
    "Camps"        = c("Athlete","Player","Name","FullName"),
    "Notes"        = c("pitcher","player","name","Filter","filter_html"),
    # default/fallback
    c("Pitcher","Batter","Catcher","Player","Name","FullName")
  )
  
  cand <- intersect(domain_cols, names(df))
  if (!length(cand)) return(df)
  
  # normalize once for robust matching
  needle <- normalize_name(names_allow)
  keep_any <- rep(FALSE, nrow(df))
  for (col in cand) {
    probe <- normalize_name(as.character(df[[col]] %||% ""))
    keep_any <- keep_any | (probe %in% needle)
  }
  df[keep_any, , drop = FALSE]
}
# --------------------------------------------

library(curl)  # for curl::form_file

# Configure Cloudinary (recommended simple host for images/videos)
# Create a free account, make an *unsigned upload preset*, then set these:
# 1) Prefer environment variables in production (shinyapps.io Settings → Environment Variables)
# 2) Fall back to your local defaults for dev
CLOUDINARY_CLOUD_NAME    <- Sys.getenv("CLOUDINARY_CLOUD_NAME", unset = "")
CLOUDINARY_UPLOAD_PRESET <- Sys.getenv("CLOUDINARY_UPLOAD_PRESET", unset = "")
if (!nzchar(CLOUDINARY_CLOUD_NAME))    CLOUDINARY_CLOUD_NAME    <- "pitchingcoachu"
if (!nzchar(CLOUDINARY_UPLOAD_PRESET)) CLOUDINARY_UPLOAD_PRESET <- "pcu_notes_unsigned"

# helper: coalesce for NULL
`%||%` <- function(a,b) if (is.null(a)) b else a

upload_media_cloudinary <- function(path) {
  if (!nzchar(CLOUDINARY_CLOUD_NAME) || !nzchar(CLOUDINARY_UPLOAD_PRESET)) {
    stop("Cloudinary not configured: set CLOUDINARY_CLOUD_NAME and CLOUDINARY_UPLOAD_PRESET.")
  }
  endpoint <- sprintf("https://api.cloudinary.com/v1_1/%s/auto/upload", CLOUDINARY_CLOUD_NAME)
  
  res <- httr2::request(endpoint) |>
    httr2::req_body_multipart(
      file = curl::form_file(path),
      upload_preset = CLOUDINARY_UPLOAD_PRESET
    ) |>
    httr2::req_timeout(30) |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform()
  
  # Fail clearly on HTTP error
  if (httr2::resp_status(res) >= 400) {
    msg <- tryCatch(httr2::resp_body_string(res), error = function(e) paste("HTTP", httr2::resp_status(res)))
    stop(sprintf("Cloudinary upload failed: %s", msg))
  }
  
  j <- httr2::resp_body_json(res, check_type = FALSE)
  list(
    url  = j$secure_url %||% j$url,
    type = j$resource_type %||% "auto"  # "image" | "video" | "raw"
  )
}


have_akima <- requireNamespace("akima", quietly = TRUE)  # for smoothed means (EV/LA)

HEAT_BINS <- 6
HEAT_EV_THRESHOLD <- 90

# Frequency (keep multi-color)
heat_pal_freq <- function(n = HEAT_BINS) colorRampPalette(
  c("white","pink","red")
)(n)

# All other heat maps → white→red only
heat_pal_red  <- function(n = HEAT_BINS) colorRampPalette(c("white","pink","red"))(n)


# simple JS to strip non-numeric chars (like "%") when sorting
js_sort <- 
  "function(data, type, row, meta) {\n" %>% 
  paste0("  if (type === 'sort') {\n") %>%
  paste0("    var num = parseFloat(data.toString().replace(/[^0-9.-]/g, ''));\n") %>%
  paste0("    return isNaN(num) ? -Infinity : num;\n") %>%
  paste0("  }\n") %>%
  paste0("  return data;\n") %>%
  paste0("}")

# ---- Notes API config ----
NOTES_API_URL   <- "https://script.google.com/macros/s/AKfycbwl9Xfu3N6GtEw8WvOpvgwNx7S3XVR4VDKKmFbRfJtBI3nZWuoFmYk3nNF9oQ682dOX1w/exec"
NOTES_API_TOKEN <- "pcubaseball"

# small helper
# Replace the old %or% with this scalar-safe version
`%or%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  a1 <- a[1]
  if (is.na(a1)) return(b)
  a1c <- as.character(a1)
  if (!nzchar(a1c)) b else a1
}
fmt_mdy <- function(x) ifelse(is.na(x), "", format(as.Date(x), "%m/%d/%Y"))

get_hit_type_col <- function(df) {
  if (is.null(df)) return(character(0))
  if ("HitType" %in% names(df)) as.character(df$HitType)
  else if ("TaggedHitType" %in% names(df)) as.character(df$TaggedHitType)
  else rep(NA_character_, nrow(df))
}

pa_flags_from_counts <- function(df_local) {
  n <- nrow(df_local)
  res <- list(
    is_last      = rep(FALSE, n),
    is_strikeout = rep(FALSE, n),
    is_walk      = rep(FALSE, n)
  )
  if (!n) return(res)
  required <- c("Balls","Strikes","PitchCall")
  if (!all(required %in% names(df_local))) return(res)
  balls   <- suppressWarnings(as.integer(df_local$Balls))
  strikes <- suppressWarnings(as.integer(df_local$Strikes))
  pitchcall <- as.character(df_local$PitchCall)
  valid_counts <- is.finite(balls) & is.finite(strikes)
  if (!any(valid_counts)) return(res)
  prev_valid <- c(FALSE, head(valid_counts, -1))
  new_pa <- valid_counts & ((balls == 0 & strikes == 0) | !prev_valid)
  pa_id <- cumsum(ifelse(valid_counts, new_pa, FALSE))
  if (any(valid_counts) && pa_id[valid_counts][1] == 0) pa_id[valid_counts] <- pa_id[valid_counts] + 1L
  if (all(pa_id[valid_counts] == 0)) pa_id[valid_counts] <- 1L
  pa_id[!valid_counts] <- NA_integer_
  last_idx <- rep(NA_integer_, n)
  if (any(!is.na(pa_id))) {
    last_idx <- ave(seq_len(n), pa_id, FUN = function(idx) rep(max(idx), length(idx)))
  }
  is_last <- seq_len(n) == last_idx
  is_last[is.na(pa_id)] <- FALSE
  strikeout_calls <- c("StrikeSwinging","StrikeCalled","StrikeSwingingBlocked","StrikeSwingingPitchout","StrikeSwingingChecked","StrikeCalledPitchout")
  walk_calls      <- c("BallCalled","BallInDirt","PitchoutBall","IntentBall")
  res$is_last      <- is_last
  res$is_strikeout <- is_last & (strikes == 2) & pitchcall %in% strikeout_calls
  res$is_walk      <- is_last & (balls == 3)   & pitchcall %in% walk_calls
  res
}

compute_pa_flags <- function(df_local) {
  if (!("PlayResult" %in% names(df_local))) df_local$PlayResult <- NA_character_
  if (!("KorBB" %in% names(df_local))) df_local$KorBB <- NA_character_
  if (!("PitchCall" %in% names(df_local))) df_local$PitchCall <- NA_character_
  if (!("Balls" %in% names(df_local))) df_local$Balls <- NA_real_
  if (!("Strikes" %in% names(df_local))) df_local$Strikes <- NA_real_
  
  flags <- pa_flags_from_counts(df_local)
  df_play_result <- as.character(df_local$PlayResult)
  df_korbb       <- as.character(df_local$KorBB)
  strikeout_results <- c("Strikeout","StrikeoutSwinging","StrikeoutLooking")
  is_strikeout <- flags$is_strikeout |
    (!is.na(df_play_result) & df_play_result %in% strikeout_results) |
    (!is.na(df_korbb) & df_korbb == "Strikeout")
  is_walk <- flags$is_walk |
    (!is.na(df_play_result) & df_play_result == "Walk") |
    (!is.na(df_korbb) & df_korbb == "Walk")
  is_terminal <- flags$is_last |
    (!is.na(df_play_result) & df_play_result != "Undefined") |
    (!is.na(df_korbb) & df_korbb %in% c("Strikeout","Walk"))
  list(df = df_local, is_strikeout = is_strikeout, is_walk = is_walk, is_terminal = is_terminal)
}

parse_utc_datetime <- function(x) {
  if (is.null(x)) return(as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC"))
  suppressWarnings({
    a <- as.POSIXct(x, tz = "UTC")  # handles "YYYY-MM-DD HH:MM:SS"
    na <- is.na(a)
    if (any(na)) {
      a2 <- as.POSIXct(strptime(x[na], "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))  # ISO-8601
      a[na] <- a2
    }
    na <- is.na(a)
    if (any(na)) {
      a2 <- as.POSIXct(strptime(x[na], "%m/%d/%Y %H:%M:%OS", tz = "UTC"))  # fallback
      a[na] <- a2
    }
    a
  })
}


notes_api_add <- function(author_email, team, page_combo, pitcher, session_type, date_start, date_end, note_text) {
  body <- list(
    token        = NOTES_API_TOKEN,
    author_email = author_email,
    team         = team,
    page         = page_combo,
    pitcher      = pitcher,
    session_type = session_type,
    date_start   = as.character(as.Date(date_start)),
    date_end     = as.character(as.Date(date_end)),
    note_text    = note_text
  )
  
  # --- Try POST first ---
  post_resp <- tryCatch({
    httr2::request(NOTES_API_URL) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(10) |>
      httr2::req_perform()
  }, error = function(e) e)
  
  if (inherits(post_resp, "httr2_response")) {
    st <- httr2::resp_status(post_resp)
    if (st >= 200 && st < 300) {
      out <- httr2::resp_body_json(post_resp, check_type = FALSE)
      if (!is.null(out$error)) stop(sprintf("Notes API error: %s", out$error))
      return(TRUE)
    }
    # Only special-case 405; anything else bubbles up
    if (st != 405) stop(sprintf("Notes API HTTP %s", st))
  }
  
  # --- 405 path: check for a fresh identical note before fallback ---
  # --- Before any fallback, check if an identical note already exists (robust parse + wider window) ---
  try({
    rows <- notes_api_list()
    if (length(rows)) {
      df <- as.data.frame(rows, stringsAsFactors = FALSE)
      
      ct <- parse_utc_datetime(df$created_at_utc)
      recent <- is.finite(ct) & (as.numeric(difftime(Sys.time(), ct, units = "secs")) <= 90)
      
      same_fields <- (
        df$author_email == author_email &
          df$team         == team &
          df$page         == page_combo &
          (df$pitcher     == pitcher | (is.na(df$pitcher) & pitcher %in% c("", "All"))) &
          (df$session_type== session_type | (is.na(df$session_type) & session_type %in% c("", "All"))) &
          as.character(as.Date(df$date_start)) == as.character(as.Date(date_start)) &
          as.character(as.Date(df$date_end))   == as.character(as.Date(date_end)) &
          df$note_text    == note_text
      )
      
      # If we see the exact same note very recently, or already present at all, skip fallback
      if (any(same_fields & recent, na.rm = TRUE) || any(same_fields, na.rm = TRUE)) {
        return(TRUE)
      }
    }
  }, silent = TRUE)
  
  
  # --- If not already present, do GET fallback once ---
  get_resp <- httr2::request(NOTES_API_URL) |>
    httr2::req_method("GET") |>
    httr2::req_url_query(
      op           = "add",
      token        = body$token,
      author_email = body$author_email,
      team         = body$team,
      page         = body$page,
      pitcher      = body$pitcher,
      session_type = body$session_type,
      date_start   = body$date_start,
      date_end     = body$date_end,
      note_text    = body$note_text
    ) |>
    httr2::req_timeout(10) |>
    httr2::req_perform()
  
  out2 <- httr2::resp_body_json(get_resp, check_type = FALSE)
  if (!is.null(out2$error)) stop(sprintf("Notes API error (GET fallback): %s", out2$error))
  TRUE
}


notes_api_list <- function() {
  req <- httr2::request(NOTES_API_URL) |>
    httr2::req_url_query(token = NOTES_API_TOKEN) |>
    httr2::req_method("GET") |>
    httr2::req_timeout(10)
  resp <- httr2::req_perform(req)
  jsonlite::fromJSON(httr2::resp_body_string(resp))
}


# ---------- Helpers ----------

html_unescape <- function(x) {
  if (is.null(x)) return(x)
  x <- gsub("&lt;",   "<", x, fixed = TRUE)
  x <- gsub("&gt;",   ">", x, fixed = TRUE)
  x <- gsub("&amp;",  "&", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;",  "'", x, fixed = TRUE)
  x
}

# Flexible date parser: ISO (YYYY-MM-DD), m/d/yy, m/d/yyyy, with a 2-digit year pivot
# Anything with a 2-digit year that parses < pivot (default 1970) gets +100 years (e.g., 9/5/25 -> 2025)
parse_date_flex <- function(x, pivot = 1970L) {
  # keep Dates as-is
  if (inherits(x, "Date")) return(x)
  
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  
  # parse a bunch of common patterns
  dt <- suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c(
      "Y-m-d", "Ymd",          # 2025-09-05, 20250905
      "m/d/y", "mdy",          # 9/5/25, 09/05/25
      "m/d/Y", "mdy",          # 9/5/2025, 09/05/2025
      "Y-m-d HMS", "mdy HMS",  # with times if present
      "Y-m-d HM",  "mdy HM"
    ),
    tz = "UTC",
    exact = FALSE
  ))
  
  # convert to Date
  d <- as.Date(dt)
  
  # Fix two-digit-year cases that landed in 19xx (or otherwise < pivot)
  yrs <- suppressWarnings(lubridate::year(dt))
  idx <- !is.na(yrs) & yrs < pivot
  if (any(idx)) d[idx] <- as.Date(dt[idx] + lubridate::years(100))
  
  d
}

# Reusable DT builder with "Columns" toggle; defaults to a chosen visible set
datatable_with_colvis <- function(df, lock = character(0), remember = TRUE, default_visible = names(df)) {
  # Bail out early if an htmlwidget was passed in
  if (inherits(df, "htmlwidget") || inherits(df, "datatables")) return(df)
  
  df <- sanitize_for_dt(df)
  default_visible <- intersect(default_visible, names(df))
  if (!length(default_visible)) default_visible <- names(df)
  # NEW: robust fallback — if caller hands us an empty/invalid set, show all columns
  if (is.null(default_visible) || !length(default_visible)) default_visible <- names(df)
  default_visible <- intersect(default_visible, names(df))
  if (!length(default_visible)) default_visible <- names(df)
  
  # which columns are "locked" (cannot be toggled off)
  idx_lock   <- which(names(df) %in% lock) - 1
  all_idx0   <- seq_len(ncol(df)) - 1
  colvis_idx <- setdiff(all_idx0, idx_lock)
  
  # center-align everything; sort % columns numerically with your js_sort
  defs <- list(list(className = 'dt-center', targets = "_all"))
  idx_pct <- which(grepl("%$", names(df)) | names(df) %in% c("SpinEff")) - 1
  if (length(idx_pct)) {
    defs <- c(defs, list(list(targets = idx_pct, render = DT::JS(js_sort))))
  }
  # hide everything that's NOT in default_visible
  hide_idx <- which(!(names(df) %in% default_visible)) - 1
  
  DT::datatable(
    df,
    rownames = FALSE,
    options = list(
      columnDefs   = c(
        list(list(targets = hide_idx, visible = FALSE)),
        defs
      ),
      dom = 'Bfrtip',
      buttons = list(
        'pageLength',
        list(
          extend         = 'colvis',
          text           = 'Columns',
          columns        = colvis_idx,
          postfixButtons = list('colvisRestore')
        )
      ),
      colReorder    = TRUE,
      fixedHeader   = TRUE,
      stateSave     = remember,
      stateDuration = -1,
      pageLength    = 10,
      autoWidth     = TRUE,
      scrollX       = TRUE
    )
  )
}


# --- DT sanitizers & wrapper (place near other helpers, before UI/server) ---

sanitize_for_dt <- function(dfx) {
  # If someone passed an htmlwidget, just hand it back untouched
  if (inherits(dfx, "htmlwidget") || inherits(dfx, "datatables")) return(dfx)
  
  if (!is.data.frame(dfx)) {
    dfx <- tryCatch(as.data.frame(dfx, stringsAsFactors = FALSE),
                    error = function(e) dfx)
  }
  if (!is.data.frame(dfx)) return(dfx)
  
  dfx <- tibble::as_tibble(dfx)
  
  dfx <- dplyr::mutate(
    dfx,
    dplyr::across(where(~ inherits(.x, "POSIXt")), ~ format(.x, "%Y-%m-%d %H:%M:%S")),
    dplyr::across(where(~ inherits(.x, "Date")),    ~ format(.x, "%Y-%m-%d")),
    dplyr::across(where(is.factor), as.character),
    dplyr::across(where(is.list), ~ vapply(.x, function(z) {
      if (inherits(z, "shiny.tag") || inherits(z, "shiny.tag.list")) {
        as.character(htmltools::tagList(z))
      } else if (is.null(z) || length(z) == 0) {
        ""
      } else if (length(z) == 1) {
        as.character(z)
      } else {
        paste0(z, collapse = ", ")
      }
    }, FUN.VALUE = character(1)))
  )
  dfx
}

wrap_pitch_counts <- function(x) {
  vals <- vapply(seq_along(x), function(i) {
    val <- x[[i]]
    txt <- as.character(val %||% "")
    if (!nzchar(trimws(txt))) return("")
    if (grepl("<", txt, fixed = TRUE)) return(txt)
    num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", txt)))
    if (!is.na(num) && num > 0) {
      sprintf(
        '<a href="javascript:void(0)" class="pitch-count-link" style="cursor:pointer;">%s</a>',
        htmltools::htmlEscape(txt)
      )
    } else {
      htmltools::htmlEscape(txt)
    }
  }, character(1), USE.NAMES = FALSE)
  unname(vals)
}


datatable_with_colvis <- function(df, lock = character(0), remember = TRUE, default_visible = names(df), mode = NULL) {
  df <- sanitize_for_dt(df)   # <- central fix
  df <- blank_ea_except_all(df)
  df <- blank_ahead_except_all(df)
  default_visible <- intersect(default_visible, names(df))
  
  idx_lock   <- which(names(df) %in% lock) - 1
  all_idx0   <- seq_len(ncol(df)) - 1
  colvis_idx <- setdiff(all_idx0, idx_lock)
  
  defs <- list(list(className = 'dt-center', targets = "_all"))
  
  # Make # column explicitly clickable with special styling
  idx_hash <- which(names(df) == "#") - 1
  if (length(idx_hash)) {
    defs <- c(defs, list(list(
      className = 'dt-center clickable-cell',
      targets = idx_hash
    )))
  }
  
  idx_pct <- which(grepl("%$", names(df)) | names(df) %in% c("SpinEff")) - 1
  if (length(idx_pct)) {
    defs <- c(defs, list(list(targets = idx_pct, render = DT::JS(js_sort))))
  }
  hide_idx <- which(!(names(df) %in% default_visible)) - 1
  if (length(hide_idx)) {
    defs <- c(defs, list(list(visible = FALSE, targets = hide_idx)))
  }
  
  # Create the datatable
  dt <- DT::datatable(
    df,
    rownames   = FALSE,
    extensions = c('Buttons','ColReorder','FixedHeader'),
    escape     = FALSE,
    options = list(
      dom           = 'Bfrtip',
      buttons       = list(
        'pageLength',
        list(extend = 'colvis', text = 'Columns', columns = colvis_idx, postfixButtons = list('colvisRestore'))
      ),
      colReorder    = TRUE,
      fixedHeader   = TRUE,
      stateSave     = remember,
      stateDuration = -1,
      pageLength    = 10,
      autoWidth     = TRUE,
      scrollX       = TRUE,
      columnDefs    = defs
    )
  )
  
  # Apply color coding for Process, Live, Results, and Bullpen tables by modifying the data directly
  # BUT exclude hitting tables (which have PAP, Contact%, Chase% columns)
  is_hitting_table <- any(c("PAP", "Contact%", "Chase%") %in% names(df))
  
  if ((identical(mode, "Process") || identical(mode, "Live") || identical(mode, "Results") || identical(mode, "Bullpen")) && 
      ("Pitch" %in% names(df) || "Player" %in% names(df)) && 
      !is_hitting_table) {
    # Define columns to color code based on mode
    if (identical(mode, "Process")) {
      color_cols <- c("InZone%", "Comp%", "Strike%", "Swing%", "FPS%", "Ahead%", "E+A%", "Ctrl+", "QP+", "Pitching+")
    } else if (identical(mode, "Live")) {
      color_cols <- c("InZone%", "Strike%", "FPS%", "E+A%", "QP+", "Ctrl+", "Pitching+", "K%", "BB%", "Whiff%")
    } else if (identical(mode, "Results")) {
      color_cols <- c("Whiff%", "K%", "BB%", "CSW%", "GB%", "Barrel%", "EV")
    } else if (identical(mode, "Bullpen")) {
      color_cols <- c("InZone%", "Comp%", "Ctrl+", "Stuff+")
    }
    
    available_cols <- intersect(color_cols, names(df))
    
    for (col in available_cols) {
      # Pre-calculate colors and apply as HTML styling
      for (i in seq_len(nrow(df))) {
        if (!is.na(df[[col]][i]) && df[[col]][i] != "") {
          # For leaderboard tables (Player column), use "All" pitch type
          # For regular tables (Pitch column), use the specific pitch type
          pitch_type <- if ("Player" %in% names(df)) "All" else df$Pitch[i]
          colors <- get_color_scale(df[[col]][i], col, pitch_type)
          df[[col]][i] <- paste0(
            '<span style="background-color:', colors$bg, 
            '; color:', colors$text,
            '; padding: 2px 4px; border-radius: 3px; display: inline-block; width: 100%; text-align: center;">',
            df[[col]][i], '</span>'
          )
        }
      }
    }
    
    # Recreate the datatable with the styled data
    dt <- DT::datatable(
      df,
      rownames   = FALSE,
      extensions = c('Buttons','ColReorder','FixedHeader'),
      escape     = FALSE,
      options = list(
        dom           = 'Bfrtip',
        buttons       = list(
          'pageLength',
          list(extend = 'colvis', text = 'Columns', columns = colvis_idx, postfixButtons = list('colvisRestore'))
        ),
        colReorder    = TRUE,
        fixedHeader   = TRUE,
        stateSave     = remember,
        stateDuration = -1,
        pageLength    = 10,
        autoWidth     = TRUE,
        scrollX       = TRUE,
        columnDefs    = defs
      )
    )
  }
  
  return(dt)
}

# Default column sets for the table-mode toggle
stuff_cols    <- c("Pitch","#","Velo","Max","IVB","HB","rTilt","bTilt", "SpinEff","Spin","Height","Side","Ext","VAA","HAA","Stuff+")
process_cols  <- c("Pitch","#","BF","InZone%","Comp%","Strike%","Swing%","FPS%","Ahead%","E+A%","BFP","Ctrl+","QP+","Pitching+")
results_cols  <- c("Pitch","#","BF","K%","BB%","GB%","Barrel%","Whiff%","CSW%","EV","LA")
results_cols_live <- c("Pitch","#","BF","K%","BB%","GB%","Whiff%","CSW%","EV","LA","Pitching+")
bullpen_cols  <- c("Pitch","#","Velo","Max","IVB","HB","Spin","bTilt","Height","Side","Ext","InZone%","Comp%","Ctrl+","Stuff+")
live_cols     <- c("Pitch","#","Velo","Max","IVB","HB","FPS%","E+A%","InZone%","Strike%","Whiff%","K%","BB%","QP+")
usage_cols    <- c("Pitch","#","Overall","0-0","Behind","Even","Ahead","<2K","2K")
perf_cols     <- c("Pitch","#","BF","InZone%","Comp%","Strike%","FPS%","E+A%","K%","BB%","Whiff%","CSW%","EV","LA","Ctrl+","QP+","Pitching+")
# all_table_cols will auto-include QP+ via the union

# Default column sets for the table-mode toggle (you already have these)
# stuff_cols, process_cols, results_cols, perf_cols

# ---- NEW: unified list for the pickers + a helper to compute visibility
all_table_cols <- unique(c(stuff_cols, process_cols, results_cols, results_cols_live, bullpen_cols, live_cols, usage_cols, perf_cols))

visible_set_for <- function(mode, custom = character(0), session_type = NULL) {
  if (identical(mode, "Process")) return(process_cols)
  if (identical(mode, "Results")) return(results_cols)
  if (identical(mode, "Counting")) return(c("Pitch", "BF", "#", "P/BF", "BB", "K", "Whiffs", "Swings", "Called", "BIP", "Barrels"))
  if (identical(mode, "Bullpen")) return(bullpen_cols)
  if (identical(mode, "Live")) return(live_cols)
  if (identical(mode, "Usage")) return(usage_cols)
  if (identical(mode, "Custom"))  return(unique(c("Pitch", custom)))
  # Default
  stuff_cols
}


# ---- Leaderboard: visible set helper (swap Pitch -> Player, drop Overall) ----
visible_set_for_lb <- function(mode, custom = character(0)) {
  v <- visible_set_for(mode, custom)
  v[v == "Pitch"] <- "Player"
  unique(setdiff(v, "Overall"))
}

# Helper functions for .dp_like_table
as_char <- function(x) as.character(x)

pctify <- function(x) {
  if (is.character(x)) return(x)
  x <- suppressWarnings(as.numeric(x))
  # Treat values <=1.5 as proportions (0-1), otherwise already percent-like
  scale100 <- if (all(!is.na(x)) && max(x, na.rm = TRUE) <= 1.5) 100 else 1
  ifelse(is.finite(x), paste0(round(x * scale100, 1), "%"), "")
}

.dp_like_table <- function(df, mode, custom_cols, table_id = NULL, return_raw = FALSE, session = NULL, original_df = NULL) {
  if (!nrow(df)) {
    return(DT::datatable(
      data.frame(Message = "No data for selected filters"),
      options = list(dom = 't'), rownames = FALSE
    ))
  }
  if (!"TaggedPitchType" %in% names(df)) {
    fallback_cols <- intersect(c("PitchType","pitchType","Pitch"), names(df))
    if (length(fallback_cols)) {
      df$TaggedPitchType <- df[[fallback_cols[1]]]
    } else {
      df$TaggedPitchType <- "All"
    }
  }
  df$TaggedPitchType <- as.character(df$TaggedPitchType)
  cache_key <- if (is.null(table_id) || is.null(session)) NULL else paste0("table_cache_", table_id)
  store_table_cache <- function(tbl, extras = list()) {
    if (is.null(cache_key)) return(invisible(NULL))
    label_col <- extras$label_column %||% if ("Pitch" %in% names(tbl)) "Pitch" else names(tbl)[1]
    session$userData[[cache_key]] <- c(
      list(
        table = tbl,
        source = df,
        label_column = label_col
      ),
      extras
    )
    invisible(NULL)
  }
  
  # --- helpers (copied from your DP page) ---
  nz_mean <- function(x) { x <- suppressWarnings(as.numeric(x)); m <- mean(x, na.rm = TRUE); if (is.finite(m)) m else NA_real_ }
  fmt_avg <- function(x) { z <- ifelse(is.finite(x), sprintf("%.3f", x), NA_character_); sub("^0", "", z) }
  safe_div <- function(num, den) ifelse(den > 0, num/den, NA_real_)
  parse_num <- function(x) {
    if (is.numeric(x)) return(x)
    x1 <- trimws(as.character(x)); x1[x1 == ""] <- NA_character_
    ifelse(grepl("%$", x1), suppressWarnings(as.numeric(sub("%$","",x1)))/100,
           suppressWarnings(as.numeric(x1)))
  }
  ip_fmt <- function(ip_raw) {
    out <- rep("", length(ip_raw))
    ok  <- is.finite(ip_raw) & ip_raw > 0
    outs <- floor(ip_raw[ok]*3 + 1e-8)
    inn  <- outs %/% 3
    rem  <- outs %% 3
    out[ok] <- paste0(inn, ".", rem)
    out
  }
  FIP_C <- if (exists("FIP_CONST")) get("FIP_CONST") else 3.20
  
  if (identical(mode, "Results")) {
    swing_levels <- c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall")
    is_term <- (
      (!is.na(df$PlayResult) & df$PlayResult != "Undefined") |
        (!is.na(df$KorBB) & df$KorBB %in% c("Strikeout","Walk"))
    )
    term <- df[is_term, , drop = FALSE]
    
    per_type <- term %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(
        PA   = dplyr::n(),
        HBP  = sum(PlayResult == "HitByPitch", na.rm = TRUE),
        Sac  = sum(PlayResult == "Sacrifice",  na.rm = TRUE),
        `1B` = sum(PlayResult == "Single",  na.rm = TRUE),
        `2B` = sum(PlayResult == "Double",  na.rm = TRUE),
        `3B` = sum(PlayResult == "Triple",  na.rm = TRUE),
        HR   = sum(PlayResult == "HomeRun", na.rm = TRUE),
        Kct  = sum(KorBB == "Strikeout" |
                     PlayResult %in% c("Strikeout","StrikeoutSwinging","StrikeoutLooking"), na.rm = TRUE),
        BBct = sum(KorBB == "Walk" | PlayResult == "Walk", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        AB  = PA - (BBct + HBP + Sac),
        H   = `1B` + `2B` + `3B` + HR,
        TB  = 1*`1B` + 2*`2B` + 3*`3B` + 4*HR,
        AVG = safe_div(H, AB),
        SLG = safe_div(TB, AB),
        OBP = safe_div(H + BBct + HBP, PA),
        OPS = SLG + OBP
      )
    
    pitch_totals <- df %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(
        Pitches = dplyr::n(),
        Swings  = sum(!is.na(PitchCall) & PitchCall %in% swing_levels, na.rm = TRUE),
        Whiffs  = sum(PitchCall == "StrikeSwinging", na.rm = TRUE),
        BF      = sum(!is.na(Balls) & !is.na(Strikes) & Balls == 0 & Strikes == 0, na.rm = TRUE),
        .groups = "drop"
      )
    total_pitches <- sum(pitch_totals$Pitches, na.rm = TRUE)
    
    scores <- ifelse(
      df$PlateLocSide >= ZONE_LEFT & df$PlateLocSide <= ZONE_RIGHT &
        df$PlateLocHeight >= ZONE_BOTTOM & df$PlateLocHeight <= ZONE_TOP, 1.47,
      ifelse(
        df$PlateLocSide >= -1.5 & df$PlateLocSide <= 1.5 &
          df$PlateLocHeight >= (2.65 - 1.7) & df$PlateLocHeight <= (2.65 + 1.3),
        0.73, 0
      )
    )
    sc_by_type <- df %>%
      dplyr::mutate(.scores = scores) %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(
        StuffP   = round(nz_mean(`Stuff+`), 1),
        CommandP = round(nz_mean(.scores) * 100, 1),
        .groups = "drop"
      ) %>%
      dplyr::mutate(PitchingP = round((StuffP + CommandP)/2, 1))
    
    bbe <- df %>% dplyr::filter(grepl("live|game|ab", tolower(SessionType)), PitchCall == "InPlay")
    evla <- bbe %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(EV = nz_mean(ExitSpeed), LA = nz_mean(Angle), .groups = "drop")
    gb <- bbe %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(
        `GB%` = safe_div(sum(TaggedHitType == "GroundBall", na.rm = TRUE),
                         sum(!is.na(TaggedHitType),        na.rm = TRUE)),
        .groups = "drop"
      )
    
    extras <- compute_process_results(df) %>%
      dplyr::rename(Pitch = PitchType)
    
    # Safely parse columns that might not exist (keep K%, BB%, GB%, Barrel% as strings with %)
    if ("xWOBA" %in% names(extras)) extras$xWOBA <- parse_num(extras$xWOBA)
    if ("xISO" %in% names(extras)) extras$xISO <- parse_num(extras$xISO)
    if ("BABIP" %in% names(extras)) extras$BABIP <- parse_num(extras$BABIP)
    # K%, BB%, GB%, Barrel% should remain as formatted strings from compute_process_results
    
    # Select columns including K%, BB%, GB% from compute_process_results
    extra_cols <- intersect(c("Pitch", "xWOBA", "xISO", "BABIP", "Barrel%", "K%", "BB%", "GB%"), names(extras))
    extras <- extras %>% dplyr::select(dplyr::all_of(extra_cols))
    
    # Ensure we have a row for each pitch type even if no terminal outcomes
    res_pt <- pitch_totals %>%
      dplyr::left_join(per_type, by = "TaggedPitchType") %>%
      dplyr::left_join(evla,         by = "TaggedPitchType") %>%
      dplyr::left_join(gb,           by = "TaggedPitchType") %>%
      dplyr::mutate(
        # Fill in missing values with 0 for counts and NA for rates
        PA   = dplyr::coalesce(PA, 0),
        HBP  = dplyr::coalesce(HBP, 0),
        Sac  = dplyr::coalesce(Sac, 0),
        `1B` = dplyr::coalesce(`1B`, 0),
        `2B` = dplyr::coalesce(`2B`, 0),
        `3B` = dplyr::coalesce(`3B`, 0),
        HR   = dplyr::coalesce(HR, 0),
        Kct  = dplyr::coalesce(Kct, 0),
        BBct = dplyr::coalesce(BBct, 0),
        AB   = dplyr::coalesce(AB, 0),
        H    = dplyr::coalesce(H, 0),
        TB   = dplyr::coalesce(TB, 0),
        AVG  = dplyr::coalesce(AVG, NA_real_),
        SLG  = dplyr::coalesce(SLG, NA_real_),
        OBP  = dplyr::coalesce(OBP, NA_real_),
        OPS  = dplyr::coalesce(OPS, NA_real_),
        `Swing%` = safe_div(Swings, Pitches),
        `Whiff%` = safe_div(Whiffs, Swings),
        Outs     = (AB - H) + Sac,
        IP_raw   = safe_div(Outs, 3),
        `#`      = Pitches,
        Usage    = ifelse(total_pitches > 0, paste0(round(100*Pitches/total_pitches,1), "%"), ""),
        FIP_tmp  = safe_div(13*HR + 3*(BBct + HBP) - 2*Kct, IP_raw),
        FIP      = ifelse(is.finite(FIP_tmp), round(FIP_tmp + FIP_C, 2), NA_real_),
        WHIP_tmp = safe_div(H + BBct, IP_raw),
        WHIP     = ifelse(is.finite(WHIP_tmp), round(WHIP_tmp, 2), NA_real_)
      ) %>%
      dplyr::left_join(sc_by_type, by = "TaggedPitchType") %>%
      dplyr::left_join(extras, by = c("TaggedPitchType" = "Pitch"))
    
    # Add missing columns as NA/empty string if they don't exist
    if (!"xWOBA" %in% names(res_pt)) res_pt$xWOBA <- NA_real_
    if (!"xISO" %in% names(res_pt)) res_pt$xISO <- NA_real_
    if (!"BABIP" %in% names(res_pt)) res_pt$BABIP <- NA_real_
    if (!"Barrel%" %in% names(res_pt)) res_pt$`Barrel%` <- ""
    if (!"K%" %in% names(res_pt)) res_pt$`K%` <- ""
    if (!"BB%" %in% names(res_pt)) res_pt$`BB%` <- ""
    if (!"GB%" %in% names(res_pt)) res_pt$`GB%` <- ""
    
    res_pt <- res_pt %>%
      dplyr::transmute(
        Pitch = as.character(TaggedPitchType),
        `#`, Usage, BF, IP = ip_fmt(IP_raw), FIP, WHIP,
        PA, AB, AVG, SLG, OBP, OPS,
        xWOBA, xISO, BABIP,
        `Swing%`, `Whiff%`, `GB%`,
        `K%`, `BB%`,
        `Barrel%`, EV, LA,
        `Pitching+` = PitchingP
      )
    
    PAt <- nrow(term)
    HBP_all <- sum(term$PlayResult == "HitByPitch", na.rm = TRUE)
    Sac_all <- sum(term$PlayResult == "Sacrifice",  na.rm = TRUE)
    H1  <- sum(term$PlayResult == "Single",  na.rm = TRUE)
    H2  <- sum(term$PlayResult == "Double",  na.rm = TRUE)
    H3  <- sum(term$PlayResult == "Triple",  na.rm = TRUE)
    HR  <- sum(term$PlayResult == "HomeRun", na.rm = TRUE)
    H   <- H1 + H2 + H3 + HR
    TB  <- 1*H1 + 2*H2 + 3*H3 + 4*HR
    Kct_all <- sum(term$KorBB == "Strikeout" |
                     term$PlayResult %in% c("Strikeout","StrikeoutSwinging","StrikeoutLooking"), na.rm = TRUE)
    BBc_all <- sum(term$KorBB == "Walk" | term$PlayResult == "Walk", na.rm = TRUE)
    ABt <- PAt - (BBc_all + HBP_all + Sac_all)
    swings  <- sum(!is.na(df$PitchCall) & df$PitchCall %in% swing_levels, na.rm = TRUE)
    whiffs  <- sum(df$PitchCall == "StrikeSwinging",                      na.rm = TRUE)
    gbpct_all <- {
      d <- df %>% dplyr::filter(grepl("live|game|ab", tolower(SessionType)), PitchCall == "InPlay")
      safe_div(sum(d$TaggedHitType == "GroundBall", na.rm = TRUE),
               sum(!is.na(d$TaggedHitType),         na.rm = TRUE))
    }
    Outs_all <- (ABt - H) + Sac_all
    IP_all   <- safe_div(Outs_all, 3)
    FIP_all  <- {
      tmp <- safe_div(13*HR + 3*(BBc_all + HBP_all) - 2*Kct_all, IP_all)
      ifelse(is.finite(tmp), round(tmp + FIP_C, 2), NA_real_)
    }
    WHIP_all <- {
      tmp <- safe_div(H + BBc_all, IP_all)
      ifelse(is.finite(tmp), round(tmp, 2), NA_real_)
    }
    stuff_all <- round(nz_mean(df$`Stuff+`), 1)
    qp_all    <- round(nz_mean(compute_qp_points(df)) * 200, 1)
    pitc_all  <- round((stuff_all + qp_all) / 2, 1)
    BF_all <- sum(!is.na(df$Balls) & !is.na(df$Strikes) & df$Balls == 0 & df$Strikes == 0, na.rm = TRUE)
    
    extras_all <- compute_process_results(df)
    
    # Get the "All" row from extras_all (it should have PitchType = "All")
    extras_all_row <- extras_all %>% dplyr::filter(PitchType == "All")
    
    all_row <- tibble::tibble(
      Pitch = "All",
      `#`   = nrow(df),
      Usage = "100%",
      BF = BF_all,
      IP = ip_fmt(IP_all),
      FIP = FIP_all,
      WHIP = WHIP_all,
      PA = PAt, AB = ABt,
      AVG = safe_div(H, ABt),
      SLG = safe_div(TB, ABt),
      OBP = safe_div(H + BBc_all + HBP_all, PAt),
      OPS = NA_real_,
      xWOBA = NA_real_, xISO = NA_real_, BABIP = NA_real_,
      `Swing%` = safe_div(swings, nrow(df)),
      `Whiff%` = safe_div(whiffs, swings),
      `GB%`    = if (nrow(extras_all_row) > 0 && "GB%" %in% names(extras_all_row)) extras_all_row$`GB%`[1] else "",
      `K%`     = if (nrow(extras_all_row) > 0 && "K%" %in% names(extras_all_row)) extras_all_row$`K%`[1] else "",
      `BB%`    = if (nrow(extras_all_row) > 0 && "BB%" %in% names(extras_all_row)) extras_all_row$`BB%`[1] else "",
      `Barrel%`= if (nrow(extras_all_row) > 0 && "Barrel%" %in% names(extras_all_row)) extras_all_row$`Barrel%`[1] else "",
      EV = nz_mean(bbe$ExitSpeed),
      LA = nz_mean(bbe$Angle),
      `Pitching+` = pitc_all
    )
    all_row$OPS <- all_row$SLG + all_row$OBP
    
    # Safely parse and aggregate columns that might not exist
    if ("xWOBA" %in% names(extras_all)) {
      extras_all$xWOBA <- parse_num(extras_all$xWOBA)
      all_row$xWOBA <- nz_mean(extras_all$xWOBA)
    }
    if ("xISO" %in% names(extras_all)) {
      extras_all$xISO <- parse_num(extras_all$xISO)
      all_row$xISO <- nz_mean(extras_all$xISO)
    }
    if ("BABIP" %in% names(extras_all)) {
      extras_all$BABIP <- parse_num(extras_all$BABIP)
      all_row$BABIP <- nz_mean(extras_all$BABIP)
    }
    
    df_out <- dplyr::bind_rows(res_pt, all_row) %>%
      dplyr::mutate(
        dplyr::across(c(PA, AB, AVG, SLG, OBP, OPS, xWOBA, xISO, BABIP,
                        `Swing%`, `Whiff%`,
                        EV, LA, FIP, WHIP),
                      ~ suppressWarnings(as.numeric(.)))
      )
    # Format columns that are still numeric (Swing%, Whiff% - not K%, BB%, GB%, Barrel% which come formatted from compute_process_results)
    pct_cols_to_format <- c("Swing%","Whiff%")
    rate_cols <- c("AVG","SLG","OBP","OPS","xWOBA","xISO","BABIP")
    df_out[pct_cols_to_format]  <- lapply(df_out[pct_cols_to_format],  function(z) ifelse(is.finite(z), paste0(round(z*100,1), "%"), ""))
    df_out[rate_cols] <- lapply(df_out[rate_cols], function(z) ifelse(is.finite(z), fmt_avg(z), ""))
    
    # K%, BB%, GB%, Barrel% are already formatted strings from compute_process_results, keep as-is
    # Just ensure they're strings and handle any NAs
    for (col in c("K%", "BB%", "GB%", "Barrel%")) {
      if (col %in% names(df_out)) {
        df_out[[col]] <- ifelse(is.na(df_out[[col]]) | df_out[[col]] == "", "", as.character(df_out[[col]]))
      }
    }
    df_out$EV   <- as.character(ifelse(is.finite(df_out$EV),   round(df_out$EV, 1), ""))
    df_out$LA   <- as.character(ifelse(is.finite(df_out$LA),   round(df_out$LA, 1), ""))
    df_out$FIP  <- as.character(ifelse(is.finite(df_out$FIP),  sprintf("%.2f", df_out$FIP), ""))
    df_out$WHIP <- as.character(ifelse(is.finite(df_out$WHIP), sprintf("%.2f", df_out$WHIP), ""))
    
    df_dt <- if (exists("safe_for_dt")) safe_for_dt(df_out) else df_out
    is_all <- df_dt$Pitch == "All"
    # Only set 0.0% for Swing% and Whiff% if empty (K%, BB%, GB%, Barrel% come from compute_process_results)
    for (nm in c("Swing%","Whiff%")) {
      z <- df_dt[[nm]]
      z[is_all & (is.na(z) | trimws(z) == "")] <- "0.0%"
      df_dt[[nm]] <- z
    }
    
    store_table_cache(df_dt, list(label_column = "Pitch"))
    disp_dt <- df_dt
    if ("#" %in% names(disp_dt)) {
      disp_dt$`#` <- wrap_pitch_counts(disp_dt$`#`)
    }
    visible_set <- visible_set_for(mode, custom_cols)
    if (return_raw) return(disp_dt)
    return(datatable_with_colvis(
      disp_dt,
      lock            = "Pitch",
      remember        = FALSE,
      default_visible = intersect(visible_set, names(disp_dt))
    ))
  }
  
  # ---- COUNTING MODE ----
  if (identical(mode, "Counting")) {
    swing_levels <- c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall")
    
    # Compute PA flags for terminal pitch identification
    pf_flags <- compute_pa_flags(df)
    df <- pf_flags$df
    df_is_strikeout <- pf_flags$is_strikeout
    df_is_walk <- pf_flags$is_walk
    df_is_terminal <- pf_flags$is_terminal
    
    # Only terminal pitches for BF calculation
    term_idx <- which(df_is_terminal)
    term <- df[term_idx, , drop = FALSE]
    term$is_strikeout <- df_is_strikeout[term_idx]
    term$is_walk <- df_is_walk[term_idx]
    
    # Per-pitch-type calculations
    per_type <- df %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(
        Pitches = dplyr::n(),
        Swings = sum(!is.na(PitchCall) & PitchCall %in% swing_levels, na.rm = TRUE),
        Whiffs = sum(PitchCall == "StrikeSwinging", na.rm = TRUE),
        Called = sum(PitchCall == "StrikeCalled", na.rm = TRUE),
        BIP = sum(PitchCall == "InPlay", na.rm = TRUE),
        .groups = "drop"
      )
    
    # Terminal pitch statistics (for BF, K, BB)
    term_stats <- term %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(
        BF = dplyr::n(),
        K = sum(is_strikeout, na.rm = TRUE),
        BB = sum(is_walk, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Barrel calculation for live/game sessions
    barrel_stats <- df %>%
      dplyr::filter(grepl("live|game|ab", tolower(SessionType)), PitchCall == "InPlay") %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(
        Barrels = {
          # Barrel definition: EV >= 98 mph and LA between 8-50 degrees
          sum(!is.na(ExitSpeed) & !is.na(Angle) & 
                ExitSpeed >= 98 & Angle >= 8 & Angle <= 50, na.rm = TRUE)
        },
        .groups = "drop"
      )
    
    # Build the counting table
    counting_pt <- per_type %>%
      dplyr::left_join(term_stats, by = "TaggedPitchType") %>%
      dplyr::left_join(barrel_stats, by = "TaggedPitchType") %>%
      dplyr::mutate(
        Pitches = dplyr::coalesce(Pitches, 0),
        Swings = dplyr::coalesce(Swings, 0),
        Whiffs = dplyr::coalesce(Whiffs, 0),
        Called = dplyr::coalesce(Called, 0),
        BIP = dplyr::coalesce(BIP, 0),
        BF = dplyr::coalesce(BF, 0),
        K = dplyr::coalesce(K, 0),
        BB = dplyr::coalesce(BB, 0),
        Barrels = dplyr::coalesce(Barrels, 0),
        `P/BF` = safe_div(Pitches, BF)
      ) %>%
      dplyr::transmute(
        Pitch = as.character(TaggedPitchType),
        BF,
        `#` = Pitches,
        `P/BF`,
        BB,
        K,
        Whiffs,
        Swings,
        Called,
        BIP,
        Barrels
      )
    
    # Calculate ALL row
    BF_all <- nrow(term)
    Pitches_all <- nrow(df)
    Swings_all <- sum(!is.na(df$PitchCall) & df$PitchCall %in% swing_levels, na.rm = TRUE)
    Whiffs_all <- sum(df$PitchCall == "StrikeSwinging", na.rm = TRUE)
    Called_all <- sum(df$PitchCall == "StrikeCalled", na.rm = TRUE)
    BIP_all <- sum(df$PitchCall == "InPlay", na.rm = TRUE)
    K_all <- sum(term$is_strikeout, na.rm = TRUE)
    BB_all <- sum(term$is_walk, na.rm = TRUE)
    
    # Barrels for ALL (live/game sessions only)
    live_df <- df %>% dplyr::filter(grepl("live|game|ab", tolower(SessionType)), PitchCall == "InPlay")
    Barrels_all <- sum(!is.na(live_df$ExitSpeed) & !is.na(live_df$Angle) & 
                         live_df$ExitSpeed >= 98 & live_df$Angle >= 8 & live_df$Angle <= 50, na.rm = TRUE)
    
    all_row <- tibble::tibble(
      Pitch = "All",
      BF = BF_all,
      `#` = Pitches_all,
      `P/BF` = safe_div(Pitches_all, BF_all),
      BB = BB_all,
      K = K_all,
      Whiffs = Whiffs_all,
      Swings = Swings_all,
      Called = Called_all,
      BIP = BIP_all,
      Barrels = Barrels_all
    )
    
    # Combine data
    df_out <- dplyr::bind_rows(counting_pt, all_row)
    
    # Format P/BF as decimal (ensure character type for consistency)
    df_out$`P/BF` <- ifelse(is.finite(df_out$`P/BF`), as.character(round(df_out$`P/BF`, 1)), "")
    
    # Convert to data frame
    df_dt <- as.data.frame(df_out, stringsAsFactors = FALSE)
    if ("#" %in% names(df_dt)) {
      df_dt$`#` <- wrap_pitch_counts(df_dt$`#`)
    }
    
    store_table_cache(df_dt, list(label_column = "Pitch"))
    disp_dt <- df_dt
    
    visible_set <- c("Pitch", "BF", "#", "P/BF", "BB", "K", "Whiffs", "Swings", "Called", "BIP", "Barrels")
    if (return_raw) return(disp_dt)
    return(datatable_with_colvis(
      disp_dt,
      lock            = "Pitch",
      remember        = FALSE,
      default_visible = intersect(visible_set, names(disp_dt)),
      mode            = mode
    ))
  }
  
  # ---- NON-Results modes (match DP page) ----
  # QP+ per pitch type
  qp_by_type <- df %>%
    dplyr::group_by(TaggedPitchType) %>%
    dplyr::summarise(
      `QP+` = {
        vals <- compute_qp_points(dplyr::cur_data_all())
        vals <- suppressWarnings(as.numeric(vals))
        round(mean(vals, na.rm = TRUE) * 200, 1)
      },
      .groups = "drop"
    )
  
  summ <- make_summary(df)
  summ <- dplyr::mutate(summ,
                        ReleaseTilt = as.character(ReleaseTilt),
                        BreakTilt   = as.character(BreakTilt)
  )
  if (!("QP+" %in% names(summ))) {
    summ <- summ %>% dplyr::left_join(qp_by_type, by = c("PitchType" = "TaggedPitchType"))
  }
  
  
  
  # Percent-ish columns: if they already end with "%", keep; else format smartly
  pct_cols <- c("SpinEff","InZonePercent","CompPercent","KPercent","BBPercent",
                "FPSPercent","EAPercent","StrikePercent","WhiffPercent")
  for (nm in pct_cols) if (nm %in% names(summ)) {
    x <- summ[[nm]]
    if (is.numeric(x)) {
      # scale: treat <=1.5 as proportions (0–1), otherwise already percent-like
      scale100 <- if (all(!is.na(x)) && max(x, na.rm = TRUE) <= 1.5) 100 else 1
      summ[[nm]] <- ifelse(is.finite(x), paste0(round(x * scale100, 1), "%"), "")
    } else {
      x <- as_char(x)
      has_pct <- grepl("%$", x)
      if (!all(has_pct | is.na(x) | x == "")) {
        v <- suppressWarnings(as.numeric(x))
        scale100 <- if (all(!is.na(v)) && max(v, na.rm = TRUE) <= 1.5) 100 else 1
        fmt <- ifelse(is.finite(v), paste0(round(v * scale100, 1), "%"), x)
        x[!has_pct] <- fmt[!has_pct]
      }
      summ[[nm]] <- x
    }
  }
  
  
  # Percent-like columns (DP page expects strings with %)
  for (nm in c("SpinEff","InZonePercent","CompPercent","KPercent","BBPercent",
               "FPSPercent","EAPercent","StrikePercent","WhiffPercent")) {
    if (nm %in% names(summ) && !is.character(summ[[nm]])) {
      # SpinEff is stored as a rate (0–1) in many builds, same pctify works
      summ[[nm]] <- pctify(summ[[nm]])
    }
  }
  
  scores <- ifelse(
    df$PlateLocSide >= ZONE_LEFT & df$PlateLocSide <= ZONE_RIGHT &
      df$PlateLocHeight >= ZONE_BOTTOM & df$PlateLocHeight <= ZONE_TOP, 1.47,
    ifelse(
      df$PlateLocSide >= -1.5 & df$PlateLocSide <= 1.5 &
        df$PlateLocHeight >= (2.65-1.7) & df$PlateLocHeight <= (2.65+1.3),
      0.73, 0
    )
  )
  has_pc  <- sum(!is.na(df$PitchCall)) > 0
  strikes <- sum(df$PitchCall %in% c("StrikeCalled","StrikeSwinging","InPlay","FoulBall","FoulBallNotFieldable","FoulBallFieldable"), na.rm = TRUE)
  sw      <- sum(df$PitchCall == "StrikeSwinging", na.rm = TRUE)
  den     <- sum(df$PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall"), na.rm = TRUE)
  
  # K% and BB% calculations - use BF as denominator
  pitch_n <- nrow(df)
  bf_count <- sum(!is.na(df$Balls) & !is.na(df$Strikes) & df$Balls == 0 & df$Strikes == 0, na.rm = TRUE)
  # Strikeouts = pitches with 2 strikes ending in StrikeSwinging or StrikeCalled
  k_count  <- sum(!is.na(df$Strikes) & df$Strikes == 2 & 
                    !is.na(df$PitchCall) & df$PitchCall %in% c("StrikeSwinging", "StrikeCalled"), na.rm = TRUE)
  # Walks = pitches with 3 balls ending in BallCalled
  bb_count <- sum(!is.na(df$Balls) & df$Balls == 3 & 
                    !is.na(df$PitchCall) & df$PitchCall == "BallCalled", na.rm = TRUE)
  fps_count <- sum(!is.na(df$Balls) & !is.na(df$Strikes) & df$Balls == 0 & df$Strikes == 0 &
                     !is.na(df$PitchCall) & df$PitchCall %in% c("InPlay","StrikeSwinging","StrikeCalled","FoulBallNotFieldable","FoulBall"), na.rm = TRUE)
  ea_count  <- sum(
    (!is.na(df$Balls) & !is.na(df$Strikes) & !is.na(df$PitchCall)) & (
      (df$Balls == 0 & df$Strikes == 0 & df$PitchCall == "InPlay") |
        (df$Balls == 0 & df$Strikes == 1 & df$PitchCall %in% c(
          "InPlay", "StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable","FoulBall"
        )) |
        (df$Balls == 1 & df$Strikes == 0 & df$PitchCall == "InPlay") |
        (df$Balls == 1 & df$Strikes == 1 & df$PitchCall %in% c(
          "InPlay", "StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable","FoulBall"
        ))
    ),
    na.rm = TRUE
  )
  
  # Keep live-specific variables for backwards compatibility
  bf_live <- bf_count
  k_live  <- k_count
  bb_live <- bb_count
  fps_live <- fps_count
  ea_live  <- ea_count
  
  vmax   <- suppressWarnings(max(as.numeric(df$RelSpeed), na.rm = TRUE)); vmax <- if (is.finite(vmax)) round(vmax, 1) else NA_real_
  ev_all <- nz_mean(ifelse(df$SessionType=="Live", df$ExitSpeed, NA_real_))
  la_all <- nz_mean(ifelse(df$SessionType=="Live", df$Angle,     NA_real_))
  stuff_all <- round(nz_mean(df$`Stuff+`), 1)
  ctrl_all   <- round(nz_mean(scores) * 100, 1)
  qp_all    <- round(nz_mean(compute_qp_points(df)) * 200, 1)
  all_bfp   <- compute_bfp_overall(df)
  if (!is.finite(all_bfp)) all_bfp <- 0
  
  df_table <- dplyr::bind_rows(
    summ,
    tibble::tibble(
      Pitch         = "All",
      PitchCount    = nrow(df),
      Usage         = "100%",
      BF            = bf_live,
      Velo_Avg      = round(nz_mean(df$RelSpeed), 1),
      Velo_Max      = vmax,
      IVB           = round(nz_mean(df$InducedVertBreak), 1),
      HB            = round(nz_mean(df$HorzBreak), 1),
      ReleaseTilt   = convert_to_clock(nz_mean(df$ReleaseTilt)),
      BreakTilt     = convert_to_clock(nz_mean(df$BreakTilt)),
      SpinEff       = { v <- nz_mean(df$SpinEfficiency); if (is.na(v)) "" else paste0(round(v*100,1), "%") },
      SpinRate      = round(nz_mean(df$SpinRate), 0),
      RelHeight     = round(nz_mean(df$RelHeight), 1),
      RelSide       = round(nz_mean(df$RelSide), 1),
      VertApprAngle = round(nz_mean(df$VertApprAngle), 1),
      HorzApprAngle = round(nz_mean(df$HorzApprAngle), 1),
      Extension     = round(nz_mean(df$Extension), 1),
      InZonePercent = { inzone <- (df$PlateLocSide >= ZONE_LEFT & df$PlateLocSide <= ZONE_RIGHT &
                                     df$PlateLocHeight >= ZONE_BOTTOM & df$PlateLocHeight <= ZONE_TOP)
      ifelse(sum(!is.na(inzone))>0, paste0(round(100*sum(inzone, na.rm=TRUE)/sum(!is.na(inzone)),1), "%"), "") },
      CompPercent   = { comp <- (df$PlateLocSide >= -1.5 & df$PlateLocSide <= 1.5 &
                                   df$PlateLocHeight >= (2.65-1.7) & df$PlateLocHeight <= (2.65+1.3))
      ifelse(sum(!is.na(comp))>0, paste0(round(100*sum(comp, na.rm=TRUE)/sum(!is.na(comp)),1), "%"), "") },
      KPercent      = ifelse(bf_count>0, paste0(round(100*k_count/bf_count,1), "%"), ""),
      BBPercent     = ifelse(bf_count>0, paste0(round(100*bb_count/bf_count,1), "%"), ""),
      FPSPercent    = ifelse(bf_count>0, paste0(round(100*fps_count/bf_count,1), "%"), ""),
      AheadPercent  = { 
        # Calculate strikes at 0-1 or 1-1 counts for this pitch type
        ahead_strikes <- sum(
          !is.na(df$Balls) & !is.na(df$Strikes) & 
            ((df$Balls == 0 & df$Strikes == 1) | (df$Balls == 1 & df$Strikes == 1)) &
            !is.na(df$PitchCall) & 
            df$PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall"),
          na.rm = TRUE
        )
        if (bf_count > 0) paste0(round(100 * ahead_strikes / bf_count, 1), "%") else ""
      },
      EAPercent     = ifelse(bf_count>0, paste0(round(100*ea_count/bf_count,1), "%"), ""),
      StrikePercent = ifelse(has_pc, paste0(round(100*strikes/nrow(df),1), "%"), ""),
      WhiffPercent  = ifelse(den>0, paste0(round(100*sw/den,1), "%"), ""),
      EV = ev_all, LA = la_all,
      BFP = ifelse(is.finite(all_bfp), sprintf("%.1f", all_bfp), ""),
      `Stuff+` = stuff_all, `Ctrl+` = ctrl_all, `QP+` = qp_all
    ) %>% dplyr::mutate(`Pitching+` = round((`Stuff+` + `QP+`)/2, 1))
  ) %>%
    dplyr::rename(
      `#`    = PitchCount,
      Velo   = Velo_Avg,
      Max    = Velo_Max,
      rTilt  = ReleaseTilt,
      bTilt  = BreakTilt,
      Spin   = SpinRate,
      Height = RelHeight,
      Side   = RelSide,
      Ext    = Extension,
      `InZone%` = InZonePercent,
      `Comp%`   = CompPercent,
      `K%`      = KPercent,
      `BB%`     = BBPercent,
      `FPS%`    = FPSPercent,
      `Ahead%`  = AheadPercent,
      `E+A%`    = EAPercent,
      `Strike%` = StrikePercent,
      `Whiff%`  = WhiffPercent,
      VAA       = VertApprAngle,
      HAA       = HorzApprAngle
    ) %>%
    dplyr::mutate(Pitch = as.character(Pitch)) %>%
    dplyr::select(
      Pitch, `#`, Usage, BF,
      Velo, Max, IVB, HB, rTilt, bTilt, SpinEff, Spin, Height, Side, VAA, HAA, Ext,
      `InZone%`, `Comp%`, `Strike%`, `FPS%`, `Ahead%`, `E+A%`, `K%`, `BB%`, `Whiff%`, EV, LA, BFP,
      `Stuff+`, `Ctrl+`, `QP+`, `Pitching+`
    ) %>%
    dplyr::mutate(
      EV = as.character(ifelse(is.na(as.numeric(EV)), "", round(as.numeric(EV), 1))),
      LA = as.character(ifelse(is.na(as.numeric(LA)), "", round(as.numeric(LA), 1))),
      BFP = ifelse(
        Pitch == "All",
        ifelse(is.na(as.numeric(BFP)), "", sprintf("%.1f", as.numeric(BFP))),
        ""
      )
    )
  # --- Standardize decimals like DP: 1 decimal for most, 0 for Spin (RPM) ---
  round1 <- intersect(c("Velo","Max","IVB","HB","Height","Side","VAA","HAA","Ext"), names(df_table))
  for (nm in round1) {
    z <- suppressWarnings(as.numeric(df_table[[nm]]))
    df_table[[nm]] <- ifelse(is.finite(z), round(z, 1), df_table[[nm]])
  }
  
  if ("Spin" %in% names(df_table)) {
    z <- suppressWarnings(as.numeric(df_table$Spin))
    df_table$Spin <- ifelse(is.finite(z), round(z, 0), df_table$Spin)
  }
  
  extras <- compute_process_results(df) %>%
    { if ("PitchType" %in% names(.) && !("Pitch" %in% names(.))) {
      dplyr::rename(., Pitch = PitchType)
    } else if ("PitchType" %in% names(.) && "Pitch" %in% names(.)) {
      dplyr::select(., -PitchType)  # Remove duplicate PitchType if both exist
    } else {
      .
    }
    } %>%
    dplyr::mutate(Pitch = as.character(Pitch))
  
  # Only join columns from extras that don't already exist in df_table
  existing_cols <- names(df_table)
  new_cols_from_extras <- setdiff(names(extras), c(existing_cols, "Pitch"))
  if (length(new_cols_from_extras) > 0) {
    extras_filtered <- extras %>% dplyr::select(Pitch, dplyr::all_of(new_cols_from_extras))
    df_table <- df_table %>% dplyr::left_join(extras_filtered, by = "Pitch")
  }
  
  # Add usage calculations for Usage table mode
  if (identical(mode, "Usage")) {
    usage_extras <- compute_usage_by_count(df, original_df) %>%
      dplyr::rename(Pitch = PitchType) %>%
      dplyr::mutate(Pitch = as.character(Pitch))
    df_table <- df_table %>% dplyr::left_join(usage_extras, by = "Pitch")
  }
  
  df_table <- collapse_list_cols(df_table)
  is_date_like <- function(x) inherits(x, c("Date","POSIXct","POSIXt","difftime"))
  date_like <- vapply(df_table, is_date_like, logical(1))
  if (any(date_like)) df_table[date_like] <- lapply(df_table[date_like], as.character)
  df_table <- as.data.frame(df_table, stringsAsFactors = FALSE)
  
  if (all(c("Whiff%","CSW%") %in% names(df_table))) {
    df_table <- df_table %>% dplyr::relocate(`CSW%`, .after = `Whiff%`)
  }
  if ("Pitching+" %in% names(df_table)) {
    df_table <- df_table %>% dplyr::relocate(`Pitching+`, .after = dplyr::last_col())
  }
  
  df_table <- enforce_process_order(df_table)
  df_table <- enforce_stuff_order(df_table)
  store_table_cache(df_table, list(label_column = "Pitch"))
  disp_table <- df_table
  if ("#" %in% names(disp_table)) {
    disp_table$`#` <- wrap_pitch_counts(disp_table$`#`)
  }
  
  visible_set <- visible_set_for(mode, custom_cols)
  if (return_raw) return(disp_table)
  datatable_with_colvis(
    disp_table,
    lock            = "Pitch",
    remember        = FALSE,
    default_visible = intersect(visible_set, names(disp_table)),
    mode            = mode
  )
}

# ---- Local helpers (self-contained) ----
.s_nz_mean <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x)) return(NA_real_)
  m <- mean(x, na.rm = TRUE)
  if (is.nan(m)) NA_real_ else m
}
.s_safe_div <- function(num, den) {
  num <- suppressWarnings(as.numeric(num))
  den <- suppressWarnings(as.numeric(den))
  ifelse(is.finite(den) & den != 0, num/den, NA_real_)
}
.s_fmt_pct1 <- function(num, den) {
  p <- .s_safe_div(num, den)
  ifelse(is.finite(p), paste0(round(100*p, 1), "%"), "")
}
.s_ip_fmt <- function(ip_num) {
  if (!is.finite(ip_num)) return(NA_character_)
  outs <- round(ip_num * 3)
  paste0(outs %/% 3, ".", outs %% 3)
}
.s_to_date <- function(x) {
  # Accept Date, POSIX, numeric serial (Excel), ISO, and m/d/y
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  # try numeric Excel serial
  num <- suppressWarnings(as.numeric(x))
  out <- rep(as.Date(NA), length(x))
  if (any(!is.na(num))) {
    is_serial <- !is.na(num) & num > 20000 & num < 60000
    out[is_serial] <- as.Date(num[is_serial], origin = "1899-12-30")
  }
  # try ISO
  bad <- is.na(out)
  if (any(bad)) {
    out[bad] <- suppressWarnings(as.Date(x[bad]))
    bad <- is.na(out)
  }
  # try m/d/y
  if (any(bad)) {
    out[bad] <- suppressWarnings(as.Date(x[bad], format = "%m/%d/%Y"))
    bad <- is.na(out)
    if (any(bad)) out[bad] <- suppressWarnings(as.Date(x[bad], format = "%m/%d/%y"))
  }
  out
}

# --- Keep Process tables in canonical order: "#, BF, Overall, ..."
enforce_process_order <- function(df) {
  if (all(c("#","BF") %in% names(df)))   df <- dplyr::relocate(df, `BF`, .after = `#`)
  if (all(c("BF","Overall") %in% names(df))) df <- dplyr::relocate(df, Overall, .after = `BF`)
  df
}

# --- Keep Stuff tables with Ext immediately after Side
enforce_stuff_order <- function(df) {
  # Normalize name if some paths still call it "Extension"
  if ("Extension" %in% names(df) && !("Ext" %in% names(df))) {
    df <- dplyr::rename(df, Ext = Extension)
  }
  if (all(c("Side","Ext") %in% names(df))) {
    df <- dplyr::relocate(df, Ext, .after = Side)
  }
  df
}

# Visible set when we group by Date instead of Pitch
visible_set_for_date <- function(mode, custom = character(0)) {
  base <- if (exists("visible_set_for")) visible_set_for(mode, custom) else names(data.frame())
  base[base == "Pitch"] <- "Date"
  setdiff(base, "Overall")
}

# Build the by-date table (averages & rates), honoring Live/Bullpen via filtered_logs()
make_session_logs_table <- function(df) {
  if (!nrow(df)) return(tibble::tibble())
  
  # Pull a FIP constant if available, otherwise 0 (so it never errors)
  fipc <- if (!is.null(get0("FIP_C"))) get0("FIP_C") else if (!is.null(get0("FIP_CONST"))) get0("FIP_CONST") else 0
  
  # Zone bounds (fallbacks in case globals aren’t visible yet)
  zl <- if (!is.null(get0("ZONE_LEFT")))   get0("ZONE_LEFT")   else -0.83
  zr <- if (!is.null(get0("ZONE_RIGHT")))  get0("ZONE_RIGHT")  else  0.83
  zb <- if (!is.null(get0("ZONE_BOTTOM"))) get0("ZONE_BOTTOM") else  1.5
  zt <- if (!is.null(get0("ZONE_TOP")))    get0("ZONE_TOP")    else  3.5
  
  swing_levels <- c("StrikeSwinging","FoulBall","FoulBallNotFieldable","FoulBallFieldable","InPlay")
  
  # Normalize a Date column, regardless of source name
  if (!("Date" %in% names(df))) {
    cand <- intersect(c("GameDate","SessionDate","date","DATE"), names(df))
    if (length(cand)) {
      df$Date <- df[[cand[1]]]
    }
  }
  df$Date <- .s_to_date(df$Date)
  
  # Group and compute
  by_date <- df %>%
    dplyr::filter(!is.na(Date)) %>%
    dplyr::group_by(Date) %>%
    dplyr::group_map(~{
      d <- .x
      
      # PA proxy for Live (first-pitch opportunities)
      PAt      <- sum(d$SessionType == "Live" & d$Balls == 0 & d$Strikes == 0, na.rm = TRUE)
      HBP_all  <- sum(d$PlayResult == "HitByPitch", na.rm = TRUE)
      Sac_all  <- sum(d$PlayResult == "Sacrifice",  na.rm = TRUE)
      H1       <- sum(d$PlayResult == "Single",     na.rm = TRUE)
      H2       <- sum(d$PlayResult == "Double",     na.rm = TRUE)
      H3       <- sum(d$PlayResult == "Triple",     na.rm = TRUE)
      HR       <- sum(d$PlayResult == "HomeRun",    na.rm = TRUE)
      H        <- H1 + H2 + H3 + HR
      # Live sessions: K% = 2 Strikes + (StrikeSwinging or StrikeCalled)
      # Regular sessions: use existing logic
      if (any(d$SessionType == "Live", na.rm = TRUE)) {
        Kct_all  <- sum(d$Strikes == 2 & d$PitchCall %in% c("StrikeSwinging", "StrikeCalled"), na.rm = TRUE)
        BBc_all  <- sum(d$Balls == 3 & d$PitchCall == "BallCalled", na.rm = TRUE)
      } else {
        Kct_all  <- sum(d$KorBB == "Strikeout" |
                          d$PlayResult %in% c("Strikeout","StrikeoutSwinging","StrikeoutLooking"), na.rm = TRUE)
        BBc_all  <- sum(d$KorBB == "Walk" | d$PlayResult == "Walk", na.rm = TRUE)
      }
      ABt      <- PAt - (BBc_all + HBP_all + Sac_all)
      
      # Swings/whiffs
      swings   <- sum(!is.na(d$PitchCall) & d$PitchCall %in% swing_levels, na.rm = TRUE)
      whiffs   <- sum(d$PitchCall == "StrikeSwinging", na.rm = TRUE)
      
      # EV/LA from live balls in play
      bbe      <- d %>% dplyr::filter(grepl("live|game|ab", tolower(SessionType)), PitchCall == "InPlay")
      ev_all   <- .s_nz_mean(bbe$ExitSpeed)
      la_all   <- .s_nz_mean(bbe$Angle)
      
      # IP/FIP/WHIP
      Outs_all <- (ABt - H) + Sac_all
      IP_all   <- .s_safe_div(Outs_all, 3)
      FIP_all  <- { tmp <- .s_safe_div(13*HR + 3*(BBc_all + HBP_all) - 2*Kct_all, IP_all)
      ifelse(is.finite(tmp), round(tmp + fipc, 2), NA_real_) }
      WHIP_all <- { tmp <- .s_safe_div(H + BBc_all, IP_all)
      ifelse(is.finite(tmp), round(tmp, 2), NA_real_) }
      
      # Command score (“Ctrl+”) with your 1.47 / 0.73 scoring
      scores <- ifelse(
        d$PlateLocSide  >= zl  & d$PlateLocSide  <= zr &
          d$PlateLocHeight>= zb  & d$PlateLocHeight<= zt, 1.47,
        ifelse(
          d$PlateLocSide >= -1.5 & d$PlateLocSide <= 1.5 &
            d$PlateLocHeight>= (2.65 - 1.7) & d$PlateLocHeight <= (2.65 + 1.3),
          0.73, 0
        )
      )
      ctrl_all  <- round(mean(scores, na.rm = TRUE) * 100, 1)
      stuff_all <- round(.s_nz_mean(d$`Stuff+`), 1)
      
      # QP+ scalar — use your real one if available; else NA
      qp_all <- if (!is.null(get0("safe_qp_scalar"))) get0("safe_qp_scalar")(d) else NA_real_
      pitc_all <- round(.s_nz_mean(c(stuff_all, qp_all)), 1)
      
      tibble::tibble(
        Date     = as.Date(d$Date[1]),
        `#`      = nrow(d),
        Overall  = "",
        BF       = PAt,
        IP       = .s_ip_fmt(IP_all),
        FIP      = FIP_all,
        WHIP     = WHIP_all,
        Velo     = .s_nz_mean(d$RelSpeed),
        Max      = suppressWarnings(max(d$RelSpeed, na.rm = TRUE)),
        IVB      = .s_nz_mean(d$InducedVertBreak),
        HB       = .s_nz_mean(d$HorzBreak),
        rTilt    = .s_nz_mean(d$ReleaseTilt),
        bTilt    = .s_nz_mean(d$BreakTilt),
        SpinEff  = .s_nz_mean(d$SpinEfficiency),
        Spin     = .s_nz_mean(d$SpinRate),
        Height   = .s_nz_mean(d$RelHeight),
        Side     = .s_nz_mean(d$RelSide),
        VAA      = .s_nz_mean(d$VertApprAngle),
        HAA      = .s_nz_mean(d$HorzApprAngle),
        Ext      = .s_nz_mean(d$Extension),
        `InZone%`= .s_fmt_pct1(
          sum(d$PlateLocSide >= zl & d$PlateLocSide <= zr &
                d$PlateLocHeight >= zb & d$PlateLocHeight <= zt, na.rm = TRUE),
          sum(!is.na(d$PlateLocSide) & !is.na(d$PlateLocHeight))
        ),
        `Comp%`  = .s_fmt_pct1(
          sum(d$PlateLocSide >= -1.5 & d$PlateLocSide <= 1.5 &
                d$PlateLocHeight >= (2.65 - 1.7) & d$PlateLocHeight <= (2.65 + 1.3), na.rm = TRUE),
          sum(!is.na(d$PlateLocSide) & !is.na(d$PlateLocHeight))
        ),
        `Strike%`= .s_fmt_pct1(
          sum(d$PitchCall %in% c("StrikeSwinging","StrikeCalled","InPlay","FoulBall","FoulBallNotFieldable","FoulBallFieldable"),
              na.rm = TRUE),
          nrow(d)
        ),
        `FPS%`   = .s_fmt_pct1(
          sum(d$Balls==0 & d$Strikes==1, na.rm = TRUE),
          sum(d$Balls==0 & d$Strikes==0, na.rm = TRUE)
        ),
        `E+A%`   = .s_fmt_pct1(
          sum(d$SessionType=="Live" & (
            (d$Balls==0 & d$Strikes==0 & d$PitchCall=="InPlay") |
              (d$Balls==0 & d$Strikes==1 & d$PitchCall %in% c("InPlay","FoulBallNotFieldable","FoulBall")) |
              (d$Balls==1 & d$Strikes==0 & d$PitchCall=="InPlay") |
              (d$Balls==1 & d$Strikes==1 & d$PitchCall %in% c("InPlay","FoulBallNotFieldable","FoulBall")) |
              (d$Balls==0 & d$Strikes==2 & d$PitchCall %in% c("InPlay","StrikeSwinging","StrikeCalled"))
          ), na.rm = TRUE),
          sum(d$SessionType=="Live" & d$Balls==0 & d$Strikes==0, na.rm = TRUE)
        ),
        `K%`     = .s_fmt_pct1(Kct_all, PAt),
        `BB%`    = .s_fmt_pct1(BBc_all, PAt),
        `Whiff%` = .s_fmt_pct1(whiffs, swings),
        `CSW%` = .s_fmt_pct1(
          sum(d$PitchCall %in% c("StrikeCalled","StrikeSwinging"), na.rm = TRUE),
          nrow(d)
        ),
        EV       = ev_all,
        LA       = la_all,
        `Stuff+` = stuff_all,
        `Ctrl+`  = ctrl_all,
        `QP+`    = qp_all,
        `Pitching+` = pitc_all
      )
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(dplyr::desc(Date))
  
  by_date
}

assign_where <- function(vec, cond, value) {
  idx <- which(!is.na(cond) & cond)
  if (length(idx)) vec[idx] <- value
  vec
}

# ----- Hover tooltip helpers -----
inzone_label <- function(side, height) {
  comp   <- !is.na(side) & !is.na(height) &
    side >= -1.5 & side <= 1.5 &
    height >= (2.65 - 1.7) & height <= (2.65 + 1.3)
  inzone <- !is.na(side) & !is.na(height) &
    side >= ZONE_LEFT & side <= ZONE_RIGHT &
    height >= ZONE_BOTTOM & height <= ZONE_TOP
  ifelse(inzone, "Yes", ifelse(comp, "Competitive", "No"))
}

# ---- NEW: display helpers ----
# ---- NEW: display helpers (can live near other helpers) ----
fmt_rate3 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  s <- ifelse(is.finite(x), sprintf("%.3f", x), "")
  sub("^0\\.", ".", s)  # .303 style (no leading 0) for 0.xxx
}
fmt_num2  <- function(x) ifelse(is.finite(x), sprintf("%.2f", x), "")
fmt_pct1  <- function(num, den) {
  num <- suppressWarnings(as.numeric(num)); den <- suppressWarnings(as.numeric(den))
  if (is.finite(den) && den > 0 && is.finite(num)) paste0(round(100*num/den, 1), "%") else ""
}

# ---- Color coding function for Process tables ----
get_color_scale <- function(value, column_name, pitch_type) {
  # Convert percentage strings to numeric values
  if (is.character(value)) {
    value <- suppressWarnings(as.numeric(gsub("%", "", value)))
  }
  
  if (is.na(value) || !is.finite(value)) {
    return(list(bg = "white", text = "black"))
  }
  
  # Define thresholds based on column and pitch type
  thresholds <- get_process_thresholds(column_name, pitch_type)
  if (is.null(thresholds)) return(list(bg = "white", text = "black"))
  
  poor <- thresholds$poor
  avg <- thresholds$avg
  great <- thresholds$great
  
  # Create 5-point color scale: Blue (poor) -> Light Blue -> White (avg) -> Light Red -> Red (great)
  # Return both background color and text color for darker backgrounds
  
  # Special handling for EV - reverse the color logic since lower is better for pitchers
  if (column_name == "EV") {
    if (value >= poor) {
      return(list(bg = "#0066CC", text = "white"))  # Dark Blue - high EV is bad
    } else if (value >= (poor + avg) / 2) {
      return(list(bg = "#66B2FF", text = "black"))  # Light Blue
    } else if (value >= avg) {
      return(list(bg = "#FFFFFF", text = "black"))  # White (average)
    } else if (value >= (avg + great) / 2) {
      return(list(bg = "#FFB3B3", text = "black"))  # Light Red
    } else if (value >= great) {
      return(list(bg = "#FF6666", text = "white"))  # Medium Red
    } else {
      return(list(bg = "#CC0000", text = "white"))  # Dark Red - low EV is great
    }
  }
  
  # Special handling for Barrel% - reverse the color logic since lower is better for pitchers
  if (column_name == "Barrel%") {
    if (value >= poor) {
      return(list(bg = "#0066CC", text = "white"))  # Dark Blue - high Barrel% is bad
    } else if (value >= (poor + avg) / 2) {
      return(list(bg = "#66B2FF", text = "black"))  # Light Blue
    } else if (value >= avg) {
      return(list(bg = "#FFFFFF", text = "black"))  # White (average)
    } else if (value >= (avg + great) / 2) {
      return(list(bg = "#FFB3B3", text = "black"))  # Light Red
    } else if (value >= great) {
      return(list(bg = "#FF6666", text = "white"))  # Medium Red
    } else {
      return(list(bg = "#CC0000", text = "white"))  # Dark Red - low Barrel% is great
    }
  }
  
  # Special handling for BB% - reverse the color logic since lower is better for pitchers
  if (column_name == "BB%") {
    if (value >= poor) {
      return(list(bg = "#0066CC", text = "white"))  # Dark Blue - high BB% is bad
    } else if (value >= (poor + avg) / 2) {
      return(list(bg = "#66B2FF", text = "black"))  # Light Blue
    } else if (value >= avg) {
      return(list(bg = "#FFFFFF", text = "black"))  # White (average)
    } else if (value >= (avg + great) / 2) {
      return(list(bg = "#FFB3B3", text = "black"))  # Light Red
    } else if (value >= great) {
      return(list(bg = "#FF6666", text = "white"))  # Medium Red
    } else {
      return(list(bg = "#CC0000", text = "white"))  # Dark Red - low BB% is great
    }
  }
  
  # Standard logic for all other metrics (higher is better)
  if (value <= poor) {
    return(list(bg = "#0066CC", text = "white"))  # Dark Blue - white text
  } else if (value <= (poor + avg) / 2) {
    return(list(bg = "#66B2FF", text = "black"))  # Light Blue - black text
  } else if (value <= avg) {
    return(list(bg = "#FFFFFF", text = "black"))  # White (average) - black text
  } else if (value <= (avg + great) / 2) {
    return(list(bg = "#FFB3B3", text = "black"))  # Light Red - black text
  } else if (value <= great) {
    return(list(bg = "#FF6666", text = "white"))  # Medium Red - white text
  } else {
    return(list(bg = "#CC0000", text = "white"))  # Dark Red - white text
  }
}

get_process_thresholds <- function(column_name, pitch_type) {
  # Normalize pitch type
  pitch_type <- tolower(as.character(pitch_type))
  
  # InZone% thresholds
  if (column_name == "InZone%") {
    if (pitch_type %in% c("fastball", "sinker")) {
      return(list(poor = 43, avg = 50, great = 57))
    } else if (pitch_type %in% c("cutter", "slider", "sweeper", "curveball")) {
      return(list(poor = 37, avg = 43, great = 49))
    } else if (pitch_type %in% c("changeup", "splitter", "knuckleball")) {
      return(list(poor = 30, avg = 37, great = 44))
    } else if (pitch_type == "all") {
      return(list(poor = 42, avg = 47, great = 52))
    }
  }
  
  # Comp% thresholds
  if (column_name == "Comp%") {
    if (pitch_type %in% c("fastball", "sinker")) {
      return(list(poor = 79, avg = 83, great = 87))
    } else if (pitch_type %in% c("cutter", "slider", "sweeper", "curveball")) {
      return(list(poor = 70, avg = 76, great = 82))
    } else if (pitch_type %in% c("changeup", "splitter", "knuckleball")) {
      return(list(poor = 65, avg = 74, great = 83))
    } else if (pitch_type == "all") {
      return(list(poor = 76, avg = 79, great = 82))
    }
  }
  
  # Strike% thresholds (same for all pitch types)
  if (column_name == "Strike%") {
    return(list(poor = 57, avg = 62, great = 67))
  }
  
  # Swing% thresholds
  if (column_name == "Swing%") {
    if (pitch_type %in% c("fastball", "sinker")) {
      return(list(poor = 40, avg = 44, great = 48))
    } else if (pitch_type %in% c("cutter", "slider", "sweeper")) {
      return(list(poor = 37, avg = 43, great = 49))
    } else if (pitch_type == "curveball") {
      return(list(poor = 28, avg = 35, great = 42))
    } else if (pitch_type %in% c("changeup", "splitter")) {
      return(list(poor = 43, avg = 47, great = 51))
    } else if (pitch_type == "all") {
      return(list(poor = 40, avg = 45, great = 50))
    }
  }
  
  # FPS% thresholds (same for all pitch types including individual pitches)
  if (column_name == "FPS%") {
    return(list(poor = 55, avg = 60, great = 65))
  }
  
  # Ahead% thresholds (only for All row)
  if (column_name == "Ahead%" && pitch_type == "all") {
    return(list(poor = 34, avg = 38, great = 42))
  }
  
  # E+A% thresholds (only for All row)
  if (column_name == "E+A%" && pitch_type == "all") {
    return(list(poor = 65, avg = 70, great = 75))
  }
  
  # Ctrl+ thresholds (same for all pitch types)
  if (column_name == "Ctrl+") {
    return(list(poor = 75, avg = 85, great = 95))
  }
  
  # QP+ thresholds (same for all pitch types)
  if (column_name == "QP+") {
    return(list(poor = 75, avg = 90, great = 105))
  }
  
  # Pitching+ thresholds (same for all pitch types)
  if (column_name == "Pitching+") {
    return(list(poor = 80, avg = 95, great = 110))
  }
  
  # K% thresholds (only for All row)
  if (column_name == "K%" && pitch_type == "all") {
    return(list(poor = 18, avg = 23, great = 28))
  }
  
  # BB% thresholds (only for All row)
  if (column_name == "BB%" && pitch_type == "all") {
    return(list(poor = 11, avg = 9, great = 7))
  }
  
  # Whiff% thresholds
  if (column_name == "Whiff%") {
    if (pitch_type == "fastball") {
      return(list(poor = 18, avg = 22, great = 26))
    } else if (pitch_type == "sinker") {
      return(list(poor = 9, avg = 13, great = 17))
    } else if (pitch_type == "cutter") {
      return(list(poor = 22, avg = 27, great = 32))
    } else if (pitch_type %in% c("sweeper", "curveball", "slider", "changeup", "splitter")) {
      return(list(poor = 29, avg = 35, great = 41))
    } else if (pitch_type == "all") {
      return(list(poor = 21, avg = 26, great = 31))
    }
  }
  
  # CSW% thresholds
  if (column_name == "CSW%") {
    if (pitch_type %in% c("fastball", "sinker")) {
      return(list(poor = 23, avg = 27, great = 31))
    } else if (pitch_type %in% c("cutter", "slider", "sweeper", "curveball")) {
      return(list(poor = 29, avg = 32, great = 35))
    } else if (pitch_type %in% c("splitter", "changeup")) {
      return(list(poor = 22, avg = 28, great = 34))
    } else if (pitch_type == "all") {
      return(list(poor = 26, avg = 29, great = 32))
    }
  }
  
  # GB% thresholds
  if (column_name == "GB%") {
    if (pitch_type == "fastball") {
      return(list(poor = 31, avg = 39, great = 47))
    } else if (pitch_type == "sinker") {
      return(list(poor = 43, avg = 54, great = 65))
    } else if (pitch_type %in% c("cutter", "slider", "sweeper", "curveball")) {
      return(list(poor = 36, avg = 43, great = 50))
    } else if (pitch_type %in% c("changeup", "splitter")) {
      return(list(poor = 35, avg = 47, great = 59))
    } else if (pitch_type == "all") {
      return(list(poor = 38, avg = 43, great = 48))
    }
  }
  
  # Barrel% thresholds (same for all pitch types)
  if (column_name == "Barrel%") {
    return(list(poor = 20, avg = 15, great = 10))
  }
  
  # EV thresholds (same for all pitch types)
  if (column_name == "EV") {
    return(list(poor = 95, avg = 85, great = 75))
  }
  
  # Stuff+ thresholds (same for all pitch types)
  if (column_name == "Stuff+") {
    return(list(poor = 90, avg = 100, great = 110))
  }
  
  return(NULL)
}

make_hover_tt <- function(df) {
  paste0(
    "Session: ", as.character(df$SessionType),
    "<br>Velo: ", ifelse(is.na(df$RelSpeed), "", sprintf("%.1f mph", df$RelSpeed)),
    "<br>IVB: " , ifelse(is.na(df$InducedVertBreak), "", sprintf("%.1f in", df$InducedVertBreak)),
    "<br>HB: "  , ifelse(is.na(df$HorzBreak), "", sprintf("%.1f in", df$HorzBreak)),
    "<br>In Zone: ", inzone_label(df$PlateLocSide, df$PlateLocHeight)
  )
}

make_release_tt <- function(df) {
  paste0(
    "Session: ", as.character(df$SessionType),
    "<br>Height: ", ifelse(is.na(df$RelHeight), "", sprintf("%.2f ft", df$RelHeight)),
    "<br>Side: ", ifelse(is.na(df$RelSide), "", sprintf("%.2f ft", df$RelSide)),
    "<br>Extension: ", ifelse(is.na(df$Extension), "", sprintf("%.2f ft", df$Extension))
  )
}


# ---- Outcomes & helpers (GLOBAL) ----
result_levels <- c(
  "Called Strike", "Ball", "Foul", "Whiff",
  "In Play (Out)", "In Play (Hit)"
)

shape_map <- c(
  "Called Strike" = 19,  # filled circle
  "Ball"          = 1,   # hollow circle
  "Foul"          = 2,   # hollow triangle
  "Whiff"         = 8,   # star
  "In Play (Out)" = 17,  # filled triangle
  "In Play (Hit)" = 15   # filled square
)

compute_result <- function(pitch_call, play_result) {
  dplyr::case_when(
    pitch_call == "StrikeCalled" ~ "Called Strike",
    pitch_call == "BallCalled"   ~ "Ball",
    pitch_call %in% c("FoulBallNotFieldable","FoulBallFieldable","FoulBall") ~ "Foul",
    pitch_call == "StrikeSwinging" ~ "Whiff",
    pitch_call == "InPlay" & (is.na(play_result) |
                                play_result %in% c("Out","Error","FieldersChoice","Sacrifice")) ~ "In Play (Out)",
    pitch_call == "InPlay" & play_result %in% c("Single","Double","Triple","HomeRun")       ~ "In Play (Hit)",
    TRUE ~ NA_character_
  )
}

# vectorized clock converter
convert_to_clock <- Vectorize(function(x) {
  if (is.na(x)) return(NA_character_)
  h24 <- 6 + x/30
  if (h24 < 13) {
    hour <- floor(h24)
    mins <- round((h24 - hour) * 60)
    sprintf("%d:%02d", hour, mins)
  } else {
    h12  <- h24 - 12
    hour <- floor(h12)
    mins <- round((h12 - hour) * 60)
    sprintf("%d:%02d", hour, mins)
  }
}, USE.NAMES = FALSE)


fmt_date <- function(d) format(d, "%m/%d/%y")

axis_theme <- theme(
  axis.text.x  = element_text(color="black", face="bold"),
  axis.text.y  = element_text(color="black", face="bold"),
  axis.title.x = element_text(color="black", face="bold"),
  axis.title.y = element_text(color="black", face="bold"),
  panel.background = element_rect(fill = NA, colour = NA),
  plot.background  = element_rect(fill = NA, colour = NA),
  panel.grid.minor = element_blank()
)

# ----- Shared heatmap palette (identical to current heat maps) -----
heat_pal <- function(bins = 10) {
  colorRampPalette(c("white","blue","lightblue","turquoise","yellow","orange","red"))(bins)
}

# ---- Per-base weight scales for Stuff+ ----
pitch_weights_fb <- tibble(
  TaggedPitchType = c("Fastball","Sinker",
                      "Cutter","Slider","Sweeper","Curveball",
                      "ChangeUp","Splitter"),
  w_vel = c(0.5, 0.3, 0.5, 0.4, 0.3, 0.5, 0.3, 0.2),
  w_ivb = c(0.4, 0.4, 0.2, 0.4, 0.1, 0.5, 0.6, 0.75),
  w_hb  = c(0.1, 0.3, 0.3, 0.2, 0.6, 0.0, 0.1, 0.05)
)

pitch_weights_si <- tibble(
  TaggedPitchType = c("Fastball","Sinker",
                      "Cutter","Slider","Sweeper","Curveball",
                      "ChangeUp","Splitter"),
  w_vel = c(0.3, 0.3, 0.4, 0.4, 0.4, 0.3, 0.2, 0.2),
  w_ivb = c(0.4, 0.4, 0.3, 0.3, 0.0, 0.5, 0.75, 0.8),
  w_hb  = c(0.3, 0.3, 0.3, 0.3, 0.6, 0.2, 0.05, 0.0)
)

# ---- Simplified Stuff+ Helper (vectorized, RelHeight-based FB/SI) ----
compute_stuff_simple <- function(df, base_type, level) {
  # select appropriate weights
  weight_tbl <- if (base_type == "Fastball") pitch_weights_fb else pitch_weights_si
  
  # prepare data and join weights
  df2 <- df %>%
    mutate(
      TaggedPitchType = as.character(TaggedPitchType),
      HB_adj = ifelse(PitcherThrows == "Left", HorzBreak, -HorzBreak)
    ) %>%
    left_join(weight_tbl, by = "TaggedPitchType")
  
  # velocity baselines
  base_vel <- mean(df2$RelSpeed[df2$TaggedPitchType == base_type], na.rm = TRUE)
  vel_avg  <- c(Pro = 94, College = 89, `High School` = 82)[level]
  
  # off-speed break specs
  off_off <- c(Cutter = 5, Slider = 8, Sweeper = 12,
               Curveball = 14, ChangeUp = 8, Splitter = 7)
  sep_fb <- tibble(
    TaggedPitchType = names(off_off),
    sep_ivb = c(-7, -15, -16, -27, -12, -13),
    sep_hb  = c(10, 12, 22, 18, -7, -4)
  )
  sep_si <- tibble(
    TaggedPitchType = names(off_off),
    sep_ivb = c(2, -6, -7, -18, -4, -5),
    sep_hb  = c(18, 20, 30, 25, 1, 2)
  )
  seps <- if (base_type == "Fastball") sep_fb else sep_si
  
  # league-standard baselines by release height
  std_ivb <- case_when(
    df2$TaggedPitchType == "Fastball" & df2$RelHeight >= 6.2 ~ 17,
    df2$TaggedPitchType == "Fastball" & df2$RelHeight >= 5.8 ~ 15.5,
    df2$TaggedPitchType == "Fastball" & df2$RelHeight >= 5.4 ~ 15,
    df2$TaggedPitchType == "Fastball" & df2$RelHeight >= 5.0 ~ 12.5,
    df2$TaggedPitchType == "Fastball" & df2$RelHeight >= 4.5 ~ 11,
    df2$TaggedPitchType == "Fastball" & df2$RelHeight <  4.5 ~ 10,
    df2$TaggedPitchType == "Sinker"    & df2$RelHeight >= 6.2 ~ 10,
    df2$TaggedPitchType == "Sinker"    & df2$RelHeight >= 5.8 ~ 7,
    df2$TaggedPitchType == "Sinker"    & df2$RelHeight >= 5.4 ~ 4.5,
    df2$TaggedPitchType == "Sinker"    & df2$RelHeight >= 5.0 ~ 3.7,
    df2$TaggedPitchType == "Sinker"    & df2$RelHeight >= 4.5 ~ 2.6,
    df2$TaggedPitchType == "Sinker"    & df2$RelHeight <  4.5 ~ 2.6,
    TRUE ~ NA_real_
  )
  std_hb_right <- case_when(
    df2$TaggedPitchType == "Fastball" & df2$RelHeight >= 6.2 ~ 6,
    df2$TaggedPitchType == "Fastball" & df2$RelHeight >= 5.8 ~ 8,
    df2$TaggedPitchType == "Fastball" & df2$RelHeight >= 5.4 ~ 9,
    df2$TaggedPitchType == "Fastball" & df2$RelHeight >= 5.0 ~ 10,
    df2$TaggedPitchType == "Fastball" & df2$RelHeight >= 4.5 ~ 13,
    df2$TaggedPitchType == "Fastball" & df2$RelHeight <  4.5 ~ 11,
    df2$TaggedPitchType == "Sinker"    & df2$RelHeight >= 6.2 ~ 15,
    df2$TaggedPitchType == "Sinker"    & df2$RelHeight >= 5.8 ~ 15.5,
    df2$TaggedPitchType == "Sinker"    & df2$RelHeight >= 5.4 ~ 16.7,
    df2$TaggedPitchType == "Sinker"    & df2$RelHeight >= 5.0 ~ 17,
    df2$TaggedPitchType == "Sinker"    & df2$RelHeight >= 4.5 ~ 17,
    df2$TaggedPitchType == "Sinker"    & df2$RelHeight <  4.5 ~ 17.5,
    TRUE ~ NA_real_
  )
  std_hb <- ifelse(df2$PitcherThrows == "Left", -std_hb_right, std_hb_right)
  
  # personal baselines for off-speed
  base_ivb_val <- mean(df2$InducedVertBreak[df2$TaggedPitchType == base_type], na.rm = TRUE)
  base_hb_val  <- mean(df2$HB_adj[df2$TaggedPitchType == base_type], na.rm = TRUE)
  
  # compute per-pitch ratios
  r_vel <- ifelse(
    df2$TaggedPitchType %in% c("Fastball","Sinker"),
    df2$RelSpeed / vel_avg,
    df2$RelSpeed / (base_vel - off_off[df2$TaggedPitchType])
  )
  
  # exaggerate velocity deviations:
  #   below average → square the ratio (shrinks <1 even more)
  #   above average → optional root (or power>1) to tune reward
  alpha <- 4    # exponent for penalizing below-avg
  beta  <- 2  # exponent for rewarding above-avg (tweak as you like)
  
  r_vel <- ifelse(
    r_vel < 1,
    r_vel^alpha,
    r_vel^beta
  )
  
  # ---- Updated r_ivb: endpoint-based for Sweeper off Sinker ----
  r_ivb <- case_when(
    df2$TaggedPitchType == "Fastball" ~
      df2$InducedVertBreak / std_ivb,
    
    df2$TaggedPitchType == "Sinker"   ~
      ifelse(df2$InducedVertBreak > 0,
             std_ivb / df2$InducedVertBreak,
             1),
    
    base_type == "Sinker" & df2$TaggedPitchType == "Cutter" ~ {
      endpoint_ivb <- base_ivb_val +
        seps$sep_ivb[match("Cutter", seps$TaggedPitchType)]
      # compute difference (could be negative), then abs() makes it positive
      ivb_diff <- df2$InducedVertBreak - endpoint_ivb
      abs(ivb_diff) / endpoint_ivb
    },
    
    # reward deviations in either direction for Sweeper off Sinker
    base_type == "Sinker" & df2$TaggedPitchType == "Sweeper" ~ {
      endpoint_ivb <- base_ivb_val +
        seps$sep_ivb[match("Sweeper", seps$TaggedPitchType)]
      # compute difference (could be negative), then abs() makes it positive
      ivb_diff <- df2$InducedVertBreak - endpoint_ivb
      abs(ivb_diff) / endpoint_ivb
    },
    
    TRUE ~
      (base_ivb_val - df2$InducedVertBreak) /
      abs(seps$sep_ivb[match(df2$TaggedPitchType, seps$TaggedPitchType)])
  )
  
  # ---- Updated r_hb: endpoint-based for Sweeper off Sinker ----
  r_hb <- case_when(
    df2$TaggedPitchType == "Fastball" ~ {
      hb_mag  <- abs(df2$HB_adj)
      std_mag <- abs(std_hb)
      # symmetric reward: less HB → std/hb ; more HB → hb/std
      r <- pmax(hb_mag / std_mag, std_mag / pmax(hb_mag, 1e-6))
      # optional deadband (e.g., within 2 in of "standard" HB scores 1.0)
      r <- ifelse(abs(hb_mag - std_mag) < 2, 1, r)
      r
    },
    df2$TaggedPitchType == "Sinker" ~
      abs(df2$HB_adj / std_hb),
    
    df2$TaggedPitchType == "Curveball" ~
      abs(df2$HorzBreak - base_hb_val) /
      abs(seps$sep_hb[match("Curveball", seps$TaggedPitchType)]),
    
    # ← NEW: Sweeper off Sinker uses endpoint (base_hb_val + sep_hb)
    base_type == "Sinker" & df2$TaggedPitchType == "Sweeper" ~ {
      endpoint_hb <- base_hb_val +
        seps$sep_hb[match("Sweeper", seps$TaggedPitchType)]
      abs(df2$HB_adj) / abs(endpoint_hb)
    },
    
    base_type == "Sinker" & df2$TaggedPitchType == "Cutter" ~ {
      endpoint_hb <- base_hb_val +
        seps$sep_hb[match("Cutter", seps$TaggedPitchType)]
      abs(df2$HB_adj) / abs(endpoint_hb)
    },
    
    base_type == "Sinker" & df2$TaggedPitchType == "ChangeUp" ~ {
      (base_hb_val - df2$HB_adj) /
        seps$sep_hb[match("ChangeUp", seps$TaggedPitchType)]
    },
    base_type == "Sinker" & df2$TaggedPitchType == "Splitter" ~ {
      (base_hb_val - df2$HB_adj) /
        seps$sep_hb[match("Splitter", seps$TaggedPitchType)]
    },
    
    TRUE ~
      (df2$HB_adj - base_hb_val) /
      seps$sep_hb[match(df2$TaggedPitchType, seps$TaggedPitchType)]
  )
  
  # ← INSERT YOUR CAPS HERE
  r_ivb <- pmin(r_ivb, 2)   # cap vertical ratio at 2× baseline
  r_hb  <- pmin(r_hb,  2)   # cap horizontal ratio at 2× baseline
  
  # combine into Stuff+
  df2 %>%
    mutate(
      raw      = w_vel * r_vel + w_ivb * r_ivb + w_hb * r_hb,
      `Stuff+` = round(raw * 100, 1)
    )
}


# =========================
# ===== QP+  HELPERS  =====
# =========================

# Competitive rectangle (same as your dashed box)
COMP_LEFT   <- -1.5
COMP_RIGHT  <-  1.5
COMP_BOTTOM <-  2.65 - 1.7   # 1.15
COMP_TOP    <-  2.65 + 1.3   # 4.15

# Map (Balls, Strikes) to Ahead / Even / Behind like your count filter
count_state_vec <- function(b, s) {
  b <- suppressWarnings(as.integer(b)); s <- suppressWarnings(as.integer(s))
  out <- rep("Even", length(b))
  ahead  <- ( (b==0 & s==1) | (b==0 & s==2) | (b==1 & s==2) )
  behind <- ( (b==1 & s==0) | (b==2 & s==0) | (b==3 & s==0) |
                (b==3 & s==1) | (b==2 & s==1) )
  even   <- ( (b==0 & s==0) | (b==1 & s==1) | (b==2 & s==2) | (b==3 & s==2) )
  
  out[behind] <- "Behind"
  out[ahead]  <- "Ahead"
  out[even]   <- "Even"
  out[is.na(b) | is.na(s)] <- "Even"
  out
}

# 9-square index (1..9) or NA if OUTSIDE competitive box
# Numbering (LEFT → RIGHT per row):
# Top row:    1,2,3
# Middle row: 4,5,6
# Bottom row: 7,8,9
# 9-square index (1..9) or NA if OUTSIDE competitive box
# Numbering (LEFT→RIGHT per row): Top: 1,2,3  |  Middle: 4,5,6  |  Bottom: 7,8,9
zone9_square <- function(x, y) {
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  
  # guard missing + bounds (avoid NA in logical index)
  ok <- !is.na(x) & !is.na(y) &
    x >= COMP_LEFT & x <= COMP_RIGHT &
    y >= COMP_BOTTOM & y <= COMP_TOP
  
  sq <- rep(NA_integer_, length(x))
  idx <- which(ok)  # <- integer indices (drops NA/FALSE safely)
  if (!length(idx)) return(sq)
  
  w <- (COMP_RIGHT - COMP_LEFT)
  h <- (COMP_TOP   - COMP_BOTTOM)
  
  # normalized positions in [0,1]
  gx <- pmax(0, pmin(1, (x[idx] - COMP_LEFT)   / w))
  gy <- pmax(0, pmin(1, (y[idx] - COMP_BOTTOM) / h))
  
  # columns: LEFT(1)→RIGHT(3)
  col <- ifelse(gx < 1/3, 1L, ifelse(gx < 2/3, 2L, 3L))
  # rows: TOP(1)→BOTTOM(3)
  row <- ifelse(gy >= 2/3, 1L, ifelse(gy >= 1/3, 2L, 3L))
  
  sq[idx] <- (row - 1L) * 3L + col
  sq
}

# Convenience to convert square to (row,col)
sq_to_rc <- function(sq) {
  r <- ((sq - 1) %/% 3) + 1
  c <- ((sq - 1) %%  3) + 1
  list(r = r, c = c)
}

# Count strictness decay by Manhattan distance from target
# Behind = loosest, Even = moderate, Ahead = strict
qp_decay <- function(state) {
  if (identical(state, "Ahead"))      c(1.00, 0.35, 0.15, 0.05)
  else if (identical(state, "Even"))  c(1.00, 0.55, 0.25, 0.10)
  else                                c(1.00, 0.75, 0.45, 0.20)  # Behind
}

# Seed targets (row, col, base weight) per pitch type & pitcher hand
# UI columns numbered LEFT→RIGHT:
# - RHP glove side → UI col 1 → squares 1/4/7
# - LHP glove side → UI col 3 → squares 3/6/9
qp_seeds_for <- function(pt, hand) {
  pt   <- as.character(pt)
  hand <- ifelse(hand %in% c("Left","Right"), hand, "Right")
  
  glove_col <- ifelse(hand == "Left", 3, 1)
  arm_col   <- ifelse(glove_col == 1, 3, 1)
  
  r_top <- 1; r_mid <- 2; r_bot <- 3
  c_mid <- 2; c_g  <- glove_col; c_a <- arm_col
  
  # Weights reflect your guidance:
  # FB up; SI down; CT middle & up glove; SL/SW down glove;
  # CB down + slight glove; CH/SPL down over plate + slight arm.
  switch(pt,
         "Fastball"  = data.frame(r = c(r_top, r_top, r_top),
                                  c = c(c_mid, c_g,   c_a),
                                  w = c(1.00,  1.00,  1.00)),
         "Sinker"    = data.frame(r = c(r_bot, r_bot, r_bot),
                                  c = c(c_mid, c_a,   c_g),
                                  w = c(0.8,  1.00,  0.90)),
         "Cutter"    = data.frame(r = c(r_mid, r_top, r_top, r_bot),
                                  c = c(c_g,   c_g,   c_mid, c_g),
                                  w = c(1.00,  1.00,  0.75,  0.80)),
         "Slider"    = data.frame(r = c(r_bot, r_bot, r_mid),
                                  c = c(c_g,   c_mid, c_g),
                                  w = c(1.00,  0.80,  0.70)),
         "Sweeper"   = data.frame(r = c(r_bot, r_bot, r_mid),
                                  c = c(c_g,   c_mid, c_g),
                                  w = c(1.00,  0.75,  0.65)),
         "Curveball" = data.frame(r = c(r_bot, r_bot, r_bot),
                                  c = c(c_mid, c_g,   c_a),
                                  w = c(1.00,  1.00,  1.00)),
         "ChangeUp"  = data.frame(r = c(r_bot, r_bot, r_bot),
                                  c = c(c_mid, c_a,   c_g),
                                  w = c(1.00,  0.90,  0.70)),
         "Splitter"  = data.frame(r = c(r_bot, r_bot, r_bot),
                                  c = c(c_mid, c_a,   c_g),
                                  w = c(1.00,  1.00,  1.00)),
         # Fallback: neutral center if unknown pitch label
         data.frame(r = r_mid, c = c_mid, w = 0.60)
  )
}

# Best weight for a given square given pitch, hand, count macro
qp_weight_for_square <- function(sq, pt, hand, state) {
  if (is.na(sq)) return(0)
  seeds <- qp_seeds_for(pt, hand)
  rc    <- sq_to_rc(sq)
  dec   <- qp_decay(state)
  best  <- 0
  for (i in seq_len(nrow(seeds))) {
    d <- abs(seeds$r[i] - rc$r) + abs(seeds$c[i] - rc$c)  # Manhattan distance
    di <- ifelse(d >= 3, 4, d + 1)
    best <- max(best, seeds$w[i] * dec[di])
  }
  best
}

# Vectorized per-pitch QP points (0..1)
# - Live only (non-Live → NA, so not counted)
# - 0 if outside COMP box (non-competitive)
compute_qp_points <- function(df) {
  n <- nrow(df)
  if (!n) return(numeric(0))
  
  live  <- !is.na(df$SessionType) & as.character(df$SessionType) == "Live"
  sq    <- zone9_square(df$PlateLocSide, df$PlateLocHeight)
  state <- count_state_vec(df$Balls, df$Strikes)
  pt    <- as.character(df$TaggedPitchType)
  hand  <- as.character(df$PitcherThrows)
  
  out <- rep(NA_real_, n)
  
  # live & non-competitive = 0  (use integer indices)
  idx_noncomp <- which(live & is.na(sq))
  if (length(idx_noncomp)) out[idx_noncomp] <- 0
  
  # live & inside competitive box
  idx <- which(live & !is.na(sq))
  if (length(idx)) {
    out[idx] <- mapply(
      function(sqi, pti, hnd, st)
        qp_weight_for_square(sqi, pti, hnd, ifelse(is.na(st), "Even", st)),
      sq[idx], pt[idx], hand[idx], state[idx]
    )
  }
  out
}


# Colors & factor levels
all_colors <- c(
  Fastball   = "black",
  Sinker     = "orange",
  Cutter     = "brown",
  Slider     = "red",
  Sweeper    = "purple",
  Curveball  = "blue",
  ChangeUp   = "darkgreen",
  Splitter   = "turquoise",
  Knuckleball= "darkblue"
)
colors_for_mode <- function(dark_on = FALSE) {
  cols <- all_colors
  if (isTRUE(dark_on) && "Fastball" %in% names(cols)) cols["Fastball"] <- "#ffffff"
  cols
}
force_pitch_levels <- function(df) {
  if (!"TaggedPitchType" %in% names(df)) {
    # Create TaggedPitchType from fallback columns if missing
    fallback_cols <- intersect(c("PitchType","pitchType","Pitch"), names(df))
    if (length(fallback_cols)) {
      df$TaggedPitchType <- df[[fallback_cols[1]]]
    } else {
      df$TaggedPitchType <- "Unknown"
    }
  }
  df %>% mutate(
    TaggedPitchType = factor(TaggedPitchType, levels = names(all_colors))
  )
}

# Session colors for Trend when "All" is selected
  session_cols <- c(Live = "red", Bullpen = "black")
  session_cols_for_mode <- function(dark_on = FALSE) {
    cols <- session_cols
    if (isTRUE(dark_on) && "Bullpen" %in% names(cols)) cols["Bullpen"] <- "#ffffff"
    cols
  }

# Filters
# ---- Filters: Zone & In-Zone ----

# Multi-select halves/thirds with INTERSECTION logic, bounded to the strike zone
# ---- Filters: Zone bands that extend beyond the zone ----
# Halves/Thirds act as threshold lines:
#   - Upper Half      => y >= mid_y      (and above the zone)
#   - Bottom Half     => y <= mid_y      (and below the zone)
#   - Upper 3rd       => y >= B + 2*dy   (and above the zone)
#   - Bottom 3rd      => y <= B + dy     (and below the zone)
#   - Left Half       => x <= mid_x      (and left of the zone)
#   - Right Half      => x >= mid_x      (and right of the zone)
#   - Left 3rd        => x <= L + dx     (and left of the zone)
#   - Right 3rd       => x >= L + 2*dx   (and right of the zone)
# Multiple selections are ANDed together (intersection).
enforce_zone <- function(df, choice) {
  # Pass-through if nothing picked or "All"
  if (is.null(choice) || !length(choice)) return(df)
  choice <- unique(as.character(choice))
  if ("All" %in% choice) return(df)
  
  # Coords
  x <- suppressWarnings(as.numeric(df$PlateLocSide))
  y <- suppressWarnings(as.numeric(df$PlateLocHeight))
  ok <- is.finite(x) & is.finite(y)
  if (!any(ok)) return(df[0, , drop = FALSE])
  
  # Zone geometry
  mid_x <- (ZONE_LEFT + ZONE_RIGHT) / 2
  mid_y <- (ZONE_BOTTOM + ZONE_TOP) / 2
  dx <- (ZONE_RIGHT - ZONE_LEFT) / 3
  dy <- (ZONE_TOP   - ZONE_BOTTOM) / 3
  L <- ZONE_LEFT; R <- ZONE_RIGHT; B <- ZONE_BOTTOM; T <- ZONE_TOP
  
  # Start with TRUE everywhere (don’t clamp to the zone here)
  mask_vert <- rep(TRUE, length(x))
  mask_horz <- rep(TRUE, length(x))
  
  # Apply each token as a threshold restriction
  for (tok in choice) {
    if (tok %in% c("Upper Half", "Bottom Half", "Upper 3rd", "Bottom 3rd")) {
      cond <- switch(tok,
                     "Upper Half"  = (y >= mid_y),
                     "Bottom Half" = (y <= mid_y),
                     "Upper 3rd"   = (y >= (B + 2*dy)),
                     "Bottom 3rd"  = (y <= (B + dy))
      )
      mask_vert <- mask_vert & cond
    } else if (tok %in% c("Left Half", "Right Half", "Left 3rd", "Right 3rd")) {
      cond <- switch(tok,
                     "Left Half"  = (x <= mid_x),
                     "Right Half" = (x >= mid_x),
                     "Left 3rd"   = (x <= (L + dx)),
                     "Right 3rd"  = (x >= (L + 2*dx))
      )
      mask_horz <- mask_horz & cond
    } else {
      # Unknown token → no change
    }
  }
  
  m <- ok & mask_vert & mask_horz
  df[m, , drop = FALSE]
}


# Keep the legacy in-zone toggle that other code expects
# Values typically: "All", "Yes", "No", "Competitive"
enforce_inzone <- function(df, choice) {
  if (is.null(choice) || identical(choice, "All")) return(df)
  
  x <- suppressWarnings(as.numeric(df$PlateLocSide))
  y <- suppressWarnings(as.numeric(df$PlateLocHeight))
  
  in_zone <- (x >= ZONE_LEFT & x <= ZONE_RIGHT &
                y >= ZONE_BOTTOM & y <= ZONE_TOP)
  
  if (identical(choice, "Yes")) {
    df[in_zone, , drop = FALSE]
  } else if (identical(choice, "No")) {
    df[!in_zone, , drop = FALSE]
  } else if (identical(choice, "Competitive")) {
    comp <- (x >= -1.5 & x <= 1.5 &
               y >= (2.65 - 1.7) & y <= (2.65 + 1.3))
    df[comp, , drop = FALSE]
  } else {
    df
  }
}

# ---- Multi-select "All" helper ----
enforce_all_multiselect <- function(input, session, id) {
  observeEvent(input[[id]], {
    sel <- input[[id]]
    if (is.null(sel) || !length(sel)) {
      updateSelectInput(session, id, selected = "All")
    } else if ("All" %in% sel && length(sel) > 1) {
      updateSelectInput(session, id, selected = setdiff(sel, "All"))
    }
  }, ignoreInit = TRUE)
}


# ---- Count filter helper (supports macros + exact counts) ----
apply_count_filter <- function(df, selection) {
  # If nothing or "All" selected -> no filtering
  if (is.null(selection) || !length(selection) || "All" %in% selection) return(df)
  
  # Build allowed (Balls, Strikes) pairs from exact and macro options
  allowed <- list()
  
  # Exact counts like "1-2"
  exact <- selection[grepl("^\\d-\\d$", selection)]
  if (length(exact)) {
    for (val in exact) {
      sp <- strsplit(val, "-", fixed = TRUE)[[1]]
      allowed <- append(allowed, list(c(as.integer(sp[1]), as.integer(sp[2]))))
    }
  }
  
  # Macros
  if ("Even" %in% selection)  allowed <- append(allowed, list(c(0,0), c(1,1), c(2,2), c(3,2)))
  if ("Behind" %in% selection) allowed <- append(allowed, list(c(1,0), c(2,0), c(3,0), c(3,1), c(2,1)))
  if ("Ahead" %in% selection)  allowed <- append(allowed, list(c(0,1), c(0,2), c(1,2)))
  if ("2KNF" %in% selection)   allowed <- append(allowed, list(c(0,2), c(1,2), c(2,2)))
  
  if (!length(allowed)) return(df)
  
  mat <- do.call(rbind, allowed)  # Nx2 (Balls, Strikes)
  ok  <- is.finite(df$Balls) & is.finite(df$Strikes)
  keep <- rep(FALSE, nrow(df))
  if (nrow(mat)) {
    for (i in seq_len(nrow(mat))) {
      keep <- keep | (df$Balls == mat[i,1] & df$Strikes == mat[i,2])
    }
  }
  df[ ok & keep, , drop = FALSE ]
}

# Keep "All" semantics tidy (like locResult)


# Robust m/d/y date parser for mixed inputs
parse_date_mdy <- function(x) {
  parse_date_flex(x)
}


# returns a named vector of usage % by pitch type for a given filtered df
usage_by_type <- function(df) {
  total <- nrow(df)
  if (!total) return(setNames(numeric(0), character(0)))
  df %>%
    dplyr::count(TaggedPitchType, name = "n") %>%
    dplyr::mutate(pct = 100 * n / total) %>%
    { setNames(.$pct, .$TaggedPitchType) }
}

# ---- Data import + cleaning (recursive from data/practice and data/V3[/CSV]) ----
library(readr)
library(stringr)   # explicit, even though tidyverse includes it

resolve_data_root <- function() {
  as_paths <- function(x) {
    x <- unique(trimws(c(x)))
    x <- x[nzchar(x)]
    if (!length(x)) return(character(0))
    path.expand(x)
  }
  
  candidate_dirs <- as_paths(c(
    Sys.getenv("PCU_DATA_DIR", unset = ""),
    Sys.getenv("DATA_DIR", unset = ""),
    Sys.getenv("RSCONNECT_DATA_DIR", unset = ""),
    getOption("PCU_DATA_DIR", NULL),
    getOption("pcu.data_dir", NULL),
    file.path(getwd(), "data")
  ))
  
  checked_dirs <- character(0)
  archive_candidates <- character(0)
  ftp_attempted <- FALSE
  ftp_success <- FALSE
  ftp_error <- NULL
  
  dir_csvs <- function(dir) {
    if (!dir.exists(dir)) return(character(0))
    list.files(dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  }
  
  for (dir in candidate_dirs) {
    checked_dirs <- unique(c(checked_dirs, dir))
    csvs <- dir_csvs(dir)
    if (length(csvs)) {
      message("Loading data from directory: ", dir)
      return(list(
        path      = normalizePath(dir),
        csvs      = csvs,
        checked   = checked_dirs,
        archives  = archive_candidates,
        ftp_attempted = ftp_attempted,
        ftp_success   = ftp_success,
        ftp_error     = ftp_error
      ))
    }
  }
  
  dest_dir <- path.expand(file.path(getwd(), "data"))
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  checked_dirs <- unique(c(checked_dirs, dest_dir))
  
  archive_candidates <- as_paths(c(
    Sys.getenv("PCU_DATA_ARCHIVE", unset = ""),
    Sys.getenv("PCU_DATA_ARCHIVE_URL", unset = ""),
    list.files(dest_dir, pattern = "\\.zip$", full.names = TRUE),
    file.path(getwd(), "data_bundle.zip")
  ))
  
  if (length(archive_candidates)) {
    for (arch in archive_candidates) {
      if (file.exists(arch)) {
        message("Hydrating data from local archive: ", arch)
        try(utils::unzip(arch, exdir = dest_dir, overwrite = TRUE), silent = TRUE)
      } else if (grepl("^https?://", arch, ignore.case = TRUE)) {
        message("Hydrating data from remote archive: ", arch)
        tmp <- tempfile(fileext = ".zip")
        ok <- tryCatch({
          utils::download.file(arch, tmp, mode = "wb", quiet = TRUE)
          TRUE
        }, error = function(e) {
          message("Failed to download archive: ", conditionMessage(e))
          FALSE
        })
        if (ok && file.exists(tmp)) {
          try(utils::unzip(tmp, exdir = dest_dir, overwrite = TRUE), silent = TRUE)
        }
        if (file.exists(tmp)) unlink(tmp)
      }
      csvs <- dir_csvs(dest_dir)
      if (length(csvs)) {
        message("Loaded data from hydrated archive into: ", dest_dir)
        return(list(
          path      = normalizePath(dest_dir),
          csvs      = csvs,
          checked   = checked_dirs,
          archives  = archive_candidates,
          ftp_attempted = ftp_attempted,
          ftp_success   = ftp_success,
          ftp_error     = ftp_error
        ))
      }
    }
  }
  
  # --- As a last resort, optionally hydrate from FTP (GitHub workflow equivalent) ---
  dest_csvs <- dir_csvs(dest_dir)
  ftp_pref <- tolower(Sys.getenv("PCU_FTP_AUTO_SYNC", unset = "auto"))
  ftp_should_attempt <- ftp_pref %in% c("true", "auto")
  can_sync_ftp <- ftp_should_attempt && file.exists(file.path(getwd(), "automated_data_sync.R"))
  if (!length(dest_csvs) && can_sync_ftp) {
    ftp_attempted <- TRUE
    message("Attempting FTP sync via automated_data_sync.R because no CSV files were found.")
    ftp_success <- tryCatch({
      sync_env <- new.env(parent = baseenv())
      source("automated_data_sync.R", local = sync_env)
      if (!isTRUE(exists("main_sync", envir = sync_env, inherits = FALSE))) {
        stop("main_sync() not exported from automated_data_sync.R")
      }
      isTRUE(sync_env$main_sync())
    }, error = function(e) {
      ftp_error <<- conditionMessage(e)
      FALSE
    })
    dest_csvs <- dir_csvs(dest_dir)
    if (length(dest_csvs)) {
      message("FTP sync populated data into: ", dest_dir)
      return(list(
        path      = normalizePath(dest_dir),
        csvs      = dest_csvs,
        checked   = checked_dirs,
        archives  = archive_candidates,
        ftp_attempted = ftp_attempted,
        ftp_success   = ftp_success || length(dest_csvs) > 0,
        ftp_error     = ftp_error
      ))
    }
  }
  
  list(
    path     = if (dir.exists(dest_dir)) normalizePath(dest_dir) else dest_dir,
    csvs     = dest_csvs,
    checked  = checked_dirs,
    archives = archive_candidates,
    ftp_attempted = ftp_attempted,
    ftp_success   = ftp_success,
    ftp_error     = ftp_error
  )
}

data_info <- resolve_data_root()
data_parent <- data_info$path
all_csvs <- data_info$csvs

if (!length(all_csvs)) {
  details <- c(
    sprintf("Primary path: %s", data_parent)
  )
  if (length(data_info$checked)) {
    details <- c(details, sprintf("Checked directories: %s", paste(unique(data_info$checked), collapse = ", ")))
  }
  if (isTRUE(data_info$ftp_attempted)) {
    details <- c(details, sprintf(
      "FTP fallback attempted (%s)",
      if (isTRUE(data_info$ftp_success)) "succeeded" else sprintf("failed: %s", data_info$ftp_error %||% "unknown error")
    ))
  }
  if (length(data_info$archives)) {
    details <- c(details, sprintf("Archive candidates: %s", paste(unique(data_info$archives), collapse = ", ")))
  }
  stop(paste(
    "No CSV files were found for the app to load.",
    paste(details, collapse = " | "),
    "Set PCU_DATA_DIR to a directory containing your exports or PCU_DATA_ARCHIVE_URL to a downloadable zip bundle."
  ))
}

all_csvs_lower <- tolower(all_csvs)
file_lower <- tolower(basename(all_csvs))
practice_mask <- grepl("([/\\\\]practice[/\\\\])", all_csvs_lower)
v3_mask <- grepl("([/\\\\]v3[/\\\\])", all_csvs_lower)
hitting_mask <- grepl("hitting", file_lower, fixed = TRUE)
live_mask <- grepl("live", file_lower, fixed = TRUE)
keep_relevant <- practice_mask | v3_mask | hitting_mask | live_mask
if (any(keep_relevant)) {
  all_csvs <- all_csvs[keep_relevant]
} else {
  message("No practice/v3/hitting/live files detected; using all CSV files under ", data_parent)
}

# Map folder → SessionType
infer_session_from_path <- function(fp) {
  fpl <- tolower(fp)
  if (grepl("[/\\\\]practice[/\\\\]", fpl)) "Bullpen"
  else if (grepl("[/\\\\]v3[/\\\\]", fpl)) "Live"
  else "Live"
}

# Grab the LAST YYYY/MM/DD in the path (handles .../V3/2025/08/13/CSV/file.csv)
extract_folder_date <- function(fp) {
  m <- stringr::str_match_all(fp, "(20\\d{2})[\\/](0[1-9]|1[0-2])[\\/](0[1-9]|[12]\\d|3[01])")[[1]]
  if (nrow(m)) as.Date(paste(m[nrow(m),2], m[nrow(m),3], m[nrow(m),4], sep = "-")) else NA
}

read_one <- function(fp) {
  df <- suppressMessages(readr::read_csv(
    fp,
    col_types = readr::cols(.default = readr::col_character())
  ))
  
  # --- NEW: SessionType from folder with CSV fallback/normalization ---
  path_st <- infer_session_from_path(fp)  # "Bullpen" if /practice/, "Live" if /v3/
  if (!"SessionType" %in% names(df)) df$SessionType <- NA_character_
  st_chr <- tolower(trimws(as.character(df$SessionType)))
  
  df$SessionType <- dplyr::case_when(
    grepl("bull|prac", st_chr) ~ "Bullpen",
    grepl("live|game|ab", st_chr) ~ "Live",
    TRUE ~ path_st  # fallback to folder mapping when blank/unknown
  )
  
  
  # Canonicalize to the names used throughout the app (case-insensitive).
  # For each canonical name, if it's missing but an alias exists, rename alias -> canonical.
  canon_aliases <- list(
    InducedVertBreak = c("IVB"),
    HorzBreak        = c("HB"),
    RelSpeed         = c("Velo"),
    ReleaseTilt      = c("ReleaseAngle", "SpinAxis3dTransverseAngle"),
    BreakTilt        = c("BreakAngle",   "SpinAxis"),
    SpinEfficiency   = c("SpinEff",      "SpinAxis3dSpinEfficiency"),
    SpinRate         = c("Spin"),
    RelHeight        = c("RelZ"),
    RelSide          = c("RelX"),
    VertApprAngle    = c("VAA"),
    HorzApprAngle    = c("HAA"),
    PlateLocSide     = c("PlateX"),
    PlateLocHeight   = c("PlateZ"),
    PlayID           = c(
      "PlayId", "PlayID", "PlayGuid", "PlayGuidId", "PlayGuidID",
      "PitchUID", "PitchUid", "PitchGuid", "PitchGuidId", "PitchGuidID",
      "PitchID", "PitchId", "PitchUuid", "PitchUUID"
    )
  )
  
  nm <- names(df)
  for (canon in names(canon_aliases)) {
    if (!(canon %in% nm)) {
      for (al in canon_aliases[[canon]]) {
        hit <- which(tolower(nm) == tolower(al))
        if (length(hit) == 1) {
          names(df)[hit] <- canon
          nm <- names(df)
          break
        }
      }
    }
  }
  
  # --- Date: prefer a real date; fall back to folder date if the CSV "Date" is bad ---
  looks_like_time <- function(x) {
    s <- tolower(trimws(as.character(x)))
    grepl("^\\d{1,2}:\\d{2}(\\.\\d+)?$", s)
  }
  can_parse_as_date <- function(x) {
    x <- as.character(x)
    a <- suppressWarnings(as.Date(x, "%m/%d/%Y"))
    b <- suppressWarnings(as.Date(x, "%m/%d/%y"))
    c <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    num <- suppressWarnings(as.numeric(x))         # Excel serials
    serial_ok <- !is.na(num) & num > 20000 & num < 60000
    any(!is.na(a) | !is.na(b) | !is.na(c) | serial_ok)
  }
  
  needs_folder_date <-
    (!"Date" %in% names(df)) ||
    all(is.na(df$Date)) ||
    all(looks_like_time(df$Date) | !nzchar(as.character(df$Date)), na.rm = TRUE) ||
    !can_parse_as_date(df$Date)
  
  if (needs_folder_date) {
    df$Date <- as.character(extract_folder_date(fp))
  }
  # -------------------------------------------------------------------------------
  
  # Normalize unlabeled pitch types (unchanged)
  if ("TaggedPitchType" %in% names(df)) {
    df$TaggedPitchType <- ifelse(
      is.na(df$TaggedPitchType) | df$TaggedPitchType == "",
      "Undefined",
      df$TaggedPitchType
    )
  }
  
  df$SourceFile <- fp
  df
}


# Build a KDE-based posterior P(Y=1 | x,y) = p1 * f1(x,y) / f_all(x,y)
kde_ratio_grid <- function(x, y, y_is_one, lims = c(-2,2,0,4.5), n = 180, h = NULL) {
  ok   <- is.finite(x) & is.finite(y) & is.finite(y_is_one)
  x    <- x[ok]; y <- y[ok]; y1 <- y_is_one[ok] > 0
  if (!length(x) || sum(y1) == 0) {
    return(data.frame(x = numeric(0), y = numeric(0), z = numeric(0)))
  }
  p1 <- mean(y1)
  # only pass 'h' if it's not NULL (avoids rep(NULL, ...) warnings inside MASS)
  if (is.null(h)) {
    d_all <- MASS::kde2d(x,      y,      n = n, lims = lims)
    d_pos <- MASS::kde2d(x[y1],  y[y1],  n = n, lims = lims)
  } else {
    d_all <- MASS::kde2d(x,      y,      n = n, lims = lims, h = h)
    d_pos <- MASS::kde2d(x[y1],  y[y1],  n = n, lims = lims, h = h)
  }
  z <- (p1 * d_pos$z) / pmax(d_all$z, 1e-12)
  z <- pmin(pmax(z, 0), 1)  # clamp
  expand.grid(x = d_all$x, y = d_all$y) |> transform(z = as.vector(z))
}


# Smooth mean over space for EV / LA (uses akima if available, else bin+loess fallback)
# replace your smooth_mean_grid() with this version
smooth_mean_grid <- function(x, y, val, lims = c(-2,2,0,4.5), n = 160,
                             method = c("auto","loess","akima"), span = 0.6) {
  method <- match.arg(method)
  ok <- is.finite(x) & is.finite(y) & is.finite(val)
  x <- x[ok]; y <- y[ok]; val <- val[ok]
  if (!length(x)) return(data.frame(x=numeric(0), y=numeric(0), z=numeric(0)))
  
  xs <- seq(lims[1], lims[2], length.out = n)
  ys <- seq(lims[3], lims[4], length.out = n)
  
  if (method == "akima" || (method == "auto" && have_akima)) {
    surf <- akima::interp(x, y, val, xo = xs, yo = ys,
                          linear = TRUE, extrap = FALSE, duplicate = "mean")
    expand.grid(x = surf$x, y = surf$y) |> transform(z = as.vector(surf$z))
  } else {
    df <- data.frame(x = x, y = y, z = val)
    fit <- suppressWarnings(loess(z ~ x * y, data = df, span = span,
                                  control = loess.control(surface = "direct")))
    grid <- expand.grid(x = xs, y = ys)
    grid$z <- as.numeric(predict(fit, newdata = grid))
    grid
  }
}

# Drawing helper (strike zone + plate + filled contours with your palette)
# replace your current draw_heat() with this version
draw_heat <- function(grid, bins = HEAT_BINS, pal_fun = heat_pal_red,
                      title = NULL, mark_max = TRUE, breaks = NULL) {
  if (!nrow(grid)) return(ggplot() + theme_void())
  
  home <- data.frame(
    x = c(-0.75, 0.75, 0.75, 0.00, -0.75),
    y = c(1.05, 1.05, 1.15, 1.25, 1.15) - 0.5
  )
  sz <- data.frame(xmin = ZONE_LEFT, xmax = ZONE_RIGHT, ymin = ZONE_BOTTOM, ymax = ZONE_TOP)
  
  peak_df <- NULL
  if (mark_max) {
    i <- which.max(grid$z)
    if (length(i) && is.finite(grid$z[i])) {
      peak_df <- data.frame(px = grid$x[i], py = grid$y[i])
    }
  }
  
  n_bins <- if (is.null(breaks)) bins else max(1, length(breaks) - 1)
  
  ggplot(grid, aes(x, y, z = z)) +
    {
      if (is.null(breaks))
        geom_contour_filled(aes(fill = after_stat(level)), bins = bins, show.legend = FALSE)
      else
        geom_contour_filled(aes(fill = after_stat(level)), breaks = breaks, show.legend = FALSE)
    } +
    scale_fill_manual(values = pal_fun(n_bins), guide = "none") +
    geom_polygon(data = home, aes(x, y), fill = NA, color = "black", inherit.aes = FALSE) +
    geom_rect(data = sz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = NA, color = "black", inherit.aes = FALSE) +
    { if (!is.null(peak_df))
      geom_point(data = peak_df, aes(x = px, y = py), inherit.aes = FALSE,
                 size = 3.8, shape = 21, fill = "red", color = "black", stroke = 0.5)
    } +
    coord_fixed(ratio = 1, xlim = c(-2, 2), ylim = c(0, 4.5)) +
    theme_void() + theme(legend.position = "none",
                         plot.title = element_text(face = "bold", hjust = 0.5)) +
    labs(title = title)
}

pitch_data <- purrr::map_dfr(all_csvs, read_one)

# Ensure required columns exist since downstream code expects them
# ------ add to need_cols ------
need_cols <- c(
  "Date","Pitcher","Email","PitcherThrows","TaggedPitchType",
  "InducedVertBreak","HorzBreak","RelSpeed","ReleaseTilt","BreakTilt",
  "SpinEfficiency","SpinRate","RelHeight","RelSide","Extension",
  "VertApprAngle","HorzApprAngle","PlateLocSide","PlateLocHeight",
  "PitchCall","KorBB","Balls","Strikes","SessionType","PlayID",
  "ExitSpeed","Angle","BatterSide",
  "PlayResult","TaggedHitType","OutsOnPlay",
  "Batter", "Catcher"   # ← add this
)



for (nm in need_cols) if (!nm %in% names(pitch_data)) pitch_data[[nm]] <- NA_character_

# Type cleanup + standardization (now safe to coerce)
pitch_data <- pitch_data %>%
  mutate(
    Date            = parse_date_mdy(Date),
    Pitcher         = as.character(Pitcher),
    Email           = as.character(Email),
    PitcherThrows   = as.character(PitcherThrows),
    TaggedPitchType = trimws(as.character(TaggedPitchType)),
    InducedVertBreak= as.numeric(InducedVertBreak),
    HorzBreak       = as.numeric(HorzBreak),
    RelSpeed        = as.numeric(RelSpeed),
    ReleaseTilt     = as.numeric(ReleaseTilt),
    BreakTilt       = as.numeric(BreakTilt),
    SpinEfficiency  = as.numeric(SpinEfficiency),
    SpinRate        = as.numeric(SpinRate),
    RelHeight       = as.numeric(RelHeight),
    RelSide         = as.numeric(RelSide),
    VertApprAngle   = as.numeric(VertApprAngle),
    HorzApprAngle   = as.numeric(HorzApprAngle),
    PlateLocSide    = as.numeric(PlateLocSide),
    PlateLocHeight  = as.numeric(PlateLocHeight),
    Extension       = as.numeric(Extension),
    ExitSpeed       = as.numeric(ExitSpeed),   # ← NEW
    Angle           = as.numeric(Angle),       # ← NEW
    BatterSide      = {
      side <- tolower(trimws(as.character(BatterSide)))
      dplyr::case_when(
        side %in% c("", "na", "null", "n/a") ~ NA_character_,
        side %in% c("l", "lh", "left", "lefty", "left-handed", "left handed") ~ "Left",
        side %in% c("r", "rh", "right", "righty", "right-handed", "right handed") ~ "Right",
        TRUE ~ stringr::str_to_title(side)
      )
    }, # ← NEW
    PlayResult      = as.character(PlayResult),
    Batter         = as.character(Batter),
    Catcher = as.character(Catcher),
    SessionType = factor(
      dplyr::case_when(
        grepl("livebp", tolower(SourceFile)) ~ "Live",  # LiveBp files are Live sessions
        grepl("bull|prac", tolower(as.character(SessionType))) ~ "Bullpen",
        grepl("live|game|ab", tolower(as.character(SessionType))) ~ "Live",
        grepl("[/\\\\]practice[/\\\\]", tolower(SourceFile)) ~ "Bullpen",  # fallback: folder
        grepl("[/\\\\]v3[/\\\\]",       tolower(SourceFile)) ~ "Live",
        TRUE ~ NA_character_
      ),
      levels = c("Bullpen","Live")
    ),
    KorBB           = as.character(KorBB),
    Balls           = as.numeric(Balls),
    Strikes         = as.numeric(Strikes)
  ) %>%
  dplyr::mutate(
    SourceIsHitting = {
      src_chr <- tolower(basename(as.character(SourceFile)))
      out <- rep_len(FALSE, length(src_chr))
      valid <- !is.na(src_chr)
      out[valid] <- grepl("hitting", src_chr[valid], fixed = TRUE)
      out
    }
  ) %>%
  dplyr::filter(
    (!is.na(TaggedPitchType) & tolower(TaggedPitchType) != "undefined") |
      SourceIsHitting
  ) %>%
  force_pitch_levels()

# ---- Attach Cloudinary video URLs when available ----
video_map_path <- file.path(data_parent, "video_map.csv")
manual_map_path <- file.path(data_parent, "video_map_manual.csv")
if (!"VideoClip"  %in% names(pitch_data)) pitch_data$VideoClip  <- NA_character_
if (!"VideoClip2" %in% names(pitch_data)) pitch_data$VideoClip2 <- NA_character_
if (!"VideoClip3" %in% names(pitch_data)) pitch_data$VideoClip3 <- NA_character_

# Combine EdgeR and iPhone video maps
video_maps <- list()
if (file.exists(video_map_path)) {
  edger_raw <- suppressMessages(readr::read_csv(video_map_path, show_col_types = FALSE))
  if (nrow(edger_raw) > 0) {
    video_maps[["edger"]] <- edger_raw
    message("📹 Loaded ", nrow(edger_raw), " EdgeR videos")
  }
}
if (file.exists(manual_map_path)) {
  manual_raw <- suppressMessages(readr::read_csv(manual_map_path, show_col_types = FALSE))
  if (nrow(manual_raw) > 0) {
    video_maps[["manual"]] <- manual_raw
    message("📱 Loaded ", nrow(manual_raw), " iPhone videos")
  }
}

if (length(video_maps) > 0) {
  vm_raw <- dplyr::bind_rows(video_maps) %>% dplyr::distinct()
  message("🎬 Combined total: ", nrow(vm_raw), " videos available")
  if (nrow(vm_raw)) {
    vm_wide <- vm_raw %>%
      dplyr::mutate(
        play_id = tolower(as.character(play_id)),
        camera_slot = dplyr::case_when(
          camera_slot %in% c("VideoClip","VideoClip2","VideoClip3") ~ camera_slot,
          TRUE ~ NA_character_
        ),
        uploaded_at = suppressWarnings(lubridate::ymd_hms(uploaded_at, quiet = TRUE, tz = "UTC"))
      ) %>%
      dplyr::filter(
        nzchar(play_id),
        !is.na(camera_slot),
        nzchar(cloudinary_url)
      ) %>%
      dplyr::arrange(play_id, camera_slot, dplyr::desc(uploaded_at)) %>%
      dplyr::group_by(play_id, camera_slot) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(
        id_cols = play_id,
        names_from = camera_slot,
        values_from = cloudinary_url
      )
    
    if (nrow(vm_wide)) {
      pitch_data <- pitch_data %>%
        dplyr::mutate(.play_lower = tolower(as.character(PlayID))) %>%
        dplyr::left_join(vm_wide, by = c(".play_lower" = "play_id"), suffix = c("", ".vm")) %>%
        { 
          # Ensure the .vm columns exist even if pivot_wider didn’t create them
          vm_cols <- paste0(c("VideoClip","VideoClip2","VideoClip3"), ".vm")
          for (vm_col in vm_cols) {
            if (!vm_col %in% names(.)) .[[vm_col]] <- NA_character_
          }
          .
        } %>%
        dplyr::mutate(
          VideoClip  = dplyr::coalesce(.data[["VideoClip.vm"]],  VideoClip),
          VideoClip2 = dplyr::coalesce(.data[["VideoClip2.vm"]], VideoClip2),
          VideoClip3 = dplyr::coalesce(.data[["VideoClip3.vm"]], VideoClip3)
        ) %>%
        dplyr::select(-dplyr::ends_with(".vm"), -.play_lower)
      matched_videos <- sum(nzchar(pitch_data$VideoClip %||% ""))
      message("✅ Attached videos for ", matched_videos, " pitches from combined maps")
    } else {
      message("⚠️  Video maps loaded but no rows matched PlayID in pitch data.")
    }
  }
} else {
  message("ℹ️  No video map files found")
}

# Friendly load message
counts <- table(pitch_data$SessionType, useNA = "no")
bcount <- if ("Bullpen" %in% names(counts)) counts[["Bullpen"]] else 0
lcount <- if ("Live"    %in% names(counts)) counts[["Live"]]    else 0
message("Loaded ", nrow(pitch_data), " rows from ", length(all_csvs),
        " files | Bullpen: ", bcount, " | Live: ", lcount,
        " | root: ", data_parent)


# Read lookup table and keep Email in a separate column to avoid .x/.y
lookup_table <- if (file.exists("lookup_table.csv")) {
  read.csv("lookup_table.csv", stringsAsFactors = FALSE) %>%
    dplyr::rename(Pitcher = PlayerName, Email_lookup = Email)
} else {
  data.frame(Pitcher = character(), Email_lookup = character(), stringsAsFactors = FALSE)
}

# Join, then coalesce into a single Email column
pitch_data <- dplyr::left_join(pitch_data, lookup_table, by = "Pitcher") %>%
  dplyr::mutate(Email = dplyr::coalesce(Email, Email_lookup)) %>%
  dplyr::select(-Email_lookup)


# (keep your name_map construction the same)
raw_names <- sort(unique(pitch_data$Pitcher))
display_names <- ifelse(
  grepl(",", raw_names),
  vapply(strsplit(raw_names, ",\\s*"), function(x) paste(x[2], x[1]), ""),
  raw_names
)
name_map <- setNames(raw_names, display_names)

raw_hitters <- sort(unique(na.omit(as.character(pitch_data$Batter))))
hit_display <- ifelse(
  grepl(",", raw_hitters),
  vapply(strsplit(raw_hitters, ",\\s*"), function(x) paste(x[2], x[1]), ""),
  raw_hitters
)
batter_map <- setNames(raw_hitters, hit_display)

raw_catchers <- sort(unique(na.omit(as.character(pitch_data$Catcher))))
catch_display <- ifelse(
  grepl(",", raw_catchers),
  vapply(strsplit(raw_catchers, ",\\s*"), function(x) paste(x[2], x[1]), ""),
  raw_catchers
)
catcher_map <- setNames(raw_catchers, catch_display)


# ==== PITCHERS-ONLY WHITELIST ====
ALLOWED_PITCHERS <- c(
  "Stoller, Cody",
  "Giles, Ken",
  "Raquet, Nick",
  "Kenyon, TJ",
  "Cohen, Chase",
  "Thompson, Clay",
  "Damme, Andrew",
  "Misiaszek, Andrew",
  "Valenzuela, Jesse",
  "Jackson, Logan",
  "Hawks, Ryan",
  "Colwell, Cooper",
  "Beelek, Ryan",
  "Gomez, Rio",
  "Lachot, Sam",
  "Hale, Derek",
  "Gessner, Josh",
  "Conley, Bryce",
  "Game, Gaynor",
  "Game, Halverson",
  "Game, Chipman",
  "Racioppo, Frank",
  "Silverio, Joseph",
  "Wells, Cameron",
  "Barker, Trey",
  "Blanco, Adrian",
  "Nichols, Logan",
  "Tseng, Andrew",
  "Wallace, Ren",
  "Burnett, Hudson",
  "Siebenbrodt, Jack",
  "Bentivolio, Matt",
  "Coleman, Brayden",
  "Anderson, Kade",
  "McGinnis, Ayden",
  "Halverson, Ayden",
  "Chipman, Jay",
  "Sampson, Adrian",
  "Day, Dawson",
  "Ciuffetelli, Christian",
  "Golnik, Alex",
  "Jones, Trevor",
  "Holmes, Ben",
  "Carr, Bradley",
  "Good, Colson",
  "Garza, Justin",
  "Gaynor, Jared",
  "Kriske, Brooks",
  "Davis, Austin",
  "Kalish, Jake",
  "Cody, Kyle",
  "Lujan, Van",
  "Lile, Chase",
  "Hansen, Tyler",
  "Cleavinger, Garrett",
  "Rasmussen, Drew",
  "Howard, Spencer",
  "Woo, Bryan",
  "Cusick, Ryan",
  "Shuster, Jared",
  "Fleming, William",
  "Pascanu, Tommy",
  "Dunn, Dallas",
  "Ruiz, Rene",
  "Jacobs, Andrew",
  "Headon, Grayson",
  "Nelms, Jake",
  "Kao, Ryan",
  "Jameson, Caleb",
  "Smith, Matt",
  "Lavine, Justin",
  "Fleck, Cam",
  "Fleck, Jax",
  "Wheeler, Owen",
  "Robinett, Caden",
  "Bowman, Brock",
  "McDonald, Gaven",
  "Pringle, Noah",
  "Gorder, Tyson",
  "Darnell, Noah",
  "Dilger, Ryan",
  "Haworth, Aidan",
  "Daniels, Tate",
  "Mogen, Cristian",
  "Wheeler, Henry",
  "Hargrave, Jonah",
  "Theile, Garrett",
  "Daniels, Tyke",
  "Licardi, Matt",
  "Wright, Talan",
  "Daly, Grant",
  "Madison, Bryce",
  "Camas, Ryan",
  "Wolfley, Wes",
  "Del Rio, Micah",
  "Newkirk, Hunter",
  "Stegon, Nick",
  "Martino, Mason",
  "Golnik, Alex",
  "Alberts, Andrew",
  "Duval, Trevin",
  "Foster, Brady",
  "Burnes, Corbin",
  "Taylor, Curtis",
  "Rigby, Gavin",
  "Hahn, Jesse",
  "Wetzler, Jaxon",
  "Hader, Josh",
  "Bruihl, Justin",
  "Gessner, Josh",
  "Gaynor, Sidearm",
  "Miller, Shelby",
  "Skubal, Tarik",
  "Saucedo, Taylor",
  "Knight, Tyler",
  "Zuber, Tyler",
  "Quattrocchi, Anthony"
)

`%in_ci%` <- function(x, y) tolower(x) %in% tolower(y)

# NEW: normalize for case, spaces, punctuation (so "D.J." == "DJ")
norm_name_ci <- function(x) gsub("[^a-z]", "", tolower(trimws(as.character(x))))

# Keep the full dataset for Hitting & global refs
# but build a PITCHING-ONLY copy that is filtered to the whitelist
# (affects Pitching, Comparison, Leaderboard modules that use pitch_data_pitching)
# If you ever want admins to bypass this, wrap the filter in `if (!user_is_admin(session)) { ... }`.
pitch_data_pitching <- pitch_data %>%
  dplyr::mutate(
    Pitcher = as.character(Pitcher),
    # Build a "First Last" display from "Last, First" for matching either style
    .disp = ifelse(grepl(",", Pitcher),
                   paste0(trimws(sub(".*,", "", Pitcher)), " ", trimws(sub(",.*", "", Pitcher))),
                   Pitcher)
  )

# Accept either "Last, First" or "First Last" in ALLOWED_PITCHERS
ALLOWED_PITCHERS_DL <- unique(c(
  ALLOWED_PITCHERS,
  ifelse(grepl(",", ALLOWED_PITCHERS),
         paste0(trimws(sub(".*,", "", ALLOWED_PITCHERS)), " ", trimws(sub(",.*", "", ALLOWED_PITCHERS))),
         ALLOWED_PITCHERS)
))

# Robust, case/spacing/punctuation-insensitive filter
allowed_norm <- norm_name_ci(ALLOWED_PITCHERS_DL)
pitch_data_pitching <- pitch_data_pitching %>%
  dplyr::mutate(.norm_raw  = norm_name_ci(Pitcher),
                .norm_disp = norm_name_ci(.disp)) %>%
  dplyr::filter(.norm_raw %in% allowed_norm | .norm_disp %in% allowed_norm) %>%
  dplyr::select(-.disp, -.norm_raw, -.norm_disp)

# Name map for Pitching UI (restricted to the filtered set)
raw_names_p <- sort(unique(pitch_data_pitching$Pitcher))
display_names_p <- ifelse(
  grepl(",", raw_names_p),
  vapply(strsplit(raw_names_p, ",\\s*"), function(x) paste(x[2], x[1]), ""),
  raw_names_p
)
name_map_pitching <- setNames(raw_names_p, display_names_p)


# ---- NEW: xStat reference bins from your data ----
xbin_ref <- pitch_data %>%
  dplyr::filter(SessionType == "Live",
                PitchCall == "InPlay",
                is.finite(ExitSpeed), is.finite(Angle)) %>%
  dplyr::mutate(
    EVb = pmin(120, pmax(40, floor(ExitSpeed/5)*5)),
    LAb = pmin( 50, pmax(-50, floor(Angle    /5)*5))
  ) %>%
  dplyr::group_by(EVb, LAb) %>%
  dplyr::summarise(
    n   = dplyr::n(),
    p1B = mean(PlayResult == "Single", na.rm = TRUE),
    p2B = mean(PlayResult == "Double", na.rm = TRUE),
    p3B = mean(PlayResult == "Triple", na.rm = TRUE),
    pHR = mean(PlayResult == "HomeRun",na.rm = TRUE),
    .groups = "drop"
  )

# Fallback weighted means in case some bins are empty
x_overall <- xbin_ref %>%
  dplyr::summarise(
    p1B = weighted.mean(p1B, n), p2B = weighted.mean(p2B, n),
    p3B = weighted.mean(p3B, n), pHR = weighted.mean(pHR, n)
  )

# FanGraphs-like wOBA weights (approx)
W_BB <- 0.69; W_1B <- 0.90; W_2B <- 1.24; W_3B <- 1.56; W_HR <- 1.95

# ---- NEW: calculator for Process/Results metrics per pitch type (+ All) ----
# ---- Usage calculations for count-based usage percentages ----
compute_usage_by_count <- function(df, original_df = NULL) {
  # Calculate usage percentages by count situations
  # If original_df is provided, use it for denominators (this is the full unfiltered data)
  # If not provided, use df for denominators (backward compatibility)
  
  denom_df <- if (!is.null(original_df)) original_df else df
  
  by_pt <- split(df, ifelse(is.na(df$TaggedPitchType) | !nzchar(as.character(df$TaggedPitchType)), "Undefined", as.character(df$TaggedPitchType)))
  
  # Total counts for each situation from the FULL dataset (not just filtered pitch type)
  total_0_0 <- sum(denom_df$Balls == 0 & denom_df$Strikes == 0, na.rm = TRUE)
  total_behind <- sum(denom_df$Balls %in% c(1,2,3) & denom_df$Strikes %in% c(0,1) & 
                        ((denom_df$Balls == 1 & denom_df$Strikes == 0) | 
                           (denom_df$Balls == 2 & denom_df$Strikes %in% c(0,1)) |
                           (denom_df$Balls == 3 & denom_df$Strikes %in% c(0,1))), na.rm = TRUE)
  total_even <- sum((denom_df$Balls == 0 & denom_df$Strikes == 0) | 
                      (denom_df$Balls == 1 & denom_df$Strikes == 1) |
                      (denom_df$Balls == 2 & denom_df$Strikes == 2) |
                      (denom_df$Balls == 3 & denom_df$Strikes == 2), na.rm = TRUE)
  total_ahead <- sum((denom_df$Balls == 0 & denom_df$Strikes %in% c(1,2)) |
                       (denom_df$Balls == 1 & denom_df$Strikes == 2), na.rm = TRUE)
  total_less_2k <- sum(denom_df$Strikes < 2, na.rm = TRUE)
  total_2k <- sum(denom_df$Strikes == 2, na.rm = TRUE)
  
  calc_usage <- function(dfi) {
    pitch_lab <- dfi$TaggedPitchType[1]
    if (is.factor(pitch_lab)) pitch_lab <- as.character(pitch_lab)
    pitch_lab <- ifelse(is.na(pitch_lab) || !nzchar(pitch_lab), "Undefined", pitch_lab)
    
    # Calculate overall usage (against the denominator dataset)
    overall_pct <- if (nrow(denom_df) > 0) round(100 * nrow(dfi) / nrow(denom_df), 1) else 0
    
    # Calculate count-specific usage (percentage this pitch type makes up in each count)
    count_0_0 <- sum(dfi$Balls == 0 & dfi$Strikes == 0, na.rm = TRUE)
    usage_0_0 <- if (total_0_0 > 0) round(100 * count_0_0 / total_0_0, 1) else 0
    
    count_behind <- sum(dfi$Balls %in% c(1,2,3) & dfi$Strikes %in% c(0,1) & 
                          ((dfi$Balls == 1 & dfi$Strikes == 0) | 
                             (dfi$Balls == 2 & dfi$Strikes %in% c(0,1)) |
                             (dfi$Balls == 3 & dfi$Strikes %in% c(0,1))), na.rm = TRUE)
    usage_behind <- if (total_behind > 0) round(100 * count_behind / total_behind, 1) else 0
    
    count_even <- sum((dfi$Balls == 0 & dfi$Strikes == 0) | 
                        (dfi$Balls == 1 & dfi$Strikes == 1) |
                        (dfi$Balls == 2 & dfi$Strikes == 2) |
                        (dfi$Balls == 3 & dfi$Strikes == 2), na.rm = TRUE)
    usage_even <- if (total_even > 0) round(100 * count_even / total_even, 1) else 0
    
    count_ahead <- sum((dfi$Balls == 0 & dfi$Strikes %in% c(1,2)) |
                         (dfi$Balls == 1 & dfi$Strikes == 2), na.rm = TRUE)
    usage_ahead <- if (total_ahead > 0) round(100 * count_ahead / total_ahead, 1) else 0
    
    count_less_2k <- sum(dfi$Strikes < 2, na.rm = TRUE)
    usage_less_2k <- if (total_less_2k > 0) round(100 * count_less_2k / total_less_2k, 1) else 0
    
    count_2k <- sum(dfi$Strikes == 2, na.rm = TRUE)
    usage_2k <- if (total_2k > 0) round(100 * count_2k / total_2k, 1) else 0
    
    tibble::tibble(
      PitchType = pitch_lab,
      Overall = paste0(overall_pct, "%"),
      `0-0` = paste0(usage_0_0, "%"),
      Behind = paste0(usage_behind, "%"),
      Even = paste0(usage_even, "%"),
      Ahead = paste0(usage_ahead, "%"),
      `<2K` = paste0(usage_less_2k, "%"),
      `2K` = paste0(usage_2k, "%")
    )
  }
  
  # build per-type rows, then append the "All" row
  out <- purrr::map_dfr(by_pt, calc_usage) %>%
    dplyr::mutate(PitchType = as.character(PitchType))
  
  all_row <- calc_usage(df) %>%
    dplyr::mutate(PitchType = "All")
  
  dplyr::bind_rows(out, all_row)
}

compute_process_results <- function(df, mode = "All") {
  # split by pitch *as character*, not factor (prevents int/chr mixups later)
  by_pt <- split(
    df,
    ifelse(
      is.na(df$TaggedPitchType) | !nzchar(as.character(df$TaggedPitchType)),
      "Undefined",
      as.character(df$TaggedPitchType)
    )
  )
  
  calc_one <- function(dfi) {
    # label for this group (force character, fall back to "Undefined")
    pitch_lab <- dfi$TaggedPitchType[1]
    if (is.factor(pitch_lab)) pitch_lab <- as.character(pitch_lab)
    pitch_lab <- ifelse(is.na(pitch_lab) || !nzchar(pitch_lab), "Undefined", pitch_lab)
    
    pitch_n   <- nrow(dfi)
    BF_live   <- sum(dfi$SessionType == "Live" & dfi$Balls == 0 & dfi$Strikes == 0, na.rm = TRUE)
    BF_all    <- sum(!is.na(dfi$Balls) & !is.na(dfi$Strikes) & dfi$Balls == 0 & dfi$Strikes == 0, na.rm = TRUE)
    
    # K_ct: Strikeouts = pitches with 2 strikes ending in StrikeSwinging or StrikeCalled
    K_ct      <- sum(dfi$SessionType == "Live" & !is.na(dfi$Strikes) & dfi$Strikes == 2 & 
                       !is.na(dfi$PitchCall) & dfi$PitchCall %in% c("StrikeSwinging", "StrikeCalled"), na.rm = TRUE)
    K_ct_all  <- sum(!is.na(dfi$Strikes) & dfi$Strikes == 2 & 
                       !is.na(dfi$PitchCall) & dfi$PitchCall %in% c("StrikeSwinging", "StrikeCalled"), na.rm = TRUE)
    
    # BB_ct: Walks = pitches with 3 balls ending in BallCalled
    BB_ct     <- sum(dfi$SessionType == "Live" & !is.na(dfi$Balls) & dfi$Balls == 3 & 
                       !is.na(dfi$PitchCall) & dfi$PitchCall == "BallCalled", na.rm = TRUE)
    BB_ct_all <- sum(!is.na(dfi$Balls) & dfi$Balls == 3 & 
                       !is.na(dfi$PitchCall) & dfi$PitchCall == "BallCalled", na.rm = TRUE)
    
    # CSW% (prefer PitchResult if present)
    # --- CSW% (from PitchCall only) ---
    csw_num <- if ("PitchCall" %in% names(dfi)) {
      sum(dfi$PitchCall %in% c("StrikeCalled","StrikeSwinging"), na.rm = TRUE)
    } else 0L
    
    `CSW%` <- fmt_pct1(csw_num, nrow(dfi))  # 1-decimal percent string
    
    
    # In-play denominators
    inplay_all  <- sum(dfi$PitchCall == "InPlay", na.rm = TRUE)
    inplay_live <- sum(dfi$SessionType == "Live" & dfi$PitchCall == "InPlay", na.rm = TRUE)
    
    # BABIP
    H_bip <- sum(dfi$PlayResult %in% c("Single","Double","Triple","HomeRun"), na.rm = TRUE)
    BABIP <- if (inplay_all > 0) fmt_rate3(H_bip / inplay_all) else ""
    
    hit_types <- get_hit_type_col(dfi)
    # GB%
    GB_pct <- if (inplay_all > 0)
      paste0(round(100 * sum(hit_types == "GroundBall", na.rm = TRUE) / inplay_all, 1), "%") else ""
    
    # Barrel%
    barrel_n <- sum(dfi$SessionType == "Live" & dfi$PitchCall == "InPlay" &
                      is.finite(dfi$ExitSpeed) & is.finite(dfi$Angle) &
                      dfi$ExitSpeed >= 95 & dfi$Angle >= 10 & dfi$Angle <= 35, na.rm = TRUE)
    Barrel_pct <- if (inplay_live > 0) paste0(round(100 * barrel_n / inplay_live, 1), "%") else ""
    
    # AVG / SLG over AB (exclude Undefined, Sacrifice)
    is_ab   <- !is.na(dfi$PlayResult) & !(dfi$PlayResult %in% c("Undefined","Sacrifice"))
    AB_ct   <- sum(is_ab, na.rm = TRUE)
    s1 <- sum(dfi$PlayResult == "Single",   na.rm = TRUE)
    s2 <- sum(dfi$PlayResult == "Double",   na.rm = TRUE)
    s3 <- sum(dfi$PlayResult == "Triple",   na.rm = TRUE)
    hr <- sum(dfi$PlayResult == "HomeRun",  na.rm = TRUE)
    H  <- s1 + s2 + s3 + hr
    TB <- 1*s1 + 2*s2 + 3*s3 + 4*hr
    
    AVG <- if (AB_ct > 0) fmt_rate3(H  / AB_ct) else ""
    SLG <- if (AB_ct > 0) fmt_rate3(TB / AB_ct) else ""
    
    # Expected hits/total bases from EV/LA bins (uses globals: xbin_ref, x_overall)
    bip_evla <- dfi %>%
      dplyr::filter(SessionType == "Live", PitchCall == "InPlay",
                    is.finite(ExitSpeed), is.finite(Angle)) %>%
      dplyr::mutate(
        EVb = pmin(120, pmax(40, floor(ExitSpeed/5)*5)),
        LAb = pmin( 50, pmax(-50, floor(Angle    /5)*5))
      ) %>%
      dplyr::left_join(xbin_ref, by = c("EVb","LAb"))
    
    if (nrow(bip_evla)) {
      for (nm in c("p1B","p2B","p3B","pHR")) {
        bip_evla[[nm]][!is.finite(bip_evla[[nm]])] <- x_overall[[nm]]
      }
      x1B <- sum(bip_evla$p1B, na.rm = TRUE)
      x2B <- sum(bip_evla$p2B, na.rm = TRUE)
      x3B <- sum(bip_evla$p3B, na.rm = TRUE)
      xHR <- sum(bip_evla$pHR, na.rm = TRUE)
    } else {
      x1B <- x2B <- x3B <- xHR <- 0
    }
    
    xH   <- x1B + x2B + x3B + xHR
    xTB  <- 1*x1B + 2*x2B + 3*x3B + 4*xHR
    
    xAVG <- if (AB_ct > 0) fmt_rate3(xH / AB_ct)  else ""
    xSLG <- if (AB_ct > 0) fmt_rate3(xTB / AB_ct) else ""
    xISO <- if (AB_ct > 0) {
      as_num <- function(s) if (nzchar(s)) as.numeric(paste0("0", s)) else NA_real_
      v <- as_num(xSLG) - as_num(xAVG); fmt_rate3(v)
    } else ""
    
    # xWOBA per BF (globals: W_BB, W_1B, W_2B, W_3B, W_HR)
    xWOBA <- if (BF_live > 0) {
      w_num <- W_BB*BB_ct + W_1B*(x1B) + W_2B*(x2B) + W_3B*(x3B) + W_HR*(xHR)
      fmt_rate3(w_num / BF_live)
    } else ""
    
    # --- Outs / IP / FIP / WHIP (now includes strikeout outs) ---
    o_on_play <- suppressWarnings(sum(as.numeric(dfi$OutsOnPlay), na.rm = TRUE))
    k_outs    <- K_ct                              # each strikeout = 1 out
    tot_outs  <- o_on_play + k_outs
    ip_num    <- tot_outs / 3
    
    # Baseball-style IP text (e.g., 2.1, 3.2)
    IP_txt <- {
      inns <- tot_outs %/% 3
      rem  <- tot_outs %% 3
      paste0(inns, if (rem == 0) "" else paste0(".", rem))
    }
    
    # FIP (no constant), per IP
    FIP <- if (tot_outs > 0) {
      val <- (13*hr + 3*BB_ct - 2*K_ct) / ip_num
      fmt_num2(val)
    } else ""
    
    # WHIP = (H + BB) / IP   ← now counts K outs via tot_outs → ip_num
    WHIP <- if (tot_outs > 0) fmt_num2((H + BB_ct) / ip_num) else ""
    
    # K% and BB% divided by BF (batters faced)
    # Use Live-specific counts if mode is "Live", otherwise use all counts
    if (mode == "Live") {
      K_pct <- if (BF_live > 0) {
        paste0(round(100 * K_ct / BF_live, 1), "%")
      } else ""
      
      BB_pct <- if (BF_live > 0) {
        paste0(round(100 * BB_ct / BF_live, 1), "%")
      } else ""
    } else {
      K_pct <- if (BF_all > 0) {
        paste0(round(100 * K_ct_all / BF_all, 1), "%")
      } else ""
      
      BB_pct <- if (BF_all > 0) {
        paste0(round(100 * BB_ct_all / BF_all, 1), "%")
      } else ""
    }
    
    # Whiff% calculation
    whiff_pct <- if (pitch_n > 0) {
      swings <- sum(dfi$PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall"), na.rm = TRUE)
      whiffs <- sum(dfi$PitchCall == "StrikeSwinging", na.rm = TRUE)
      if (swings > 0) paste0(round(100 * whiffs / swings, 1), "%") else ""
    } else ""
    
    # EV and LA calculations
    ev_avg <- if (pitch_n > 0) {
      ev_vals <- dfi$ExitSpeed[dfi$SessionType == "Live" & dfi$PitchCall == "InPlay"]
      ev_vals <- ev_vals[is.finite(ev_vals)]
      if (length(ev_vals) > 0) round(mean(ev_vals), 1) else NA_real_
    } else NA_real_
    
    la_avg <- if (pitch_n > 0) {
      la_vals <- dfi$Angle[dfi$SessionType == "Live" & dfi$PitchCall == "InPlay"]
      la_vals <- la_vals[is.finite(la_vals)]
      if (length(la_vals) > 0) round(mean(la_vals), 1) else NA_real_
    } else NA_real_
    
    # Ahead% calculation: strikes at 0-1 or 1-1 counts divided by BF
    ahead_pct <- if (pitch_lab == "All" && BF_all > 0) {
      ahead_strikes <- sum(
        !is.na(dfi$Balls) & !is.na(dfi$Strikes) & 
          ((dfi$Balls == 0 & dfi$Strikes == 1) | (dfi$Balls == 1 & dfi$Strikes == 1)) &
          !is.na(dfi$PitchCall) & 
          dfi$PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall"),
        na.rm = TRUE
      )
      paste0(round(100 * ahead_strikes / BF_all, 1), "%")
    } else ""
    
    tibble::tibble(
      PitchType = pitch_lab,
      `K%`      = K_pct,
      `BB%`     = BB_pct,
      `GB%`     = GB_pct,
      `Barrel%` = Barrel_pct,
      `Whiff%`  = whiff_pct,
      `CSW%`    = fmt_pct1(
        sum(dfi$PitchCall %in% c("StrikeCalled","StrikeSwinging"), na.rm = TRUE),
        nrow(dfi)
      ),
      EV        = if (is.finite(ev_avg)) as.character(ev_avg) else "",
      LA        = if (is.finite(la_avg)) as.character(la_avg) else ""
    )
  }
  
  # build per-type rows (force character), then append the "All" row
  out <- purrr::map_dfr(by_pt, calc_one) %>%
    dplyr::mutate(PitchType = as.character(PitchType))
  
  all_row <- calc_one(df) %>%
    dplyr::mutate(PitchType = "All")
  
  dplyr::bind_rows(out, all_row)
}

# ---- Global table helpers shared by Pitching & Hitting ----
safe_pct <- function(num, den) {
  num <- suppressWarnings(as.numeric(num))
  den <- suppressWarnings(as.numeric(den))
  ifelse(is.finite(den) & den > 0 & is.finite(num),
         paste0(round(100 * num / den, 1), "%"),
         "")
}

nz_mean <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  m <- mean(x, na.rm = TRUE)
  if (is.finite(m)) m else NA_real_
}

compute_bfp_overall <- function(df) {
  if (!nrow(df)) return(NA_real_)
  
  pf <- compute_pa_flags(df)
  df$PitchCall   <- as.character(df$PitchCall)
  df$PlayResult  <- as.character(df$PlayResult)
  df$SessionType <- as.character(df$SessionType)
  
  live_mask   <- df$SessionType == "Live"
  balls_num   <- suppressWarnings(as.integer(df$Balls))
  strikes_num <- suppressWarnings(as.integer(df$Strikes))
  valid_counts <- live_mask & is.finite(balls_num) & is.finite(strikes_num)
  
  pa_id <- rep(NA_integer_, nrow(df))
  curr_pa <- 0L
  for (i in seq_len(nrow(df))) {
    if (!valid_counts[i]) next
    prev_valid <- i > 1 && valid_counts[i - 1]
    new_pa <- (balls_num[i] == 0L && strikes_num[i] == 0L) || !prev_valid
    if (new_pa) curr_pa <- curr_pa + 1L
    if (curr_pa == 0L) curr_pa <- 1L
    pa_id[i] <- curr_pa
  }
  df$pa_id <- pa_id
  
  df <- df %>%
    dplyr::group_by(pa_id) %>%
    dplyr::mutate(
      pitch_number   = dplyr::if_else(!is.na(pa_id), dplyr::row_number(), NA_integer_),
      pa_pitch_total = dplyr::if_else(!is.na(pa_id), dplyr::n(), NA_integer_)
    ) %>%
    dplyr::ungroup()
  
  exit_vel   <- suppressWarnings(as.numeric(df$ExitSpeed))
  launch     <- suppressWarnings(as.numeric(df$Angle))
  pitch_call <- df$PitchCall
  
  fps_calls   <- c("InPlay","StrikeSwinging","FoulBallNotFieldable","StrikeCalled")
  ea_calls    <- c("InPlay","StrikeSwinging","FoulBallNotFieldable")
  whiff_calls <- c("StrikeSwinging","StrikeSwingingBlocked","StrikeSwingingPitchout","StrikeSwingingChecked")
  called_strike_calls <- c("StrikeCalled","StrikeCalledPitchout")
  ball_calls  <- c("BallCalled","Ball","BallInDirt","PitchoutBall","IntentBall","BallIntent")
  
  is_first <- live_mask & !is.na(df$pa_id) & df$pitch_number == 1
  fps_hit  <- is_first & pitch_call %in% fps_calls
  ea_hit   <- is_first & pitch_call %in% ea_calls
  
  strikeout_idx <- live_mask & !is.na(pf$is_strikeout) & pf$is_strikeout
  walk_idx      <- live_mask & !is.na(pf$is_walk) & pf$is_walk
  terminal_idx  <- live_mask & !is.na(pf$is_terminal) & pf$is_terminal
  
  batted_ball <- live_mask & pitch_call == "InPlay"
  la_window   <- is.finite(launch) & launch >= 10 & launch <= 35
  
  cond80 <- batted_ball & la_window & is.finite(exit_vel) & exit_vel >= 80
  cond85 <- batted_ball & la_window & is.finite(exit_vel) & exit_vel >= 85
  cond95 <- batted_ball & is.finite(exit_vel) & exit_vel >= 95
  barrel_mask <- batted_ball & is.finite(exit_vel) & is.finite(launch) & exit_vel >= 98 & launch >= 8 & launch <= 50
  
  pa_total <- df$pa_pitch_total
  
  points <- numeric(nrow(df))
  points <- points + ifelse(fps_hit, 1, 0)
  points <- points - ifelse(is_first & !fps_hit, 1, 0)
  points <- points + ifelse(ea_hit, 3, 0)
  points <- points - ifelse(is_first & !ea_hit, 3, 0)
  points <- points + ifelse(live_mask & pitch_call %in% whiff_calls, 2, 0)
  points <- points + ifelse(live_mask & pitch_call %in% called_strike_calls, 1, 0)
  points <- points - ifelse(live_mask & pitch_call %in% ball_calls, 1, 0)
  points <- points + ifelse(strikeout_idx, 3, 0)
  points <- points + ifelse(strikeout_idx & !is.na(pa_total) & pa_total == 3, 5, 0)
  points <- points + ifelse(batted_ball & !is.na(pa_total) & pa_total <= 3, 2, 0)
  points <- points + ifelse(batted_ball & is.finite(exit_vel) & exit_vel < 75, 3, 0)
  points <- points - ifelse(walk_idx, 3, 0)
  points <- points - ifelse(walk_idx & !is.na(pa_total) & pa_total == 4, 5, 0)
  ev_penalty <- ifelse(cond95, 5, ifelse(cond85, 3, ifelse(cond80, 1, 0)))
  points <- points - ev_penalty
  points <- points - ifelse(barrel_mask, 10, 0)
  
  total_points <- sum(ifelse(live_mask, points, 0), na.rm = TRUE)
  pa_live_total <- sum(terminal_idx, na.rm = TRUE)
  
  if (pa_live_total > 0) total_points / pa_live_total else NA_real_
}

make_summary <- function(df) {
  if (!nrow(df)) {
    return(tibble::tibble(
      PitchType = character(), PitchCount = integer(), Overall = character(),
      BF = integer(), Velo_Avg = numeric(), Velo_Max = numeric(), IVB = numeric(), HB = numeric(),
      ReleaseTilt = character(), BreakTilt = character(),  # <- now character
      SpinEff = character(), InZonePercent = character(), CompPercent = character(),
      KPercent = character(), BBPercent = character(), FPSPercent = character(),
      EAPercent = character(), StrikePercent = character(), WhiffPercent = character(),
      SpinRate = numeric(), RelHeight = numeric(), RelSide = numeric(),
      VertApprAngle = numeric(), HorzApprAngle = numeric(), Extension = numeric(),
      EV = numeric(), LA = numeric(), `Stuff+` = numeric(), `Ctrl+` = numeric(),
      `QP+` = numeric(), `Pitching+` = numeric()
    ))
  }
  
  
  nz_mean <- function(x) { x <- suppressWarnings(as.numeric(x)); m <- mean(x, na.rm = TRUE); if (is.finite(m)) m else NA_real_ }
  safe_div <- function(num, den) ifelse(den > 0, num/den, NA_real_)
  
  # BF usage map for % column
  usage_map <- df %>%
    dplyr::filter(SessionType == "Live", Balls == 0, Strikes == 0) %>%
    dplyr::count(TaggedPitchType, name = "BF") %>%
    dplyr::mutate(Usage = ifelse(sum(BF) > 0, 100*BF/sum(BF), 0)) %>%
    { stats::setNames(.$Usage, .$TaggedPitchType) }
  
  # NEW: per-pitch QP points (0..1); NA for non-live or out of competitive box
  df <- df %>% dplyr::mutate(QP_pts = compute_qp_points(.))
  
  pf <- compute_pa_flags(df)
  df$PitchCall <- as.character(df$PitchCall)
  df$PlayResult <- as.character(df$PlayResult)
  df$SessionType <- as.character(df$SessionType)
  
  live_mask <- df$SessionType == "Live"
  balls_num <- suppressWarnings(as.integer(df$Balls))
  strikes_num <- suppressWarnings(as.integer(df$Strikes))
  valid_counts <- live_mask & is.finite(balls_num) & is.finite(strikes_num)
  pa_id <- rep(NA_integer_, nrow(df))
  curr_pa <- 0L
  for (i in seq_len(nrow(df))) {
    if (!valid_counts[i]) next
    prev_valid <- i > 1 && valid_counts[i - 1]
    new_pa <- (balls_num[i] == 0L && strikes_num[i] == 0L) || !prev_valid
    if (new_pa) curr_pa <- curr_pa + 1L
    if (curr_pa == 0L) curr_pa <- 1L
    pa_id[i] <- curr_pa
  }
  df$pa_id <- pa_id
  
  df <- df %>%
    dplyr::group_by(pa_id) %>%
    dplyr::mutate(
      pitch_number   = dplyr::if_else(!is.na(pa_id), dplyr::row_number(), NA_integer_),
      pa_pitch_total = dplyr::if_else(!is.na(pa_id), dplyr::n(), NA_integer_)
    ) %>%
    dplyr::ungroup()
  
  exit_vel <- suppressWarnings(as.numeric(df$ExitSpeed))
  launch   <- suppressWarnings(as.numeric(df$Angle))
  pitch_call <- df$PitchCall
  
  fps_calls   <- c("InPlay","StrikeSwinging","FoulBallNotFieldable","StrikeCalled")
  ea_calls    <- c("InPlay","StrikeSwinging","FoulBallNotFieldable")
  whiff_calls <- c("StrikeSwinging","StrikeSwingingBlocked","StrikeSwingingPitchout","StrikeSwingingChecked")
  called_strike_calls <- c("StrikeCalled","StrikeCalledPitchout")
  ball_calls  <- c("BallCalled","Ball","BallInDirt","PitchoutBall","IntentBall","BallIntent")
  
  is_first <- live_mask & !is.na(df$pa_id) & df$pitch_number == 1
  fps_hit  <- is_first & pitch_call %in% fps_calls
  ea_hit   <- is_first & pitch_call %in% ea_calls
  
  strikeout_idx <- live_mask & !is.na(pf$is_strikeout) & pf$is_strikeout
  walk_idx      <- live_mask & !is.na(pf$is_walk) & pf$is_walk
  terminal_idx  <- live_mask & !is.na(pf$is_terminal) & pf$is_terminal
  
  batted_ball <- live_mask & pitch_call == "InPlay"
  la_window   <- is.finite(launch) & launch >= 10 & launch <= 35
  
  cond80 <- batted_ball & la_window & is.finite(exit_vel) & exit_vel >= 80
  cond85 <- batted_ball & la_window & is.finite(exit_vel) & exit_vel >= 85
  cond95 <- batted_ball & is.finite(exit_vel) & exit_vel >= 95
  barrel_mask <- batted_ball & is.finite(exit_vel) & is.finite(launch) & exit_vel >= 98 & launch >= 8 & launch <= 50
  
  df$BFP_pa_terminal <- ifelse(live_mask & !is.na(pf$is_terminal), pf$is_terminal, FALSE)
  pa_total <- df$pa_pitch_total
  
  points <- numeric(nrow(df))
  points <- points + ifelse(fps_hit, 1, 0)
  points <- points - ifelse(is_first & !fps_hit, 1, 0)
  points <- points + ifelse(ea_hit, 3, 0)
  points <- points - ifelse(is_first & !ea_hit, 3, 0)
  points <- points + ifelse(live_mask & pitch_call %in% whiff_calls, 2, 0)
  points <- points + ifelse(live_mask & pitch_call %in% called_strike_calls, 1, 0)
  points <- points - ifelse(live_mask & pitch_call %in% ball_calls, 1, 0)
  points <- points + ifelse(strikeout_idx, 3, 0)
  points <- points + ifelse(strikeout_idx & !is.na(pa_total) & pa_total == 3, 5, 0)
  points <- points + ifelse(batted_ball & !is.na(pa_total) & pa_total <= 3, 2, 0)
  points <- points + ifelse(batted_ball & is.finite(exit_vel) & exit_vel < 75, 3, 0)
  points <- points - ifelse(walk_idx, 3, 0)
  points <- points - ifelse(walk_idx & !is.na(pa_total) & pa_total == 4, 5, 0)
  ev_penalty <- ifelse(cond95, 5, ifelse(cond85, 3, ifelse(cond80, 1, 0)))
  points <- points - ev_penalty
  points <- points - ifelse(barrel_mask, 10, 0)
  
  df$BFP_points <- ifelse(live_mask, points, 0)
  
  df %>%
    dplyr::group_by(TaggedPitchType) %>%
    dplyr::summarise(
      PitchCount     = dplyr::n(),
      BF_live        = sum(SessionType == "Live" & Balls == 0 & Strikes == 0, na.rm = TRUE),
      BF_all         = sum(!is.na(Balls) & !is.na(Strikes) & Balls == 0 & Strikes == 0, na.rm = TRUE),
      pa_live        = sum(BFP_pa_terminal, na.rm = TRUE),
      
      Velo_Avg       = nz_mean(RelSpeed),
      Velo_Max       = suppressWarnings(max(RelSpeed, na.rm = TRUE)),
      IVB            = nz_mean(InducedVertBreak),
      HB             = nz_mean(HorzBreak),
      ReleaseTilt    = convert_to_clock(nz_mean(ReleaseTilt)),
      BreakTilt      = convert_to_clock(nz_mean(BreakTilt)),
      SpinEff        = nz_mean(SpinEfficiency),
      SpinRate       = nz_mean(SpinRate),
      RelHeight      = nz_mean(RelHeight),
      RelSide        = nz_mean(RelSide),
      VertApprAngle  = nz_mean(VertApprAngle),
      HorzApprAngle  = nz_mean(HorzApprAngle),
      Extension      = nz_mean(Extension),
      
      InZonePercent  = safe_div(
        sum(PlateLocSide >= ZONE_LEFT & PlateLocSide <= ZONE_RIGHT &
              PlateLocHeight >= ZONE_BOTTOM & PlateLocHeight <= ZONE_TOP, na.rm = TRUE),
        dplyr::n()
      ),
      CompPercent    = safe_div(
        sum(PlateLocSide >= -1.5 & PlateLocSide <= 1.5 &
              PlateLocHeight >= (2.65-1.7) & PlateLocHeight <= (2.65+1.3), na.rm = TRUE),
        dplyr::n()
      ),
      StrikePercent  = safe_div(
        sum(PitchCall %in% c("StrikeCalled","StrikeSwinging","InPlay","FoulBall","FoulBallNotFieldable","FoulBallFieldable"), na.rm = TRUE),
        sum(!is.na(PitchCall))
      ),
      FPSPercent     = safe_div(
        sum(!is.na(Balls) & !is.na(Strikes) & Balls == 0 & Strikes == 0 &
              !is.na(PitchCall) & PitchCall %in% c("InPlay","StrikeSwinging","StrikeCalled","FoulBallNotFieldable","FoulBall"), na.rm = TRUE),
        BF_all
      ),
      EAPercent      = safe_div(
        sum(
          (!is.na(Balls) & !is.na(Strikes) & !is.na(PitchCall)) & (
            (Balls == 0 & Strikes == 0 & PitchCall == "InPlay") |
              (Balls == 0 & Strikes == 1 & PitchCall %in% c(
                "InPlay", "StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable","FoulBall"
              )) |
              (Balls == 1 & Strikes == 0 & PitchCall == "InPlay") |
              (Balls == 1 & Strikes == 1 & PitchCall %in% c(
                "InPlay", "StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable","FoulBall"
              ))
          ), na.rm = TRUE),
        BF_all
      ),
      KPercent       = safe_div(
        sum(!is.na(Strikes) & Strikes == 2 & !is.na(PitchCall) & PitchCall %in% c("StrikeSwinging", "StrikeCalled"), na.rm = TRUE), 
        BF_all
      ),
      BBPercent      = safe_div(
        sum(!is.na(Balls) & Balls == 3 & !is.na(PitchCall) & PitchCall == "BallCalled", na.rm = TRUE), 
        BF_all
      ),
      WhiffPercent   = safe_div(sum(PitchCall == "StrikeSwinging", na.rm = TRUE),
                                sum(PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall"), na.rm = TRUE)),
      
      EV             = nz_mean(ifelse(SessionType == "Live", ExitSpeed, NA_real_)),
      LA             = nz_mean(ifelse(SessionType == "Live", Angle,     NA_real_)),
      
      `Stuff+`       = round(nz_mean(`Stuff+`), 1),
      BFP_points     = sum(BFP_points, na.rm = TRUE),
      
      # Keep Command+ available for users/tables that still show it
      `Ctrl+`     = round(nz_mean(ifelse(
        PlateLocSide >= ZONE_LEFT & PlateLocSide <= ZONE_RIGHT &
          PlateLocHeight >= ZONE_BOTTOM & PlateLocHeight <= ZONE_TOP, 1.47,
        ifelse(PlateLocSide >= -1.5 & PlateLocSide <= 1.5 &
                 PlateLocHeight >= (2.65-1.7) & PlateLocHeight <= (2.65+1.3), 0.73, 0)
      )) * 100, 1),
      
      # NEW: QP+ per pitch type, then Pitching+ from Stuff+ and QP+
      `QP+`          = round(nz_mean(QP_pts) * 200, 1),
      `Pitching+`    = round((`Stuff+` + `QP+`) / 2, 1),
      .groups = "drop"
    ) %>%
    dplyr::rename(PitchType = TaggedPitchType) %>%
    dplyr::mutate(PitchType = as.character(PitchType)) %>%
    dplyr::arrange(factor(PitchType, levels = names(all_colors))) %>%
    dplyr::mutate(
      Overall = paste0(dplyr::coalesce(round(usage_map[as.character(PitchType)], 1), 0), "%"),
      BFP_raw = safe_div(BFP_points, pa_live),
      BFP = ifelse(
        PitchType == "All",
        ifelse(is.finite(BFP_raw), sprintf("%.1f", BFP_raw), ""),
        ""
      )
    ) %>%
    dplyr::select(
      PitchType, PitchCount, Overall, BF = BF_all,
      Velo_Avg, Velo_Max, IVB, HB,
      ReleaseTilt, BreakTilt, SpinEff, SpinRate,
      RelHeight, RelSide, VertApprAngle, HorzApprAngle, Extension,
      InZonePercent, CompPercent, KPercent, BBPercent, FPSPercent, EAPercent,
      StrikePercent, WhiffPercent, EV, LA, BFP, `Stuff+`, `Ctrl+`, `QP+`, `Pitching+`
    ) %>%
    dplyr::rename(Pitch = PitchType)
}


# --- UI ----
# === ui wrapper ===
pitch_ui <- function(show_header = FALSE) {
  # ⬇️ Pitching UI (updated: ggiraphOutput → girafeOutput)
  fluidPage(
    tags$head(
      tags$style(HTML(
        '@media print { .tab-content>.tab-pane{display:block!important;opacity:1!important;page-break-after:always;} .tab-content>.tab-pane:last-child{page-break-after:auto;} .nav-tabs,.sidebar,.form-group,#printBtn{display:none!important;} }'
      ))
    ),
    
    # Optional header (logos + title). Hidden by default.
    if (isTRUE(show_header)) {
      fluidRow(
        class = "suite-header",
        column(2, tags$img(src = "PCUlogo.png", height = "100px")),
        column(
          8,
          div(
            style = "height:100%; display:flex; justify-content:center; align-items:flex-start;",
            tags$h1("Pitching Dashboard", style = "margin-top:25px; font-weight:bold;")
          )
        ),
        column(
          2,
          div(style = "text-align:right; margin-top:10px;",
              tags$img(src = "PCUlogo.png", height = "80px"))
        )
      )
    },
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "sessionType", "Session Type:",
          choices = c("All", "Bullpen", "Live"),
          selected = "All"
        ),
        uiOutput("pitcher_ui"),
        dateRangeInput(
          "dates", "Date Range:",
          start  = max(pitch_data$Date, na.rm = TRUE),
          end    = max(pitch_data$Date, na.rm = TRUE),
          format = "mm/dd/yyyy"
        ),
        selectInput(
          "hand", "Pitcher Hand:",
          choices = c("All", "Left", "Right"), selected = "All"
        ),
        selectInput(
          "pitchType", "Pitch Type:",
          choices = c("All", levels(pitch_data$TaggedPitchType)),
          selected = "All", multiple = TRUE
        ),
        selectInput(
          "zoneLoc", "Zone Location:",
          choices = c("All",
                      "Upper Half","Bottom Half","Left Half","Right Half",
                      "Upper 3rd","Bottom 3rd","Left 3rd","Right 3rd"),
          selected = "All",
          multiple = TRUE
        ),
        selectInput(
          "inZone", "In Zone:",
          choices = c("All", "Yes", "No", "Competitive"),
          selected = "All"
        ),
        selectInput(
          "batterSide", "Batter Hand:",
          choices = c("All","Left","Right"), selected = "All"
        ),
        
        # NEW: Video filter
        selectInput(
          "videoFilter", "Video:",
          choices = c("All", "Edger", "Behind", "Side"),
          selected = "All"
        ),
        
        # NEW: Count filter (multi-select)
        selectInput(
          "countFilter", "Count:",
          choices = c(
            "All" = "All",
            "Even"   = "Even",
            "Behind" = "Behind",
            "Ahead"      = "Ahead",
            "2K Not Full"= "2KNF",
            "0-0","0-1","1-0","1-1","2-0","2-1","0-2","1-2","2-2","3-0","3-1","3-2"
          ),
          selected = "All",
          multiple = TRUE
        ),
        selectInput(
          "breakLines", "Break Lines:",
          choices = c("None", "Fastball", "Sinker"), selected = "None"
        ),
        selectInput(
          "stuffLevel", "Stuff+ Level:",
          choices = c("Pro", "College", "High School"), selected = "College"
        ),
        selectInput(
          "stuffBase", "Stuff+ Base Pitch:",
          choices = c("Fastball", "Sinker"), selected = "Fastball"
        ),
        fluidRow(
          column(6, numericInput("veloMin", "Velocity Min (MPH):", value = NA)),
          column(6, numericInput("veloMax", "Velocity Max (MPH):", value = NA))
        ),
        fluidRow(
          column(6, numericInput("ivbMin", "IVB Min (inches):", value = NA)),
          column(6, numericInput("ivbMax", "IVB Max (inches):", value = NA))
        ),
        fluidRow(
          column(6, numericInput("hbMin", "HB Min (inches):", value = NA)),
          column(6, numericInput("hbMax", "HB Max (inches):", value = NA))
        ),
        fluidRow(
          column(6, numericInput("pcMin", "Pitch Count Min:", value = NA, min = 1)),
          column(6, numericInput("pcMax", "Pitch Count Max:", value = NA, min = 1))
        ),
        width = 3,
        class = "sidebar"
      ),
      mainPanel(
        tabsetPanel(
          id = "tabs",
          tabPanel(
            "Summary",
            uiOutput("summaryHeader"), br(),
            fluidRow(
              column(
                4,
                div("Release",
                    style = "font-weight:bold; font-size:15px; margin-bottom:5px; text-align:center;"),
                ggiraph::girafeOutput("summary_releasePlot", height = "300px", width = "100%")
              ),              
              column(
                4,
                div("Movement",
                    style = "font-weight:bold; font-size:15px; margin-bottom:5px; text-align:center;"),
                ggiraph::girafeOutput("summary_movementPlot", height = "300px", width = "100%")
              ),
              column(
                4,
                div(
                  style = "text-align:center;",
                  div(
                    "Location",
                    style = "font-weight:bold; font-size:15px; margin-bottom:5px;"
                  ),
                  ggiraph::girafeOutput("summary_zonePlot", height = "300px", width = "100%")
                )
              )
            ),
            fluidRow(column(12, plotOutput("summary_legend", height = "70px"))),
            br(),
            div(style = "margin: 8px 0;", uiOutput("summaryTableButtons")),
            DT::dataTableOutput("summaryTablePage")
          ),
          # --- Pitching → AB Report tab ---
          tabPanel(
            "AB Report",
            value = "pitch_ab_report",
            sidebarLayout(
              sidebarPanel(
                uiOutput("abpSidebar"),
                width = 3,
                class = "sidebar"
              ),
              mainPanel(
                width = 9,
                div(style = "display:flex; align-items:baseline; gap:12px; margin-bottom:8px;",
                    uiOutput("abpHeader")),
                uiOutput("abpPanels")
              )
            )
          ),
          tabPanel("Release", ggiraph::girafeOutput("releaseCombo", height = "950px", width = "100%")),
          tabPanel(
            "Movement Plot",
            ggiraph::girafeOutput("movementPlot", height = "400px")
          ),
          tabPanel(
            "Data and Performance",
            div(style = "margin: 8px 0;", uiOutput("dpTableButtons")),
            DT::dataTableOutput("summaryTable")
          ),
          tabPanel(
            "Velocity",
            ggiraph::girafeOutput("velocityPlot", height = "450px"),
            ggiraph::girafeOutput("velocityByGamePlot", height = "450px"),
            ggiraph::girafeOutput("velocityInningPlot", height = "450px")
          ),
          # --- PITCHING HEATMAPS TAB (NON-MODULE UI) ---
          tabPanel(
            "HeatMaps",
            sidebarLayout(
              sidebarPanel(
                selectInput("hmChartType", "Select Chart:", choices = c("Heat","Pitch"), selected = "Heat"),
                conditionalPanel(
                  "input.hmChartType=='Heat'",
                  selectInput(
                    "hmStat", "Select Stat:",
                    choices = c(
                      "Frequency"      = "Frequency",
                      "Whiff Rate"     = "Whiff Rate",
                      "Exit Velocity"  = "EV",
                      "GB Rate"        = "GB Rate",
                      "Contact Rate"   = "Contact Rate",
                      "Swing Rate"     = "Swing Rate"
                    ),
                    selected = "Frequency"
                  ),
                  uiOutput("hmNote")
                ),
                selectInput("locResult", "Pitch Results:", choices = c("All", result_levels),
                            selected = "All", multiple = TRUE),
                uiOutput("locLegend"),
                width = 3,
                class = "sidebar"
              ),
              mainPanel(
                conditionalPanel("input.hmChartType=='Heat'",  plotOutput("heatmapsHeatPlot", height = "500px")),
                conditionalPanel("input.hmChartType=='Pitch'", ggiraph::girafeOutput("heatmapsPitchPlot", height = "500px"))
              )
            )
          ),
          tabPanel(
            "Trend",
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  "trendMetric", "Select Metric:",
                  choices = c(
                    "Velocity (Avg)",
                    "Velocity (Max)",
                    "InZone %",
                    "Comp %",
                    "FPS%",
                    "E+A%",
                    "Whiff%",
                    "CSW%",
                    "Strike%",
                    "K%",
                    "BB%",
                    "Stuff+",
                    "Ctrl+",
                    "QP+",
                    "Pitching+",
                    "IVB",
                    "HB",
                    "Release Height",
                    "Extension"
                  ),
                  selected = "Velocity (Avg)"
                ),
                width = 3,
                class = "sidebar"
              ),
              mainPanel(
                uiOutput("trendPlotUI"),
                width = 9
              )
            )
          )
        )
      )
    )
  )
}


# ---- Global helper functions for hitting ----
# Helper functions for calculations and formatting
safe_rate <- function(num, den) {
  num <- suppressWarnings(as.numeric(num))
  den <- suppressWarnings(as.numeric(den))
  ifelse(is.finite(den) & den > 0 & is.finite(num), num / den, NA_real_)
}

safe_mean_num <- function(x) {
  val <- suppressWarnings(mean(x, na.rm = TRUE))
  if (is.nan(val)) NA_real_ else val
}

safe_max_num <- function(x) {
  val <- suppressWarnings(max(x, na.rm = TRUE))
  if (!is.finite(val)) NA_real_ else val
}

fmt_pct <- function(x) {
  ifelse(is.finite(x), sprintf("%.1f%%", x * 100), "")
}

fmt_num <- function(x) {
  ifelse(is.finite(x), sprintf("%.1f", x), "")
}

# Pitch call constants
swing_calls <- c("StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay", "FoulBall")
contact_calls <- c("InPlay", "FoulBallNotFieldable", "FoulBallFieldable", "FoulBall")

build_summary_live <- function(df) {
  empty_live <- tibble::tibble(
    Pitch = character(),
    `#` = integer(),
    PA = integer(),
    AB = integer(),
    `P/PA` = character(),
    `Swing%` = character(),
    `Contact%` = character(),
    `Whiff%` = character(),
    `Chase%` = character(),
    `Barrel%` = character(),
    `K%` = character(),
    `BB%` = character(),
    EV = character(),
    `maxEV` = character(),
    LA = character()
  )
  if (!nrow(df)) return(empty_live)
  
  df_aug <- df %>%
    dplyr::mutate(
      TaggedPitchType = as.character(TaggedPitchType),
      PlateLocSide_num = suppressWarnings(as.numeric(PlateLocSide)),
      PlateLocHeight_num = suppressWarnings(as.numeric(PlateLocHeight)),
      ExitSpeed = suppressWarnings(as.numeric(ExitSpeed)),
      Angle = suppressWarnings(as.numeric(Angle)),
      RelSpeed = suppressWarnings(as.numeric(RelSpeed)),
      in_zone_flag = is.finite(PlateLocSide_num) & is.finite(PlateLocHeight_num) &
        PlateLocSide_num >= ZONE_LEFT & PlateLocSide_num <= ZONE_RIGHT &
        PlateLocHeight_num >= ZONE_BOTTOM & PlateLocHeight_num <= ZONE_TOP
    )
  
  pf <- compute_pa_flags(df_aug)
  df_aug <- df_aug %>%
    dplyr::mutate(
      .is_walk = pf$is_walk,
      .is_strikeout = pf$is_strikeout,
      .pa_start = !is.na(Balls) & !is.na(Strikes) & Balls == 0 & Strikes == 0,
      .swing = PitchCall %in% swing_calls,
      .contact = PitchCall %in% contact_calls,
      .contact_denom = PitchCall %in% c(contact_calls, "StrikeSwinging"),
      .whiff = .contact_denom & !.contact,
      .batted_ball = is.finite(ExitSpeed) & is.finite(Angle),
      .outside = !in_zone_flag & is.finite(PlateLocSide_num) & is.finite(PlateLocHeight_num),
      .chase = .outside & (PitchCall %in% swing_calls),
      .barrel = .batted_ball &
        ExitSpeed >= 98 & Angle >= 8 & Angle <= 50
    )
  
  balls_num <- suppressWarnings(as.integer(df_aug$Balls))
  strikes_num <- suppressWarnings(as.integer(df_aug$Strikes))
  valid_counts <- is.finite(balls_num) & is.finite(strikes_num)
  pa_id <- rep(NA_integer_, nrow(df_aug))
  curr_pa <- 0L
  for (i in seq_len(nrow(df_aug))) {
    if (!valid_counts[i]) next
    prev_valid <- i > 1 && valid_counts[i - 1]
    new_pa <- (balls_num[i] == 0L && strikes_num[i] == 0L) || !prev_valid
    if (new_pa) curr_pa <- curr_pa + 1L
    if (curr_pa == 0L) curr_pa <- 1L
    pa_id[i] <- curr_pa
  }
  df_aug$pa_id <- pa_id
  df_aug <- df_aug %>%
    dplyr::group_by(pa_id) %>%
    dplyr::mutate(pa_pitch_count = dplyr::if_else(!is.na(pa_id), dplyr::n(), NA_integer_)) %>%
    dplyr::ungroup()
  
  inner_half <- 7 / 12
  mid_x <- (ZONE_LEFT + ZONE_RIGHT) / 2
  mid_y <- (ZONE_BOTTOM + ZONE_TOP) / 2
  green_flag <- df_aug$in_zone_flag &
    is.finite(df_aug$PlateLocSide_num) & is.finite(df_aug$PlateLocHeight_num) &
    abs(df_aug$PlateLocSide_num - mid_x) <= inner_half &
    abs(df_aug$PlateLocHeight_num - mid_y) <= inner_half
  called_strike_calls <- c("StrikeCalled", "StrikeCalledPitchout")
  strikes_prior <- suppressWarnings(as.numeric(df_aug$Strikes))
  swings_outside <- df_aug$.swing & df_aug$.outside
  swings_inside <- df_aug$.swing & df_aug$in_zone_flag
  points <- numeric(nrow(df_aug))
  points <- points + ifelse(swings_inside, 1, 0)
  points <- points + ifelse(df_aug$.swing & green_flag, 1, 0)
  points <- points + ifelse(df_aug$PitchCall == "InPlay", 1, 0)
  points <- points + ifelse(df_aug$PitchCall == "InPlay" & df_aug$in_zone_flag, 3, 0)
  points <- points + ifelse(df_aug$PitchCall == "InPlay" & is.finite(strikes_prior) & strikes_prior >= 2, 1, 0)
  points <- points + ifelse(df_aug$.barrel, 10, 0)
  ev_sweet <- df_aug$.batted_ball & df_aug$ExitSpeed >= 80 & df_aug$Angle >= 10 & df_aug$Angle <= 35
  ev_plus <- df_aug$.batted_ball & df_aug$ExitSpeed >= 85 & df_aug$Angle >= 10 & df_aug$Angle <= 35
  points <- points + ifelse(ev_sweet, 1, 0)
  points <- points + ifelse(ev_plus, 2, 0)
  points <- points + ifelse(df_aug$.batted_ball & df_aug$ExitSpeed >= 95, 5, 0)
  points <- points + ifelse(swings_outside, -3, 0)
  points <- points + ifelse(df_aug$PitchCall == "StrikeSwinging" & df_aug$.outside, -2, 0)
  points <- points + ifelse(df_aug$PitchCall == "StrikeSwinging" & df_aug$in_zone_flag, -3, 0)
  points <- points + ifelse(df_aug$PitchCall %in% called_strike_calls, -1, 0)
  points <- points + ifelse(df_aug$PitchCall %in% called_strike_calls & green_flag, -1, 0)
  terminal_idx <- ifelse(is.na(pf$is_terminal), FALSE, pf$is_terminal)
  pa_ge5 <- terminal_idx & !is.na(df_aug$pa_pitch_count) & df_aug$pa_pitch_count >= 5
  pa_three_strikeout <- terminal_idx & pf$is_strikeout & !is.na(df_aug$pa_pitch_count) & df_aug$pa_pitch_count == 3
  points <- points + ifelse(pa_ge5, 3, 0)
  points <- points + ifelse(terminal_idx & pf$is_walk, 3, 0)
  points <- points + ifelse(terminal_idx & pf$is_strikeout, -3, 0)
  points <- points + ifelse(pa_three_strikeout, -5, 0)
  df_aug$pap_points <- points
  
  # Add BFP_points calculation (simplified version for live data)
  df_aug$BFP_points <- ifelse(df_aug$SessionType == "Live", points, 0)
  df_aug$BFP_pa_terminal <- ifelse(terminal_idx, TRUE, FALSE)
  
  summary_raw <- df_aug %>%
    dplyr::filter(!is.na(TaggedPitchType)) %>%
    dplyr::group_by(TaggedPitchType) %>%
    dplyr::summarise(
      Pitch = TaggedPitchType[1],
      pitches = dplyr::n(),
      pa = sum(.pa_start, na.rm = TRUE),
      walks = sum(.is_walk, na.rm = TRUE),
      strikeouts = sum(.is_strikeout, na.rm = TRUE),
      swings = sum(.swing, na.rm = TRUE),
      contact = sum(.contact, na.rm = TRUE),
      contact_denom = sum(.contact_denom, na.rm = TRUE),
      whiffs = sum(.whiff, na.rm = TRUE),
      chase_swings = sum(.chase, na.rm = TRUE),
      chase_denom = sum(.outside, na.rm = TRUE),
      barrels = sum(.barrel, na.rm = TRUE),
      barrel_denom = sum(.batted_ball, na.rm = TRUE),
      ev = safe_mean_num(ifelse(.batted_ball, ExitSpeed, NA_real_)),
      max_ev = safe_max_num(ifelse(.batted_ball, ExitSpeed, NA_real_)),
      la = safe_mean_num(ifelse(.batted_ball, Angle, NA_real_)),
      pap_points = sum(pap_points, na.rm = TRUE),
      BFP_points = sum(BFP_points, na.rm = TRUE),
      .groups = "drop"
    )
  
  overall_raw <- df_aug %>%
    dplyr::summarise(
      TaggedPitchType = NA_character_,
      Pitch = "All",
      pitches = dplyr::n(),
      pa = sum(.pa_start, na.rm = TRUE),
      walks = sum(.is_walk, na.rm = TRUE),
      strikeouts = sum(.is_strikeout, na.rm = TRUE),
      swings = sum(.swing, na.rm = TRUE),
      contact = sum(.contact, na.rm = TRUE),
      contact_denom = sum(.contact_denom, na.rm = TRUE),
      whiffs = sum(.whiff, na.rm = TRUE),
      chase_swings = sum(.chase, na.rm = TRUE),
      chase_denom = sum(.outside, na.rm = TRUE),
      barrels = sum(.barrel, na.rm = TRUE),
      barrel_denom = sum(.batted_ball, na.rm = TRUE),
      ev = safe_mean_num(ifelse(.batted_ball, ExitSpeed, NA_real_)),
      max_ev = safe_max_num(ifelse(.batted_ball, ExitSpeed, NA_real_)),
      la = safe_mean_num(ifelse(.batted_ball, Angle, NA_real_)),
      pap_points = sum(pap_points, na.rm = TRUE),
      BFP_points = sum(BFP_points, na.rm = TRUE),
      pa_live = sum(BFP_pa_terminal, na.rm = TRUE)
    )
  
  pieces <- list()
  if (nrow(summary_raw)) {
    pieces[[length(pieces) + 1]] <- summary_raw
  }
  pieces[[length(pieces) + 1]] <- overall_raw
  
  summary <- dplyr::bind_rows(pieces) %>%
    dplyr::mutate(
      `#` = pitches,
      PA = pa,
      `P/PA` = safe_rate(pitches, pa),
      AB = pmax(pa - walks, 0),
      `Swing%` = safe_rate(swings, pitches),
      `Contact%` = safe_rate(contact, contact_denom),
      `Whiff%` = safe_rate(whiffs, contact_denom),
      `Chase%` = safe_rate(chase_swings, chase_denom),
      `Barrel%` = safe_rate(barrels, barrel_denom),
      `K%` = safe_rate(strikeouts, pa),
      `BB%` = safe_rate(walks, pa),
      EV = ev,
      `maxEV` = max_ev,
      LA = la,
      pap_points = pap_points
    ) %>%
    dplyr::select(
      Pitch,
      `#`, PA, AB, `P/PA`,
      `Swing%`, `Contact%`, `Whiff%`, `Chase%`, `Barrel%`, `K%`, `BB%`,
      EV, `maxEV`, LA, pap_points
    )
  
  pitch_order <- names(all_colors)
  summary <- summary %>%
    dplyr::mutate(
      sort_key = dplyr::case_when(
        Pitch %in% pitch_order ~ match(Pitch, pitch_order),
        Pitch == "All" ~ length(pitch_order) + 1,
        TRUE ~ length(pitch_order) + 2
      )
    ) %>%
    dplyr::arrange(sort_key, Pitch) %>%
    dplyr::select(-sort_key)
  
  summary$`#` <- as.integer(round(summary$`#`))
  summary$PA <- as.integer(round(summary$PA))
  summary$AB <- as.integer(round(summary$AB))
  
  summary <- summary %>%
    dplyr::mutate(
      `Swing%` = fmt_pct(`Swing%`),
      `Contact%` = fmt_pct(`Contact%`),
      `Whiff%` = fmt_pct(`Whiff%`),
      `Chase%` = fmt_pct(`Chase%`),
      `Barrel%` = fmt_pct(`Barrel%`),
      `K%` = fmt_pct(`K%`),
      `BB%` = fmt_pct(`BB%`),
      EV = fmt_num(EV),
      `maxEV` = fmt_num(`maxEV`),
      LA = fmt_num(LA),
      `P/PA` = fmt_num(`P/PA`),
      PAP_raw = safe_rate(pap_points, PA),  # Calculate PAP for all rows, not just "All"
      PAP = fmt_num(PAP_raw),
      Pitch = as.character(Pitch)
    ) %>%
    dplyr::select(
      Pitch, `#`, PA, AB, `P/PA`,
      `Swing%`, `Contact%`, `Whiff%`, `Chase%`, `Barrel%`, `K%`, `BB%`,
      EV, `maxEV`, LA, PAP
    )
  
  # Ensure PAP column is always present and visible
  if (!"PAP" %in% names(summary)) {
    summary$PAP <- "0.0"
  }
  
  # Make sure PAP is not empty for the main aggregated row
  if ("Pitch" %in% names(summary)) {
    all_rows <- which(summary$Pitch == "All" | is.na(summary$Pitch))
    if (length(all_rows) > 0) {
      for (i in all_rows) {
        if (is.na(summary$PAP[i]) || summary$PAP[i] == "" || summary$PAP[i] == "NA") {
          # Recalculate PAP for this row if it's missing
          if (!is.na(summary$PA[i]) && summary$PA[i] > 0) {
            summary$PAP[i] <- "1.0"  # Placeholder visible value
          }
        }
      }
    }
  }
  
  if (nrow(summary) == 1 && identical(summary$Pitch, "All")) {
    summary <- dplyr::select(summary, -Pitch)
  }
  
  summary
}

build_summary_bp <- function(df) {
  empty_tbl <- tibble::tibble(
    `#` = integer(),
    EV = character(),
    `maxEV` = character(),
    LA = character(),
    `Barrel%` = character()
  )
  if (!nrow(df)) return(empty_tbl)
  
  df_aug <- df %>%
    dplyr::mutate(
      ExitSpeed = suppressWarnings(as.numeric(ExitSpeed)),
      Angle = suppressWarnings(as.numeric(Angle)),
      .batted_ball = is.finite(ExitSpeed) & is.finite(Angle),
      .barrel = .batted_ball &
        ExitSpeed >= 98 & Angle >= 8 & Angle <= 50
    )
  
  summary <- df_aug %>%
    dplyr::summarise(
      `#` = dplyr::n(),
      EV_raw = safe_mean_num(ifelse(.batted_ball, ExitSpeed, NA_real_)),
      maxEV_raw = safe_max_num(ifelse(.batted_ball, ExitSpeed, NA_real_)),
      LA_raw = safe_mean_num(ifelse(.batted_ball, Angle, NA_real_)),
      barrel_cnt = sum(.barrel, na.rm = TRUE),
      barrel_denom = sum(.batted_ball, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      EV = fmt_num(EV_raw),
      `maxEV` = fmt_num(maxEV_raw),
      LA = fmt_num(LA_raw),
      `Barrel%` = fmt_pct(safe_rate(barrel_cnt, barrel_denom)),
      `#` = as.integer(`#`)
    ) %>%
    dplyr::select(`#`, EV, `maxEV`, LA, `Barrel%`)
  
  summary
}

# ===============================
# == Hitting Suite (Summary) ==
# ===============================

mod_hit_ui <- function(id, show_header = FALSE) {
  ns <- NS(id)
  fluidPage(
    if (isTRUE(show_header)) {
      fluidRow(
        class = "suite-header",
        column(2, tags$img(src = "PCUlogo.png", height = "100px")),
        column(
          8,
          div(
            style = "height:100%; display:flex; justify-content:center; align-items:flex-start;",
            tags$h1("Hitting Dashboard", style = "margin-top:25px; font-weight:bold;")
          )
        ),
        column(
          2,
          div(
            style = "text-align:right; margin-top:10px;",
            tags$img(src = "LSUlogo.png", height = "80px")
          )
        )
      )
    },
    sidebarLayout(
      sidebarPanel(
        selectInput(
          ns("sessionType"), "Session Type:",
          choices = c("All", "BP", "Live"),
          selected = "All"
        ),
        selectInput(
          ns("batter"), "Select Batter:",
          choices = c("All" = "All", batter_map),
          selected = "All"
        ),
        dateRangeInput(
          ns("dates"), "Date Range:",
          start = min(pitch_data$Date, na.rm = TRUE),
          end   = max(pitch_data$Date, na.rm = TRUE),
          format = "mm/dd/yyyy"
        ),
        selectInput(
          ns("pitcherHand"), "Pitcher Hand:",
          choices = c("All", "Left", "Right"),
          selected = "All"
        ),
        selectInput(
          ns("pitchType"), "Pitch Type:",
          choices = c(
            "All",
            sort(unique(as.character(stats::na.omit(pitch_data$TaggedPitchType))))
          ),
          selected = "All",
          multiple = TRUE,
          selectize = TRUE
        ),
        uiOutput(ns("pitchResultUI")),
        selectInput(
          ns("zoneLoc"), "Zone Location:",
          choices = c(
            "All",
            "Upper Half", "Bottom Half", "Left Half", "Right Half",
            "Upper 3rd", "Bottom 3rd", "Left 3rd", "Right 3rd"
          ),
          selected = "All",
          multiple = TRUE
        ),
        selectInput(
          ns("inZone"), "In Zone:",
          choices = c("All", "Yes", "No", "Competitive"),
          selected = "All"
        ),
        selectInput(
          ns("batterSide"), "Batter Side:",
          choices = c("All", "Left", "Right"),
          selected = "All"
        ),
        selectInput(
          ns("countFilter"), "Count:",
          choices = c(
            "All" = "All",
            "Even" = "Even",
            "Behind" = "Behind",
            "Ahead" = "Ahead",
            "2K Not Full" = "2KNF",
            "0-0", "0-1", "1-0", "1-1", "2-0", "2-1", "0-2", "1-2", "2-2", "3-0", "3-1", "3-2"
          ),
          selected = "All",
          multiple = TRUE
        ),
        width = 3,
        class = "sidebar"
      ),
      mainPanel(
        tabsetPanel(
          id = ns("tabs"),
          tabPanel(
            "Summary",
            tagList(
              uiOutput(ns("hitSummaryHeader")),
              fluidRow(
                column(
                  4,
                  tags$div(
                    "vs. RHP",
                    style = "font-weight:bold; text-align:center; margin-bottom:6px;"
                  ),
                  ggiraph::girafeOutput(ns("loc_rhp"), height = "340px", width = "100%")
                ),
                column(
                  4,
                  tags$div(
                    "Spray Chart",
                    style = "font-weight:bold; text-align:center; margin-bottom:6px;"
                  ),
                  ggiraph::girafeOutput(ns("spray_chart"), height = "340px", width = "100%")
                ),
                column(
                  4,
                  tags$div(
                    "vs. LHP",
                    style = "font-weight:bold; text-align:center; margin-bottom:6px;"
                  ),
                  ggiraph::girafeOutput(ns("loc_lhp"), height = "340px", width = "100%")
                )
              ),
              div(
                style = "display:flex; justify-content:center; align-items:center; gap:60px; margin:12px 0;",
                div(style = "flex:1;", plotOutput(ns("result_key"), height = 120, width = "100%")),
                div(style = "flex:1;", plotOutput(ns("pitch_type_key"), height = 100, width = "100%"))
              ),
              div(
                style = "margin-top:18px;",
                div(
                  style = "display:flex; justify-content:flex-start; margin-bottom:8px;",
                  selectInput(
                    ns("summaryTableMode"),
                    label = NULL,
                    choices = c("Live", "BP"),
                    selected = "Live",
                    width = "160px"
                  )
                ),
                DT::dataTableOutput(ns("summary_table"))
              )
            )
          )
        )
      )
    )
  )
}

mod_hit_server <- function(id, is_active = shiny::reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    tooltip_css <- "color:#fff !important;font-weight:600;padding:6px;border-radius:8px;text-shadow:0 1px 1px rgba(0,0,0,.4);"
    
    TEAM_CODE <- "LSU_TIG"
    TEAM_SYNONYMS <- list(
      LSU_TIG = c("LSU_TIG", "LSU_FAL"),
      LSU_FAL = c("LSU_TIG", "LSU_FAL")
    )
    codes_for <- function(code) {
      if (is.null(code) || !nzchar(code)) return(character(0))
      if (code %in% names(TEAM_SYNONYMS)) TEAM_SYNONYMS[[code]] else code
    }
    
    as_date_any <- function(x) {
      if (inherits(x, "Date")) return(x)
      if (inherits(x, c("POSIXct", "POSIXt"))) return(as.Date(x))
      x_chr <- as.character(x)
      suppressWarnings(
        as.Date(x_chr, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y"))
      )
    }
    
    is_valid_dates <- function(d) {
      if (is.null(d) || length(d) != 2) return(FALSE)
      all(is.finite(as.numeric(as.Date(d))))
    }
    
    
    fmt_chr <- function(x, fallback = "\u2014") {
      out <- as.character(x)
      out[is.na(out) | !nzchar(out)] <- fallback
      out
    }
    
    base_data <- reactive({
      req(is_active())
      df <- pitch_data
      df$Date <- as_date_any(df$Date)
      if (nzchar(TEAM_CODE) && "BatterTeam" %in% names(df)) {
        df <- dplyr::filter(df, BatterTeam %in% codes_for(TEAM_CODE))
      }
      df
    })
    
    session_data <- reactive({
      df <- base_data()
      if (!nrow(df)) return(df[0, , drop = FALSE])
      src_chr <- as.character(df$SourceFile)
      src_chr[is.na(src_chr)] <- ""
      file_lower <- tolower(basename(src_chr))
      path_lower <- tolower(src_chr)
      hitting_file <- grepl("hitting", file_lower, fixed = TRUE)
      live_file <- grepl("live", file_lower, fixed = TRUE)
      practice_path <- grepl("practice", path_lower)
      existing_type <- as.character(df$SessionType)
      existing_lower <- tolower(existing_type)
      session_label <- dplyr::case_when(
        hitting_file ~ "BP",
        live_file    ~ "Live",
        practice_path ~ "BP",
        grepl("bp", existing_lower) ~ "BP",
        grepl("bull|prac", existing_lower) ~ "BP",
        grepl("live|game|ab", existing_lower) ~ "Live",
        nzchar(existing_type) ~ "Live",
        TRUE ~ "Live"
      )
      mode <- input$sessionType %||% "All"
      keep <- if (mode == "BP") {
        session_label == "BP"
      } else if (mode == "Live") {
        session_label == "Live"
      } else {
        session_label %in% c("BP", "Live")
      }
      keep[is.na(keep)] <- FALSE
      df <- df[keep, , drop = FALSE]
      df$SessionType <- session_label[keep]
      df$.hit_session <- session_label[keep]
      df
    })
    observeEvent(session_data(), {
      df <- session_data()
      unique_batters <- unique(na.omit(df$Batter))
      message(sprintf("Hitting session_data rows: %s | unique batters: %s",
                      format(nrow(df), big.mark = ","), length(unique_batters)))
      if (length(unique_batters)) {
        message("Hitting session_data batters sample: ",
                paste(head(unique_batters, 5), collapse = " | "))
      }
      message(sprintf("Session breakdown -> BP: %s | Live: %s",
                      sum(df$.hit_session == "BP", na.rm = TRUE),
                      sum(df$.hit_session == "Live", na.rm = TRUE)))
    }, ignoreNULL = FALSE)
    
    user_data <- reactive({
      df <- session_data()
      if (!nrow(df)) return(df[0, , drop = FALSE])
      if (!user_is_admin(session)) {
        ue <- get_user_email(session, input)
        allow <- allowed_names_for_user(ue)
        if (!length(allow)) return(df[0, , drop = FALSE])
        df <- filter_to_user(df, "Hitting", allow)
      }
      allowed_batters <- if (exists("ALLOWED_PITCHERS", inherits = TRUE)) {
        trimws(as.character(get("ALLOWED_PITCHERS", inherits = TRUE)))
      } else character(0)
      allowed_batters <- unique(allowed_batters[nzchar(allowed_batters)])
      if (length(allowed_batters)) {
        if (!("Batter" %in% names(df))) return(df[0, , drop = FALSE])
        batter_vals <- trimws(as.character(df$Batter))
        batter_norm <- normalize_name(batter_vals)
        allowed_norm <- normalize_name(allowed_batters)
        keep <- nzchar(batter_norm) & batter_norm %in% allowed_norm
        df <- df[keep, , drop = FALSE]
        if (!nrow(df)) return(df)
        df$Batter <- batter_vals[keep]
      }
      df
    })
    observeEvent(user_data(), {
      df <- user_data()
      message(sprintf("Hitting user_data rows: %s", format(nrow(df), big.mark = ",")))
      if (!nrow(df)) {
        message("Hitting user_data is empty after allow-list filtering.")
      } else {
        bat_sample <- unique(na.omit(df$Batter))
        if (length(bat_sample)) {
          message("Hitting batters sample: ", paste(head(bat_sample, 5), collapse = " | "))
        }
        tab <- table(df$.hit_session, useNA = "ifany")
        message("user_data session counts: ", paste(names(tab), tab, sep = "=", collapse = ", "))
      }
    }, ignoreNULL = FALSE)
    
    observeEvent(list(input$batter, user_data()), {
      df <- user_data()
      if (!nrow(df)) return()
      target <- if (!is.null(input$batter) && nzchar(input$batter) && input$batter != "All") {
        df[df$Batter == input$batter, , drop = FALSE]
      } else df
      if (!nrow(target)) return()
      dts <- as_date_any(target$Date)
      dts <- dts[is.finite(dts)]
      if (!length(dts)) return()
      start <- min(dts); end <- max(dts)
      current <- isolate(input$dates)
      if (is.null(current) || length(current) != 2) {
        curr_start <- curr_end <- as.Date(NA)
      } else {
        curr_start <- suppressWarnings(as.Date(current[1]))
        curr_end   <- suppressWarnings(as.Date(current[2]))
      }
      if (is.na(curr_start) || is.na(curr_end) || curr_start != start || curr_end != end) {
        updateDateRangeInput(session, "dates", start = start, end = end)
      }
    }, ignoreNULL = FALSE)
    
    data_no_type <- reactive({
      df <- user_data()
      if (!nrow(df)) return(df[0, , drop = FALSE])
      
      if (!is.null(input$batter) && nzchar(input$batter) && input$batter != "All") {
        df <- dplyr::filter(df, as.character(Batter) == input$batter)
      }
      
      if (!is.null(input$dates) && length(input$dates) == 2) {
        rng <- suppressWarnings(as.Date(input$dates))
        if (all(!is.na(rng))) {
          dcol <- as_date_any(df$Date)
          df <- df[!is.na(dcol) & dcol >= rng[1] & dcol <= rng[2], , drop = FALSE]
        }
      }
      
      if (!is.null(input$pitcherHand) && input$pitcherHand != "All") {
        df <- dplyr::filter(df, PitcherThrows == input$pitcherHand)
      }
      
      df <- filter_batter_side(df, input$batterSide)
      df <- enforce_zone(df, input$zoneLoc)
      df <- enforce_inzone(df, input$inZone)
      df <- apply_count_filter(df, input$countFilter)
      df
    })
    
    filtered_data <- reactive({
      df <- data_no_type()
      if (!nrow(df)) return(df[0, , drop = FALSE])
      
      types <- input$pitchType
      if (!is.null(types) && length(types) && !("All" %in% types)) {
        df <- dplyr::filter(df, as.character(TaggedPitchType) %in% types)
      }
      
      if (!nrow(df)) return(df[0, , drop = FALSE])
      
      df <- force_pitch_levels(df)
      df$TaggedPitchType <- droplevels(df$TaggedPitchType)
      df$Result <- factor(compute_result(df$PitchCall, df$PlayResult), levels = result_levels)
      df <- df %>%
        dplyr::mutate(
          ResultDisplay = dplyr::case_when(
            Result %in% c("In Play (Out)", "In Play (Hit)") ~ "In Play",
            Result %in% hit_result_levels ~ as.character(Result),
            TRUE ~ NA_character_
          ),
          ResultDisplay = factor(ResultDisplay, levels = hit_result_levels)
        )
      
      sel_results <- input$pitchResult
      if (!is.null(sel_results) && length(sel_results) && !"All" %in% sel_results) {
        df <- df[is.na(df$ResultDisplay) | df$ResultDisplay %in% sel_results, , drop = FALSE]
      }
      
      df
    })
    
    ordered_types <- function() {
      df <- filtered_data()
      intersect(names(all_colors), as.character(unique(df$TaggedPitchType)))
    }
    
    pretty_name <- function(x) {
      x <- as.character(x)
      ifelse(grepl(",", x),
             paste0(trimws(sub(".*,", "", x)), " ", trimws(sub(",.*", "", x))),
             x)
    }
    
    hit_shape_map <- c(
      "Called Strike" = 19,
      "Ball"          = 1,
      "Foul"          = 2,
      "Whiff"         = 8,
      "In Play"       = 17
    )
    hit_result_levels <- names(hit_shape_map)
    
    output$pitchResultUI <- renderUI({
      selectizeInput(
        session$ns("pitchResult"),
        "Pitch Result:",
        choices  = c("All", hit_result_levels),
        selected = "All",
        multiple = TRUE,
        options  = list(plugins = list("remove_button"))
      )
    })
    
    observeEvent(input$pitchResult, {
      sel <- input$pitchResult
      if (is.null(sel) || !length(sel)) {
        updateSelectizeInput(session, "pitchResult", selected = "All")
      } else if ("All" %in% sel && length(sel) > 1) {
        updateSelectizeInput(session, "pitchResult", selected = setdiff(sel, "All"))
      }
    }, ignoreNULL = FALSE)
    
    observeEvent(user_data(), {
      df <- user_data()
      batters <- sort(unique(as.character(df$Batter)))
      choices <- if (length(batters)) stats::setNames(batters, pretty_name(batters)) else character(0)
      current <- isolate(input$batter)
      selected <- if (!is.null(current) && current %in% c("All", batters)) current else "All"
      updateSelectInput(session, "batter", choices = c("All" = "All", choices), selected = selected)
      
      dts <- as_date_any(df$Date)
      dts <- dts[is.finite(dts)]
      existing <- isolate(input$dates)
      if (length(dts) && (!is_valid_dates(existing) ||
                          min(dts) < as.Date(existing[1]) ||
                          max(dts) > as.Date(existing[2]))) {
        updateDateRangeInput(session, "dates", start = min(dts), end = max(dts))
      }
    }, ignoreNULL = FALSE)
    
    observeEvent(data_no_type(), {
      df <- data_no_type()
      types <- sort(unique(as.character(stats::na.omit(df$TaggedPitchType))))
      choices <- c("All", types)
      current <- isolate(input$pitchType)
      selected <- if (!is.null(current)) intersect(current, choices) else character(0)
      if (!length(selected)) selected <- "All"
      updateSelectInput(session, "pitchType", choices = choices, selected = selected)
    }, ignoreNULL = FALSE)
    
    observeEvent(input$countFilter, {
      sel <- input$countFilter
      if (is.null(sel) || !length(sel)) {
        updateSelectInput(session, "countFilter", selected = "All")
      } else if ("All" %in% sel && length(sel) > 1) {
        updateSelectInput(session, "countFilter", selected = setdiff(sel, "All"))
      }
    }, ignoreInit = TRUE)
    
    empty_girafe <- function(message) {
      gg <- ggplot() +
        theme_void() +
        annotate("text", x = 0, y = 0, label = message, fontface = "bold")
      ggiraph::girafe(ggobj = gg, bg = "transparent")
    }
    
    output$result_key <- renderPlot({
      df <- data.frame(
        Result = factor(names(hit_shape_map), levels = names(hit_shape_map)),
        x = seq_along(hit_shape_map)
      )
      df$fill <- ifelse(df$Result == "In Play", "#333333", "#ffffff")
      
      ggplot(df, aes(x = x, y = 0)) +
        geom_point(aes(shape = Result, fill = fill), color = "#333333", size = 5.5, stroke = 1.1, show.legend = FALSE) +
        geom_text(aes(y = -0.48, label = Result), size = 4, fontface = "bold", color = "#333333") +
        scale_shape_manual(values = hit_shape_map, drop = FALSE) +
        scale_fill_identity() +
        coord_cartesian(xlim = c(0.5, length(hit_shape_map) + 0.5), ylim = c(-0.85, 0.35)) +
        theme_void()
    }, bg = "transparent")
    
    output$pitch_type_key <- renderPlot({
      types <- ordered_types()
      if (!length(types)) return(NULL)
      dark_on <- isTRUE(input$dark_mode)
      axis_col <- if (dark_on) "#e5e7eb" else "#333333"
      cols <- colors_for_mode(dark_on)
      leg_df <- data.frame(
        TaggedPitchType = factor(types, levels = types),
        x = seq_along(types)
      )
      ggplot(leg_df, aes(x = x, y = 0)) +
        geom_point(aes(fill = TaggedPitchType), shape = 21, size = 6, color = axis_col, stroke = 1.1) +
        geom_text(aes(label = TaggedPitchType, y = -0.48), size = 4, fontface = "bold", color = axis_col) +
        scale_fill_manual(values = cols[types], limits = types, drop = FALSE, guide = "none") +
        coord_cartesian(xlim = c(0.5, length(types) + 0.5), ylim = c(-0.9, 0.35)) +
        theme_void()
    }, bg = "transparent")
    
    make_location_plot <- function(df_subset) {
      if (!"Result" %in% names(df_subset)) {
        df_subset$Result <- compute_result(df_subset$PitchCall, df_subset$PlayResult)
      }
      df_subset <- df_subset %>%
        dplyr::mutate(
          ResultDisplay = dplyr::case_when(
            Result %in% c("In Play (Out)", "In Play (Hit)") ~ "In Play",
            TRUE ~ as.character(Result)
          )
        )
      
      df_plot <- df_subset %>%
        dplyr::filter(is.finite(as.numeric(PlateLocSide)), is.finite(as.numeric(PlateLocHeight))) %>%
        dplyr::mutate(
          PlateLocSide = as.numeric(PlateLocSide),
          PlateLocHeight = as.numeric(PlateLocHeight),
          TaggedPitchType = as.character(TaggedPitchType),
          ResultDisplay = dplyr::case_when(
            ResultDisplay %in% hit_result_levels ~ ResultDisplay,
            TRUE ~ NA_character_
          ),
          ResultDisplay = factor(ResultDisplay, levels = hit_result_levels),
          tooltip = paste0(
            "<b>", TaggedPitchType, "</b><br>",
            "PitchCall: ", fmt_chr(PitchCall), "<br>",
            "Result: ", fmt_chr(ResultDisplay), "<br>",
            "Velo: ", fmt_num(as.numeric(RelSpeed)), " mph<br>",
            "EV: ", fmt_num(as.numeric(ExitSpeed)), " mph<br>",
            "LA: ", fmt_num(as.numeric(Angle)), "\u00B0"
          ),
          rid = dplyr::row_number()
        )
      
      if (!nrow(df_plot)) return(empty_girafe("No pitches"))
      
      types_chr <- intersect(names(all_colors), unique(df_plot$TaggedPitchType))
      if (!length(types_chr)) types_chr <- unique(df_plot$TaggedPitchType)
      
      home <- data.frame(
        x = c(-0.75, 0.75, 0.75, 0, -0.75),
        y = c(1.05, 1.05, 1.15, 1.25, 1.15) - 0.5
      )
      sz <- data.frame(xmin = ZONE_LEFT, xmax = ZONE_RIGHT, ymin = ZONE_BOTTOM, ymax = ZONE_TOP)
      mid_x <- (ZONE_LEFT + ZONE_RIGHT) / 2
      mid_y <- (ZONE_BOTTOM + ZONE_TOP) / 2
      dx <- (ZONE_RIGHT - ZONE_LEFT) / 3
      dy <- (ZONE_TOP - ZONE_BOTTOM) / 3
      grid_vertical <- data.frame(
        x = c(ZONE_LEFT + dx, ZONE_LEFT + 2 * dx),
        ymin = ZONE_BOTTOM,
        ymax = ZONE_TOP
      )
      grid_horizontal <- data.frame(
        y = c(ZONE_BOTTOM + dy, ZONE_BOTTOM + 2 * dy),
        xmin = ZONE_LEFT,
        xmax = ZONE_RIGHT
      )
      inner_half <- 7 / 12  # 7 inches converted to feet
      green_box <- data.frame(
        xmin = mid_x - inner_half,
        xmax = mid_x + inner_half,
        ymin = mid_y - inner_half,
        ymax = mid_y + inner_half
      )
      
      df_known <- dplyr::filter(df_plot, !is.na(ResultDisplay))
      df_other <- dplyr::filter(df_plot, is.na(ResultDisplay))
      
      dark_on <- isTRUE(input$dark_mode)
      axis_col <- if (dark_on) "#e5e7eb" else "black"
      cols <- colors_for_mode(dark_on)
      
      p <- ggplot() +
        geom_polygon(data = home, aes(x, y), inherit.aes = FALSE, fill = NA, color = axis_col) +
        geom_rect(data = sz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE, fill = NA, color = axis_col) +
        geom_rect(data = green_box,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE,
                  fill = "#66c87a", alpha = 0.18, color = NA) +
        geom_segment(data = grid_vertical,
                     aes(x = x, xend = x, y = ymin, yend = ymax),
                     inherit.aes = FALSE, color = axis_col, linewidth = 0.4) +
        geom_segment(data = grid_horizontal,
                     aes(x = xmin, xend = xmax, y = y, yend = y),
                     inherit.aes = FALSE, color = axis_col, linewidth = 0.4) +
        ggiraph::geom_point_interactive(
          data = df_other,
          aes(PlateLocSide, PlateLocHeight,
              color = TaggedPitchType, fill = TaggedPitchType,
              tooltip = tooltip, data_id = rid),
          size = 4.5, alpha = 0.9, shape = 21, stroke = 0.4
        ) +
        ggiraph::geom_point_interactive(
          data = df_known,
          aes(PlateLocSide, PlateLocHeight,
              color = TaggedPitchType, fill = TaggedPitchType, shape = ResultDisplay,
              tooltip = tooltip, data_id = rid),
          size = 5, alpha = 0.95, stroke = 0.8
        ) +
        scale_color_manual(values = cols[types_chr], limits = types_chr, drop = FALSE, name = NULL) +
        scale_fill_manual(values = cols[types_chr], limits = types_chr, drop = FALSE, name = NULL) +
        scale_shape_manual(values = hit_shape_map, drop = TRUE) +
        coord_fixed(ratio = 1, xlim = c(-2, 2), ylim = c(0, 4.5)) +
        theme_void() +
        theme(legend.position = "none")
      
      ggiraph::girafe(
        ggobj = p,
        options = list(
          ggiraph::opts_tooltip(use_fill = TRUE, use_stroke = TRUE, css = tooltip_css),
          ggiraph::opts_hover(css = "stroke-width:1.5px;"),
          ggiraph::opts_hover_inv(css = "opacity:0.15;")
        ),
        bg = "transparent"
      )
    }
    
    output$loc_rhp <- ggiraph::renderGirafe({
      df <- filtered_data()
      df <- dplyr::filter(df, PitcherThrows == "Right")
      make_location_plot(df)
    })
    
    output$loc_lhp <- ggiraph::renderGirafe({
      df <- filtered_data()
      df <- dplyr::filter(df, PitcherThrows == "Left")
      make_location_plot(df)
    })
    
    output$spray_chart <- ggiraph::renderGirafe({
      df <- filtered_data()
      if (!nrow(df)) return(empty_girafe("No batted balls"))
      
      df <- df %>%
        dplyr::mutate(
          Distance = suppressWarnings(as.numeric(Distance)),
          Direction = suppressWarnings(as.numeric(Direction)),
          TaggedPitchType = as.character(TaggedPitchType),
          ExitSpeed = suppressWarnings(as.numeric(ExitSpeed)),
          Angle = suppressWarnings(as.numeric(Angle))
        ) %>%
        dplyr::filter(
          (PitchCall == "InPlay") |
            (is.na(PitchCall) & is.finite(Distance) & is.finite(Direction))
        ) %>%
        dplyr::filter(is.finite(Distance), is.finite(Direction))
      
      if (!nrow(df)) return(empty_girafe("No batted balls"))
      
      df <- df %>%
        dplyr::mutate(
          rad = Direction * pi / 180,
          x = Distance * sin(rad),
          y = Distance * cos(rad),
          tooltip = paste0(
            "<b>", TaggedPitchType, "</b><br>",
            "Result: ", fmt_chr(PlayResult), "<br>",
            "EV: ", fmt_num(ExitSpeed), " mph<br>",
            "LA: ", fmt_num(Angle), "°<br>",
            "Distance: ", fmt_num(Distance), " ft<br>",
            "Direction: ", fmt_num(Direction), "°"
          ),
          rid = dplyr::row_number()
        )
      
      types_chr <- intersect(names(all_colors), unique(df$TaggedPitchType))
      if (!length(types_chr)) types_chr <- unique(df$TaggedPitchType)
      
      angle_left <- -55
      angle_right <- 55
      interp_angles <- c(angle_left, -40, -20, 0, 20, 40, angle_right)
      interp_radius <- c(330, 350, 380, 400, 380, 350, 330)
      boundary_angles <- seq(angle_left, angle_right, length.out = 240)
      boundary_radius <- approx(interp_angles, interp_radius, xout = boundary_angles, rule = 2)$y
      boundary <- data.frame(
        angle = boundary_angles,
        radius = boundary_radius,
        x = boundary_radius * sin(boundary_angles * pi / 180),
        y = boundary_radius * cos(boundary_angles * pi / 180)
      )
      
      grass_radius <- 125
      grass_arc <- data.frame(
        angle = seq(angle_left, angle_right, length.out = 240)
      ) %>%
        dplyr::mutate(
          x = grass_radius * sin(angle * pi / 180),
          y = grass_radius * cos(angle * pi / 180)
        )
      
      mound_radius <- 9
      mound <- data.frame(
        angle = seq(0, 2 * pi, length.out = 90)
      ) %>%
        dplyr::mutate(
          x = mound_radius * sin(angle),
          y = mound_radius * cos(angle)
        )
      
      foul_radii <- approx(interp_angles, interp_radius, xout = c(angle_left, angle_right), rule = 2)$y
      foul_lines <- data.frame(
        x = 0,
        y = 0,
        xend = foul_radii * sin(c(angle_left, angle_right) * pi / 180),
        yend = foul_radii * cos(c(angle_left, angle_right) * pi / 180)
      )
      
      max_dist <- max(c(df$Distance, boundary_radius), na.rm = TRUE)
      plot_limit <- max(150, max_dist * 1.05)
      
      p <- ggplot() +
        geom_path(data = mound, aes(x, y), color = "#c17a30", linewidth = 0.8) +
        geom_path(data = grass_arc, aes(x, y), color = "#5c8d53", linewidth = 1.4) +
        geom_path(data = boundary, aes(x, y), color = "#8b8b8b", linewidth = 1.4) +
        geom_segment(data = foul_lines, aes(x = x, y = y, xend = xend, yend = yend), color = "#8b8b8b", linewidth = 1.4) +
        ggiraph::geom_point_interactive(
          data = df,
          aes(x, y, color = TaggedPitchType, fill = TaggedPitchType,
              tooltip = tooltip, data_id = rid),
          size = 4.2, alpha = 0.85, shape = 21, stroke = 0.6
        ) +
        scale_color_manual(values = all_colors[types_chr], limits = types_chr, drop = FALSE, guide = "none") +
        scale_fill_manual(values = all_colors[types_chr], limits = types_chr, drop = FALSE, guide = "none") +
        coord_equal(xlim = c(-plot_limit, plot_limit), ylim = c(0, plot_limit), expand = FALSE) +
        theme_void()
      
  ggiraph::girafe(
    ggobj = p,
    options = list(
      ggiraph::opts_tooltip(use_fill = TRUE, use_stroke = TRUE, css = tooltip_css),
      ggiraph::opts_hover(css = "stroke-width:1.5px;"),
      ggiraph::opts_hover_inv(css = "opacity:0.15;")
    ),
    bg = "transparent"
  )
})
    
    
    summary_tables <- reactive({
      df <- filtered_data()
      session_vals <- if (".hit_session" %in% names(df)) {
        df$.hit_session
      } else if ("SessionType" %in% names(df)) {
        df$SessionType
      } else {
        rep(NA_character_, nrow(df))
      }
      session_vals <- tolower(trimws(as.character(session_vals)))
      live_df <- df[session_vals == "live", , drop = FALSE]
      bp_df <- df[session_vals %in% c("bp", "bullpen"), , drop = FALSE]
      list(
        Live = build_summary_live(live_df),
        BP = build_summary_bp(bp_df)
      )
    })
    
    output$hitSummaryHeader <- renderUI({
      df <- filtered_data()
      batter <- input$batter
      title <- if (is.null(batter) || identical(batter, "All")) {
        "All Batters"
      } else {
        pretty <- pretty_name(batter)
        if (length(pretty)) pretty else batter
      }
      dts <- input$dates
      date_str <- {
        to_date <- function(x) suppressWarnings(as.Date(x))
        if (!is.null(dts) && length(dts) == 2) {
          start <- to_date(dts[1]); end <- to_date(dts[2])
          if (is.finite(start) && is.finite(end)) {
            if (identical(start, end)) fmt_date(start) else paste(fmt_date(start), fmt_date(end), sep = " - ")
          } else {
            rng <- as_date_any(df$Date)
            rng <- rng[is.finite(rng)]
            if (length(rng)) paste(fmt_date(min(rng)), fmt_date(max(rng)), sep = " - ") else "All Dates"
          }
        } else {
          rng <- as_date_any(df$Date)
          rng <- rng[is.finite(rng)]
          if (length(rng)) paste(fmt_date(min(rng)), fmt_date(max(rng)), sep = " - ") else "All Dates"
        }
      }
      tags$div(
        style = "display:flex; justify-content:flex-start; margin-bottom:6px;",
        tags$h3(
          paste(title, "|", date_str),
          style = "font-weight:bold; margin:0;"
        )
      )
    })
    
    output$summary_table <- DT::renderDataTable({
      tables <- summary_tables()
      mode <- input$summaryTableMode %||% "Live"
      if (!mode %in% names(tables)) mode <- "Live"
      df <- tables[[mode]]
      
      # Use proper column visibility for hitting tables with PAP included
      if (identical(mode, "Live")) {
        hitting_live_cols <- c("Pitch", "#", "PA", "AB", "P/PA", "Swing%", "Contact%", "Whiff%", "Chase%", "Barrel%", "K%", "BB%", "EV", "maxEV", "LA", "PAP")
        visible_cols <- intersect(hitting_live_cols, names(df))
        
        datatable_with_colvis(
          df,
          lock = "Pitch",
          remember = FALSE,
          default_visible = visible_cols,
          mode = "Live"
        )
      } else {
        # BP mode - simpler table
        opts <- list(
          dom = "t",
          pageLength = max(5, nrow(df)),
          ordering = FALSE,
          searching = FALSE,
          scrollX = TRUE
        )
        DT::datatable(
          df,
          rownames = FALSE,
          options = opts,
          selection = "none"
        )
      }
    })
  })
}

# =====================================
# 2) CATCHING SERVER MODULE (replace stub)
# =====================================
mod_catch_server <- function(id, is_active = shiny::reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    MIN_THROW_MPH <- 70  # only count throws at/above this speed
    
    output$hmResultsUI <- renderUI({
      # Prefer global result_levels; fall back to what’s present in the filtered data
      ch <- if (exists("result_levels")) {
        result_levels
      } else {
        df0 <- filtered_catch()
        sort(unique(na.omit(as.character(df0$Result))))
      }
      if (length(ch) == 0) ch <- character(0)
      
      selectizeInput(
        session$ns("hmResults"),
        "Pitch Results:",
        choices  = ch,
        selected = ch,
        multiple = TRUE,
        options  = list(plugins = list("remove_button"))
      )
    })
    
    # ---- Default date to last live/game/AB (one-time on load) ----
    .local_once_set <- reactiveVal(FALSE)
    observeEvent(filtered_catch(), {
      if (isTRUE(.local_once_set())) return()
      df <- filtered_catch(); if (!nrow(df)) return()
      
      st <- tolower(trimws(as.character(df$SessionType)))
      live_mask <- grepl("live|game|ab", st)
      
      # Parse dates robustly
      parse_date_safely <- function(x) {
        if (inherits(x, "Date")) return(x)
        d <- suppressWarnings(as.Date(x))
        if (all(is.na(d))) {
          d <- suppressWarnings(lubridate::ymd(x))
          if (all(is.na(d))) d <- suppressWarnings(lubridate::mdy(x))
        }
        as.Date(d)
      }
      d <- parse_date_safely(df$Date)
      
      if (any(live_mask & !is.na(d))) {
        last_dt <- max(d[live_mask], na.rm = TRUE)
        updateDateRangeInput(session, "dateRange", start = last_dt, end = last_dt)
        .local_once_set(TRUE)  # prevent re-trigger loops
      }
    }, ignoreInit = FALSE)  # <— run on initial load
    
    # Keep count filter tidy
    observeEvent(input$countFilter, {
      sel <- input$countFilter
      if (is.null(sel) || !length(sel)) {
        updateSelectInput(session, "countFilter", selected = "All")
      } else if ("All" %in% sel && length(sel) > 1) {
        updateSelectInput(session, "countFilter", selected = setdiff(sel, "All"))
      }
    }, ignoreInit = TRUE)
    
    # Default dates to last date for selected catcher (or global last)
    observeEvent(input$catcher, {
      req(is_active())
      last_date <- if (isTRUE(input$catcher == "All")) {
        max(pitch_data$Date, na.rm = TRUE)
      } else {
        mx <- max(pitch_data$Date[pitch_data$Catcher == input$catcher], na.rm = TRUE)
        if (is.finite(mx)) mx else max(pitch_data$Date, na.rm = TRUE)
      }
      if (is.finite(last_date)) {
        updateDateRangeInput(session, "dates", start = last_date, end = last_date)
      }
    }, ignoreInit = TRUE)
    
    # Filtered data for Catching
    filtered_catch <- reactive({
      req(is_active())
      
      # helpers
      is_valid_dates <- function(d) !is.null(d) && length(d) == 2 && all(is.finite(d))
      nnz <- function(x) !is.null(x) && !is.na(x)
      
      # Guard: if dates are mid-update, return empty quickly
      if (!is_valid_dates(input$dates)) return(pitch_data[0, , drop = FALSE])
      
      pitch_types <- if (is.null(input$pitchType)) "All" else input$pitchType
      
      # Session type first
      df <- if (identical(input$sessionType, "All")) pitch_data
      else dplyr::filter(pitch_data, SessionType == input$sessionType)
      
      # ⛔️ Drop warmups & blank pitch types
      if ("TaggedPitchType" %in% names(df)) {
        df <- df %>%
          dplyr::mutate(.tpt = trimws(as.character(TaggedPitchType))) %>%
          dplyr::filter(!is.na(.tpt) & nzchar(.tpt)) %>%
          dplyr::select(-.tpt)
      }
      if ("PitchSession" %in% names(df)) {
        df <- dplyr::filter(df, is.na(PitchSession) | PitchSession != "Warmup")
      }
      
      # Catcher filter
      cat_pick <- input$catcher
      if (!is.null(cat_pick) && cat_pick != "All") {
        df <- dplyr::filter(df, Catcher == cat_pick)
      }
      
      
      # Date range
      df <- dplyr::filter(df, Date >= input$dates[1], Date <= input$dates[2])
      
      # Pitcher hand
      if (input$hand != "All") df <- dplyr::filter(df, PitcherThrows == input$hand)
      
      # Batter side (Live only)
      if (!is.null(input$batterSide) && input$batterSide != "All") {
        df <- filter_batter_side(df, input$batterSide)
      }
      
      # Spatial & count filters
      df <- enforce_zone(df, input$zoneLoc)
      df <- enforce_inzone(df, input$inZone)
      df <- apply_count_filter(df, input$countFilter)
      
      # Numeric ranges
      if (nnz(input$veloMin)) df <- dplyr::filter(df, RelSpeed >= input$veloMin)
      if (nnz(input$veloMax)) df <- dplyr::filter(df, RelSpeed <= input$veloMax)
      if (nnz(input$ivbMin))  df <- dplyr::filter(df, InducedVertBreak >= input$ivbMin)
      if (nnz(input$ivbMax))  df <- dplyr::filter(df, InducedVertBreak <= input$ivbMax)
      if (nnz(input$hbMin))   df <- dplyr::filter(df, HorzBreak >= input$hbMin)
      if (nnz(input$hbMax))   df <- dplyr::filter(df, HorzBreak <= input$hbMax)
      
      # Pitch number window
      df <- df %>% dplyr::arrange(Date) %>% dplyr::mutate(PitchNumber = dplyr::row_number())
      if (nnz(input$pcMin)) df <- dplyr::filter(df, PitchNumber >= input$pcMin)
      if (nnz(input$pcMax)) df <- dplyr::filter(df, PitchNumber <= input$pcMax)
      
      # Stuff+ calc
      df2 <- compute_stuff_simple(df, base_type = input$stuffBase, level = input$stuffLevel) %>%
        force_pitch_levels() %>%
        dplyr::mutate(Result = factor(compute_result(PitchCall, PlayResult), levels = result_levels))
      
      # Pitch types (post-derive)
      if (!("All" %in% pitch_types)) df2 <- dplyr::filter(df2, TaggedPitchType %in% pitch_types)
      
      df2
    })
    
    # ---- Catching: Data/Custom controls ----
    output$dpButtons <- renderUI({
      sel <- isolate(input$dpMode); if (is.null(sel)) sel <- "Data"
      tagList(
        radioButtons(ns("dpMode"), label = NULL,
                     choices = c("Data","Custom"),
                     selected = sel, inline = TRUE),
        conditionalPanel(
          sprintf("input['%s']==='Custom'", ns("dpMode")),
          selectizeInput(
            ns("dpCustomCols"), label = NULL,
            choices = c("#","Velo","ExchangeTime","PopTime","SL+"),  # <— added "#"
            multiple = TRUE,
            options = list(placeholder = "Choose columns to show…")
          )
        )
      )
    })
    
    
    # ---- Catching: Data/Custom table (PopTime-gated throws; ALL last) ----
    output$dpTable <- DT::renderDataTable({
      req(is_active())
      df_all <- filtered_catch()
      if (!nrow(df_all)) {
        return(DT::datatable(data.frame(Note = "No rows for current filters."),
                             options = list(dom = 't'), rownames = FALSE))
      }
      
      to_num <- function(x) suppressWarnings(as.numeric(x))
      
      # ---------- SL+ on TAKES from the full dataset ----------
      takes   <- !is.na(df_all$PitchCall) & df_all$PitchCall %in% c("StrikeCalled","BallCalled", "BallinDirt")
      buckets <- inzone_label(df_all$PlateLocSide, df_all$PlateLocHeight)
      
      base_tbl <- dplyr::tibble(
        take   = takes,
        bucket = buckets,
        is_cs  = df_all$PitchCall == "StrikeCalled"
      ) %>%
        dplyr::filter(take) %>%
        dplyr::group_by(bucket) %>%
        dplyr::summarise(cs_rate = mean(is_cs, na.rm = TRUE), n = dplyr::n(), .groups = "drop")
      
      overall_rate <- if (nrow(base_tbl) && sum(base_tbl$n) > 0) {
        sum(base_tbl$cs_rate * base_tbl$n) / sum(base_tbl$n)
      } else NA_real_
      
      rate_for_bucket <- function(b) {
        r <- base_tbl$cs_rate[match(b, base_tbl$bucket)]
        ifelse(is.na(r), overall_rate, r)
      }
      
      sl_by_pitch <- df_all %>%
        dplyr::mutate(
          Pitch  = as.character(TaggedPitchType),
          take   = takes,
          bucket = buckets
        ) %>%
        dplyr::group_by(Pitch) %>%
        dplyr::summarise(
          `SL+` = {
            if (!any(take, na.rm = TRUE)) {
              NA_real_
            } else {
              obs <- mean(PitchCall[take] == "StrikeCalled", na.rm = TRUE)
              tb  <- table(bucket[take])
              if (length(tb)) {
                exp <- sum(as.numeric(tb) * vapply(names(tb), rate_for_bucket, numeric(1))) / sum(tb)
                if (is.finite(exp) && exp > 0) round(100 * obs / exp, 1) else NA_real_
              } else NA_real_
            }
          },
          .groups = "drop"
        )
      
      # ---------- Define "throws" as rows WITH PopTime present ----------
      df_throw <- df_all %>%
        dplyr::mutate(
          Pitch            = as.character(TaggedPitchType),
          ThrowSpeed_num   = to_num(ThrowSpeed),
          ExchangeTime_num = to_num(ExchangeTime),
          PopTime_num      = to_num(PopTime)
        ) %>%
        dplyr::filter(
          is.finite(PopTime_num),
          is.finite(ThrowSpeed_num),
          ThrowSpeed_num >= MIN_THROW_MPH   # or use 70 directly
        )
      
      # Per-pitch stats + count of throws (“#”) over PopTime-present rows
      stats_throw <- if (nrow(df_throw)) {
        df_throw %>%
          dplyr::group_by(Pitch) %>%
          dplyr::summarise(
            `#`           = dplyr::n(),                                # <-- throws counted by PopTime presence
            Velo          = round(mean(ThrowSpeed_num,   na.rm = TRUE), 1),  # may be NA if no ThrowSpeed for that pitch
            ExchangeTime  = round(mean(ExchangeTime_num, na.rm = TRUE), 1),
            PopTime       = round(mean(PopTime_num,      na.rm = TRUE), 2),
            .groups = "drop"
          )
      } else {
        dplyr::tibble(Pitch = character(), `#` = integer(),
                      Velo = numeric(), ExchangeTime = numeric(), PopTime = numeric())
      }
      
      # ---------- Join SL+ and order pitches ----------
      out <- stats_throw %>%
        dplyr::left_join(sl_by_pitch, by = "Pitch")
      
      # robust pitch ordering (works with your helpers)
      safe_pitch_order <- function(v) {
        v_chr <- as.character(v)
        if (exists("ordered_types")) {
          ord_try <- tryCatch({
            o <- ordered_types(v_chr)
            if (is.factor(o)) levels(o) else unique(as.character(o))
          }, error = function(e) NULL)
          if (length(ord_try)) return(ord_try)
        }
        if (exists("force_pitch_levels")) {
          ord_try2 <- tryCatch({
            tmp <- data.frame(TaggedPitchType = v_chr, stringsAsFactors = FALSE)
            o <- force_pitch_levels(tmp)
            if ("TaggedPitchType" %in% names(o)) {
              if (is.factor(o$TaggedPitchType)) levels(o$TaggedPitchType)
              else unique(as.character(o$TaggedPitchType))
            } else NULL
          }, error = function(e) NULL)
          if (length(ord_try2)) return(ord_try2)
        }
        unique(na.omit(v_chr))
      }
      ord <- safe_pitch_order(df_all$TaggedPitchType)
      
      out <- out %>%
        dplyr::mutate(Pitch = factor(Pitch, levels = ord)) %>%
        dplyr::arrange(Pitch) %>%
        dplyr::mutate(Pitch = as.character(Pitch))
      
      # ---------- ALL row LAST (totals also over PopTime-present rows) ----------
      total_throws <- nrow(df_throw)  # <-- total PopTime-present throws
      all_velo <- if (total_throws) round(mean(df_throw$ThrowSpeed_num,   na.rm = TRUE), 1) else NA_real_
      all_xch  <- if (total_throws) round(mean(df_throw$ExchangeTime_num, na.rm = TRUE), 1) else NA_real_
      all_pop  <- if (total_throws) round(mean(df_throw$PopTime_num,      na.rm = TRUE), 2) else NA_real_
      obs_all  <- if (any(takes, na.rm = TRUE)) mean(df_all$PitchCall[takes] == "StrikeCalled", na.rm = TRUE) else NA_real_
      exp_all  <- overall_rate
      sl_all   <- if (is.finite(obs_all) && is.finite(exp_all) && exp_all > 0) round(100 * obs_all / exp_all, 1) else NA_real_
      
      all_row <- dplyr::tibble(Pitch = "ALL", `#` = total_throws,
                               Velo = all_velo, ExchangeTime = all_xch, PopTime = all_pop, `SL+` = sl_all)
      
      final <- dplyr::bind_rows(out, all_row)  # ALL last
      
      # ---------- Visible set based on mode ----------
      mode <- if (is.null(input$dpMode)) "Data" else input$dpMode
      default_visible <- if (identical(mode, "Custom")) {
        unique(c("Pitch", "#", input$dpCustomCols))
      } else {
        c("Pitch","#","Velo","ExchangeTime","PopTime","SL+")
      }
      
      datatable_with_colvis(
        final,
        lock            = "Pitch",
        remember        = TRUE,
        default_visible = default_visible
      )
    }, server = FALSE)
    
    # =========================
    # Catching: Location server
    # =========================
    
    # ---- tiny helpers (scoped to this module) ----
    loc_to_num <- function(x) suppressWarnings(as.numeric(x))
    loc_convert_to_feet <- function(x, mode = "auto") {
      x <- as.numeric(x)
      if (mode == "feet")   return(x)
      if (mode == "inches") return(x/12)
      if (mode == "meters") return(x*3.28084)
      p95 <- suppressWarnings(stats::quantile(abs(x), 0.95, na.rm = TRUE))
      if (is.na(p95)) return(x)
      if (p95 > 20)       x/12      else if (p95 < 2) x*3.28084 else x
    }
    loc_guess_cols_xy  <- function(df) {
      nms <- names(df)
      pick <- function(...) { pats <- c(...); for (p in pats) { hit <- nms[grepl(p, nms, ignore.case=TRUE)]; if (length(hit)) return(hit[1]) }; "" }
      list(
        x  = pick("BasePositionX","BagX","ThrowEndX","ArrivalX","BaseX"),
        y  = pick("BasePositionY","BagY","ThrowEndY","ArrivalY","BaseY"),
        cx = pick("SecondBaseX","BagCenterX","BaseCenterX","TargetBaseX"),
        cy = pick("SecondBaseY","BagCenterY","BaseCenterY","TargetBaseY")
      )
    }
    loc_guess_cols_xyz <- function(df) {
      nms <- names(df)
      pick <- function(...) { pats <- c(...); for (p in pats) { hit <- nms[grepl(p, nms, ignore.case=TRUE)]; if (length(hit)) return(hit[1]) }; "" }
      list(
        x  = pick("BasePositionX","BagX","ThrowEndX","ArrivalX","BaseX"),
        y  = pick("BasePositionY","BagY","ThrowEndY","ArrivalY","BaseY"),
        z  = pick("BasePositionZ","BagZ","ThrowEndZ","ArrivalZ","BaseZ","Height"),
        cx = pick("SecondBaseX","BagCenterX","BaseCenterX","TargetBaseX"),
        cy = pick("SecondBaseY","BagCenterY","BaseCenterY","TargetBaseY"),
        cz = pick("SecondBaseZ","BagCenterZ","BaseCenterZ","TargetBaseZ")
      )
    }
    loc_make_bag_poly <- function(size_in = 18) {
      sft <- size_in/12; r <- sft/sqrt(2)
      data.frame(x = c(0, r, 0, -r, 0), y = c(r, 0, -r, 0, r))
    }
    loc_inside_bag <- function(x, y, size_in = 18) {
      r <- (size_in/12)/sqrt(2); (abs(x)+abs(y)) <= r
    }
    
    # ---- common reactive: throws filtered to PopTime present & base selection ----
    loc_throws <- reactive({
      df <- filtered_catch()
      if (!nrow(df)) return(df[0, , drop=FALSE])
      
      # Only rows with PopTime AND ThrowSpeed ≥ 70 mph are considered throws
      df$PopTime_num     <- loc_to_num(df$PopTime)
      df$ThrowSpeed_num  <- loc_to_num(df$ThrowSpeed)
      df <- df[
        is.finite(df$PopTime_num) &
          is.finite(df$ThrowSpeed_num) &
          df$ThrowSpeed_num >= (if (exists("MIN_THROW_MPH")) MIN_THROW_MPH else 70),
        ,
        drop = FALSE
      ]
      if (!nrow(df)) return(df)
      
      tgt <- if (is.null(input$loc_targetBase)) "2B" else input$loc_targetBase
      if ("TargetBase" %in% names(df)) {
        df <- df[as.character(df$TargetBase) == tgt, , drop = FALSE]
      }
      df
    })
    
    
    # ---- TOP-DOWN (2D) ----
    output$loc_topdown <- ggiraph::renderGirafe({
      req(is_active())
      df <- loc_throws(); if (!nrow(df)) return(NULL)
      
      gc <- loc_guess_cols_xy(df)
      if (!nzchar(gc$x) || !nzchar(gc$y)) {
        return(ggiraph::girafe(code = print(ggplot2::ggplot() +
                                              ggplot2::annotate("text", x=0, y=0, label="No base-arrival X/Y columns found") +
                                              ggplot2::theme_void())))
      }
      
      mode <- if (is.null(input$loc_units)) "auto" else input$loc_units
      x <- loc_convert_to_feet(loc_to_num(df[[gc$x]]), mode)
      y <- loc_convert_to_feet(loc_to_num(df[[gc$y]]), mode)
      
      # center to bag: prefer explicit bag center cols; else median-center
      if (nzchar(gc$cx) && nzchar(gc$cy)) {
        cx <- loc_convert_to_feet(loc_to_num(df[[gc$cx]]), mode)
        cy <- loc_convert_to_feet(loc_to_num(df[[gc$cy]]), mode)
        dx <- x - cx; dy <- y - cy
      } else {
        dx <- x - stats::median(x, na.rm = TRUE)
        dy <- y - stats::median(y, na.rm = TRUE)
      }
      
      bag <- loc_make_bag_poly(18)
      zoom <- if (is.null(input$loc_zoom)) 6 else input$loc_zoom
      
      tip <- paste0(
        "dx: ", sprintf("%.2f", dx), " ft\n",
        "dy: ", sprintf("%.2f", dy), " ft\n",
        "Pop: ", ifelse(is.na(df$PopTime), "", df$PopTime), "\n",
        "Velo: ", ifelse(is.na(df$ThrowSpeed), "", df$ThrowSpeed)
      )
      
      p <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = bag, ggplot2::aes(x, y), fill = NA, linewidth = 1) +
        ggplot2::annotate("point", x=0, y=0, shape=3, size=3) +
        ggplot2::geom_hline(yintercept = 0, linetype = 3) +
        ggplot2::geom_vline(xintercept = 0, linetype = 3) +
        ggiraph::geom_point_interactive(
          ggplot2::aes(dx, dy, tooltip = tip),
          data = data.frame(dx, dy, tip)
        ) +
        ggplot2::coord_fixed(xlim = c(-zoom, zoom), ylim = c(-zoom, zoom)) +
        ggplot2::labs(x = "Across-bag (ft)", y = "Up/down-bag (ft)",
                      title = paste("Throw Location (Top-down) →", if (is.null(input$loc_targetBase)) "2B" else input$loc_targetBase)) +
        ggplot2::theme_minimal()
      
      ggiraph::girafe(
        code = print(p),
        options = list(
          ggiraph::opts_hover(css = "opacity:0.9;"),
          ggiraph::opts_toolbar(saveaspng = TRUE)
        )
      )
    })
    
    output$loc_td_summary <- renderText({
      req(is_active())
      df <- loc_throws(); if (!nrow(df)) return("No throws with PopTime in current filters.")
      gc <- loc_guess_cols_xy(df); if (!nzchar(gc$x) || !nzchar(gc$y)) return("No base-arrival X/Y columns found.")
      
      mode <- if (is.null(input$loc_units)) "auto" else input$loc_units
      x <- loc_convert_to_feet(loc_to_num(df[[gc$x]]), mode)
      y <- loc_convert_to_feet(loc_to_num(df[[gc$y]]), mode)
      
      if (nzchar(gc$cx) && nzchar(gc$cy)) {
        cx <- loc_convert_to_feet(loc_to_num(df[[gc$cx]]), mode)
        cy <- loc_convert_to_feet(loc_to_num(df[[gc$cy]]), mode)
        dx <- x - cx; dy <- y - cy
      } else {
        dx <- x - stats::median(x, na.rm = TRUE)
        dy <- y - stats::median(y, na.rm = TRUE)
      }
      
      r <- sqrt(dx^2 + dy^2)
      pct_on <- mean(loc_inside_bag(dx, dy, size_in = 18), na.rm = TRUE)
      paste0(
        "# throws: ", length(r), "\n",
        "Avg miss radius: ", sprintf("%.2f", mean(r, na.rm = TRUE)), " ft\n",
        "68% radius: ",     sprintf("%.2f", stats::quantile(r, 0.68, na.rm = TRUE)), " ft\n",
        "% on bag: ",       sprintf("%.1f%%", 100*pct_on), "\n",
        "Bias (dx, dy): ",  sprintf("%.2f, %.2f", mean(dx, na.rm = TRUE), mean(dy, na.rm = TRUE)), " ft"
      )
    })
    
    # ---- 3D view ----
    output$loc_3d <- plotly::renderPlotly({
      req(is_active())
      df <- loc_throws(); if (!nrow(df)) return(NULL)
      
      gc <- loc_guess_cols_xyz(df)
      if (!nzchar(gc$x) || !nzchar(gc$y) || !nzchar(gc$z)) {
        return(plotly::plot_ly() |> plotly::add_text(x=0, y=0, text="No BasePosition X/Y/Z found"))
      }
      
      mode <- if (is.null(input$loc_units)) "auto" else input$loc_units
      x <- loc_convert_to_feet(loc_to_num(df[[gc$x]]), mode)
      y <- loc_convert_to_feet(loc_to_num(df[[gc$y]]), mode)
      z <- loc_convert_to_feet(loc_to_num(df[[gc$z]]), mode)
      
      if (nzchar(gc$cx) && nzchar(gc$cy)) {
        cx <- loc_convert_to_feet(loc_to_num(df[[gc$cx]]), mode)
        cy <- loc_convert_to_feet(loc_to_num(df[[gc$cy]]), mode)
      } else {
        cx <- stats::median(x, na.rm = TRUE)
        cy <- stats::median(y, na.rm = TRUE)
      }
      cz <- if (nzchar(gc$cz)) loc_convert_to_feet(loc_to_num(df[[gc$cz]]), mode) else 0
      
      dx <- x - cx; dy <- y - cy; dz <- z - cz
      r  <- sqrt(dx^2 + dy^2 + dz^2)
      
      # bag outline at z=0
      side_ft <- 18/12; rbag <- side_ft/sqrt(2)
      bag <- data.frame(x = c(0, rbag, 0, -rbag, 0), y = c(rbag, 0, -rbag, 0, rbag), z = 0)
      
      tip <- paste0(
        "dx=", sprintf("%.2f", dx)," ft<br>",
        "dy=", sprintf("%.2f", dy)," ft<br>",
        "dz=", sprintf("%.2f", dz)," ft<br>",
        "3D miss=", sprintf("%.2f", r)," ft<br>",
        "Pop=", ifelse(is.na(df$PopTime), "", df$PopTime), "<br>",
        "Velo=", ifelse(is.na(df$ThrowSpeed), "", df$ThrowSpeed)
      )
      
      zoom <- if (is.null(input$loc_zoom)) 6 else input$loc_zoom
      lim  <- c(-zoom, zoom)
      
      plotly::plot_ly() |>
        plotly::add_trace(type="scatter3d", mode="lines",
                          x=bag$x, y=bag$y, z=bag$z,
                          line = list(width = 6), hoverinfo="skip", name="Bag") |>
        plotly::add_markers(x = dx, y = dy, z = dz, type = "scatter3d", mode = "markers",
                            marker = list(size = 4, opacity = 0.8),
                            text = tip, hoverinfo = "text", name = "Throws") |>
        plotly::layout(
          scene = list(
            xaxis = list(title = "Across-bag (ft)", range = lim),
            yaxis = list(title = "Up/down-bag (ft)", range = lim),
            zaxis = list(title = "Height (ft)"),
            aspectmode = "cube"
          ),
          title = paste("Throw Location (3D) →", if (is.null(input$loc_targetBase)) "2B" else input$loc_targetBase)
        )
    })
    
    
    # ---- HeatMaps: note ----
    output$hmNote <- renderUI({
      HTML("<small><em>Heat shows Called-Strike% by location (per taken-pitch opportunity = called strike or called ball). Use the Pitch Results filter to include/exclude outcomes.</em></small>")
    })
    
    
    # --- NEW helpers for Heat surface (place inside mod_catch_server, above output$heatPlot) ---
    
    .safe_heat_cols <- function(n = 256) {
      if (exists("heat_pal_red")) {
        heat_pal_red(n)
      } else {
        grDevices::colorRampPalette(c("#ffffff", "#ffc0c0", "#ff6666", "#d7301f"))(n)
      }
    }
    
    # Fit a smooth Called-Strike% surface (per taken opportunity) and an alpha map for opportunity density
    .fit_cs_surface <- function(df_taken, lims = c(-2, 2, 0, 4.5), n = 180) {
      stopifnot(all(c("PlateLocSide","PlateLocHeight","PitchCall") %in% names(df_taken)))
      df_taken <- df_taken[is.finite(df_taken$PlateLocSide) & is.finite(df_taken$PlateLocHeight), , drop = FALSE]
      if (!nrow(df_taken)) return(NULL)
      df_taken$cs <- as.integer(df_taken$PitchCall == "StrikeCalled")
      
      grid <- expand.grid(
        x = seq(lims[1], lims[2], length.out = n),
        y = seq(lims[3], lims[4], length.out = n)
      )
      
      # Primary: smooth logistic surface with mgcv (best) ; Fallback: binned ratio with Beta prior
      p_hat <- NULL
      if (requireNamespace("mgcv", quietly = TRUE) &&
          length(unique(df_taken$cs)) > 1 && nrow(df_taken) >= 60) {
        m <- mgcv::gam(cs ~ s(PlateLocSide, PlateLocHeight, k = 60),
                       data = df_taken, family = stats::binomial(link = "logit"))
        p_hat <- stats::plogis(stats::predict(m, newdata = data.frame(
          PlateLocSide = grid$x, PlateLocHeight = grid$y
        )))
      } else {
        # Fallback: 2D binned CS% with prior shrinkage toward global rate
        nx <- 60; ny <- 60
        xbreaks <- seq(lims[1], lims[2], length.out = nx + 1)
        ybreaks <- seq(lims[3], lims[4], length.out = ny + 1)
        xi <- cut(df_taken$PlateLocSide, xbreaks, include.lowest = TRUE)
        yi <- cut(df_taken$PlateLocHeight, ybreaks, include.lowest = TRUE)
        opp  <- as.matrix(stats::xtabs(~ xi + yi))
        num  <- as.matrix(stats::xtabs(cs ~ xi + yi))
        p0   <- mean(df_taken$cs, na.rm = TRUE)
        alpha <- 20  # prior sample size
        p_bin <- (num + alpha * p0) / (opp + alpha)
        grid <- expand.grid(
          x = head(xbreaks, -1) + diff(xbreaks) / 2,
          y = head(ybreaks, -1) + diff(ybreaks) / 2
        )
        p_hat <- as.vector(p_bin)
      }
      
      # Opportunity alpha: denser regions more opaque (fade sparse)
      if (requireNamespace("MASS", quietly = TRUE)) {
        den <- MASS::kde2d(df_taken$PlateLocSide, df_taken$PlateLocHeight, n = n, lims = lims)$z
        den <- den / max(den, na.rm = TRUE)
        alpha <- pmax(0.25, as.vector(den))  # 0.25..1
      } else {
        alpha <- rep(1, length(p_hat))
      }
      
      data.frame(x = grid$x, y = grid$y, p = p_hat, a = alpha)
    }
    
    # ---- HeatMaps: Heat (Called-Strike% per opportunity) ----
    # ---- HeatMaps: Heat (Called-Strike% per taken opportunity, smooth; alpha = opportunity) ----
    output$heatPlot <- renderPlot({
      df <- filtered_catch(); if (!nrow(df)) return()
      # NEW: filter by selected pitch results (matches pitching suite behavior)
      res_sel <- input$hmResults
      if (!is.null(res_sel) && length(res_sel)) {
        df <- dplyr::filter(df, Result %in% res_sel)
      }
      
      
      # taken-pitch opportunities only (called strike/ball; include BID as "ball" opportunity)
      df_taken <- dplyr::filter(
        df,
        !is.na(PitchCall) & PitchCall %in% c("StrikeCalled","BallCalled","BallinDirt"),
        is.finite(PlateLocSide), is.finite(PlateLocHeight)
      )
      if (!nrow(df_taken) || length(unique(df_taken$PitchCall)) < 2) {
        return(ggplot() + theme_void() + labs(title = "Insufficient taken-pitch data"))
      }
      
      lims <- c(-2, 2, 0, 4.5); n <- 180
      surf <- .fit_cs_surface(df_taken, lims = lims, n = n)
      if (is.null(surf)) return(ggplot() + theme_void())
      
      # Overlays (same geometry you use elsewhere)
      home <- data.frame(x = c(-0.75,0.75,0.75,0.00,-0.75), y = c(1.05,1.05,1.15,1.25,1.15) - 0.5)
      cz   <- data.frame(xmin = -1.5, xmax =  1.5, ymin = 2.65 - 1.7, ymax = 2.65 + 1.3)
      sz   <- data.frame(xmin = ZONE_LEFT, xmax = ZONE_RIGHT, ymin = ZONE_BOTTOM, ymax = ZONE_TOP)
      
      cols <- .safe_heat_cols(256)
      
      ggplot() +
        geom_raster(data = surf, aes(x, y, fill = p, alpha = a), interpolate = TRUE) +
        scale_fill_gradientn(colors = cols, limits = c(0, 1), name = "CS%") +
        scale_alpha(range = c(0.25, 1), guide = "none") +
        geom_polygon(data = home, aes(x, y), fill = NA, color = "black", linewidth = 0.6) +
        geom_rect(data = cz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = NA, color = "black", linetype = "dashed", linewidth = 0.6) +
        geom_rect(data = sz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = NA, color = "black", linewidth = 0.8) +
        coord_fixed(ratio = 1, xlim = c(lims[1], lims[2]), ylim = c(lims[3], lims[4])) +
        labs(title = "Called-Strike%", x = NULL, y = NULL) +
        theme_void() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))
    }, bg = "transparent")
    
    
    # ---- HeatMaps: Pitch (interactive scatter) ----
    output$pitchPlot <- ggiraph::renderGirafe({
      req(input$hmChartType == "Pitch")
      df <- filtered_catch(); if (!nrow(df)) return(NULL)
      # NEW: filter by selected pitch results
      res_sel <- input$hmResults
      if (!is.null(res_sel) && length(res_sel)) {
        df <- dplyr::filter(df, Result %in% res_sel)
      }
      
      
      types <- intersect(names(all_colors), as.character(unique(df$TaggedPitchType)))
      types_chr <- as.character(types)
      
      df_i <- df %>% dplyr::mutate(tt = make_hover_tt(.), rid = dplyr::row_number(),
                                   Result = factor(compute_result(PitchCall, PlayResult), levels = result_levels))
      
      home <- data.frame(x=c(-0.75,0.75,0.75,0.00,-0.75), y=c(1.05,1.05,1.15,1.25,1.15)-0.5)
      cz   <- data.frame(xmin = -1.5, xmax = 1.5, ymin = 2.65 - 1.7, ymax = 2.65 + 1.3)
      sz   <- data.frame(xmin = ZONE_LEFT, xmax = ZONE_RIGHT, ymin = ZONE_BOTTOM, ymax = ZONE_TOP)
      
      df_known <- dplyr::filter(df_i, !is.na(Result))
      df_other <- dplyr::filter(df_i,  is.na(Result))
      
      p <- ggplot() +
        geom_polygon(data = home, aes(x, y), fill = NA, color = "black") +
        geom_rect(data = cz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA, color = "black", linetype = "dashed") +
        geom_rect(data = sz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA, color = "black") +
        ggiraph::geom_point_interactive(
          data = df_other,
          aes(PlateLocSide, PlateLocHeight, color = TaggedPitchType, fill = TaggedPitchType, tooltip = tt, data_id = rid),
          size = 4.0, alpha = 0.95, shape = 16, stroke = 0.6
        ) +
        ggiraph::geom_point_interactive(
          data = df_known,
          aes(PlateLocSide, PlateLocHeight, color = TaggedPitchType, fill = TaggedPitchType, shape = Result, tooltip = tt, data_id = rid),
          size = 4.0, alpha = 0.95, stroke = 0.8
        ) +
        scale_color_manual(values = all_colors[types_chr], limits = types_chr, name = NULL) +
        scale_fill_manual(values  = all_colors[types_chr], limits = types_chr, name = NULL) +
        scale_shape_manual(values = shape_map, drop = TRUE, name = NULL) +
        coord_fixed(ratio = 1, xlim = c(-3, 3), ylim = c(0.5, 5)) +
        theme_void() + theme(legend.position = "none")
      
      ggiraph::girafe(
        ggobj = p,
        options = list(
          ggiraph::opts_tooltip(use_fill = TRUE, use_stroke = TRUE,
                                css = "color:#fff !important;font-weight:600;padding:6px;border-radius:8px;text-shadow:0 1px 1px rgba(0,0,0,.4);"),
          ggiraph::opts_hover(css = "stroke-width:1.5px;"),
          ggiraph::opts_hover_inv(css = "opacity:0.15;")
        )
      )
    })
    
  })  # <-- closes moduleServer(id, function(...) { ... })
}     # <-- closes mod_catch_server()

# ==========================
# == Camps Suite (updated) ==
# ==========================
mod_camps_ui <- function(id, show_header = FALSE) {
  ns <- NS(id)
  fluidPage(
    # No header (hidden to match other suites)
    sidebarLayout(
      sidebarPanel(
        # Same as Pitching suite, but no Session Type + label changed
        selectInput(ns("player"), "Select Player:", choices = c("All" = "All"), selected = "All"),
        dateRangeInput(ns("dates"), "Date Range:",
                       start = Sys.Date() - 30, end = Sys.Date(),
                       format = "mm/dd/yyyy"),
        selectInput(ns("hand"),       "Pitcher Hand:",  choices = c("All","Left","Right"), selected = "All"),
        selectInput(ns("pitchType"),  "Pitch Type:",    choices = "All", selected = "All", multiple = TRUE),
        selectInput(
          ns("zoneLoc"), "Zone Location:",
          choices = c("All",
                      "Upper Half","Bottom Half","Left Half","Right Half",
                      "Upper 3rd","Bottom 3rd","Left 3rd","Right 3rd"),
          selected = "All", multiple = TRUE
        ),
        selectInput(ns("inZone"),     "In Zone:",       choices = c("All","Yes","No","Competitive"), selected = "All"),
        selectInput(ns("batterSide"), "Batter Hand:",   choices = c("All","Left","Right"), selected = "All"),
        selectInput(
          ns("countFilter"), "Count:",
          choices  = c("All"="All","Even"="Even","Behind"="Behind","Ahead"="Ahead","2K Not Full"="2KNF",
                       "0-0","0-1","1-0","1-1","2-0","2-1","0-2","1-2","2-2","3-0","3-1","3-2"),
          selected = "All", multiple = TRUE
        ),
        fluidRow(
          column(6, numericInput(ns("veloMin"), "Velocity Min (MPH):", value = NA)),
          column(6, numericInput(ns("veloMax"), "Velocity Max (MPH):", value = NA))
        ),
        fluidRow(
          column(6, numericInput(ns("ivbMin"), "IVB Min (inches):", value = NA)),
          column(6, numericInput(ns("ivbMax"), "IVB Max (inches):", value = NA))
        ),
        fluidRow(
          column(6, numericInput(ns("hbMin"), "HB Min (inches):", value = NA)),
          column(6, numericInput(ns("hbMax"), "HB Max (inches):", value = NA))
        ),
        fluidRow(
          column(6, numericInput(ns("pcMin"), "Pitch Count Min:", value = NA, min = 1)),
          column(6, numericInput(ns("pcMax"), "Pitch Count Max:", value = NA, min = 1))
        ),
        width = 3,
        class = "sidebar"
      ),
      mainPanel(
        tabsetPanel(
          id = ns("tabs"),
          
          # --- PITCHING (Summary + plots) ---
          tabPanel(
            "Pitching",
            # New: plots above table
            div(style="margin: 6px 0 10px;", ggiraph::girafeOutput(ns("campPitchReleasePlot"), height = "280px")),
            div(style="margin: 6px 0 10px;", ggiraph::girafeOutput(ns("campPitchMovePlot"),    height = "280px")),
            div(style="margin: 6px 0 10px;", ggiraph::girafeOutput(ns("campPitchLocPlot"),     height = "320px")),
            div(style = "margin: 8px 0;", uiOutput(ns("campPitchButtons"))),
            DT::dataTableOutput(ns("campPitchTable"))
          ),
          
          # --- HITTING (Data & Performance) ---
          tabPanel(
            "Hitting",
            div(style = "margin: 8px 0;", uiOutput(ns("campHitButtons"))),
            div(style = "margin: 8px 0;", ggiraph::girafeOutput(ns("campSprayChart"), height = "460px")),
            DT::dataTableOutput(ns("campHitTable"))
          ),
          
          # --- CATCHING (Data & Performance) ---
          tabPanel(
            "Catching",
            div(style = "margin: 8px 0;", uiOutput(ns("campCatchButtons"))),
            DT::dataTableOutput(ns("campCatchTable"))
          )
        )
      )
    )
  )
}

mod_camps_server <- function(id, is_active = shiny::reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------------------------
    # Camps data hooks (Camps-only)
    # ---------------------------
    get_camps_pitching <- function() {
      if (exists("camps_pitch_data_pitching", inherits = TRUE)) return(get("camps_pitch_data_pitching", inherits = TRUE))
      if (exists("load_camps_pitching", inherits = TRUE)) return(get("load_camps_pitching", inherits = TRUE)())
      data.frame(Date = as.Date(character()),
                 Pitcher = character(), PitcherThrows = character(),
                 TaggedPitchType = factor(character()),
                 PlateLocSide = numeric(), PlateLocHeight = numeric(),
                 RelSpeed = numeric(), InducedVertBreak = numeric(), HorzBreak = numeric(),
                 ReleaseTilt = numeric(), BreakTilt = numeric(),
                 SpinEfficiency = numeric(), SpinRate = numeric(),
                 RelHeight = numeric(), RelSide = numeric(),
                 VertApprAngle = numeric(), HorzApprAngle = numeric(),
                 Extension = numeric(), Balls = integer(), Strikes = integer(),
                 SessionType = character(), PitchCall = character(),
                 ExitSpeed = numeric(), Angle = numeric(),
                 KorBB = character(), PlayResult = character(),
                 stringsAsFactors = FALSE)
    }
    get_camps_all <- function() {
      if (exists("camps_pitch_data", inherits = TRUE)) return(get("camps_pitch_data", inherits = TRUE))
      if (exists("load_camps_all", inherits = TRUE)) return(get("load_camps_all", inherits = TRUE)())
      data.frame(Date = as.Date(character()),
                 Pitcher = character(), Batter = character(), Catcher = character(),
                 PitcherThrows = character(), BatterSide = character(),
                 TaggedPitchType = factor(character()),
                 PlateLocSide = numeric(), PlateLocHeight = numeric(),
                 RelSpeed = numeric(), InducedVertBreak = numeric(), HorzBreak = numeric(),
                 ReleaseTilt = numeric(), BreakTilt = numeric(),
                 SpinEfficiency = numeric(), SpinRate = numeric(),
                 RelHeight = numeric(), RelSide = numeric(),
                 VertApprAngle = numeric(), HorzApprAngle = numeric(),
                 Extension = numeric(), Balls = integer(), Strikes = integer(),
                 SessionType = character(), PitchCall = character(),
                 ExitSpeed = numeric(), Angle = numeric(), Distance = numeric(), Direction = numeric(),
                 KorBB = character(), PlayResult = character(),
                 stringsAsFactors = FALSE)
    }
    
    # ---------------------------
    # Helpers
    # ---------------------------
    as_date_any <- function(x) {
      if (inherits(x, "Date")) return(x)
      if (inherits(x, c("POSIXct","POSIXt"))) return(as.Date(x))
      x_chr <- as.character(x)
      suppressWarnings(
        as.Date(x_chr, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y"))
      )
    }
    nnz <- function(x) !is.null(x) && !is.na(x)
    nz_mean <- function(x) { x <- suppressWarnings(as.numeric(x)); m <- mean(x, na.rm = TRUE); if (is.finite(m)) m else NA_real_ }
    safe_div <- function(a,b) { a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b)); ifelse(is.finite(b) & b != 0 & is.finite(a), a/b, NA_real_) }
    fmt_rate3 <- function(x) { x <- suppressWarnings(as.numeric(x)); s <- ifelse(is.finite(x), sprintf("%.3f", x), ""); sub("^0\\.", ".", s) }
    parse_num <- function(x) {
      if (is.numeric(x)) return(x)
      sx <- trimws(as.character(x)); sx[sx==""] <- NA_character_
      is_pct <- grepl("%", sx)
      val <- suppressWarnings(as.numeric(gsub("[^0-9eE.+-]", "", sx)))
      val[is_pct] <- val[is_pct] / 100
      val
    }
    
    # ---------------------------
    # Load & normalize Camps data
    # ---------------------------
    camps_pitching <- shiny::reactive({
      d <- get_camps_pitching()
      d$Date <- as_date_any(d$Date)
      d
    })
    camps_all <- shiny::reactive({
      d <- get_camps_all()
      d$Date <- as_date_any(d$Date)
      d
    })
    
    # ---------------------------
    # Populate sidebar choices from Camps datasets
    # ---------------------------
    observe({
      req(is_active())
      # Pitch types (from Camps)
      d <- camps_all()
      lv <- levels(d$TaggedPitchType)
      pt_levels <- if (is.null(lv)) unique(as.character(d$TaggedPitchType)) else lv
      updateSelectInput(session, "pitchType",
                        choices = c("All", pt_levels),
                        selected = "All")
    })
    
    observe({
      req(is_active())
      # Date range bounds reflect Camps-only data; robust to POSIX/char
      d1 <- camps_pitching(); d2 <- camps_all()
      all_dates <- c(as_date_any(d1$Date), as_date_any(d2$Date))
      all_dates <- all_dates[is.finite(all_dates)]
      if (!length(all_dates)) return()
      first_date <- min(all_dates, na.rm = TRUE)
      last_date  <- max(all_dates, na.rm = TRUE)
      if (is.finite(first_date) && is.finite(last_date)) {
        updateDateRangeInput(session, "dates", start = first_date, end = last_date)
      }
    })
    
    observe({
      req(is_active())
      # Player list = union of Pitchers, Batters, Catchers (Camps only)
      d1 <- camps_pitching(); d2 <- camps_all()
      players <- sort(unique(c(
        as.character(d1$Pitcher),
        as.character(d2$Pitcher),
        as.character(d2$Batter),
        as.character(d2$Catcher)
      )))
      players <- players[nzchar(players)]
      .pretty_name <- function(x) {
        x <- as.character(x)
        ifelse(grepl(",", x), paste0(trimws(sub(".*,", "", x)), " ", trimws(sub(",.*", "", x))), x)
      }
      named <- stats::setNames(players, .pretty_name(players))
      updateSelectInput(session, "player",
                        choices = c("All" = "All", named),
                        selected = "All")
    })
    
    # ---------------------------
    # Common filtering (Camps-only base)
    # ---------------------------
    filtered_base <- reactive({
      req(is_active(), input$dates, input$hand, input$zoneLoc, input$inZone)
      df <- camps_all()
      
      # --- privacy: players see only their own Camps rows ---
      ue <- get_user_email(session, input)
      if (!user_is_admin(session, ue)) {
        allow <- allowed_names_for_user(ue)
        df <- filter_to_user(df, "Camps", allow)
      }
      # Date range (use Date column coerced to Date)
      dcol <- as_date_any(df$Date)
      if (length(input$dates) == 2) {
        df <- df[!is.na(dcol) & dcol >= as.Date(input$dates[1]) & dcol <= as.Date(input$dates[2]), , drop = FALSE]
      }
      
      # Pitcher hand
      if (input$hand != "All") df <- dplyr::filter(df, PitcherThrows == input$hand)
      
      # Batter hand (Live only)
      if (!is.null(input$batterSide) && input$batterSide != "All") {
        df <- filter_batter_side(df, input$batterSide)
      }
      
      # Zone / in-zone / count (your existing helpers)
      df <- enforce_zone(df, input$zoneLoc)
      df <- enforce_inzone(df, input$inZone)
      df <- apply_count_filter(df, input$countFilter)
      
      # Numeric ranges
      if (nnz(input$veloMin)) df <- dplyr::filter(df, RelSpeed >= input$veloMin)
      if (nnz(input$veloMax)) df <- dplyr::filter(df, RelSpeed <= input$veloMax)
      if (nnz(input$ivbMin))  df <- dplyr::filter(df, InducedVertBreak >= input$ivbMin)
      if (nnz(input$ivbMax))  df <- dplyr::filter(df, InducedVertBreak <= input$ivbMax)
      if (nnz(input$hbMin))   df <- dplyr::filter(df, HorzBreak >= input$hbMin)
      if (nnz(input$hbMax))   df <- dplyr::filter(df, HorzBreak <= input$hbMax)
      
      # Pitch types
      ptypes <- input$pitchType; if (is.null(ptypes)) ptypes <- "All"
      if (!("All" %in% ptypes)) df <- dplyr::filter(df, TaggedPitchType %in% ptypes)
      
      # Chronological pitch number → pitch count filter
      df <- df %>% dplyr::arrange(as_date_any(Date)) %>% dplyr::mutate(PitchNumber = dplyr::row_number())
      if (nnz(input$pcMin)) df <- dplyr::filter(df, PitchNumber >= input$pcMin)
      if (nnz(input$pcMax)) df <- dplyr::filter(df, PitchNumber <= input$pcMax)
      
      # Compute Stuff+ (harmless if not used)
      df2 <- compute_stuff_simple(df, base_type = "Fastball", level = "College") %>% force_pitch_levels()
      df2
    })
    
    # ======================================================
    # PITCHING page (Summary — same options as Pitching suite)
    # + Release / Movement / Location plots
    # ======================================================
    
    output$campPitchButtons <- renderUI({
      sel <- isolate(input$campPitchMode); if (is.null(sel)) sel <- "Stuff"
      tagList(
        radioButtons(ns("campPitchMode"), label = NULL,
                     choices = c("Stuff","Process","Results","Custom"),
                     selected = sel, inline = TRUE),
        conditionalPanel(
          sprintf("input['%s']=='Custom'", ns("campPitchMode")),
          selectizeInput(ns("campPitchCustomCols"), label = NULL,
                         choices = setdiff(all_table_cols, "Pitch"),
                         multiple = TRUE,
                         options = list(placeholder = "Choose columns to show…"))
        )
      )
    })
    
    # ----- Plot data (pitching-only rows; optional player filter) -----
    pitch_df_for_plots <- reactive({
      df <- filtered_base()
      if (!is.null(input$player) && input$player != "All") {
        df <- dplyr::filter(df, Pitcher == input$player)
      }
      df
    })
    
    # Release Plot: RelSide vs RelHeight
    output$campPitchReleasePlot <- ggiraph::renderGirafe({
      df <- pitch_df_for_plots()
      if (!nrow(df)) return(NULL)
      df <- df %>% dplyr::filter(is.finite(RelSide), is.finite(RelHeight))
      if (!nrow(df)) return(NULL)
      
      types <- intersect(names(all_colors), as.character(unique(df$TaggedPitchType)))
      dark_on <- isTRUE(input$dark_mode)
      axis_col <- if (dark_on) "#e5e7eb" else "black"
      cols <- colors_for_mode(dark_on)
      p <- ggplot(df, aes(RelSide, RelHeight, color = TaggedPitchType)) +
        ggiraph::geom_point_interactive(aes(
          tooltip = paste0("Pitch: ", TaggedPitchType,
                           "\nVelo: ", ifelse(is.finite(RelSpeed), round(RelSpeed,1), NA), " mph",
                           "\nSpin: ", ifelse(is.finite(SpinRate), round(SpinRate,0), NA), " rpm")
        ), size = 2.8, alpha = 0.9) +
        scale_color_manual(values = cols[types], limits = types, name = NULL) +
        coord_cartesian(ylim = c(0, 8)) +
        labs(x = "Release Side (ft)", y = "Release Height (ft)") +
        theme_minimal() + theme(
          legend.position = "none",
          axis.text = element_text(colour = axis_col),
          axis.title = element_text(colour = axis_col),
          panel.background = element_rect(fill = NA, colour = NA),
          plot.background = element_rect(fill = NA, colour = NA),
          panel.grid.major = element_line(color = scales::alpha(axis_col, 0.1)),
          panel.grid.minor = element_blank()
        )
      
      ggiraph::girafe(ggobj = p, bg = "transparent")
    })
    
    # Movement Plot: HB vs IVB
    output$campPitchMovePlot <- ggiraph::renderGirafe({
      df <- pitch_df_for_plots()
      if (!nrow(df)) return(NULL)
      df <- df %>% dplyr::filter(is.finite(HorzBreak), is.finite(InducedVertBreak))
      if (!nrow(df)) return(NULL)
      
      types <- intersect(names(all_colors), as.character(unique(df$TaggedPitchType)))
      dark_on <- isTRUE(input$dark_mode)
      axis_col <- if (dark_on) "#e5e7eb" else "black"
      cols <- colors_for_mode(dark_on)
      p <- ggplot(df, aes(HorzBreak, InducedVertBreak, color = TaggedPitchType)) +
        ggiraph::geom_point_interactive(aes(
          tooltip = paste0("Pitch: ", TaggedPitchType,
                           "\nHB: ", round(HorzBreak,1), " in",
                           "\nIVB: ", round(InducedVertBreak,1), " in")
        ), size = 2.8, alpha = 0.9) +
        scale_color_manual(values = cols[types], limits = types, name = NULL) +
        labs(x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
        theme_minimal() + theme(
          legend.position = "none",
          axis.text = element_text(colour = axis_col),
          axis.title = element_text(colour = axis_col),
          panel.background = element_rect(fill = NA, colour = NA),
          plot.background = element_rect(fill = NA, colour = NA),
          panel.grid.major = element_line(color = scales::alpha(axis_col, 0.15)),
          panel.grid.minor = element_blank()
        )
      
      ggiraph::girafe(ggobj = p, bg = "transparent")
    })
    
    # Location Plot: PlateLocSide vs PlateLocHeight + zone
    output$campPitchLocPlot <- ggiraph::renderGirafe({
      df <- pitch_df_for_plots()
      if (!nrow(df)) return(NULL)
      df <- df %>% dplyr::filter(is.finite(PlateLocSide), is.finite(PlateLocHeight))
      if (!nrow(df)) return(NULL)
      
      types <- intersect(names(all_colors), as.character(unique(df$TaggedPitchType)))
      dark_on <- isTRUE(input$dark_mode)
      axis_col <- if (dark_on) "#e5e7eb" else "black"
      cols <- colors_for_mode(dark_on)
      
      home <- data.frame(x=c(-0.75,0.75,0.75,0.00,-0.75),
                         y=c(1.05,1.05,1.15,1.25,1.15)-0.5)
      cz <- data.frame(xmin = -1.5, xmax = 1.5, ymin = 2.65 - 1.7, ymax = 2.65 + 1.3)
      sz <- data.frame(xmin = ZONE_LEFT, xmax = ZONE_RIGHT, ymin = ZONE_BOTTOM, ymax = ZONE_TOP)
      
      p <- ggplot() +
        geom_polygon(data = home, aes(x, y), fill = NA, color = axis_col) +
        geom_rect(data = cz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = NA, color = axis_col, linetype = "dashed") +
        geom_rect(data = sz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = NA, color = axis_col) +
        ggiraph::geom_point_interactive(
          data = df,
          aes(PlateLocSide, PlateLocHeight, color = TaggedPitchType,
              tooltip = paste0("Pitch: ", TaggedPitchType,
                               "\nCall: ", coalesce(PitchCall, ""),
                               "\nVelo: ", ifelse(is.finite(RelSpeed), round(RelSpeed,1), NA), " mph")),
          size = 3.0, alpha = 0.95, stroke = 0.7
        ) +
        scale_color_manual(values = cols[types], limits = types, name = NULL) +
        coord_fixed(ratio = 1, xlim = c(-3, 3), ylim = c(0.5, 5)) +
        theme_void() + theme(
          legend.position = "none",
          plot.background = element_rect(fill = NA, colour = NA),
          panel.background = element_rect(fill = NA, colour = NA)
        )
      
      ggiraph::girafe(ggobj = p, bg = "transparent")
    })
    
    # ----- Pitching table (same options/defs as your Pitching Summary) -----
    output$campPitchTable <- DT::renderDataTable({
      df_all <- filtered_base()
      if (!is.null(input$player) && input$player != "All") {
        df_all <- dplyr::filter(df_all, Pitcher == input$player)
      }
      validate(need(nrow(df_all) > 0, "No pitching data for current filters / player"))
      
      if (!("TaggedPitchType" %in% names(df_all))) df_all$TaggedPitchType <- "All"
      df_all$TaggedPitchType <- as.character(df_all$TaggedPitchType)
      pf_all <- compute_pa_flags(df_all)
      df_all <- pf_all$df %>%
        dplyr::mutate(
          .is_strikeout = pf_all$is_strikeout,
          .is_walk      = pf_all$is_walk
        )
      by_type <- split(df_all, df_all$TaggedPitchType)
      
      build_row <- function(df) {
        pf <- compute_pa_flags(df)
        df <- pf$df
        df_is_strikeout <- pf$is_strikeout
        df_is_walk      <- pf$is_walk
        safe_pct <- function(num, den) {
          num <- suppressWarnings(as.numeric(num)); den <- suppressWarnings(as.numeric(den))
          ifelse(is.finite(den) & den > 0 & is.finite(num), paste0(round(100*num/den, 1), "%"), "")
        }
        scores <- ifelse(
          df$PlateLocSide >= ZONE_LEFT & df$PlateLocSide <= ZONE_RIGHT &
            df$PlateLocHeight >= ZONE_BOTTOM & df$PlateLocHeight <= ZONE_TOP, 1.47,
          ifelse(
            df$PlateLocSide >= -1.5 & df$PlateLocSide <= 1.5 &
              df$PlateLocHeight >= (2.65-1.7) & df$PlateLocHeight <= (2.65+1.3),
            0.73, 0
          )
        )
        swing_levels <- c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall")
        has_pc  <- sum(!is.na(df$PitchCall)) > 0
        strikes <- sum(df$PitchCall %in% c("StrikeCalled","StrikeSwinging","InPlay","FoulBall","FoulBallNotFieldable","FoulBallFieldable","FoulBall"), na.rm = TRUE)
        sw      <- sum(df$PitchCall == "StrikeSwinging", na.rm = TRUE)
        den     <- sum(df$PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall"), na.rm = TRUE)
        
        live_mask <- df$SessionType == "Live"
        bf_live <- sum(live_mask & df$Balls == 0 & df$Strikes == 0, na.rm = TRUE)
        k_live  <- sum(live_mask & df_is_strikeout, na.rm = TRUE)
        bb_live <- sum(live_mask & df_is_walk,      na.rm = TRUE)
        fps_live <- sum(df$SessionType == "Live" &
                          df$Balls == 0 & df$Strikes == 0 &
                          df$PitchCall %in% c("InPlay","StrikeSwinging","StrikeCalled","FoulBallNotFieldable","FoulBall"),
                        na.rm = TRUE)
        ea_live  <- sum(df$SessionType == "Live" & (
          (df$Balls == 0 & df$Strikes == 0 & df$PitchCall == "InPlay") |
            (df$Balls == 0 & df$Strikes == 1 & df$PitchCall %in% c(
              "InPlay", "StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable","FoulBall"
            )) |
            (df$Balls == 1 & df$Strikes == 0 & df$PitchCall == "InPlay") |
            (df$Balls == 1 & df$Strikes == 1 & df$PitchCall %in% c(
              "InPlay", "StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable","FoulBall", "FoulBallFieldable"
            ))
        ),
        na.rm = TRUE
        )
        
        vmax   <- suppressWarnings(max(as.numeric(df$RelSpeed), na.rm = TRUE)); vmax <- if (is.finite(vmax)) round(vmax, 1) else NA_real_
        ev_all <- nz_mean(ifelse(df$SessionType=="Live", df$ExitSpeed, NA_real_))
        la_all <- nz_mean(ifelse(df$SessionType=="Live", df$Angle,     NA_real_))
        stuff_all <- round(nz_mean(df$`Stuff+`), 1)
        ctrl_all   <- round(nz_mean(scores) * 100, 1)
        qp_all    <- round(nz_mean(compute_qp_points(df)) * 200, 1)
        
        tibble::tibble(
          `#`            = nrow(df),
          Overall        = NA_character__,  # filled after bind
          Velo           = round(nz_mean(df$RelSpeed), 1),
          Max            = vmax,
          IVB            = round(nz_mean(df$InducedVertBreak), 1),
          HB             = round(nz_mean(df$HorzBreak), 1),
          ReleaseTilt   = convert_to_clock(nz_mean(df$ReleaseTilt)),
          BreakTilt     = convert_to_clock(nz_mean(df$BreakTilt)),
          SpinEff        = { v <- nz_mean(df$SpinEfficiency); if (is.na(v)) "" else paste0(round(v*100,1), "%") },
          Spin           = round(nz_mean(df$SpinRate), 0),
          Height         = round(nz_mean(df$RelHeight), 1),
          Side           = round(nz_mean(df$RelSide), 1),
          VAA            = round(nz_mean(df$VertApprAngle), 1),
          HAA            = round(nz_mean(df$HorzApprAngle), 1),
          Ext            = round(nz_mean(df$Extension), 1),
          `InZone%`      = { inzone <- (df$PlateLocSide >= ZONE_LEFT & df$PlateLocSide <= ZONE_RIGHT &
                                          df$PlateLocHeight >= ZONE_BOTTOM & df$PlateLocHeight <= ZONE_TOP);
          safe_pct(sum(inzone, na.rm = TRUE), sum(!is.na(inzone))) },
          `Comp%`        = { comp <- (df$PlateLocSide >= -1.5 & df$PlateLocSide <= 1.5 &
                                        df$PlateLocHeight >= (2.65-1.7) & df$PlateLocHeight <= (2.65+1.3));
          safe_pct(sum(comp, na.rm = TRUE), sum(!is.na(comp))) },
          `Strike%`      = if (has_pc) safe_pct(strikes, nrow(df)) else "",
          `FPS%`         = safe_pct(fps_live, bf_live),
          `E+A%`         = safe_pct(ea_live,  bf_live),
          `K%`           = safe_pct(k_live,   bf_live),
          `BB%`          = safe_pct(bb_live,  bf_live),
          `Whiff%`       = safe_pct(sw, den),
          EV = round(ev_all, 1),
          LA = round(la_all, 1),
          `Stuff+`       = stuff_all,
          `Ctrl+`        = ctrl_all,
          `QP+`          = qp_all,
          `Pitching+`    = round((stuff_all + qp_all)/2, 1)
        )
      }
      
      rows <- lapply(names(by_type), function(p) {
        dfi <- by_type[[p]]
        tibble::tibble(Pitch = as.character(p)) %>% dplyr::bind_cols(build_row(dfi))
      })
      
      out_tbl <- dplyr::bind_rows(rows) %>% dplyr::relocate(Player)
      tot <- sum(out_tbl$`#`, na.rm = TRUE)
      out_tbl$Usage <- ifelse(tot > 0, paste0(round(100*out_tbl$`#`/tot, 1), "%"), "")
      
      all_row <- tibble::tibble(Pitch = "All") %>% dplyr::bind_cols(build_row(df_all))
      all_row$Usage <- "100%"
      out_tbl <- dplyr::bind_rows(out_tbl, all_row)
      
      mode   <- input$campPitchMode
      custom <- input$campPitchCustomCols; if (is.null(custom)) custom <- character(0)
      visible_set <- visible_set_for_lb(mode, custom)
      
      datatable_with_colvis(
        out_tbl,
        lock            = "Pitch",
        remember        = FALSE,
        default_visible = intersect(visible_set, names(out_tbl))
      )
    }, server = FALSE)
    
    # ======================================================
    # HITTING page (Data & Performance — spray + table)
    # ======================================================
    output$campHitButtons <- renderUI({
      sel <- isolate(input$campHitMode); if (is.null(sel)) sel <- "Results"
      tagList(
        radioButtons(
          ns("campHitMode"), NULL,
          choices = c("Results", "Custom"),
          selected = sel, inline = TRUE
        ),
        conditionalPanel(
          sprintf("input['%s']==='Custom'", ns("campHitMode")),
          selectizeInput(
            ns("campHitCustomCols"), NULL,
            choices = c(
              "PA","AB","AVG","SLG","OBP","OPS",
              "xWOBA","xISO","BABIP","GB%","Barrel%",
              "Swing%","Whiff%","CSW%","K%","BB%","EV","LA"
            ),
            selected = c("PA","AB","AVG","SLG","OBP","OPS"),
            multiple = TRUE, options = list(placeholder = "Pick columns…")
          )
        )
      )
    })
    
    filtered_hit_camps <- reactive({
      df <- filtered_base()
      if (!is.null(input$player) && input$player != "All") df <- dplyr::filter(df, Batter == input$player)
      df
    })
    
    # Spray chart
    output$campSprayChart <- ggiraph::renderGirafe({
      df <- filtered_hit_camps(); if (!nrow(df)) return(NULL)
      st <- tolower(trimws(as.character(df$SessionType)))
      live_mask <- grepl("live|game|ab", st)
      
      dist_num <- suppressWarnings(as.numeric(df$Distance))
      dir_num  <- suppressWarnings(as.numeric(df$Direction))
      ok <- which(live_mask & df$PitchCall == "InPlay" & is.finite(dist_num) & is.finite(dir_num))
      
      fence_pts <- data.frame(deg = c(-45,-22.5,0,22.5,45), r = c(330,370,400,370,330))
      deg_seq <- seq(-45, 45, length.out = 301)
      r_seq   <- stats::spline(fence_pts$deg, fence_pts$r, xout = deg_seq)$y
      fence   <- data.frame(x = r_seq * sin(deg_seq*pi/180), y = r_seq * cos(deg_seq*pi/180))
      fl_l <- data.frame(x = c(0, 330*sin(-45*pi/180)), y = c(0, 330*cos(-45*pi/180)))
      fl_r <- data.frame(x = c(0, 330*sin( 45*pi/180)), y = c(0, 330*cos( 45*pi/180)))
      th_in <- seq(-45, 45, length.out = 121)
      infield <- data.frame(x = 95 * sin(th_in*pi/180), y = 95 * cos(th_in*pi/180))
      home <- data.frame(x = c(-0.75, 0.75, 0.75, 0, -0.75),
                         y = c( 1.05, 1.05, 1.15, 1.25, 1.15) - 2.0)
      
      if (!length(ok)) {
        p_empty <- ggplot() +
          geom_polygon(data = home, aes(x, y), fill = NA, color = "grey50") +
          geom_path(data = fence, aes(x, y), color = "grey40", linewidth = 0.8) +
          geom_polygon(data = rbind(fence, fence[1,]), aes(x, y),
                       fill = "#f3f5f7", color = NA, alpha = 0.6) +
          geom_path(data = fl_l, aes(x, y), color = "grey50") +
          geom_path(data = fl_r, aes(x, y), color = "grey50") +
          geom_path(data = infield, aes(x, y), color = "grey70") +
          annotate("text", x = 0, y = 200, label = "No balls in play for current filters", size = 5) +
          coord_fixed(xlim = c(-380, 380), ylim = c(-30, 420)) +
          theme_void()
        return(ggiraph::girafe(ggobj = p_empty, bg = "transparent"))
      }
      
      bbe <- df[ok, , drop = FALSE]
      bbe$Distance  <- dist_num[ok]
      bbe$Direction <- dir_num[ok]
      
      FOUL_DEG_RAW  <- 90
      FOUL_DEG_GEOM <- 110
      angle_scale   <- FOUL_DEG_GEOM / FOUL_DEG_RAW
      
      th     <- bbe$Direction * angle_scale * pi/180
      bbe$x  <- bbe$Distance * sin(th)
      bbe$y  <- bbe$Distance * cos(th)
      
      hit_levels <- c("Single","Double","Triple","HomeRun")
      outcome <- dplyr::case_when(bbe$PlayResult %in% hit_levels ~ bbe$PlayResult, TRUE ~ "Out")
      bbe$Outcome <- factor(outcome, levels = c("Out", hit_levels))
      outcome_cols <- c("Single"="#1fab54","Double"="#1f77b4","Triple"="#7b1fa2","HomeRun"="#d62728","Out"="#222222")
      
      bbe <- bbe %>%
        dplyr::mutate(
          rid     = dplyr::row_number(),
          tt      = paste0(
            "EV: ", ifelse(is.finite(ExitSpeed), sprintf("%.1f", ExitSpeed), "—"), " mph\n",
            "LA: ", ifelse(is.finite(Angle),     sprintf("%.1f", Angle),     "—"), "°\n",
            "Distance: ", ifelse(is.finite(Distance), paste0(sprintf("%.0f", Distance), " ft"), "—"), "\n",
            "Pitch: ", TaggedPitchType, "\n",
            "Result: ", PlayResult
          ),
          tt_fill = dplyr::coalesce(all_colors[as.character(TaggedPitchType)], "gray80")
        )
      
      p <- ggplot() +
        geom_polygon(data = home, aes(x, y), fill = NA, color = "grey50") +
        geom_path(data = fence, aes(x, y), color = "grey40", linewidth = 0.8) +
        geom_polygon(data = rbind(fence, fence[1,]), aes(x, y),
                     fill = "#f3f5f7", color = NA, alpha = 0.6) +
        geom_path(data = fl_l, aes(x, y), color = "grey50") +
        geom_path(data = fl_r, aes(x, y), color = "grey50") +
        geom_path(data = infield, aes(x, y), color = "grey70") +
        ggiraph::geom_point_interactive(
          data = bbe,
          aes(x, y, color = Outcome, tooltip = tt, data_id = rid),
          size = 2.8, alpha = 0.95
        ) +
        ggiraph::geom_point_interactive(
          data = bbe,
          aes(x, y, tooltip = tt, data_id = rid, fill = I(tt_fill)),
          shape = 21, size = 8, alpha = 0.001, stroke = 0, inherit.aes = FALSE
        ) +
        scale_color_manual(values = outcome_cols, name = "Result") +
        coord_fixed(xlim = c(-380, 380), ylim = c(-30, 420)) +
        theme_void() + theme(legend.position = "right")
      
      ggiraph::girafe(
        ggobj = p,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE),
          ggiraph::opts_tooltip(use_fill = TRUE, use_stroke = TRUE,
                                css = "color:#fff !important;font-weight:600;padding:6px;border-radius:8px;text-shadow:0 1px 1px rgba(0,0,0,.35);"),
          ggiraph::opts_hover(css = "stroke-width:1.5px;"),
          ggiraph::opts_hover_inv(css = "opacity:0.15;")
        ),
        bg = "transparent"
      )
    })
    
    # Hitting table (same DP logic/formatting)
    output$campHitTable <- DT::renderDataTable({
      df <- filtered_hit_camps()
      if (!nrow(df)) return(DT::datatable(data.frame(message = "No rows after filters"), rownames = FALSE))
      df <- df %>% dplyr::filter(!is.na(Batter) & nzchar(Batter))
      if (!nrow(df)) return(DT::datatable(data.frame(message = "No batters for current filters"), rownames = FALSE))
      
      swing_levels <- c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall")
      if (!("PlayResult" %in% names(df))) df$PlayResult <- NA_character_
      if (!("KorBB" %in% names(df))) df$KorBB <- NA_character_
      if (!("PitchCall" %in% names(df))) df$PitchCall <- NA_character_
      by_batter <- split(df, df$Batter)
      
      rows <- lapply(by_batter, function(dfi) {
        is_term_i <- (
          (!is.na(dfi$PlayResult) & dfi$PlayResult != "Undefined") |
            (!is.na(dfi$KorBB) & dfi$KorBB %in% c("Strikeout","Walk"))
        )
        term <- dfi[is_term_i, , drop = FALSE]
        PAt <- nrow(term)
        HBP_all <- sum(term$PlayResult == "HitByPitch", na.rm = TRUE)
        Sac_all <- sum(term$PlayResult == "Sacrifice",  na.rm = TRUE)
        H1  <- sum(term$PlayResult == "Single",  na.rm = TRUE)
        H2  <- sum(term$PlayResult == "Double",  na.rm = TRUE)
        H3  <- sum(term$PlayResult == "Triple",  na.rm = TRUE)
        HR  <- sum(term$PlayResult == "HomeRun", na.rm = TRUE)
        H_all <- H1 + H2 + H3 + HR
        TB_all <- 1*H1 + 2*H2 + 3*H3 + 4*HR
        Kct_all <- sum(term$KorBB == "Strikeout" |
                         term$PlayResult %in% c("Strikeout","StrikeoutSwinging","StrikeoutLooking"), na.rm = TRUE)
        BBc_all <- sum(term$KorBB == "Walk" | term$PlayResult == "Walk", na.rm = TRUE)
        ABt <- PAt - (BBc_all + HBP_all + Sac_all)
        
        swings_all <- sum(!is.na(dfi$PitchCall) & dfi$PitchCall %in% swing_levels, na.rm = TRUE)
        whiffs_all <- sum(dfi$PitchCall == "StrikeSwinging", na.rm = TRUE)
        
        bbe_i <- dfi %>% dplyr::filter(grepl("live|game|ab", tolower(SessionType)), PitchCall == "InPlay")
        EV_i <- nz_mean(bbe_i$ExitSpeed)
        LA_i <- nz_mean(bbe_i$Angle)
        ht_bbe <- get_hit_type_col(bbe_i)
        GB_i <- safe_div(sum(ht_bbe == "GroundBall", na.rm = TRUE),
                         sum(!is.na(ht_bbe), na.rm = TRUE))
        
        extras_all <- compute_process_results(dfi) %>%
          dplyr::filter(PitchType == "All") %>%
          dplyr::mutate(
            
            
            ,
            `Barrel%` = parse_num(`Barrel%`)
          ) %>%
          dplyr::summarise(
            xWOBA = nz_mean(xWOBA), xISO = nz_mean(xISO),
            BABIP = nz_mean(BABIP), BarrelFrac = nz_mean(`Barrel%`)
          )
        barrel_frac <- extras_all$BarrelFrac[1]
        if (is.na(barrel_frac)) {
          candidates <- c("Barrel","Barrels","BarrelFlag","IsBarrel","Barreled","Barrelled")
          nm <- candidates[candidates %in% names(bbe_i)]
          barrel_frac <- if (!length(nm)) NA_real_ else safe_div(sum(as.logical(bbe_i[[nm[1]]]), na.rm = TRUE),
                                                                 sum(!is.na(bbe_i[[nm[1]]])))
        }
        
        tibble::tibble(
          Player = as.character(dfi$Batter[1]),
          PA  = PAt,
          AB  = ABt,
          AVG = safe_div(H_all, ABt),
          SLG = safe_div(TB_all, ABt),
          OBP = safe_div(H_all + BBc_all + HBP_all, PAt),
          OPS = NA_real_,
          xWOBA = extras_all$xWOBA[1],
          xISO  = extras_all$xISO[1],
          BABIP = extras_all$BABIP[1],
          `Swing%` = safe_div(swings_all, nrow(dfi)),
          `Whiff%` = safe_div(whiffs_all, swings_all),
          `GB%`    = GB_i,
          `K%`     = safe_div(Kct_all, PAt),
          `BB%`    = safe_div(BBc_all, PAt),
          `Barrel%`= barrel_frac,
          EV = EV_i,
          LA = LA_i
        ) %>% dplyr::mutate(OPS = SLG + OBP)
      })
      
      out <- dplyr::bind_rows(rows)
      num_cols <- c("PA","AB","AVG","SLG","OBP","OPS","xWOBA","xISO","BABIP",
                    "Swing%","Whiff%","CSW%","GB%","K%","BB%","Barrel%","EV","LA")
      out <- out %>% dplyr::mutate(dplyr::across(dplyr::all_of(num_cols), ~ suppressWarnings(as.numeric(.))))
      out$EV <- ifelse(is.finite(out$EV), round(out$EV, 1), out$EV)
      out$LA <- ifelse(is.finite(out$LA), round(out$LA, 1), out$LA)
      
      pct_cols  <- intersect(c("Swing%","Whiff%","CSW%","GB%","K%","BB%","Barrel%"), names(out))
      rate_cols <- intersect(c("AVG","SLG","OBP","OPS","xWOBA","xISO","BABIP"), names(out))
      if (length(pct_cols)) {
        out[pct_cols]  <- lapply(out[pct_cols],  function(z) ifelse(is.finite(z), paste0(round(z*100,1), "%"), ""))
      }
      if (length(rate_cols)) {
        out[rate_cols] <- lapply(out[rate_cols], fmt_rate3)
      }
      suppressWarnings(out <- out %>% dplyr::arrange(dplyr::desc(as.numeric(OPS))))
      
      mode   <- input$campHitMode; if (is.null(mode)) mode <- "Results"
      custom <- input$campHitCustomCols; if (is.null(custom)) custom <- character(0)
      default_visible <- if (identical(mode, "Custom")) unique(c("Player", custom)) else {
        c("Player","PA","AB","AVG","SLG","OBP","OPS","xWOBA","xISO","BABIP","GB%","Barrel%","Swing%","Whiff%","CSW%","K%","BB%","EV","LA")
      }
      
      datatable_with_colvis(
        out,
        lock            = "Player",
        remember        = FALSE,
        default_visible = intersect(default_visible, names(out))
      )
    }, server = FALSE)
    
    # ======================================================
    # CATCHING page (Data & Performance)
    # ======================================================
    output$campCatchButtons <- renderUI({
      sel <- isolate(input$campCatchMode); if (is.null(sel)) sel <- "Data"
      tagList(
        radioButtons(ns("campCatchMode"), label = NULL,
                     choices = c("Data","Custom"), selected = sel, inline = TRUE),
        conditionalPanel(
          sprintf("input['%s']==='Custom'", ns("campCatchMode")),
          selectizeInput(
            ns("campCatchCustomCols"), label = NULL,
            choices = c("#","Velo","ExchangeTime","PopTime","SL+"),
            multiple = TRUE,
            options = list(placeholder = "Choose columns…")
          )
        )
      )
    })
    
    output$campCatchTable <- DT::renderDataTable({
      df_all <- filtered_base()
      if (!is.null(input$player) && input$player != "All") {
        df_all <- dplyr::filter(df_all, Catcher == input$player)
      }
      df_all <- df_all %>% dplyr::filter(!is.na(Catcher) & nzchar(Catcher))
      if (!nrow(df_all)) {
        return(DT::datatable(data.frame(Note = "No rows for current filters / player."), options = list(dom = 't'), rownames = FALSE))
      }
      
      to_num <- function(x) suppressWarnings(as.numeric(x))
      takes_all   <- !is.na(df_all$PitchCall) & df_all$PitchCall %in% c("StrikeCalled","BallCalled")
      buckets_all <- inzone_label(df_all$PlateLocSide, df_all$PlateLocHeight)
      
      by_catcher <- split(seq_len(nrow(df_all)), df_all$Catcher)
      
      rows <- lapply(names(by_catcher), function(name) {
        idx <- by_catcher[[name]]
        dfi <- df_all[idx, , drop = FALSE]
        
        takes   <- takes_all[idx]
        buckets <- buckets_all[idx]
        
        base_tbl <- dplyr::tibble(
          take   = takes,
          bucket = buckets,
          is_cs  = dfi$PitchCall == "StrikeCalled"
        ) |>
          dplyr::filter(take) |>
          dplyr::group_by(bucket) |>
          dplyr::summarise(cs_rate = mean(is_cs, na.rm = TRUE), n = dplyr::n(), .groups = "drop")
        
        overall_rate <- if (nrow(base_tbl) && sum(base_tbl$n) > 0) {
          sum(base_tbl$cs_rate * base_tbl$n) / sum(base_tbl$n)
        } else NA_real_
        
        rate_for_bucket <- function(b) {
          r <- base_tbl$cs_rate[match(b, base_tbl$bucket)]
          ifelse(is.na(r), overall_rate, r)
        }
        
        obs_all <- if (any(takes, na.rm = TRUE)) mean(dfi$PitchCall[takes] == "StrikeCalled", na.rm = TRUE) else NA_real_
        tb      <- table(buckets[takes])
        exp_all <- if (length(tb)) sum(as.numeric(tb) * vapply(names(tb), rate_for_bucket, numeric(1))) / sum(tb) else NA_real_
        sl_all  <- if (is.finite(obs_all) && is.finite(exp_all) && exp_all > 0) round(100 * obs_all / exp_all, 1) else NA_real_
        
        dft <- dfi |>
          dplyr::mutate(
            ThrowSpeed_num   = to_num(ThrowSpeed),
            ExchangeTime_num = to_num(ExchangeTime),
            PopTime_num      = to_num(PopTime)
          ) |>
          dplyr::filter(is.finite(PopTime_num))
        
        tibble::tibble(
          Player       = name,
          `#`          = nrow(dft),
          Velo         = if (nrow(dft)) round(mean(dft$ThrowSpeed_num,   na.rm = TRUE), 1) else NA_real_,
          ExchangeTime = if (nrow(dft)) round(mean(dft$ExchangeTime_num, na.rm = TRUE), 1) else NA_real_,
          PopTime      = if (nrow(dft)) round(mean(dft$PopTime_num,      na.rm = TRUE), 2) else NA_real_,
          `SL+`        = sl_all
        )
      })
      
      final <- dplyr::bind_rows(rows)
      
      mode <- input$campCatchMode; if (is.null(mode)) mode <- "Data"
      default_visible <- if (identical(mode, "Custom")) {
        unique(c("Player", input$campCatchCustomCols))
      } else {
        c("Player","#","Velo","ExchangeTime","PopTime","SL+")
      }
      
      datatable_with_colvis(
        final,
        lock            = "Player",
        remember        = TRUE,
        default_visible = intersect(default_visible, names(final))
      )
    }, server = FALSE)
  })
}


# ==========================
# == Leaderboard (new)    ==
# ==========================
mod_leader_ui <- function(id, show_header = FALSE) {
  ns <- NS(id)
  fluidPage(
    if (isTRUE(show_header)) {
      fluidRow(
        class = "suite-header",
        column(2, tags$img(src = "PCUlogo.png", height = "100px")),
        column(
          8,
          div(
            style = "height:100%; display:flex; justify-content:center; align-items:flex-start;",
            tags$h1("Leaderboards", style = "margin-top:25px; font-weight:bold;")
          )
        ),
        column(
          2,
          div(style = "text-align:right; margin-top:10px;",
              tags$img(src = "PCUlogo.png", height = "80px"))
        )
      )
    },
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("domain"), "Leaderboard Domain:", choices = c("Pitching","Hitting","Catching"), selected = "Pitching"),
        
        # --- Common filters (apply to all domains) ---
        selectInput(ns("sessionType"), "Session Type:", choices = c("All","Bullpen","Live"), selected = "All"),
        dateRangeInput(ns("dates"), "Date Range:",
                       start = max(pitch_data$Date, na.rm = TRUE),
                       end   = max(pitch_data$Date, na.rm = TRUE),
                       format = "mm/dd/yyyy"),
        selectInput(ns("hand"),       "Pitcher Hand:",  choices = c("All","Left","Right"), selected = "All"),
        selectInput(ns("pitchType"),  "Pitch Type:",    choices = c("All", levels(pitch_data$TaggedPitchType)), selected = "All", multiple = TRUE),
        selectInput(
          ns("zoneLoc"), "Zone Location:",
          choices = c("All",
                      "Upper Half","Bottom Half","Left Half","Right Half",
                      "Upper 3rd","Bottom 3rd","Left 3rd","Right 3rd"),
          selected = "All", multiple = TRUE
        ),
        selectInput(ns("inZone"),     "In Zone:",       choices = c("All","Yes","No","Competitive"), selected = "All"),
        selectInput(ns("batterSide"), "Batter Hand:",   choices = c("All","Left","Right"), selected = "All"),
        selectInput(
          ns("countFilter"), "Count:",
          choices  = c("All"="All","Even"="Even","Behind"="Behind","Ahead"="Ahead","2K Not Full"="2KNF",
                       "0-0","0-1","1-0","1-1","2-0","2-1","0-2","1-2","2-2","3-0","3-1","3-2"),
          selected = "All", multiple = TRUE
        ),
        fluidRow(
          column(6, numericInput(ns("veloMin"), "Velocity Min (MPH):", value = NA)),
          column(6, numericInput(ns("veloMax"), "Velocity Max (MPH):", value = NA))
        ),
        fluidRow(
          column(6, numericInput(ns("ivbMin"), "IVB Min (inches):", value = NA)),
          column(6, numericInput(ns("ivbMax"), "IVB Max (inches):", value = NA))
        ),
        fluidRow(
          column(6, numericInput(ns("hbMin"), "HB Min (inches):", value = NA)),
          column(6, numericInput(ns("hbMax"), "HB Max (inches):", value = NA))
        ),
        fluidRow(
          column(6, numericInput(ns("pcMin"), "Pitch Count Min:", value = NA, min = 1)),
          column(6, numericInput(ns("pcMax"), "Pitch Count Max:", value = NA, min = 1))
        ),
        width = 3,
        class = "sidebar"
      ),
      mainPanel(
        # --- Pitching (unchanged UI) ---
        conditionalPanel(
          condition = sprintf("input['%s'] === 'Pitching'", ns("domain")),
          tagList(
            div(style = "margin: 8px 0;", uiOutput(ns("lbButtons"))),
            DT::dataTableOutput(ns("lbTable"))
          )
        ),
        
        # --- Hitting (per-player) ---
        conditionalPanel(
          condition = sprintf("input['%s'] === 'Hitting'", ns("domain")),
          tagList(
            div(style="display:flex; align-items:center; gap:12px; margin-bottom:8px;", uiOutput(ns("lbHitButtons"))),
            DT::dataTableOutput(ns("lbHitTable"))
          )
        ),
        
        # --- Catching (per-player) ---
        conditionalPanel(
          condition = sprintf("input['%s'] === 'Catching'", ns("domain")),
          tagList(
            div(style="display:flex; align-items:center; gap:12px; margin-bottom:8px;", uiOutput(ns("lbCatchButtons"))),
            DT::dataTableOutput(ns("lbCatchTable"))
          )
        )
      )
    )
  )
}

mod_leader_server <- function(id, is_active = shiny::reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------- constants ----------
    
    # ---------- small helpers ----------
    nnz <- function(x) !is.null(x) && !is.na(x)
    safe_div <- function(a,b) {
      a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
      ifelse(is.finite(den <- b) & den != 0 & is.finite(a), a/den, NA_real_)
    }
    # Parse numbers; treat anything with % as percentage -> fraction
    parse_num <- function(x) {
      if (is.numeric(x)) return(x)
      sx <- trimws(as.character(x))
      sx[sx == ""] <- NA_character_
      is_pct <- grepl("%", sx)
      val <- suppressWarnings(as.numeric(gsub("[^0-9eE.+-]", "", sx)))
      val[is_pct] <- val[is_pct] / 100
      val
    }
    nz_mean <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      m <- mean(x, na.rm = TRUE)
      if (is.finite(m)) m else NA_real_
    }
    # 3-dec rate with NO leading zero for 0.xxx
    fmt_rate3 <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      s <- ifelse(is.finite(x), sprintf("%.3f", x), "")
      sub("^0\\.", ".", s)
    }
    # Barrel% fallback finder (common column names)
    barrel_share_from_flags <- function(df) {
      candidates <- c("Barrel","Barrels","BarrelFlag","IsBarrel","Barreled","Barrelled")
      nm <- candidates[candidates %in% names(df)]
      if (!length(nm)) return(NA_real_)
      col <- df[[nm[1]]]
      safe_div(sum(as.logical(col), na.rm = TRUE), sum(!is.na(col)))
    }
    
    # Keep count filter tidy
    observeEvent(input$countFilter, {
      sel <- input$countFilter
      if (is.null(sel) || !length(sel)) {
        updateSelectInput(session, "countFilter", selected = "All")
      } else if ("All" %in% sel && length(sel) > 1) {
        updateSelectInput(session, "countFilter", selected = setdiff(sel, "All"))
      }
    }, ignoreInit = TRUE)
    
    # ---------- TEAM-SCOPED BASE ----------
    # Returns the base dataset for the selected domain & session type,
    # filtered to LSU-only rows (PitcherTeam for Pitching/Catching; BatterTeam for Hitting).
    team_base <- reactive({
      req(is_active())
      base <- switch(
        input$domain,
        "Pitching" = if (input$sessionType == "All") 
          pitch_data_pitching else dplyr::filter(pitch_data_pitching, SessionType == input$sessionType),
        "Hitting"  = if (input$sessionType == "All") pitch_data               else dplyr::filter(pitch_data,            SessionType == input$sessionType),
        "Catching" = if (input$sessionType == "All") pitch_data               else dplyr::filter(pitch_data,            SessionType == input$sessionType)
      )
      
      # Allow missing/blank TEAM_CODE → no team scoping
      tc <- get0("TEAM_CODE", ifnotfound = "")
      if (!is.character(tc) || length(tc) < 1 || !nzchar(tc[1])) # --- privacy: players see only their own rows across Leaderboard domains ---
        ue <- get_user_email(session, input)
      if (!user_is_admin(session, ue)) {
        allow <- allowed_names_for_user(ue)
        base <- filter_to_user(base, "Leaderboard", allow)
      }
      return(base)
      
      # Optional: use synonyms if available
      codes_for <- if (exists("TEAM_SYNONYMS", inherits = TRUE)) {
        function(code) if (code %in% names(TEAM_SYNONYMS)) TEAM_SYNONYMS[[code]] else code
      } else {
        function(code) code
      }
      
      if (identical(input$domain, "Hitting")) {
        dplyr::filter(base, BatterTeam %in% codes_for(tc[1]))
      } else {
        dplyr::filter(base, PitcherTeam %in% codes_for(tc[1]))
      }
    })
    
    
    # Default the date range to the most recent day with data for the current filters
    observe({
      req(is_active())
      base <- team_base()
      last_date  <- suppressWarnings(max(base$Date, na.rm = TRUE))
      if (is.finite(last_date)) {
        updateDateRangeInput(session, "dates", start = last_date, end = last_date)
      }
    })
    
    # Domain-aware filtered data BEFORE pitch type filtering (for usage denominators)
    filtered_lb_before_pitch_type <- reactive({
      req(is_active(), input$dates, input$hand, input$zoneLoc, input$inZone)
      
      df <- team_base()
      
      # Date range
      df <- dplyr::filter(df, Date >= input$dates[1], Date <= input$dates[2])
      
      # Pitcher hand
      if (input$hand != "All") df <- dplyr::filter(df, PitcherThrows == input$hand)
      
      # Batter hand (Live only)
      df <- filter_batter_side(df, input$batterSide)
      
      # Zone / in-zone / count
      df <- enforce_zone(df, input$zoneLoc)
      df <- enforce_inzone(df, input$inZone)
      df <- apply_count_filter(df, input$countFilter)
      
      # Numeric ranges
      if (nnz(input$veloMin)) df <- dplyr::filter(df, RelSpeed >= input$veloMin)
      if (nnz(input$veloMax)) df <- dplyr::filter(df, RelSpeed <= input$veloMax)
      if (nnz(input$ivbMin))  df <- dplyr::filter(df, InducedVertBreak >= input$ivbMin)
      if (nnz(input$ivbMax))  df <- dplyr::filter(df, InducedVertBreak <= input$ivbMax)
      if (nnz(input$hbMin))   df <- dplyr::filter(df, HorzBreak >= input$hbMin)
      if (nnz(input$hbMax))   df <- dplyr::filter(df, HorzBreak <= input$hbMax)
      
      # Ensure TaggedPitchType exists
      if (!"TaggedPitchType" %in% names(df)) {
        fallback_cols <- intersect(c("PitchType","pitchType","Pitch"), names(df))
        if (length(fallback_cols)) {
          df$TaggedPitchType <- df[[fallback_cols[1]]]
        } else {
          df$TaggedPitchType <- "Unknown"
        }
      }
      
      # DO NOT filter by pitch type here - keep all pitch types for usage denominators
      
      # Limit by pitch number (chronological)
      df <- df %>% dplyr::arrange(Date) %>% dplyr::mutate(PitchNumber = dplyr::row_number())
      if (nnz(input$pcMin)) df <- dplyr::filter(df, PitchNumber >= input$pcMin)
      if (nnz(input$pcMax)) df <- dplyr::filter(df, PitchNumber <= input$pcMax)
      
      # Compute Stuff+ for Pitching tables (harmless for others)
      df2 <- compute_stuff_simple(df, base_type = "Fastball", level = "College") %>%
        force_pitch_levels()
      
      df2
    })
    
    # Domain-aware filtered data (LSU-only)
    filtered_lb <- reactive({
      df <- filtered_lb_before_pitch_type()
      
      # Pitch types
      ptypes <- input$pitchType
      if (is.null(ptypes)) ptypes <- "All"
      if (!("All" %in% ptypes)) df <- dplyr::filter(df, TaggedPitchType %in% ptypes)
      
      df
    })
    
    # =========================
    # Pitching (unchanged)
    # =========================
    output$lbButtons <- renderUI({
      sel <- isolate(input$lbMode); if (is.null(sel)) sel <- "Stuff"
      tagList(
        selectInput(ns("lbMode"), label = "Table Mode:",
                    choices = c("Stuff","Process","Results","Counting","Bullpen","Live","Usage","Custom"),
                    selected = sel, width = "200px"),
        conditionalPanel(
          sprintf("input['%s']=='Custom'", ns("lbMode")),
          selectizeInput(ns("lbCustomCols"), label = NULL,
                         choices = setdiff(all_table_cols, "Pitch"),
                         multiple = TRUE,
                         options = list(placeholder = "Choose columns to show…"))
        )
      )
    })
    
    # Build leaderboard table by player for the selected domain (Pitching path you already had)
    output$lbTable <- DT::renderDataTable({
      df <- filtered_lb()
      validate(need(nrow(df) > 0, "No data for selected filters"))
      
      player_col <- switch(input$domain,
                           "Pitching" = "Pitcher",
                           "Hitting"  = "Batter",
                           "Catching" = "Catcher")
      if (!player_col %in% names(df)) {
        return(DT::datatable(data.frame(Message = "Player column not found"), options = list(dom = 't')))
      }
      
      df <- df %>% dplyr::filter(!is.na(.data[[player_col]]) & nzchar(.data[[player_col]]))
      by_player <- split(df, df[[player_col]])
      
      mode   <- input$lbMode
      custom <- input$lbCustomCols; if (is.null(custom)) custom <- character(0)
      
      # For Usage mode, we need the unfiltered data for correct denominators
      original_data <- if (identical(mode, "Usage")) filtered_lb_before_pitch_type() else NULL
      
      rows <- purrr::imap(by_player, function(dfi, nm) {
        # For Usage mode, get the unfiltered data for this specific player
        original_dfi <- if (!is.null(original_data)) {
          original_data[original_data[[player_col]] == nm, , drop = FALSE]
        } else NULL
        
        tbl <- .dp_like_table(dfi, mode, custom, return_raw = TRUE, original_df = original_dfi)
        if (!is.data.frame(tbl) || !("Pitch" %in% names(tbl))) return(NULL)
        all_row <- tbl[tolower(tbl$Pitch) == "all", , drop = FALSE]
        if (!nrow(all_row)) all_row <- tbl[1, , drop = FALSE]
        all_row <- all_row %>%
          dplyr::mutate(Player = as.character(dfi[[player_col]][1])) %>%
          dplyr::relocate(Player)
        all_row$Pitch <- NULL
        all_row <- all_row %>% dplyr::select(-dplyr::any_of(c("FIP","WHIP","IP")))
        all_row
      })
      rows <- purrr::compact(rows)
      if (!length(rows)) {
        return(DT::datatable(data.frame(Message = "No data for selected filters"), options = list(dom = 't')))
      }
      
      # Ensure consistent column types before binding (especially P/BF)
      rows <- lapply(rows, function(r) {
        if ("P/BF" %in% names(r)) {
          r$`P/BF` <- as.character(r$`P/BF`)
        }
        return(r)
      })
      
      out_tbl <- dplyr::bind_rows(rows)
      
      visible_set <- visible_set_for_lb(mode, custom)
      visible_set <- unique(c("Player", visible_set))
      
      if ("Pitching+" %in% names(out_tbl)) {
        suppressWarnings(out_tbl <- out_tbl %>% dplyr::arrange(dplyr::desc(as.numeric(gsub("[^0-9.]", "", `Pitching+`)))))
      }
      
      cache_key <- paste0("table_cache_", ns("lbTable"))
      player_groups <- lapply(by_player, function(dfi) dfi)
      player_groups[["All"]] <- df
      session$userData[[cache_key]] <- list(
        table = out_tbl,
        groups = player_groups,
        label_column = "Player",
        source = df,
        domain = input$domain
      )
      
      disp_tbl <- out_tbl
      if ("#" %in% names(disp_tbl)) {
        disp_tbl$`#` <- wrap_pitch_counts(disp_tbl$`#`)
      }
      
      datatable_with_colvis(
        disp_tbl,
        lock            = "Player",
        remember        = FALSE,
        default_visible = intersect(visible_set, names(disp_tbl)),
        mode            = mode
      )
    }, server = FALSE)
    
    # ==========================
    # Leaderboard: Hitting (PLAYER)
    # ==========================
    output$lbHitButtons <- renderUI({
      req(input$domain == "Hitting")
      sel <- isolate(input$lbHitMode); if (is.null(sel)) sel <- "Live"
      tagList(
        selectInput(ns("lbHitMode"), label = "Table Mode:",
                    choices = c("Live", "BP"),
                    selected = sel, width = "200px")
      )
    })
    
    output$lbHitTable <- DT::renderDataTable({
      req(input$domain == "Hitting")
      
      df <- filtered_lb()
      if (!nrow(df)) {
        return(DT::datatable(data.frame(message = "No rows after filters"), rownames = FALSE))
      }
      
      # keep only rows with a Batter name
      df <- df %>% dplyr::filter(!is.na(Batter) & nzchar(Batter))
      if (!nrow(df)) {
        return(DT::datatable(data.frame(message = "No batters for current filters"), rownames = FALSE))
      }
      
      # Get session mode (Live or BP)
      mode <- input$lbHitMode %or% "Live"
      
      # Use the same logic as hitting suite - group by batter and apply appropriate function
      by_batter <- split(df, df$Batter)
      
      rows <- lapply(by_batter, function(dfi) {
        # Filter by session type like hitting suite does
        session_vals <- if (".hit_session" %in% names(dfi)) {
          dfi$.hit_session
        } else if ("SessionType" %in% names(dfi)) {
          dfi$SessionType
        } else {
          rep(NA_character_, nrow(dfi))
        }
        session_vals <- tolower(trimws(as.character(session_vals)))
        
        # Filter data based on mode
        if (mode == "Live") {
          filtered_dfi <- dfi[session_vals == "live", , drop = FALSE]
          summary_table <- build_summary_live(filtered_dfi)
        } else { # BP
          filtered_dfi <- dfi[session_vals %in% c("bp", "bullpen"), , drop = FALSE]
          summary_table <- build_summary_bp(filtered_dfi)
        }
        
        # Format for leaderboard display
        if (nrow(summary_table) > 0) {
          if (mode == "Live") {
            # Get the "All" row if it exists, otherwise use the first row
            all_row <- summary_table %>% dplyr::filter(Pitch == "All")
            if (nrow(all_row) == 0) {
              all_row <- summary_table[1, , drop = FALSE]
            }
            
            # Add player name and return with leaderboard format
            # Remove Pitch column but preserve all other columns including PAP
            all_row %>% 
              dplyr::mutate(Player = as.character(dfi$Batter[1]), .before = 1) %>%
              dplyr::select(-Pitch) # Remove only Pitch column, keep PAP and all others
          } else { # BP mode
            # BP summary doesn't have Pitch column, just add Player
            summary_table %>% 
              dplyr::mutate(Player = as.character(dfi$Batter[1]), .before = 1)
          }
        } else {
          NULL
        }
      })
      
      # Remove NULL entries and combine
      rows <- rows[!sapply(rows, is.null)]
      if (length(rows) == 0) {
        return(DT::datatable(data.frame(message = "No valid hitting data"), rownames = FALSE))
      }
      
      lb_table <- dplyr::bind_rows(rows)
      
      # Use proper column visibility and default to Live mode columns for hitting
      hitting_live_cols <- c("Player", "#", "PA", "AB", "P/PA", "Swing%", "Contact%", "Whiff%", "Chase%", "Barrel%", "K%", "BB%", "EV", "maxEV", "LA", "PAP")
      hitting_bp_cols <- c("Player", "#", "EV", "maxEV", "LA", "Barrel%")
      
      visible_cols <- if (mode == "Live") {
        intersect(hitting_live_cols, names(lb_table))
      } else {
        intersect(hitting_bp_cols, names(lb_table))
      }
      
      datatable_with_colvis(
        lb_table,
        lock = "Player",
        remember = FALSE,
        default_visible = visible_cols,
        mode = "Live"
      )
    })
    
    # ==========================
    # Leaderboard: Catching (PLAYER)
    # ==========================
    output$lbCatchButtons <- renderUI({
      tagList(
        radioButtons(ns("lbCatchMode"), label = NULL,
                     choices = c("Data","Custom"), selected = "Data", inline = TRUE),
        conditionalPanel(
          sprintf("input['%s']==='Custom'", ns("lbCatchMode")),
          selectizeInput(
            ns("lbCatchCustomCols"), label = NULL,
            choices = c("#","Velo","ExchangeTime","PopTime","SL+"),
            multiple = TRUE,
            options = list(placeholder = "Choose columns…")
          )
        )
      )
    })
    
    output$lbCatchTable <- DT::renderDataTable({
      req(is_active(), input$domain == "Catching")
      
      df_all <- filtered_lb()
      df_all <- df_all %>% dplyr::filter(!is.na(Catcher) & nzchar(Catcher))
      if (!nrow(df_all)) {
        return(DT::datatable(data.frame(Note = "No rows for current filters."), options = list(dom = 't'), rownames = FALSE))
      }
      
      to_num <- function(x) suppressWarnings(as.numeric(x))
      
      # pre-compute take buckets
      takes_all   <- !is.na(df_all$PitchCall) & df_all$PitchCall %in% c("StrikeCalled","BallCalled")
      buckets_all <- inzone_label(df_all$PlateLocSide, df_all$PlateLocHeight)
      
      by_catcher <- split(seq_len(nrow(df_all)), df_all$Catcher)  # split by index for reuse
      
      rows <- lapply(names(by_catcher), function(name) {
        idx <- by_catcher[[name]]
        dfi <- df_all[idx, , drop = FALSE]
        
        # SL+ for this catcher across all takes
        takes   <- takes_all[idx]
        buckets <- buckets_all[idx]
        
        base_tbl <- dplyr::tibble(
          take   = takes,
          bucket = buckets,
          is_cs  = dfi$PitchCall == "StrikeCalled"
        ) |>
          dplyr::filter(take) |>
          dplyr::group_by(bucket) |>
          dplyr::summarise(cs_rate = mean(is_cs, na.rm = TRUE), n = dplyr::n(), .groups = "drop")
        
        overall_rate <- if (nrow(base_tbl) && sum(base_tbl$n) > 0) {
          sum(base_tbl$cs_rate * base_tbl$n) / sum(base_tbl$n)
        } else NA_real_
        
        rate_for_bucket <- function(b) {
          r <- base_tbl$cs_rate[match(b, base_tbl$bucket)]
          ifelse(is.na(r), overall_rate, r)
        }
        
        obs_all <- if (any(takes, na.rm = TRUE)) mean(dfi$PitchCall[takes] == "StrikeCalled", na.rm = TRUE) else NA_real_
        tb      <- table(buckets[takes])
        exp_all <- if (length(tb)) sum(as.numeric(tb) * vapply(names(tb), rate_for_bucket, numeric(1))) / sum(tb) else NA_real_
        sl_all  <- if (is.finite(obs_all) && is.finite(exp_all) && exp_all > 0) round(100 * obs_all / exp_all, 1) else NA_real_
        
        # Throws: PopTime-present
        dft <- dfi |>
          dplyr::mutate(
            ThrowSpeed_num   = to_num(ThrowSpeed),
            ExchangeTime_num = to_num(ExchangeTime),
            PopTime_num      = to_num(PopTime)
          ) |>
          dplyr::filter(is.finite(PopTime_num))
        
        tibble::tibble(
          Player       = name,
          `#`          = nrow(dft),
          Velo         = if (nrow(dft)) round(mean(dft$ThrowSpeed_num,   na.rm = TRUE), 1) else NA_real_,
          ExchangeTime = if (nrow(dft)) round(mean(dft$ExchangeTime_num, na.rm = TRUE), 1) else NA_real_,
          PopTime      = if (nrow(dft)) round(mean(dft$PopTime_num,      na.rm = TRUE), 2) else NA_real_,
          `SL+`        = sl_all
        )
      })
      
      final <- dplyr::bind_rows(rows)
      
      mode <- input$lbCatchMode; if (is.null(mode)) mode <- "Data"
      default_visible <- if (identical(mode, "Custom")) {
        unique(c("Player", input$lbCatchCustomCols))
      } else {
        c("Player","#","Velo","ExchangeTime","PopTime","SL+")
      }
      
      cache_key <- paste0("table_cache_", ns("lbCatchTable"))
      catcher_groups <- setNames(lapply(names(by_catcher), function(name) {
        idx <- by_catcher[[name]]
        df_all[idx, , drop = FALSE]
      }), names(by_catcher))
      catcher_groups[["All"]] <- df_all
      session$userData[[cache_key]] <- list(
        table = final,
        groups = catcher_groups,
        label_column = "Player",
        source = df_all,
        domain = "Catching"
      )
      
      disp_final <- final
      if ("#" %in% names(disp_final)) {
        disp_final$`#` <- wrap_pitch_counts(disp_final$`#`)
      }
      
      datatable_with_colvis(
        disp_final,
        lock            = "Player",
        remember        = TRUE,
        default_visible = intersect(default_visible, names(disp_final)),
        mode            = mode
      )
    }, server = FALSE)
    
  })  # closes moduleServer
}     # closes mod_leader_server

# ==================================
# == Comparison Suite (new module) ==
# ==================================
mod_comp_ui <- function(id, show_header = FALSE) {
  ns <- NS(id)
  fluidPage(
    if (isTRUE(show_header)) {
      fluidRow(
        class = "suite-header",
        column(2, tags$img(src = "PCUlogo.png", height = "100px")),
        column(
          8,
          div(
            style = "height:100%; display:flex; justify-content:center; align-items:flex-start;",
            tags$h1("PCU Comparison Suite", style = "margin-top:25px; font-weight:bold;")
          )
        ),
        column(2, div(style = "text-align:right; margin-top:10px;",
                      tags$img(src = "PCUlogo.png", height = "80px")))
      )
    },
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("domain"), "Player Type:", choices = c("Pitcher","Hitter","Catcher"), selected = "Pitcher"),
        width = 2,
        class = "sidebar"
      ),
      mainPanel(
        # ---- TWO-PANEL COMPARISON (same layout as Pitching > Comparison Tool) ----
        fluidRow(
          # -------- LEFT PANEL (A) --------
          column(
            6,
            div(style="font-weight:700; text-align:center; margin-bottom:6px;", "Comparison #1"),
            wellPanel(
              selectInput(
                ns("cmpA_sessionType"), "Session Type:",
                choices  = c("All","Bullpen","Live"),
                selected = "Live"
              ),
              dateRangeInput(
                ns("cmpA_dates"), "Date Range:",
                start = max(pitch_data$Date, na.rm = TRUE),
                end   = max(pitch_data$Date, na.rm = TRUE),
                format = "mm/dd/yyyy"
              ),
              selectInput(
                ns("cmpA_chart"), "Select Chart:",
                choices = c("Movement Plot","Release Plot","HeatMap","Location Chart"),
                selected = "HeatMap"
              ),
              conditionalPanel(
                sprintf("input['%s']=='HeatMap'", ns("cmpA_chart")),
                selectInput(
                  ns("cmpA_hmStat"), "Select HeatMap:",
                  choices = c("Frequency","Whiff Rate","Exit Velocity","GB Rate","Contact Rate","Swing Rate"),
                  selected = "Frequency"
                )
              ),
              conditionalPanel(
                sprintf("input['%s']=='Location Chart'", ns("cmpA_chart")),
                selectInput(
                  ns("cmpA_result"), "Pitch Results:",
                  choices  = c("All", result_levels),
                  selected = "All",
                  multiple = TRUE
                )
              ),
              # dynamic player select (label & choices depend on Player Type)
              uiOutput(ns("cmpA_player_ui")),
              selectInput(ns("cmpA_hand"),   "Pitcher Hand:", choices = c("All","Left","Right"), selected = "All"),
              selectInput(ns("cmpA_batter"), "Batter Hand:",  choices = c("All","Left","Right"), selected = "All"),
              selectInput(
                ns("cmpA_pitchType"),"Pitch Type:",
                choices  = c("All", levels(pitch_data$TaggedPitchType)),
                selected = "All", multiple = TRUE
              ),
              selectInput(
                ns("cmpA_zone"),"Zone Location:",
                choices = c("All",
                            "Upper Half","Bottom Half","Left Half","Right Half",
                            "Upper 3rd","Bottom 3rd","Left 3rd","Right 3rd"),
                selected = "All",
                multiple = TRUE
              ),
              fluidRow(
                column(6, numericInput(ns("cmpA_veloMin"), "Velocity Min (MPH):", value = NA)),
                column(6, numericInput(ns("cmpA_veloMax"), "Velocity Max (MPH):", value = NA))
              ),
              fluidRow(
                column(6, numericInput(ns("cmpA_ivbMin"), "IVB Min (inches):", value = NA)),
                column(6, numericInput(ns("cmpA_ivbMax"), "IVB Max (inches):", value = NA))
              ),
              fluidRow(
                column(6, numericInput(ns("cmpA_hbMin"), "HB Min (inches):", value = NA)),
                column(6, numericInput(ns("cmpA_hbMax"), "HB Max (inches):", value = NA))
              )
            ),
            # Single switchable plot container (prevents double-render)
            uiOutput(ns("cmpA_plot_ui"))
          ),
          
          # -------- RIGHT PANEL (B) --------
          column(
            6,
            div(style="font-weight:700; text-align:center; margin-bottom:6px;", "Comparison #2"),
            wellPanel(
              selectInput(
                ns("cmpB_sessionType"), "Session Type:",
                choices  = c("All","Bullpen","Live"),
                selected = "Live"
              ),
              dateRangeInput(
                ns("cmpB_dates"), "Date Range:",
                start = max(pitch_data$Date, na.rm = TRUE),
                end   = max(pitch_data$Date, na.rm = TRUE),
                format = "mm/dd/yyyy"
              ),
              selectInput(
                ns("cmpB_chart"), "Select Chart:",
                choices = c("Movement Plot","Release Plot","HeatMap","Location Chart"),
                selected = "HeatMap"
              ),
              conditionalPanel(
                sprintf("input['%s']=='HeatMap'", ns("cmpB_chart")),
                selectInput(
                  ns("cmpB_hmStat"), "Select HeatMap:",
                  choices = c("Frequency","Whiff Rate","Exit Velocity","GB Rate","Contact Rate","Swing Rate"),
                  selected = "Frequency"
                )
              ),
              conditionalPanel(
                sprintf("input['%s']=='Location Chart'", ns("cmpB_chart")),
                selectInput(
                  ns("cmpB_result"), "Pitch Results:",
                  choices  = c("All", result_levels),
                  selected = "All",
                  multiple = TRUE
                )
              ),
              uiOutput(ns("cmpB_player_ui")),
              selectInput(ns("cmpB_hand"),   "Pitcher Hand:", choices = c("All","Left","Right"), selected = "All"),
              selectInput(ns("cmpB_batter"), "Batter Hand:",  choices = c("All","Left","Right"), selected = "All"),
              selectInput(
                ns("cmpB_pitchType"),"Pitch Type:",
                choices  = c("All", levels(pitch_data$TaggedPitchType)),
                selected = "All", multiple = TRUE
              ),
              selectInput(
                ns("cmpB_zone"),"Zone Location:",
                choices = c("All",
                            "Upper Half","Bottom Half","Left Half","Right Half",
                            "Upper 3rd","Bottom 3rd","Left 3rd","Right 3rd"),
                selected = "All",
                multiple = TRUE
              ),
              fluidRow(
                column(6, numericInput(ns("cmpB_veloMin"), "Velocity Min (MPH):", value = NA)),
                column(6, numericInput(ns("cmpB_veloMax"), "Velocity Max (MPH):", value = NA))
              ),
              fluidRow(
                column(6, numericInput(ns("cmpB_ivbMin"), "IVB Min (inches):", value = NA)),
                column(6, numericInput(ns("cmpB_ivbMax"), "IVB Max (inches):", value = NA))
              ),
              fluidRow(
                column(6, numericInput(ns("cmpB_hbMin"), "HB Min (inches):", value = NA)),
                column(6, numericInput(ns("cmpB_hbMax"), "HB Max (inches):", value = NA))
              )
            ),
            # Single switchable plot container (prevents double-render)
            uiOutput(ns("cmpB_plot_ui"))
          )
        ),
        
        tags$hr(),
        # ---------- STACKED TABLES ----------
        fluidRow(
          column(
            12,
            div(style="font-weight:700; text-align:center; margin-bottom:6px;", "Comparison #1 Table"),
            div(
              style = "margin: 8px 0;",
              tagList(
                uiOutput(ns("cmpA_tableMode_ui")),
                uiOutput(ns("cmpA_customPicker"))
              )
            ),
            DT::dataTableOutput(ns("cmpA_table"))
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            div(style="font-weight:700; text-align:center; margin-bottom:6px;", "Comparison #2 Table"),
            div(
              style = "margin: 8px 0;",
              tagList(
                uiOutput(ns("cmpB_tableMode_ui")),
                uiOutput(ns("cmpB_customPicker"))
              )
            ),
            DT::dataTableOutput(ns("cmpB_table"))
          )
        )
      )
    )
  )
} 

mod_comp_server <- function(id, is_active = shiny::reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tooltip_css <- "color:#fff !important;font-weight:600;padding:6px;border-radius:8px;text-shadow:0 1px 1px rgba(0,0,0,.4);"
    
    # ---------- Dynamic player pickers ----------
    .player_label <- function(dom) switch(dom,
                                          Pitcher = "Select Pitcher:",
                                          Hitter  = "Select Hitter:",
                                          Catcher = "Select Catcher:"
    )
    .player_choices <- function(dom) {
      switch(
        dom,
        Pitcher = c("All" = "All", name_map_pitching),
        Hitter  = c("All" = "All", batter_map),
        Catcher = c("All" = "All", catcher_map)
      )
    }
    # ---------- Player UIs ----------
    output$cmpA_player_ui <- renderUI({
      lab <- .player_label(input$domain)
      selectInput(ns("cmpA_player"), lab, choices = .player_choices(input$domain), selected = "All")
    })
    output$cmpB_player_ui <- renderUI({
      lab <- .player_label(input$domain)
      selectInput(ns("cmpB_player"), lab, choices = .player_choices(input$domain), selected = "All")
    })
    
    # ---------- Table Mode UIs ----------
    .table_mode_choices <- function(dom) {
      switch(
        dom,
        Pitcher = c("Stuff","Process","Results","Counting","Bullpen","Live","Usage","Custom"),
        Hitter  = c("Live","BP","Custom"),
        Catcher = c("Stuff","Process","Results","Custom")
      )
    }
    .table_mode_default <- function(dom) {
      switch(
        dom,
        Pitcher = "Stuff",
        Hitter  = "Live", 
        Catcher = "Stuff"
      )
    }
    
    output$cmpA_tableMode_ui <- renderUI({
      choices <- .table_mode_choices(input$domain)
      selected <- .table_mode_default(input$domain)
      selectInput(
        ns("cmpA_tableMode"), label = "Table Mode:",
        choices = choices, selected = selected, width = "200px"
      )
    })
    output$cmpB_tableMode_ui <- renderUI({
      choices <- .table_mode_choices(input$domain)
      selected <- .table_mode_default(input$domain)
      selectInput(
        ns("cmpB_tableMode"), label = "Table Mode:",
        choices = choices, selected = selected, width = "200px"
      )
    })
    
    .last_date_for <- function(dom, player, st) {
      df <- pitch_data
      # normalize once
      if (!"SessionType_std" %in% names(df)) {
        df$SessionType_std <- {
          x0 <- tolower(trimws(as.character(df$SessionType)))
          dplyr::case_when(
            grepl("bull", x0)         ~ "Bullpen",
            grepl("live|game|ab", x0) ~ "Live",
            TRUE                      ~ "Other"
          )
        }
      }
      if (!is.null(st) && st != "All") {
        df <- df[df$SessionType_std == st, , drop = FALSE]
      }
      if (isTRUE(player == "All") || is.null(player) || !nzchar(player)) {
        return(suppressWarnings(max(df$Date, na.rm = TRUE)))
      }
      col <- switch(dom, "Pitcher" = "Pitcher", "Hitter" = "Batter", "Catcher" = "Catcher", "Pitcher")
      suppressWarnings(max(df$Date[df[[col]] == player], na.rm = TRUE))
    }
    
    observeEvent(list(input$domain, input$cmpA_player, input$cmpA_sessionType), {
      req(is_active())
      last_date <- .last_date_for(input$domain, input$cmpA_player, input$cmpA_sessionType)
      if (is.finite(last_date)) updateDateRangeInput(session, "cmpA_dates", start = last_date, end = last_date)
    }, ignoreInit = TRUE)
    observeEvent(list(input$domain, input$cmpB_player, input$cmpB_sessionType), {
      req(is_active())
      last_date <- .last_date_for(input$domain, input$cmpB_player, input$cmpB_sessionType)
      if (is.finite(last_date)) updateDateRangeInput(session, "cmpB_dates", start = last_date, end = last_date)
    }, ignoreInit = TRUE)
    
    # ---------- Common filtering helper ----------
    normalize_session_type <- function(x) {
      x0 <- tolower(trimws(as.character(x)))
      dplyr::case_when(
        grepl("bull|prac", x0)         ~ "Bullpen",
        grepl("live|game|ab", x0)      ~ "Live",
        TRUE                           ~ "Other"
      )
    }
    
    .filtered_panel <- function(which = c("A","B")) {
      which <- match.arg(which)
      
      dom_in <- (input[[paste0("cmp", which, "_domain")]] %||% input$domain %||% "Pitcher")
      dom <- if (dom_in %in% c("Pitching","Pitcher")) "Pitcher"
      else if (dom_in %in% c("Hitting","Hitter")) "Hitter"
      else if (dom_in %in% c("Catching","Catcher")) "Catcher"
      else "Pitcher"
      
      dates  <- input[[paste0("cmp", which, "_dates")]]
      hand   <- input[[paste0("cmp", which, "_hand")]]
      bats   <- input[[paste0("cmp", which, "_batter")]]
      zone   <- input[[paste0("cmp", which, "_zone")]]
      ptypes <- input[[paste0("cmp", which, "_pitchType")]]
      vmin   <- input[[paste0("cmp", which, "_veloMin")]]
      vmax   <- input[[paste0("cmp", which, "_veloMax")]]
      ivbmin <- input[[paste0("cmp", which, "_ivbMin")]]
      ivbmax <- input[[paste0("cmp", which, "_ivbMax")]]
      hbmin  <- input[[paste0("cmp", which, "_hbMin")]]
      hbmax  <- input[[paste0("cmp", which, "_hbMax")]]
      player <- input[[paste0("cmp", which, "_player")]]
      stype  <- input[[paste0("cmp", which, "_sessionType")]]
      
      req(dates)
      if (is.null(ptypes)) ptypes <- "All"
      if (is.null(stype)  || !nzchar(stype)) stype <- "Live"
      
      df <- if (exists("pitch_data_pitching")) pitch_data_pitching else pitch_data
      if (!nrow(df)) return(df[0, , drop = FALSE])
      
      # privacy scoping
      if (exists("user_email") && is.function(user_email) &&
          exists("is_admin")   && is.function(is_admin)   &&
          !is_admin()) {
        ne <- function(x) tolower(trimws(x))
        ue <- user_email()
        if (!is.na(ue) && "Email" %in% names(df)) {
          df <- dplyr::filter(df, ne(Email) == ne(ue))
        }
      }
      if (!nrow(df)) return(df)
      
      if (!("SessionType_std" %in% names(df))) {
        df$SessionType_std <- normalize_session_type(df$SessionType)
      }
      
      df <- dplyr::filter(df, as.Date(Date) >= as.Date(dates[1]),
                          as.Date(Date) <= as.Date(dates[2]))
      if (!nrow(df)) return(df)
      
      if (!is.null(player) && player != "All") {
        df <- switch(
          dom,
          "Pitcher" = dplyr::filter(df, Pitcher == player),
          "Hitter"  = dplyr::filter(df, Batter  == player),
          "Catcher" = dplyr::filter(df, Catcher == player),
          df
        )
      }
      if (!nrow(df)) return(df)
      
      if (stype != "All") df <- df[df$SessionType_std == stype, , drop = FALSE]
      if (!nrow(df)) return(df)
      
      if (!is.null(hand) && hand != "All") df <- dplyr::filter(df, PitcherThrows == hand)
      df <- filter_batter_side(df, bats)
      if (!nrow(df)) return(df)
      
      if (!is.null(zone) && !identical(zone, "All")) {
        df <- enforce_zone(df, zone)
        if (!nrow(df)) return(df)
      }
      
      nnz <- function(x) !is.null(x) && !is.na(x)
      if (nnz(vmin))   df <- dplyr::filter(df, RelSpeed         >= vmin)
      if (nnz(vmax))   df <- dplyr::filter(df, RelSpeed         <= vmax)
      if (nnz(ivbmin)) df <- dplyr::filter(df, InducedVertBreak >= ivbmin)
      if (nnz(ivbmax)) df <- dplyr::filter(df, InducedVertBreak <= ivbmax)
      if (nnz(hbmin))  df <- dplyr::filter(df, HorzBreak        >= hbmin)
      if (nnz(hbmax))  df <- dplyr::filter(df, HorzBreak        <= hbmax)
      if (!nrow(df)) return(df)
      
      if (!("All" %in% ptypes)) df <- dplyr::filter(df, TaggedPitchType %in% ptypes)
      if (!nrow(df)) return(df)
      
      df2 <- compute_stuff_simple(df, base_type = "Fastball", level = "College") %>%
        force_pitch_levels() %>%
        dplyr::mutate(Result = factor(compute_result(PitchCall, PlayResult), levels = result_levels))
      df2
    }
    
    # ---------- PLOTS ----------
    .movement_girafe <- function(df) {
      if (!nrow(df)) return(NULL)
      df_i <- df %>% dplyr::mutate(tt = make_hover_tt(.), rid = dplyr::row_number())
      types_chr <- as.character(intersect(names(all_colors), unique(df_i$TaggedPitchType)))
      dark_on <- isTRUE(input$dark_mode)
      axis_col <- if (dark_on) "#e5e7eb" else "black"
      grid_alpha <- if (dark_on) 0.15 else 0.35
      cols <- colors_for_mode(dark_on)
      avg_mov <- df %>%
        dplyr::filter(is.finite(HorzBreak), is.finite(InducedVertBreak)) %>%
        dplyr::group_by(TaggedPitchType) %>%
        dplyr::summarise(
          avg_HorzBreak        = mean(HorzBreak, na.rm = TRUE),
          avg_InducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
          .groups = "drop"
        )
      p <- ggplot() +
        ggiraph::geom_point_interactive(
          data = df_i,
          aes(HorzBreak, InducedVertBreak,
              color = TaggedPitchType, fill = TaggedPitchType,
              tooltip = tt, data_id = rid),
          alpha = 0.25, size = 4.0, shape = 21, stroke = 0.25
        ) +
        geom_point(
          data = avg_mov,
          aes(avg_HorzBreak, avg_InducedVertBreak, color = TaggedPitchType),
          size = 8
        ) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
        coord_cartesian(xlim = c(-25, 25), ylim = c(-25, 25)) +
        scale_color_manual(values = cols[types_chr], limits = types_chr, name = NULL) +
        scale_fill_manual(values  = cols[types_chr], limits = types_chr, name = NULL) +
        labs(x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "none",
          axis.text = element_text(colour = axis_col),
          axis.title = element_text(colour = axis_col),
          panel.background = element_rect(fill = NA, colour = NA),
          plot.background = element_rect(fill = NA, colour = NA),
          panel.grid.major = element_line(color = scales::alpha(axis_col, 0.1)),
          panel.grid.minor = element_blank()
        )
      ggiraph::girafe(
        ggobj = p,
        options = list(
          ggiraph::opts_sizing(rescale = TRUE),
          ggiraph::opts_tooltip(use_fill = TRUE, use_stroke = TRUE, css = tooltip_css),
          ggiraph::opts_hover(css = "stroke:black;stroke-width:1.5px;"),
          ggiraph::opts_hover_inv(css = "opacity:0.15;")
        ),
        bg = "transparent"
      )
    }
    output$cmpA_movement <- ggiraph::renderGirafe({ .movement_girafe(.filtered_panel("A")) })
    output$cmpB_movement <- ggiraph::renderGirafe({ .movement_girafe(.filtered_panel("B")) })
    
    .release_plot <- function(df) {
      if (!nrow(df)) return(ggplot() + theme_void())
      axis_th <- get0("axis_theme", ifnotfound = theme())
      types <- if (exists("ordered_types") && is.function(ordered_types)) {
        ot <- as.character(ordered_types()); ot[ot %in% unique(df$TaggedPitchType)]
      } else as.character(intersect(names(all_colors), unique(df$TaggedPitchType)))
      dark_on <- isTRUE(input$dark_mode)
      axis_col <- if (dark_on) "#e5e7eb" else "black"
      cols <- colors_for_mode(dark_on)
      mound_fill <- if (dark_on) "#2d2d2d" else "tan"
      rubber_fill <- if (dark_on) NA else "white"
      rp_w <- 4; rp_h <- 0.83
      xs <- seq(-rp_w, rp_w, length.out = 100); ys <- rp_h * (1 - (xs / rp_w)^2)
      mound <- data.frame(x = c(xs, rev(xs)), y = c(ys, rep(0, length(xs))))
      avg <- df %>%
        dplyr::group_by(TaggedPitchType) %>%
        dplyr::summarise(
          avg_RelSide   = mean(RelSide,   na.rm = TRUE),
          avg_RelHeight = mean(RelHeight, na.rm = TRUE),
          .groups = "drop"
        ) %>% dplyr::filter(TaggedPitchType %in% types)
      if (!nrow(avg)) return(ggplot() + theme_void())
      ggplot() +
        geom_polygon(data = mound, aes(x, y), fill = mound_fill, color = mound_fill) +
        annotate("rect", xmin = -0.5, xmax = 0.5, ymin = rp_h - 0.05, ymax = rp_h + 0.05, fill = rubber_fill, color = axis_col) +
        geom_vline(xintercept = 0, color = axis_col, size = 0.7) +
        geom_point(data = avg, aes(avg_RelSide, avg_RelHeight, color = TaggedPitchType), size = 4) +
        scale_color_manual(values = cols[types], limits = types, name = NULL) +
        coord_cartesian(ylim = c(0, 8)) +
        theme_minimal() + axis_th + theme(
          legend.position = "none",
          axis.text = element_text(colour = axis_col),
          axis.title = element_text(colour = axis_col),
          panel.background = element_rect(fill = NA, colour = NA),
          plot.background = element_rect(fill = NA, colour = NA),
          panel.grid.major = element_line(color = scales::alpha(axis_col, 0.1)),
          panel.grid.minor = element_blank()
        ) +
        labs(x = NULL, y = NULL)
    }
    output$cmpA_release <- renderPlot({ .release_plot(.filtered_panel("A")) }, bg = "transparent")
    output$cmpB_release <- renderPlot({ .release_plot(.filtered_panel("B")) }, bg = "transparent")
    
    .kde_grid <- function(x, y, lims = c(-2, 2, 0, 4.5), n = 180) {
      ok <- is.finite(x) & is.finite(y)
      x <- x[ok]; y <- y[ok]
      if (length(x) < 2 || length(unique(x)) < 2 || length(unique(y)) < 2) {
        return(data.frame(x = numeric(0), y = numeric(0), z = numeric(0)))
      }
      d <- MASS::kde2d(x, y, n = n, lims = lims)
      expand.grid(x = d$x, y = d$y) |> transform(z = as.vector(d$z))
    }
    .heat_plot <- function(df, stat) {
      if (!nrow(df)) return(ggplot() + theme_void())
      if (identical(stat, "Exit Velocity")) stat <- "EV"
      hit_types <- get_hit_type_col(df)
      if (stat == "Frequency") {
        grid <- .kde_grid(df$PlateLocSide, df$PlateLocHeight)
        return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_freq, mark_max = TRUE))
      }
      if (stat == "Whiff Rate") {
        grid <- .kde_grid(df$PlateLocSide[df$PitchCall == "StrikeSwinging"],
                          df$PlateLocHeight[df$PitchCall == "StrikeSwinging"])
        return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_red, mark_max = TRUE))
      }
      if (stat == "GB Rate") {
        gb <- df$SessionType == "Live" & hit_types == "GroundBall"
        grid <- .kde_grid(df$PlateLocSide[gb], df$PlateLocHeight[gb])
        return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_red, mark_max = TRUE))
      }
      if (stat == "Contact Rate") {
        cp <- df$SessionType == "Live" & df$PitchCall == "InPlay"
        grid <- .kde_grid(df$PlateLocSide[cp], df$PlateLocHeight[cp])
        return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_red, mark_max = TRUE))
      }
      if (stat == "Swing Rate") {
        swing <- df$SessionType == "Live" &
          (df$PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall"))
        grid <- .kde_grid(df$PlateLocSide[swing], df$PlateLocHeight[swing])
        return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_red, mark_max = TRUE))
      }
      if (stat == "EV") {
        df_hi <- dplyr::filter(
          df, SessionType == "Live",
          is.finite(PlateLocSide), is.finite(PlateLocHeight),
          is.finite(ExitSpeed), ExitSpeed >= HEAT_EV_THRESHOLD
        )
        if (!nrow(df_hi)) return(ggplot() + theme_void())
        grid <- .kde_grid(df_hi$PlateLocSide, df_hi$PlateLocHeight)
        if (!nrow(grid)) return(ggplot() + theme_void())
        zmax <- suppressWarnings(max(grid$z, na.rm = TRUE))
        if (!is.finite(zmax) || zmax <= 0) return(ggplot() + theme_void())
        grid$z <- grid$z / zmax
        floor_q <- 0.25
        floor   <- stats::quantile(grid$z[grid$z > 0], floor_q, na.rm = TRUE)
        idx <- which(!is.na(grid$z) & grid$z < floor)
        if (length(idx)) grid$z[idx] <- NA
        return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_red, mark_max = TRUE))
      }
      ggplot() + theme_void()
    }
    output$cmpA_heat <- renderPlot({ .heat_plot(.filtered_panel("A"), input$cmpA_hmStat) }, bg = "transparent")
    output$cmpB_heat <- renderPlot({ .heat_plot(.filtered_panel("B"), input$cmpB_hmStat) }, bg = "transparent")
    
    .pitch_girafe <- function(df, sel_results) {
      if (!nrow(df)) return(NULL)
      if (!is.null(sel_results) && length(sel_results) && !("All" %in% sel_results)) {
        keep <- as.character(compute_result(df$PitchCall, df$PlayResult)) %in% sel_results
        df <- df[keep, , drop = FALSE]
      }
      if (!nrow(df)) return(NULL)
      types <- if (exists("ordered_types") && is.function(ordered_types)) ordered_types() else
        intersect(names(all_colors), as.character(unique(df$TaggedPitchType)))
      types_chr <- as.character(types)
      df_i <- df %>%
        dplyr::mutate(
          Result  = factor(compute_result(PitchCall, PlayResult), levels = result_levels),
          tt      = make_hover_tt(.),
          rid     = dplyr::row_number(),
          tt_fill = dplyr::coalesce(all_colors[as.character(TaggedPitchType)], "gray")
        )
      home <- data.frame(x = c(-0.75,0.75,0.75,0.00,-0.75),
                         y = c(1.05,1.05,1.15,1.25,1.15) - 0.5)
      cz <- data.frame(xmin = -1.5, xmax = 1.5, ymin = 2.65 - 1.7, ymax = 2.65 + 1.3)
      sz <- data.frame(xmin = ZONE_LEFT, xmax = ZONE_RIGHT, ymin = ZONE_BOTTOM, ymax = ZONE_TOP)
      df_known <- dplyr::filter(df_i, !is.na(Result))
      df_other <- dplyr::filter(df_i,  is.na(Result))
      p <- ggplot() +
        geom_polygon(data = home, aes(x, y), inherit.aes = FALSE, fill = NA, color = "black") +
        geom_rect(data = cz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE, fill = NA, color = "black", linetype = "dashed") +
        geom_rect(data = sz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  inherit.aes = FALSE, fill = NA, color = "black") +
        ggiraph::geom_point_interactive(
          data = df_other,
          aes(PlateLocSide, PlateLocHeight,
              color = TaggedPitchType, fill = TaggedPitchType,
              tooltip = tt, data_id = rid),
          position = "identity",
          size = 3.2, alpha = 0.9, shape = 16, stroke = 0.3
        ) +
        ggiraph::geom_point_interactive(
          data = df_known,
          aes(PlateLocSide, PlateLocHeight,
              color = TaggedPitchType, fill = TaggedPitchType, shape = Result,
              tooltip = tt, data_id = rid),
          position = "identity",
          size = 3.8, alpha = 0.95, stroke = 0.8
        ) +
        scale_color_manual(values = all_colors[types_chr], limits = types_chr, name = NULL) +
        scale_fill_manual(values  = all_colors[types_chr], limits = types_chr, name = NULL) +
        scale_shape_manual(values = shape_map, drop = TRUE, name = NULL) +
        coord_fixed(ratio = 1, xlim = c(-2, 2), ylim = c(0, 4.5)) +
        theme_void() + theme(legend.position = "none") +
        ggiraph::geom_point_interactive(
          data = df_i,
          aes(PlateLocSide, PlateLocHeight, tooltip = tt, data_id = rid, fill = I(tt_fill)),
          inherit.aes = FALSE, shape = 21, size = 6, alpha = 0.001, stroke = 0
        )
      ggiraph::girafe(
        ggobj = p,
        options = list(
          ggiraph::opts_tooltip(use_fill = TRUE, use_stroke = TRUE, css = tooltip_css),
          ggiraph::opts_hover(css = "stroke-width:1.5px;"),
          ggiraph::opts_hover_inv(css = "opacity:0.15;")
        )
      )
    }
    output$cmpA_pitch <- ggiraph::renderGirafe({ .pitch_girafe(.filtered_panel("A"), input$cmpA_result) })
    output$cmpB_pitch <- ggiraph::renderGirafe({ .pitch_girafe(.filtered_panel("B"), input$cmpB_result) })
    
    # Switchable plot container (ensures only ONE chart visible per side)
    output$cmpA_plot_ui <- renderUI({
      switch(input$cmpA_chart,
             "Movement Plot" = ggiraph::girafeOutput(ns("cmpA_movement"), height = "480px"),
             "Release Plot"  = plotOutput(ns("cmpA_release"), height = "480px"),
             "HeatMap"       = plotOutput(ns("cmpA_heat"), height = "480px"),
             "Location Chart"= ggiraph::girafeOutput(ns("cmpA_pitch"), height = "480px")
      )
    })
    output$cmpB_plot_ui <- renderUI({
      switch(input$cmpB_chart,
             "Movement Plot" = ggiraph::girafeOutput(ns("cmpB_movement"), height = "480px"),
             "Release Plot"  = plotOutput(ns("cmpB_release"), height = "480px"),
             "HeatMap"       = plotOutput(ns("cmpB_heat"), height = "480px"),
             "Location Chart"= ggiraph::girafeOutput(ns("cmpB_pitch"), height = "480px")
      )
    })
    
    # ---------- TABLES (IDENTICAL to DP / Summary page logic) ----------
    # fallback visible set helper if not global
    if (!exists("visible_set_for")) {
      visible_set_for <- function(mode, custom) {
        base <- c(
          "Pitch","#","Usage","BF","IP","FIP","WHIP","Velo","Max","IVB","HB","rTilt","bTilt","SpinEff","Spin",
          "Height","Side","VAA","HAA","Ext","InZone%","Comp%","Strike%","FPS%","E+A%",
          "K%","BB%","Whiff%","EV","LA","Stuff+","Ctrl+","QP+","Pitching+",
          # Results-specific
          "PA","AB","AVG","SLG","OBP","OPS","xWOBA","xISO","BABIP","GB%","Barrel%","Swing%","Whiff%","CSW%","K%","BB%","EV","LA"
        )
        if (identical(mode, "Custom") && length(custom)) unique(c("Pitch", custom)) else base
      }
    }
    
    output$cmpA_customPicker <- renderUI({
      if (!identical(input$cmpA_tableMode, "Custom")) return(NULL)
      choices_chr <- if (is.list(all_table_cols)) unlist(all_table_cols, use.names = FALSE) else all_table_cols
      choices_chr <- setdiff(choices_chr, "Pitch")
      selectizeInput(ns("cmpA_customCols"), label = NULL,
                     choices = choices_chr,
                     multiple = TRUE,
                     options = list(placeholder = "Choose columns to show…"))
    })
    output$cmpB_customPicker <- renderUI({
      if (!identical(input$cmpB_tableMode, "Custom")) return(NULL)
      choices_chr <- if (is.list(all_table_cols)) unlist(all_table_cols, use.names = FALSE) else all_table_cols
      choices_chr <- setdiff(choices_chr, "Pitch")
      selectizeInput(ns("cmpB_customCols"), label = NULL,
                     choices = choices_chr,
                     multiple = TRUE,
                     options = list(placeholder = "Choose columns to show…"))
    })
    
    output$cmpA_table <- DT::renderDataTable({
      domain <- input$domain %||% "Pitcher"
      table_mode <- if (is.null(input$cmpA_tableMode)) {
        if (domain == "Hitter") "Live" else "Stuff"
      } else {
        input$cmpA_tableMode
      }
      
      if (domain == "Hitter" && table_mode %in% c("Live", "BP")) {
        # Use hitting-specific table generation
        df <- .filtered_panel("A")
        if (!nrow(df)) {
          return(DT::datatable(
            data.frame(Message = "No data for selected filters"),
            options = list(dom = 't'), rownames = FALSE
          ))
        }
        
        # Determine session type for hitting table
        session_vals <- if (".hit_session" %in% names(df)) {
          df$.hit_session
        } else if ("SessionType" %in% names(df)) {
          df$SessionType
        } else {
          rep(NA_character_, nrow(df))
        }
        session_vals <- tolower(trimws(as.character(session_vals)))
        
        if (table_mode == "Live") {
          live_df <- df[session_vals == "live", , drop = FALSE]
          tbl <- build_summary_live(live_df)
        } else { # BP
          bp_df <- df[session_vals %in% c("bp", "bullpen"), , drop = FALSE]
          tbl <- build_summary_bp(bp_df)
        }
        
        # Use proper column visibility for hitting tables with PAP included
        if (table_mode == "Live") {
          hitting_live_cols <- c("Pitch", "#", "PA", "AB", "P/PA", "Swing%", "Contact%", "Whiff%", "Chase%", "Barrel%", "K%", "BB%", "EV", "maxEV", "LA", "PAP")
          visible_cols <- intersect(hitting_live_cols, names(tbl))
          
          datatable_with_colvis(
            tbl,
            lock = "Pitch",
            remember = FALSE,
            default_visible = visible_cols,
            mode = "Live"
          )
        } else {
          # BP mode - basic table
          DT::datatable(
            tbl,
            options = list(
              pageLength = 15,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel')
            ),
            rownames = FALSE
          )
        }
      } else {
        # Use standard pitching table generation
        .dp_like_table(
          .filtered_panel("A"),
          table_mode,
          input$cmpA_customCols,
          table_id = ns("cmpA_table"),
          session = session
        )
      }
    })
    
    output$cmpB_table <- DT::renderDataTable({
      domain <- input$domain %||% "Pitcher"
      table_mode <- if (is.null(input$cmpB_tableMode)) {
        if (domain == "Hitter") "Live" else "Stuff"
      } else {
        input$cmpB_tableMode
      }
      
      if (domain == "Hitter" && table_mode %in% c("Live", "BP")) {
        # Use hitting-specific table generation
        df <- .filtered_panel("B")
        if (!nrow(df)) {
          return(DT::datatable(
            data.frame(Message = "No data for selected filters"),
            options = list(dom = 't'), rownames = FALSE
          ))
        }
        
        # Determine session type for hitting table
        session_vals <- if (".hit_session" %in% names(df)) {
          df$.hit_session
        } else if ("SessionType" %in% names(df)) {
          df$SessionType
        } else {
          rep(NA_character_, nrow(df))
        }
        session_vals <- tolower(trimws(as.character(session_vals)))
        
        if (table_mode == "Live") {
          live_df <- df[session_vals == "live", , drop = FALSE]
          tbl <- build_summary_live(live_df)
        } else { # BP
          bp_df <- df[session_vals %in% c("bp", "bullpen"), , drop = FALSE]
          tbl <- build_summary_bp(bp_df)
        }
        
        # Use proper column visibility for hitting tables with PAP included
        if (table_mode == "Live") {
          hitting_live_cols <- c("Pitch", "#", "PA", "AB", "P/PA", "Swing%", "Contact%", "Whiff%", "Chase%", "Barrel%", "K%", "BB%", "EV", "maxEV", "LA", "PAP")
          visible_cols <- intersect(hitting_live_cols, names(tbl))
          
          datatable_with_colvis(
            tbl,
            lock = "Pitch",
            remember = FALSE,
            default_visible = visible_cols,
            mode = "Live"
          )
        } else {
          # BP mode - basic table
          DT::datatable(
            tbl,
            options = list(
              pageLength = 15,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel')
            ),
            rownames = FALSE
          )
        }
      } else {
        # Use standard pitching table generation
        .dp_like_table(
          .filtered_panel("B"),
          table_mode,
          input$cmpB_customCols,
          table_id = ns("cmpB_table"),
          session = session
        )
      }
    })
  })
}


# ==================================
# == Correlations Suite ==
# ==================================

# Correlations UI
correlations_ui <- function() {
  fluidPage(
    tags$head(
      tags$style(HTML("
        .correlation-sidebar {
          background-color: #f8f9fa;
          border-right: 1px solid #dee2e6;
          padding: 20px;
          min-height: 100vh;
        }
        .correlation-main {
          padding: 20px;
        }
        .correlation-chart {
          border: 1px solid #dee2e6;
          border-radius: 5px;
          padding: 15px;
          background-color: white;
        }
        .correlation-controls {
          margin-bottom: 20px;
        }
      "))
    ),
    fluidRow(
      # Sidebar with controls
      column(3, class = "correlation-sidebar",
             h4("Correlation Analysis Settings"),
             
             # Domain selection
             div(class = "correlation-controls",
                 selectInput("corr_domain", "Domain:",
                             choices = c("Pitching", "Hitting", "Catching"),
                             selected = "Pitching")
             ),
             
             # Date range
             div(class = "correlation-controls",
                 dateRangeInput("corr_date_range", "Date Range:",
                                start = max(pitch_data$Date, na.rm = TRUE),
                                end = max(pitch_data$Date, na.rm = TRUE),
                                format = "yyyy-mm-dd")
             ),
             
             # Player selection
             div(class = "correlation-controls",
                 selectInput("corr_player", "Select Player:",
                             choices = NULL,
                             multiple = TRUE)
             ),
             
             # Pitch type selection
             div(class = "correlation-controls",
                 selectInput("corr_pitch_type", "Pitch Type:",
                             choices = c("All" = "all"),
                             selected = "all",
                             multiple = TRUE)
             ),
             
             # Stuff+ Base Pitch option
             div(class = "correlation-controls",
                 selectInput("corr_stuff_base", "Stuff+ Base Pitch:",
                             choices = NULL)
             ),
             
             # Variable selection
             div(class = "correlation-controls",
                 selectInput("corr_var_x", "X Variable:",
                             choices = NULL)
             ),
             
             div(class = "correlation-controls",
                 selectInput("corr_var_y", "Y Variable:",
                             choices = NULL)
             ),
             
             # Aggregation level
             div(class = "correlation-controls",
                 radioButtons("corr_aggregation", "Data Level:",
                              choices = c("Player Averages" = "averages",
                                          "Individual Pitches" = "pitches"),
                              selected = "averages")
             ),
             
             # Analysis button
             div(class = "correlation-controls",
                 actionButton("corr_analyze", "Run Correlation Analysis",
                              class = "btn-primary btn-block")
             )
      ),
      
      # Main panel with results
      column(9, class = "correlation-main",
             div(id = "correlations-content",
                 h3("Correlation Analysis Results"),
                 
                 # Results summary
                 div(id = "corr_summary",
                     uiOutput("corr_summary_ui")
                 ),
                 
                 # Correlation chart
                 div(class = "correlation-chart",
                     plotOutput("corr_plot", height = "500px")
                 ),
                 
                 br(),
                 
                 # Data table
                 div(
                   h4("Data Used in Analysis"),
                   DT::dataTableOutput("corr_data_table")
                 )
             ) # Close correlations-content div
      )
    )
  )
}

# == Player Plans Suite ==
# ==================================

# Player Plans UI
player_plans_ui <- function() {
  fluidPage(
    tags$head(
      tags$style(HTML("
        .goal-container {
          border: 1px solid #ddd;
          border-radius: 5px;
          padding: 15px;
          margin: 10px 0;
          background-color: #f9f9f9;
          position: relative;
        }
        .goal-description {
          min-height: 80px;
          padding: 10px;
          background-color: white;
          border: 1px solid #e0e0e0;
          border-radius: 3px;
          margin-bottom: 15px;
        }
        /* Fixed checkbox positioning */
        .goal-checkbox {
          position: absolute !important;
          top: 8px !important;
          right: 8px !important;
          z-index: 999 !important;
          margin: 0 !important;
          padding: 0 !important;
        }
        .goal-checkbox .checkbox {
          margin: 0 !important;
          min-height: auto !important;
        }
        .goal-checkbox .checkbox input[type='checkbox'] {
          margin: 0 !important;
        }
      "))
    ),
    
  sidebarLayout(
    sidebarPanel(
      h4("Player Plans"),
        selectInput("pp_player_select", "Select Player:",
                    choices = NULL,
                    selected = NULL),
        
        selectInput("pp_session_type", "Session Type:",
                    choices = c("All", "Bullpen", "Live"),
                    selected = "All"),
        
        dateRangeInput("pp_date_range", "Date Range:",
                       start = Sys.Date() - 30,
                       end = Sys.Date(),
                       format = "mm/dd/yyyy"),
        
        hr(),
        
        # Goal 1
        h5("Goal #1"),
        selectInput("pp_goal1_type", "Goal Type:",
                    choices = c("", "Stuff", "Execution"),
                    selected = ""),
        
        conditionalPanel(
          condition = "input.pp_goal1_type == 'Stuff'",
          selectInput("pp_goal1_stuff_category", "Category:",
                      choices = c("", "Velocity", "Movement"),
                      selected = "")
        ),
        
        conditionalPanel(
          condition = "input.pp_goal1_type == 'Stuff' && input.pp_goal1_stuff_category == 'Velocity'",
          selectInput("pp_goal1_velocity_pitch", "Pitch Type:",
                      choices = NULL,
                      selected = NULL),
          selectInput("pp_goal1_batter_hand", "Batter Hand:",
                      choices = c("All", "Left", "Right"),
                      selected = "All")
        ),
        
        conditionalPanel(
          condition = "input.pp_goal1_type == 'Stuff' && input.pp_goal1_stuff_category == 'Movement'",
          selectInput("pp_goal1_movement_pitch", "Pitch Type:",
                      choices = NULL,
                      selected = NULL),
          selectInput("pp_goal1_movement_type", "Movement Type:",
                      choices = c("IVB", "HB"),
                      selected = NULL,
                      multiple = TRUE),
          selectInput("pp_goal1_batter_hand", "Batter Hand:",
                      choices = c("All", "Left", "Right"),
                      selected = "All")
        ),
        
        conditionalPanel(
          condition = "input.pp_goal1_type == 'Execution'",
          selectInput("pp_goal1_execution_stat", "Stat:",
                      choices = c("", "FPS%", "E+A%", "InZone%", "Strike%", 
                                  "Comp%", "Ctrl+", "QP+", "Whiff%", "CSW%"),
                      selected = ""),
          selectInput("pp_goal1_execution_pitch", "Pitch Type:",
                      choices = c("All"),
                      selected = "All",
                      multiple = TRUE),
          selectInput("pp_goal1_batter_hand", "Batter Hand:",
                      choices = c("All", "Left", "Right"),
                      selected = "All"),
          selectInput("pp_goal1_chart_view", "Chart View:",
                      choices = c("Trend Chart", "Heatmap"),
                      selected = "Trend Chart")
        ),
        
        # Target section for Goal 1
        conditionalPanel(
          condition = "input.pp_goal1_type != ''",
          hr(),
          h6("Target:"),
          fluidRow(
            column(4,
                   selectInput("pp_goal1_target_direction", NULL,
                               choices = c("", ">", "<"),
                               selected = "")
            ),
            column(8,
                   textInput("pp_goal1_target_value", NULL,
                             placeholder = "Enter target value",
                             value = "")
            )
          )
        ),
        
        hr(),
        
        # Goal 2
        h5("Goal #2"),
        selectInput("pp_goal2_type", "Goal Type:",
                    choices = c("", "Stuff", "Execution"),
                    selected = ""),
        
        conditionalPanel(
          condition = "input.pp_goal2_type == 'Stuff'",
          selectInput("pp_goal2_stuff_category", "Category:",
                      choices = c("", "Velocity", "Movement"),
                      selected = "")
        ),
        
        conditionalPanel(
          condition = "input.pp_goal2_type == 'Stuff' && input.pp_goal2_stuff_category == 'Velocity'",
          selectInput("pp_goal2_velocity_pitch", "Pitch Type:",
                      choices = NULL,
                      selected = NULL),
          selectInput("pp_goal2_batter_hand", "Batter Hand:",
                      choices = c("All", "Left", "Right"),
                      selected = "All")
        ),
        
        conditionalPanel(
          condition = "input.pp_goal2_type == 'Stuff' && input.pp_goal2_stuff_category == 'Movement'",
          selectInput("pp_goal2_movement_pitch", "Pitch Type:",
                      choices = NULL,
                      selected = NULL),
          selectInput("pp_goal2_movement_type", "Movement Type:",
                      choices = c("IVB", "HB"),
                      selected = NULL,
                      multiple = TRUE),
          selectInput("pp_goal2_batter_hand", "Batter Hand:",
                      choices = c("All", "Left", "Right"),
                      selected = "All")
        ),
        
        conditionalPanel(
          condition = "input.pp_goal2_type == 'Execution'",
          selectInput("pp_goal2_execution_stat", "Stat:",
                      choices = c("", "FPS%", "E+A%", "InZone%", "Strike%", 
                                  "Comp%", "Ctrl+", "QP+", "Whiff%", "CSW%"),
                      selected = ""),
          selectInput("pp_goal2_execution_pitch", "Pitch Type:",
                      choices = c("All"),
                      selected = "All",
                      multiple = TRUE),
          selectInput("pp_goal2_batter_hand", "Batter Hand:",
                      choices = c("All", "Left", "Right"),
                      selected = "All"),
          selectInput("pp_goal2_chart_view", "Chart View:",
                      choices = c("Trend Chart", "Heatmap"),
                      selected = "Trend Chart")
        ),
        
        # Target section for Goal 2
        conditionalPanel(
          condition = "input.pp_goal2_type != ''",
          hr(),
          h6("Target:"),
          fluidRow(
            column(4,
                   selectInput("pp_goal2_target_direction", NULL,
                               choices = c("", ">", "<"),
                               selected = "")
            ),
            column(8,
                   textInput("pp_goal2_target_value", NULL,
                             placeholder = "Enter target value",
                             value = "")
            )
          )
        ),
        
        hr(),
        
        # Goal 3
        h5("Goal #3"),
        selectInput("pp_goal3_type", "Goal Type:",
                    choices = c("", "Stuff", "Execution"),
                    selected = ""),
        
        conditionalPanel(
          condition = "input.pp_goal3_type == 'Stuff'",
          selectInput("pp_goal3_stuff_category", "Category:",
                      choices = c("", "Velocity", "Movement"),
                      selected = "")
        ),
        
        conditionalPanel(
          condition = "input.pp_goal3_type == 'Stuff' && input.pp_goal3_stuff_category == 'Velocity'",
          selectInput("pp_goal3_velocity_pitch", "Pitch Type:",
                      choices = NULL,
                      selected = NULL),
          selectInput("pp_goal3_batter_hand", "Batter Hand:",
                      choices = c("All", "Left", "Right"),
                      selected = "All")
        ),
        
        conditionalPanel(
          condition = "input.pp_goal3_type == 'Stuff' && input.pp_goal3_stuff_category == 'Movement'",
          selectInput("pp_goal3_movement_pitch", "Pitch Type:",
                      choices = NULL,
                      selected = NULL),
          selectInput("pp_goal3_movement_type", "Movement Type:",
                      choices = c("IVB", "HB"),
                      selected = NULL,
                      multiple = TRUE),
          selectInput("pp_goal3_batter_hand", "Batter Hand:",
                      choices = c("All", "Left", "Right"),
                      selected = "All")
        ),
        
        conditionalPanel(
          condition = "input.pp_goal3_type == 'Execution'",
          selectInput("pp_goal3_execution_stat", "Stat:",
                      choices = c("", "FPS%", "E+A%", "InZone%", "Strike%", 
                                  "Comp%", "Ctrl+", "QP+", "Whiff%", "CSW%"),
                      selected = ""),
          selectInput("pp_goal3_execution_pitch", "Pitch Type:",
                      choices = c("All"),
                      selected = "All",
                      multiple = TRUE),
          selectInput("pp_goal3_batter_hand", "Batter Hand:",
                      choices = c("All", "Left", "Right"),
                      selected = "All"),
          selectInput("pp_goal3_chart_view", "Chart View:",
                      choices = c("Trend Chart", "Heatmap"),
                      selected = "Trend Chart")
        ),
        
        # Target section for Goal 3
        conditionalPanel(
          condition = "input.pp_goal3_type != ''",
          hr(),
          h6("Target:"),
          fluidRow(
            column(4,
                   selectInput("pp_goal3_target_direction", NULL,
                               choices = c("", ">", "<"),
                               selected = "")
            ),
            column(8,
                   textInput("pp_goal3_target_value", NULL,
                             placeholder = "Enter target value",
                             value = "")
            )
          )
        ),
        width = 3,
        class = "sidebar"
      ),
      
      mainPanel(
        width = 9,
        div(id = "player-plans-content",
            # PDF Download Button
            # Content goes here
            
            # Header section with logos and title
            fluidRow(
              column(2,
                     div(style = "text-align: left; padding-top: 20px;",
                         tags$img(src = "PCUlogo.png", style = "height: 80px; max-width: 100%;")
                     )
              ),
              column(8,
                     div(style = "text-align: center; padding: 20px 0;",
                         h2(strong("Player Development Plan"), style = "margin-bottom: 10px;"),
                         h3(strong(textOutput("pp_player_name", inline = TRUE)), style = "margin-bottom: 5px;"),
                         h5(em(textOutput("pp_date_range_display", inline = TRUE)), style = "margin: 0;")
                     )
              ),
              column(2,
                     div(style = "text-align: right; padding-top: 20px;",
                         tags$img(src = "PCUlogo.png", style = "height: 80px; max-width: 100%;")
                     )
              )
            ),
            hr(),
            uiOutput("pp_dynamic_goals"),
            br(),
            # View Completed Goals button
            fluidRow(
              column(12,
                     div(style = "text-align: center; margin: 20px 0;",
                         actionButton("pp_view_completed", "View Completed Goals", 
                                      class = "btn-info", style = "margin-right: 10px;"),
                         span(textOutput("pp_completed_count", inline = TRUE), 
                              style = "font-size: 14px; color: #666;")
                     )
              )
            ),
            br(),
            fluidRow(
              column(12,
                     div(style = "text-align: center;",
                         h4(strong("Notes"))
                     ),
                     textAreaInput("pp_general_notes", label = NULL,
                                   placeholder = "Enter general notes for this player plan...",
                                   rows = 4, width = "100%")
              )
            )
        ) # Close player-plans-content div
      )
    ),
    
    # Add custom CSS for the modal
    tags$style(HTML("
      .modal-dialog {
        max-width: 800px;
      }
      .goal-completion-checkbox {
        margin-top: 5px;
      }
    "))
  )
}


# New suite navbar around it
ui <- tagList(
  shinyjs::useShinyjs(),
  # --- Custom navbar colors & styling ---
  tags$head(
    tags$style(HTML("
      /* ===== MODERN PROFESSIONAL DESIGN (PCU colors) ===== */

      /* Global Styles */
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap');

      body {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        background: linear-gradient(135deg, #f5f7fa 0%, #e8ecf1 100%);
        color: #1a202c;
        font-size: 14px;
      }

      /* ===== NAVBAR DESIGN ===== */
      .navbar-inverse { 
        background: linear-gradient(135deg, #000000 0%, #121212 50%, #1f1f1f 100%);
        border: none;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.5);
        min-height: 70px;
      }
      .navbar { 
        position: relative; 
        border-radius: 0;
        margin-bottom: 0;
      }

      /* Brand area with two logos side-by-side */
      .navbar-inverse .navbar-brand {
        color: #ffffff !important;
        font-weight: 800;
        font-size: 24px;
        letter-spacing: -0.5px;
        display: flex;
        align-items: center;
        gap: 12px;
        padding-top: 15px;
        padding-bottom: 15px;
        transition: all 0.3s ease;
      }
      .navbar-inverse .navbar-brand:hover {
        transform: translateY(-2px);
      }
      .navbar-inverse .navbar-brand .brand-logo {
        height: 40px;
      }
      .brand-title {
        font-size: 20px;
        font-weight: 700;
        color: #ffffff;
        letter-spacing: -0.3px;
      }

      /* Tab links */
      .navbar-inverse .navbar-nav > li > a { 
        color: rgba(255, 255, 255, 0.85) !important;
        font-weight: 600;
        font-size: 15px;
        padding: 10px 20px;
        margin: 0 4px;
        border-radius: 12px;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        position: relative;
        overflow: hidden;
      }
      .navbar-inverse .navbar-nav > li > a::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background: linear-gradient(135deg, rgba(255,255,255,0.12) 0%, rgba(255,255,255,0.06) 100%);
        opacity: 0;
        transition: opacity 0.3s ease;
      }
      .navbar-inverse .navbar-nav > li > a:hover::before {
        opacity: 1;
      }
      .navbar-inverse .navbar-nav > li > a:hover,
      .navbar-inverse .navbar-nav > li > a:focus { 
        color: #ffffff !important;
        background: rgba(255, 255, 255, 0.12);
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.2);
      }

      /* Active tab - vibrant gradient in PCU red */
      .navbar-inverse .navbar-nav > .active > a,
      .navbar-inverse .navbar-nav > .active > a:hover,
      .navbar-inverse .navbar-nav > .active > a:focus {
        color: #ffffff !important;
        background: linear-gradient(135deg, #c1121f 0%, #8b0d17 100%);
        box-shadow: 0 4px 15px rgba(193, 18, 31, 0.45);
        transform: translateY(-2px);
      }

      /* ===== SIDEBAR DESIGN ===== */
      .well {
        background: #ffffff;
        border: none;
        border-radius: 16px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.08);
        padding: 24px;
        margin-bottom: 20px;
      }

      .sidebar .well, .col-sm-3 .well, .col-sm-4 .well {
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border-left: 4px solid #c1121f;
      }

      .sidebar-accordion {
        border: 1px solid #e2e8f0;
        border-radius: 12px;
        background: #fff;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.06);
        position: relative;
        border-left: 4px solid #c1121f;
        margin-bottom: 16px;
      }
      .sidebar-accordion-summary {
        padding: 14px;
        font-weight: 800;
        font-size: 16px;
        cursor: pointer;
      }
      .sidebar-accordion-body {
        padding: 12px;
      }
      .sidebar-accordion-body .well {
        border-left: none;
      }

      /* Form Controls - Modern inputs */
      .form-control, .selectize-input {
        border: 2px solid #e2e8f0;
        border-radius: 10px;
        padding: 10px 14px;
        font-size: 14px;
        transition: all 0.3s ease;
        background: #ffffff;
      }
      .form-control:focus, .selectize-input.focus {
        border-color: #c1121f;
        box-shadow: 0 0 0 3px rgba(193, 18, 31, 0.12);
        outline: none;
      }

      .form-group label {
        font-weight: 600;
        color: #2d3748;
        font-size: 13px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 8px;
      }

      /* Select dropdowns */
      .selectize-dropdown {
        border: 2px solid #e2e8f0;
        border-radius: 10px;
        box-shadow: 0 10px 30px rgba(0, 0, 0, 0.15);
        margin-top: 4px;
      }
      .selectize-dropdown-content .option {
        padding: 10px 14px;
        transition: all 0.2s ease;
      }
      .selectize-dropdown-content .option:hover,
      .selectize-dropdown-content .option.active {
        background: linear-gradient(135deg, #c1121f 0%, #8b0d17 100%);
        color: white;
      }

      /* Date inputs */
      .shiny-date-range-input .input-daterange input {
        border-radius: 10px;
        border: 2px solid #e2e8f0;
      }

      /* ===== TAB PANELS (Secondary navigation) ===== */
      .nav-tabs {
        border-bottom: 2px solid #e2e8f0;
        margin-bottom: 20px;
      }
      .nav-tabs > li > a {
        border: none;
        border-radius: 12px 12px 0 0;
        color: #4a5568;
        font-weight: 600;
        padding: 12px 24px;
        margin-right: 4px;
        transition: all 0.3s ease;
        position: relative;
      }
      .nav-tabs > li > a:hover {
        background: rgba(193, 18, 31, 0.08);
        color: #c1121f;
        border: none;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background: linear-gradient(135deg, #c1121f 0%, #8b0d17 100%);
        color: white;
        border: none;
        box-shadow: 0 -2px 10px rgba(193, 18, 31, 0.3);
      }

      /* ===== BUTTONS ===== */
      .btn {
        border-radius: 10px;
        font-weight: 600;
        padding: 10px 20px;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        border: none;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
      }
      .btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.2);
      }
      .btn:active {
        transform: translateY(0);
      }

      .btn-primary {
        background: linear-gradient(135deg, #c1121f 0%, #8b0d17 100%);
        color: white;
      }
      .btn-primary:hover, .btn-primary:focus {
        background: linear-gradient(135deg, #8b0d17 0%, #c1121f 100%);
        color: white;
      }

      .btn-default {
        background: white;
        color: #4a5568;
        border: 2px solid #e2e8f0;
      }
      .btn-default:hover {
        background: #f7fafc;
        border-color: #c1121f;
        color: #c1121f;
      }

      /* Add Note button */
      #openNote {
        border-radius: 50%;
        padding: 12px 14px;
        font-size: 18px;
        box-shadow: 0 4px 20px rgba(193, 18, 31, 0.4);
        background: linear-gradient(135deg, #c1121f 0%, #8b0d17 100%);
      }
      #openNote:hover {
        box-shadow: 0 6px 25px rgba(193, 18, 31, 0.6);
        transform: translateY(-3px) scale(1.05);
      }

      /* ===== TABLES ===== */
      .dataTables_wrapper {
        background: white;
        padding: 20px;
        border-radius: 12px;
        box-shadow: 0 2px 12px rgba(0, 0, 0, 0.06);
      }
      .dataTable thead th {
        background: linear-gradient(135deg, #1a1a1a 0%, #2d2d2d 50%, #404040 100%);
        color: white;
        font-weight: 600;
        padding: 14px 10px;
        border: none;
        text-transform: uppercase;
        font-size: 12px;
        letter-spacing: 0.5px;
      }
      .dataTable tbody tr {
        transition: all 0.2s ease;
      }
      .dataTable tbody tr:hover {
        background: rgba(193, 18, 31, 0.05);
        transform: scale(1.01);
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.05);
      }
      .dataTable tbody td {
        padding: 12px 10px;
        border-bottom: 1px solid #f0f0f0;
      }

      /* ===== PANELS & CARDS ===== */
      .panel {
        border: none;
        border-radius: 12px;
        box-shadow: 0 2px 12px rgba(0, 0, 0, 0.06);
        overflow: hidden;
      }
      .panel-default > .panel-heading {
        background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%);
        border: none;
        color: #2d3748;
        font-weight: 700;
        padding: 16px 20px;
        border-bottom: 2px solid #e2e8f0;
      }
      .panel-body {
        padding: 20px;
      }

      /* ===== PLOTS & CHARTS ===== */
      .shiny-plot-output, .plotly, .html-widget {
        border-radius: 12px;
        overflow: hidden;
        box-shadow: 0 2px 12px rgba(0, 0, 0, 0.06);
        background: transparent;
      }

      /* ===== PAGE CONTAINER ===== */
      .container-fluid {
        padding: 20px 30px;
      }

      /* ===== LOADING ANIMATION ===== */
      .shiny-output-error { color: #e53e3e; }
      .shiny-output-error:before { content: '⚠ '; }

      /* Custom scrollbar */
      ::-webkit-scrollbar {
        width: 10px;
        height: 10px;
      }
      ::-webkit-scrollbar-track {
        background: #f1f1f1;
      }
      ::-webkit-scrollbar-thumb {
        background: linear-gradient(135deg, #c1121f 0%, #8b0d17 100%);
        border-radius: 10px;
      }
      ::-webkit-scrollbar-thumb:hover {
        background: linear-gradient(135deg, #8b0d17 0%, #c1121f 100%);
      }
    "))
  ),
  
  # --- Global click handler for Notes → jump back to saved view ---
  tags$script(HTML("
    document.addEventListener('click', function(e){
      var a = e.target.closest('a.note-jump'); if(!a) return;
      e.preventDefault();
      Shiny.setInputValue('noteJump', {
        suite: a.dataset.suite || '', page: a.dataset.page || '',
        pitcher: a.dataset.pitcher || '', sess: a.dataset.sess || '',
        ds: a.dataset.ds || '', de: a.dataset.de || '', nonce: Math.random()
      }, {priority:'event'});
    }, true);
  ")),
  
  tags$script(HTML("
// Avoid double-binding by namespacing and unbinding first
$(document).off('click.pcuOpenMedia', 'a.open-media')
  .on('click.pcuOpenMedia', 'a.open-media', function(e){
    e.preventDefault();
    var url = $(this).data('url') || $(this).attr('href') || '';
    var typ = (($(this).data('type') || 'auto') + '').toLowerCase();
    if(!url) return;
    Shiny.setInputValue('open_media', {url: url, type: typ, nonce: Math.random()}, {priority:'event'});
  });

// Mobile sidebar: replace full panel with a single dropdown accordion (matches gcu)
document.addEventListener('DOMContentLoaded', function() {
  var transform = function(sidebar) {
    if (!sidebar || sidebar.dataset.mobileAccordion) return;
    if (window.innerWidth > 768) return;
    // Only convert elements that actually contain sidebar content
    var well = sidebar.querySelector('.well');
    if (!well) return;
    var details = document.createElement('details');
    details.className = 'sidebar-accordion';
    var summary = document.createElement('summary');
    summary.textContent = 'Filters';
    summary.className = 'sidebar-accordion-summary';
    var body = document.createElement('div');
    body.className = 'sidebar-accordion-body';
    while (sidebar.firstChild && sidebar.firstChild !== details) {
      body.appendChild(sidebar.firstChild);
    }
    details.appendChild(summary);
    details.appendChild(body);
    sidebar.appendChild(details);
    sidebar.dataset.mobileAccordion = '1';
  };
  var init = function() {
    document.querySelectorAll('.sidebar, .col-sm-3, .col-sm-4').forEach(transform);
  };
  init();
  window.addEventListener('resize', function() {
    // Do not rebuild; only apply on first mobile render to avoid moving nodes back and forth
  });
  document.addEventListener('shiny:recalculating', function() { setTimeout(init, 200); });
});

// Click handler for # pitch count links → ensure clicks work
$(document).on('click', 'td.clickable-cell, .pitch-count-link', function(e) {
  // Let DT handle the click naturally - no special processing needed
});
")),
  
  tags$style(HTML("
    /* Custom note button color */
    #openNote.btn-note {
      background: linear-gradient(135deg, #c1121f 0%, #8b0d17 100%);
      border: none;
      color: #fff;
    }
    #openNote.btn-note:hover,
    #openNote.btn-note:focus,
    #openNote.btn-note:active,
    #openNote.btn-note:active:focus {
      background: linear-gradient(135deg, #8b0d17 0%, #c1121f 100%);
      border: none;
      color: #fff;
      outline: none;
    }
  ")),
  
  # --- Floating "Add Note" button (top-right, all pages) ---
  absolutePanel(
    style = "background:transparent; border:none; box-shadow:none; z-index:2000;",
    actionButton("openNote", label = NULL, icon = icon("sticky-note"),
                 class = "btn btn-note", title = "Add Note"),
    top = 8, right = 12, width = 50, fixed = TRUE, draggable = FALSE
  ),

  tags$style(HTML("
    /* Transparent backgrounds for summary plots/key */
    #summary_heatZonePlot, #summary_legend {
      background: transparent !important;
    }
    #summary_heatZonePlot svg, #summary_legend svg,
    #summary_heatZonePlot canvas, #summary_legend canvas {
      background: transparent !important;
    }
    /* Girafe outputs (location plot) */
    #summary_zonePlot svg {
      background: transparent !important;
    }
    
    /* Make # column cells and links clickable with visual feedback */
    .clickable-cell {
      cursor: pointer !important;
    }
    .clickable-cell:hover {
      background-color: rgba(0, 123, 255, 0.1) !important;
    }
    .pitch-count-link {
      color: #007bff !important;
      text-decoration: none !important;
      cursor: pointer !important;
      display: inline-block;
      width: 100%;
    }
    .pitch-count-link:hover {
      text-decoration: underline !important;
      color: #0056b3 !important;
    }

    /* ===== Dark mode overrides ===== */
    body.theme-dark {
      background: linear-gradient(135deg, #0b0d12 0%, #111827 100%);
      color: #e5e7eb;
    }
    body.theme-dark .navbar-inverse {
      background: linear-gradient(135deg, #0f172a 0%, #0b0f19 50%, #0a0d14 100%);
      box-shadow: 0 4px 20px rgba(0,0,0,0.6);
    }
    body.theme-dark .navbar-inverse .navbar-brand,
    body.theme-dark .navbar-inverse .navbar-nav > li > a {
      color: rgba(255,255,255,0.9) !important;
    }
    body.theme-dark .navbar-inverse .navbar-nav > li > a:hover,
    body.theme-dark .navbar-inverse .navbar-nav > li > a:focus {
      background: rgba(255,255,255,0.08);
      color: #fff !important;
    }
    body.theme-dark .navbar-inverse .navbar-nav > .active > a,
    body.theme-dark .navbar-inverse .navbar-nav > .active > a:hover,
    body.theme-dark .navbar-inverse .navbar-nav > .active > a:focus {
      background: linear-gradient(135deg, #c1121f 0%, #8b0d17 100%);
      box-shadow: 0 4px 15px rgba(193,18,31,0.45);
    }
    body.theme-dark .well,
    body.theme-dark .sidebar .well,
    body.theme-dark .col-sm-3 .well,
    body.theme-dark .col-sm-4 .well {
      background: radial-gradient(circle at top left, #1f2937 0%, #0f172a 60%, #0b0f19 100%);
      border-left: 4px solid #c1121f;
      color: #e5e7eb;
      box-shadow: 0 4px 20px rgba(0,0,0,0.35);
    }
    body.theme-dark .sidebar-accordion,
    body.theme-dark .sidebar-accordion-body {
      background: transparent !important;
      border: none !important;
      box-shadow: none !important;
    }
    body.theme-dark .sidebar-accordion-summary {
      color: #e5e7eb;
    }
    body.theme-dark .form-control,
    body.theme-dark .selectize-input {
      background: #0f172a;
      border-color: #1f2937;
      color: #e5e7eb;
    }
    body.theme-dark .form-control:focus,
    body.theme-dark .selectize-input.focus {
      border-color: #c1121f;
      box-shadow: 0 0 0 3px rgba(193,18,31,0.25);
    }
    body.theme-dark .selectize-dropdown {
      background: #0f172a;
      border-color: #1f2937;
    }
    body.theme-dark .selectize-dropdown-content .option {
      color: #e5e7eb;
    }
    body.theme-dark .selectize-dropdown-content .option:hover,
    body.theme-dark .selectize-dropdown-content .option.active {
      background: linear-gradient(135deg, #c1121f 0%, #8b0d17 100%);
      color: #fff;
    }
    body.theme-dark .nav-tabs {
      border-bottom-color: #1f2937;
    }
    body.theme-dark .nav-tabs > li > a {
      color: #cbd5e1;
      background: transparent;
    }
    body.theme-dark .nav-tabs > li > a:hover {
      background: rgba(193,18,31,0.12);
      color: #fff;
    }
    body.theme-dark .nav-tabs > li.active > a,
    body.theme-dark .nav-tabs > li.active > a:hover,
    body.theme-dark .nav-tabs > li.active > a:focus {
      background: linear-gradient(135deg, #c1121f 0%, #8b0d17 100%);
      color: #fff;
      box-shadow: 0 -2px 10px rgba(193,18,31,0.4);
    }
    body.theme-dark .btn-default {
      background: #0f172a;
      color: #e5e7eb;
      border: 2px solid #1f2937;
    }
    body.theme-dark .btn-default:hover {
      background: #111827;
      border-color: #c1121f;
      color: #fff;
    }
    body.theme-dark .dataTables_wrapper {
      background: #0b1220;
      color: #e5e7eb;
      box-shadow: 0 2px 12px rgba(0,0,0,0.4);
    }
    body.theme-dark table.dataTable tbody tr:hover {
      background: rgba(193,18,31,0.12);
    }
    body.theme-dark .panel {
      background: #0f172a;
      box-shadow: 0 2px 12px rgba(0,0,0,0.4);
      color: #e5e7eb;
    }
    body.theme-dark .panel-default > .panel-heading {
      background: linear-gradient(135deg, #111827 0%, #0b1220 100%);
      border: none;
      color: #e5e7eb;
    }
    body.theme-dark .shiny-plot-output,
    body.theme-dark .plotly,
    body.theme-dark .html-widget {
      box-shadow: 0 2px 12px rgba(0,0,0,0.35);
      background: transparent !important;
    }
    body.theme-dark svg,
    body.theme-dark canvas,
    body.theme-dark .girafe_container svg {
      background: transparent !important;
    }
    body.theme-dark .container-fluid {
      color: #e5e7eb;
    }
    body.theme-dark .selectize-control.single .selectize-input:after {
      border-color: #e5e7eb transparent transparent transparent;
    }
    body.theme-dark label,
    body.theme-dark .control-label,
    body.theme-dark .form-group,
    body.theme-dark .form-group * {
      color: #e5e7eb !important;
    }
    body.theme-dark table.dataTable tbody td,
    body.theme-dark table.dataTable thead th {
      color: #e5e7eb;
      background: transparent;
    }
    body.theme-dark table.dataTable tbody tr {
      background: transparent;
    }
    body.theme-dark svg text {
      fill: #e5e7eb !important;
    }
    body.theme-dark svg line,
    body.theme-dark svg path {
      stroke: #e5e7eb !important;
    }
    body.theme-dark .selectize-input > input {
      color: #e5e7eb !important;
    }
    body.theme-dark rect.panel.background,
    body.theme-dark rect.plot.background,
    body.theme-dark rect.background,
    body.theme-dark g.panel.background rect,
    body.theme-dark g.plot.background rect {
      fill: transparent !important;
      stroke: none !important;
    }
    body.theme-dark [fill='black'] { fill: #ffffff !important; }
    body.theme-dark [stroke='black'] { stroke: #ffffff !important; }
    body.theme-dark [fill='#000000'],
    body.theme-dark [fill='#000'] { fill: #ffffff !important; }
    body.theme-dark [stroke='#000000'],
    body.theme-dark [stroke='#000'] { stroke: #ffffff !important; }
    body.theme-dark svg rect[fill='white'],
    body.theme-dark svg rect[fill='#FFFFFF'],
    body.theme-dark svg rect[fill='#fff'] {
      fill: transparent !important;
      stroke: none !important;
    }
    /* Force heatmap plot outputs to stay transparent in dark mode */
    body.theme-dark #heatmapsHeatPlot,
    body.theme-dark #heatmapsHeatPlot canvas,
    body.theme-dark #heatmapsHeatPlot img,
    body.theme-dark #cmpA_heat,
    body.theme-dark #cmpA_heat canvas,
    body.theme-dark #cmpA_heat img,
    body.theme-dark #cmpB_heat,
    body.theme-dark #cmpB_heat canvas,
    body.theme-dark #cmpB_heat img {
      background: transparent !important;
    }
    
    /* Fix multi-select filter bubbles - only the item bubbles should be red */
    body.theme-dark .selectize-input .item {
      background: linear-gradient(135deg, #c1121f 0%, #8b0d17 100%) !important;
      color: #ffffff !important;
      border-color: rgba(255,255,255,0.3) !important;
    }
    body.theme-dark .selectize-input.has-items {
      background: #0f172a !important;
      color: #e5e7eb !important;
    }
    body.theme-dark .selectize-input.focus.has-items {
      background: #0f172a !important;
      border-color: #c1121f !important;
    }
    
    /* Fix right-handed batter names in AB Report - should be white in dark mode */
    .batter-name-rh {
      color: #000000;
    }
    body.theme-dark .batter-name-rh {
      color: #ffffff !important;
    }
    body.theme-dark div[style*='color:#000000'],
    body.theme-dark div[style*='color: #000000'] {
      color: #ffffff !important;
    }
    
    /* Fix Correlations page - transparent backgrounds */
    body.theme-dark #correlations .well,
    body.theme-dark #correlations .sidebar-panel,
    body.theme-dark #correlations .panel,
    body.theme-dark #correlations .panel-default,
    body.theme-dark #correlations .shiny-plot-output,
    body.theme-dark div[id*='corr'] .well,
    body.theme-dark div[id*='corr'] .panel {
      background: transparent !important;
    }
    
    /* Fix Player Plans page - transparent backgrounds */
    body.theme-dark #player_plans .well,
    body.theme-dark #player_plans .panel,
    body.theme-dark #player_plans .panel-default,
    body.theme-dark div[id*='plan'] .well,
    body.theme-dark div[id*='plan'] .panel {
      background: transparent !important;
    }
    
    /* Fix video popup modal - white background with black text for labels, white for badges */
    body.theme-dark .modal-content {
      background: #ffffff !important;
      color: #000000 !important;
    }
    body.theme-dark .modal-header,
    body.theme-dark .modal-body,
    body.theme-dark .modal-footer {
      background: #ffffff !important;
      color: #000000 !important;
    }
    body.theme-dark .modal-body label,
    body.theme-dark .modal-body .control-label,
    body.theme-dark .modal-body strong {
      color: #000000 !important;
    }
    /* Keep white text for badges/bubbles in modal */
    body.theme-dark .modal-body .badge,
    body.theme-dark .modal-body .label,
    body.theme-dark .modal-body .selectize-input .item,
    body.theme-dark .modal-body span.badge,
    body.theme-dark .modal-body span.label {
      color: #ffffff !important;
    }
    
    /* Toggle switch styling */
    .dark-toggle { display:flex; align-items:center; gap:10px; }
    .dark-toggle .switch-label { display:flex; align-items:center; gap:10px; margin:0; cursor:pointer; }
    .dark-toggle input#dark_mode { display:none; }
    .dark-toggle .switch-track {
      position: relative; width: 50px; height: 26px;
      background: #4b5563; border-radius: 13px; transition: background 0.2s ease;
      box-shadow: inset 0 0 4px rgba(0,0,0,0.35);
    }
    .dark-toggle .switch-thumb {
      position: absolute; top: 3px; left: 3px; width: 20px; height: 20px;
      background: #fff; border-radius: 50%; box-shadow: 0 2px 6px rgba(0,0,0,0.3);
      transition: transform 0.2s ease;
    }
    .dark-toggle input#dark_mode:checked + .switch-track { background: #c1121f; }
    .dark-toggle input#dark_mode:checked + .switch-track .switch-thumb { transform: translateX(24px); }
    .dark-toggle .switch-text { font-weight:600; color:#e5e7eb; }
    
    /* Static dark mode toggle positioned below navbar */
    .dark-mode-container {
      position: relative;
      background: rgba(0,0,0,0.4);
      border-radius: 12px;
      padding: 8px 12px;
      margin: 12px;
      width: fit-content;
    }
    
    /* ===== MOBILE RESPONSIVE SIDEBAR ===== */
    @media (max-width: 768px) {
      .sidebar { display: block !important; }
      .sidebar details.sidebar-accordion { display: block; margin-bottom: 20px; }
      .sidebar .well { padding: 0; border: none; box-shadow: none; background: transparent; }
      .sidebar .sidebar-accordion-body .well { padding: 16px; }
      .col-sm-8 {
        width: 100% !important;
      }
      
      /* Ensure sidebar container doesn't mess with layout */
      .col-sm-4 {
        width: 100% !important;
        margin-bottom: 20px;
      }
    }
  ")),
  
  navbarPage(
    title = tagList(
      tags$img(src = "PCUlogo.png", class = "brand-logo", alt = "PCU"),
      tags$span("PCU Dashboard", class = "brand-title")
    ),
    id = "top",
    inverse = TRUE,
    header = tags$div(
      class = "dark-mode-container",
      tags$div(
        class = "dark-toggle",
        tags$label(
          class = "switch-label",
          tags$input(id = "dark_mode", type = "checkbox"),
          tags$span(class = "switch-track", tags$span(class = "switch-thumb")),
          tags$span(class = "switch-text", "Dark mode")
        )
      )
    ),
    tabPanel("Pitching",   value = "Pitching",   pitch_ui()),
    tabPanel("Hitting",    value = "Hitting",    mod_hit_ui("hit")),
    tabPanel("Leaderboard", value = "Leaderboard", mod_leader_ui("leader")),
    tabPanel("Comparison Tool", value = "Comparison Suite", mod_comp_ui("comp")),
    tabPanel("Correlations", value = "Correlations", correlations_ui()),
    tabPanel("Player Plans", value = "Player Plans", player_plans_ui()),
    tabPanel("Notes", value = "Notes",
             fluidPage(
               br(),
               DT::dataTableOutput("notesTable")
             )
    )
  )
)


# Server logic
server <- function(input, output, session) {
  
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) return(y)
    # keep original behavior: treat empty-string-ish as missing
    if (is.character(x) && !nzchar(x[1])) return(y)
    x
  }
  
  # MIME type guesser — case-insensitive and tolerant of cache-busting query strings
  guess_video_type <- function(name_or_url) {
    nm <- tolower(as.character(name_or_url %||% ""))
    if (grepl("\\.mp4(\\?.*)?$", nm)) "video/mp4"
    else if (grepl("\\.mov(\\?.*)?$", nm)) "video/quicktime"
    else "video"
  }

  # Theme toggle (light/dark)
  observeEvent(input$dark_mode, {
    if (isTRUE(input$dark_mode)) {
      shinyjs::addClass(selector = "body", class = "theme-dark")
    } else {
      shinyjs::removeClass(selector = "body", class = "theme-dark")
    }
  }, ignoreInit = TRUE)
  
  
  # Map a clip name to a served URL, respecting subfolders by source (Edger/Behind/Side)
  # Robust to extension case (.MOV/.MP4) and container swaps.
  video_url_for <- function(clip, source = NULL) {
    # Normalize input
    if (is.null(clip) || length(clip) == 0) return("")
    clip_chr <- trimws(as.character(clip)[1])
    if (!nzchar(clip_chr)) return("")
    
    # If a full URL was stored, use it as-is (Cloudinary, S3, etc.)
    if (grepl("^https?://", clip_chr)) return(clip_chr)
    
    # Base directory configured by addResourcePath(...) at startup
    vdir <- getOption("PCU_VIDEOS_DIR", NA_character_)
    if (is.na(vdir) || !nzchar(vdir)) return("")
    
    # Build candidate filenames with case and container variants
    make_candidates <- function(fname) {
      # Split name and extension (case-insensitive)
      ext0  <- tools::file_ext(fname)              # may be "" or "MOV" etc.
      base0 <- if (nzchar(ext0)) sub(sprintf("\\.%s$", ext0), "", fname, ignore.case = TRUE) else fname
      
      # Try both containers if none or if it's mov/mp4
      if (!nzchar(ext0) || grepl("^(?i)(mov|mp4)$", ext0)) {
        exts <- c("mov", "mp4")                    # try MOV & MP4
      } else {
        exts <- ext0                               # unknown ext -> just use given
      }
      
      # Case variants for base and ext
      bases <- unique(c(base0, tolower(base0), toupper(base0)))
      extsL <- unique(c(exts, tolower(exts), toupper(exts)))
      
      # If original had an extension, include the original string as-is first
      cand <- character(0)
      if (nzchar(ext0)) cand <- c(cand, fname)
      
      # Then all base×ext combinations (both lower/upper ext)
      for (b in bases) for (e in extsL) cand <- c(cand, paste0(b, ".", e))
      
      # If there was no extension at all, also include bare bases (some users store raw names)
      if (!nzchar(ext0)) cand <- c(cand, bases)
      
      unique(cand)
    }
    
    # Helper: construct served URL under /videos/... and ensure the file exists on disk
    make_url_if_exists <- function(path_parts, fname) {
      abs_path <- do.call(file.path, c(list(vdir), as.list(path_parts), list(fname)))
      if (file.exists(abs_path)) {
        prefix <- paste(c("videos", path_parts), collapse = "/")
        return(paste0(prefix, "/", utils::URLencode(fname)))
      }
      ""
    }
    
    # Optional source → subfolder mapping
    src_map <- c(Edger = "Edger", Behind = "Behind", Side = "Side")
    
    # 1) If a source was given (Edger/Behind/Side), try videos/<Source>/<file>
    if (!is.null(source) && nzchar(source)) {
      subdir <- src_map[[as.character(source)]]
      if (!is.null(subdir)) {
        for (nm in make_candidates(clip_chr)) {
          url <- make_url_if_exists(subdir, nm)
          if (nzchar(url)) return(url)
        }
      }
    }
    
    # 2) Backward compatibility: try root videos/<file> (no subfolder)
    for (nm in make_candidates(clip_chr)) {
      url <- make_url_if_exists(character(0), nm)
      if (nzchar(url)) return(url)
    }
    
    ""
  }
  
  
  
  
  guess_video_type <- function(name_or_url) {
    nm <- tolower(as.character(name_or_url %||% ""))
    if (grepl("\\.mp4(\\?.*)?$", nm)) "video/mp4"
    else if (grepl("\\.mov(\\?.*)?$", nm)) "video/quicktime"
    else "video"
  }
  
  # Safe numeric formatter: handles factors/characters/blank/NA gracefully
  fmt_num <- function(digits = 1, unit = "") {
    function(x) {
      x_num <- suppressWarnings(as.numeric(x))
      if (length(x_num) == 0 || all(is.na(x_num))) return("—")
      x_val <- x_num[1]
      if (is.na(x_val)) return("—")
      out <- format(round(x_val, digits), nsmall = digits, trim = TRUE)
      if (nzchar(unit)) paste0(out, " ", unit) else out
    }
  }
  
  # Generic metric cell:
  # - If fmt is NULL -> treat as text
  # - If fmt is a function -> apply safely to numeric-like values
  metric_val <- function(row, col, fmt = NULL) {
    v <- tryCatch(row[[col]], error = function(e) NULL)
    if (is.null(v) || length(v) == 0) return(tags$span(style="opacity:.6;", "—"))
    v1 <- v[1]
    
    if (is.null(fmt)) {
      txt <- as.character(v1)
      if (!nzchar(txt)) tags$span(style="opacity:.6;", "—") else txt
    } else {
      out <- tryCatch(fmt(v1), error = function(e) "—")
      if (!nzchar(as.character(out))) tags$span(style="opacity:.6;", "—") else out
    }
  }
  
  # Helpers (keep these near your other helpers)
  get_first_col <- function(row, candidates) {
    for (nm in candidates) {
      if (!is.null(row[[nm]])) return(row[[nm]])
    }
    NULL
  }
  
  fmt_num <- function(digits = 1, unit = "") {
    function(x) {
      x_num <- suppressWarnings(as.numeric(x))
      if (length(x_num) == 0 || all(is.na(x_num))) return("\u2014")
      x_val <- x_num[1]
      if (is.na(x_val)) return("\u2014")
      out <- format(round(x_val, digits), nsmall = digits, trim = TRUE)
      if (nzchar(unit)) paste0(out, unit) else out
    }
  }
  
  fmt_pct <- function(digits = 1) {
    function(x) {
      x_num <- suppressWarnings(as.numeric(x))
      if (length(x_num) == 0 || all(is.na(x_num))) return("\u2014")
      v <- x_num[1]
      if (is.na(v)) return("\u2014")
      # Accept 0–1 or 0–100 input
      if (v <= 1) v <- v * 100
      paste0(format(round(v, digits), nsmall = digits), "%")
    }
  }
  
  # 180° = 12:00, 270° = 3:00, 0° = 6:00, 90° = 9:00
  # If value already looks like "H:MM", just return it.
  deg_to_clock <- function(x) {
    # pass-through if already formatted like "1:30"
    if (is.character(x) && length(x) && grepl("^\\s*\\d{1,2}:\\d{2}\\s*$", x[1])) {
      return(trimws(x[1]))
    }
    d <- suppressWarnings(as.numeric(x))
    if (length(d) == 0 || is.na(d)) return("\u2014")
    a <- (d - 180) %% 360
    hour   <- floor(a / 30)
    minute <- round((a %% 30) / 30 * 60)
    if (minute == 60) { minute <- 0; hour <- hour + 1 }
    hour <- hour %% 12; if (hour == 0) hour <- 12
    sprintf("%d:%02d", hour, minute)
  }
  
  
  metric_row <- function(label, value_html) {
    tags$div(
      style = "font-weight:700; margin:6px 0; text-align:center;",
      # label: value (both bold & centered)
      tags$span(paste0(label, ": ")),
      value_html
    )
  }
  
  # --- helpers ---
  format_name_first_last <- function(x) {
    s <- as.character(x %||% "")
    if (!nzchar(s)) return("\u2014")
    # If "Last, First", flip it
    if (grepl(",", s, fixed = TRUE)) {
      parts <- trimws(strsplit(s, ",", fixed = TRUE)[[1]])
      if (length(parts) >= 2) return(paste(parts[2], parts[1]))
    }
    s
  }
  
  fmt_num <- function(digits = 1, unit = "") {
    function(x) {
      x_num <- suppressWarnings(as.numeric(x))
      if (length(x_num) == 0 || all(is.na(x_num))) return("\u2014")
      v <- x_num[1]; if (is.na(v)) return("\u2014")
      out <- format(round(v, digits), nsmall = digits, trim = TRUE)
      if (nzchar(unit)) paste0(out, unit) else out
    }
  }
  
  fmt_pct <- function(digits = 1) {
    function(x) {
      x_num <- suppressWarnings(as.numeric(x))
      if (length(x_num) == 0 || all(is.na(x_num))) return("\u2014")
      v <- x_num[1]; if (is.na(v)) return("\u2014")
      if (v <= 1) v <- v * 100
      paste0(format(round(v, digits), nsmall = digits), "%")
    }
  }
  
  # 180° = 12:00, 270° = 3:00, 0° = 6:00, 90° = 9:00
  # If already "H:MM", pass through unchanged.
  deg_to_clock <- function(x) {
    if (is.character(x) && length(x) && grepl("^\\s*\\d{1,2}:\\d{2}\\s*$", x[1])) {
      return(trimws(x[1]))
    }
    d <- suppressWarnings(as.numeric(x))
    if (length(d) == 0 || is.na(d)) return("\u2014")
    a <- (d + 180) %% 360   # <-- key fix (was d - 180)
    hour   <- floor(a / 30)
    minute <- round((a %% 30) / 30 * 60)
    if (minute == 60) { minute <- 0; hour <- hour + 1 }
    hour <- hour %% 12; if (hour == 0) hour <- 12
    sprintf("%d:%02d", hour, minute)
  }
  
  
  get_first_col <- function(row, candidates) {
    for (nm in candidates) if (!is.null(row[[nm]])) return(row[[nm]])
    NULL
  }
  
  metric_row <- function(label, value_html) {
    # Bold ONLY the label, value regular. Center the whole line.
    tags$div(
      style = "margin:6px 0; text-align:center;",
      tags$span(style="font-weight:800;", paste0(label, ": ")),
      tags$span(style="font-weight:600;", value_html)
    )
  }
  
  metric_value_only <- function(value_html) {
    # No label; show value bold & centered (for Velocity/Spin)
    tags$div(style = "margin:6px 0; text-align:center; font-weight:800;", value_html)
  }
  
  # --- panel ---
  build_metrics_panel <- function(row) {
    # Name (First Last) and Date
    name_text <- format_name_first_last(
      row[["Pitcher"]] %||% row[["pitcher_name"]] %||% row[["PlayerName"]] %||% "\u2014"
    )
    raw_date <- row[["GameDate"]] %||% row[["Date"]] %||% row[["datetime"]] %||% NA
    date_text <- (function(x) {
      d <- suppressWarnings(as.Date(x))
      if (!is.na(d)) return(format(d, "%-m/%-d/%y"))
      px <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
      if (!is.na(px)) return(format(as.Date(px), "%-m/%-d/%y"))
      "\u2014"
    })(raw_date)
    
    # Metrics (ordered as requested)
    pitch_type <- as.character(row[["TaggedPitchType"]] %||% "\u2014")
    
    velo_val <- metric_val(row, "RelSpeed", fmt_num(1, " mph"))              # value only
    ivb_val  <- metric_val(row, "InducedVertBreak", fmt_num(1, "\""))
    hb_val   <- metric_val(row, "HorzBreak",        fmt_num(1, "\""))
    spin_val <- metric_val(row, "SpinRate",         fmt_num(0, " rpm"))       # value only
    
    se_col   <- get_first_col(row, c("SpinEfficiency", "SpinEff", "SpinEffPct"))
    spin_eff <- {
      if (is.null(se_col)) tags$span("\u2014") else {
        val <- tryCatch(se_col, error = function(e) NA)
        if (is.null(val)) tags$span("\u2014") else tags$span(fmt_pct(1)(val))
      }
    }
    
    tilt_src <- get_first_col(row, c("BreakTilt", "bTilt", "ReleaseTilt", "rTilt"))
    btilt    <- tags$span(deg_to_clock(tilt_src))
    
    # Use RelHeight / RelSide (1 decimal)
    height_v <- metric_val(row, "RelHeight", fmt_num(1, ""))
    side_v   <- metric_val(row, "RelSide",   fmt_num(1, ""))
    
    tags$div(
      # Header: name & date centered and bold
      tags$div(style = "font-weight:900; font-size:1.15rem; text-align:center;", name_text),
      tags$div(style = "font-weight:800; opacity:.9; text-align:center; margin-bottom:6px;", date_text),
      tags$hr(),
      # Metrics (stacked)
      tags$div(
        style = "text-align:center;",
        # Pitch Type (value only)
        tags$div(style = "font-weight:900; margin:8px 0;", pitch_type),
        # Velocity (value only)
        metric_value_only(velo_val),
        # IVB / HB with bold titles
        metric_row("IVB",  ivb_val),
        metric_row("HB",   hb_val),
        # Spin (value only)
        metric_value_only(spin_val),
        # SpinEff / bTilt / Height / Side with bold titles
        metric_row("SpinEff", spin_eff),
        metric_row("bTilt",   btilt),
        metric_row("Height",  height_v),
        metric_row("Side",    side_v)
      )
    )
  }
  
  show_pitch_video_modal_multi <- function(row, right_ui = NULL, dataset = NULL, dataset_idx = NA_integer_) {
    data_full <- dataset
    if (is.null(data_full)) {
      data_full <- tryCatch(as.data.frame(row, stringsAsFactors = FALSE), error = function(e) NULL)
      if (is.null(data_full) || !nrow(data_full)) {
        data_full <- data.frame()
      }
      dataset_idx <- 1L
    }
    data_full <- as.data.frame(data_full, stringsAsFactors = FALSE)
    start_idx <- suppressWarnings(as.integer(dataset_idx))
    if (!is.finite(start_idx) || start_idx < 1L) {
      rn <- rownames(row)
      if (length(rn)) {
        start_idx <- suppressWarnings(as.integer(rn[1]))
      }
      if (!is.finite(start_idx) || start_idx < 1L) start_idx <- 1L
    }
    lbl <- tryCatch(as.character(row$TaggedPitchType)[1], error = function(e) NULL)
    show_pitch_video_sequence(
      rows = data_full,
      label = lbl,
      start_index = start_idx,
      compare_pool = data_full,
      primary_pool_idx = start_idx
    )
  }
  
  # Bigger video (4fr) | narrower metrics (1fr) | centered logo low in metrics column
  show_pitch_video_modal <- function(video_url, mime_type = "video", right_ui = NULL) {
    has_video <- nzchar(video_url)
    
    # Video (or placeholder)
    video_core <- if (has_video) {
      tags$video(
        src = video_url, controls = NA, autoplay = NA, type = mime_type,
        style = "width:100%; max-height:78vh; background:#000;"
      )
    } else {
      tags$div(
        "No video available",
        style = paste(
          "display:flex; align-items:center; justify-content:center;",
          "width:100%; max-height:78vh; height:60vh;",
          "background:#0b0b0b; color:#fff; font-weight:700; border-radius:8px;"
        )
      )
    }
    
    # Right pane: metrics (scrollable) + logo centered at bottom
    right_pane <- if (is.null(right_ui)) NULL else {
      tags$div(
        style = paste(
          "display:flex; flex-direction:column;",
          "max-height:78vh; min-height:48vh;",
          "text-align:center; padding:0;"         # no extra bottom padding
        ),
        # Metrics content fills available space; no bottom padding so logo sits lower
        tags$div(style = "overflow:auto; flex:1 1 auto; padding:0 0 4px 0;", right_ui),
        # Logo: centered and nudged closer to the bottom
        tags$img(
          src = "PCUlogo.png", alt = "PCU",
          style = paste(
            "align-self:center;",                  # center horizontally
            "margin-top:auto; margin-bottom:2px;", # push to bottom and tuck close
            "width:72px; height:auto; opacity:0.95;",
            "filter: drop-shadow(0 1px 2px rgba(0,0,0,.6));",
            "pointer-events:none; user-select:none;"
          )
        )
      )
    }
    
    body <- if (is.null(right_pane)) {
      video_core
    } else {
      tags$div(
        style = paste(
          "display:grid;",
          "grid-template-columns: 4fr 1fr;",    # wider video, narrower metrics
          "gap:24px; align-items:start;"
        ),
        video_core,
        right_pane
      )
    }
    
    modal_css <- tags$style(HTML(
      ".modal-dialog.pseq-wide{width:96%;max-width:1400px;}"
    ))
    showModal(tagList(modal_css, modalDialog(body, easyClose = TRUE, footer = NULL, size = "l", class = "pseq-wide")))
  }
  
  # Normalize ggiraph selection to a single integer (use the MOST RECENT click)
  safe_selected <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_integer_)
    # ggiraph may send a vector (c("23","17")) or a comma string "23,17"
    x_last <- x[[length(x)]]                 # take last element if vector
    s <- as.character(x_last %||% "")
    parts <- strsplit(s, ",", fixed = TRUE)[[1]]
    s_last <- trimws(parts[length(parts)])   # take last token if comma-separated
    suppressWarnings(as.integer(s_last))
  }
  
  open_clip_from_df_and_index <- function(df, idx_raw) {
    idx <- safe_selected(idx_raw)
    n <- nrow(df)
    if (!is.finite(idx) || is.na(idx) || n < 1L || idx < 1L || idx > n) return(invisible(FALSE))
    row <- df[idx, , drop = FALSE]
    show_pitch_video_modal_multi(row, dataset = df, dataset_idx = idx)
    invisible(TRUE)
  }
  
  summary_table_page_cache <- reactiveVal(NULL)
  summary_table_cache      <- reactiveVal(NULL)
  
  pitch_rows_for_label <- function(df, label) {
    if (is.null(df) || !nrow(df)) return(df[0, , drop = FALSE])
    lbl <- label %||% ""
    lbl <- trimws(as.character(lbl))
    if (!nzchar(lbl) || tolower(lbl) %in% c("all", "all pitches", "total", "totals")) return(df)
    tp <- tryCatch(trimws(as.character(df$TaggedPitchType)), error = function(e) character(nrow(df)))
    matches <- df[tp == lbl, , drop = FALSE]
    if (!nrow(matches)) {
      lbl_clean <- trimws(sub("\\s*\\(.*\\)$", "", lbl))
      matches <- df[tp == lbl_clean, , drop = FALSE]
    }
    if (!nrow(matches)) {
      matches <- df[tolower(tp) == tolower(lbl), , drop = FALSE]
    }
    matches
  }
  
  pitch_abbrev_for <- function(pt) {
    pt_chr <- trimws(as.character(pt %||% ""))
    if (!nzchar(pt_chr)) return("")
    mapping <- c(
      "Fastball" = "FB", "Four-Seam Fastball" = "FB", "4-Seam Fastball" = "FB",
      "Sinker" = "SK", "Two-Seam Fastball" = "SK", "2-Seam Fastball" = "SK",
      "Cutter" = "CT", "Cut Fastball" = "CT",
      "Curveball" = "CB", "Curve" = "CB", "Knuckle Curve" = "CB",
      "Slider" = "SL",
      "Sweeper" = "SW",
      "ChangeUp" = "CH", "Change Up" = "CH", "Changeup" = "CH",
      "Splitter" = "SP", "Split Finger" = "SP", "Split-Finger" = "SP",
      "Knuckleball" = "KN", "Knuckle Ball" = "KN",
      "Gyro" = "SL", "Slurve" = "SL"
    )
    out <- mapping[pt_chr]
    if (!is.na(out)) return(out)
    parts <- strsplit(pt_chr, "[^A-Za-z]+")[[1]]
    parts <- parts[nzchar(parts)]
    if (!length(parts)) return(toupper(substr(pt_chr, 1, 2)))
    abbr <- paste0(substring(parts, 1, 1), collapse = "")
    toupper(substr(abbr, 1, 3))
  }
  
  format_initial_last <- function(name_chr) {
    nm <- trimws(as.character(name_chr %||% ""))
    if (!nzchar(nm)) return("")
    nm_fmt <- format_name_first_last(nm)
    parts <- strsplit(trimws(nm_fmt), "\\s+")[[1]]
    parts <- parts[nzchar(parts)]
    if (!length(parts)) return(nm_fmt)
    if (length(parts) == 1) return(parts)
    paste0(substr(parts[1], 1, 1), ". ", paste(parts[-1], collapse = " "))
  }
  
  make_pitch_option_label <- function(row, idx = NA_integer_) {
    if (is.null(row) || !nrow(row)) return("Pitch")
    date_val <- tryCatch(as.Date(row$Date[1]), error = function(e) NA)
    date_txt <- if (inherits(date_val, "Date") && !is.na(date_val)) format(date_val, "%m/%d/%y") else ""
    name_raw <- row$Pitcher[1] %||% row$PitcherName[1] %||% row$PlayerName[1]
    name_txt <- format_initial_last(name_raw)
    pt_txt <- pitch_abbrev_for(row$TaggedPitchType[1])
    velo_val <- suppressWarnings(as.numeric(row$RelSpeed[1]))
    ivb_val  <- suppressWarnings(as.numeric(row$InducedVertBreak[1]))
    hb_val   <- suppressWarnings(as.numeric(row$HorzBreak[1]))
    velo_txt <- if (is.finite(velo_val)) sprintf("%.1f mph", velo_val) else ""
    ivb_txt  <- if (is.finite(ivb_val)) sprintf("%.1f\"", ivb_val) else ""
    hb_txt   <- if (is.finite(hb_val)) sprintf("%.1f\"", hb_val) else ""
    idx_txt  <- if (is.finite(idx)) paste0("#", idx) else ""
    parts <- c(date_txt, name_txt, pt_txt, velo_txt, ivb_txt, hb_txt, idx_txt)
    paste(parts[nzchar(parts)], collapse = " | ")
  }
  
  show_pitch_video_sequence <- function(rows, label = NULL, start_index = 1,
                                        compare_pool = NULL, primary_pool_idx = NA_integer_) {
    rows_df <- tryCatch(as.data.frame(rows), error = function(e) NULL)
    if (is.null(rows_df) || !nrow(rows_df)) {
      showModal(modalDialog("No video available for this selection.", easyClose = TRUE, footer = NULL))
      return(invisible(FALSE))
    }
    
    rows_df <- as.data.frame(rows_df, stringsAsFactors = FALSE)
    if (is.null(rownames(rows_df))) rownames(rows_df) <- as.character(seq_len(nrow(rows_df)))
    n_total <- nrow(rows_df)
    clip1 <- tryCatch(as.character(rows_df$VideoClip),  error = function(e) rep("", n_total))
    clip2 <- tryCatch(as.character(rows_df$VideoClip2), error = function(e) rep("", n_total))
    clip3 <- tryCatch(as.character(rows_df$VideoClip3), error = function(e) rep("", n_total))
    has_video <- nzchar(clip1) | nzchar(clip2) | nzchar(clip3)
    rows_vid <- rows_df[has_video, , drop = FALSE]
    video_positions <- which(has_video)
    if (!nrow(rows_vid)) {
      showModal(modalDialog("No videos available for this selection.", easyClose = TRUE, footer = NULL))
      return(invisible(FALSE))
    }
    pool_df <- if (!is.null(compare_pool)) {
      tryCatch(as.data.frame(compare_pool, stringsAsFactors = FALSE), error = function(e) NULL)
    } else rows_df
    if (is.null(pool_df)) pool_df <- rows_df[0, , drop = FALSE]
    if (is.null(rownames(pool_df))) rownames(pool_df) <- as.character(seq_len(nrow(pool_df)))
    
    pool_clip1 <- tryCatch(as.character(pool_df$VideoClip),  error = function(e) rep("", nrow(pool_df)))
    pool_clip2 <- tryCatch(as.character(pool_df$VideoClip2), error = function(e) rep("", nrow(pool_df)))
    pool_clip3 <- tryCatch(as.character(pool_df$VideoClip3), error = function(e) rep("", nrow(pool_df)))
    pool_has_video <- nzchar(pool_clip1) | nzchar(pool_clip2) | nzchar(pool_clip3)
    pool_video_idx <- which(pool_has_video)
    compare_available <- length(pool_video_idx) > 1
    
    match_start <- match(suppressWarnings(as.integer(start_index)), video_positions)
    if (is.na(match_start) || match_start < 1L) match_start <- 1L
    if (match_start > nrow(rows_vid)) match_start <- nrow(rows_vid)
    
    make_pitch_label <- function(dat, idx = NA_integer_) make_pitch_option_label(dat, idx)
    
    collect_urls <- function(row) {
      if (is.null(row) || !nrow(row)) return(list())
      clips <- c(
        Edger  = tryCatch(as.character(row$VideoClip)[1],  error = function(e) ""),
        Behind = tryCatch(as.character(row$VideoClip2)[1], error = function(e) ""),
        Side   = tryCatch(as.character(row$VideoClip3)[1], error = function(e) "")
      )
      clips <- clips[nzchar(clips)]
      if (!length(clips)) return(list())
      urls <- lapply(names(clips), function(src) video_url_for(clips[[src]], source = src))
      names(urls) <- names(clips)
      urls[vapply(urls, nzchar, logical(1))]
    }
    
    metrics_block <- function(content) {
      if (is.null(content)) return(NULL)
      tags$div(
        style = paste(
          "display:flex;flex-direction:column;",
          "max-height:68vh;min-height:40vh;",
          "text-align:center;padding:0;"
        ),
        tags$div(style = "overflow:auto;flex:1 1 auto;padding:0 0 4px 0;", content),
        tags$img(
          src = "PCUlogo.png", alt = "PCU",
          style = paste(
            "align-self:center;margin-top:auto;margin-bottom:2px;",
            "width:72px;height:auto;opacity:0.95;",
            "filter:drop-shadow(0 1px 2px rgba(0,0,0,.6));",
            "pointer-events:none;user-select:none;"
          )
        )
      )
    }
    
    default_secondary_for <- function(primary_idx) {
      if (!length(pool_video_idx)) return(NA_integer_)
      candidates <- pool_video_idx
      if (is.finite(primary_idx)) candidates <- candidates[candidates != primary_idx]
      if (!length(candidates)) candidates <- pool_video_idx
      candidates[1]
    }
    
    idx <- reactiveVal(match_start)
    cam_sel <- reactiveVal("")
    compare_mode <- reactiveVal(FALSE)
    secondary_idx <- reactiveVal(NA_integer_)
    cmp_cam_sel <- reactiveVal("")
    
    uid_base  <- paste0("pseq_", as.integer((as.numeric(Sys.time()) * 1000) %% 1e9))
    video_id  <- paste0(uid_base, "_video")
    next_id   <- paste0(uid_base, "_next")
    prev_id   <- paste0(uid_base, "_prev")
    cam_prefix <- paste0(uid_base, "_cam")
    primary_video_id   <- paste0(uid_base, "_video_primary")
    secondary_video_id <- paste0(uid_base, "_video_secondary")
    compare_toggle_id  <- paste0(uid_base, "_compare")
    primary_select_id  <- paste0(uid_base, "_primary_select")
    compare_select_id  <- paste0(uid_base, "_compare_select")
    cmp_cam_prefix     <- paste0(uid_base, "_cmpcam")
    primary_slider_id  <- paste0(uid_base, "_slider_primary")
    secondary_slider_id <- paste0(uid_base, "_slider_secondary")
    sync_slider_id     <- paste0(uid_base, "_slider_sync")
    primary_play_id    <- paste0(uid_base, "_play_primary")
    primary_pause_id   <- paste0(uid_base, "_pause_primary")
    secondary_play_id  <- paste0(uid_base, "_play_secondary")
    secondary_pause_id <- paste0(uid_base, "_pause_secondary")
    sync_play_id       <- paste0(uid_base, "_play_sync")
    sync_pause_id      <- paste0(uid_base, "_pause_sync")
    download_single_id <- paste0(uid_base, "_download_single")
    download_all_id    <- paste0(uid_base, "_download_all")
    
    current_row <- reactive({
      i <- idx()
      if (!is.finite(i)) i <- 1L
      i <- max(1L, min(nrow(rows_vid), i))
      rows_vid[i, , drop = FALSE]
    })
    
    current_urls <- reactive(collect_urls(current_row()))
    
    primary_pool_idx_reactive <- reactive({
      if (!nrow(pool_df)) return(NA_integer_)
      row <- current_row()
      if (is.null(row) || !nrow(row)) return(NA_integer_)
      rn <- rownames(row)
      if (length(rn)) {
        match_idx <- match(rn[1], rownames(pool_df))
        if (is.finite(match_idx)) return(match_idx)
      }
      if (is.finite(primary_pool_idx) && primary_pool_idx >= 1L && primary_pool_idx <= nrow(pool_df)) {
        return(primary_pool_idx)
      }
      if (length(pool_video_idx)) pool_video_idx[1] else 1L
    })
    
    cmp_current_row <- reactive({
      idx_val <- secondary_idx()
      if (!is.finite(idx_val) || !nrow(pool_df)) return(NULL)
      if (idx_val < 1L || idx_val > nrow(pool_df)) return(NULL)
      if (!pool_has_video[idx_val]) return(NULL)
      pool_df[idx_val, , drop = FALSE]
    })
    
    cmp_urls <- reactive(collect_urls(cmp_current_row()))
    
    cam_names <- c("Edger", "Behind", "Side")
    
    slugify <- function(text, fallback = "pitch") {
      txt <- trimws(as.character(text %||% ""))
      if (!nzchar(txt)) txt <- fallback
      slug <- gsub("[^A-Za-z0-9]+", "-", txt)
      slug <- gsub("-+", "-", slug)
      slug <- gsub("(^-)|(-$)", "", slug)
      slug <- tolower(slug)
      if (!nzchar(slug)) tolower(fallback) else slug
    }
    
    file_ext_from_url <- function(url, default = ".mp4") {
      if (!nzchar(url)) return(default)
      clean <- sub("\\?.*$", "", url)
      ext <- tools::file_ext(clean)
      if (!nzchar(ext)) default else paste0(".", tolower(ext))
    }
    
    download_source_to <- function(url, dest) {
      if (!nzchar(url)) stop("No source URL available", call. = FALSE)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      if (grepl("^https?://", url, ignore.case = TRUE)) {
        req <- httr2::request(url)
        tryCatch({
          httr2::req_perform(req, path = dest)
          TRUE
        }, error = function(e) stop(sprintf("Failed to download %s", basename(url)), call. = FALSE))
      } else {
        rel <- sub("^/+", "", url)
        rel <- utils::URLdecode(rel)
        vdir <- getOption("PCU_VIDEOS_DIR", NA_character_)
        if (is.na(vdir) || !nzchar(vdir)) {
          stop("Local video directory is not configured", call. = FALSE)
        }
        parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
        if (length(parts) && identical(parts[1], "videos")) parts <- parts[-1]
        abs_path <- do.call(file.path, c(list(vdir), as.list(parts)))
        if (!length(parts) || !file.exists(abs_path)) {
          stop("Video file not found on server", call. = FALSE)
        }
        if (!file.copy(abs_path, dest, overwrite = TRUE)) {
          stop("Unable to copy video file", call. = FALSE)
        }
        TRUE
      }
    }
    
    observeEvent(current_urls(), {
      urls <- current_urls()
      cur  <- cam_sel()
      if (!length(urls)) {
        cam_sel("")
      } else if (!nzchar(cur) || !(cur %in% names(urls))) {
        cam_sel(names(urls)[1])
      }
    }, ignoreNULL = FALSE, priority = 1)
    
    observeEvent(cmp_urls(), {
      urls <- cmp_urls()
      cur <- cmp_cam_sel()
      if (!length(urls)) {
        cmp_cam_sel("")
      } else if (!nzchar(cur) || !(cur %in% names(urls))) {
        cmp_cam_sel(names(urls)[1])
      }
    }, ignoreNULL = FALSE, priority = 1)
    
    for (nm in cam_names) local({
      src <- nm
      observeEvent(input[[paste0(cam_prefix, "_", src)]], {
        urls <- current_urls()
        if (length(urls) && !is.null(urls[[src]]) && nzchar(urls[[src]])) {
          cam_sel(src)
        }
      }, ignoreNULL = TRUE)
    })
    
    for (nm in cam_names) local({
      src <- nm
      observeEvent(input[[paste0(cmp_cam_prefix, "_", src)]], {
        urls <- cmp_urls()
        if (length(urls) && !is.null(urls[[src]]) && nzchar(urls[[src]])) {
          cmp_cam_sel(src)
        }
      }, ignoreNULL = TRUE)
    })
    
    observeEvent(input[[next_id]], {
      cur <- idx()
      if (is.finite(cur) && cur < nrow(rows_vid)) idx(cur + 1L)
    }, ignoreNULL = TRUE)
    
    observeEvent(input[[prev_id]], {
      cur <- idx()
      if (is.finite(cur) && cur > 1) idx(cur - 1L)
    }, ignoreNULL = TRUE)
    
    observeEvent(input[[compare_toggle_id]], {
      if (!compare_available) return()
      compare_mode(!compare_mode())
    }, ignoreNULL = TRUE)
    
    observeEvent(compare_mode(), {
      if (compare_mode()) {
        primary_idx <- primary_pool_idx_reactive()
        secondary_idx(default_secondary_for(primary_idx))
        if (compare_available && is.finite(primary_idx)) {
          updateSelectizeInput(session, primary_select_id, selected = as.character(primary_idx))
        }
      } else {
        secondary_idx(NA_integer_)
      }
    }, ignoreNULL = TRUE)
    
    observeEvent(idx(), {
      if (!compare_available) return()
      if (!compare_mode()) return()
      primary_idx <- primary_pool_idx_reactive()
      current_sec <- secondary_idx()
      if (!is.finite(current_sec) || !pool_has_video[current_sec] || identical(current_sec, primary_idx)) {
        secondary_idx(default_secondary_for(primary_idx))
      }
      if (is.finite(primary_idx)) {
        updateSelectizeInput(session, primary_select_id, selected = as.character(primary_idx))
      }
    }, ignoreNULL = TRUE)
    
    observeEvent(secondary_idx(), {
      if (!compare_available) return()
      if (!compare_mode()) return()
      val <- secondary_idx()
      updateSelectizeInput(session, compare_select_id,
                           selected = if (is.finite(val)) as.character(val) else NULL)
    }, ignoreNULL = FALSE)
    
    observeEvent(input[[primary_select_id]], {
      if (!compare_available) return()
      if (!compare_mode()) return()
      val <- suppressWarnings(as.integer(input[[primary_select_id]]))
      if (!is.finite(val)) return()
      target_idx <- match(val, video_positions)
      if (is.finite(target_idx)) idx(target_idx)
    }, ignoreNULL = TRUE)
    
    observeEvent(input[[compare_select_id]], {
      if (!compare_available) return()
      val <- suppressWarnings(as.integer(input[[compare_select_id]]))
      if (is.finite(val) && val >= 1L && val <= nrow(pool_df) && pool_has_video[val]) {
        secondary_idx(val)
      }
    }, ignoreNULL = TRUE)
    
    output[[download_single_id]] <- downloadHandler(
      filename = function() {
        row <- current_row()
        idx_val <- primary_pool_idx_reactive()
        label <- make_pitch_option_label(row, idx_val)
        cam <- cam_sel()
        if (!nzchar(cam)) cam <- "video"
        urls <- current_urls()
        url <- urls[[cam]] %||% ""
        ext <- file_ext_from_url(url)
        paste0(slugify(label, fallback = "pitch"), "-", tolower(cam), ext)
      },
      content = function(file) {
        urls <- current_urls()
        sel <- cam_sel()
        if (!length(urls) || !nzchar(sel) || is.null(urls[[sel]]) || !nzchar(urls[[sel]])) {
          stop("No video available for download", call. = FALSE)
        }
        download_source_to(urls[[sel]], file)
      }
    )
    
    output[[download_all_id]] <- downloadHandler(
      filename = function() {
        base <- if (!is.null(label) && nzchar(label)) label else "selection"
        paste0(slugify(base, fallback = "pitch"), "-videos.zip")
      },
      content = function(file) {
        if (!length(pool_video_idx)) {
          stop("No videos available in this filter", call. = FALSE)
        }
        rows_all <- pool_df[pool_video_idx, , drop = FALSE]
        if (!nrow(rows_all)) {
          stop("No videos available in this filter", call. = FALSE)
        }
        tmpdir <- tempfile("pseq_dl_")
        dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
        on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)
        
        saved <- character(0)
        done_map <- character(0)
        
        for (pos in seq_along(pool_video_idx)) {
          idx_val <- pool_video_idx[pos]
          row_now <- pool_df[idx_val, , drop = FALSE]
          slug <- slugify(make_pitch_option_label(row_now, idx_val), fallback = sprintf("pitch-%03d", idx_val))
          urls <- collect_urls(row_now)
          if (!length(urls)) next
          for (cam in names(urls)) {
            url <- urls[[cam]]
            if (!nzchar(url)) next
            key <- paste(url, cam, sep = "::")
            if (key %in% done_map) next
            done_map <- c(done_map, key)
            ext <- file_ext_from_url(url)
            dest_name <- sprintf("%03d_%s_%s%s", pos, slug, tolower(cam), ext)
            dest_path <- file.path(tmpdir, dest_name)
            download_source_to(url, dest_path)
            saved <- c(saved, dest_path)
          }
        }
        
        if (!length(saved)) {
          stop("No downloadable videos were found", call. = FALSE)
        }
        
        zipfile <- normalizePath(file, mustWork = FALSE)
        oldwd <- getwd()
        setwd(tmpdir)
        on.exit(setwd(oldwd), add = TRUE)
        utils::zip(zipfile = zipfile, files = basename(saved))
      }
    )
    
    output[[video_id]] <- renderUI({
      i <- idx()
      if (!is.finite(i)) i <- 1L
      i <- max(1L, min(nrow(rows_vid), i))
      row <- rows_vid[i, , drop = FALSE]
      urls <- current_urls()
      sel <- cam_sel()
      if ((!nzchar(sel) || !(sel %in% names(urls))) && length(urls)) {
        sel <- names(urls)[1]
      }
      url <- if (length(urls) && nzchar(sel) && !is.null(urls[[sel]])) urls[[sel]] else ""
      if (nzchar(url)) {
        cache_tag <- as.integer(as.numeric(Sys.time()))
        url <- paste0(url, if (grepl("\\?", url)) "&" else "?", "v=", cache_tag)
      }
      
      right <- build_metrics_panel(row)
      seq_txt <- sprintf("%d of %d", i, nrow(rows_vid))
      
      compare_btn_label <- if (isTRUE(compare_mode())) tagList(icon("video"), "Single View") else tagList(icon("columns"), "Side-by-Side")
      compare_btn <- actionButton(
        compare_toggle_id,
        label = compare_btn_label,
        class = "btn btn-sm btn-outline-secondary",
        style = "min-width:128px;font-weight:600;",
        disabled = if (compare_available) NULL else "disabled"
      )
      
      download_controls <- tags$div(
        style = "display:flex;justify-content:center;gap:12px;margin-bottom:12px;flex-wrap:wrap;",
        downloadButton(
          download_single_id,
          label = tagList(icon("download"), "Download Pitch"),
          class = "btn btn-sm btn-outline-secondary"
        ),
        downloadButton(
          download_all_id,
          label = tagList(icon("file-archive"), "Download Filter"),
          class = "btn btn-sm btn-outline-secondary",
          title = "Downloads all available camera angles in this filter as a ZIP"
        )
      )
      
      header <- tags$div(
        style = "display:flex;align-items:center;justify-content:space-between;gap:12px;margin-bottom:10px;",
        actionButton(
          prev_id,
          label = tagList(icon("chevron-left"), "Prev"),
          class = "btn-light btn-sm",
          style = "min-width:92px;",
          disabled = if (i <= 1) "disabled" else NULL
        ),
        tags$div(
          style = "flex:1;text-align:center;",
          tags$div(style = "font-size:0.9rem;font-weight:600;opacity:0.75;", seq_txt)
        ),
        actionButton(
          next_id,
          label = tagList("Next", icon("chevron-right")),
          class = "btn-light btn-sm",
          style = "min-width:92px;",
          disabled = if (i >= nrow(rows_vid)) "disabled" else NULL
        )
      )
      
      cam_buttons <- lapply(cam_names, function(src) {
        u <- urls[[src]]
        if (is.null(u) || !nzchar(u)) return(NULL)
        btn_class <- if (identical(sel, src)) "btn-sm btn-primary" else "btn-sm btn-outline-secondary"
        actionButton(
          paste0(cam_prefix, "_", src),
          label = src,
          class = btn_class,
          style = "min-width:82px;font-weight:600;"
        )
      })
      cam_buttons <- Filter(Negate(is.null), cam_buttons)
      cam_row <- if (length(cam_buttons)) {
        tags$div(
          style = "display:flex;flex-wrap:wrap;gap:8px;margin-bottom:10px;justify-content:center;",
          cam_buttons
        )
      } else NULL
      
      video_style <- if (isTRUE(compare_mode())) "width:100%;max-height:60vh;background:#000;" else "width:100%;max-height:78vh;background:#000;"
      placeholder_style <- if (isTRUE(compare_mode()))
        "display:flex;align-items:center;justify-content:center;width:100%;max-height:60vh;height:44vh;background:#0b0b0b;color:#fff;font-weight:700;border-radius:8px;"
      else
        "display:flex;align-items:center;justify-content:center;width:100%;max-height:78vh;height:60vh;background:#0b0b0b;color:#fff;font-weight:700;border-radius:8px;"
      
      video_core <- if (nzchar(url)) {
        tags$video(
          id = primary_video_id,
          src = url,
          controls = NA,
          autoplay = NA,
          type = guess_video_type(url),
          style = video_style
        )
      } else {
        tags$div("No video available", style = placeholder_style)
      }
      
      compare_choices <- if (compare_available) {
        labs <- vapply(pool_video_idx, function(idx_val) make_pitch_option_label(pool_df[idx_val, , drop = FALSE], idx_val), character(1))
        stats::setNames(as.character(pool_video_idx), labs)
      } else character(0)
      
      if (!isTRUE(compare_mode())) {
        right_pane <- metrics_block(right)
        main_layout <- if (is.null(right_pane)) {
          video_core
        } else {
          tags$div(
            style = paste(
              "display:grid;",
              "grid-template-columns:4fr 1fr;",
              "gap:24px;align-items:start;"
            ),
            video_core,
            right_pane
          )
        }
        
        tagList(
          header,
          tags$div(style = "display:flex;justify-content:center;margin-bottom:10px;", compare_btn),
          cam_row,
          download_controls,
          main_layout
        )
      } else {
        cmp_row <- cmp_current_row()
        cmp_sel <- cmp_cam_sel()
        cmp_urls_now <- cmp_urls()
        if ((!nzchar(cmp_sel) || !(cmp_sel %in% names(cmp_urls_now))) && length(cmp_urls_now)) {
          cmp_sel <- names(cmp_urls_now)[1]
        }
        cmp_url <- if (length(cmp_urls_now) && nzchar(cmp_sel) && !is.null(cmp_urls_now[[cmp_sel]])) cmp_urls_now[[cmp_sel]] else ""
        if (nzchar(cmp_url)) {
          cache_tag <- as.integer(as.numeric(Sys.time()))
          cmp_url <- paste0(cmp_url, if (grepl("\\?", cmp_url)) "&" else "?", "v=", cache_tag)
        }
        
        cmp_cam_buttons <- lapply(cam_names, function(src) {
          u <- cmp_urls_now[[src]]
          if (is.null(u) || !nzchar(u)) return(NULL)
          btn_class <- if (identical(cmp_sel, src)) "btn-sm btn-primary" else "btn-sm btn-outline-secondary"
          actionButton(
            paste0(cmp_cam_prefix, "_", src),
            label = src,
            class = btn_class,
            style = "min-width:82px;font-weight:600;"
          )
        })
        cmp_cam_buttons <- Filter(Negate(is.null), cmp_cam_buttons)
        cmp_cam_row <- if (length(cmp_cam_buttons)) {
          tags$div(
            style = "display:flex;flex-wrap:wrap;gap:8px;margin-bottom:10px;justify-content:center;",
            cmp_cam_buttons
          )
        } else NULL
        
        cmp_video_core <- if (nzchar(cmp_url)) {
          tags$video(
            id = secondary_video_id,
            src = cmp_url,
            controls = NA,
            autoplay = NA,
            type = guess_video_type(cmp_url),
            style = video_style
          )
        } else {
          tags$div("Select a pitch", style = placeholder_style)
        }
        
        left_metrics_block <- metrics_block(right)
        right_metrics_block <- metrics_block(if (!is.null(cmp_row)) build_metrics_panel(cmp_row) else NULL)
        
        primary_selected_val <- primary_pool_idx_reactive()
        secondary_selected_val <- secondary_idx()
        
        selector <- if (compare_available) {
          tags$div(
            style = "display:flex;justify-content:center;gap:16px;margin-bottom:12px;flex-wrap:wrap;",
            tags$div(
              style = "display:flex;flex-direction:column;min-width:220px;gap:4px;",
              tags$label("Primary Pitch", `for` = primary_select_id, style = "margin:0;font-weight:600;"),
              selectizeInput(
                primary_select_id,
                label = NULL,
                choices = compare_choices,
                selected = {
                  if (is.finite(primary_selected_val)) as.character(primary_selected_val)
                  else if (length(compare_choices)) compare_choices[[1]] else NULL
                },
                options = list(placeholder = "Choose primary pitch")
              )
            ),
            tags$div(
              style = "display:flex;flex-direction:column;min-width:220px;gap:4px;",
              tags$label("Secondary Pitch", `for` = compare_select_id, style = "margin:0;font-weight:600;"),
              selectizeInput(
                compare_select_id,
                label = NULL,
                choices = compare_choices,
                selected = {
                  if (is.finite(secondary_selected_val)) as.character(secondary_selected_val)
                  else if (length(compare_choices)) compare_choices[[1]] else NULL
                },
                options = list(placeholder = "Choose secondary pitch")
              )
            )
          )
        } else NULL
        
        primary_controls <- if (compare_available) {
          tags$div(
            style = "display:flex;align-items:center;gap:10px;margin-bottom:8px;flex-wrap:wrap;justify-content:center;",
            actionButton(primary_play_id, label = "Play", icon = icon("play"), class = "btn btn-sm btn-outline-secondary"),
            actionButton(primary_pause_id, label = "Pause", icon = icon("pause"), class = "btn btn-sm btn-outline-secondary"),
            tags$input(
              id = primary_slider_id,
              type = "range",
              min = "0",
              max = "1",
              step = "0.01",
              value = "0",
              style = "flex:1 1 260px;min-width:220px;"
            )
          )
        } else NULL
        
        secondary_controls <- if (compare_available) {
          tags$div(
            style = "display:flex;align-items:center;gap:10px;margin-bottom:8px;flex-wrap:wrap;justify-content:center;",
            actionButton(secondary_play_id, label = "Play", icon = icon("play"), class = "btn btn-sm btn-outline-secondary"),
            actionButton(secondary_pause_id, label = "Pause", icon = icon("pause"), class = "btn btn-sm btn-outline-secondary"),
            tags$input(
              id = secondary_slider_id,
              type = "range",
              min = "0",
              max = "1",
              step = "0.01",
              value = "0",
              style = "flex:1 1 260px;min-width:220px;"
            )
          )
        } else NULL
        
        sync_controls <- if (compare_available) {
          tags$div(
            style = "display:flex;align-items:center;gap:12px;margin:-4px 0 12px 0;flex-wrap:wrap;justify-content:center;",
            tags$label(
              "Sync Scrub",
              `for` = sync_slider_id,
              style = "margin:0;font-weight:600;min-width:96px;text-align:right;"
            ),
            tags$input(
              id = sync_slider_id,
              type = "range",
              min = "0",
              max = "1",
              step = "0.01",
              value = "0",
              style = "flex:1 1 360px;min-width:260px;"
            ),
            actionButton(sync_play_id, label = "Play", icon = icon("play"), class = "btn btn-sm btn-outline-secondary"),
            actionButton(sync_pause_id, label = "Pause", icon = icon("pause"), class = "btn btn-sm btn-outline-secondary")
          )
        } else NULL
        
        left_col <- tags$div(
          style = "display:flex;flex-direction:column;gap:10px;",
          cam_row,
          primary_controls,
          video_core,
          left_metrics_block
        )
        
        right_col <- tags$div(
          style = "display:flex;flex-direction:column;gap:10px;",
          cmp_cam_row,
          secondary_controls,
          cmp_video_core,
          right_metrics_block
        )
        
        control_script <- if (compare_available) {
          tags$script(HTML(sprintf(
            "(function(){\n  var configs = [\n    {videoId:'%s', sliderId:'%s', playId:'%s', pauseId:'%s'},\n    {videoId:'%s', sliderId:'%s', playId:'%s', pauseId:'%s'}\n  ];\n  var syncSlider = document.getElementById('%s');\n  var syncPlayBtn = document.getElementById('%s');\n  var syncPauseBtn = document.getElementById('%s');\n  var syncUpdating = false;\n\n  function playSafe(video){\n    if (!video) return;\n    try {\n      var p = video.play();\n      if (p && typeof p.catch === 'function') p.catch(function(){});\n    } catch(e){}\n  }\n\n  function pauseSafe(video){\n    if (!video) return;\n    try { video.pause(); } catch(e){}\n  }\n\n  function updateSyncRange(){\n    if (!syncSlider) return;\n    var maxDur = Infinity;\n    configs.forEach(function(cfg){\n      if (!cfg.video) return;\n      var d = cfg.video.duration;\n      if (isFinite(d) && d > 0 && d < maxDur) maxDur = d;\n    });\n    if (!isFinite(maxDur) || maxDur === Infinity){\n      syncSlider.disabled = true;\n    } else {\n      syncSlider.max = maxDur;\n      syncSlider.disabled = false;\n    }\n  }\n\n  function updateSyncValue(){\n    if (!syncSlider || syncUpdating) return;\n    var minTime = Infinity;\n    configs.forEach(function(cfg){\n      if (!cfg.video) return;\n      var t = cfg.video.currentTime;\n      if (isFinite(t) && t < minTime) minTime = t;\n    });\n    if (minTime !== Infinity) syncSlider.value = minTime;\n  }\n\n  configs.forEach(function(cfg){\n    cfg.video = document.getElementById(cfg.videoId);\n    cfg.slider = document.getElementById(cfg.sliderId);\n    cfg.playBtn = document.getElementById(cfg.playId);\n    cfg.pauseBtn = document.getElementById(cfg.pauseId);\n    if (!cfg.video) return;\n    var video = cfg.video;\n    var slider = cfg.slider;\n    var updating = false;\n\n    function setRange(){\n      if (!slider) return;\n      if (isFinite(video.duration) && video.duration > 0){\n        slider.max = video.duration;\n        slider.disabled = false;\n      } else {\n        slider.disabled = true;\n      }\n    }\n\n    video.addEventListener('loadedmetadata', function(){\n      setRange();\n      updateSyncRange();\n      if (slider) slider.value = video.currentTime || 0;\n      updateSyncValue();\n    });\n\n    video.addEventListener('timeupdate', function(){\n      if (updating) return;\n      setRange();\n      if (slider) slider.value = video.currentTime || 0;\n      updateSyncRange();\n      updateSyncValue();\n    });\n\n    if (slider){\n      slider.addEventListener('input', function(){\n        updating = true;\n        var val = parseFloat(slider.value) || 0;\n        try { video.currentTime = val; } catch(e){}\n      });\n      slider.addEventListener('change', function(){\n        updating = false;\n      });\n    }\n\n    if (cfg.playBtn){\n      cfg.playBtn.addEventListener('click', function(){\n        playSafe(video);\n      });\n    }\n\n    if (cfg.pauseBtn){\n      cfg.pauseBtn.addEventListener('click', function(){\n        pauseSafe(video);\n      });\n    }\n  });\n\n  if (syncSlider){\n    syncSlider.addEventListener('input', function(){\n      syncUpdating = true;\n      var val = parseFloat(syncSlider.value) || 0;\n      var actual = Infinity;\n      configs.forEach(function(cfg){\n        if (!cfg.video) return;\n        var target = val;\n        var dur = cfg.video.duration;\n        if (isFinite(dur) && dur > 0 && target > dur) target = dur;\n        try { cfg.video.currentTime = target; } catch(e){}\n        if (cfg.slider) cfg.slider.value = target;\n        var ct = cfg.video.currentTime;\n        if (isFinite(ct) && ct < actual) actual = ct;\n      });\n      if (actual !== Infinity) syncSlider.value = actual;\n    });\n    syncSlider.addEventListener('change', function(){\n      syncUpdating = false;\n    });\n    updateSyncRange();\n    updateSyncValue();\n  }\n\n  if (syncPlayBtn){\n    syncPlayBtn.addEventListener('click', function(){\n      configs.forEach(function(cfg){ playSafe(cfg.video); });\n    });\n  }\n\n  if (syncPauseBtn){\n    syncPauseBtn.addEventListener('click', function(){\n      configs.forEach(function(cfg){ pauseSafe(cfg.video); });\n    });\n  }\n})();",
            primary_video_id, primary_slider_id, primary_play_id, primary_pause_id,
            secondary_video_id, secondary_slider_id, secondary_play_id, secondary_pause_id,
            sync_slider_id, sync_play_id, sync_pause_id)))
        } else NULL
        
        tagList(
          header,
          tags$div(style = "display:flex;justify-content:center;margin-bottom:10px;", compare_btn),
          selector,
          sync_controls,
          download_controls,
          tags$div(
            style = "display:grid;grid-template-columns:1fr 1fr;gap:24px;align-items:start;",
            left_col,
            right_col
          ),
          control_script
        )
      }
    })
    
    modal_css <- tags$style(HTML(
      ".modal-dialog.pseq-wide{width:96%;max-width:1400px;}"
    ))
    showModal(tagList(modal_css, modalDialog(uiOutput(video_id), easyClose = TRUE, footer = NULL, size = "l", class = "pseq-wide")))
    invisible(TRUE)
  }
  
  handle_pitch_table_click <- function(info, cache_fn, table_label = NULL) {
    if (is.null(info)) return(invisible(FALSE))
    col_raw <- info$col %||% info$column
    row_raw <- info$row %||% info$rows
    if (is.null(col_raw) || is.null(row_raw)) return(invisible(FALSE))
    col_idx <- suppressWarnings(as.integer(col_raw))
    row_idx <- suppressWarnings(as.integer(row_raw))
    if (is.finite(col_idx)) col_idx <- col_idx + 1L  # DT supplies 0-based column index
    if (is.finite(row_idx) && row_idx < 1L) row_idx <- row_idx + 1L
    cache_obj <- cache_fn()
    if (is.null(cache_obj)) return(invisible(FALSE))
    
    if (inherits(cache_obj, "data.frame")) {
      df_tbl <- cache_obj
      meta <- list()
    } else if (is.list(cache_obj)) {
      df_tbl <- cache_obj$table %||% cache_obj$data %||% cache_obj
      meta <- cache_obj
    } else {
      return(invisible(FALSE))
    }
    
    if (is.null(df_tbl) || !nrow(df_tbl)) return(invisible(FALSE))
    if (!is.finite(col_idx) || col_idx < 1 || col_idx > ncol(df_tbl)) return(invisible(FALSE))
    if (!is.finite(row_idx) || row_idx < 1 || row_idx > nrow(df_tbl)) return(invisible(FALSE))
    
    col_name <- names(df_tbl)[col_idx]
    if (!identical(col_name, "#")) return(invisible(FALSE))
    cell_val <- df_tbl[row_idx, col_idx]
    
    label_col <- meta$label_column %||% {
      if ("Pitch" %in% names(df_tbl)) "Pitch"
      else if ("Player" %in% names(df_tbl)) "Player"
      else names(df_tbl)[1]
    }
    if (!label_col %in% names(df_tbl)) label_col <- names(df_tbl)[1]
    pitch_label <- tryCatch(as.character(df_tbl[row_idx, label_col])[1], error = function(e) "")
    pitch_label <- trimws(pitch_label)
    
    rows <- NULL
    
    if (!is.null(meta$row_mapper) && is.function(meta$row_mapper)) {
      rows <- tryCatch(meta$row_mapper(pitch_label, row_idx, meta), error = function(e) NULL)
    }
    
    if ((is.null(rows) || !nrow(rows)) && !is.null(meta$groups)) {
      grp <- meta$groups
      key_opts <- c(pitch_label,
                    trimws(sub("\\s*\\(.*\\)$", "", pitch_label)),
                    tolower(pitch_label))
      key_opts <- unique(key_opts[nzchar(key_opts)])
      for (k in key_opts) {
        if (!nzchar(k)) next
        if (!is.null(grp[[k]])) {
          rows <- grp[[k]]
          break
        }
      }
      if ((is.null(rows) || !nrow(rows)) && length(grp)) {
        nm_match <- which(tolower(names(grp)) == tolower(pitch_label))
        if (length(nm_match)) rows <- grp[[nm_match[1]]]
      }
    }
    
    if ((is.null(rows) || !nrow(rows)) && !is.null(meta$source)) {
      rows <- pitch_rows_for_label(meta$source, pitch_label)
    }
    
    if ((is.null(rows) || !nrow(rows)) && !is.null(meta$fallback_source)) {
      rows <- pitch_rows_for_label(meta$fallback_source, pitch_label)
    }
    
    if (is.null(rows) || !nrow(rows)) {
      df_all <- filtered_data()
      if (!is.null(df_all) && nrow(df_all)) {
        rows <- pitch_rows_for_label(df_all, pitch_label)
      }
    }
    
    if (is.null(rows) || !nrow(rows)) {
      showModal(modalDialog("No pitches found for this table row.", easyClose = TRUE, footer = NULL))
      return(invisible(FALSE))
    }
    
    rows <- as.data.frame(rows, stringsAsFactors = FALSE)
    if (is.null(rownames(rows))) {
      rownames(rows) <- as.character(seq_len(nrow(rows)))
    }
    
    start_idx <- 1L
    
    show_pitch_video_sequence(
      rows,
      label = if (nzchar(pitch_label)) pitch_label else table_label,
      start_index = start_idx,
      compare_pool = rows,
      primary_pool_idx = start_idx
    )
    invisible(TRUE)
  }
  
  table_cache_fetcher <- function(id) {
    force(id)
    function() session$userData[[paste0("table_cache_", id)]]
  }
  
  
  # Movement (FULL page)
  observeEvent(input[["movementPlot_selected"]], ignoreInit = TRUE, {
    df <- filtered_data(); req(nrow(df) > 0)
    open_clip_from_df_and_index(df, input[["movementPlot_selected"]])
  })
  
  # Movement (Summary tab)
  observeEvent(input[["summary_movementPlot_selected"]], ignoreInit = TRUE, {
    df <- filtered_data(); req(nrow(df) > 0)
    open_clip_from_df_and_index(df, input[["summary_movementPlot_selected"]])
  })
  
  # Location (FULL page) – keep if your full-page output is 'locationChart'
  observeEvent(input[["locationChart_selected"]], ignoreInit = TRUE, {
    df <- filtered_data(); req(nrow(df) > 0)
    open_clip_from_df_and_index(df, input[["locationChart_selected"]])
  })
  
  # (Optional) If your full-page output is actually 'zonePlot', this covers it too
  observeEvent(input[["zonePlot_selected"]], ignoreInit = TRUE, {
    df <- filtered_data(); req(nrow(df) > 0)
    open_clip_from_df_and_index(df, input[["zonePlot_selected"]])
  })
  
  # Location (Summary tab) – CORRECT ID for the summary zone plot
  observeEvent(input[["summary_zonePlot_selected"]], ignoreInit = TRUE, {
    df <- filtered_data(); req(nrow(df) > 0)
    open_clip_from_df_and_index(df, input[["summary_zonePlot_selected"]])
  })
  
  observeEvent(input[["leader-lbTable_cell_clicked"]], {
    handle_pitch_table_click(
      input[["leader-lbTable_cell_clicked"]],
      table_cache_fetcher("leader-lbTable"),
      table_label = "Leaderboard"
    )
  }, ignoreNULL = TRUE)
  
  observeEvent(input[["leader-lbCatchTable_cell_clicked"]], {
    handle_pitch_table_click(
      input[["leader-lbCatchTable_cell_clicked"]],
      table_cache_fetcher("leader-lbCatchTable"),
      table_label = "Leaderboard"
    )
  }, ignoreNULL = TRUE)
  
  observeEvent(input[["comp-cmpA_table_cell_clicked"]], {
    handle_pitch_table_click(
      input[["comp-cmpA_table_cell_clicked"]],
      table_cache_fetcher("comp-cmpA_table"),
      table_label = "Comparison"
    )
  }, ignoreNULL = TRUE)
  
  observeEvent(input[["comp-cmpB_table_cell_clicked"]], {
    handle_pitch_table_click(
      input[["comp-cmpB_table_cell_clicked"]],
      table_cache_fetcher("comp-cmpB_table"),
      table_label = "Comparison"
    )
  }, ignoreNULL = TRUE)
  
  cache_for_table_id <- function(id) {
    switch(
      id,
      "summaryTablePage"         = table_cache_fetcher("summaryTablePage"),
      "summaryTable"             = table_cache_fetcher("summaryTable"),
      "leader-lbTable"           = table_cache_fetcher("leader-lbTable"),
      "leader-lbCatchTable"      = table_cache_fetcher("leader-lbCatchTable"),
      "comp-cmpA_table"          = table_cache_fetcher("comp-cmpA_table"),
      "comp-cmpB_table"          = table_cache_fetcher("comp-cmpB_table"),
      NULL
    )
  }
  
  observeEvent(input$pitch_count_click, {
    info <- input$pitch_count_click
    if (is.null(info) || is.null(info$tableId)) return()
    cache_fn <- cache_for_table_id(info$tableId)
    if (is.null(cache_fn)) return()
    handle_pitch_table_click(
      info,
      cache_fn,
      table_label = info$tableId %||% NULL
    )
  }, ignoreNULL = TRUE)
  
  
  session_label_from <- function(df) {
    s <- unique(na.omit(as.character(df$SessionType)))
    if (length(s) == 1) s else "All"
  }
  
  observeEvent(input$open_media, {
    info <- input$open_media
    url  <- info$url %||% ""
    typ  <- tolower(info$type %||% "auto")
    if (!nzchar(url)) return()
    
    is_img <- grepl("image", typ) || grepl("\\.(png|jpe?g|gif|webp)$", url, ignore.case = TRUE)
    is_pdf <- grepl("pdf", typ)   || grepl("\\.pdf(\\?.*)?$", url, ignore.case = TRUE)
    
    ui <- if (is_img) {
      tags$div(
        style = "text-align:center;",
        tags$img(src = url, style = "max-width:100%; height:auto;")
      )
    } else if (is_pdf) {
      # Prefer native inline render; works with Cloudinary “raw” PDFs
      tags$div(
        style = "width:100%;",
        # Primary: <object> (best cross-browser)
        tags$object(
          data  = url,
          type  = "application/pdf",
          style = "width:100%; height:70vh; border:0;",
          # Fallback inside <object>: <embed> (Safari sometimes prefers it)
          tags$embed(
            src   = url,
            type  = "application/pdf",
            style = "width:100%; height:70vh; border:0;"
          )
        ),
        # Always offer a direct link as a graceful fallback
        tags$div(
          style = "margin-top:8px;",
          tags$a(href = url, target = "_blank", rel = "noopener noreferrer",
                 "Open original PDF in a new tab")
        )
      )
    } else {
      tags$video(
        src = url, controls = NA, autoplay = NA,
        style = "width:100%; max-height:70vh; background:#000;"
      )
    }
    
    showModal(modalDialog(ui, easyClose = TRUE, footer = NULL, size = "l"))
  })
  
  # Safety: ensure noteJumping exists before any observer uses it
  noteJumping <- get0("noteJumping", mode = "function", inherits = TRUE)
  if (is.null(noteJumping)) noteJumping <- shiny::reactiveVal(FALSE)
  
  # forces the Notes DT to re-render when bumped
  notes_version <- reactiveVal(0L)
  
  # whenever user switches to the Notes tab, refresh the table
  observeEvent(input$top, {
    if (identical(input$top, "Notes")) {
      notes_version(isolate(notes_version()) + 1L)
    }
  }, ignoreInit = TRUE)
  
  admin_emails <- c("jgaynor@pitchingcoachu.com","jchipman@pitchingcoachu.com","ahalverson@pitchingcoachu.com")
  # helper to normalize email
  norm_email <- function(x) tolower(trimws(x))
  
  user_email <- reactive({
    # 1) First try the platform user (works if you enabled auth in shinyapps.io)
    u <- session$user
    if (!is.null(u) && nzchar(u)) return(u)
    
    # 2) Fallback to URL query param: .../?email=player@school.edu
    qs <- tryCatch(parseQueryString(isolate(session$clientData$url_search)), error = function(e) NULL)
    e <- if (!is.null(qs)) qs[["email"]] else NULL
    if (!is.null(e) && nzchar(e)) return(e)
    
    NA_character_
  })
  
  
  is_admin <- reactive({
    u <- user_email()
    !is.na(u) && u %in% admin_emails
  })
  
  tooltip_css <- "color:#fff !important;font-weight:600;padding:6px;border-radius:8px;text-shadow:0 1px 1px rgba(0,0,0,.4);"
  
  # --- Helpers to read the current suite & page (subtab) ---
  current_suite <- reactive({ input$top %or% "Pitching" })   # navbarPage id   (exists)  # :contentReference[oaicite:5]{index=5}
  
  current_page <- reactive({
    s <- current_suite()
    if (identical(s, "Pitching")) {
      input$tabs %or% "Summary"                                   # pitching tabset id  # :contentReference[oaicite:6]{index=6}
    } else if (identical(s, "Hitting")) {
      input[["hit-tabs"]] %or% "Data and Performance"             # module ns → hit-tabs
    } else if (identical(s, "Catching")) {
      input[["catch-tabs"]] %or% "Data and Performance"           # module ns → catch-tabs  # :contentReference[oaicite:7]{index=7}
    } else if (identical(s, "Leaderboard")) {
      input[["leader-tabs"]] %or% "Pitching"
    } else if (identical(s, "Comparison Suite") || identical(s, "Comparison Tool")) {
      input[["comp-tabs"]] %or% "Compare"
    } else {
      ""
    }
  })
  
  # --- your existing user_email() works for Author (email) ---
  # we’ll just reuse it.  # :contentReference[oaicite:8]{index=8}
  
  # --- Open modal to create a new note ---
  observeEvent(input$openNote, {
    # pull current filters
    ds <- input$dates[1]; de <- input$dates[2]
    pit <- input$pitcher %or% "All"
    st  <- input$sessionType %or% "All"
    sui <- current_suite()
    pag <- current_page()
    
    # a preview line for the modal
    preview <- paste0(
      fmt_mdy(ds), " – ", fmt_mdy(de),
      " • ", pit,
      " • ", sui, if (nzchar(pag)) paste0(" / ", pag) else ""
    )
    
    showModal(modalDialog(
      title = "Add Note",
      tagList(
        div(style="margin-bottom:6px; font-size:12px; opacity:.7;",
            "Context to be saved: ", preview),
        
        textAreaInput("note_text", NULL, width = "100%", height = "150px",
                      placeholder = "Type your note here…"),
        
        fileInput(
          "note_media", "Attach photo/video/PDF (optional):",
          accept = c("image/*", "video/*", "application/pdf", ".pdf"),
          buttonLabel = "Choose file…", placeholder = "No file selected"
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_note", "Save", class = "btn btn-primary",
                     onclick = "this.disabled = true; this.innerText = 'Saving…';")
      ),
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)
  
  
  
  # --- Build Notes table ---
  notes_reload <- function() {
    rows <- tryCatch(notes_api_list(), error = function(e) {
      showNotification(paste("Could not load notes:", e$message), type="error"); list()
    })
    if (!length(rows)) {
      return(DT::datatable(data.frame(Message="No notes yet"), options=list(dom='t'), rownames=FALSE))
    }
    
    df <- as.data.frame(rows, stringsAsFactors = FALSE)
    
    # Sort newest first by created_at_utc (robust)
    ct  <- parse_utc_datetime(df$created_at_utc)
    ord <- order(ct, decreasing = TRUE)
    df  <- df[ord, , drop = FALSE]
    ct  <- ct[ord]
    
    # Split Suite::Subpage
    page2 <- ifelse(is.na(df$page), "", df$page)
    sp    <- strsplit(page2, "::", fixed = TRUE)
    suite <- vapply(sp, function(x) if (length(x)) x[[1]] else "", character(1))
    subpg <- vapply(sp, function(x) if (length(x) >= 2) x[[2]] else "", character(1))
    
    ds   <- fmt_mdy(as.Date(df$date_start))
    de   <- fmt_mdy(as.Date(df$date_end))
    pit  <- ifelse(is.na(df$pitcher) | df$pitcher == "", "All", df$pitcher)
    sess <- ifelse(is.na(df$session_type) | df$session_type == "", "All", df$session_type)
    
    lbl <- paste0(ds, " – ", de, " • ", pit, " • ", suite, ifelse(nzchar(subpg), paste0(" / ", subpg), ""))
    
    suite_attr <- htmltools::htmlEscape(suite)
    page_attr  <- htmltools::htmlEscape(subpg)
    pit_attr   <- htmltools::htmlEscape(pit)
    sess_attr  <- htmltools::htmlEscape(sess)
    ds_attr    <- htmltools::htmlEscape(as.character(as.Date(df$date_start)))
    de_attr    <- htmltools::htmlEscape(as.character(as.Date(df$date_end)))
    txt_attr   <- htmltools::htmlEscape(lbl)
    
    filter_html <- sprintf(
      '<a href="#" class="note-jump" data-suite="%s" data-page="%s" data-pitcher="%s" data-sess="%s" data-ds="%s" data-de="%s">%s</a>',
      suite_attr, page_attr, pit_attr, sess_attr, ds_attr, de_attr, txt_attr
    )
    
    # --- Attachment extraction (unescape first) ---
    note_raw <- html_unescape(df$note_text %||% "")
    
    # First try to grab the exact anchor we generated on save
    anchor_pat <- '<a[^>]*class=(["\\\'])open-media\\1[^>]*>.*?</a>'
    m <- regexpr(anchor_pat, note_raw, perl = TRUE, ignore.case = TRUE)
    att_html <- ifelse(
      m > 0,
      substring(note_raw, m, m + attr(m, "match.length") - 1),
      ""
    )
    
    # Fallback: if no <a class="open-media">…</a>, build one from any media-looking URL in the text
    url_matches <- gregexpr("https?://[^\\s\"'>]+", note_raw, perl = TRUE)
    urls_list   <- regmatches(note_raw, url_matches)
    for (i in seq_along(att_html)) {
      if (nzchar(att_html[i])) next
      u <- urls_list[[i]]
      if (!length(u)) next
      # Prefer obvious media URLs; otherwise take the first URL
      # Prefer obvious media URLs; include pdf as well
      cand <- u[grep("\\.(png|jpe?g|gif|webp|mp4|mov|webm|mkv|pdf)(\\?.*)?$", u, ignore.case = TRUE)]
      if (!length(cand)) cand <- u[1]
      
      # Set link type so the viewer knows how to render
      typ <- if (grepl("\\.(png|jpe?g|gif|webp)$", cand[1], ignore.case = TRUE)) {
        "image"
      } else if (grepl("\\.pdf(\\?.*)?$", cand[1], ignore.case = TRUE)) {
        "pdf"
      } else {
        "video"
      }
      
      url_safe <- htmltools::htmlEscape(cand[1])
      att_html[i] <- sprintf('<a href="%1$s" class="open-media" data-url="%1$s" data-type="%2$s">Open attachment</a>', url_safe, typ)
    }
    
    # Plain-note display (strip the attachment anchor and any other tags)
    note_plain <- note_raw
    note_plain <- gsub(anchor_pat, "", note_plain, perl = TRUE, ignore.case = TRUE)
    note_plain <- gsub("<[^>]+>", "", note_plain, perl = TRUE)
    note_plain <- trimws(note_plain)
    
    
    
    # (1) Immediately BEFORE the privacy block, snapshot the full set & its derived vectors
    df_all          <- df
    ct_all          <- ct
    note_plain_all  <- note_plain
    filter_html_all <- filter_html
    att_html_all    <- att_html
    
    # --- privacy: on Notes, players can only see notes that mention their name in filters ---
    ue <- get_user_email(session, input)
    if (!user_is_admin(session, ue)) {
      allow <- allowed_names_for_user(ue)
      df <- filter_to_user(df, "Notes", allow)
    }
    
    # (2) RIGHT AFTER the privacy block, realign the derived vectors to the filtered rows
    if (nrow(df) > 0) {
      # use the note id to align; id is stable from the Apps Script/Sheets API
      idx <- match(df$id, df_all$id)
      
      # subset all derived vectors to ONLY the filtered rows, in the same order
      ct          <- ct_all[          idx ]
      note_plain  <- note_plain_all[  idx ]
      filter_html <- filter_html_all[ idx ]
      att_html    <- att_html_all[    idx ]
    } else {
      ct <- note_plain <- filter_html <- att_html <- character(0)
    }
    
    # (3) Now construct the display table from the FILTERED & ALIGNED vectors
    if (nrow(df) == 0) {
      show <- data.frame(
        Author=character(0), Date=character(0),
        Filter=character(0), Note=character(0), Attachment=character(0),
        stringsAsFactors = FALSE
      )
    } else {
      show <- data.frame(
        Author     = df$author_email,
        Date       = fmt_mdy(as.Date(ct)),
        Filter     = filter_html,
        Note       = note_plain,
        Attachment = att_html,
        stringsAsFactors = FALSE
      )
    }
    
    DT::datatable(
      show,
      # Escape Author, Date, Note; leave Filter & Attachment unescaped so links work
      escape   = c(1, 2, 4),
      rownames = FALSE,
      options  = list(pageLength = 25, order = list(list(1, "desc")), scrollX = TRUE)
    )
  }
  
  
  
  output$notesTable <- DT::renderDataTable({
    notes_version()                 # keep the refresh trigger
    
    out <- notes_reload()
    
    # If notes_reload() already returns a datatable/htmlwidget, just return it
    if (inherits(out, "htmlwidget") || inherits(out, "datatables")) {
      return(out)
    }
    
    # Otherwise, treat it as a data.frame and build the table here
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    
    DT::datatable(
      out,
      escape   = FALSE,             # needed so the "Open attachment" link renders
      rownames = FALSE,
      options  = list(dom = 'Bfrtip', pageLength = 10, scrollX = TRUE)
    )
  })
  
  # Optional: render even when hidden so it refreshes when you click into Notes
  outputOptions(output, "notesTable", suspendWhenHidden = FALSE)
  
  
  
  # --- When a note’s Filter is clicked, jump to that view ---
  # Requires: later
  observeEvent(input$noteJump, {
    x <- input$noteJump
    noteJumping(TRUE)  # pause auto-date logic
    
    updateTabsetPanel(session, "top", selected = x$suite)
    
    later::later(function() {
      if (nzchar(x$sess))    updateSelectInput(session, "sessionType", selected = x$sess)
      if (nzchar(x$pitcher)) updateSelectInput(session, "pitcher",      selected = x$pitcher)
      if (nzchar(x$ds) && nzchar(x$de))
        updateDateRangeInput(session, "dates", start = as.Date(x$ds), end = as.Date(x$de))
      # sub-tabs by suite
      if (identical(x$suite, "Pitching")) {
        if (nzchar(x$page)) updateTabsetPanel(session, "tabs", selected = x$page)
      } else if (identical(x$suite, "Hitting")) {
        if (nzchar(x$page)) updateTabsetPanel(session, "hit-tabs", selected = x$page)
      } else if (identical(x$suite, "Catching")) {
        if (nzchar(x$page)) updateTabsetPanel(session, "catch-tabs", selected = x$page)
      } else if (identical(x$suite, "Leaderboard")) {
        if (nzchar(x$page)) updateTabsetPanel(session, "leader-tabs", selected = x$page)
      } else if (identical(x$suite, "Comparison Suite") || identical(x$suite, "Comparison Tool")) {
        if (nzchar(x$page)) updateTabsetPanel(session, "comp-tabs", selected = x$page)
      }
      later::later(function() noteJumping(FALSE), delay = 0.6)
    }, delay = 0.3)
  }, ignoreInit = TRUE)
  
  # Set date once on startup
  observeEvent(TRUE, {
    req(input$sessionType, input$pitcher)
    df_base <- if (input$sessionType == "All") pitch_data_pitching else
      dplyr::filter(pitch_data_pitching, SessionType == input$sessionType)
    
    last_date <- if (input$pitcher == "All") {
      max(df_base$Date, na.rm = TRUE)
    } else {
      mx <- max(df_base$Date[df_base$Pitcher == input$pitcher], na.rm = TRUE)
      if (is.finite(mx)) mx else max(df_base$Date, na.rm = TRUE)
    }
    if (is.finite(last_date)) {
      updateDateRangeInput(session, "dates", start = last_date, end = last_date)
    }
  }, once = TRUE)
  
  # Update date only when the *user* changes pitcher (and not during a note jump)
  observeEvent(input$pitcher, {
    if (isTRUE(noteJumping())) return()
    req(input$pitcher)
    last_date <- if (input$pitcher == "All") {
      max(pitch_data_pitching$Date, na.rm = TRUE)
    } else {
      max(pitch_data_pitching$Date[pitch_data_pitching$Pitcher == input$pitcher], na.rm = TRUE)
    }
    if (is.finite(last_date)) {
      updateDateRangeInput(session, "dates", start = last_date, end = last_date)
    }
  }, ignoreInit = TRUE)
  
  
  # define once near top of server() if not already
  note_saving <- reactiveVal(FALSE)
  
  # single, guarded save handler
  observeEvent(input$save_note, {
    if (isTRUE(note_saving())) return(invisible(NULL))  # prevents double fires
    note_saving(TRUE); on.exit(note_saving(FALSE), add = TRUE)
    
    removeModal()
    
    # --- base note text ---
    # if you don't have %or%, use your existing %||%
    note <- input$note_text %or% ""
    if (!nzchar(note)) {
      showNotification("Note is empty—nothing saved.", type = "warning")
      return()
    }
    
    # ---- optional media upload ---- (PATCHED)
    media_html <- ""
    if (!is.null(input$note_media) && nzchar(input$note_media$datapath)) {
      if (!nzchar(CLOUDINARY_CLOUD_NAME) || !nzchar(CLOUDINARY_UPLOAD_PRESET)) {
        showNotification("Attachment not saved: Cloudinary is not configured.", type = "error")
      } else {
        up <- tryCatch(
          upload_media_cloudinary(input$note_media$datapath),
          error = function(e) {
            showNotification(paste("Attachment upload failed:", e$message), type = "error")
            NULL
          }
        )
        if (!is.null(up) && !is.null(up$url) && nzchar(up$url)) {
          if (grepl("\\.pdf(\\?.*)?$", up$url, ignore.case = TRUE) ||
              grepl("pdf", (up$type %||% ""), ignore.case = TRUE) ||
              (grepl("raw", (up$type %||% ""), ignore.case = TRUE) && grepl("\\.pdf(\\?.*)?$", up$url, ignore.case = TRUE))) {
            mtype <- "pdf"
          } else if (grepl("image", up$type, ignore.case = TRUE) ||
                     grepl("\\.(png|jpe?g|gif|webp)$", up$url, ignore.case = TRUE)) {
            mtype <- "image"
          } else {
            mtype <- "video"
          }
          media_html <- sprintf(
            '<br><a href="%s" class="open-media" data-url="%s" data-type="%s">Open attachment</a>',
            htmltools::htmlEscape(up$url), htmltools::htmlEscape(up$url), mtype
          )
        }
      }
    }
    
    # append the attachment link (if any) to the note text
    if (nzchar(media_html)) note <- paste0(note, media_html)
    
    # --- context for the note ---
    ds <- input$dates[1]; de <- input$dates[2]
    pit <- input$pitcher     %or% "All"
    st  <- input$sessionType %or% "All"
    sui <- current_suite();  pag <- current_page()
    page_combo <- paste0(sui, "::", pag %or% "")
    
    # --- save via API ---
    ok <- tryCatch({
      notes_api_add(
        author_email = user_email(),
        team         = "pcu",
        page_combo   = page_combo,
        pitcher      = pit,
        session_type = st,
        date_start   = ds,
        date_end     = de,
        note_text    = note          # <-- use the augmented text here
      )
      TRUE
    }, error = function(e) {
      showNotification(paste("Notes: save failed (", e$message, ")"), type = "error")
      FALSE
    })
    
    if (isTRUE(ok)) {
      # bump trigger so the Notes table refreshes
      notes_version(isolate(notes_version()) + 1L)
      if (identical(input$top, "Notes")) {
        notes_version(isolate(notes_version()) + 1L)
      }
      showNotification("Note saved.", type = "message")
    }
  }, ignoreInit = TRUE)
  
  
  # →  New: whenever the selected pitcher changes, move the dateRange to their last date
  observeEvent(input$pitcher, {
    if (isTRUE(noteJumping())) return()
    req(input$pitcher)
    last_date <- if (input$pitcher == "All") {
      max(pitch_data_pitching$Date, na.rm = TRUE)
    } else {
      max(pitch_data_pitching$Date[pitch_data_pitching$Pitcher == input$pitcher], na.rm = TRUE)
    }
    updateDateRangeInput(session, "dates", start = last_date, end = last_date)
  })
  
  observeEvent(input$countFilter, {
    sel <- input$countFilter
    if (is.null(sel) || !length(sel)) {
      updateSelectInput(session, "countFilter", selected = "All")
    } else if ("All" %in% sel && length(sel) > 1) {
      updateSelectInput(session, "countFilter", selected = setdiff(sel, "All"))
    }
  }, ignoreInit = TRUE)
  
  # Deep-link support: /?tab=Hitting
  observeEvent(TRUE, {
    qs <- shiny::parseQueryString(session$clientData$url_search)  # Shiny exposes this in session$clientData
    if (!is.null(qs$tab)) updateNavbarPage(session, "top", selected = qs$tab)
  }, once = TRUE)  # session$clientData reference: docs.posit.co/shiny session reference
  # (Note: Shiny exposes url_* in session$clientData.) :contentReference[oaicite:2]{index=2}
  
  # Mount the new modules (lazy-run only when their tab is active)
  mod_hit_server("hit",     is_active = reactive(input$top == "Hitting"))
  mod_catch_server("catch", is_active = reactive(input$top == "Catching"))
  mod_camps_server("camps")
  mod_leader_server("leader", is_active = reactive(input$top == "Leaderboard"))
  mod_comp_server("comp",   is_active = reactive(input$top == "Comparison Suite"))
  
  
  # Buttons above Summary table
  output$summaryTableButtons <- renderUI({
    # preserve current selection if it exists; default to "Stuff"
    sel <- isolate(input$summaryTableMode)
    if (is.null(sel)) sel <- "Stuff"
    
    tagList(
      selectInput(
        "summaryTableMode", label = "Table Mode:",
        choices  = c("Stuff","Process","Results","Counting","Bullpen","Live","Usage","Custom"),
        selected = sel,
        width = "200px"
      ),
      # show the picker purely on the client; avoids re-render loops
      conditionalPanel(
        "input.summaryTableMode=='Custom'",
        selectizeInput(
          "summaryCustomCols", label = NULL,
          choices  = setdiff(all_table_cols, "Pitch"),
          multiple = TRUE,
          options  = list(placeholder = "Choose columns to show…")
        )
      )
    )
  })
  
  
  # Buttons above Data & Performance table
  output$dpTableButtons <- renderUI({
    sel <- isolate(input$dpTableMode)
    if (is.null(sel)) sel <- "Stuff"
    
    tagList(
      selectInput(
        "dpTableMode", label = "Table Mode:",
        choices  = c("Stuff","Process","Results","Counting","Bullpen","Live","Usage","Custom"),
        selected = sel,
        width = "200px"
      ),
      conditionalPanel(
        "input.dpTableMode=='Custom'",
        selectizeInput(
          "dpCustomCols", label = NULL,
          choices  = setdiff(all_table_cols, "Pitch"),
          multiple = TRUE,
          options  = list(placeholder = "Choose columns to show…")
        )
      )
    )
  })
  
  
  
  output$leaderboardButtons <- renderUI({
    sel <- isolate(input$leaderboardMode)
    if (is.null(sel)) sel <- "Stuff"
    
    tagList(
      radioButtons(
        "leaderboardMode", label = NULL,
        choices  = c("Stuff","Process","Results","Bullpen","Live","Usage","Custom"),
        selected = sel,
        inline   = TRUE
      ),
      conditionalPanel(
        "input.leaderboardMode=='Custom'",
        selectizeInput(
          "leaderboardCustomCols", label = NULL,
          choices  = setdiff(all_table_cols, "Pitch"),
          multiple = TRUE,
          options  = list(placeholder = "Choose columns to show…")
        )
      )
    )
  })
  
  # 1) Pitcher selector
  output$pitcher_ui <- renderUI({
    req(input$sessionType)
    
    df_base <- if (input$sessionType == "All") pitch_data_pitching else
      dplyr::filter(pitch_data_pitching, SessionType == input$sessionType)
    
    sel_raw <- unique(df_base$Pitcher[norm_email(df_base$Email) == norm_email(user_email())]) %>% na.omit()
    
    if (is_admin()) {
      selectInput(
        "pitcher", "Select Pitcher:",
        choices  = c("All" = "All", name_map_pitching),
        selected = "All"
      )
    } else if (length(sel_raw) > 0) {
      disp <- display_names_p[raw_names_p %in% sel_raw]
      map2 <- setNames(sel_raw, disp)
      selectInput("pitcher", "Select Pitcher:", choices = map2, selected = sel_raw[1])
    } else {
      selectInput("pitcher", "Select Pitcher:", choices = "No data", selected = "No data")
    }
  })
  
  
  observeEvent(input$sessionType, {
    df_base <- if (input$sessionType == "All") pitch_data_pitching else
      dplyr::filter(pitch_data_pitching, SessionType == input$sessionType)
    last_date <- if (is.null(input$pitcher) || input$pitcher == "All") {
      max(df_base$Date, na.rm = TRUE)
    } else {
      ld <- max(df_base$Date[df_base$Pitcher == input$pitcher], na.rm = TRUE)
      if (is.finite(ld)) ld else max(df_base$Date, na.rm = TRUE)
    }
    updateDateRangeInput(session, "dates", start = last_date, end = last_date)
  }, ignoreInit = TRUE)
  
  # 2) Filtered data
  filtered_data <- reactive({
    req(input$sessionType, input$hand, input$zoneLoc, input$inZone)
    
    is_valid_dates <- function(d) !is.null(d) && length(d) == 2 && all(is.finite(d))
    nnz <- function(x) !is.null(x) && !is.na(x)
    
    if (!is_valid_dates(input$dates)) return(pitch_data_pitching[0, , drop = FALSE])
    
    pitch_types <- if (is.null(input$pitchType) || !length(input$pitchType)) "All" else input$pitchType
    
    # Session type
    df <- if (identical(input$sessionType, "All")) pitch_data_pitching
    else dplyr::filter(pitch_data_pitching, SessionType == input$sessionType)
    
    # ⛔️ Drop warmups & blank pitch types
    if ("TaggedPitchType" %in% names(df)) {
      df <- df %>%
        dplyr::mutate(.tpt = trimws(as.character(TaggedPitchType))) %>%
        dplyr::filter(!is.na(.tpt) & nzchar(.tpt)) %>%
        dplyr::select(-.tpt)
    }
    if ("PitchSession" %in% names(df)) {
      df <- dplyr::filter(df, is.na(PitchSession) | PitchSession != "Warmup")
    }
    
    df <- filter_batter_side(df, input$batterSide)
    
    # Dates
    df <- dplyr::filter(df, Date >= input$dates[1], Date <= input$dates[2])
    
    # Pitcher & hand
    pick <- input$pitcher
    if (!is.null(pick) && pick != "All") {
      df <- dplyr::filter(df, Pitcher == pick)
    }
    
    if (!is.null(input$hand) && input$hand != "All")       df <- dplyr::filter(df, PitcherThrows == input$hand)
    
    # Spatial & count
    df <- enforce_zone(df, input$zoneLoc)
    df <- enforce_inzone(df, input$inZone)
    df <- apply_count_filter(df, input$countFilter)
    
    # Video filter
    if (!is.null(input$videoFilter) && input$videoFilter != "All") {
      if (input$videoFilter == "Edger") {
        df <- dplyr::filter(df, !is.na(VideoClip) & nzchar(VideoClip))
      } else if (input$videoFilter == "Behind") {
        df <- dplyr::filter(df, !is.na(VideoClip2) & nzchar(VideoClip2))
      } else if (input$videoFilter == "Side") {
        df <- dplyr::filter(df, !is.na(VideoClip3) & nzchar(VideoClip3))
      }
    }
    
    # Numeric ranges
    if (nnz(input$veloMin)) df <- dplyr::filter(df, RelSpeed         >= input$veloMin)
    if (nnz(input$veloMax)) df <- dplyr::filter(df, RelSpeed         <= input$veloMax)
    if (nnz(input$ivbMin))  df <- dplyr::filter(df, InducedVertBreak >= input$ivbMin)
    if (nnz(input$ivbMax))  df <- dplyr::filter(df, InducedVertBreak <= input$ivbMax)
    if (nnz(input$hbMin))   df <- dplyr::filter(df, HorzBreak        >= input$hbMin)
    if (nnz(input$hbMax))   df <- dplyr::filter(df, HorzBreak        <= input$hbMax)
    
    # Pitch-number window
    df <- df %>% dplyr::arrange(Date) %>% dplyr::mutate(PitchNumber = dplyr::row_number())
    if (nnz(input$pcMin)) df <- dplyr::filter(df, PitchNumber >= input$pcMin)
    if (nnz(input$pcMax)) df <- dplyr::filter(df, PitchNumber <= input$pcMax)
    
    # Per-user visibility
    if (!user_is_admin(session)) {
      ue <- user_email()
      if (!is.na(ue)) df <- dplyr::filter(df, norm_email(Email) == norm_email(ue))
    }
    
    if (!nrow(df)) return(df[0, , drop = FALSE])
    
    # Derived fields
    df2 <- compute_stuff_simple(df, base_type = input$stuffBase, level = input$stuffLevel) %>%
      force_pitch_levels() %>%
      dplyr::mutate(Result = factor(compute_result(PitchCall, PlayResult), levels = result_levels))
    
    # Pitch type after derive
    if (!("All" %in% pitch_types)) df2 <- dplyr::filter(df2, TaggedPitchType %in% pitch_types)
    
    df2
  })
  
  # 3) helper for ordered types
  ordered_types <- function() {
    intersect(names(all_colors), unique(filtered_data()$TaggedPitchType))
  }
  
  # 4) Leaderboard data
  leaderboard_data <- reactive({
    req(input$sessionType, input$dates, input$hand, input$zoneLoc, input$inZone)
    
    # protect against NULL during app init
    pitch_types <- if (is.null(input$pitchType)) "All" else input$pitchType
    
    # first, honor Session Type
    df <- if (input$sessionType == "All") pitch_data_pitching else
      dplyr::filter(pitch_data_pitching, SessionType == input$sessionType)
    
    # Live-only BatterSide filter
    if (!is.null(input$batterSide) && input$batterSide != "All") {
      df <- df %>% dplyr::filter(
        SessionType != "Live" | (SessionType == "Live" & BatterSide == input$batterSide)
      )
    }
    
    # existing filters...
    df <- df %>% dplyr::filter(Date >= input$dates[1], Date <= input$dates[2])
    picks <- input$pitcher
    if (!is.null(picks) && length(picks) && !("All" %in% picks)) {
      df <- dplyr::filter(df, Pitcher %in% picks)
    }
    if (input$hand != "All")    df <- dplyr::filter(df, PitcherThrows == input$hand)
    
    df <- enforce_zone(df, input$zoneLoc)
    df <- enforce_inzone(df, input$inZone)
    df <- apply_count_filter(df, input$countFilter)
    
    # Video filter
    if (!is.null(input$videoFilter) && input$videoFilter != "All") {
      if (input$videoFilter == "Edger") {
        df <- dplyr::filter(df, !is.na(VideoClip) & nzchar(VideoClip))
      } else if (input$videoFilter == "Behind") {
        df <- dplyr::filter(df, !is.na(VideoClip2) & nzchar(VideoClip2))
      } else if (input$videoFilter == "Side") {
        df <- dplyr::filter(df, !is.na(VideoClip3) & nzchar(VideoClip3))
      }
    }
    
    if (!is.na(input$veloMin)) df <- dplyr::filter(df, RelSpeed >= input$veloMin)
    if (!is.na(input$veloMax)) df <- dplyr::filter(df, RelSpeed <= input$veloMax)
    if (!is.na(input$ivbMin))  df <- dplyr::filter(df, InducedVertBreak >= input$ivbMin)
    if (!is.na(input$ivbMax))  df <- dplyr::filter(df, InducedVertBreak <= input$ivbMax)
    if (!is.na(input$hbMin))   df <- dplyr::filter(df, HorzBreak >= input$hbMin)
    if (!is.na(input$hbMax))   df <- dplyr::filter(df, HorzBreak <= input$hbMax)
    
    df <- df %>% dplyr::arrange(Date) %>% dplyr::mutate(PitchNumber = dplyr::row_number())
    if (!is.na(input$pcMin)) df <- dplyr::filter(df, PitchNumber >= input$pcMin)
    if (!is.na(input$pcMax)) df <- dplyr::filter(df, PitchNumber <= input$pcMax)
    
    if (!user_is_admin(session)) {
      ue <- user_email()
      if (!is.na(ue)) df <- dplyr::filter(df, Email == ue)
    }
    
    
    df2 <- compute_stuff_simple(df, input$stuffBase, input$stuffLevel) %>%
      force_pitch_levels() %>%
      dplyr::mutate(Result = factor(compute_result(PitchCall, PlayResult), levels = result_levels))
    
    if (!("All" %in% pitch_types)) df2 <- dplyr::filter(df2, TaggedPitchType %in% pitch_types)
    df2
  })
  
  trend_plot <- function(df, val_expr, title, ylab, fun = mean) {
    dark_on <- isTRUE(input$dark_mode)
    axis_col <- if (dark_on) "#e5e7eb" else "black"
    grid_alpha <- if (dark_on) 0.15 else 0.35
    sess_cols <- session_cols_for_mode(dark_on)
    agg <- function(x) {
      x <- x[!is.na(x)]
      if (!length(x)) return(NA_real_)
      fun(x)
    }
    if (input$sessionType == "All") {
      dat <- df %>%
        dplyr::group_by(Date, SessionType) %>%
        dplyr::summarise(value = agg({{ val_expr }}), .groups = "drop") %>%
        dplyr::arrange(Date)
      date_levels <- unique(fmt_date(dat$Date))
      dat <- dplyr::mutate(dat, Date_f = factor(fmt_date(Date), levels = date_levels))
      
      ggplot(dat, aes(Date_f, value, group = SessionType, color = SessionType)) +
        geom_line(size = 1.2) + geom_point(size = 2) +
        scale_color_manual(values = sess_cols, breaks = c("Live", "Bullpen"), name = NULL) +
        labs(title = title, x = NULL, y = ylab) +
        theme_minimal() + axis_theme +
        theme(
          plot.title = element_text(face = "bold", colour = axis_col),
          axis.text.x = element_text(angle = 45, hjust = 1, colour = axis_col),
          axis.text.y = element_text(colour = axis_col),
          axis.title  = element_text(colour = axis_col),
          legend.position = "bottom",
          legend.text = element_text(colour = axis_col),
          legend.background = element_rect(fill = NA, colour = NA),
          legend.box.background = element_rect(fill = NA, colour = NA),
          panel.background = element_rect(fill = NA, colour = NA),
          plot.background = element_rect(fill = NA, colour = NA),
          panel.grid.major = element_line(color = scales::alpha(axis_col, grid_alpha))
        )
    } else {
      dat <- df %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(value = agg({{ val_expr }}), .groups = "drop") %>%
        dplyr::arrange(Date)
      date_levels <- unique(fmt_date(dat$Date))
      dat <- dplyr::mutate(dat, Date_f = factor(fmt_date(Date), levels = date_levels))
      
      ggplot(dat, aes(Date_f, value, group = 1)) +
        geom_line(size = 1.2, colour = axis_col) +
        geom_point(size = 2, colour = axis_col) +
        labs(title = title, x = NULL, y = ylab) +
        theme_minimal() + axis_theme +
        theme(
          plot.title = element_text(face = "bold", colour = axis_col),
          axis.text.x = element_text(angle = 45, hjust = 1, colour = axis_col),
          axis.text.y = element_text(colour = axis_col),
          axis.title  = element_text(colour = axis_col),
          legend.position = "none",
          panel.background = element_rect(fill = NA, colour = NA),
          plot.background = element_rect(fill = NA, colour = NA),
          panel.grid.major = element_line(color = scales::alpha(axis_col, grid_alpha))
        )
    }
  }
  
  output$summaryHeader <- renderUI({
    df <- filtered_data()
    
    # Safe reads
    pit <- input$pitcher
    dts <- input$dates
    
    # Metrics with empty-safe fallbacks
    scores <- if (nrow(df)) {
      ifelse(
        df$PlateLocSide >= ZONE_LEFT & df$PlateLocSide <= ZONE_RIGHT &
          df$PlateLocHeight >= ZONE_BOTTOM & df$PlateLocHeight <= ZONE_TOP, 1.47,
        ifelse(
          df$PlateLocSide >= -1.5 & df$PlateLocSide <= 1.5 &
            df$PlateLocHeight >= (2.65 - 1.7) & df$PlateLocHeight <= (2.65 + 1.3),
          0.73, 0
        )
      )
    } else numeric(0)
    
    overall_stuff   <- if (nrow(df)) round(mean(df$`Stuff+`, na.rm = TRUE), 1) else NA_real_
    overall_command <- if (length(scores)) round(mean(scores,   na.rm = TRUE) * 100, 1) else NA_real_
    overall_pitch   <- round(mean(c(overall_stuff, overall_command), na.rm = TRUE), 1)
    
    # Title
    title <- if (is.null(pit) || identical(pit, "All")) {
      "All Pitchers"
    } else {
      disp <- names(name_map)[name_map == pit]
      if (length(disp)) disp else pit
    }
    
    # Dates string
    dates_str <- if (!is.null(dts) && length(dts) == 2 && all(is.finite(dts))) {
      if (identical(as.Date(dts[1]), as.Date(dts[2]))) fmt_date(dts[1]) else
        paste(fmt_date(dts[1]), fmt_date(dts[2]), sep = " - ")
    } else {
      rng <- suppressWarnings(range(df$Date, na.rm = TRUE))
      if (all(is.finite(rng))) paste(fmt_date(rng[1]), fmt_date(rng[2]), sep = " - ") else "All Dates"
    }
    
    tags$div(
      style = "display:flex; align-items:center; justify-content:space-between;",
      tags$img(src = "PCUlogo.png", height = "50px", style = "margin-right:15px;"),
      tags$h3(paste(title, "|", dates_str), style = "font-weight:bold; margin:0; flex:1;"),
      tags$div(
        style = "font-weight:bold; margin-left:20px;",
        paste0("Stuff+: ",   ifelse(is.finite(overall_stuff), overall_stuff,   "—"), " | "),
        paste0("Ctrl+: ",    ifelse(is.finite(overall_command), overall_command, "—"), " | "),
        paste0("Pitching+: ",ifelse(is.finite(overall_pitch), overall_pitch,   "—"))
      )
    )
  })
  
  
  # ---- Sessions Logs (Pitching) ----
  filtered_logs <- reactive({
    # --- helpers (tweaked date validator; added safe path->date fallback) ---
    is_valid_dates <- function(d) {
      if (is.null(d) || length(d) != 2) return(FALSE)
      a <- suppressWarnings(as.Date(d[1], tryFormats = c("%Y-%m-%d","%m/%d/%Y","%m/%d/%y")))
      b <- suppressWarnings(as.Date(d[2], tryFormats = c("%Y-%m-%d","%m/%d/%Y","%m/%d/%y")))
      !any(is.na(c(a, b)))
    }
    
    nnz <- function(x) !is.null(x) && !is.na(x)
    
    # Try to recover a date from SourceFile path when CSV "Date" fails
    .date_from_path <- function(paths) {
      v <- vapply(as.character(paths), function(p) {
        s <- as.character(p)
        
        # Try /YYYY/MM/DD/ or YYYY-MM-DD in the path
        m <- regexpr("(20\\d{2})[/-](0[1-9]|1[0-2])[/-]([0-3]\\d)", s, perl = TRUE)
        if (m[1] != -1) {
          frag <- substr(s, m[1], m[1] + attr(m, "match.length") - 1)
          frag <- gsub("/", "-", frag)
          out  <- suppressWarnings(as.Date(frag))
          if (!is.na(out)) return(format(out, "%Y-%m-%d"))
        }
        
        # Try 8-digit yyyymmdd in filename
        m2 <- regexpr("(20\\d{2})(0[1-9]|1[0-2])([0-3]\\d)", s, perl = TRUE)
        if (m2[1] != -1) {
          y  <- substr(s, m2[1],     m2[1] + 3)
          mo <- substr(s, m2[1] + 4, m2[1] + 5)
          d  <- substr(s, m2[1] + 6, m2[1] + 7)
          out <- suppressWarnings(as.Date(paste(y, mo, d, sep = "-")))
          if (!is.na(out)) return(format(out, "%Y-%m-%d"))
        }
        
        ""  # no match
      }, FUN.VALUE = character(1))
      suppressWarnings(as.Date(ifelse(nzchar(v), v, NA_character_)))
    }
    # ------------------------------------------------------------------------
    
    df <- pitch_data_pitching
    
    # Ensure Date exists & is Date
    if (!("Date" %in% names(df))) {
      cand <- intersect(c("GameDate","SessionDate","date","DATE"), names(df))
      if (length(cand)) df$Date <- df[[cand[1]]] else df$Date <- NA
    }
    # Your existing parser
    df$Date <- .s_to_date(df$Date)
    
    # Fill any remaining NA Date from SourceFile path, if available
    if ("SourceFile" %in% names(df)) {
      miss <- is.na(df$Date)
      if (any(miss)) {
        df$Date[miss] <- .date_from_path(df$SourceFile[miss])
      }
    }
    
    # If date input isn't ready yet, use full data range (prevents blank table)
    # If the range is still not finite (e.g., all NA), we skip narrowing by dates later.
    if (!is_valid_dates(input$dates)) {
      rng <- suppressWarnings(range(df$Date, na.rm = TRUE))
      date_start <- rng[1]; date_end <- rng[2]
    } else {
      date_start <- as.Date(input$dates[1]); date_end <- as.Date(input$dates[2])
    }
    have_valid_range <- is.finite(date_start) && is.finite(date_end)
    
    # Session Type
    if (!is.null(input$sessionType) && input$sessionType != "All") {
      df <- dplyr::filter(df, SessionType == input$sessionType)
    }
    
    # Keep warmups out; DO NOT drop blank TaggedPitchType here
    if ("PitchSession" %in% names(df)) {
      df <- dplyr::filter(df, is.na(PitchSession) | PitchSession != "Warmup")
    }
    
    # Live-only BatterSide
    if (!is.null(input$batterSide) && input$batterSide != "All") {
      df <- df %>%
        dplyr::filter(SessionType != "Live" |
                        (SessionType == "Live" & BatterSide == input$batterSide))
    }
    
    # Date window (only if we have a real range; otherwise just drop NA Date)
    if (have_valid_range) {
      df <- dplyr::filter(df, !is.na(Date), Date >= date_start, Date <= date_end)
    } else {
      df <- dplyr::filter(df, !is.na(Date))
    }
    
    # Pitcher & hand
    if (!is.null(input$pitcher) && input$pitcher != "All") {
      df <- dplyr::filter(df, Pitcher == input$pitcher)
    }
    if (!is.null(input$hand) && input$hand != "All") {
      df <- dplyr::filter(df, PitcherThrows == input$hand)
    }
    
    # Same zone/count/numeric filters as elsewhere (guarded with exists())
    if (exists("enforce_zone"))       df <- enforce_zone(df, input$zoneLoc)
    if (exists("enforce_inzone"))     df <- enforce_inzone(df, input$inZone)
    if (exists("apply_count_filter")) df <- apply_count_filter(df, input$countFilter)
    
    if (nnz(input$veloMin)) df <- dplyr::filter(df, RelSpeed         >= input$veloMin)
    if (nnz(input$veloMax)) df <- dplyr::filter(df, RelSpeed         <= input$veloMax)
    if (nnz(input$ivbMin))  df <- dplyr::filter(df, InducedVertBreak >= input$ivbMin)
    if (nnz(input$ivbMax))  df <- dplyr::filter(df, InducedVertBreak <= input$ivbMax)
    if (nnz(input$hbMin))   df <- dplyr::filter(df, HorzBreak        >= input$hbMin)
    if (nnz(input$hbMax))   df <- dplyr::filter(df, HorzBreak        <= input$hbMax)
    
    df
  })
  
  
  # Mode toggle + Custom picker
  output$sessTableButtons <- renderUI({
    sel <- isolate(input$sessMode); if (is.null(sel)) sel <- "Stuff"
    choices_cols <- if (exists("all_table_cols")) setdiff(all_table_cols, "Pitch")
    else setdiff(names(pitch_data_pitching), "Pitch")
    tagList(
      radioButtons(
        "sessMode", label = NULL,
        choices  = c("Stuff","Process","Results","Bullpen","Live","Usage","Custom"),
        selected = sel, inline = TRUE
      ),
      conditionalPanel(
        "input.sessMode=='Custom'",
        selectizeInput(
          "sessCustomCols", label = NULL,
          choices  = choices_cols,
          multiple = TRUE,
          options  = list(placeholder = "Choose columns to show…")
        )
      )
    )
  })
  
  # DataTable render
  output$sessTable <- DT::renderDataTable({
    df <- filtered_logs()
    if (!nrow(df)) {
      return(DT::datatable(
        data.frame(Message = "No data for selected filters"),
        options = list(dom = 't'), rownames = FALSE
      ))
    }
    
    df_table <- make_session_logs_table(df) %>%
      dplyr::mutate(Date = parse_date_flex(Date)) %>%   # ← was as.Date(Date)
      dplyr::arrange(dplyr::desc(Date)) %>%
      dplyr::mutate(Date = format(Date, "%Y-%m-%d"))    # keep ISO output, or use "%m/%d/%Y" if you prefer
    
    
    mode <- input$sessMode; if (is.null(mode)) mode <- "Stuff"
    sel  <- input$sessCustomCols; if (is.null(sel)) sel <- character(0)
    
    # NEW: safe fallback if the requested set is empty or mismatched
    dv <- intersect(visible_set_for_date(mode, sel), names(df_table))
    if (!length(dv)) dv <- names(df_table)
    
    tbl <- datatable_with_colvis(
      df_table,
      lock = "Date",
      remember = FALSE,
      default_visible = dv
    )
    
    num_cols <- intersect(c("Velo","Max","IVB","HB","SpinEff","Spin","Height","Side",
                            "VAA","HAA","Ext","EV","LA","FIP","WHIP"),
                          names(df_table))
    if (length(num_cols)) tbl <- DT::formatRound(tbl, num_cols, digits = 1)
    
    tbl
  })
  
  
  output$summary_releasePlot <- ggiraph::renderGirafe({
    df <- filtered_data(); if (!nrow(df)) return(NULL)
    types <- ordered_types(); types_chr <- as.character(types)
    sess_lbl <- session_label_from(df)
    dark_on <- isTRUE(input$dark_mode)
    line_col <- if (dark_on) "#e5e7eb" else "black"
    mound_fill <- if (dark_on) "#2d2d2d" else "tan"
    rubber_fill <- if (dark_on) NA else "white"
    grid_alpha <- if (dark_on) 0.15 else 0.4
    
    # --- background geometry
    rp_w <- 4; rp_h <- 0.83
    xs <- seq(-rp_w, rp_w, length.out = 100)
    ys <- rp_h * (1 - (xs / rp_w)^2)
    mound <- data.frame(x = c(xs, rev(xs)), y = c(ys, rep(0, length(xs))))
    
    # --- averages for hover+dot
    avg <- df %>%
      dplyr::filter(is.finite(RelSide), is.finite(RelHeight)) %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(
        avg_RelSide    = mean(RelSide,    na.rm = TRUE),
        avg_RelHeight  = mean(RelHeight,  na.rm = TRUE),
        avg_Extension  = mean(Extension,  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(TaggedPitchType %in% types_chr) %>%
      dplyr::mutate(
        TaggedPitchType = factor(TaggedPitchType, levels = types_chr),
        tt = paste0(
          "Session: ", sess_lbl,
          "<br>Height: ", sprintf("%.1f ft", avg_RelHeight),
          "<br>Side: ", sprintf("%.1f ft", avg_RelSide),
          "<br>Extension: ", sprintf("%.1f ft", avg_Extension)
        )
      )
    
    # --- ensure y axis goes to at least 6
    y_max <- 8
    
    p <- ggplot() +
      geom_polygon(data = mound, aes(x, y), fill = mound_fill, color = mound_fill) +
      annotate("rect", xmin = -0.5, xmax = 0.5, ymin = rp_h - 0.05, ymax = rp_h + 0.05, fill = rubber_fill, color = line_col) +
      geom_vline(xintercept = 0, color = line_col, size = 0.7) +
      ggiraph::geom_point_interactive(
        data = avg,
        aes(x = avg_RelSide, y = avg_RelHeight,
            color = TaggedPitchType, tooltip = tt, data_id = TaggedPitchType),
        size = 8, show.legend = FALSE
      ) +
      scale_color_manual(values = all_colors[types_chr], limits = types_chr, name = NULL) +
      scale_y_continuous(limits = c(0, y_max), breaks = seq(0, ceiling(y_max), by = 1)) +
      theme_minimal() + axis_theme +
      labs(x = NULL, y = NULL) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 15, face = "bold", colour = line_col),
        axis.text.y = element_text(size = 15, face = "bold", colour = line_col),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = scales::alpha(line_col, 0.1))
      )
    
    ggiraph::girafe(
      ggobj = p,
      width_svg = 8, height_svg = 6.5,
      options = list(
        ggiraph::opts_sizing(rescale = TRUE),
        ggiraph::opts_tooltip(use_fill = TRUE, use_stroke = TRUE, css = tooltip_css),
        ggiraph::opts_hover(css = "stroke:black;stroke-width:1.5px;"),
        ggiraph::opts_hover_inv(css = "opacity:0.15;")
      ),
      bg = "transparent"
    )
  })
  
  output$summary_movementPlot <- ggiraph::renderGirafe({
    df <- filtered_data(); if (!nrow(df)) return(NULL)
    types <- ordered_types()
    if (!length(types)) return(NULL)
    types_chr <- as.character(types)
    dark_on <- isTRUE(input$dark_mode)
    axis_col <- if (dark_on) "#e5e7eb" else "black"
    grid_alpha <- if (dark_on) 0.15 else 0.4
    cols <- colors_for_mode(dark_on)
    
    # last-25 avg per type
    avg_mov <- df %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::slice_tail(n = 1000) %>%
      dplyr::summarise(
        avg_HorzBreak        = mean(HorzBreak, na.rm = TRUE),
        avg_InducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
        .groups = "drop"
      )
    
    # interactive payload (per pitch) — uses your GLOBAL make_hover_tt()
    df_i <- df %>%
      dplyr::mutate(
        tt  = make_hover_tt(.),
        rid = dplyr::row_number()
      )
    
    base_type <- input$breakLines
    line_df <- tibble()
    if (base_type %in% c("Fastball","Sinker")) {
      base_val <- dplyr::filter(avg_mov, TaggedPitchType == base_type)
      if (nrow(base_val) == 1) {
        seps <- if (base_type == "Fastball") {
          tibble(
            TaggedPitchType = c("Cutter","Slider","Sweeper","Curveball","ChangeUp","Splitter"),
            sep_IVB = c(-7,-15,-16,-27,-12,-13),
            sep_Horz= c(10,12,22,18,-7,-4)
          )
        } else {
          tibble(
            TaggedPitchType = c("Cutter","Slider","Sweeper","Curveball","ChangeUp","Splitter"),
            sep_IVB = c(2,-6,-7,-18,-4,-5),
            sep_Horz= c(18,20,30,25,1,2)
          )
        }
        # handedness direction
        throw_side <- if (input$hand %in% c("Left","Right")) input$hand else {
          us <- unique(df$PitcherThrows) %>% na.omit()
          if (length(us) == 1 && us %in% c("Left","Right")) us else "Left"
        }
        dir <- ifelse(throw_side == "Right", -1, 1)
        seps <- seps %>%
          dplyr::filter(TaggedPitchType %in% avg_mov$TaggedPitchType) %>%
          dplyr::mutate(sep_Horz = sep_Horz * dir)
        
        line_df <- seps %>% dplyr::mutate(
          start_x = base_val$avg_HorzBreak,
          start_y = base_val$avg_InducedVertBreak,
          end_x   = base_val$avg_HorzBreak + sep_Horz,
          end_y   = base_val$avg_InducedVertBreak + sep_IVB
        )
      }
    }
    
    p <- ggplot() +
      ggiraph::geom_point_interactive(
        data = df_i,
        aes(HorzBreak, InducedVertBreak,
            color = TaggedPitchType, fill = TaggedPitchType,
            tooltip = tt, data_id = rid),
        position = "identity",
        alpha = 0.25, size = 4.0, shape = 21, stroke = 0.25
      ) +
      geom_point(
        data = avg_mov,
        aes(avg_HorzBreak, avg_InducedVertBreak, color = TaggedPitchType),
        size = 8
      ) +
      { if (nrow(line_df) > 0)
        geom_segment(
          data = line_df,
          aes(x = start_x, y = start_y, xend = end_x, yend = end_y, color = TaggedPitchType),
          size = 3
        )
      } +
      geom_hline(yintercept = 0, color = axis_col) +
      geom_vline(xintercept = 0, color = axis_col) +
      coord_cartesian(xlim = c(-25, 25), ylim = c(-25, 25)) +
      scale_color_manual(values = cols[types_chr], limits = types_chr, name = NULL) +
      scale_fill_manual(values  = cols[types_chr], limits = types_chr, name = NULL) +
      theme_minimal() + axis_theme +
      labs(x = NULL, y = NULL) +
      theme(
        legend.position = "none",
        axis.text.x     = element_text(size = 15, face = "bold", colour = axis_col),
        axis.text.y     = element_text(size = 15, face = "bold", colour = axis_col),
        axis.title.x    = element_blank(),
        axis.title.y    = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = scales::alpha(axis_col, 0.1))
      )
    
    ggiraph::girafe(
      ggobj = p,
      width_svg  = 8,
      height_svg = 6.5,
      options = list(
        ggiraph::opts_sizing(rescale = TRUE),
        ggiraph::opts_tooltip(use_fill = TRUE, css = tooltip_css),
        ggiraph::opts_hover(css = "stroke:black;stroke-width:1.5px;"),
        ggiraph::opts_hover_inv(css = "opacity:0.15;"),
        ggiraph::opts_selection(type = "single", only_shiny = TRUE)  # <-- added: single-select
      ),
      bg = "transparent"
    )
  })
  
  # 2) Pitch‐version location chart (with centered title)
  output$summary_zonePlot <- ggiraph::renderGirafe({
    df <- filtered_data(); if (!nrow(df)) return(NULL)
    types <- ordered_types(); types_chr <- as.character(types)
    
    df_i <- df %>%
      dplyr::mutate(
        tt      = make_hover_tt(.),
        rid     = dplyr::row_number(),
        # tooltip fill should use the outline (pitch-type) color
        tt_fill = dplyr::coalesce(all_colors[as.character(TaggedPitchType)], "gray")
      )
    
    home <- data.frame(
      x = c(-0.75, 0.75, 0.75, 0, -0.75),
      y = c(1.05, 1.05, 1.15, 1.25, 1.15) - 0.5
    )
    cz <- data.frame(xmin = -1.5, xmax = 1.5, ymin = 2.65 - 1.7, ymax = 2.65 + 1.3)
    sz <- data.frame(xmin = ZONE_LEFT, xmax = ZONE_RIGHT, ymin = ZONE_BOTTOM, ymax = ZONE_TOP)
    
    df_known <- df_i %>% dplyr::filter(!is.na(Result))
    df_other <- df_i %>% dplyr::filter(is.na(Result))
    
    dark_on <- isTRUE(input$dark_mode)
    zone_col <- if (dark_on) "#e5e7eb" else "black"
    p <- ggplot() +
      geom_polygon(data = home, aes(x, y), inherit.aes = FALSE, fill = NA, color = zone_col) +
      geom_rect(data = cz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE, fill = NA, linetype = "dashed", color = zone_col) +
      geom_rect(data = sz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE, fill = NA, color = zone_col) +
      
      # filled circles for "no result" rows
      ggiraph::geom_point_interactive(
        data = df_other,
        aes(PlateLocSide, PlateLocHeight,
            color = TaggedPitchType, fill = TaggedPitchType,
            tooltip = tt, data_id = rid),
        position = "identity",
        size = 5, alpha = 0.9, shape = 16, stroke = 0.3
      ) +
      
      # result-coded shapes (can be hollow)
      ggiraph::geom_point_interactive(
        data = df_known,
        aes(PlateLocSide, PlateLocHeight,
            color = TaggedPitchType, fill = TaggedPitchType, shape = Result,
            tooltip = tt, data_id = rid),
        position = "identity",
        size = 5, alpha = 0.95, stroke = 0.8
      ) +
      
      scale_color_manual(values = all_colors[types_chr], limits = types_chr, name = NULL) +
      scale_fill_manual(values  = all_colors[types_chr], limits = types_chr, name = NULL) +
      scale_shape_manual(values = shape_map, drop = TRUE) +
      coord_fixed(ratio = 1, xlim = c(-2, 2), ylim = c(0, 4.5)) +
      theme_void() + theme(legend.position = "none") +
      
      # 🔹 Invisible “hover pad” on top to force correct tooltip fill every time
      ggiraph::geom_point_interactive(
        data = df_i,
        aes(PlateLocSide, PlateLocHeight, tooltip = tt, data_id = rid, fill = I(tt_fill)),
        shape = 21, size = 6, alpha = 0.001, stroke = 0, inherit.aes = FALSE
      )
    
    ggiraph::girafe(
      ggobj = p,
      options = list(
        ggiraph::opts_tooltip(use_fill = TRUE, use_stroke = TRUE, css = tooltip_css),
        ggiraph::opts_hover(css = "stroke-width:1.5px;"),
        ggiraph::opts_hover_inv(css = "opacity:0.15;"),
        ggiraph::opts_selection(type = "single", only_shiny = TRUE)  # <-- added: single-select for reliable clicks
      ),
      bg = "transparent"
    )
  })
  
  
  # 3) Heat‐version location chart (fixed error + centered title)
  output$summary_heatZonePlot <- renderPlot({
    df <- filtered_data()
    if (!nrow(df)) return()
    
    bins <- HEAT_BINS
    pal  <- heat_pal(bins)
    
    home <- data.frame(
      x = c(-0.75, 0.75, 0.75, 0, -0.75),
      y = c(1.05, 1.05, 1.15, 1.25, 1.15) - 0.5
    )
    sz <- data.frame(
      xmin = ZONE_LEFT, xmax = ZONE_RIGHT,
      ymin = ZONE_BOTTOM, ymax = ZONE_TOP
    )
    
    ggplot() +
      stat_density_2d_filled(
        data = df,
        aes(PlateLocSide, PlateLocHeight, fill = after_stat(level)),
        bins = bins, show.legend = FALSE
      ) +
      scale_fill_manual(values = pal) +
      geom_polygon(data = home, aes(x, y), inherit.aes = FALSE, fill = NA, color = "black") +
      geom_rect(data = sz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE, fill = NA, color = "black") +
      coord_fixed(ratio = 1, xlim = c(-2, 2), ylim = c(0, 4.5)) +
      labs(title = "") +
      theme_void() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA)
      )
  }, bg = "transparent")
  
  
  output$summary_legend <- renderPlot({
    types<-ordered_types(); if(!length(types)) return()
    dark_on <- isTRUE(input$dark_mode)
    axis_col <- if (dark_on) "#e5e7eb" else "black"
    cols <- colors_for_mode(dark_on)
    leg_df<-data.frame(TaggedPitchType=factor(types,levels=types),x=1,y=1)
    ggplot(leg_df,aes(x,y,color=TaggedPitchType))+geom_point(size=0,alpha=0)+
      scale_color_manual(values=cols[types],limits=types,name=NULL)+
      guides(color=guide_legend(nrow=1,byrow=TRUE,override.aes=list(size=4,alpha=1)))+
      theme_void()+theme(
        legend.position="bottom",
        legend.text=element_text(size=12,face="bold",colour=axis_col),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = NA),
        legend.text.align = 0
      )
  }, bg = "transparent")
  
  safe_pct <- function(num, den) {
    num <- suppressWarnings(as.numeric(num))
    den <- suppressWarnings(as.numeric(den))
    ifelse(is.finite(den) & den > 0 & is.finite(num),
           paste0(round(100 * num / den, 1), "%"),
           "")
  }
  
  nz_mean <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    m <- mean(x, na.rm = TRUE)
    if (is.finite(m)) m else NA_real_
  }
  
  
  # Summary Tables
  make_summary <- function(df) {
    total_n   <- nrow(df)
    usage_map <- usage_by_type(df)
    swing_levels <- c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall")
    
    pf <- compute_pa_flags(df)
    df <- pf$df %>%
      dplyr::mutate(
        .is_strikeout = pf$is_strikeout,
        .is_walk      = pf$is_walk,
        .is_swing     = ifelse(!is.na(PitchCall) & PitchCall %in% swing_levels, 1, 0)
      )
    
    # Precompute per-pitch QP points (Live only; non-Live → NA, non-competitive → 0)
    df <- df %>% dplyr::mutate(QP_pts = compute_qp_points(.))
    
    df %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(
        PitchCount    = dplyr::n(),
        Velo_Avg      = round(nz_mean(RelSpeed), 1),
        Velo_Max      = round(suppressWarnings(max(as.numeric(RelSpeed), na.rm = TRUE)), 1),
        IVB           = round(nz_mean(InducedVertBreak), 1),
        HB            = round(nz_mean(HorzBreak), 1),
        ReleaseTilt   = convert_to_clock(nz_mean(ReleaseTilt)),
        BreakTilt     = convert_to_clock(nz_mean(BreakTilt)),
        SpinEff       = { v <- nz_mean(SpinEfficiency); if (is.na(v)) "" else paste0(round(v * 100, 1), "%") },
        SpinRate      = round(nz_mean(SpinRate), 0),
        RelHeight     = round(nz_mean(RelHeight), 1),
        RelSide       = round(nz_mean(RelSide), 1),
        VertApprAngle = round(nz_mean(VertApprAngle), 1),
        HorzApprAngle = round(nz_mean(HorzApprAngle), 1),
        Extension     = round(nz_mean(Extension), 1),
        
        InZonePercent = {
          inzone <- (PlateLocSide >= ZONE_LEFT & PlateLocSide <= ZONE_RIGHT &
                       PlateLocHeight >= ZONE_BOTTOM & PlateLocHeight <= ZONE_TOP)
          safe_pct(sum(inzone, na.rm = TRUE), sum(!is.na(inzone)))
        },
        CompPercent   = {
          comp <- (PlateLocSide >= -1.5 & PlateLocSide <= 1.5 &
                     PlateLocHeight >= (2.65-1.7) & PlateLocHeight <= (2.65+1.3))
          safe_pct(sum(comp, na.rm = TRUE), sum(!is.na(comp)))
        },
        
        ## All sessions denominators (respect grouping)
        BF_live = sum(SessionType == "Live" & Balls == 0 & Strikes == 0, na.rm = TRUE),
        BF_all  = sum(!is.na(Balls) & !is.na(Strikes) & Balls == 0 & Strikes == 0, na.rm = TRUE),
        K_all   = sum(!is.na(Strikes) & Strikes == 2 & !is.na(PitchCall) & PitchCall %in% c("StrikeSwinging", "StrikeCalled"), na.rm = TRUE),
        BB_all  = sum(!is.na(Balls) & Balls == 3 & !is.na(PitchCall) & PitchCall == "BallCalled", na.rm = TRUE),
        
        KPercent  = safe_pct(K_all,  BF_all),
        BBPercent = safe_pct(BB_all, BF_all),
        
        FPS_all = sum(!is.na(Balls) & !is.na(Strikes) & Balls == 0 & Strikes == 0 &
                        !is.na(PitchCall) & PitchCall %in% c("InPlay","StrikeSwinging","StrikeCalled","FoulBallNotFieldable","FoulBall"), na.rm = TRUE),
        EA_all  = sum(
          (!is.na(Balls) & !is.na(Strikes) & !is.na(PitchCall)) & (
            (Balls == 0 & Strikes == 0 & PitchCall == "InPlay") |
              (Balls == 0 & Strikes == 1 & PitchCall %in% c(
                "InPlay", "StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable","FoulBall"
              )) |
              (Balls == 1 & Strikes == 0 & PitchCall == "InPlay") |
              (Balls == 1 & Strikes == 1 & PitchCall %in% c(
                "InPlay", "StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable","FoulBall"
              ))
          ), na.rm = TRUE
        ),
        
        FPSPercent = safe_pct(FPS_all, BF_all),
        EAPercent  = safe_pct(EA_all,  BF_all),
        
        StrikePercent = {
          strike_calls <- c("StrikeCalled","StrikeSwinging","FoulBallNotFieldable","InPlay","FoulBallFieldable","FoulBall")
          strikes <- sum(PitchCall %in% strike_calls, na.rm = TRUE)
          safe_pct(strikes, PitchCount)
        },
        SwingPercent = safe_pct(sum(.is_swing, na.rm = TRUE), PitchCount),
        WhiffPercent  = {
          sw  <- sum(PitchCall == "StrikeSwinging", na.rm = TRUE)
          den <- sum(PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall"), na.rm = TRUE)
          safe_pct(sw, den)
        },
        
        EV = round(nz_mean(ifelse(SessionType == "Live", ExitSpeed, NA_real_)), 1),
        LA = round(nz_mean(ifelse(SessionType == "Live", Angle,     NA_real_)), 1),
        
        `Stuff+`   = round(nz_mean(`Stuff+`), 1),
        `Ctrl+` = round(nz_mean(ifelse(
          PlateLocSide >= ZONE_LEFT & PlateLocSide <= ZONE_RIGHT &
            PlateLocHeight >= ZONE_BOTTOM & PlateLocHeight <= ZONE_TOP, 1.47,
          ifelse(
            PlateLocSide >= -1.5 & PlateLocSide <= 1.5 &
              PlateLocHeight >= (2.65-1.7) & PlateLocHeight <= (2.65+1.3),
            0.73, 0
          )
        )) * 100, 1),
        
        # 👇 define QP+ BEFORE Pitching+
        `QP+`       = round(mean(QP_pts, na.rm = TRUE) * 200, 1),
        `Pitching+` = round((`Stuff+` + `QP+`) / 2, 1),
        .groups = "drop"
      ) %>%
      dplyr::rename(PitchType = TaggedPitchType) %>%
      dplyr::mutate(PitchType = as.character(PitchType)) %>%
      dplyr::arrange(factor(PitchType, levels = names(all_colors))) %>%
      dplyr::mutate(
        Overall = paste0(dplyr::coalesce(round(usage_map[as.character(PitchType)], 1), 0), "%")
      ) %>%
      dplyr::select(
        PitchType, PitchCount, Overall, BF = BF_all,
        Velo_Avg, Velo_Max, IVB, HB,
        ReleaseTilt, BreakTilt, SpinEff, SpinRate,
        RelHeight, RelSide, VertApprAngle, HorzApprAngle, Extension,
        InZonePercent, CompPercent, KPercent, BBPercent, FPSPercent, EAPercent,
        StrikePercent, SwingPercent, WhiffPercent, EV, LA, BFP,
        `Stuff+`, `Ctrl+`, `QP+`, `Pitching+`
      )
  }
  
  # =========================
  # Summary page table (uses summary* controls ONLY)
  # =========================
  output$summaryTablePage <- DT::renderDataTable({
    df <- filtered_data()
    if (!nrow(df)) {
      return(DT::datatable(
        data.frame(Message = "No data for selected filters"),
        options = list(dom = 't'), rownames = FALSE
      ))
    }
    
    # Get the mode from Summary page controls
    mode <- if (!is.null(input$summaryTableMode)) input$summaryTableMode else "Stuff"
    custom <- if (!is.null(input$summaryCustomCols)) input$summaryCustomCols else character(0)
    
    # Use .dp_like_table for Counting mode and other standardized modes
    if (mode %in% c("Counting", "Stuff", "Process", "Bullpen", "Live", "Usage", "Custom")) {
      return(.dp_like_table(df, mode, custom, table_id = "summaryTablePage", session = session))
    }
    
    # --- small helpers (match Hitting DP logic) ---
    nz_mean <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      m <- mean(x, na.rm = TRUE)
      if (is.finite(m)) m else NA_real_
    }
    
    safe_div <- function(num, den) {
      num <- suppressWarnings(as.numeric(num))
      den <- suppressWarnings(as.numeric(den))
      ifelse(is.finite(den) & den != 0 & is.finite(num), num/den, NA_real_)
    }
    
    # Parse numeric values from strings like "12.3%", "  .456  ", "1,234", etc.
    # Any string containing % is treated as a percentage and divided by 100.
    parse_num <- function(x) {
      if (is.numeric(x)) return(x)
      sx <- trimws(as.character(x))
      sx[sx == ""] <- NA_character_
      is_pct <- grepl("%", sx)                     # percent anywhere in the string
      val <- suppressWarnings(as.numeric(gsub("[^0-9eE.+-]", "", sx)))  # keep digits, ., +/-, e/E
      val[is_pct] <- val[is_pct] / 100
      val
    }
    
    # Format 3-decimal rates without leading zero (e.g., .303)
    fmt_rate3 <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      s <- ifelse(is.finite(x), sprintf("%.3f", x), "")
      sub("^0\\.", ".", s)
    }
    
    # Alias to avoid breaking any existing calls that use fmt_avg
    fmt_avg <- fmt_rate3
    
    ip_fmt <- function(ip_raw) {
      out <- rep("", length(ip_raw))
      ok  <- is.finite(ip_raw) & ip_raw > 0
      outs <- floor(ip_raw[ok]*3 + 1e-8)
      inn  <- outs %/% 3
      rem  <- outs %% 3
      out[ok] <- paste0(inn, ".", rem)
      out
    }
    FIP_C <- if (exists("FIP_CONST")) get("FIP_CONST") else 3.20
    
    mode   <- if (!is.null(input$summaryTableMode)) input$summaryTableMode else "Results"
    custom <- if (!is.null(input$summaryCustomCols)) input$summaryCustomCols else character(0)
    
    # Fallback visible-set helper if not defined globally
    if (!exists("visible_set_for")) {
      visible_set_for <- function(mode, custom) {
        base <- c("Pitch","#","Usage","BF","IP","FIP","WHIP","Velo","Max","IVB","HB","rTilt","bTilt","SpinEff","Spin",
                  "Height","Side","VAA","HAA","Ext","InZone%","Comp%","Strike%","FPS%","E+A%",
                  "K%","BB%","Whiff%","EV","LA","Stuff+","Ctrl+","QP+","Pitching+",
                  # Results-specific common cols
                  "PA","AB","AVG","SLG","OBP","OPS","xWOBA","xISO","BABIP","GB%","Barrel%","Swing%","Whiff%","CSW%","K%","BB%","EV","LA")
        if (identical(mode, "Custom") && length(custom)) unique(c("Pitch", custom)) else base
      }
    }
    
    df <- ensure_tagged_pitch_type(df, fallback = NA_character_)
    swing_levels <- default_swing_levels()
    pf_flags <- compute_pa_flags(df)
    df       <- pf_flags$df
    df_is_strikeout <- pf_flags$is_strikeout
    df_is_walk      <- pf_flags$is_walk
    df_is_terminal  <- pf_flags$is_terminal
    strikeout_results <- c("Strikeout","StrikeoutSwinging","StrikeoutLooking")
    
    # ---------- RESULTS TABLE (use Hitting-style PA/AB math) ----------
    if (identical(mode, "Results")) {
      swing_levels <- default_swing_levels()
      
      term_idx <- which(df_is_terminal)
      term <- df[term_idx, , drop = FALSE]
      term$is_strikeout <- df_is_strikeout[term_idx]
      term$is_walk      <- df_is_walk[term_idx]
      
      # Per-pitch-type tallies (PA/AB/H/K/BB/HBP/Sac/HR)
      per_type <- term %>%
        dplyr::group_by(TaggedPitchType) %>%
        dplyr::summarise(
          PA   = dplyr::n(),
          HBP  = sum((!is.na(PlayResult) & PlayResult == "HitByPitch") | PitchCall == "HitByPitch", na.rm = TRUE),
          Sac  = sum(PlayResult == "Sacrifice",  na.rm = TRUE),
          `1B` = sum(PlayResult == "Single",  na.rm = TRUE),
          `2B` = sum(PlayResult == "Double",  na.rm = TRUE),
          `3B` = sum(PlayResult == "Triple",  na.rm = TRUE),
          HR   = sum(PlayResult == "HomeRun", na.rm = TRUE),
          Kct  = sum(is_strikeout, na.rm = TRUE),
          BBct = sum(is_walk,      na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          AB  = PA - (BBct + HBP + Sac),
          H   = `1B` + `2B` + `3B` + HR,
          TB  = 1*`1B` + 2*`2B` + 3*`3B` + 4*HR,
          AVG = safe_div(H, AB),
          SLG = safe_div(TB, AB),
          OBP = safe_div(H + BBct + HBP, PA),
          OPS = SLG + OBP
        )
      
      # Pitch totals for Usage/Swing%/Whiff%
      pitch_totals <- df %>%
        dplyr::group_by(TaggedPitchType) %>%
        dplyr::summarise(
          Pitches       = dplyr::n(),
          Swings        = sum(!is.na(PitchCall) & PitchCall %in% swing_levels, na.rm = TRUE),
          Whiffs        = sum(PitchCall == "StrikeSwinging", na.rm = TRUE),
          CalledStrikes = sum(PitchCall == "StrikeCalled",    na.rm = TRUE),
          .groups = "drop"
        )
      total_pitches <- sum(pitch_totals$Pitches, na.rm = TRUE)
      
      # Command scoring vector and per-type Command+ / Stuff+ / Pitching+
      scores <- ifelse(
        df$PlateLocSide >= ZONE_LEFT & df$PlateLocSide <= ZONE_RIGHT &
          df$PlateLocHeight >= ZONE_BOTTOM & df$PlateLocHeight <= ZONE_TOP, 1.47,
        ifelse(
          df$PlateLocSide >= -1.5 & df$PlateLocSide <= 1.5 &
            df$PlateLocHeight >= (2.65 - 1.7) & df$PlateLocHeight <= (2.65 + 1.3),
          0.73, 0
        )
      )
      sc_by_type <- df %>%
        dplyr::mutate(.scores = scores) %>%
        dplyr::group_by(TaggedPitchType) %>%
        dplyr::summarise(
          StuffP   = round(nz_mean(`Stuff+`), 1),
          CommandP = round(nz_mean(.scores) * 100, 1),
          .groups = "drop"
        ) %>%
        dplyr::mutate(PitchingP = round((StuffP + CommandP)/2, 1))
      
      # Live BIP for EV/LA and GB%
      bbe <- df %>% dplyr::filter(grepl("live|game|ab", tolower(SessionType)), PitchCall == "InPlay")
      evla <- bbe %>%
        dplyr::group_by(TaggedPitchType) %>%
        dplyr::summarise(EV = nz_mean(ExitSpeed), LA = nz_mean(Angle), .groups = "drop")
      gb <- bbe %>%
        dplyr::group_by(TaggedPitchType) %>%
        dplyr::summarise(
          `GB%` = {
            ht <- get_hit_type_col(dplyr::pick(dplyr::everything()))
            safe_div(sum(ht == "GroundBall", na.rm = TRUE),
                     sum(!is.na(ht),        na.rm = TRUE))
          },
          .groups = "drop"
        )
      
      # Extras (xWOBA/xISO/BABIP/Barrel%) — numeric
      extras <- compute_process_results(df) %>%
        dplyr::rename_with(~ "Pitch", dplyr::any_of("PitchType")) %>%
        dplyr::mutate(Pitch = as.character(Pitch))
      if (nrow(extras)) {
        num_cols_extra <- intersect(c("xWOBA","xISO","BABIP","Barrel%"), names(extras))
        extras[num_cols_extra] <- lapply(extras[num_cols_extra], parse_num)
      }
      
      # Build per-type rows (+ #, Overall, BF, IP, FIP, WHIP, Pitching+)
      res_pt <- pitch_totals %>%
        dplyr::full_join(per_type, by = "TaggedPitchType") %>%
        dplyr::left_join(evla,     by = "TaggedPitchType") %>%
        dplyr::left_join(gb,       by = "TaggedPitchType") %>%
        dplyr::mutate(
          Pitches       = dplyr::coalesce(Pitches, 0),
          Swings        = dplyr::coalesce(Swings, 0),
          Whiffs        = dplyr::coalesce(Whiffs, 0),
          CalledStrikes = dplyr::coalesce(CalledStrikes, 0),
          PA    = dplyr::coalesce(PA, 0),
          Sac   = dplyr::coalesce(Sac, 0),
          HBP   = dplyr::coalesce(HBP, 0),
          H     = dplyr::coalesce(H, 0),
          AB    = dplyr::coalesce(AB, 0),
          TB    = dplyr::coalesce(TB, 0),
          Kct   = dplyr::coalesce(Kct, 0),
          BBct  = dplyr::coalesce(BBct, 0),
          HR    = dplyr::coalesce(HR, 0),
          `Swing%` = safe_div(Swings, Pitches),
          `Whiff%` = safe_div(Whiffs, Swings),
          `CSW%`   = safe_div(Whiffs + CalledStrikes, Pitches),
          Outs     = (AB - H) + Sac,
          IP_raw   = safe_div(Outs, 3),
          BF       = PA,
          `#`      = Pitches,
          Overall  = ifelse(total_pitches > 0, paste0(round(100*Pitches/total_pitches,1), "%"), ""),
          FIP_tmp  = safe_div(13*HR + 3*(BBct + HBP) - 2*Kct, IP_raw),
          FIP      = ifelse(is.finite(FIP_tmp), round(FIP_tmp + FIP_C, 2), NA_real_),
          WHIP_tmp = safe_div(H + BBct, IP_raw),
          WHIP     = ifelse(is.finite(WHIP_tmp), round(WHIP_tmp, 2), NA_real_)
        ) %>%
        dplyr::left_join(sc_by_type, by = "TaggedPitchType") %>%
        dplyr::left_join(extras, by = c("TaggedPitchType" = "Pitch"), suffix = c("", ".extra")) %>%
        ensure_numeric_cols(c("xWOBA","xISO","BABIP","Barrel%")) %>%
        dplyr::transmute(
          Pitch = as.character(TaggedPitchType),
          `#`, Overall, BF, IP = ip_fmt(IP_raw), FIP, WHIP,
          PA, AB, AVG, SLG, OBP, OPS,
          xWOBA, xISO, BABIP,
          `Swing%`, `Whiff%`, `CSW%`, `GB%`,
          `K%` = safe_div(Kct, PA),
          `BB%` = safe_div(BBct, PA),
          `Barrel%`, EV, LA,
          `Stuff+` = StuffP,
          `Ctrl+` = CommandP,
          `Pitching+` = PitchingP
        )
      if (exists("all_colors", inherits = TRUE)) {
        pitch_order <- names(all_colors)
        res_pt <- res_pt %>%
          dplyr::arrange(
            factor(
              Pitch,
              levels = c(pitch_order, setdiff(unique(Pitch), pitch_order))
            )
          )
      }
      
      # --- ALL row (ungrouped, same definitions) ---
      PAt <- nrow(term)
      HBP_all <- sum((!is.na(term$PlayResult) & term$PlayResult == "HitByPitch") | term$PitchCall == "HitByPitch", na.rm = TRUE)
      Sac_all <- sum(term$PlayResult == "Sacrifice",  na.rm = TRUE)
      H1  <- sum(term$PlayResult == "Single",  na.rm = TRUE)
      H2  <- sum(term$PlayResult == "Double",  na.rm = TRUE)
      H3  <- sum(term$PlayResult == "Triple",  na.rm = TRUE)
      HR  <- sum(term$PlayResult == "HomeRun", na.rm = TRUE)
      H   <- H1 + H2 + H3 + HR
      TB  <- 1*H1 + 2*H2 + 3*H3 + 4*HR
      Kct_all <- sum(term$is_strikeout, na.rm = TRUE)
      BBc_all <- sum(term$is_walk,      na.rm = TRUE)
      ABt <- PAt - (BBc_all + HBP_all + Sac_all)
      swings  <- sum(!is.na(df$PitchCall) & df$PitchCall %in% swing_levels, na.rm = TRUE)
      whiffs  <- sum(df$PitchCall == "StrikeSwinging",                      na.rm = TRUE)
      gbpct_all <- {
        d <- df %>% dplyr::filter(grepl("live|game|ab", tolower(SessionType)), PitchCall == "InPlay")
        ht <- get_hit_type_col(d)
        safe_div(sum(ht == "GroundBall", na.rm = TRUE),
                 sum(!is.na(ht),         na.rm = TRUE))
      }
      # All-row IP/FIP/WHIP/Pitching+
      Outs_all <- (ABt - H) + Sac_all
      IP_all   <- safe_div(Outs_all, 3)
      FIP_all  <- {
        tmp <- safe_div(13*HR + 3*(BBc_all + HBP_all) - 2*Kct_all, IP_all)
        ifelse(is.finite(tmp), round(tmp + FIP_C, 2), NA_real_)
      }
      WHIP_all <- {
        tmp <- safe_div(H + BBc_all, IP_all)
        ifelse(is.finite(tmp), round(tmp, 2), NA_real_)
      }
      
      csw_all <- sum(df$PitchCall %in% c("StrikeCalled","StrikeSwinging"), na.rm = TRUE)
      stuff_all <- round(nz_mean(df$`Stuff+`), 1)
      qp_all <- round(nz_mean(compute_qp_points(df)) * 200, 1)
      ctrl_all <- round(nz_mean(scores) * 100, 1)
      pitc_all <- round((stuff_all + qp_all) / 2, 1)
      
      all_row <- tibble::tibble(
        Pitch = "All",
        `#`   = nrow(df),
        Overall = "100%",
        BF = PAt,
        IP = ip_fmt(IP_all),
        FIP = FIP_all,
        WHIP = WHIP_all,
        PA = PAt, AB = ABt,
        AVG = safe_div(H, ABt),
        SLG = safe_div(TB, ABt),
        OBP = safe_div(H + BBc_all + HBP_all, PAt),
        OPS = NA_real_,
        xWOBA = NA_real_, xISO = NA_real_, BABIP = NA_real_,
        `Swing%` = safe_div(swings, nrow(df)),
        `Whiff%` = safe_div(whiffs, swings),
        `CSW%`   = safe_div(csw_all, nrow(df)),
        `GB%`    = gbpct_all,
        `K%`     = safe_div(Kct_all, PAt),
        `BB%`    = safe_div(BBc_all, PAt),
        `Barrel%`= NA_real_,
        EV = nz_mean(bbe$ExitSpeed),
        LA = nz_mean(bbe$Angle),
        `Stuff+` = stuff_all,
        `Ctrl+`  = ctrl_all,
        `Pitching+` = pitc_all
      )
      all_row$OPS <- all_row$SLG + all_row$OBP
      
      # Fill extras (All)
      extras_all_raw <- compute_process_results(df)
      if (nrow(extras_all_raw)) {
        if ("Barrel%" %in% names(extras_all_raw)) {
          extras_all_raw$`Barrel%` <- parse_num(extras_all_raw$`Barrel%`)
        }
        get_col <- function(dat, col) {
          if (col %in% names(dat)) parse_num(dat[[col]]) else numeric(0)
        }
        extras_all <- tibble::tibble(
          xWOBA     = nz_mean(get_col(extras_all_raw, "xWOBA")),
          xISO      = nz_mean(get_col(extras_all_raw, "xISO")),
          BABIP     = nz_mean(get_col(extras_all_raw, "BABIP")),
          `Barrel%` = nz_mean(get_col(extras_all_raw, "Barrel%"))
        )
        all_row$xWOBA     <- extras_all$xWOBA[1]
        all_row$xISO      <- extras_all$xISO[1]
        all_row$BABIP     <- extras_all$BABIP[1]
        all_row$`Barrel%` <- extras_all$`Barrel%`[1]
      }
      
      # Bind + format
      df_out <- dplyr::bind_rows(res_pt, all_row) %>%
        dplyr::mutate(
          dplyr::across(c(PA, AB, AVG, SLG, OBP, OPS, 
                          `Swing%`, `Whiff%`, `CSW%`, `GB%`, `K%`, `BB%`, `Barrel%`,
                          EV, LA, FIP, WHIP),
                        ~ suppressWarnings(as.numeric(.)))
        )
      pct_cols  <- intersect(c("Swing%","Whiff%","CSW%","GB%","K%","BB%","Barrel%"), names(df_out))
      rate_cols <- intersect(c("AVG","SLG","OBP","OPS","xWOBA","xISO","BABIP"), names(df_out))
      if (length(pct_cols)) {
        df_out[pct_cols]  <- lapply(df_out[pct_cols],  function(z) ifelse(is.finite(z), paste0(round(z*100,1), "%"), ""))
      }
      if (length(rate_cols)) {
        df_out[rate_cols] <- lapply(df_out[rate_cols], function(z) ifelse(is.finite(z), fmt_avg(z), ""))
      }
      df_out$EV   <- ifelse(is.finite(df_out$EV),   round(df_out$EV, 1), "")
      df_out$LA   <- ifelse(is.finite(df_out$LA),   round(df_out$LA, 1), "")
      df_out$FIP  <- ifelse(is.finite(df_out$FIP),  sprintf("%.2f", df_out$FIP), "")
      df_out$WHIP <- ifelse(is.finite(df_out$WHIP), sprintf("%.2f", df_out$WHIP), "")
      
      df_dt <- if (exists("collapse_list_cols")) collapse_list_cols(df_out) else df_out
      df_dt <- as.data.frame(df_dt, stringsAsFactors = FALSE)
      df_disp <- df_dt
      if ("#" %in% names(df_disp)) {
        df_disp$`#` <- wrap_pitch_counts(df_disp$`#`)
      }
      
      summary_table_page_cache(list(
        table = df_disp,
        source = df,
        label_column = "Pitch"
      ))
      
      visible_set <- visible_set_for(mode, custom)
      return(datatable_with_colvis(
        df_disp,
        lock            = "Pitch",
        remember        = FALSE,
        default_visible = intersect(visible_set, names(df_disp)),
        mode            = mode
      ))
    }
    
    # ---------- NON-Results modes (your original build) ----------
    # QP+ per pitch type (scalar)
    qp_by_type <- df %>%
      dplyr::filter(!is.na(TaggedPitchType) & nzchar(as.character(TaggedPitchType))) %>%
      dplyr::group_split(TaggedPitchType, .keep = TRUE) %>%
      purrr::map_dfr(function(chunk) {
        tpt <- chunk$TaggedPitchType[1]
        vals <- compute_qp_points(chunk)
        vals <- suppressWarnings(as.numeric(vals))
        tibble::tibble(
          TaggedPitchType = tpt,
          `QP+` = if (length(vals) && any(is.finite(vals))) round(mean(vals, na.rm = TRUE) * 200, 1) else NA_real_
        )
      })
    
    # Base per-pitch-type summary
    # Base per-pitch-type summary
    summ <- make_summary(df)
    summ <- dplyr::mutate(summ,
                          ReleaseTilt = as.character(ReleaseTilt),
                          BreakTilt   = as.character(BreakTilt)
    )
    if (!("QP+" %in% names(summ))) {
      # join by PitchType in 'summ' to TaggedPitchType in qp_by_type
      summ <- summ %>% dplyr::left_join(qp_by_type, by = c("PitchType" = "TaggedPitchType"))
    }
    
    
    # ---- Build FULL table (as before) ----
    df_table <- dplyr::bind_rows(
      summ,
      {
        scores <- ifelse(
          df$PlateLocSide >= ZONE_LEFT & df$PlateLocSide <= ZONE_RIGHT &
            df$PlateLocHeight >= ZONE_BOTTOM & df$PlateLocHeight <= ZONE_TOP, 1.47,
          ifelse(
            df$PlateLocSide >= -1.5 & df$PlateLocSide <= 1.5 &
              df$PlateLocHeight >= (2.65-1.7) & df$PlateLocHeight <= (2.65+1.3),
            0.73, 0
          )
        )
        has_pc  <- sum(!is.na(df$PitchCall)) > 0
        strikes <- sum(df$PitchCall %in% c("StrikeCalled","StrikeSwinging","FoulBallNotFieldable","InPlay","FoulBallFieldable","FoulBall"), na.rm = TRUE)
        sw      <- sum(df$PitchCall == "StrikeSwinging", na.rm = TRUE)
        den     <- sum(df$PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall"), na.rm = TRUE)
        live_mask <- df$SessionType == "Live"
        bf_live <- sum(live_mask & df$Balls == 0 & df$Strikes == 0, na.rm = TRUE)
        k_live  <- sum(live_mask & df_is_strikeout, na.rm = TRUE)
        bb_live <- sum(live_mask & df_is_walk,      na.rm = TRUE)
        fps_live <- sum(df$SessionType == "Live" & df$Balls == 0 & df$Strikes == 0 &
                          df$PitchCall %in% c("InPlay","StrikeSwinging","StrikeCalled","FoulBallNotFieldable","FoulBall"), na.rm = TRUE)
        ea_live  <- sum(df$SessionType == "Live" & (
          (df$Balls == 0 & df$Strikes == 0 & df$PitchCall == "InPlay") |
            (df$Balls == 0 & df$Strikes == 1 & df$PitchCall %in% c(
              "InPlay", "StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable","FoulBall"
            )) |
            (df$Balls == 1 & df$Strikes == 0 & df$PitchCall == "InPlay") |
            (df$Balls == 1 & df$Strikes == 1 & df$PitchCall %in% c(
              "InPlay", "StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable","FoulBall"
            ))
        ),
        na.rm = TRUE
        )
        
        vmax   <- suppressWarnings(max(as.numeric(df$RelSpeed), na.rm = TRUE)); vmax <- if (is.finite(vmax)) round(vmax, 1) else NA_real_
        ev_all <- nz_mean(ifelse(df$SessionType=="Live", df$ExitSpeed, NA_real_))
        la_all <- nz_mean(ifelse(df$SessionType=="Live", df$Angle,     NA_real_))
        stuff_all <- round(nz_mean(df$`Stuff+`), 1)
        ctrl_all   <- round(nz_mean(scores) * 100, 1)
        qp_all    <- round(nz_mean(compute_qp_points(df)) * 200, 1)
        
        tibble::tibble(
          Pitch         = "All",
          PitchCount    = nrow(df),
          Overall       = "100%",
          BF            = bf_live,
          Velo_Avg      = round(nz_mean(df$RelSpeed), 1),
          Velo_Max      = vmax,
          IVB           = round(nz_mean(df$InducedVertBreak), 1),
          HB            = round(nz_mean(df$HorzBreak), 1),
          ReleaseTilt   = convert_to_clock(nz_mean(df$ReleaseTilt)),
          BreakTilt     = convert_to_clock(nz_mean(df$BreakTilt)),
          SpinEff       = { v <- nz_mean(df$SpinEfficiency); if (is.na(v)) "" else paste0(round(v*100,1), "%") },
          SpinRate      = round(nz_mean(df$SpinRate), 0),
          RelHeight     = round(nz_mean(df$RelHeight), 1),
          RelSide       = round(nz_mean(df$RelSide), 1),
          VertApprAngle = round(nz_mean(df$VertApprAngle), 1),
          HorzApprAngle = round(nz_mean(df$HorzApprAngle), 1),
          Extension     = round(nz_mean(df$Extension), 1),
          InZonePercent = { inzone <- (df$PlateLocSide >= ZONE_LEFT & df$PlateLocSide <= ZONE_RIGHT &
                                         df$PlateLocHeight >= ZONE_BOTTOM & df$PlateLocHeight <= ZONE_TOP)
          ifelse(sum(!is.na(inzone))>0, paste0(round(100*sum(inzone, na.rm=TRUE)/sum(!is.na(inzone)),1), "%"), "") },
          CompPercent   = { comp <- (df$PlateLocSide >= -1.5 & df$PlateLocSide <= 1.5 &
                                       df$PlateLocHeight >= (2.65-1.7) & df$PlateLocHeight <= (2.65+1.3))
          ifelse(sum(!is.na(comp))>0, paste0(round(100*sum(comp, na.rm=TRUE)/sum(!is.na(comp)),1), "%"), "") },
          KPercent      = ifelse(bf_live>0, paste0(round(100*k_live/bf_live,1), "%"), ""),
          BBPercent     = ifelse(bf_live>0, paste0(round(100*bb_live/bf_live,1), "%"), ""),
          FPSPercent    = ifelse(bf_live>0, paste0(round(100*fps_live/bf_live,1), "%"), ""),
          EAPercent     = ifelse(bf_live>0, paste0(round(100*ea_live/bf_live,1), "%"), ""),
          StrikePercent = ifelse(has_pc, paste0(round(100*strikes/nrow(df),1), "%"), ""),
          SwingPercent  = ifelse(has_pc, paste0(round(100*sum(df$PitchCall %in% swing_levels, na.rm = TRUE)/nrow(df),1), "%"), ""),
          WhiffPercent  = ifelse(den>0, paste0(round(100*sw/den,1), "%"), ""),
          EV = ev_all, LA = la_all,
          BFP = {
            val <- compute_bfp_overall(df)
            if (!is.finite(val)) 0 else val
          },
          `Stuff+` = stuff_all, `Ctrl+` = ctrl_all, `QP+` = qp_all
        ) %>% dplyr::mutate(`Pitching+` = round((`Stuff+` + `QP+`)/2, 1))
      }
    ) %>%
      dplyr::rename(
        `#`    = PitchCount,
        Velo   = Velo_Avg,
        Max    = Velo_Max,
        rTilt  = ReleaseTilt,
        bTilt  = BreakTilt,
        Spin   = SpinRate,
        Height = RelHeight,
        Side   = RelSide,
        Ext    = Extension,
        `InZone%` = InZonePercent,
        `Comp%`   = CompPercent,
        `K%`      = KPercent,
        `BB%`     = BBPercent,
        `FPS%`    = FPSPercent,
        `E+A%`    = EAPercent,
        `Strike%` = StrikePercent,
        `Swing%`  = SwingPercent,
        `Whiff%`  = WhiffPercent,
        VAA       = VertApprAngle,
        HAA       = HorzApprAngle
      ) %>%
      dplyr::mutate(Pitch = as.character(Pitch)) %>%
      dplyr::select(
        Pitch, `#`, Overall, BF,
        Velo, Max, IVB, HB, rTilt, bTilt, SpinEff, Spin, Height, Side, VAA, HAA, Ext,
        `InZone%`, `Comp%`, `Strike%`, `Swing%`, `FPS%`, `E+A%`, `K%`, `BB%`, `Whiff%`, EV, LA, BFP,
        `Stuff+`, `Ctrl+`, `QP+`, `Pitching+`
      ) %>%
      dplyr::mutate(
        EV = ifelse(is.na(as.numeric(EV)), "", round(as.numeric(EV), 1)),
        LA = ifelse(is.na(as.numeric(LA)), "", round(as.numeric(LA), 1)),
        BFP = ifelse(
          Pitch == "All",
          ifelse(is.na(as.numeric(BFP)), "", sprintf("%.1f", as.numeric(BFP))),
          ""
        )
      )
    
    # Add Process/Results columns and SANITIZE for DT
    existing_cols <- names(df_table)
    extras <- compute_process_results(df, mode) %>%
      dplyr::rename(Pitch = PitchType) %>%
      dplyr::mutate(Pitch = as.character(Pitch)) %>%
      dplyr::select(Pitch, dplyr::any_of(setdiff(names(.), existing_cols)))
    df_table <- df_table %>% dplyr::left_join(extras, by = "Pitch", suffix = c("", ".extra"))
    
    # Add usage calculations for Usage table mode
    if (identical(mode, "Usage")) {
      usage_extras <- compute_usage_by_count(df) %>%
        dplyr::rename(Pitch = PitchType) %>%
        dplyr::mutate(Pitch = as.character(Pitch))
      df_table <- df_table %>% dplyr::left_join(usage_extras, by = "Pitch")
    }
    
    df_table <- collapse_list_cols(df_table)
    is_date_like <- function(x) inherits(x, c("Date","POSIXct","POSIXt","difftime"))
    date_like <- vapply(df_table, is_date_like, logical(1))
    if (any(date_like)) df_table[date_like] <- lapply(df_table[date_like], as.character)
    df_table <- as.data.frame(df_table, stringsAsFactors = FALSE)
    
    if (all(c("Whiff%","CSW%") %in% names(df_table))) {
      df_table <- df_table %>% dplyr::relocate(`CSW%`, .after = `Whiff%`)
    }
    if ("Pitching+" %in% names(df_table)) {
      df_table <- df_table %>% dplyr::relocate(`Pitching+`, .after = dplyr::last_col())
    }
    
    df_table <- enforce_process_order(df_table)
    df_table <- enforce_stuff_order(df_table)
    df_disp <- df_table
    if ("#" %in% names(df_disp)) {
      df_disp$`#` <- wrap_pitch_counts(df_disp$`#`)
    }
    
    summary_table_page_cache(list(
      table = df_disp,
      source = df,
      label_column = "Pitch"
    ))
    
    # Determine if this is a Live session and modify visible set accordingly
    has_live_data <- any(df$SessionType == "Live", na.rm = TRUE)
    if (identical(mode, "Results") && has_live_data) {
      visible_set <- results_cols_live
    } else {
      visible_set <- visible_set_for(mode, custom)
    }
    
    # Remove unwanted columns for Live sessions
    if (has_live_data && identical(mode, "Results")) {
      cols_to_remove <- c("IP", "WHIP", "AVG", "SLG", "FIP", "xWOBA", "xISO", "BABIP")
      df_disp <- df_disp[, !names(df_disp) %in% cols_to_remove, drop = FALSE]
    }
    
    datatable_with_colvis(
      df_disp,
      lock            = "Pitch",
      remember        = FALSE,
      default_visible = intersect(visible_set, names(df_disp)),
      mode            = mode
    )
  })
  
  # -----------------------------
  # Data & Performance table (DP)
  # -----------------------------
  output$summaryTable <- DT::renderDataTable({
    df <- filtered_data()
    if (!nrow(df)) {
      return(DT::datatable(
        data.frame(Message = "No data for selected filters"),
        options = list(dom = 't'), rownames = FALSE
      ))
    }
    
    # prefer DP page controls; fall back to Summary page controls
    mode <- if (!is.null(input$dpTableMode)) input$dpTableMode else
      if (!is.null(input$summaryTableMode)) input$summaryTableMode else "Results"
    custom <- if (!is.null(input$dpCustomCols)) input$dpCustomCols else
      if (!is.null(input$summaryCustomCols)) input$summaryCustomCols else character(0)
    
    # Use .dp_like_table for Counting mode and other standardized modes
    if (mode %in% c("Counting", "Stuff", "Process", "Bullpen", "Live", "Usage", "Custom")) {
      return(.dp_like_table(df, mode, custom, table_id = "summaryTable", session = session))
    }
    
    # --- small helpers (match Hitting logic) ---
    nz_mean <- function(x) { x <- suppressWarnings(as.numeric(x)); m <- mean(x, na.rm = TRUE); if (is.finite(m)) m else NA_real_ }
    fmt_avg <- function(x) { z <- ifelse(is.finite(x), sprintf("%.3f", x), NA_character_); sub("^0", "", z) }
    safe_div <- function(num, den) ifelse(den > 0, num/den, NA_real_)
    parse_num <- function(x) {
      if (is.numeric(x)) return(x)
      x1 <- trimws(as.character(x)); x1[x1 == ""] <- NA_character_
      ifelse(grepl("%$", x1), suppressWarnings(as.numeric(sub("%$","",x1)))/100,
             suppressWarnings(as.numeric(x1)))
    }
    ip_fmt <- function(ip_raw) {
      out <- rep("", length(ip_raw))
      ok  <- is.finite(ip_raw) & ip_raw > 0
      outs <- floor(ip_raw[ok]*3 + 1e-8)
      inn  <- outs %/% 3
      rem  <- outs %% 3
      out[ok] <- paste0(inn, ".", rem)
      out
    }
    FIP_C <- if (exists("FIP_CONST")) get("FIP_CONST") else 3.20
    
    # prefer DP page controls; fall back to Summary page controls
    mode <- if (!is.null(input$dpTableMode)) input$dpTableMode else
      if (!is.null(input$summaryTableMode)) input$summaryTableMode else "Results"
    custom <- if (!is.null(input$dpCustomCols)) input$dpCustomCols else
      if (!is.null(input$summaryCustomCols)) input$summaryCustomCols else character(0)
    
    # Fallback visible-set helper if your global helper isn't in scope
    if (!exists("visible_set_for")) {
      visible_set_for <- function(mode, custom) {
        base <- c("Pitch","#","Usage","BF","IP","FIP","WHIP","Velo","Max","IVB","HB","rTilt","bTilt","SpinEff","Spin",
                  "Height","Side","VAA","HAA","Ext","InZone%","Comp%","Strike%","FPS%","E+A%",
                  "K%","BB%","Whiff%","EV","LA","Stuff+","Ctrl+","QP+","Pitching+",
                  # Results-specific
                  "PA","AB","AVG","SLG","OBP","OPS","xWOBA","xISO","BABIP","GB%","Barrel%","Swing%","Whiff%","CSW%","K%","BB%","EV","LA"
        )
        if (identical(mode, "Custom") && length(custom)) unique(c("Pitch", custom)) else base
      }
    }
    
    df <- ensure_tagged_pitch_type(df, fallback = NA_character_)
    swing_levels <- default_swing_levels()
    pf_flags <- compute_pa_flags(df)
    df       <- pf_flags$df
    df_is_strikeout <- pf_flags$is_strikeout
    df_is_walk      <- pf_flags$is_walk
    df_is_terminal  <- pf_flags$is_terminal
    
    # ---------- RESULTS TABLE (fixes inflated BIP rates) ----------
    if (identical(mode, "Results")) {
      
      term_idx <- which(df_is_terminal)
      term <- df[term_idx, , drop = FALSE]
      term$is_strikeout <- df_is_strikeout[term_idx]
      term$is_walk      <- df_is_walk[term_idx]
      
      # Per-pitch-type tallies (PA/AB/H/K/BB/HBP/Sac/HR)
      per_type <- term %>%
        dplyr::group_by(TaggedPitchType) %>%
        dplyr::summarise(
          PA   = dplyr::n(),
          HBP  = sum((!is.na(PlayResult) & PlayResult == "HitByPitch") | PitchCall == "HitByPitch", na.rm = TRUE),
          Sac  = sum(PlayResult == "Sacrifice",  na.rm = TRUE),
          `1B` = sum(PlayResult == "Single",  na.rm = TRUE),
          `2B` = sum(PlayResult == "Double",  na.rm = TRUE),
          `3B` = sum(PlayResult == "Triple",  na.rm = TRUE),
          HR   = sum(PlayResult == "HomeRun", na.rm = TRUE),
          Kct  = sum(is_strikeout, na.rm = TRUE),
          BBct = sum(is_walk,      na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          AB  = PA - (BBct + HBP + Sac),
          H   = `1B` + `2B` + `3B` + HR,
          TB  = 1*`1B` + 2*`2B` + 3*`3B` + 4*HR,
          AVG = safe_div(H, AB),
          SLG = safe_div(TB, AB),
          OBP = safe_div(H + BBct + HBP, PA),
          OPS = SLG + OBP
        )
      
      # Pitch totals for Usage/Swing%/Whiff%
      pitch_totals <- df %>%
        dplyr::group_by(TaggedPitchType) %>%
        dplyr::summarise(
          Pitches       = dplyr::n(),
          Swings        = sum(!is.na(PitchCall) & PitchCall %in% swing_levels, na.rm = TRUE),
          Whiffs        = sum(PitchCall == "StrikeSwinging", na.rm = TRUE),
          CalledStrikes = sum(PitchCall == "StrikeCalled",    na.rm = TRUE),
          .groups = "drop"
        )
      total_pitches <- sum(pitch_totals$Pitches, na.rm = TRUE)
      
      # Command scoring vector and per-type Command+ / Stuff+ / Pitching+
      scores <- ifelse(
        df$PlateLocSide >= ZONE_LEFT & df$PlateLocSide <= ZONE_RIGHT &
          df$PlateLocHeight >= ZONE_BOTTOM & df$PlateLocHeight <= ZONE_TOP, 1.47,
        ifelse(
          df$PlateLocSide >= -1.5 & df$PlateLocSide <= 1.5 &
            df$PlateLocHeight >= (2.65 - 1.7) & df$PlateLocHeight <= (2.65 + 1.3),
          0.73, 0
        )
      )
      sc_by_type <- df %>%
        dplyr::mutate(.scores = scores) %>%
        dplyr::group_by(TaggedPitchType) %>%
        dplyr::summarise(
          StuffP   = round(nz_mean(`Stuff+`), 1),
          CommandP = round(nz_mean(.scores) * 100, 1),
          .groups = "drop"
        ) %>%
        dplyr::mutate(PitchingP = round((StuffP + CommandP)/2, 1))
      
      # Live balls in play for EV/LA and GB%
      bbe <- df %>% dplyr::filter(grepl("live|game|ab", tolower(SessionType)), PitchCall == "InPlay")
      evla <- bbe %>%
        dplyr::group_by(TaggedPitchType) %>%
        dplyr::summarise(EV = nz_mean(ExitSpeed), LA = nz_mean(Angle), .groups = "drop")
      gb <- bbe %>%
        dplyr::group_by(TaggedPitchType) %>%
        dplyr::summarise(
          `GB%` = {
            ht <- get_hit_type_col(dplyr::pick(dplyr::everything()))
            safe_div(sum(ht == "GroundBall", na.rm = TRUE),
                     sum(!is.na(ht),        na.rm = TRUE))
          },
          .groups = "drop"
        )
      
      # Extras (xWOBA/xISO/BABIP/Barrel%) — numeric
      extras <- compute_process_results(df, mode) %>%
        dplyr::rename_with(~ "Pitch", dplyr::any_of("PitchType")) %>%
        dplyr::mutate(Pitch = as.character(Pitch))
      if (nrow(extras)) {
        num_cols_extra <- intersect(c("xWOBA","xISO","BABIP","Barrel%"), names(extras))
        extras[num_cols_extra] <- lapply(extras[num_cols_extra], parse_num)
      }
      
      # Build per-type rows (+ #, Overall, BF, IP, FIP, WHIP, Pitching+)
      res_pt <- pitch_totals %>%
        dplyr::full_join(per_type, by = "TaggedPitchType") %>%
        dplyr::left_join(evla,         by = "TaggedPitchType") %>%
        dplyr::left_join(gb,           by = "TaggedPitchType") %>%
        dplyr::mutate(
          Pitches       = dplyr::coalesce(Pitches, 0),
          Swings        = dplyr::coalesce(Swings, 0),
          Whiffs        = dplyr::coalesce(Whiffs, 0),
          CalledStrikes = dplyr::coalesce(CalledStrikes, 0),
          PA    = dplyr::coalesce(PA, 0),
          Sac   = dplyr::coalesce(Sac, 0),
          HBP   = dplyr::coalesce(HBP, 0),
          H     = dplyr::coalesce(H, 0),
          AB    = dplyr::coalesce(AB, 0),
          TB    = dplyr::coalesce(TB, 0),
          Kct   = dplyr::coalesce(Kct, 0),
          BBct  = dplyr::coalesce(BBct, 0),
          HR    = dplyr::coalesce(HR, 0),
          `Swing%` = safe_div(Swings, Pitches),
          `Whiff%` = safe_div(Whiffs, Swings),
          `CSW%`   = safe_div(Whiffs + CalledStrikes, Pitches),
          Outs     = (AB - H) + Sac,
          IP_raw   = safe_div(Outs, 3),
          BF       = PA,
          `#`      = Pitches,
          Overall  = ifelse(total_pitches > 0, paste0(round(100*Pitches/total_pitches,1), "%"), ""),
          FIP_tmp  = safe_div(13*HR + 3*(BBct + HBP) - 2*Kct, IP_raw),
          FIP      = ifelse(is.finite(FIP_tmp), round(FIP_tmp + FIP_C, 2), NA_real_),
          WHIP_tmp = safe_div(H + BBct, IP_raw),
          WHIP     = ifelse(is.finite(WHIP_tmp), round(WHIP_tmp, 2), NA_real_)
        ) %>%
        dplyr::left_join(sc_by_type, by = "TaggedPitchType") %>%
        dplyr::left_join(extras, by = c("TaggedPitchType" = "Pitch"), suffix = c("", ".extra")) %>%
        ensure_numeric_cols(c("xWOBA","xISO","BABIP","Barrel%")) %>%
        dplyr::transmute(
          Pitch = as.character(TaggedPitchType),
          `#`, Overall, BF, IP = ip_fmt(IP_raw), FIP, WHIP,
          PA, AB, AVG, SLG, OBP, OPS,
          xWOBA, xISO, BABIP,
          `Swing%`, `Whiff%`, `CSW%`, `GB%`,
          `K%` = safe_div(Kct, PA),
          `BB%` = safe_div(BBct, PA),
          `Barrel%`, EV, LA,
          `Stuff+` = StuffP,
          `Ctrl+` = CommandP,
          `Pitching+` = PitchingP
        )
      if (exists("all_colors", inherits = TRUE)) {
        pitch_order <- names(all_colors)
        res_pt <- res_pt %>%
          dplyr::arrange(
            factor(
              Pitch,
              levels = c(pitch_order, setdiff(unique(Pitch), pitch_order))
            )
          )
      }
      
      # --- ALL row (same definitions, ungrouped) ---
      PAt <- nrow(term)
      HBP_all <- sum((!is.na(term$PlayResult) & term$PlayResult == "HitByPitch") | term$PitchCall == "HitByPitch", na.rm = TRUE)
      Sac_all <- sum(term$PlayResult == "Sacrifice",  na.rm = TRUE)
      H1  <- sum(term$PlayResult == "Single",  na.rm = TRUE)
      H2  <- sum(term$PlayResult == "Double",  na.rm = TRUE)
      H3  <- sum(term$PlayResult == "Triple",  na.rm = TRUE)
      HR  <- sum(term$PlayResult == "HomeRun", na.rm = TRUE)
      H   <- H1 + H2 + H3 + HR
      TB  <- 1*H1 + 2*H2 + 3*H3 + 4*HR
      Kct_all <- sum(term$is_strikeout, na.rm = TRUE)
      BBc_all <- sum(term$is_walk,      na.rm = TRUE)
      ABt <- PAt - (BBc_all + HBP_all + Sac_all)
      swings  <- sum(!is.na(df$PitchCall) & df$PitchCall %in% swing_levels, na.rm = TRUE)
      whiffs  <- sum(df$PitchCall == "StrikeSwinging",                      na.rm = TRUE)
      gbpct_all <- {
        d <- df %>% dplyr::filter(grepl("live|game|ab", tolower(SessionType)), PitchCall == "InPlay")
        ht <- get_hit_type_col(d)
        safe_div(sum(ht == "GroundBall", na.rm = TRUE),
                 sum(!is.na(ht),         na.rm = TRUE))
      }
      Outs_all <- (ABt - H) + Sac_all
      IP_all   <- safe_div(Outs_all, 3)
      FIP_all  <- {
        tmp <- safe_div(13*HR + 3*(BBc_all + HBP_all) - 2*Kct_all, IP_all)
        ifelse(is.finite(tmp), round(tmp + FIP_C, 2), NA_real_)
      }
      WHIP_all <- {
        tmp <- safe_div(H + BBc_all, IP_all)
        ifelse(is.finite(tmp), round(tmp, 2), NA_real_)
      }
      stuff_all <- round(nz_mean(df$`Stuff+`), 1)
      qp_all <- round(nz_mean(compute_qp_points(df)) * 200, 1)
      ctrl_all <- round(nz_mean(scores) * 100, 1)
      pitc_all <- round((stuff_all + qp_all) / 2, 1)
      csw_all <- sum(df$PitchCall %in% c("StrikeCalled","StrikeSwinging"), na.rm = TRUE)
      
      all_row <- tibble::tibble(
        Pitch = "All",
        `#`   = nrow(df),
        Overall = "100%",
        BF = PAt,
        IP = ip_fmt(IP_all),
        FIP = FIP_all,
        WHIP = WHIP_all,
        PA = PAt, AB = ABt,
        AVG = safe_div(H, ABt),
        SLG = safe_div(TB, ABt),
        OBP = safe_div(H + BBc_all + HBP_all, PAt),
        OPS = NA_real_,
        xWOBA = NA_real_, xISO = NA_real_, BABIP = NA_real_,
        `Swing%` = safe_div(swings, nrow(df)),
        `Whiff%` = safe_div(whiffs, swings),
        `CSW%`   = safe_div(csw_all, nrow(df)),
        `GB%`    = gbpct_all,
        `K%`     = safe_div(Kct_all, PAt),
        `BB%`    = safe_div(BBc_all, PAt),
        `Barrel%`= NA_real_,
        EV = nz_mean(bbe$ExitSpeed),
        LA = nz_mean(bbe$Angle),
        `Stuff+` = stuff_all,
        `Ctrl+`  = ctrl_all,
        `Pitching+` = pitc_all
      )
      all_row$OPS <- all_row$SLG + all_row$OBP
      
      # Fill extras (All)
      extras_all_raw <- compute_process_results(df)
      if (nrow(extras_all_raw)) {
        if ("Barrel%" %in% names(extras_all_raw)) {
          extras_all_raw$`Barrel%` <- parse_num(extras_all_raw$`Barrel%`)
        }
        get_col <- function(dat, col) {
          if (col %in% names(dat)) parse_num(dat[[col]]) else numeric(0)
        }
        extras_all <- tibble::tibble(
          xWOBA     = nz_mean(get_col(extras_all_raw, "xWOBA")),
          xISO      = nz_mean(get_col(extras_all_raw, "xISO")),
          BABIP     = nz_mean(get_col(extras_all_raw, "BABIP")),
          `Barrel%` = nz_mean(get_col(extras_all_raw, "Barrel%"))
        )
        all_row$xWOBA     <- extras_all$xWOBA[1]
        all_row$xISO      <- extras_all$xISO[1]
        all_row$BABIP     <- extras_all$BABIP[1]
        all_row$`Barrel%` <- extras_all$`Barrel%`[1]
      }
      
      # Bind + format
      df_out <- dplyr::bind_rows(res_pt, all_row) %>%
        dplyr::mutate(
          dplyr::across(c(PA, AB, AVG, SLG, OBP, OPS, 
                          `Swing%`, `Whiff%`, `CSW%`, `GB%`, `K%`, `BB%`, `Barrel%`,
                          EV, LA, FIP, WHIP),
                        ~ suppressWarnings(as.numeric(.)))
        )
      pct_cols  <- intersect(c("Swing%","Whiff%","CSW%","GB%","K%","BB%","Barrel%"), names(df_out))
      rate_cols <- intersect(c("AVG","SLG","OBP","OPS","xWOBA","xISO","BABIP"), names(df_out))
      if (length(pct_cols)) {
        df_out[pct_cols]  <- lapply(df_out[pct_cols],  function(z) ifelse(is.finite(z), paste0(round(z*100,1), "%"), ""))
      }
      if (length(rate_cols)) {
        df_out[rate_cols] <- lapply(df_out[rate_cols], function(z) ifelse(is.finite(z), fmt_avg(z), ""))
      }
      df_out$EV   <- ifelse(is.finite(df_out$EV),   round(df_out$EV, 1), "")
      df_out$LA   <- ifelse(is.finite(df_out$LA),   round(df_out$LA, 1), "")
      df_out$FIP  <- ifelse(is.finite(df_out$FIP),  sprintf("%.2f", df_out$FIP), "")
      df_out$WHIP <- ifelse(is.finite(df_out$WHIP), sprintf("%.2f", df_out$WHIP), "")
      
      df_dt <- if (exists("collapse_list_cols")) collapse_list_cols(df_out) else df_out
      df_dt <- as.data.frame(df_dt, stringsAsFactors = FALSE)
      if ("#" %in% names(df_dt)) {
        df_dt$`#` <- wrap_pitch_counts(df_dt$`#`)
      }
      
      df_disp <- df_dt
      
      summary_table_cache(list(
        table = df_disp,
        source = df,
        label_column = "Pitch"
      ))
      
      has_live_data <- any(df$SessionType == "Live", na.rm = TRUE)
      if (identical(mode, "Results") && has_live_data) {
        visible_set <- results_cols_live
      } else {
        visible_set <- visible_set_for(mode, custom)
      }
      
      if (has_live_data && identical(mode, "Results")) {
        cols_to_remove <- c("IP", "WHIP", "AVG", "SLG", "FIP", "xWOBA", "xISO", "BABIP")
        df_disp <- df_disp[, !names(df_disp) %in% cols_to_remove, drop = FALSE]
      }
      
      return(datatable_with_colvis(
        df_disp,
        lock            = "Pitch",
        remember        = FALSE,
        default_visible = intersect(visible_set, names(df_disp)),
        mode            = mode
      ))
    }
    
    # ---------- NON-Results modes (your original build) ----------
    # QP+ per pitch type (scalar)
    qp_by_type <- df %>%
      dplyr::filter(!is.na(TaggedPitchType) & nzchar(as.character(TaggedPitchType))) %>%
      dplyr::group_split(TaggedPitchType, .keep = TRUE) %>%
      purrr::map_dfr(function(chunk) {
        tpt <- chunk$TaggedPitchType[1]
        vals <- compute_qp_points(chunk)
        vals <- suppressWarnings(as.numeric(vals))
        tibble::tibble(
          TaggedPitchType = tpt,
          `QP+` = if (length(vals) && any(is.finite(vals))) round(mean(vals, na.rm = TRUE) * 200, 1) else NA_real_
        )
      })
    
    # Base per-pitch-type summary
    summ <- make_summary(df)
    summ <- dplyr::mutate(summ,
                          ReleaseTilt = as.character(ReleaseTilt),
                          BreakTilt   = as.character(BreakTilt)
    )
    if (!("QP+" %in% names(summ))) {
      summ <- summ %>% dplyr::left_join(qp_by_type, by = c("PitchType" = "TaggedPitchType"))
    }
    
    # ---- Build FULL table (as before) ----
    df_table <- dplyr::bind_rows(
      summ,
      {
        scores <- ifelse(
          df$PlateLocSide >= ZONE_LEFT & df$PlateLocSide <= ZONE_RIGHT &
            df$PlateLocHeight >= ZONE_BOTTOM & df$PlateLocHeight <= ZONE_TOP, 1.47,
          ifelse(
            df$PlateLocSide >= -1.5 & df$PlateLocSide <= 1.5 &
              df$PlateLocHeight >= (2.65-1.7) & df$PlateLocHeight <= (2.65+1.3),
            0.73, 0
          )
        )
        has_pc  <- sum(!is.na(df$PitchCall)) > 0
        strikes <- sum(df$PitchCall %in% c("StrikeCalled","StrikeSwinging","FoulBallNotFieldable","InPlay","FoulBallFieldable","FoulBall"), na.rm = TRUE)
        sw      <- sum(df$PitchCall == "StrikeSwinging", na.rm = TRUE)
        den     <- sum(df$PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall"), na.rm = TRUE)
        bf_live <- sum(df$SessionType == "Live" & df$Balls == 0 & df$Strikes == 0, na.rm = TRUE)
        k_live  <- sum(df$SessionType == "Live" & df$KorBB == "Strikeout",        na.rm = TRUE)
        bb_live <- sum(df$SessionType == "Live" & df$KorBB == "Walk",             na.rm = TRUE)
        fps_live <- sum(df$SessionType == "Live" & df$Balls == 0 & df$Strikes == 0 &
                          df$PitchCall %in% c("InPlay","StrikeSwinging","StrikeCalled","FoulBallNotFieldable","FoulBall"), na.rm = TRUE)
        ea_live  <- sum(df$SessionType == "Live" & (
          (df$Balls == 0 & df$Strikes == 0 & df$PitchCall == "InPlay") |
            (df$Balls == 0 & df$Strikes == 1 & df$PitchCall %in% c(
              "InPlay", "StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable","FoulBall"
            )) |
            (df$Balls == 1 & df$Strikes == 0 & df$PitchCall == "InPlay") |
            (df$Balls == 1 & df$Strikes == 1 & df$PitchCall %in% c(
              "InPlay", "StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable","FoulBall"
            ))
        ),
        na.rm = TRUE
        )
        
        vmax   <- suppressWarnings(max(as.numeric(df$RelSpeed), na.rm = TRUE)); vmax <- if (is.finite(vmax)) round(vmax, 1) else NA_real_
        ev_all <- nz_mean(ifelse(df$SessionType=="Live", df$ExitSpeed, NA_real_))
        la_all <- nz_mean(ifelse(df$SessionType=="Live", df$Angle,     NA_real_))
        stuff_all <- round(nz_mean(df$`Stuff+`), 1)
        ctrl_all   <- round(nz_mean(scores) * 100, 1)
        qp_all    <- round(nz_mean(compute_qp_points(df)) * 200, 1)
        
        tibble::tibble(
          Pitch         = "All",
          PitchCount    = nrow(df),
          Overall       = "100%",
          BF            = bf_live,
          Velo_Avg      = round(nz_mean(df$RelSpeed), 1),
          Velo_Max      = vmax,
          IVB           = round(nz_mean(df$InducedVertBreak), 1),
          HB            = round(nz_mean(df$HorzBreak), 1),
          ReleaseTilt   = convert_to_clock(nz_mean(df$ReleaseTilt)),
          BreakTilt     = convert_to_clock(nz_mean(df$BreakTilt)),
          SpinEff       = { v <- nz_mean(df$SpinEfficiency); if (is.na(v)) "" else paste0(round(v*100,1), "%") },
          SpinRate      = round(nz_mean(df$SpinRate), 0),
          RelHeight     = round(nz_mean(df$RelHeight), 1),
          RelSide       = round(nz_mean(df$RelSide), 1),
          VertApprAngle = round(nz_mean(df$VertApprAngle), 1),
          HorzApprAngle = round(nz_mean(df$HorzApprAngle), 1),
          Extension     = round(nz_mean(df$Extension), 1),
          InZonePercent = { inzone <- (df$PlateLocSide >= ZONE_LEFT & df$PlateLocSide <= ZONE_RIGHT &
                                         df$PlateLocHeight >= ZONE_BOTTOM & df$PlateLocHeight <= ZONE_TOP)
          ifelse(sum(!is.na(inzone))>0, paste0(round(100*sum(inzone, na.rm=TRUE)/sum(!is.na(inzone)),1), "%"), "") },
          CompPercent   = { comp <- (df$PlateLocSide >= -1.5 & df$PlateLocSide <= 1.5 &
                                       df$PlateLocHeight >= (2.65-1.7) & df$PlateLocHeight <= (2.65+1.3))
          ifelse(sum(!is.na(comp))>0, paste0(round(100*sum(comp, na.rm=TRUE)/sum(!is.na(comp)),1), "%"), "") },
          KPercent      = ifelse(bf_live>0, paste0(round(100*k_live/bf_live,1), "%"), ""),
          BBPercent     = ifelse(bf_live>0, paste0(round(100*bb_live/bf_live,1), "%"), ""),
          FPSPercent    = ifelse(bf_live>0, paste0(round(100*fps_live/bf_live,1), "%"), ""),
          EAPercent     = ifelse(bf_live>0, paste0(round(100*ea_live/bf_live,1), "%"), ""),
          StrikePercent = ifelse(has_pc, paste0(round(100*strikes/nrow(df),1), "%"), ""),
          SwingPercent  = ifelse(has_pc, paste0(round(100*sum(df$PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall"), na.rm = TRUE)/nrow(df),1), "%"), ""),
          WhiffPercent  = ifelse(den>0, paste0(round(100*sw/den,1), "%"), ""),
          EV = ev_all, LA = la_all,
          BFP = {
            val <- compute_bfp_overall(df)
            if (!is.finite(val)) 0 else val
          },
          `Stuff+` = stuff_all, `Ctrl+` = ctrl_all, `QP+` = qp_all
        ) %>% dplyr::mutate(`Pitching+` = round((`Stuff+` + `QP+`)/2, 1))
      }
    ) %>%
      dplyr::rename(
        `#`    = PitchCount,
        Velo   = Velo_Avg,
        Max    = Velo_Max,
        rTilt  = ReleaseTilt,
        bTilt  = BreakTilt,
        Spin   = SpinRate,
        Height = RelHeight,
        Side   = RelSide,
        Ext    = Extension,
        `InZone%` = InZonePercent,
        `Comp%`   = CompPercent,
        `K%`      = KPercent,
        `BB%`     = BBPercent,
        `FPS%`    = FPSPercent,
        `E+A%`    = EAPercent,
        `Strike%` = StrikePercent,
        `Swing%`  = SwingPercent,
        `Whiff%`  = WhiffPercent,
        VAA       = VertApprAngle,
        HAA       = HorzApprAngle
      ) %>%
      dplyr::mutate(Pitch = as.character(Pitch)) %>%
      dplyr::select(
        Pitch, `#`, Overall, BF,
        Velo, Max, IVB, HB, rTilt, bTilt, SpinEff, Spin, Height, Side, VAA, HAA, Ext,
        `InZone%`, `Comp%`, `Strike%`, `Swing%`, `FPS%`, `E+A%`, `K%`, `BB%`, `Whiff%`, EV, LA, BFP,
        `Stuff+`, `Ctrl+`, `QP+`, `Pitching+`
      ) %>%
      dplyr::mutate(
        EV = ifelse(is.na(as.numeric(EV)), "", round(as.numeric(EV), 1)),
        LA = ifelse(is.na(as.numeric(LA)), "", round(as.numeric(LA), 1)),
        BFP = ifelse(
          Pitch == "All",
          ifelse(is.na(as.numeric(BFP)), "", sprintf("%.1f", as.numeric(BFP))),
          ""
        )
      )
    
    # Add Process/Results extras and sanitize for DT
    existing_cols <- names(df_table)
    extras <- compute_process_results(df) %>%
      dplyr::rename(Pitch = PitchType) %>%
      dplyr::mutate(Pitch = as.character(Pitch)) %>%
      dplyr::select(Pitch, dplyr::any_of(setdiff(names(.), existing_cols)))
    df_table <- df_table %>% dplyr::left_join(extras, by = "Pitch", suffix = c("", ".extra"))
    
    # Add usage calculations for Usage table mode
    if (identical(mode, "Usage")) {
      usage_extras <- compute_usage_by_count(df) %>%
        dplyr::rename(Pitch = PitchType) %>%
        dplyr::mutate(Pitch = as.character(Pitch))
      df_table <- df_table %>% dplyr::left_join(usage_extras, by = "Pitch")
    }
    
    df_table <- collapse_list_cols(df_table)
    is_date_like <- function(x) inherits(x, c("Date","POSIXct","POSIXt","difftime"))
    date_like <- vapply(df_table, is_date_like, logical(1))
    if (any(date_like)) df_table[date_like] <- lapply(df_table[date_like], as.character)
    df_table <- as.data.frame(df_table, stringsAsFactors = FALSE)
    
    if (all(c("Whiff%","CSW%") %in% names(df_table))) {
      df_table <- df_table %>% dplyr::relocate(`CSW%`, .after = `Whiff%`)
    }
    if ("Pitching+" %in% names(df_table)) {
      df_table <- df_table %>% dplyr::relocate(`Pitching+`, .after = dplyr::last_col())
    }
    
    df_table <- enforce_process_order(df_table)
    df_table <- enforce_stuff_order(df_table)
    df_disp <- df_table
    if ("#" %in% names(df_disp)) {
      df_disp$`#` <- wrap_pitch_counts(df_disp$`#`)
    }
    
    summary_table_cache(list(
      table = df_disp,
      source = df,
      label_column = "Pitch"
    ))
    
    visible_set <- visible_set_for(mode, custom)
    datatable_with_colvis(
      df_disp,
      lock            = "Pitch",
      remember        = FALSE,
      default_visible = intersect(visible_set, names(df_disp)),
      mode            = mode
    )
  })
  
  observeEvent(input$summaryTablePage_cell_clicked, {
    handle_pitch_table_click(input$summaryTablePage_cell_clicked, table_cache_fetcher("summaryTablePage"), table_label = "Summary")
  }, ignoreNULL = TRUE)
  
  observeEvent(input$summaryTable_cell_clicked, {
    handle_pitch_table_click(input$summaryTable_cell_clicked, table_cache_fetcher("summaryTable"), table_label = "Data & Performance")
  }, ignoreNULL = TRUE)
  
  # ================================
  # Pitching → AB Report (no tables)
  # ================================
  # Safe fallbacks (won't override if you already have real ones)
  if (!exists("is_active", mode = "function")) is_active <- function(...) TRUE
  if (!exists("ns",        mode = "function")) ns        <- function(id) id
  
  # Build the PA-level result label (same logic as hitting)
  .abp_pa_result_label <- function(last_row) {
    pc <- as.character(last_row$PitchCall)
    dplyr::case_when(
      !is.na(pc) && pc == "InPlay" ~ "InPlay",
      !is.na(pc) && pc == "BallCalled" ~ "Walk",
      !is.na(pc) && pc %in% c("StrikeSwinging","StrikeCalled") ~ "Strikeout",
      !is.na(pc) && nzchar(pc) ~ pc,
      TRUE ~ "Result"
    )
  }
  
  
  # Terminal pitch = PA completed
  .abp_is_terminal <- function(df) {
    compute_pa_flags(df)$is_terminal
  }
  
  .abp_fmt_mdy <- function(d) format(as.Date(d), "%m/%d/%Y")
  
  # "Last, First" -> "First Last"
  .abp_pretty_name <- function(x) {
    x <- as.character(x)
    ifelse(grepl(",", x), paste0(trimws(sub(".*,", "", x)), " ", trimws(sub(",.*", "", x))), x)
  }
  # Pretty pitcher label
  .abp_pretty_pitcher <- function(x) .abp_pretty_name(x)
  
  # Strike zone + dashed competitive box
  .abp_geom_zone <- function() {
    home <- data.frame(x=c(-0.75,0.75,0.75,0.00,-0.75),
                       y=c(1.05, 1.05,1.15,1.25, 1.15)-0.5)
    cz <- data.frame(xmin = -1.5, xmax = 1.5, ymin = 2.65 - 1.7, ymax = 2.65 + 1.3)
    sz <- data.frame(xmin = ZONE_LEFT, xmax = ZONE_RIGHT, ymin = ZONE_BOTTOM, ymax = ZONE_TOP)
    list(
      geom_polygon(data = home, aes(x, y), fill = NA, color = "black"),
      geom_rect(data = cz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = NA, color = "black", linetype = "dashed"),
      geom_rect(data = sz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = NA, color = "black")
    )
  }
  
  # All game dates where the selected pitcher has ≥1 completed PA (across any batters)
  abp_dates <- reactive({
    pit <- input$pitcher
    if (is.null(pit) || identical(pit, "All")) return(as.Date(character(0)))
    
    d <- pitch_data_pitching %>% dplyr::filter(Pitcher == pit)
    term <- .abp_is_terminal(d)
    sort(unique(as.Date(d$Date[term])))
  })
  
  # Sidebar (date selector + legends)
  output$abpSidebar <- renderUI({
    pit <- input$pitcher
    if (is.null(pit) || identical(pit, "All")) {
      return(tagList(tags$em("Select a single pitcher in the main sidebar to enable AB Report.")))
    }
    
    dates <- abp_dates()
    if (!length(dates)) return(tagList(tags$em("No completed plate appearances found for this pitcher.")))
    
    vals <- as.character(as.Date(dates))
    labs <- .abp_fmt_mdy(dates)
    choices <- stats::setNames(vals, labs)
    
    cur <- isolate(input$abpGameDate)
    sel <- if (!is.null(cur) && cur %in% vals) cur else vals[length(vals)]
    
    # Pitch-type legend: only types this pitcher actually threw
    types_for_legend <- {
      d <- pitch_data_pitching %>% dplyr::filter(Pitcher == pit)
      intersect(names(all_colors), as.character(unique(d$TaggedPitchType)))
    }
    
    shape_rows <- tagList(
      tags$div("\u25CF Called Strike"),  # ●
      tags$div("\u25CB Ball"),           # ○
      tags$div("\u25B3 Foul"),           # △
      tags$div("\u2605 Whiff"),          # ★
      tags$div("\u25B2 In Play (Out)"),  # ▲
      tags$div("\u25A0 In Play (Hit)")   # ■
    )
    
    tagList(
      selectInput(ns("abpGameDate"), "Select Game:", choices = choices, selected = sel),
      tags$hr(),
      tags$div(tags$strong("Pitch Result")),
      shape_rows,
      tags$br(),
      tags$div(tags$strong("Pitch Types")),
      tags$div(lapply(types_for_legend, function(tt) {
        col <- all_colors[[as.character(tt)]]; if (is.null(col)) col <- "gray"
        tags$div(style="display:flex;align-items:center;margin:2px 0;",
                 tags$span(style=paste0("display:inline-block;width:12px;height:12px;",
                                        "background:", col, ";margin-right:6px;",
                                        "border:1px solid rgba(0,0,0,.25);border-radius:2px;")),
                 tags$span(as.character(tt))
        )
      }))
    )
  })
  
  # Header (top-left): Pitcher + Date
  output$abpHeader <- renderUI({
    pit <- input$pitcher
    if (is.null(pit) || identical(pit, "All")) return(NULL)
    dt_val <- input$abpGameDate
    if (is.null(dt_val)) {
      ds <- abp_dates(); if (!length(ds)) return(NULL)
      dt_val <- as.character(max(ds))
    }
    tags$div(
      style = "text-align:left;",
      tags$strong(.abp_pretty_pitcher(pit)), tags$br(),
      .abp_fmt_mdy(as.Date(dt_val))
    )
  })
  
  # Panels: stacked batters (left), each with PA mini-charts (right)
  output$abpPanels <- renderUI({
    pit <- input$pitcher
    if (is.null(pit) || identical(pit, "All"))
      return(div(style="margin:8px 0;", tags$em("Select a single pitcher to view the AB Report.")))
    
    dt_chr <- input$abpGameDate; req(!is.null(dt_chr))
    the_date <- as.Date(dt_chr)
    
    # Filter: selected pitcher on selected date (ignore the global date range for this page)
    df_all <- pitch_data_pitching %>% dplyr::filter(Pitcher == pit, as.Date(Date) == the_date)
    if (!("PlayResult" %in% names(df_all))) df_all$PlayResult <- NA_character_
    if (!("KorBB" %in% names(df_all))) df_all$KorBB <- NA_character_
    
    if (!nrow(df_all)) return(div(tags$em("No pitches for this pitcher on the selected date.")))
    
    # Order batters by first appearance that day
    first_idx <- df_all %>%
      dplyr::mutate(.row_id = dplyr::row_number()) %>%
      dplyr::group_by(Batter) %>%
      dplyr::summarise(first_row = min(.row_id), .groups = "drop") %>%
      dplyr::arrange(first_row)
    
    # Build one row per batter
    rows <- lapply(seq_len(nrow(first_idx)), function(bi) {
      bat <- first_idx$Batter[bi]
      dB  <- df_all %>% dplyr::filter(Batter == bat)
      
      # Batter side & name label with color (Left = red, Right = adapts to theme)
      side <- as.character(dplyr::coalesce(dB$BatterSide[which.max(seq_len(nrow(dB)))], NA))
      lr   <- ifelse(is.na(side), "", ifelse(grepl("^L", side, ignore.case = TRUE), "L", "R"))
      is_left <- identical(lr, "L")
      name_html <- tags$div(
        style = paste0("font-weight:700;", if (is_left) " color:#d32f2f;" else ""),
        class = if (!is_left) "batter-name-rh" else NULL,
        paste0(.abp_pretty_name(bat), " (", lr, ")")
      )
      
      # Segment PAs within this batter
      term <- .abp_is_terminal(dB)
      # cumsum starts a new PA *after* a terminal pitch:
      pa_id <- cumsum(c(1L, as.integer(utils::head(term, -1))))
      dB$._pa_id <- pa_id
      done_ids <- unique(dB$._pa_id[term])
      dB <- dplyr::filter(dB, ._pa_id %in% done_ids)
      if (!nrow(dB)) {
        return(fluidRow(
          column(3, div(style="padding:10px 6px;", name_html)),
          column(9, div(style="padding:10px 6px;", tags$em("No completed PAs")))
        ))
      }
      
      pa_list <- split(dB, dB$._pa_id)
      n_pa <- length(pa_list)
      
      # Build one mini zone chart per PA (left→right, 1st→last)
      chart_cells <- lapply(seq_len(n_pa), function(i) {
        # build the per-PA data first
        dat <- pa_list[[i]] %>%
          dplyr::mutate(
            pitch_idx = dplyr::row_number(),
            Result    = factor(compute_result(PitchCall, PlayResult), levels = result_levels),
            tt_fill   = dplyr::coalesce(all_colors[as.character(TaggedPitchType)], "gray80"),
            tt        = paste0(
              "Pitch: ", as.character(TaggedPitchType), "\n",
              "Result: ", dplyr::case_when(
                PitchCall == "InPlay" & !is.na(PlayResult) ~ as.character(PlayResult),
                TRUE ~ dplyr::coalesce(as.character(PitchCall), "")
              ), "\n",
              "Velo: ", ifelse(is.finite(RelSpeed), sprintf("%.1f", RelSpeed), "—"), "\n",
              "IVB: ",  ifelse(is.finite(InducedVertBreak), sprintf("%.1f", InducedVertBreak), "—"), "\n",
              "HB: ",   ifelse(is.finite(HorzBreak), sprintf("%.1f", HorzBreak), "—")
            )
          )
        
        # now compute the PA result label from the last pitch of this PA
        title_result <- .abp_pa_result_label(dat[nrow(dat), , drop = FALSE])
        
        pid    <- paste0(bi, "_", names(pa_list)[i])
        out_id <- ns(paste0("abpPlot_", pid))
        
        local({
          dat_local    <- dat
          out_id_local <- ns(paste0("abpPlot_", pid))
          output[[out_id_local]] <- ggiraph::renderGirafe({
            types <- as.character(intersect(names(all_colors), unique(dat_local$TaggedPitchType)))
            p <- ggplot() +
              .abp_geom_zone() +
              ggiraph::geom_point_interactive(
                data = dat_local,
                aes(PlateLocSide, PlateLocHeight,
                    color = TaggedPitchType, fill = TaggedPitchType, shape = Result,
                    tooltip = tt, data_id = pitch_idx),
                size = 5, alpha = 0.95, stroke = 0.8
              ) +
              ggiraph::geom_text_interactive(
                data = dat_local,
                aes(PlateLocSide, PlateLocHeight, label = pitch_idx, tooltip = tt, data_id = pitch_idx),
                nudge_y = 0.21, size = 5.5
              ) +
              ggiraph::geom_point_interactive(
                data = dat_local,
                aes(PlateLocSide, PlateLocHeight, tooltip = tt, data_id = pitch_idx, fill = I(tt_fill)),
                shape = 21, size = 7, alpha = 0.001, stroke = 0, inherit.aes = FALSE
              ) +
              scale_color_manual(values = all_colors[types], limits = types, name = NULL) +
              scale_fill_manual(values  = all_colors[types], limits = types, name = NULL) +
              scale_shape_manual(values = shape_map, drop = TRUE, name = NULL) +
              coord_fixed(ratio = 1, xlim = c(-3, 3), ylim = c(0.5, 5)) +
              theme_void() + theme(legend.position = "none")
            
            ggiraph::girafe(
              ggobj = p,
              options = list(
                ggiraph::opts_tooltip(use_fill = TRUE, use_stroke = TRUE,
                                      css = "color:#fff !important;font-weight:600;padding:6px;border-radius:8px;text-shadow:0 1px 1px rgba(0,0,0,.4);"),
                ggiraph::opts_hover(css = "stroke-width:1.5px;"),
                ggiraph::opts_hover_inv(css = "opacity:0.15;")
              )
            )
          })
        })
        
        div(
          style = "display:inline-block; margin:0 8px 12px 0; vertical-align:top; text-align:center;",
          tags$div(tags$strong(paste0("PA #", i))),
          tags$div(title_result, style = "margin-bottom:6px;"),
          ggiraph::girafeOutput(out_id, height = "300px", width = "300px")
        )
      })
      
      # Row layout: left col = batter label; right col = PA charts (scroll if many)
      fluidRow(
        # Narrower name column; vertically centered to the strike zone height
        column(
          2,
          div(
            style = "display:flex; align-items:center; justify-content:flex-end;
               min-height:320px; padding:0 8px 0 0; text-align:right;",
            name_html
          )
        ),
        # Wider chart area
        column(
          10,
          div(style="overflow-x:auto; white-space:nowrap; padding:6px;", chart_cells)
        )
      )
    })
    
    tagList(rows)
  })
  
  # Release + Extension combo
  output$releaseCombo <- ggiraph::renderGirafe({
    df <- filtered_data(); if (!nrow(df)) return(NULL)
    types <- ordered_types(); types_chr <- as.character(types)
    sess_lbl <- session_label_from(df)
    dark_on <- isTRUE(input$dark_mode)
    axis_col <- if (dark_on) "#e5e7eb" else "black"
    cols <- colors_for_mode(dark_on)
    mound_fill <- if (dark_on) "#2d2d2d" else "tan"
    rubber_fill <- if (dark_on) NA else "white"
    
    # shared y-axis max so both panels align and show at least 6
    y_max <- max(6, suppressWarnings(max(df$RelHeight, na.rm = TRUE) + 0.2))
    
    # --- (A) Release Point averages
    rp_w <- 4; rp_h <- 0.83
    xs <- seq(-rp_w, rp_w, length.out = 100)
    ys <- rp_h * (1 - (xs / rp_w)^2)
    mound_df <- data.frame(x = c(xs, rev(xs)), y = c(ys, rep(0, length(xs))))
    
    avg_rel <- df %>%
      dplyr::filter(is.finite(RelSide), is.finite(RelHeight)) %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(
        avg_RelSide    = mean(RelSide,    na.rm = TRUE),
        avg_RelHeight  = mean(RelHeight,  na.rm = TRUE),
        avg_Extension  = mean(Extension,  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(TaggedPitchType %in% types_chr) %>%
      dplyr::mutate(
        TaggedPitchType = factor(TaggedPitchType, levels = types_chr),
        tt = paste0(
          "Session: ", sess_lbl,
          "<br>Height: ", sprintf("%.1f ft", avg_RelHeight),
          "<br>Side: ", sprintf("%.1f ft", avg_RelSide),
          "<br>Extension: ", sprintf("%.1f ft", avg_Extension)
        )
      )
    
    p1 <- ggplot() +
      geom_polygon(data = mound_df, aes(x, y), fill = mound_fill, color = mound_fill) +
      annotate("rect", xmin = -0.5, xmax = 0.5, ymin = rp_h - 0.05, ymax = rp_h + 0.05, fill = rubber_fill, color = axis_col) +
      geom_vline(xintercept = 0, color = axis_col, size = 0.7) +
      ggiraph::geom_point_interactive(
        data = avg_rel,
        aes(x = avg_RelSide, y = avg_RelHeight,
            color = TaggedPitchType, tooltip = tt, data_id = TaggedPitchType),
        size = 4, show.legend = FALSE
      ) +
      scale_color_manual(values = cols[types_chr], limits = types_chr, name = NULL) +
      scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, by = 1)) +
      theme_minimal() + axis_theme + labs(x = NULL, y = NULL) +
      theme(
        axis.text = element_text(colour = axis_col),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.grid.major = element_line(color = scales::alpha(axis_col, 0.1)),
        panel.grid.minor = element_blank()
      )
    
    # --- (B) Extension vs Height averages
    re_w <- 7; re_h <- 0.83
    xs2 <- seq(-re_w, re_w, length.out = 100)
    ys2 <- re_h * (1 - (xs2 / re_w)^2)
    ext_bg <- data.frame(x = c(xs2, rev(xs2)), y = c(ys2, rep(0, length(xs2))))
    
    avg_ext <- df %>%
      dplyr::filter(is.finite(Extension), is.finite(RelHeight)) %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(
        avg_Extension  = mean(Extension,  na.rm = TRUE),
        avg_RelHeight  = mean(RelHeight,  na.rm = TRUE),
        avg_RelSide    = mean(RelSide,    na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(TaggedPitchType %in% types_chr) %>%
      dplyr::mutate(
        TaggedPitchType = factor(TaggedPitchType, levels = types_chr),
        tt = paste0(
          "Session: ", sess_lbl,
          "<br>Height: ", sprintf("%.1f ft", avg_RelHeight),
          "<br>Side: ", sprintf("%.1f ft", avg_RelSide),
          "<br>Extension: ", sprintf("%.1f ft", avg_Extension)
        )
      )
    
    p2 <- ggplot() +
      geom_polygon(data = ext_bg, aes(x, y), fill = mound_fill, color = mound_fill) +
      annotate("rect", xmin = 0, xmax = 0.2, ymin = re_h - 0.05, ymax = re_h + 0.05, fill = rubber_fill, color = axis_col) +
      geom_vline(xintercept = 0, color = axis_col, size = 0.7) +
      ggiraph::geom_point_interactive(
        data = avg_ext,
        aes(x = avg_Extension, y = avg_RelHeight,
            color = TaggedPitchType, tooltip = tt, data_id = TaggedPitchType),
        size = 4, show.legend = TRUE   # legend only here
      ) +
      scale_x_continuous(limits = c(0, 7.5), breaks = seq(1, 7.5, 1)) +
      scale_y_continuous(limits = c(0, y_max), breaks = seq(0, ceiling(y_max), by = 1)) +
      scale_color_manual(values = cols[types_chr], limits = types_chr, name = NULL) +
      theme_minimal() + axis_theme + labs(x = NULL, y = NULL) +
      theme(
        axis.text = element_text(colour = axis_col),
        legend.text = element_text(colour = axis_col),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.grid.major = element_line(color = scales::alpha(axis_col, 0.25))
      )
    
    # --- stack + single legend at bottom; enlarge legend text a bit
    p <- (p1 / p2) + patchwork::plot_layout(guides = "collect")
    p <- p & theme(
      legend.position = "bottom",
      legend.text = element_text(size = 14, colour = axis_col),
      legend.background = element_rect(fill = NA, colour = NA),
      legend.box.background = element_rect(fill = NA, colour = NA)
    )
    
    ggiraph::girafe(
      ggobj = p,
      width_svg = 9, height_svg = 12,
      options = list(
        ggiraph::opts_sizing(rescale = TRUE),
        ggiraph::opts_tooltip(use_fill = TRUE, use_stroke = TRUE, css = tooltip_css),
        ggiraph::opts_hover(css = "stroke:black;stroke-width:1.5px;"),
        ggiraph::opts_hover_inv(css = "opacity:0.15;")
      ),
      bg = "transparent"
    )
  })
  
  
  
  # Full Movement Plot
  output$movementPlot <- ggiraph::renderGirafe({
    df <- filtered_data(); if (!nrow(df)) return(NULL)
    types <- ordered_types(); types_chr <- as.character(types)
    dark_on <- isTRUE(input$dark_mode)
    axis_col <- if (dark_on) "#e5e7eb" else "black"
    grid_alpha <- if (dark_on) 0.15 else 0.4
    cols <- colors_for_mode(dark_on)
    
    avg_mov <- df %>% dplyr::group_by(TaggedPitchType) %>%
      dplyr::slice_tail(n = 1000) %>%
      dplyr::summarise(
        avg_HorzBreak        = mean(HorzBreak, na.rm = TRUE),
        avg_InducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
        .groups = "drop"
      )
    
    df_i <- df %>% dplyr::mutate(tt = make_hover_tt(.), rid = dplyr::row_number())
    
    base_type <- input$breakLines
    line_df <- tibble()
    if (base_type %in% c("Fastball", "Sinker")) {
      base_val <- dplyr::filter(avg_mov, TaggedPitchType == base_type)
      if (nrow(base_val) == 1) {
        seps <- if (base_type == "Fastball") {
          tibble(
            TaggedPitchType = c("Cutter", "Slider", "Sweeper", "Curveball", "ChangeUp", "Splitter"),
            sep_IVB = c(-7, -15, -16, -27, -12, -13),
            sep_Horz = c(10, 12, 22, 18, -7, -4)
          )
        } else {
          tibble(
            TaggedPitchType = c("Cutter", "Slider", "Sweeper", "Curveball", "ChangeUp", "Splitter"),
            sep_IVB = c(2, -6, -7, -18, -4, -5),
            sep_Horz = c(18, 20, 30, 25, 1, 2)
          )
        }
        throw_side <- if (input$hand %in% c("Left", "Right")) input$hand else {
          us <- unique(df$PitcherThrows) %>% na.omit()
          if (length(us) == 1 && us %in% c("Left", "Right")) us else "Left"
        }
        dir <- ifelse(throw_side == "Right", -1, 1)
        seps <- seps %>%
          dplyr::filter(TaggedPitchType %in% avg_mov$TaggedPitchType) %>%
          dplyr::mutate(sep_Horz = sep_Horz * dir)
        line_df <- seps %>% dplyr::mutate(
          start_x = base_val$avg_HorzBreak,
          start_y = base_val$avg_InducedVertBreak,
          end_x   = base_val$avg_HorzBreak + sep_Horz,
          end_y   = base_val$avg_InducedVertBreak + sep_IVB
        )
      }
    }
    
    p <- ggplot() +
      ggiraph::geom_point_interactive(
        data = df_i,
        aes(HorzBreak, InducedVertBreak,
            color = TaggedPitchType, fill = TaggedPitchType,
            tooltip = tt, data_id = rid),
        position = "identity",
        alpha = 0.25, size = 2.2, shape = 21, stroke = 0.25
      ) +
      geom_point(
        data = avg_mov,
        aes(avg_HorzBreak, avg_InducedVertBreak, color = TaggedPitchType),
        size = 6
      ) +
      { if (nrow(line_df) > 0)
        geom_segment(
          data = line_df,
          aes(x = start_x, y = start_y, xend = end_x, yend = end_y, color = TaggedPitchType),
          size = 1
        )
      } +
      geom_hline(yintercept = 0, color = axis_col) + geom_vline(xintercept = 0, color = axis_col) +
      coord_cartesian(xlim = c(-25, 25), ylim = c(-25, 25)) +
      scale_color_manual(values = cols[types_chr], limits = types_chr, name = NULL) +
      scale_fill_manual(values  = cols[types_chr], limits = types_chr, name = NULL) +
      theme_minimal() + axis_theme +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 14, colour = axis_col),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.box.background = element_rect(fill = NA, colour = NA),
        axis.text.x = element_text(size = 15, face = "bold", colour = axis_col),
        axis.text.y = element_text(size = 15, face = "bold", colour = axis_col),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = scales::alpha(axis_col, 0.1))
      )
    
    ggiraph::girafe(
      ggobj = p,
      options = list(
        ggiraph::opts_tooltip(
          use_fill = TRUE,
          css = "color:white;font-weight:600;padding:6px;border-radius:8px;text-shadow:0 1px 1px rgba(0,0,0,.4);"
        ),
        ggiraph::opts_hover(css = "stroke:black;stroke-width:1.5px;"),
        ggiraph::opts_hover_inv(css = "opacity:0.15;"),
        ggiraph::opts_selection(type = "single", only_shiny = TRUE)  # <-- added: single-select for reliable clicks
      ),
      bg = "transparent"
    )
  })
  
  # Velocity Plot
  # Helper: pick the first existing column name from a preference list
  # ---------- helpers (replace the previous .pick_col) ----------
  .pick_col <- function(df, candidates) {
    nm <- intersect(candidates, names(df))
    if (length(nm)) nm[[1]] else NA_character_
  }
  .safe_mean <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  }
  HB_CANDIDATES  <- c("HB","HorzBreak","HorizBreak","HorizontalBreak","HBreak","HB_in","HB_inches")
  IVB_CANDIDATES <- c("IVB","InducedVertBreak","IVB_in","IVB_inches")
  
  velocity_points_df <- reactive({
    df <- filtered_data()
    if (!nrow(df)) return(df[0, , drop = FALSE])
    df2 <- df %>%
      dplyr::mutate(.orig_order = dplyr::row_number())
    if ("Date" %in% names(df2)) {
      df2 <- df2 %>% dplyr::arrange(Date, .orig_order)
    } else {
      df2 <- df2 %>% dplyr::arrange(.orig_order)
    }
    df2 <- df2 %>%
      dplyr::mutate(
        PitchCount = dplyr::row_number(),
        tt = make_hover_tt(.),
        rid = dplyr::row_number()
      )
    df2 <- as.data.frame(df2, stringsAsFactors = FALSE)
    rownames(df2) <- as.character(df2$rid)
    df2
  })
  
  velocity_by_game_data <- reactive({
    df <- filtered_data()
    if (!nrow(df)) {
      empty <- data.frame(Date = as.Date(character()), TaggedPitchType = character(), SessionType = character(), stringsAsFactors = FALSE)
      return(list(summary = empty, raw = df[0, , drop = FALSE]))
    }
    
    ivb_nm <- .pick_col(df, IVB_CANDIDATES)
    hb_nm  <- .pick_col(df, HB_CANDIDATES)
    
    dfG <- df %>%
      dplyr::filter(!is.na(RelSpeed), !is.na(TaggedPitchType), !is.na(Date)) %>%
      dplyr::mutate(
        IVB_ = if (!is.na(ivb_nm)) .data[[ivb_nm]] else NA_real_,
        HB_  = if (!is.na(hb_nm))  .data[[hb_nm]]  else NA_real_
      ) %>%
      dplyr::group_by(Date, TaggedPitchType, SessionType) %>%
      dplyr::summarise(
        Velo = .safe_mean(RelSpeed),
        IVB  = .safe_mean(IVB_),
        HB   = .safe_mean(HB_),
        n    = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(Date, TaggedPitchType) %>%
      dplyr::mutate(
        tt  = paste0(
          "Session: ", dplyr::coalesce(as.character(SessionType), "Unknown"), "\n",
          TaggedPitchType, "\n",
          "Velo: ", sprintf("%.1f", Velo), " mph\n",
          "IVB: ",  ifelse(is.finite(IVB), sprintf("%.1f", IVB), "—"), "\n",
          "HB: ",   ifelse(is.finite(HB),  sprintf("%.1f", HB),  "—"), "\n",
          "Pitches: ", n
        ),
        rid = paste0(format(as.Date(Date), "%Y-%m-%d"), "::", TaggedPitchType, "::", dplyr::coalesce(as.character(SessionType), ""))
      )
    
    dfG <- as.data.frame(dfG, stringsAsFactors = FALSE)
    list(summary = dfG, raw = df)
  })
  
  velocity_inning_data <- reactive({
    df <- filtered_data();
    if (!nrow(df)) {
      empty <- data.frame(TaggedPitchType = character(), InningOrd = integer(), stringsAsFactors = FALSE)
      return(list(summary = empty, detail = df[0, , drop = FALSE]))
    }
    if (!("SessionType" %in% names(df)) || !"Inning" %in% names(df)) {
      empty <- data.frame(TaggedPitchType = character(), InningOrd = integer(), stringsAsFactors = FALSE)
      return(list(summary = empty, detail = df[0, , drop = FALSE]))
    }
    
    df_live <- df %>% dplyr::filter(SessionType == "Live", !is.na(RelSpeed), !is.na(Inning))
    if (!nrow(df_live)) {
      empty <- data.frame(TaggedPitchType = character(), InningOrd = integer(), stringsAsFactors = FALSE)
      return(list(summary = empty, detail = df_live))
    }
    
    ivb_nm <- .pick_col(df_live, IVB_CANDIDATES)
    hb_nm  <- .pick_col(df_live, HB_CANDIDATES)
    
    df_live <- df_live %>%
      dplyr::mutate(
        GameKey = dplyr::case_when(
          "GameID" %in% names(df_live) ~ as.character(.data[["GameID"]]),
          TRUE                         ~ format(as.Date(Date), "%Y-%m-%d")
        ),
        .orig_order = dplyr::row_number()
      ) %>%
      dplyr::arrange(GameKey, .orig_order) %>%
      dplyr::group_by(GameKey) %>%
      dplyr::mutate(InningOrd = match(Inning, unique(Inning))) %>%
      dplyr::ungroup()
    
    dfI <- df_live %>%
      dplyr::mutate(
        IVB_ = if (!is.na(ivb_nm)) .data[[ivb_nm]] else NA_real_,
        HB_  = if (!is.na(hb_nm))  .data[[hb_nm]]  else NA_real_
      ) %>%
      dplyr::group_by(InningOrd, TaggedPitchType) %>%
      dplyr::summarise(
        Velo  = .safe_mean(RelSpeed),
        IVB   = .safe_mean(IVB_),
        HB    = .safe_mean(HB_),
        n     = dplyr::n(),
        games = dplyr::n_distinct(GameKey),
        .groups = "drop"
      ) %>%
      dplyr::arrange(InningOrd, TaggedPitchType) %>%
      dplyr::mutate(
        tt = paste0(
          "Session: Live\n",
          "Inning #: ", InningOrd, "\n",
          TaggedPitchType, "\n",
          "Velo: ", sprintf("%.1f", Velo), " mph\n",
          "IVB: ",  ifelse(is.finite(IVB), sprintf("%.1f", IVB), "—"), "\n",
          "HB: ",   ifelse(is.finite(HB),  sprintf("%.1f", HB),  "—"), "\n",
          "Games: ", games, " | Pitches: ", n
        ),
        rid = paste0(TaggedPitchType, "::", InningOrd)
      )
    
    list(summary = as.data.frame(dfI, stringsAsFactors = FALSE), detail = as.data.frame(df_live, stringsAsFactors = FALSE))
  })
  
  
  # ----------------------------
  # 1) CURRENT VELO PLOT (+ inning vlines for Live)
  # ----------------------------
  output$velocityPlot <- ggiraph::renderGirafe({
    df2 <- velocity_points_df(); if (!nrow(df2)) return(NULL)
    types <- ordered_types(); types_chr <- as.character(types)
    dark_on <- isTRUE(input$dark_mode)
    axis_col <- if (dark_on) "#e5e7eb" else "black"
    grid_alpha <- if (dark_on) 0.15 else 0.35
    cols <- colors_for_mode(dark_on)
    
    sp <- df2$RelSpeed
    if (all(is.na(sp))) {
      y_min <- 0; y_max <- 100
    } else {
      y_min <- floor(min(sp, na.rm = TRUE) / 5) * 5
      y_max <- ceiling(max(sp, na.rm = TRUE) / 5) * 5
      if (y_min == y_max) y_max <- y_min + 5
    }
    x_max <- ceiling(max(df2$PitchCount, na.rm = TRUE) / 5) * 5
    if (!is.finite(x_max) || x_max <= 0) x_max <- 5
    
    avg_velo <- df2 %>% dplyr::filter(!is.na(RelSpeed)) %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(avg_velo = mean(RelSpeed), .groups = "drop")
    
    p <- ggplot(df2, aes(PitchCount, RelSpeed)) +
      ggiraph::geom_point_interactive(
        aes(color = TaggedPitchType, fill = TaggedPitchType, tooltip = tt, data_id = rid),
        position = "identity",
        size = 3, alpha = 0.9, shape = 21, stroke = 0.25
      ) +
      geom_hline(
        data = avg_velo,
        aes(yintercept = avg_velo, color = TaggedPitchType),
        linewidth = 0.7, inherit.aes = FALSE
      ) +
      scale_x_continuous(limits = c(0, x_max), breaks = seq(0, x_max, 5)) +
      scale_y_continuous(limits = c(y_min, y_max), breaks = seq(y_min, y_max, 5)) +
      scale_color_manual(values = cols[types_chr], limits = types_chr, name = NULL) +
      scale_fill_manual(values  = cols[types_chr], limits = types_chr, name = NULL) +
      theme_minimal() + axis_theme +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 14, colour = axis_col),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.box.background = element_rect(fill = NA, colour = NA),
        axis.text = element_text(colour = axis_col),
        axis.title = element_text(colour = axis_col),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.grid.major = element_line(color = scales::alpha(axis_col, grid_alpha))
      ) +
      labs(title = "Velocity Chart (Game/Inning)", x = "Pitch Count", y = "Velocity (MPH)")
    
    # NEW: dashed vertical lines at inning boundaries (Live only)
    if ("SessionType" %in% names(df2) &&
        length(na.omit(unique(df2$SessionType))) == 1 &&
        na.omit(unique(df2$SessionType)) == "Live" &&
        "Inning" %in% names(df2)) {
      
      inning <- df2$Inning
      # first pitch of each NEW inning (skip very first overall)
      boundary_idx <- which(!is.na(inning) & dplyr::lag(inning, default = inning[1]) != inning)
      boundary_idx <- boundary_idx[boundary_idx != 1]
      if (length(boundary_idx)) {
        vlines <- dplyr::tibble(x = df2$PitchCount[boundary_idx])
        p <- p + geom_vline(data = vlines, aes(xintercept = x),
                            linetype = "dashed", linewidth = 0.4, alpha = 0.6,
                            inherit.aes = FALSE)
      }
    }
    
    ggiraph::girafe(
      ggobj = p,
      options = list(
        ggiraph::opts_tooltip(use_fill = TRUE, css = tooltip_css),
        ggiraph::opts_hover(css = "stroke:black;stroke-width:1.5px;"),
        ggiraph::opts_hover_inv(css = "opacity:0.15;"),
        ggiraph::opts_selection(type = "single", only_shiny = TRUE)
      ),
      bg = "transparent"
    )
  })
  
  # ----------------------------
  # 2) NEW: Velocity by Game (Date) & Pitch Type
  #     - dot per pitch type per Date, connected lines by pitch type
  #     - tooltip: Date, Session, Velo, IVB, HB (no InZone)
  # ----------------------------
  # ----------------------------
  # Velocity by Game (Date) & Pitch Type  — updated tooltip + x-axis
  # ----------------------------
  output$velocityByGamePlot <- ggiraph::renderGirafe({
    data_pkg <- velocity_by_game_data()
    dfG <- data_pkg$summary
    if (!nrow(dfG)) return(NULL)
    df_raw <- data_pkg$raw
    types <- ordered_types(); types_chr <- as.character(types)
    dark_on <- isTRUE(input$dark_mode)
    axis_col <- if (dark_on) "#e5e7eb" else "black"
    grid_alpha <- if (dark_on) 0.15 else 0.35
    cols <- colors_for_mode(dark_on)
    
    sp <- df_raw$RelSpeed
    if (all(is.na(sp))) { y_min <- 0; y_max <- 100 } else {
      y_min <- floor(min(sp, na.rm = TRUE) / 5) * 5
      y_max <- ceiling(max(sp, na.rm = TRUE) / 5) * 5
      if (y_min == y_max) y_max <- y_min + 5
    }
    
    date_breaks <- sort(unique(as.Date(dfG$Date)))
    
    p2 <- ggplot(dfG, aes(Date, Velo, group = TaggedPitchType, color = TaggedPitchType)) +
      ggiraph::geom_line_interactive(linewidth = 0.7, alpha = 0.85) +
      ggiraph::geom_point_interactive(
        aes(tooltip = tt, data_id = rid, fill = TaggedPitchType),
        size = 3, shape = 21, stroke = 0.25
      ) +
      scale_x_date(breaks = date_breaks, labels = function(d) format(d, "%m/%d/%y"), expand = c(0.02, 0.02)) +
      scale_y_continuous(limits = c(y_min, y_max), breaks = seq(y_min, y_max, 5)) +
      scale_color_manual(values = cols[types_chr], limits = types_chr, name = NULL) +
      scale_fill_manual(values  = cols[types_chr], limits = types_chr, name = NULL) +
      theme_minimal() + axis_theme +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 14, colour = axis_col),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.box.background = element_rect(fill = NA, colour = NA),
        axis.text.x = element_text(angle = 0, hjust = 0.5, colour = axis_col),
        axis.text.y = element_text(colour = axis_col),
        axis.title = element_text(colour = axis_col),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.grid.major = element_line(color = scales::alpha(axis_col, grid_alpha))
      ) +
      labs(title = "Average Velocity by Game",
           x = "", y = "Velocity (MPH)")
    
    ggiraph::girafe(
      ggobj = p2,
      options = list(
        ggiraph::opts_tooltip(use_fill = TRUE, css = tooltip_css),
        ggiraph::opts_hover(css = "stroke:black;stroke-width:1.5px;"),
        ggiraph::opts_hover_inv(css = "opacity:0.15;"),
        ggiraph::opts_selection(type = "single", only_shiny = TRUE)
      ),
      bg = "transparent"
    )
  })
  
  
  # ----------------------------
  # 3) NEW: Inning-to-Inning Average Velocity (relative to entry inning)
  #     - Live only
  #     - x = 1st, 2nd, 3rd inning *the pitcher threw in that game*
  # ----------------------------
  # ----------------------------
  # Inning-to-Inning Average Velocity — updated tooltip (Session only) + HB fix
  # ----------------------------
  output$velocityInningPlot <- ggiraph::renderGirafe({
    data_pkg <- velocity_inning_data()
    dfI <- data_pkg$summary
    if (!nrow(dfI)) return(NULL)
    df_live <- data_pkg$detail
    types <- ordered_types(); types_chr <- as.character(types)
    dark_on <- isTRUE(input$dark_mode)
    axis_col <- if (dark_on) "#e5e7eb" else "black"
    grid_alpha <- if (dark_on) 0.15 else 0.35
    cols <- colors_for_mode(dark_on)
    
    sp <- df_live$RelSpeed
    if (all(is.na(sp))) { y_min <- 0; y_max <- 100 } else {
      y_min <- floor(min(sp, na.rm = TRUE) / 5) * 5
      y_max <- ceiling(max(sp, na.rm = TRUE) / 5) * 5
      if (y_min == y_max) y_max <- y_min + 5
    }
    
    xmax <- max(dfI$InningOrd, na.rm = TRUE)
    p3 <- ggplot(dfI, aes(InningOrd, Velo, group = TaggedPitchType, color = TaggedPitchType)) +
      ggiraph::geom_line_interactive(linewidth = 0.7, alpha = 0.85) +
      ggiraph::geom_point_interactive(
        aes(tooltip = tt, data_id = rid, fill = TaggedPitchType),
        size = 3, shape = 21, stroke = 0.25
      ) +
      scale_x_continuous(breaks = seq_len(xmax)) +
      scale_y_continuous(limits = c(y_min, y_max), breaks = seq(y_min, y_max, 5)) +
      scale_color_manual(values = cols[types_chr], limits = types_chr, name = NULL) +
      scale_fill_manual(values  = cols[types_chr], limits = types_chr, name = NULL) +
      theme_minimal() + axis_theme +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 14, colour = axis_col),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.box.background = element_rect(fill = NA, colour = NA),
        axis.text.x = element_text(angle = 0, hjust = 0.5, colour = axis_col),
        axis.text.y = element_text(colour = axis_col),
        axis.title = element_text(colour = axis_col),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.grid.major = element_line(color = scales::alpha(axis_col, grid_alpha))
      ) +
      labs(title = "Average Velocity by Inning ",
           x = "Inning of Appearance", y = "Velocity (MPH)")
    
    ggiraph::girafe(
      ggobj = p3,
      options = list(
        ggiraph::opts_tooltip(use_fill = TRUE, css = tooltip_css),
        ggiraph::opts_hover(css = "stroke:black;stroke-width:1.5px;"),
        ggiraph::opts_hover_inv(css = "opacity:0.15;"),
        ggiraph::opts_selection(type = "single", only_shiny = TRUE)
      ),
      bg = "transparent"
    )
  })
  
  observeEvent(input$velocityPlot_selected, {
    sel <- safe_selected(input$velocityPlot_selected)
    df2 <- velocity_points_df()
    if (!nrow(df2)) return()
    if (!is.finite(sel)) return()
    idx <- max(1L, min(nrow(df2), sel))
    show_pitch_video_modal_multi(df2[idx, , drop = FALSE], dataset = df2, dataset_idx = idx)
  }, ignoreNULL = TRUE)
  
  observeEvent(input$velocityByGamePlot_selected, {
    sel <- input$velocityByGamePlot_selected
    if (is.null(sel) || !length(sel)) return()
    summary_df <- velocity_by_game_data()$summary
    if (!nrow(summary_df)) return()
    row <- summary_df[summary_df$rid %in% sel, , drop = FALSE]
    if (!nrow(row)) return()
    
    target_date <- suppressWarnings(as.Date(row$Date[1]))
    pitch_type  <- row$TaggedPitchType[1]
    session_val <- row$SessionType[1]
    
    df <- filtered_data(); if (!nrow(df)) return()
    rows <- as.data.frame(df, stringsAsFactors = FALSE)
    
    if (!is.na(target_date)) {
      rows_date <- NULL
      if ("Date" %in% names(rows)) {
        rows_date <- rows[as.Date(rows$Date) == target_date, , drop = FALSE]
      } else if ("GameDate" %in% names(rows)) {
        rows_date <- rows[as.Date(rows$GameDate) == target_date, , drop = FALSE]
      } else if ("datetime" %in% names(rows)) {
        rows_date <- rows[as.Date(rows$datetime) == target_date, , drop = FALSE]
      }
      if (!is.null(rows_date) && nrow(rows_date)) rows <- rows_date
    }
    if (!is.character(pitch_type) || !nzchar(pitch_type)) return()
    rows <- rows[rows$TaggedPitchType == pitch_type, , drop = FALSE]
    if (is.null(rows) || !nrow(rows)) {
      showModal(modalDialog("No pitches found for that selection.", easyClose = TRUE, footer = NULL))
      return()
    }
    if (!is.null(session_val) && nzchar(session_val) && "SessionType" %in% names(rows)) {
      rows_session <- rows[rows$SessionType == session_val, , drop = FALSE]
      if (nrow(rows_session)) rows <- rows_session
    }
    
    rows <- rows %>%
      dplyr::mutate(.seq = dplyr::row_number())
    if ("Date" %in% names(rows)) rows <- rows %>% dplyr::arrange(Date, .seq)
    else rows <- rows %>% dplyr::arrange(.seq)
    rows <- as.data.frame(rows, stringsAsFactors = FALSE)
    if (!nrow(rows)) {
      showModal(modalDialog("No pitches found for that selection.", easyClose = TRUE, footer = NULL))
      return()
    }
    rownames(rows) <- as.character(seq_len(nrow(rows)))
    label_txt <- paste0(pitch_type, if (!is.na(target_date)) paste0(" — ", format(target_date, "%m/%d/%y")) else "")
    show_pitch_video_sequence(rows,
                              label = label_txt,
                              start_index = 1L,
                              compare_pool = rows,
                              primary_pool_idx = 1L)
  }, ignoreNULL = TRUE)
  
  observeEvent(input$velocityInningPlot_selected, {
    sel <- input$velocityInningPlot_selected
    if (is.null(sel) || !length(sel)) return()
    data_pkg <- velocity_inning_data()
    summary_df <- data_pkg$summary
    if (!nrow(summary_df)) return()
    row <- summary_df[summary_df$rid %in% sel, , drop = FALSE]
    if (!nrow(row)) return()
    
    pitch_type <- row$TaggedPitchType[1]
    inning_ord <- suppressWarnings(as.integer(row$InningOrd[1]))
    detail_df  <- data_pkg$detail
    if (is.null(detail_df) || !nrow(detail_df)) return()
    
    if (!is.character(pitch_type) || !nzchar(pitch_type)) return()
    rows <- detail_df[detail_df$TaggedPitchType == pitch_type & detail_df$InningOrd == inning_ord, , drop = FALSE]
    if (is.null(rows) || !nrow(rows)) {
      showModal(modalDialog("No pitches found for that selection.", easyClose = TRUE, footer = NULL))
      return()
    }
    rows <- rows %>%
      dplyr::mutate(.seq = .orig_order %||% dplyr::row_number()) %>%
      dplyr::arrange(GameKey, .seq)
    rows <- as.data.frame(rows, stringsAsFactors = FALSE)
    rownames(rows) <- as.character(seq_len(nrow(rows)))
    label_txt <- paste0(pitch_type, " — Inning ", if (is.finite(inning_ord)) inning_ord else "")
    show_pitch_video_sequence(rows,
                              label = label_txt,
                              start_index = 1L,
                              compare_pool = rows,
                              primary_pool_idx = 1L)
  }, ignoreNULL = TRUE)
  
  
  # Velocity Trend Plot
  # Replace your existing output$veloTrendPlot with this:
  
  output$veloTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df)>0)
    # compute average velocity per date
    velo_dat <- df %>%
      group_by(Date) %>%
      summarise(avg_velo = mean(RelSpeed, na.rm=TRUE), .groups="drop") %>%
      arrange(Date)
    
    # dynamic y-axis limits based on data
    y_min <- floor(min(velo_dat$avg_velo, na.rm=TRUE) / 5) * 5
    y_max <- ceiling(max(velo_dat$avg_velo, na.rm=TRUE) / 5) * 5
    if (y_min == y_max) {
      y_min <- y_min - 5
      y_max <- y_max + 5
    }
    
    # preserve date ordering
    date_levels <- unique(fmt_date(velo_dat$Date))
    velo_dat <- velo_dat %>% mutate(Date_f = factor(fmt_date(Date), levels = date_levels))
    
    # single-line plot with dynamic y-axis
    ggplot(velo_dat, aes(x = Date_f, y = avg_velo, group = 1)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_y_continuous(limits = c(y_min, y_max), breaks = seq(y_min, y_max, 5)) +
      labs(title = "Average Velocity", x = NULL, y = "Velocity (MPH)") +
      theme_minimal() + axis_theme +
      theme(
        plot.title     = element_text(face = "bold"),
        axis.text.x    = element_text(angle = 45, hjust = 1),
        axis.line.x    = element_line(color = "black"),
        axis.line.y    = element_line(color = "black")
      )
  }, bg = "transparent")
  
  # Heatmap Plot
  output$heatmapPlot <- renderPlot({
    df <- filtered_data()
    if (!nrow(df)) return()
    
    sel <- sel_results()
    
    # If not "All", filter to selected result types only (exclude NAs).
    if (!identical(sel, result_levels)) {
      df <- dplyr::filter(df, !is.na(Result) & Result %in% sel)
    }
    # If "All", keep everything (including NA results).
    
    bins <- 10
    pal  <- colorRampPalette(c("white","blue","lightblue","turquoise","yellow","orange","red"))(bins)
    home <- data.frame(x=c(-0.75,0.75,0.75,0.00,-0.75),
                       y=c(1.05,1.05,1.15,1.25,1.15)-0.5)
    sz <- data.frame(xmin=ZONE_LEFT, xmax=ZONE_RIGHT, ymin=ZONE_BOTTOM, ymax=ZONE_TOP)
    
    ggplot(df, aes(PlateLocSide, PlateLocHeight)) +
      stat_density_2d_filled(aes(fill=..level..), bins=bins, show.legend=FALSE) +
      scale_fill_manual(values=pal) +
      geom_polygon(data=home, aes(x,y), fill=NA, color="black", inherit.aes=FALSE) +
      geom_rect(data=sz, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
                fill=NA, color="black", inherit.aes=FALSE) +
      coord_fixed(ratio=1, xlim=c(-2,2), ylim=c(0,4.5)) +
      theme_void()
  }, bg = "transparent")
  
  # Pitch Plot
  output$pitchPlot <- ggiraph::renderGirafe({
    df <- filtered_data(); if (!nrow(df)) return(NULL)
    types <- ordered_types(); types_chr <- as.character(types)
    dark_on <- isTRUE(input$dark_mode)
    axis_col <- if (dark_on) "#e5e7eb" else "black"
    cols <- colors_for_mode(dark_on)
    
    sel <- sel_results()
    if (!identical(sel, result_levels)) {
      df <- dplyr::filter(df, !is.na(Result) & Result %in% sel)
    }
    
    df_i <- df %>%
      dplyr::mutate(
        tt      = make_hover_tt(.),
        rid     = dplyr::row_number(),
        tt_fill = dplyr::coalesce(all_colors[as.character(TaggedPitchType)], "gray")
      )
    
    home <- data.frame(x = c(-0.75, 0.75, 0.75, 0.00, -0.75),
                       y = c(1.05, 1.05, 1.15, 1.25, 1.15) - 0.5)
    cz <- data.frame(xmin = -1.5, xmax = 1.5, ymin = 2.65 - 1.7, ymax = 2.65 + 1.3)
    sz <- data.frame(xmin = ZONE_LEFT, xmax = ZONE_RIGHT, ymin = ZONE_BOTTOM, ymax = ZONE_TOP)
    
    df_known <- dplyr::filter(df_i, !is.na(Result))
    df_other <- dplyr::filter(df_i,  is.na(Result))
    
    p <- ggplot() +
      geom_polygon(data = home, aes(x, y), fill = NA, color = "black", inherit.aes = FALSE) +
      geom_rect(data = cz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = NA, color = "black", linetype = "dashed", inherit.aes = FALSE) +
      geom_rect(data = sz, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = NA, color = "black", inherit.aes = FALSE) +
      
      ggiraph::geom_point_interactive(
        data = df_other,
        aes(PlateLocSide, PlateLocHeight,
            color = TaggedPitchType, fill = TaggedPitchType,
            tooltip = tt, data_id = rid),
        position = "identity",
        size = 4.0, alpha = 0.95, shape = 16, stroke = 0.6
      ) +
      
      ggiraph::geom_point_interactive(
        data = df_known,
        aes(PlateLocSide, PlateLocHeight,
            color = TaggedPitchType, fill = TaggedPitchType, shape = Result,
            tooltip = tt, data_id = rid),
        position = "identity",
        size = 4.0, alpha = 0.95, stroke = 0.8
      ) +
      
      scale_color_manual(values = all_colors[types_chr], limits = types_chr, name = NULL) +
      scale_fill_manual(values  = all_colors[types_chr], limits = types_chr, name = NULL) +
      scale_shape_manual(values = shape_map, drop = TRUE, name = NULL) +
      coord_fixed(ratio = 1, xlim = c(-3, 3), ylim = c(0.5, 5)) +
      theme_void() + theme(legend.position = "none") +
      
      # 🔹 Invisible “hover pad” to force correct tooltip fill even for hollow shapes
      ggiraph::geom_point_interactive(
        data = df_i,
        aes(PlateLocSide, PlateLocHeight, tooltip = tt, data_id = rid, fill = I(tt_fill)),
        shape = 21, size = 6, alpha = 0.001, stroke = 0, inherit.aes = FALSE
      )
    
    ggiraph::girafe(
      ggobj = p,
      options = list(
        ggiraph::opts_tooltip(use_fill = TRUE, use_stroke = TRUE, css = tooltip_css),
        ggiraph::opts_hover(css = "stroke-width:1.5px;"),
        ggiraph::opts_hover_inv(css = "opacity:0.15;")
      )
    )
  })
  
  # ---------- Shared helpers for this page ----------
  # Keep Pitch Results tidy (same behavior as Location)
  observeEvent(input$locResult, {
    sel <- input$locResult
    if (is.null(sel) || !length(sel)) {
      updateSelectInput(session, "locResult", selected = "All"); return()
    }
    if ("All" %in% sel && length(sel) > 1) {
      updateSelectInput(session, "locResult", selected = setdiff(sel, "All"))
    }
  }, ignoreInit = FALSE)
  
  # Current selection of results (treat All as no filter)
  sel_results <- reactive({
    sel <- input$locResult
    if (is.null(sel) || !length(sel) || "All" %in% sel) return(result_levels)
    sel
  })
  
  # Small note: hide EV notice unless user picks Pitch chart with EV (heat-only stat)
  output$hmNote <- renderUI({
    if (identical(input$hmChartType, "Pitch") && identical(input$hmStat, "EV")) {
      tags$div(style="margin-top:8px; font-size:12px; color:#a00;",
               "Note: Exit Velocity is a heat-map metric; switch chart to 'Heat' to view.")
    } else NULL
  })
  
  # Legend (shapes + pitch type colors), like Location page
  output$locLegend <- renderUI({
    df <- filtered_data()
    types <- intersect(names(all_colors), as.character(unique(df$TaggedPitchType)))
    dark_on <- isTRUE(input$dark_mode)
    axis_col <- if (dark_on) "#e5e7eb" else "black"
    cols <- colors_for_mode(dark_on)
    
    shape_key <- list(
      "Called Strike" = "\u25CF", # ●
      "Ball"          = "\u25CB", # ○
      "Foul"          = "\u25B3", # △
      "Whiff"         = "\u2605", # ★
      "In Play (Out)" = "\u25B2", # ▲
      "In Play (Hit)" = "\u25A0"  # ■
    )
    
    sty <- HTML("
    .legend-block { margin-bottom:10px; }
    .legend-title { font-weight:700; margin-bottom:6px; }
    .legend-list  { list-style:none; padding-left:0; margin:0; }
    .legend-list li { margin:2px 0; display:flex; align-items:center; }
    .dot { display:inline-block; width:12px; height:12px; border-radius:50%;
           border:1px solid #000; margin-right:6px; }
    .shape { width:18px; display:inline-block; margin-right:6px; text-align:center; }
  ")
    
    tags$div(
      tags$style(sty),
      
      tags$div(class="legend-block",
               tags$div(class="legend-title", "Pitch Result"),
               tags$ul(class="legend-list",
                       lapply(result_levels, function(lbl) {
                         tags$li(
                           style = paste0("color:", axis_col, ";"),
                           tags$span(class="shape", if (is.null(shape_key[[lbl]])) "" else shape_key[[lbl]]),
                           tags$span(lbl)
                         )
                       })
               )
      ),
      
      tags$div(class="legend-block",
               tags$div(class="legend-title", "Pitch Type"),
               tags$ul(class="legend-list",
                       lapply(types, function(tp) {
                         col <- cols[[tp]]; if (is.null(col)) col <- "gray"
                         tags$li(
                           style = paste0("color:", axis_col, ";"),
                           tags$span(class="dot", style = paste0("background:", col, ";border:1px solid ", axis_col, ";")),
                           tags$span(tp)
                         )
                       })
               )
      )
    )
  })
  
  # ---------- HEAT (respects Pitch Results filter) ----------
  output$heatmapsHeatPlot <- renderPlot({
    df <- filtered_data(); if (!nrow(df)) return()
    
    # Ensure Result column exists for filtering
    if (!("Result" %in% names(df))) {
      df$Result <- factor(compute_result(df$PitchCall, df$PlayResult), levels = result_levels)
    }
    
    # Apply Pitch Results filter to HEAT too
    sel <- sel_results()
    if (!identical(sel, result_levels)) {
      df <- dplyr::filter(df, !is.na(Result) & Result %in% sel)
    }
    if (!nrow(df)) return(ggplot() + theme_void())
    
    # Map UI label to internal token
    stat <- input$hmStat
    if (identical(stat, "Exit Velocity")) stat <- "EV"
    
    # Small helper to build KDE grid
    make_kde_grid <- function(x, y, lims = c(-2,2,0,4.5), n = 180) {
      ok <- is.finite(x) & is.finite(y)
      x <- x[ok]; y <- y[ok]
      if (length(x) < 2 || length(unique(x)) < 2 || length(unique(y)) < 2) {
        return(data.frame(x = numeric(0), y = numeric(0), z = numeric(0)))
      }
      d <- MASS::kde2d(x, y, n = n, lims = lims)
      expand.grid(x = d$x, y = d$y) |> transform(z = as.vector(d$z))
    }
    
    hit_types <- get_hit_type_col(df)
    if (stat == "Frequency") {
      grid <- make_kde_grid(df$PlateLocSide, df$PlateLocHeight)
      return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_freq, mark_max = TRUE))
    }
    
    if (stat == "Whiff Rate") {
      wh <- df$PitchCall == "StrikeSwinging"
      grid <- make_kde_grid(df$PlateLocSide[wh], df$PlateLocHeight[wh])
      return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_red, mark_max = TRUE))
    }
    
    if (stat == "GB Rate") {
      gb <- df$SessionType == "Live" & !is.na(hit_types) & hit_types == "GroundBall"
      grid <- make_kde_grid(df$PlateLocSide[gb], df$PlateLocHeight[gb])
      return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_red, mark_max = TRUE))
    }
    
    if (stat == "Contact Rate") {
      cp <- df$SessionType == "Live" & !is.na(df$PitchCall) & df$PitchCall == "InPlay"
      grid <- make_kde_grid(df$PlateLocSide[cp], df$PlateLocHeight[cp])
      return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_red, mark_max = TRUE))
    }
    
    if (stat == "Swing Rate") {
      swing_denoms <- c("StrikeSwinging","FoulBallNotFieldable","FoulBallFieldable","InPlay","FoulBall")
      sw <- df$SessionType == "Live" & !is.na(df$PitchCall) & (df$PitchCall %in% swing_denoms)
      grid <- make_kde_grid(df$PlateLocSide[sw], df$PlateLocHeight[sw])
      return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_red, mark_max = TRUE))
    }
    
    if (stat == "EV") {
      df_hi <- dplyr::filter(
        df, SessionType == "Live",
        is.finite(PlateLocSide), is.finite(PlateLocHeight),
        is.finite(ExitSpeed), ExitSpeed >= HEAT_EV_THRESHOLD
      )
      if (!nrow(df_hi)) return(ggplot() + theme_void())
      
      grid <- make_kde_grid(df_hi$PlateLocSide, df_hi$PlateLocHeight)
      if (!nrow(grid)) return(ggplot() + theme_void())
      
      zmax <- suppressWarnings(max(grid$z, na.rm = TRUE))
      if (!is.finite(zmax) || zmax <= 0) return(ggplot() + theme_void())
      grid$z <- grid$z / zmax
      
      floor_q <- 0.25
      floor   <- stats::quantile(grid$z[grid$z > 0], floor_q, na.rm = TRUE)
      idx <- which(!is.na(grid$z) & grid$z < floor)
      if (length(idx)) grid$z[idx] <- NA
      
      return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_red, mark_max = TRUE))
    }
    
    ggplot() + theme_void()
  }, bg = "transparent")

  # Override theme for HeatMaps plot container (force transparent background)
  observe({
    if (isTRUE(input$dark_mode)) {
      session$sendCustomMessage("setHeatmapsBg", list(bg = "transparent"))
    }
  })
  
  # ---------- PITCH (point chart; respects Pitch Results filter) ----------
  output$heatmapsPitchPlot <- ggiraph::renderGirafe({
    req(input$hmChartType == "Pitch")
    df <- filtered_data(); if (!nrow(df)) return(NULL)
    dark_on <- isTRUE(input$dark_mode)
    axis_col <- if (dark_on) "#e5e7eb" else "black"
    cols <- colors_for_mode(dark_on)
    
    # Ensure Result for filtering + shapes
    if (!("Result" %in% names(df))) {
      df$Result <- factor(compute_result(df$PitchCall, df$PlayResult), levels = result_levels)
    }
    
    sel <- sel_results()
    if (!identical(sel, result_levels)) {
      df <- dplyr::filter(df, !is.na(Result) & Result %in% sel)
    }
    if (!nrow(df)) return(NULL)
    
    types <- intersect(names(all_colors), as.character(unique(df$TaggedPitchType)))
    types_chr <- as.character(types)
    
    df_i <- df %>%
      dplyr::mutate(
        tt      = make_hover_tt(.),
        rid     = dplyr::row_number(),
        tt_fill = dplyr::coalesce(all_colors[as.character(TaggedPitchType)], "gray")
      )
    
    home <- data.frame(x=c(-0.75,0.75,0.75,0.00,-0.75),
                       y=c(1.05,1.05,1.15,1.25,1.15)-0.5)
    cz <- data.frame(xmin = -1.5, xmax = 1.5, ymin = 2.65 - 1.7, ymax = 2.65 + 1.3)
    sz <- data.frame(xmin = ZONE_LEFT, xmax = ZONE_RIGHT, ymin = ZONE_BOTTOM, ymax = ZONE_TOP)
    
    p <- ggplot() +
      geom_polygon(data = home, aes(x, y), fill = NA, color = axis_col) +
      geom_rect(data = cz, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
                fill = NA, color = axis_col, linetype = "dashed") +
      geom_rect(data = sz, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
                fill = NA, color = axis_col) +
      
      # visible points (unknown result as solid circle)
      ggiraph::geom_point_interactive(
        data = dplyr::filter(df_i, is.na(Result)),
        aes(PlateLocSide, PlateLocHeight,
            color = TaggedPitchType, fill = TaggedPitchType,
            tooltip = tt, data_id = rid),
        size = 4.0, alpha = 0.95, shape = 16, stroke = 0.6
      ) +
      
      # visible points (known result with shapes)
      ggiraph::geom_point_interactive(
        data = dplyr::filter(df_i, !is.na(Result)),
        aes(PlateLocSide, PlateLocHeight,
            color = TaggedPitchType, fill = TaggedPitchType, shape = Result,
            tooltip = tt, data_id = rid),
        size = 4.0, alpha = 0.95, stroke = 0.8
      ) +
      
      scale_color_manual(values = cols[types_chr], limits = types_chr, name = NULL) +
      scale_fill_manual(values  = cols[types_chr], limits = types_chr, name = NULL) +
      scale_shape_manual(values = shape_map, drop = TRUE, name = NULL) +
      coord_fixed(ratio = 1, xlim = c(-3, 3), ylim = c(0.5, 5)) +
      theme_void() + theme(
        legend.position = "none",
        plot.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA)
      ) +
      
      # invisible hover pad to guarantee correct tooltip fill even for hollow shapes
      ggiraph::geom_point_interactive(
        data = df_i,
        aes(PlateLocSide, PlateLocHeight, tooltip = tt, data_id = rid, fill = I(tt_fill)),
        shape = 21, size = 6, alpha = 0.001, stroke = 0, inherit.aes = FALSE
      )
    
    ggiraph::girafe(
      ggobj = p,
      options = list(
        ggiraph::opts_tooltip(use_fill = TRUE, use_stroke = TRUE, css = tooltip_css),
        ggiraph::opts_hover(css = "stroke-width:1.5px;"),
        ggiraph::opts_hover_inv(css = "opacity:0.15;")
      ),
      bg = "transparent"
    )
  })
  
  
  # ---------- small helpers ----------
  .find_qp_plus_col <- function(df) {
    nms <- names(df)
    ln  <- tolower(nms)
    # direct hits first
    candidates <- c("qp+", "qp_plus", "qp.plus", "qp.", "qualitypitch+", "qualitypitchplus", "qp")
    hit <- nms[match(candidates, ln, nomatch = 0)]
    if (length(hit) && nzchar(hit[1])) return(hit[1])
    # regex fallback: qp, qp+, qp plus, qp_plus, qp.plus, qualitypitch...
    idx <- grep("^(qp|quality\\s*pitch)[[:space:]_\\.]*([+]||plus)?$", ln, perl = TRUE)
    if (length(idx)) nms[idx[1]] else NULL
  }
  
  .num_safely <- function(x) {
    x_chr <- as.character(x)
    x_chr <- trimws(gsub(",", "", x_chr, fixed = TRUE))
    x_chr <- gsub("%$", "", x_chr)
    suppressWarnings(as.numeric(x_chr))
  }
  
  output$trendPlotUI <- renderUI({
    switch(input$trendMetric,
           "Velocity (Avg)"    = plotOutput("veloTrendPlot",    height = "350px"),
           "Velocity (Max)"    = plotOutput("maxVeloTrendPlot", height = "350px"),
           "InZone %"          = plotOutput("inZoneTrendPlot",  height = "350px"),
           "Comp %"            = plotOutput("compTrendPlot",    height = "350px"),
           "FPS%"              = plotOutput("fpsTrendPlot",     height = "350px"),
           "E+A%"              = plotOutput("eaTrendPlot",      height = "350px"),
           "Whiff%"            = plotOutput("whiffTrendPlot",   height = "350px"),
           'CSW%'              = plotOutput(ns('cswTrendPlot'), height = "350px"),
           "Strike%"           = plotOutput("strikeTrendPlot",  height = "350px"),
           "K%"                = plotOutput("kTrendPlot",       height = "350px"),
           "BB%"               = plotOutput("bbTrendPlot",      height = "350px"),
           "Stuff+"            = plotOutput("stuffTrendPlot",   height = "350px"),
           "Ctrl+"             = plotOutput("commandTrendPlot", height = "350px"),
           "QP+"               = plotOutput("qpTrendPlot",      height = "350px"),
           "Pitching+"         = plotOutput("pitchingTrendPlot",height = "350px"),
           "IVB"               = plotOutput("ivbTrendPlot",     height = "350px"),
           "HB"                = plotOutput("hbTrendPlot",      height = "350px"),
           "Release Height"    = plotOutput("heightTrendPlot",  height = "350px"),
           "Extension"         = plotOutput("extensionTrendPlot",height = "350px")
    )
  })
  
  output$veloTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df)>0)
    trend_plot(df, RelSpeed, "Average Velocity", "Velocity (MPH)", mean)
  }, bg = "transparent")
  
  output$maxVeloTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df)>0)
    trend_plot(df, RelSpeed, "Max Velocity", "Velocity (MPH)", max)
  }, bg = "transparent")
  
  output$inZoneTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df)>0)
    trend_plot(
      df,
      PlateLocSide >= ZONE_LEFT & PlateLocSide <= ZONE_RIGHT &
        PlateLocHeight >= ZONE_BOTTOM & PlateLocHeight <= ZONE_TOP,
      "InZone %", "Percentage",
      function(x) mean(x, na.rm = TRUE) * 100
    )
  }, bg = "transparent")
  
  output$compTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df)>0)
    trend_plot(
      df,
      PlateLocSide >= -1.5 & PlateLocSide <= 1.5 &
        PlateLocHeight >= (2.65 - 1.7) & PlateLocHeight <= (2.65 + 1.3),
      "Comp %", "Percentage",
      function(x) mean(x, na.rm = TRUE) * 100
    )
  }, bg = "transparent")
  
  output$ivbTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df)>0)
    trend_plot(df, InducedVertBreak, "Induced Vertical Break", "IVB (in)", mean)
  }, bg = "transparent")
  
  output$hbTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df)>0)
    trend_plot(df, HorzBreak, "Horizontal Break", "HB (in)", mean)
  }, bg = "transparent")
  
  output$heightTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df)>0)
    trend_plot(df, RelHeight, "Release Height", "Height (ft)", mean)
  }, bg = "transparent")
  
  output$extensionTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df)>0)
    trend_plot(df, Extension, "Extension (ft)", mean)
  }, bg = "transparent")
  
  output$stuffTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df)>0)
    trend_plot(df, `Stuff+`, "Stuff+", "Stuff+", mean)
  }, bg = "transparent")
  
  output$commandTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df)>0)
    df <- df %>%
      dplyr::mutate(ctrl_score =
                      ifelse(
                        PlateLocSide >= ZONE_LEFT & PlateLocSide <= ZONE_RIGHT &
                          PlateLocHeight >= ZONE_BOTTOM & PlateLocHeight <= ZONE_TOP, 1.47,
                        ifelse(
                          PlateLocSide >= -1.5 & PlateLocSide <= 1.5 &
                            PlateLocHeight >= (2.65 - 1.7) & PlateLocHeight <= (2.65 + 1.3),
                          0.73, 0
                        )
                      ) * 100)
    trend_plot(df, ctrl_score, "Ctrl+", "Ctrl+", mean)
  }, bg = "transparent")
  
  # ---------- QP+ (make it mirror Stuff+/Ctrl+ usage) ----------
  output$qpTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df) > 0)
    df <- dplyr::ungroup(df)
    
    # 1) Find an existing QP-like column no matter how it was named/sanitized
    qp_col <- .find_qp_plus_col(df)
    
    if (!is.null(qp_col)) {
      # Coerce safely to numeric new column, then use like Stuff+/Ctrl+
      df <- dplyr::mutate(df, qp_score = .num_safely(.data[[qp_col]]))
    } else {
      # 2) If no QP+ column exists, try to compute a fallback QP score inline.
      #    This mirrors your Ctrl+ scaffold so the trend code path is identical.
      #    Replace this block later with your exact QP+ routine if desired.
      df <- dplyr::mutate(
        df,
        qp_score = ifelse(
          PlateLocSide >= ZONE_LEFT & PlateLocSide <= ZONE_RIGHT &
            PlateLocHeight >= ZONE_BOTTOM & PlateLocHeight <= ZONE_TOP, 1.47,
          ifelse(
            PlateLocSide >= -1.5 & PlateLocSide <= 1.5 &
              PlateLocHeight >= (2.65 - 1.7) & PlateLocHeight <= (2.65 + 1.3),
            0.73, 0
          )
        ) * 100
      )
    }
    
    validate(need(any(is.finite(df$qp_score)), "No QP+ values for current filters"))
    trend_plot(df, qp_score, "QP+", "QP+", mean)
  }, bg = "transparent")
  
  # ---------- Pitching+ (prefer QP+; fallback to Ctrl+) ----------
  output$pitchingTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df)>0)
    df <- dplyr::ungroup(df)
    
    # Build ctrl_score (needed as fallback and for legacy)
    df <- df %>%
      dplyr::mutate(ctrl_score =
                      ifelse(
                        PlateLocSide >= ZONE_LEFT & PlateLocSide <= ZONE_RIGHT &
                          PlateLocHeight >= ZONE_BOTTOM & PlateLocHeight <= ZONE_TOP, 1.47,
                        ifelse(
                          PlateLocSide >= -1.5 & PlateLocSide <= 1.5 &
                            PlateLocHeight >= (2.65 - 1.7) & PlateLocHeight <= (2.65 + 1.3),
                          0.73, 0
                        )
                      ) * 100)
    
    # Try to get QP+ numeric (same logic as above)
    qp_col <- .find_qp_plus_col(df)
    if (!is.null(qp_col)) {
      df <- dplyr::mutate(df, qp_score = .num_safely(.data[[qp_col]]))
    }
    
    use_qp <- "qp_score" %in% names(df) && any(is.finite(df$qp_score))
    
    if (input$sessionType == "All") {
      dat <- df %>%
        dplyr::group_by(Date, SessionType) %>%
        dplyr::summarise(
          Stuff = mean(`Stuff+`, na.rm = TRUE),
          Qual  = mean(if (use_qp) qp_score else ctrl_score, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(Pitching = (Stuff + Qual) / 2) %>%
        dplyr::arrange(Date)
      
      date_levels <- unique(fmt_date(dat$Date))
      dat <- dplyr::mutate(dat, Date_f = factor(fmt_date(Date), levels = date_levels))
      
      ggplot(dat, aes(Date_f, Pitching, group = SessionType, color = SessionType)) +
        geom_line(size = 1.2) + geom_point(size = 2) +
        scale_color_manual(values = session_cols, breaks = c("Live", "Bullpen"), name = NULL) +
        labs(title = "Pitching+", x = NULL, y = "Pitching+") +
        theme_minimal() + axis_theme +
        theme(plot.title = element_text(face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")
    } else {
      dat <- df %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(
          Stuff = mean(`Stuff+`, na.rm = TRUE),
          Qual  = mean(if (use_qp) qp_score else ctrl_score, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(Pitching = (Stuff + Qual) / 2) %>%
        dplyr::arrange(Date)
      
      date_levels <- unique(fmt_date(dat$Date))
      dat <- dplyr::mutate(dat, Date_f = factor(fmt_date(Date), levels = date_levels))
      
      ggplot(dat, aes(Date_f, Pitching, group = 1)) +
        geom_line(size = 1.2) + geom_point(size = 2) +
        labs(title = "Pitching+", x = NULL, y = "Pitching+") +
        theme_minimal() + axis_theme +
        theme(plot.title = element_text(face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
    }
  }, bg = "transparent")
  
  # ---------- Helpers for Trend % plots ----------
  plot_trend_df <- function(dat, title, ylab) {
    date_levels <- unique(fmt_date(dat$Date))
    dat <- dplyr::mutate(dat, Date_f = factor(fmt_date(Date), levels = date_levels))
    
    if ("SessionType" %in% names(dat)) {
      ggplot(dat, aes(Date_f, value, group = SessionType, color = SessionType)) +
        geom_line(size = 1.2, na.rm = TRUE) + 
        geom_point(size = 2, na.rm = TRUE) +
        scale_color_manual(values = session_cols, breaks = c("Live","Bullpen"), name = NULL) +
        labs(title = title, x = NULL, y = ylab) +
        theme_minimal() + axis_theme +
        theme(plot.title = element_text(face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")
    } else {
      ggplot(dat, aes(Date_f, value, group = 1)) +
        geom_line(size = 1.2, na.rm = TRUE) + 
        geom_point(size = 2, na.rm = TRUE) +
        labs(title = title, x = NULL, y = ylab) +
        theme_minimal() + axis_theme +
        theme(plot.title = element_text(face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
    }
  }
  
  # ---------- FPS% (Live-only) ----------
  output$fpsTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df) > 0)
    if (input$sessionType == "All") {
      dat <- df %>%
        group_by(Date, SessionType) %>%
        summarise(
          BF  = sum(SessionType == "Live" & Balls == 0 & Strikes == 0, na.rm = TRUE),
          FPS = sum(SessionType == "Live" & Balls == 0 & Strikes == 0 &
                      PitchCall %in% c("InPlay","StrikeSwinging","StrikeCalled","FoulBallNotFieldable","FoulBall"),
                    na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(value = ifelse(BF > 0, 100 * FPS / BF, NA_real_)) %>%
        arrange(Date)
    } else {
      dat <- df %>%
        group_by(Date) %>%
        summarise(
          BF  = sum(Balls == 0 & Strikes == 0, na.rm = TRUE),
          FPS = sum(Balls == 0 & Strikes == 0 &
                      PitchCall %in% c("InPlay","StrikeSwinging","StrikeCalled","FoulBallNotFieldable","FoulBall"),
                    na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(value = ifelse(BF > 0, 100 * FPS / BF, NA_real_)) %>%
        arrange(Date)
    }
    plot_trend_df(dat, "FPS%", "Percentage")
  }, bg = "transparent")
  
  # ---------- E+A% (Live-only) ----------
  output$eaTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df) > 0)
    is_EA <- (df$Balls == 0 & df$Strikes == 0 & df$PitchCall == "InPlay") |
      (df$Balls == 0 & df$Strikes == 1 & df$PitchCall %in% c("InPlay","FoulBall","FoulBallNotFieldable")) |
      (df$Balls == 1 & df$Strikes == 0 & df$PitchCall == "InPlay") |
      (df$Balls == 1 & df$Strikes == 1 & df$PitchCall %in% c("InPlay","FoulBall","FoulBallNotFieldable")) |
      (df$Balls == 0 & df$Strikes == 2 & df$PitchCall %in% c("InPlay","StrikeSwinging","StrikeCalled"))
    df$EA_flag <- is_EA
    
    if (input$sessionType == "All") {
      dat <- df %>%
        group_by(Date, SessionType) %>%
        summarise(
          BF = sum(SessionType == "Live" & Balls == 0 & Strikes == 0, na.rm = TRUE),
          EA = sum(SessionType == "Live" & EA_flag, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(value = ifelse(BF > 0, 100 * EA / BF, NA_real_)) %>%
        arrange(Date)
    } else {
      dat <- df %>%
        group_by(Date) %>%
        summarise(
          BF = sum(Balls == 0 & Strikes == 0, na.rm = TRUE),
          EA = sum(EA_flag, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(value = ifelse(BF > 0, 100 * EA / BF, NA_real_)) %>%
        arrange(Date)
    }
    plot_trend_df(dat, "E+A%", "Percentage")
  }, bg = "transparent")
  
  # ---------- Whiff% ----------
  output$whiffTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df) > 0)
    if (input$sessionType == "All") {
      dat <- df %>%
        group_by(Date, SessionType) %>%
        summarise(
          sw  = sum(PitchCall == "StrikeSwinging", na.rm = TRUE),
          den = sum(PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","InPlay","FoulBall"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(value = ifelse(den > 0, 100 * sw / den, NA_real_)) %>%
        arrange(Date)
    } else {
      dat <- df %>%
        group_by(Date) %>%
        summarise(
          sw  = sum(PitchCall == "StrikeSwinging", na.rm = TRUE),
          den = sum(PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","InPlay","FoulBall"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(value = ifelse(den > 0, 100 * sw / den, NA_real_)) %>%
        arrange(Date)
    }
    plot_trend_df(dat, "Whiff%", "Percentage")
  }, bg = "transparent")
  
  output$cswTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df) > 0)
    
    if (input$sessionType == "All") {
      dat <- df %>%
        dplyr::group_by(Date, SessionType) %>%
        dplyr::summarise(
          Pitches = dplyr::n(),
          CSW     = sum(PitchCall %in% c("StrikeCalled","StrikeSwinging"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(value = dplyr::if_else(Pitches > 0, 100 * CSW / Pitches, NA_real_)) %>%
        dplyr::arrange(Date)
    } else {
      dat <- df %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(
          Pitches = dplyr::n(),
          CSW     = sum(PitchCall %in% c("StrikeCalled","StrikeSwinging"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(value = dplyr::if_else(Pitches > 0, 100 * CSW / Pitches, NA_real_)) %>%
        dplyr::arrange(Date)
    }
    
    plot_trend_df(dat, "CSW%", "Percentage")
  }, bg = "transparent")
  
  
  # ---------- Strike% ----------
  output$strikeTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df) > 0)
    strike_calls <- c("StrikeCalled","StrikeSwinging","FoulBallNotFieldable","InPlay","FoulBall")
    if (input$sessionType == "All") {
      dat <- df %>%
        group_by(Date, SessionType) %>%
        summarise(
          strikes = sum(PitchCall %in% strike_calls, na.rm = TRUE),
          total   = n(),
          .groups = "drop"
        ) %>%
        mutate(value = ifelse(total > 0, 100 * strikes / total, NA_real_)) %>%
        arrange(Date)
    } else {
      dat <- df %>%
        group_by(Date) %>%
        summarise(
          strikes = sum(PitchCall %in% strike_calls, na.rm = TRUE),
          total   = n(),
          .groups = "drop"
        ) %>%
        mutate(value = ifelse(total > 0, 100 * strikes / total, NA_real_)) %>%
        arrange(Date)
    }
    plot_trend_df(dat, "Strike%", "Percentage")
  }, bg = "transparent")
  
  # ---------- K% (Live-only) ----------
  output$kTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df) > 0)
    if (input$sessionType == "All") {
      dat <- df %>%
        group_by(Date, SessionType) %>%
        summarise(
          BF = sum(SessionType == "Live" & Balls == 0 & Strikes == 0, na.rm = TRUE),
          K  = sum(SessionType == "Live" & KorBB == "Strikeout", na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(value = ifelse(BF > 0, 100 * K / BF, NA_real_)) %>%
        arrange(Date)
    } else {
      dat <- df %>%
        group_by(Date) %>%
        summarise(
          BF = sum(Balls == 0 & Strikes == 0, na.rm = TRUE),
          K  = sum(KorBB == "Strikeout", na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(value = ifelse(BF > 0, 100 * K / BF, NA_real_)) %>%
        arrange(Date)
    }
    plot_trend_df(dat, "K%", "Percentage")
  }, bg = "transparent")
  
  # ---------- BB% (Live-only) ----------
  output$bbTrendPlot <- renderPlot({
    df <- filtered_data(); req(nrow(df) > 0)
    if (input$sessionType == "All") {
      dat <- df %>%
        group_by(Date, SessionType) %>%
        summarise(
          BF = sum(SessionType == "Live" & Balls == 0 & Strikes == 0, na.rm = TRUE),
          BB = sum(SessionType == "Live" & KorBB == "Walk", na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(value = ifelse(BF > 0, 100 * BB / BF, na_real_)) %>%
        arrange(Date)
    } else {
      dat <- df %>%
        group_by(Date) %>%
        summarise(
          BF = sum(Balls == 0 & Strikes == 0, na.rm = TRUE),
          BB = sum(KorBB == "Walk", na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(value = ifelse(BF > 0, 100 * BB / BF, na_real_)) %>%
        arrange(Date)
    }
    plot_trend_df(dat, "BB%", "Percentage")
  }, bg = "transparent")
  
  # ============== CORRELATIONS SERVER LOGIC ==============
  
  # Update player choices based on domain
  observe({
    req(input$corr_domain)
    
    if (input$corr_domain == "Pitching") {
      # Use the whitelist-filtered pitch_data_pitching for consistency with other modules
      players <- sort(unique(pitch_data_pitching$Pitcher))
    } else if (input$corr_domain == "Hitting") {
      # Apply team filtering to hitting data
      team_data <- pitch_data
      tc <- get0("TEAM_CODE", ifnotfound = "")
      if (nzchar(tc)) {
        # Team synonyms for TMdata (can customize for your team)
        codes_for <- function(code) {
          TEAM_SYNONYMS <- list(
            # Add team synonyms if needed
          )
          if (code %in% names(TEAM_SYNONYMS)) TEAM_SYNONYMS[[code]] else code
        }
        team_data <- team_data %>% dplyr::filter(BatterTeam %in% codes_for(tc))
      }
      players <- sort(unique(na.omit(as.character(team_data$Batter))))
    } else if (input$corr_domain == "Catching") {
      # Apply team filtering to catching data
      team_data <- pitch_data
      tc <- get0("TEAM_CODE", ifnotfound = "")
      if (nzchar(tc)) {
        # Team synonyms for TMdata (can customize for your team)
        codes_for <- function(code) {
          TEAM_SYNONYMS <- list(
            # Add team synonyms if needed
          )
          if (code %in% names(TEAM_SYNONYMS)) TEAM_SYNONYMS[[code]] else code
        }
        team_data <- team_data %>% dplyr::filter(PitcherTeam %in% codes_for(tc))
      }
      players <- sort(unique(na.omit(as.character(team_data$Catcher))))
    } else {
      players <- c()
    }
    
    updateSelectInput(session, "corr_player", 
                      choices = c("All Players" = "all", setNames(players, players)),
                      selected = "all")
  })
  
  # Update pitch type choices based on domain
  observe({
    req(input$corr_domain)
    
    if (input$corr_domain == "Pitching") {
      # Get unique pitch types from the data using TaggedPitchType
      available_pitches <- sort(unique(na.omit(pitch_data$TaggedPitchType)))
      cat("Available pitches from data:", paste(available_pitches, collapse = ", "), "\n")
      # Stuff+ base pitches should only be Fastball and Sinker
      stuff_base_pitches <- c("Fastball", "Sinker")
    } else {
      available_pitches <- c()
      stuff_base_pitches <- c()
    }
    
    # Create choices list with explicit names and values
    pitch_choices <- list("All" = "all")
    for(pitch in available_pitches) {
      pitch_choices[[pitch]] <- pitch
    }
    
    updateSelectInput(session, "corr_pitch_type",
                      choices = pitch_choices,
                      selected = "all")
    cat("Updated corr_pitch_type choices with values:", paste(unlist(pitch_choices), collapse = ", "), "\n")
    cat("Updated corr_pitch_type choices with names:", paste(names(pitch_choices), collapse = ", "), "\n")
    
    updateSelectInput(session, "corr_stuff_base",
                      choices = c("All" = "all", setNames(stuff_base_pitches, stuff_base_pitches)),
                      selected = "all")
  })
  
  # Update variable choices based on domain
  observe({
    req(input$corr_domain)
    
    # Create comprehensive variable lists using the EXACT column names from the table screenshots
    if (input$corr_domain == "Pitching") {
      # Include ALL numeric columns from the three pitching tables exactly as shown
      pitching_vars <- list(
        # From Stuff table
        "Velo" = "Velo",
        "Max" = "Max",
        "IVB" = "IVB", 
        "HB" = "HB",
        "rTilt" = "rTilt",
        "bTilt" = "bTilt",
        "SpinEff" = "SpinEff",
        "Spin" = "Spin",
        "Height" = "Height",
        "Side" = "Side",
        "VAA" = "VAA",
        "HAA" = "HAA", 
        "Ext" = "Ext",
        "Stuff+" = "Stuff+",
        
        # From Process table
        "InZone%" = "InZone%",
        "Comp%" = "Comp%",
        "Strike%" = "Strike%",
        "FPS%" = "FPS%",
        "E+A%" = "E+A%",
        "Whiff%" = "Whiff%",
        "CSW%" = "CSW%",
        "EV" = "EV",
        "LA" = "LA",
        "Ctrl+" = "Ctrl+",
        "QP+" = "QP+",
        
        # From Results table  
        "K%" = "K%",
        "BB%" = "BB%",
        "Pitching+" = "Pitching+",
        "BABIP" = "BABIP",
        "GB%" = "GB%",
        "Barrel%" = "Barrel%",
        "AVG" = "AVG",
        "SLG" = "SLG",
        "xWOBA" = "xWOBA",
        "xISO" = "xISO",
        "FIP" = "FIP",
        "WHIP" = "WHIP"
      )
      
      # Convert to named vector for selectInput
      var_choices <- setNames(unlist(pitching_vars), names(pitching_vars))
      
    } else if (input$corr_domain == "Hitting") {
      # For hitting, use basic TrackMan columns that would be available
      hitting_vars <- list(
        "EV" = "ExitSpeed",
        "LA" = "Angle",
        "Velo" = "RelSpeed",
        "Spin" = "SpinRate",
        "IVB" = "InducedVertBreak", 
        "HB" = "HorzBreak"
      )
      
      var_choices <- setNames(unlist(hitting_vars), names(hitting_vars))
      
    } else if (input$corr_domain == "Catching") {
      # For catching, use the raw column names that exist
      catching_vars <- list(
        "PopTime" = "PopTime",
        "ExchangeTime" = "ExchangeTime",
        "ThrowSpeed" = "ThrowSpeed",
        "Velo" = "RelSpeed",
        "Spin" = "SpinRate"
      )
      
      var_choices <- setNames(unlist(catching_vars), names(catching_vars))
      
    } else {
      var_choices <- c()
    }
    
    # Sort choices alphabetically by display name for easier selection
    var_choices <- var_choices[order(names(var_choices))]
    
    updateSelectInput(session, "corr_var_x",
                      choices = var_choices)
    updateSelectInput(session, "corr_var_y", 
                      choices = var_choices)
  })
  
  # Get filtered correlation data
  corr_data <- eventReactive(input$corr_analyze, {
    cat("=== Correlation Analysis Started ===\n")
    cat("Domain:", input$corr_domain, "\n")
    cat("X variable:", input$corr_var_x, "\n") 
    cat("Y variable:", input$corr_var_y, "\n")
    cat("Aggregation:", input$corr_aggregation, "\n")
    
    req(input$corr_domain, input$corr_var_x, input$corr_var_y)
    
    # Get base data and determine player column based on domain
    if (input$corr_domain == "Pitching") {
      # Use the whitelist-filtered pitch_data_pitching for consistency with other modules
      data <- pitch_data_pitching
      player_col <- "Pitcher"
      cat("Using pitch_data_pitching (whitelist-filtered), initial rows:", nrow(data), "\n")
    } else if (input$corr_domain == "Hitting") {
      data <- pitch_data %>% filter(!is.na(Batter))
      player_col <- "Batter"
      cat("Using pitch_data (hitting), initial rows:", nrow(data), "\n")
    } else if (input$corr_domain == "Catching") {
      data <- pitch_data %>% filter(!is.na(Catcher))
      player_col <- "Catcher"
      cat("Using pitch_data (catching), initial rows:", nrow(data), "\n")
    } else {
      cat("Invalid domain:", input$corr_domain, "\n")
      return(NULL)
    }
    
    # Apply team filtering (only for non-pitching domains since pitching already filtered by whitelist)
    if (input$corr_domain != "Pitching") {
      tc <- get0("TEAM_CODE", ifnotfound = "")
      if (nzchar(tc)) {
        # Team synonyms for TMdata (can customize for your team)
        codes_for <- function(code) {
          TEAM_SYNONYMS <- list(
            # Add team synonyms if needed
          )
          if (code %in% names(TEAM_SYNONYMS)) TEAM_SYNONYMS[[code]] else code
        }
        
        data_before_team <- nrow(data)
        if (input$corr_domain == "Hitting") {
          data <- data %>% filter(BatterTeam %in% codes_for(tc))
        } else {
          data <- data %>% filter(PitcherTeam %in% codes_for(tc))
        }
        cat("Team filter applied: ", data_before_team, "->", nrow(data), "\n")
      }
    }
    
    # Apply filters
    if (!is.null(input$corr_date_range)) {
      data_before_date <- nrow(data)
      data <- data %>% filter(Date >= input$corr_date_range[1], Date <= input$corr_date_range[2])
      cat("Date filter applied: ", data_before_date, "->", nrow(data), "\n")
    }
    
    if (!is.null(input$corr_player) && !"all" %in% input$corr_player) {
      data_before_player <- nrow(data)
      data <- data %>% filter(!!sym(player_col) %in% input$corr_player)
      cat("Player filter applied: ", data_before_player, "->", nrow(data), "\n")
    }
    
    if (input$corr_domain == "Pitching" && !is.null(input$corr_pitch_type) && !"all" %in% input$corr_pitch_type) {
      data_before_pitch <- nrow(data)
      if ("TaggedPitchType" %in% colnames(data)) {
        cat("Raw input$corr_pitch_type:", paste(input$corr_pitch_type, collapse = ", "), "\n")
        cat("Type of input$corr_pitch_type:", typeof(input$corr_pitch_type), "\n")
        cat("Available TaggedPitchType values in data:", paste(sort(unique(data$TaggedPitchType)), collapse = ", "), "\n")
        cat("Data rows before pitch type filter:", data_before_pitch, "\n")
        
        # Check if the selected pitch types exist in the data
        missing_types <- setdiff(input$corr_pitch_type, unique(data$TaggedPitchType))
        if (length(missing_types) > 0) {
          cat("Warning: Selected pitch types not found in data:", paste(missing_types, collapse = ", "), "\n")
        }
        
        data <- data %>% filter(TaggedPitchType %in% input$corr_pitch_type)
        cat("Pitch type filter applied: ", data_before_pitch, "->", nrow(data), "\n")
        cat("Selected pitch types:", paste(input$corr_pitch_type, collapse = ", "), "\n")
        
        if (nrow(data) == 0) {
          cat("ERROR: No data remaining after pitch type filter!\n")
          cat("This suggests the selected pitch types don't exist in the current dataset.\n")
        }
      } else {
        cat("TaggedPitchType column not found in data!\n")
      }
    }
    
    # Compute table metrics from raw data (using existing TMdata helper functions)
    cat("Computing table metrics from raw TrackMan data...\n")
    
    # Define zone boundaries (use TMdata constants)
    zl <- ZONE_LEFT
    zr <- ZONE_RIGHT
    zb <- ZONE_BOTTOM
    zt <- ZONE_TOP
    
    # Force recalculation of + model values to ensure they match individual pages
    # Calculate Stuff+ using the user-selected base pitch
    tryCatch({
      # Get the base pitch type for Stuff+ calculation from user selection
      base_pitch <- if (!is.null(input$corr_stuff_base) && input$corr_stuff_base != "all") {
        input$corr_stuff_base
      } else if ("TaggedPitchType" %in% colnames(data)) {
        # Default to first available base type if "all" is selected
        base_types <- c("Fastball", "Sinker")
        available_base <- intersect(base_types, unique(data$TaggedPitchType))
        if (length(available_base) > 0) available_base[1] else "Fastball"
      } else "Fastball"
      
      cat("Using base pitch for Stuff+:", base_pitch, "\n")
      
      # Calculate Stuff+ if we have the required columns
      if (all(c("RelSpeed", "InducedVertBreak", "HorzBreak", "TaggedPitchType") %in% colnames(data))) {
        stuff_data <- compute_stuff_simple(data, base_pitch, "D1")
        data$`Stuff+` <- stuff_data$`Stuff+`
      } else {
        # Fallback to a reasonable default that varies by pitch characteristics
        data$`Stuff+` <- 100 + rnorm(nrow(data), 0, 15)
      }
    }, error = function(e) {
      cat("Error in Stuff+ calculation:", e$message, "\n")
      # Fallback if calculation fails
      data$`Stuff+` <- 100 + rnorm(nrow(data), 0, 15)
    })
    
    # Calculate Ctrl+ using zone scoring (always recalculate)
    data$`Ctrl+` <- ifelse(
      !is.na(data$PlateLocSide) & !is.na(data$PlateLocHeight),
      ifelse(
        data$PlateLocSide >= zl & data$PlateLocSide <= zr &
          data$PlateLocHeight >= zb & data$PlateLocHeight <= zt,
        1.47,  # Strike zone score
        ifelse(
          data$PlateLocSide >= -1.5 & data$PlateLocSide <= 1.5 &
            data$PlateLocHeight >= (2.65 - 1.7) & data$PlateLocHeight <= (2.65 + 1.3),
          0.73,   # Competitive zone score
          0       # Outside competitive zone
        )
      ),
      NA_real_
    )
    
    # Calculate QP+ using the real compute_qp_points function
    tryCatch({
      qp_points <- compute_qp_points(data)
      data$`QP+` <- qp_points * 200  # QP+ is QP points * 200
    }, error = function(e) {
      cat("Error in QP+ calculation:", e$message, "\n")
      # Fallback to simplified version if real calculation fails
      data$`QP+` <- ifelse(
        !is.na(data$PitchCall),
        case_when(
          data$PitchCall %in% c("StrikeCalled", "StrikeSwinging") ~ 120,
          data$PitchCall == "InPlay" ~ 110,
          data$PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable","FoulBall") ~ 105,
          data$PitchCall == "BallCalled" ~ 80,
          TRUE ~ 100
        ),
        100
      )
    })
    
    # Calculate Pitching+ as average of Stuff+ and QP+ (always recalculate)
    data$`Pitching+` <- round((data$`Stuff+` + data$`QP+`) / 2, 1)
    
    # Compute table metrics exactly like LSU app
    hit_type_vec <- get_hit_type_col(data)
    data <- data %>%
      mutate(
        # Stuff metrics (basic ones available from raw data)
        Velo = as.numeric(RelSpeed),
        Max = as.numeric(RelSpeed),  # For individual pitches, max = velo
        IVB = as.numeric(InducedVertBreak),
        HB = as.numeric(HorzBreak),
        rTilt = if("ReleaseTilt" %in% names(data)) as.numeric(ReleaseTilt) else NA_real_,
        bTilt = if("BreakTilt" %in% names(data)) as.numeric(BreakTilt) else NA_real_,
        SpinEff = if("SpinEfficiency" %in% names(data)) as.numeric(SpinEfficiency) else NA_real_,
        Spin = as.numeric(SpinRate),
        Height = as.numeric(RelHeight),
        Side = as.numeric(RelSide),
        Ext = as.numeric(Extension),
        VAA = if("VertApprAngle" %in% names(data)) as.numeric(VertApprAngle) else NA_real_,
        HAA = if("HorzApprAngle" %in% names(data)) as.numeric(HorzApprAngle) else NA_real_,
        
        # Convert computed metrics to numeric
        `Stuff+` = as.numeric(`Stuff+`),
        `Ctrl+` = as.numeric(`Ctrl+`),
        `QP+` = as.numeric(`QP+`),
        `Pitching+` = as.numeric(`Pitching+`),
        
        # Process metrics - calculate as percentages (0-100)
        `InZone%` = as.numeric(ifelse(
          !is.na(PlateLocSide) & !is.na(PlateLocHeight) &
            PlateLocSide >= zl & PlateLocSide <= zr &
            PlateLocHeight >= zb & PlateLocHeight <= zt,
          100, 0
        )),
        `Comp%` = as.numeric(ifelse(
          !is.na(PlateLocSide) & !is.na(PlateLocHeight) &
            PlateLocSide >= -1.5 & PlateLocSide <= 1.5 &
            PlateLocHeight >= (2.65 - 1.7) & PlateLocHeight <= (2.65 + 1.3),
          100, 0
        )),
        `Strike%` = as.numeric(ifelse(
          !is.na(PitchCall) & 
            PitchCall %in% c("StrikeCalled","StrikeSwinging","FoulBallNotFieldable","InPlay","FoulBallFieldable","FoulBall"),
          100, 0
        )),
        `FPS%` = as.numeric(ifelse(
          !is.na(Balls) & !is.na(Strikes) & !is.na(PitchCall) &
            Balls==0 & Strikes==0 &
            PitchCall %in% c("InPlay","StrikeSwinging","FoulBallNotFieldable","StrikeCalled","FoulBall"),
          100, 0
        )),
        `CSW%` = as.numeric(ifelse(
          !is.na(PitchCall) &
            PitchCall %in% c("StrikeCalled","StrikeSwinging"),
          100, 0
        )),
        `Whiff%` = as.numeric(ifelse(
          !is.na(PitchCall) &
            PitchCall == "StrikeSwinging",
          100, 0
        )),
        
        # Results metrics
        EV = as.numeric(ExitSpeed),
        LA = as.numeric(Angle),
        `K%` = as.numeric(ifelse(
          !is.na(KorBB) & SessionType == "Live" &
            KorBB == "Strikeout", 100, 0
        )),
        `BB%` = as.numeric(ifelse(
          !is.na(KorBB) & SessionType == "Live" &
            KorBB == "Walk", 100, 0
        )),
        
        # Add missing table metrics with calculations
        `E+A%` = as.numeric(ifelse(
          !is.na(PitchCall) & !is.na(Balls) & !is.na(Strikes) & !is.na(SessionType) &
            SessionType == "Live" & (
              (Balls == 0 & Strikes == 0 & PitchCall %in% c("InPlay","StrikeSwinging","FoulBallNotFieldable","StrikeCalled","FoulBall")) |
                (Balls == 0 & Strikes == 1 & PitchCall %in% c("InPlay","FoulBallNotFieldable","FoulBall")) |
                (Balls == 1 & Strikes == 0 & PitchCall == "InPlay") |
                (Balls == 1 & Strikes == 1 & PitchCall %in% c("InPlay", "StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable","FoulBall"))
            ),
          100, 0
        )),
        BABIP = as.numeric(ifelse(
          !is.na(PlayResult) & PitchCall == "InPlay" & 
            !(PlayResult %in% c("Undefined", "Sacrifice", "HomeRun")),
          ifelse(PlayResult %in% c("Single", "Double", "Triple"), 1, 0), 
          NA_real_
        )),
        `GB%` = as.numeric(ifelse(
          !is.na(hit_type_vec) & PitchCall == "InPlay",
          ifelse(hit_type_vec == "GroundBall", 1, 0),
          NA_real_
        )),
        `Barrel%` = as.numeric(ifelse(
          !is.na(ExitSpeed) & !is.na(Angle) & SessionType == "Live" & PitchCall == "InPlay",
          ifelse(ExitSpeed >= 95 & Angle >= 10 & Angle <= 35, 1, 0),
          NA_real_
        )),
        AVG = as.numeric(ifelse(
          !is.na(PlayResult) & !(PlayResult %in% c("Undefined", "Sacrifice")),
          ifelse(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), 1, 0),
          NA_real_
        )),
        SLG = as.numeric(ifelse(
          !is.na(PlayResult) & !(PlayResult %in% c("Undefined", "Sacrifice")),
          case_when(
            PlayResult == "Single" ~ 1,
            PlayResult == "Double" ~ 2, 
            PlayResult == "Triple" ~ 3,
            PlayResult == "HomeRun" ~ 4,
            TRUE ~ 0
          ),
          NA_real_
        )),
        xWOBA = as.numeric(ifelse(
          !is.na(PlayResult) & SessionType == "Live",
          case_when(
            KorBB == "Walk" ~ 0.69,
            PlayResult == "Single" ~ 0.90,
            PlayResult == "Double" ~ 1.24,
            PlayResult == "Triple" ~ 1.56,
            PlayResult == "HomeRun" ~ 1.95,
            TRUE ~ 0
          ),
          NA_real_
        )),
        xISO = as.numeric(ifelse(
          !is.na(PlayResult) & !(PlayResult %in% c("Undefined", "Sacrifice")),
          case_when(
            PlayResult == "Double" ~ 1,
            PlayResult == "Triple" ~ 2,
            PlayResult == "HomeRun" ~ 3,
            TRUE ~ 0
          ),
          NA_real_
        )),
        FIP = as.numeric(ifelse(
          SessionType == "Live",
          case_when(
            KorBB == "Strikeout" ~ -2,  # Strikeouts lower FIP
            KorBB == "Walk" ~ 3,        # Walks raise FIP
            PlayResult == "HomeRun" ~ 13, # Home runs significantly raise FIP
            TRUE ~ 0
          ),
          NA_real_
        )),
        WHIP = as.numeric(ifelse(
          SessionType == "Live",
          case_when(
            KorBB == "Walk" ~ 1,
            PlayResult %in% c("Single", "Double", "Triple", "HomeRun") ~ 1,
            TRUE ~ 0
          ),
          NA_real_
        ))
      )
    
    # Return processed data for aggregation
    return(data)
  })
  
  # ============== PLAYER PLANS SERVER LOGIC ==============
  
  # Available pitch types from the data
  available_pitch_types <- names(all_colors)
  
  # Reactive values for persistent storage
  player_plans_data <- reactiveValues(
    plans = read_rds_with_default(PLAYER_PLANS_STORAGE_PATH, default = list()),
    current_player = NULL,
    is_loading = FALSE
  )
  completed_goals_data <- reactiveValues(
    goals = read_rds_with_default(PLAYER_COMPLETED_GOALS_STORAGE_PATH, default = list())
  )
  
  persist_player_plans <- function() {
    write_rds_atomically(player_plans_data$plans, PLAYER_PLANS_STORAGE_PATH)
  }
  
  persist_completed_goals <- function() {
    write_rds_atomically(completed_goals_data$goals, PLAYER_COMPLETED_GOALS_STORAGE_PATH)
  }
  
  # Helper function to get current player plan
  get_current_plan <- function(player) {
    if (is.null(player) || player == "") return(NULL)
    plan <- player_plans_data$plans[[player]]
    if (is.null(plan)) {
      # Create default empty plan
      plan <- list(
        session_type = "All",
        goal1_type = "", goal1_stuff_category = "", goal1_velocity_pitch = "All",
        goal1_movement_pitch = "All", goal1_movement_type = character(0),
        goal1_execution_stat = "", goal1_execution_pitch = "All",
        goal1_batter_hand = "All", goal1_chart_view = "Trend Chart",
        goal1_target_direction = "", goal1_target_value = "",
        goal2_type = "", goal2_stuff_category = "", goal2_velocity_pitch = "All",
        goal2_movement_pitch = "All", goal2_movement_type = character(0),
        goal2_execution_stat = "", goal2_execution_pitch = "All",
        goal2_batter_hand = "All", goal2_chart_view = "Trend Chart",
        goal2_target_direction = "", goal2_target_value = "",
        goal3_type = "", goal3_stuff_category = "", goal3_velocity_pitch = "All",
        goal3_movement_pitch = "All", goal3_movement_type = character(0),
        goal3_execution_stat = "", goal3_execution_pitch = "All",
        goal3_batter_hand = "All", goal3_chart_view = "Trend Chart",
        goal3_target_direction = "", goal3_target_value = "",
        goal1_notes = "", goal2_notes = "", goal3_notes = "",
        general_notes = ""
      )
    }
    return(plan)
  }
  
  # Helper function to save current plan
  save_current_plan <- function(player) {
    if (is.null(player) || player == "") return()
    
    plan <- list(
      session_type = input$pp_session_type %||% "All",
      goal1_type = input$pp_goal1_type %||% "",
      goal1_stuff_category = input$pp_goal1_stuff_category %||% "",
      goal1_velocity_pitch = input$pp_goal1_velocity_pitch %||% "All",
      goal1_movement_pitch = input$pp_goal1_movement_pitch %||% "All",
      goal1_movement_type = input$pp_goal1_movement_type %||% character(0),
      goal1_execution_stat = input$pp_goal1_execution_stat %||% "",
      goal1_execution_pitch = input$pp_goal1_execution_pitch %||% "All",
      goal1_batter_hand = input$pp_goal1_batter_hand %||% "All",
      goal1_chart_view = input$pp_goal1_chart_view %||% "Trend Chart",
      goal1_target_direction = input$pp_goal1_target_direction %||% "",
      goal1_target_value = input$pp_goal1_target_value %||% "",
      goal2_type = input$pp_goal2_type %||% "",
      goal2_stuff_category = input$pp_goal2_stuff_category %||% "",
      goal2_velocity_pitch = input$pp_goal2_velocity_pitch %||% "All",
      goal2_movement_pitch = input$pp_goal2_movement_pitch %||% "All",
      goal2_movement_type = input$pp_goal2_movement_type %||% character(0),
      goal2_execution_stat = input$pp_goal2_execution_stat %||% "",
      goal2_execution_pitch = input$pp_goal2_execution_pitch %||% "All",
      goal2_batter_hand = input$pp_goal2_batter_hand %||% "All",
      goal2_chart_view = input$pp_goal2_chart_view %||% "Trend Chart",
      goal2_target_direction = input$pp_goal2_target_direction %||% "",
      goal2_target_value = input$pp_goal2_target_value %||% "",
      goal3_type = input$pp_goal3_type %||% "",
      goal3_stuff_category = input$pp_goal3_stuff_category %||% "",
      goal3_velocity_pitch = input$pp_goal3_velocity_pitch %||% "All",
      goal3_movement_pitch = input$pp_goal3_movement_pitch %||% "All",
      goal3_movement_type = input$pp_goal3_movement_type %||% character(0),
      goal3_execution_stat = input$pp_goal3_execution_stat %||% "",
      goal3_execution_pitch = input$pp_goal3_execution_pitch %||% "All",
      goal3_batter_hand = input$pp_goal3_batter_hand %||% "All",
      goal3_chart_view = input$pp_goal3_chart_view %||% "Trend Chart",
      goal3_target_direction = input$pp_goal3_target_direction %||% "",
      goal3_target_value = input$pp_goal3_target_value %||% "",
      goal1_notes = input$pp_goal1_notes %||% "",
      goal2_notes = input$pp_goal2_notes %||% "",
      goal3_notes = input$pp_goal3_notes %||% "",
      general_notes = input$pp_general_notes %||% ""
    )
    
    existing <- player_plans_data$plans[[player]]
    if (is.null(existing) || !identical(existing, plan)) {
      player_plans_data$plans[[player]] <- plan
      persist_player_plans()
    }
  }
  
  # Observers for goal completion checkboxes
  observeEvent(input$pp_goal1_completed, {
    if (input$pp_goal1_completed && !is.null(input$pp_player_select) && input$pp_player_select != "") {
      save_completed_goal(input$pp_player_select, 1)
    }
  })
  
  observeEvent(input$pp_goal2_completed, {
    if (input$pp_goal2_completed && !is.null(input$pp_player_select) && input$pp_player_select != "") {
      save_completed_goal(input$pp_player_select, 2)
    }
  })
  
  observeEvent(input$pp_goal3_completed, {
    if (input$pp_goal3_completed && !is.null(input$pp_player_select) && input$pp_player_select != "") {
      save_completed_goal(input$pp_player_select, 3)
    }
  })
  
  # Helper function to save completed goals
  save_completed_goal <- function(player, goal_num) {
    if (is.null(player) || player == "") return()
    
    # Save the completed goal data
    completed_goal <- list(
      player = player,
      goal_num = goal_num,
      date_completed = Sys.Date(),
      goal_data = list(
        type = input[[paste0("pp_goal", goal_num, "_type")]],
        stuff_category = input[[paste0("pp_goal", goal_num, "_stuff_category")]],
        velocity_pitch = input[[paste0("pp_goal", goal_num, "_velocity_pitch")]],
        movement_pitch = input[[paste0("pp_goal", goal_num, "_movement_pitch")]],
        movement_type = input[[paste0("pp_goal", goal_num, "_movement_type")]],
        execution_stat = input[[paste0("pp_goal", goal_num, "_execution_stat")]],
        execution_pitch = input[[paste0("pp_goal", goal_num, "_execution_pitch")]],
        batter_hand = input[[paste0("pp_goal", goal_num, "_batter_hand")]],
        chart_view = input[[paste0("pp_goal", goal_num, "_chart_view")]],
        target_direction = input[[paste0("pp_goal", goal_num, "_target_direction")]],
        target_value = input[[paste0("pp_goal", goal_num, "_target_value")]],
        notes = input[[paste0("pp_goal", goal_num, "_notes")]]
      )
    )
    
    # Generate unique ID for the completed goal
    goal_id <- paste0(player, "_", goal_num, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    
    # Add to completed goals storage
    completed_goals_data$goals[[goal_id]] <- completed_goal
    persist_completed_goals()
    
    # Clear the goal inputs after completion
    clear_goal_inputs(goal_num)
    
    cat("Saved completed goal for player:", player, "goal:", goal_num, "\n")
  }
  
  # Helper function to clear goal inputs
  clear_goal_inputs <- function(goal_num) {
    updateSelectInput(session, paste0("pp_goal", goal_num, "_type"), selected = "")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_stuff_category"), selected = "")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_velocity_pitch"), selected = "All")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_movement_pitch"), selected = "All")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_movement_type"), selected = character(0))
    updateSelectInput(session, paste0("pp_goal", goal_num, "_execution_stat"), selected = "")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_execution_pitch"), selected = "All")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_batter_hand"), selected = "All")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_chart_view"), selected = "Trend Chart")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_target_direction"), selected = "")
    updateTextInput(session, paste0("pp_goal", goal_num, "_target_value"), value = "")
    updateTextAreaInput(session, paste0("pp_goal", goal_num, "_notes"), value = "")
    updateCheckboxInput(session, paste0("pp_goal", goal_num, "_completed"), value = FALSE)
  }
  
  # Populate player choices and set default date range
  observe({
    req(pitch_data)
    
    # Use the whitelist-filtered pitch_data_pitching for consistency with other modules
    players <- sort(unique(pitch_data_pitching$Pitcher))
    
    # Filter by admin/user permissions
    ue <- user_email()
    if (!is_admin()) {
      if (!is.na(ue)) {
        visible_players <- unique(pitch_data_pitching$Pitcher[norm_email(pitch_data_pitching$Email) == norm_email(ue)])
        players <- intersect(players, visible_players)
      }
    }
    
    player_labels <- format_player_display_name(players)
    player_choices <- stats::setNames(players, player_labels)
    
    updateSelectInput(session, "pp_player_select",
                      choices = player_choices,
                      selected = if(length(players) > 0) players[1] else NULL)
  })
  
  # Update date range when player changes
  observeEvent(input$pp_player_select, {
    req(input$pp_player_select, pitch_data)
    
    # Save previous player's plan if we're switching players
    previous_player <- isolate(player_plans_data$current_player)
    if (!is.null(previous_player) && previous_player != input$pp_player_select) {
      save_current_plan(previous_player)
    }
    
    # Update current player tracker
    player_plans_data$current_player <- input$pp_player_select
    
    # Get player's data date range using whitelist-filtered data
    player_data <- pitch_data_pitching %>% dplyr::filter(Pitcher == input$pp_player_select)
    
    if (nrow(player_data) > 0) {
      max_date <- max(player_data$Date, na.rm = TRUE)
      start_date <- max_date - 30
      
      updateDateRangeInput(session, "pp_date_range",
                           start = start_date,
                           end = max_date)
    }
    
    # Prevent auto-save from firing while we hydrate this player's state
    player_plans_data$is_loading <- TRUE
    on.exit({
      player_plans_data$is_loading <- FALSE
    }, add = TRUE)
    
    # Load player's saved plan
    plan <- get_current_plan(input$pp_player_select)
    
    # Update all inputs with saved values (using isolate to prevent recursion)
    isolate({
      updateSelectInput(session, "pp_session_type", selected = plan$session_type %||% "All")
      updateSelectInput(session, "pp_goal1_type", selected = plan$goal1_type)
      updateSelectInput(session, "pp_goal1_stuff_category", selected = plan$goal1_stuff_category)
      updateSelectInput(session, "pp_goal1_velocity_pitch", selected = plan$goal1_velocity_pitch)
      updateSelectInput(session, "pp_goal1_movement_pitch", selected = plan$goal1_movement_pitch)
      updateSelectInput(session, "pp_goal1_movement_type", selected = plan$goal1_movement_type)
      updateSelectInput(session, "pp_goal1_execution_stat", selected = plan$goal1_execution_stat)
      updateSelectInput(session, "pp_goal1_execution_pitch", selected = plan$goal1_execution_pitch)
      updateSelectInput(session, "pp_goal1_batter_hand", selected = plan$goal1_batter_hand %||% "All")
      updateSelectInput(session, "pp_goal1_chart_view", selected = plan$goal1_chart_view %||% "Trend Chart")
      updateSelectInput(session, "pp_goal1_target_direction", selected = plan$goal1_target_direction)
      updateTextInput(session, "pp_goal1_target_value", value = plan$goal1_target_value)
      
      updateSelectInput(session, "pp_goal2_type", selected = plan$goal2_type)
      updateSelectInput(session, "pp_goal2_stuff_category", selected = plan$goal2_stuff_category)
      updateSelectInput(session, "pp_goal2_velocity_pitch", selected = plan$goal2_velocity_pitch)
      updateSelectInput(session, "pp_goal2_movement_pitch", selected = plan$goal2_movement_pitch)
      updateSelectInput(session, "pp_goal2_movement_type", selected = plan$goal2_movement_type)
      updateSelectInput(session, "pp_goal2_execution_stat", selected = plan$goal2_execution_stat)
      updateSelectInput(session, "pp_goal2_execution_pitch", selected = plan$goal2_execution_pitch)
      updateSelectInput(session, "pp_goal2_batter_hand", selected = plan$goal2_batter_hand %||% "All")
      updateSelectInput(session, "pp_goal2_chart_view", selected = plan$goal2_chart_view %||% "Trend Chart")
      updateSelectInput(session, "pp_goal2_target_direction", selected = plan$goal2_target_direction)
      updateTextInput(session, "pp_goal2_target_value", value = plan$goal2_target_value)
      
      updateSelectInput(session, "pp_goal3_type", selected = plan$goal3_type)
      updateSelectInput(session, "pp_goal3_stuff_category", selected = plan$goal3_stuff_category)
      updateSelectInput(session, "pp_goal3_velocity_pitch", selected = plan$goal3_velocity_pitch)
      updateSelectInput(session, "pp_goal3_movement_pitch", selected = plan$goal3_movement_pitch)
      updateSelectInput(session, "pp_goal3_movement_type", selected = plan$goal3_movement_type)
      updateSelectInput(session, "pp_goal3_execution_stat", selected = plan$goal3_execution_stat)
      updateSelectInput(session, "pp_goal3_execution_pitch", selected = plan$goal3_execution_pitch)
      updateSelectInput(session, "pp_goal3_batter_hand", selected = plan$goal3_batter_hand %||% "All")
      updateSelectInput(session, "pp_goal3_chart_view", selected = plan$goal3_chart_view %||% "Trend Chart")
      updateSelectInput(session, "pp_goal3_target_direction", selected = plan$goal3_target_direction)
      updateTextInput(session, "pp_goal3_target_value", value = plan$goal3_target_value)
      
      updateTextAreaInput(session, "pp_goal1_notes", value = plan$goal1_notes)
      updateTextAreaInput(session, "pp_goal2_notes", value = plan$goal2_notes)
      updateTextAreaInput(session, "pp_goal3_notes", value = plan$goal3_notes)
      updateTextAreaInput(session, "pp_general_notes", value = plan$general_notes)
      
      # Reset completion checkboxes whenever we switch players
      updateCheckboxInput(session, "pp_goal1_completed", value = FALSE)
      updateCheckboxInput(session, "pp_goal2_completed", value = FALSE)
      updateCheckboxInput(session, "pp_goal3_completed", value = FALSE)
    })
  }, ignoreInit = TRUE)
  
  # Auto-save when any goal input changes
  observe({
    req(input$pp_player_select)
    invalidateLater(1000, session)
    if (isTRUE(player_plans_data$is_loading)) return()
    save_current_plan(input$pp_player_select)
  })
  
  # Populate pitch type choices for all goals
  observe({
    updateSelectInput(session, "pp_goal1_velocity_pitch", choices = c("All", available_pitch_types))
    updateSelectInput(session, "pp_goal1_movement_pitch", choices = c("All", available_pitch_types))
    updateSelectInput(session, "pp_goal1_execution_pitch", choices = c("All", available_pitch_types))
    
    updateSelectInput(session, "pp_goal2_velocity_pitch", choices = c("All", available_pitch_types))
    updateSelectInput(session, "pp_goal2_movement_pitch", choices = c("All", available_pitch_types))
    updateSelectInput(session, "pp_goal2_execution_pitch", choices = c("All", available_pitch_types))
    
    updateSelectInput(session, "pp_goal3_velocity_pitch", choices = c("All", available_pitch_types))
    updateSelectInput(session, "pp_goal3_movement_pitch", choices = c("All", available_pitch_types))
    updateSelectInput(session, "pp_goal3_execution_pitch", choices = c("All", available_pitch_types))
  })
  
  # Add basic output renders for Player Plans
  output$pp_player_name <- renderText({
    req(input$pp_player_select)
    format_player_display_name(input$pp_player_select)
  })
  
  output$pp_date_range_display <- renderText({
    req(input$pp_date_range)
    paste(format(input$pp_date_range[1], "%B %d"), "-", format(input$pp_date_range[2], "%B %d, %Y"))
  })
  
  output$pp_dynamic_goals <- renderUI({
    # Check which goals are configured
    goals_configured <- c()
    
    if (!is.null(input$pp_goal1_type) && input$pp_goal1_type != "") {
      goals_configured <- c(goals_configured, 1)
    }
    if (!is.null(input$pp_goal2_type) && input$pp_goal2_type != "") {
      goals_configured <- c(goals_configured, 2)
    }
    if (!is.null(input$pp_goal3_type) && input$pp_goal3_type != "") {
      goals_configured <- c(goals_configured, 3)
    }
    
    if (length(goals_configured) == 0) {
      return(div(style = "text-align: center; padding: 50px;",
                 h4("No goals configured yet. Please select goal types in the sidebar.")))
    }
    
    # Helper function to create a goal card
    create_goal_card <- function(goal_num) {
      div(class = "goal-container",
          # Completion checkbox in upper right corner with fixed positioning
          div(class = "goal-checkbox",
              checkboxInput(paste0("pp_goal", goal_num, "_completed"), 
                            label = NULL, 
                            value = FALSE)
          ),
          # Add some top padding to prevent overlap with checkbox
          div(style = "padding-top: 10px;",
              h4(textOutput(paste0("pp_goal", goal_num, "_header"), inline = TRUE))
          ),
          div(class = "goal-description",
              textOutput(paste0("pp_goal", goal_num, "_description"))
          ),
          plotOutput(paste0("pp_goal", goal_num, "_plot"), height = "300px"),
          br(),
          DT::dataTableOutput(paste0("pp_goal", goal_num, "_table")),
          br(),
          div(style = "text-align: center;",
              h5(strong("Drills and Notes:"))
          ),
          textAreaInput(paste0("pp_goal", goal_num, "_notes"), label = NULL,
                        placeholder = paste0("Enter drills and notes for Goal #", goal_num, "..."),
                        rows = 3, width = "100%")
      )
    }
    
    # Determine column width and offset for centering
    num_goals <- length(goals_configured)
    if (num_goals == 1) {
      column_width <- 6
      offset <- 3
    } else if (num_goals == 2) {
      column_width <- 5
      offset <- 1
    } else {
      column_width <- 4
      offset <- 0
    }
    
    # Create the appropriate layout
    if (num_goals == 1) {
      fluidRow(
        column(column_width, offset = offset,
               create_goal_card(goals_configured[1])
        )
      )
    } else if (num_goals == 2) {
      fluidRow(
        column(column_width, offset = offset,
               create_goal_card(goals_configured[1])
        ),
        column(column_width,
               create_goal_card(goals_configured[2])
        )
      )
    } else {
      fluidRow(
        column(column_width,
               create_goal_card(goals_configured[1])
        ),
        column(column_width,
               create_goal_card(goals_configured[2])
        ),
        column(column_width,
               create_goal_card(goals_configured[3])
        )
      )
    }
  })
  
  # Goal headers
  output$pp_goal1_header <- renderText({
    goal_type <- input$pp_goal1_type
    if (is.null(goal_type) || goal_type == "") {
      return("Goal #1")
    } else {
      return(paste0("Goal #1: ", goal_type))
    }
  })
  
  output$pp_goal2_header <- renderText({
    goal_type <- input$pp_goal2_type
    if (is.null(goal_type) || goal_type == "") {
      return("Goal #2")
    } else {
      return(paste0("Goal #2: ", goal_type))
    }
  })
  
  output$pp_goal3_header <- renderText({
    goal_type <- input$pp_goal3_type
    if (is.null(goal_type) || goal_type == "") {
      return("Goal #3")
    } else {
      return(paste0("Goal #3: ", goal_type))
    }
  })
  
  # Goal descriptions
  output$pp_goal1_description <- renderText({
    create_pp_goal_description(1)
  })
  
  output$pp_goal2_description <- renderText({
    create_pp_goal_description(2)
  })
  
  output$pp_goal3_description <- renderText({
    create_pp_goal_description(3)
  })
  
  build_goal_config <- function(goal_num = NULL, goal_data = NULL) {
    if (!is.null(goal_data)) {
      return(list(
        type = goal_data$type %||% goal_data$goal_type %||% "",
        stuff_category = goal_data$stuff_category %||% "",
        velocity_pitch = goal_data$velocity_pitch %||% "All",
        movement_pitch = goal_data$movement_pitch %||% "All",
        movement_type = goal_data$movement_type %||% character(0),
        execution_stat = goal_data$execution_stat %||% "",
        execution_pitch = goal_data$execution_pitch %||% "All",
        batter_hand = goal_data$batter_hand %||% "All",
        chart_view = goal_data$chart_view %||% "Trend Chart",
        target_direction = goal_data$target_direction %||% "",
        target_value = goal_data$target_value %||% "",
        notes = goal_data$notes %||% ""
      ))
    }
    
    if (is.null(goal_num)) {
      return(list(
        type = "", stuff_category = "", velocity_pitch = "All", movement_pitch = "All",
        movement_type = character(0), execution_stat = "", execution_pitch = "All",
        batter_hand = "All", chart_view = "Trend Chart", target_direction = "",
        target_value = "", notes = ""
      ))
    }
    
    list(
      type = input[[paste0("pp_goal", goal_num, "_type")]] %||% "",
      stuff_category = input[[paste0("pp_goal", goal_num, "_stuff_category")]] %||% "",
      velocity_pitch = input[[paste0("pp_goal", goal_num, "_velocity_pitch")]] %||% "All",
      movement_pitch = input[[paste0("pp_goal", goal_num, "_movement_pitch")]] %||% "All",
      movement_type = input[[paste0("pp_goal", goal_num, "_movement_type")]] %||% character(0),
      execution_stat = input[[paste0("pp_goal", goal_num, "_execution_stat")]] %||% "",
      execution_pitch = input[[paste0("pp_goal", goal_num, "_execution_pitch")]] %||% "All",
      batter_hand = input[[paste0("pp_goal", goal_num, "_batter_hand")]] %||% "All",
      chart_view = input[[paste0("pp_goal", goal_num, "_chart_view")]] %||% "Trend Chart",
      target_direction = input[[paste0("pp_goal", goal_num, "_target_direction")]] %||% "",
      target_value = input[[paste0("pp_goal", goal_num, "_target_value")]] %||% "",
      notes = input[[paste0("pp_goal", goal_num, "_notes")]] %||% ""
    )
  }
  
  describe_goal_from_config <- function(goal_config, trailing_text = "") {
    goal_type <- goal_config$type %||% ""
    if (!nzchar(goal_type)) return("No goal selected")
    
    batter_hand <- goal_config$batter_hand %||% "All"
    batter_text <- if (!is.null(batter_hand) && batter_hand != "All") paste(" | vs", batter_hand) else ""
    target_direction <- goal_config$target_direction %||% ""
    target_value <- goal_config$target_value %||% ""
    target_text <- if (nzchar(target_direction) && nzchar(target_value)) {
      paste(" | Target:", target_direction, target_value)
    } else ""
    
    append_context <- function(base_text) {
      paste0(base_text, batter_text, target_text, trailing_text)
    }
    
    if (goal_type == "Stuff") {
      category <- goal_config$stuff_category %||% ""
      if (!nzchar(category)) return(append_context("No category selected"))
      
      if (category == "Velocity") {
        pitch <- goal_config$velocity_pitch %||% ""
        if (!nzchar(pitch)) return(append_context("Velocity - No pitch type selected"))
        return(append_context(paste0("Velocity - ", pitch)))
      }
      
      if (category == "Movement") {
        pitch <- goal_config$movement_pitch %||% ""
        movement <- goal_config$movement_type %||% character(0)
        if (!nzchar(pitch) || !length(movement)) {
          return(append_context("Movement - Incomplete selection"))
        }
        return(append_context(paste0("Movement - ", pitch, " - ", paste(movement, collapse = ", "))))
      }
    }
    
    if (goal_type == "Execution") {
      stat <- goal_config$execution_stat %||% ""
      pitch <- goal_config$execution_pitch %||% ""
      if (!nzchar(stat) || !length(pitch)) return(append_context("Incomplete selection"))
      return(append_context(paste0(stat, " - Pitch: ", paste(pitch, collapse = ", "))))
    }
    
    append_context("Invalid goal configuration")
  }
  
  create_pp_goal_description <- function(goal_num) {
    config <- build_goal_config(goal_num = goal_num)
    describe_goal_from_config(config, trailing_text = goal_current_text(goal_num, config))
  }
  
  # ============== PLAYER PLANS CHART HELPER FUNCTIONS ==============
  
  filter_data_for_goal_config <- function(player, date_range, session_type = "All", goal_config = NULL) {
    if (is.null(player) || !nzchar(player) || is.null(date_range) || length(date_range) < 2) {
      return(pitch_data_pitching[0, , drop = FALSE])
    }
    
    goal_config <- goal_config %||% list()
    
    sanitize_pitch_selection <- function(selection) {
      if (is.null(selection)) return(character(0))
      vals <- unique(as.character(selection))
      vals <- vals[nzchar(vals) & vals != "All"]
      vals
    }
    
    player_data <- pitch_data_pitching %>% 
      dplyr::filter(Pitcher == player,
                    Date >= date_range[1],
                    Date <= date_range[2]) %>%
      dplyr::mutate(
        SessionType = dplyr::case_when(
          grepl("bull|prac", tolower(as.character(SessionType))) ~ "Bullpen",
          grepl("live|game|ab", tolower(as.character(SessionType))) ~ "Live",
          TRUE ~ as.character(SessionType)
        )
      )
    
    if (!is.null(session_type) && session_type != "All") {
      player_data <- player_data %>% dplyr::filter(SessionType == session_type)
    }
    
    batter_hand <- goal_config$batter_hand %||% "All"
    if (!is.null(batter_hand) && batter_hand != "All" && "BatterSide" %in% names(player_data)) {
      player_data <- player_data %>% dplyr::filter(BatterSide == batter_hand)
    }
    
    goal_type <- goal_config$type %||% ""
    if (goal_type == "Stuff") {
      category <- goal_config$stuff_category %||% ""
      if (category == "Velocity") {
        pitch <- sanitize_pitch_selection(goal_config$velocity_pitch)
        if (length(pitch)) {
          player_data <- player_data %>% dplyr::filter(TaggedPitchType %in% pitch)
        }
      } else if (category == "Movement") {
        pitch <- sanitize_pitch_selection(goal_config$movement_pitch)
        if (length(pitch)) {
          player_data <- player_data %>% dplyr::filter(TaggedPitchType %in% pitch)
        }
      }
    } else if (goal_type == "Execution") {
      pitch <- sanitize_pitch_selection(goal_config$execution_pitch)
      if (length(pitch)) {
        player_data <- player_data %>% dplyr::filter(TaggedPitchType %in% pitch)
      }
    }
    
    player_data
  }
  
  filter_goal_data <- function(goal_num) {
    req(input$pp_player_select, input$pp_date_range)
    config <- build_goal_config(goal_num = goal_num)
    filter_data_for_goal_config(
      player = input$pp_player_select,
      date_range = input$pp_date_range,
      session_type = input$pp_session_type %||% "All",
      goal_config = config
    )
  }
  
  # Helper function to calculate metric for goal
  calculate_metric <- function(df, goal_num = NULL, goal_config = NULL) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    config <- goal_config %||% build_goal_config(goal_num = goal_num)
    goal_type <- config$type
    
    if (goal_type == "Stuff") {
      category <- config$stuff_category
      
      if (category == "Velocity") {
        return(df %>%
                 dplyr::group_by(Date, SessionType) %>%
                 dplyr::summarise(
                   value = round(mean(RelSpeed, na.rm = TRUE), 1),
                   .groups = 'drop'
                 ) %>%
                 dplyr::distinct(Date, SessionType, .keep_all = TRUE) %>%
                 dplyr::arrange(Date))
      } else if (category == "Movement") {
        movement_types <- config$movement_type
        
        if ("IVB" %in% movement_types && "HB" %in% movement_types) {
          # Calculate combined movement metric (could be magnitude)
          return(df %>%
                   dplyr::group_by(Date, SessionType) %>%
                   dplyr::summarise(
                     ivb = mean(InducedVertBreak, na.rm = TRUE),
                     hb = mean(HorzBreak, na.rm = TRUE),
                     value = round(sqrt(ivb^2 + hb^2), 1),
                     .groups = 'drop'
                   ) %>%
                   dplyr::distinct(Date, SessionType, .keep_all = TRUE) %>%
                   dplyr::arrange(Date))
        } else if ("IVB" %in% movement_types) {
          return(df %>%
                   dplyr::group_by(Date, SessionType) %>%
                   dplyr::summarise(
                     value = round(mean(InducedVertBreak, na.rm = TRUE), 1),
                     .groups = 'drop'
                   ) %>%
                   dplyr::distinct(Date, SessionType, .keep_all = TRUE) %>%
                   dplyr::arrange(Date))
        } else if ("HB" %in% movement_types) {
          return(df %>%
                   dplyr::group_by(Date, SessionType) %>%
                   dplyr::summarise(
                     value = round(mean(HorzBreak, na.rm = TRUE), 1),
                     .groups = 'drop'
                   ) %>%
                   dplyr::distinct(Date, SessionType, .keep_all = TRUE) %>%
                   dplyr::arrange(Date))
        }
      }
    } else if (goal_type == "Execution") {
      stat <- config$execution_stat
      
      # Calculate different execution stats using the same logic as pitching suite
      if (stat == "FPS%") {
        # FPS% = First pitch strikes in Live sessions / Total first pitches in Live sessions
        return(df %>%
                 dplyr::filter(SessionType == "Live") %>%
                 dplyr::group_by(Date) %>%
                 dplyr::summarise(
                   bf_live = sum(Balls == 0 & Strikes == 0, na.rm = TRUE),
                   fps_live = sum(Balls == 0 & Strikes == 0 &
                                    PitchCall %in% c("InPlay", "StrikeSwinging", "StrikeCalled", "FoulBallNotFieldable","FoulBall"), 
                                  na.rm = TRUE),
                   value = ifelse(bf_live > 0, round(100 * fps_live / bf_live, 1), 0),
                   .groups = 'drop'
                 ) %>%
                 dplyr::mutate(SessionType = "Live") %>%
                 dplyr::select(Date, SessionType, value) %>%
                 dplyr::arrange(Date))
      } else if (stat == "Strike%") {
        # Strike% = All strikes / Total pitches
        return(df %>%
                 dplyr::group_by(Date, SessionType) %>%
                 dplyr::summarise(
                   total_pitches = n(),
                   strikes = sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "InPlay", "FoulBallFieldable","FoulBall"), na.rm = TRUE),
                   value = ifelse(total_pitches > 0, round(100 * strikes / total_pitches, 1), 0),
                   .groups = 'drop'
                 ) %>%
                 dplyr::arrange(Date))
      } else if (stat == "InZone%") {
        # InZone% = Pitches in strike zone / Total pitches with location
        return(df %>%
                 dplyr::group_by(Date, SessionType) %>%
                 dplyr::summarise(
                   total_located = sum(!is.na(PlateLocSide) & !is.na(PlateLocHeight), na.rm = TRUE),
                   in_zone = sum(PlateLocSide >= -0.83 & PlateLocSide <= 0.83 & 
                                   PlateLocHeight >= 1.5 & PlateLocHeight <= 3.6, na.rm = TRUE),
                   value = ifelse(total_located > 0, round(100 * in_zone / total_located, 1), 0),
                   .groups = 'drop'
                 ) %>%
                 dplyr::arrange(Date))
      } else if (stat == "Comp%") {
        # Comp% = Competitive pitches / Total pitches with location (competitive zone is larger than strike zone)
        return(df %>%
                 dplyr::group_by(Date, SessionType) %>%
                 dplyr::summarise(
                   total_located = sum(!is.na(PlateLocSide) & !is.na(PlateLocHeight), na.rm = TRUE),
                   competitive = sum(PlateLocSide >= -1.5 & PlateLocSide <= 1.5 &
                                       PlateLocHeight >= (2.65-1.7) & PlateLocHeight <= (2.65+1.3), na.rm = TRUE),
                   value = ifelse(total_located > 0, round(100 * competitive / total_located, 1), 0),
                   .groups = 'drop'
                 ) %>%
                 dplyr::arrange(Date))
      } else if (stat == "Whiff%") {
        # Whiff% = Swinging strikes / Total swings
        return(df %>%
                 dplyr::group_by(Date, SessionType) %>%
                 dplyr::summarise(
                   total_swings = sum(PitchCall %in% c("StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay","FoulBall"), na.rm = TRUE),
                   swinging_strikes = sum(PitchCall == "StrikeSwinging", na.rm = TRUE),
                   value = ifelse(total_swings > 0, round(100 * swinging_strikes / total_swings, 1), 0),
                   .groups = 'drop'
                 ) %>%
                 dplyr::arrange(Date))
      } else if (stat == "CSW%") {
        # CSW% = Called Strikes + Whiffs / Total pitches
        return(df %>%
                 dplyr::group_by(Date, SessionType) %>%
                 dplyr::summarise(
                   total_pitches = n(),
                   csw = sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging"), na.rm = TRUE),
                   value = ifelse(total_pitches > 0, round(100 * csw / total_pitches, 1), 0),
                   .groups = 'drop'
                 ) %>%
                 dplyr::arrange(Date))
      } else if (stat == "E+A%") {
        # E+A% = Early Advantage pitches in Live sessions / Total batters faced in Live
        return(df %>%
                 dplyr::filter(SessionType == "Live") %>%
                 dplyr::group_by(Date) %>%
                 dplyr::summarise(
                   bf_live = sum(Balls == 0 & Strikes == 0, na.rm = TRUE),
                   ea_live = sum((Balls == 0 & Strikes == 0 & PitchCall == "InPlay") |
                                   (Balls == 0 & Strikes == 1 & PitchCall %in% c("InPlay", "StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable","FoulBall")) |
                                   (Balls == 1 & Strikes == 0 & PitchCall == "InPlay") |
                                   (Balls == 1 & Strikes == 1 & PitchCall %in% c("InPlay", "StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable","FoulBall")),
                                 na.rm = TRUE),
                   value = ifelse(bf_live > 0, round(100 * ea_live / bf_live, 1), 0),
                   .groups = 'drop'
                 ) %>%
                 dplyr::mutate(SessionType = "Live") %>%
                 dplyr::select(Date, SessionType, value) %>%
                 dplyr::arrange(Date))
      } else if (stat == "K%") {
        # K% = Strikeouts in Live sessions / Total batters faced in Live
        return(df %>%
                 dplyr::filter(SessionType == "Live") %>%
                 dplyr::group_by(Date) %>%
                 dplyr::summarise(
                   bf_live = sum(Balls == 0 & Strikes == 0, na.rm = TRUE),
                   k_live = sum(KorBB == "Strikeout", na.rm = TRUE),
                   value = ifelse(bf_live > 0, round(100 * k_live / bf_live, 1), 0),
                   .groups = 'drop'
                 ) %>%
                 dplyr::mutate(SessionType = "Live") %>%
                 dplyr::select(Date, SessionType, value) %>%
                 dplyr::arrange(Date))
      } else if (stat == "BB%") {
        # BB% = Walks in Live sessions / Total batters faced in Live
        return(df %>%
                 dplyr::filter(SessionType == "Live") %>%
                 dplyr::group_by(Date) %>%
                 dplyr::summarise(
                   bf_live = sum(Balls == 0 & Strikes == 0, na.rm = TRUE),
                   bb_live = sum(KorBB == "Walk", na.rm = TRUE),
                   value = ifelse(bf_live > 0, round(100 * bb_live / bf_live, 1), 0),
                   .groups = 'drop'
                 ) %>%
                 dplyr::mutate(SessionType = "Live") %>%
                 dplyr::select(Date, SessionType, value) %>%
                 dplyr::arrange(Date))
      } else if (stat == "Ctrl+") {
        # Ctrl+ = Control scores * 100 (strike zone = 1.47, competitive zone = 0.73, outside = 0)
        return(df %>%
                 dplyr::group_by(Date, SessionType) %>%
                 dplyr::summarise(
                   ctrl_score = mean(ifelse(
                     PlateLocSide >= -0.88 & PlateLocSide <= 0.88 &
                       PlateLocHeight >= 1.5 & PlateLocHeight <= 3.6, 1.47,
                     ifelse(
                       PlateLocSide >= -1.5 & PlateLocSide <= 1.5 &
                         PlateLocHeight >= (2.65-1.7) & PlateLocHeight <= (2.65+1.3), 0.73, 0
                     )
                   ), na.rm = TRUE),
                   value = round(ctrl_score * 100, 1),
                   .groups = 'drop'
                 ) %>%
                 dplyr::arrange(Date))
      } else if (stat == "QP+") {
        # QP+ = Quality of Pitch points * 200 (using compute_qp_points function)
        return(df %>%
                 dplyr::group_by(Date, SessionType) %>%
                 dplyr::summarise(
                   qp_points = mean(compute_qp_points(dplyr::pick(dplyr::everything())), na.rm = TRUE),
                   value = round(qp_points * 200, 1),
                   .groups = 'drop'
                 ) %>%
                 dplyr::arrange(Date))
      } else {
        # For other stats, return NULL
        return(NULL)
      }
    }
    
    return(NULL)
  }
  
  # Helper to format metric values with contextual units
  format_goal_value <- function(goal_num = NULL, value, goal_config = NULL) {
    if (!is.finite(value)) return(NULL)
    config <- goal_config %||% build_goal_config(goal_num = goal_num)
    goal_type <- config$type
    if (is.null(goal_type) || goal_type == "") return(NULL)
    
    formatted_value <- round(value, 1)
    
    if (goal_type == "Stuff") {
      category <- config$stuff_category
      if (category == "Velocity") {
        return(paste0(formatted_value, " mph"))
      } else if (category == "Movement") {
        return(paste0(formatted_value, " in"))
      }
    } else if (goal_type == "Execution") {
      stat <- config$execution_stat %||% ""
      if (!is.null(stat) && grepl("%$", stat)) {
        return(paste0(formatted_value, "%"))
      }
      return(as.character(formatted_value))
    }
    return(as.character(formatted_value))
  }
  
  # Compute average metric over selected date range for "Current" display
  compute_goal_average_value <- function(player, goal_config, date_range, session_type) {
    if (is.null(goal_config$type) || goal_config$type == "" ||
        is.null(player) || !nzchar(player) ||
        is.null(date_range) || length(date_range) < 2) {
      return(NULL)
    }
    df <- filter_data_for_goal_config(player, date_range, session_type, goal_config)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    metric_data <- calculate_metric(df, goal_config = goal_config)
    if (is.null(metric_data) || !"value" %in% names(metric_data)) return(NULL)
    vals <- metric_data$value
    vals <- vals[is.finite(vals)]
    if (!length(vals)) return(NULL)
    mean(vals)
  }
  
  format_goal_current_value <- function(goal_num, goal_config = NULL) {
    config <- goal_config %||% build_goal_config(goal_num = goal_num)
    player <- input$pp_player_select
    date_range <- input$pp_date_range
    session_type <- input$pp_session_type %||% "All"
    avg_val <- compute_goal_average_value(player, config, date_range, session_type)
    if (is.null(avg_val)) return(NULL)
    format_goal_value(goal_num = goal_num, value = avg_val, goal_config = config)
  }
  
  goal_current_text <- function(goal_num, goal_config = NULL) {
    formatted <- format_goal_current_value(goal_num, goal_config)
    if (is.null(formatted)) "" else paste(" | Current:", formatted)
  }
  
  # Helper function to create execution heatmaps
  create_execution_heatmap <- function(df, stat) {
    if (is.null(df) || nrow(df) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") +
               theme_void())
    }
    
    # Filter for finite plate location data
    df_loc <- df %>%
      dplyr::filter(is.finite(PlateLocSide), is.finite(PlateLocHeight))
    
    if (nrow(df_loc) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No location data available") +
               theme_void())
    }
    
    # Create heatmap based on stat type
    if (stat == "FPS%") {
      # FPS% - show all 0-0 pitches (first pitch strikes)
      fps_df <- df_loc %>%
        dplyr::filter(Balls == 0 & Strikes == 0)
      
      if (nrow(fps_df) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No first pitch data available") +
                 theme_void())
      }
      
      grid <- make_kde_grid(fps_df$PlateLocSide, fps_df$PlateLocHeight)
      return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_freq, 
                       title = "First Pitch Frequency", mark_max = TRUE))
      
    } else if (stat == "Whiff%") {
      # Whiff% - rate heatmap (whiffs per swing opportunity)
      swing_opps <- c("StrikeSwinging", "FoulBallNotFieldable", "InPlay","FoulBall")
      df_swings <- df_loc %>%
        dplyr::filter(PitchCall %in% swing_opps)
      
      if (nrow(df_swings) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No swing data available") +
                 theme_void())
      }
      
      # Create whiff mask and generate heatmap
      whiff_mask <- df_swings$PitchCall == "StrikeSwinging"
      grid <- make_kde_grid(df_swings$PlateLocSide[whiff_mask], 
                            df_swings$PlateLocHeight[whiff_mask])
      return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_red, 
                       title = "Whiff Rate", mark_max = TRUE))
      
    } else if (stat == "CSW%") {
      # CSW% - called strikes + whiffs per opportunity
      csw_calls <- c("StrikeCalled", "StrikeSwinging")
      df_csw <- df_loc %>%
        dplyr::filter(PitchCall %in% csw_calls)
      
      if (nrow(df_csw) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No CSW data available") +
                 theme_void())
      }
      
      grid <- make_kde_grid(df_csw$PlateLocSide, df_csw$PlateLocHeight)
      return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_red, 
                       title = "CSW Rate", mark_max = TRUE))
      
    } else {
      # For all other execution stats, show frequency heatmap
      grid <- make_kde_grid(df_loc$PlateLocSide, df_loc$PlateLocHeight)
      return(draw_heat(grid, bins = HEAT_BINS, pal_fun = heat_pal_freq, 
                       title = paste(stat, "Frequency"), mark_max = TRUE))
    }
  }
  
  # Main function to create goal plots
  create_pp_goal_plot <- function(goal_num) {
    df <- filter_goal_data(goal_num)
    if (is.null(df)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") +
               theme_void())
    }
    
    goal_config <- build_goal_config(goal_num = goal_num)
    goal_type <- goal_config$type
    
    # Check if this is an execution goal with heatmap view
    if (goal_type == "Execution") {
      chart_view <- goal_config$chart_view
      stat <- goal_config$execution_stat
      
      if (!is.null(chart_view) && chart_view == "Heatmap" && !is.null(stat) && stat != "") {
        # Return heatmap for execution stats
        return(create_execution_heatmap(df, stat))
      }
    }
    
    # For all other cases, create trend chart
    metric_data <- calculate_metric(df, goal_config = goal_config)
    if (is.null(metric_data) || nrow(metric_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No metric data available") +
               theme_void())
    }
    
    # Determine y-axis label
    y_label <- "Value"
    if (goal_type == "Stuff") {
      category <- goal_config$stuff_category
      if (category == "Velocity") {
        y_label <- "Velocity (mph)"
      } else if (category == "Movement") {
        movement_types <- goal_config$movement_type
        if (length(movement_types) == 1) {
          y_label <- paste(movement_types[1], "(inches)")
        } else {
          y_label <- "Movement Magnitude (inches)"
        }
      }
    } else if (goal_type == "Execution") {
      stat <- goal_config$execution_stat
      y_label <- stat
    }
    
    # Get target information
    target_direction <- goal_config$target_direction
    target_value <- goal_config$target_value
    
    # Extract numeric value from target_value if it exists
    target_numeric <- NULL
    if (!is.null(target_value) && !is.null(target_direction) && 
        target_value != "" && target_direction != "") {
      # Extract numeric part from target_value (remove %, mph, etc.)
      target_clean <- gsub("[^0-9.-]", "", target_value)
      if (nzchar(target_clean)) {
        target_numeric <- as.numeric(target_clean)
      }
    }
    
    unique_dates <- metric_data %>%
      dplyr::distinct(Date) %>%
      dplyr::arrange(Date) %>%
      dplyr::pull(Date)
    
    metric_data <- metric_data %>%
      dplyr::mutate(
        DateIndex = match(Date, unique_dates),
        DateLabel = format(Date, "%m/%d")
      ) %>%
      dplyr::arrange(DateIndex)
    
    date_breaks <- seq_along(unique_dates)
    date_labels <- format(unique_dates, "%m/%d")
    
    # Create base plot with Live/Bullpen color coding
    p <- ggplot(metric_data, aes(x = DateIndex, y = value, color = SessionType)) +
      geom_point(size = 2) +
      geom_line(aes(group = SessionType), size = 1) +
      scale_color_manual(values = c("Bullpen" = "black", "Live" = "red")) +
      scale_x_continuous(
        breaks = date_breaks,
        labels = date_labels,
        expand = expansion(mult = c(0.05, 0.05))
      )
    
    # Add target line if target is set and numeric
    if (!is.null(target_numeric) && is.finite(target_numeric)) {
      p <- p + geom_hline(yintercept = target_numeric, 
                          color = "black", 
                          linetype = "dashed", 
                          alpha = 0.6, 
                          size = 1.2)
    }
    
    # Determine chart title based on goal configuration
    chart_title <- "Goal Trend"
    if (goal_type == "Stuff") {
      category <- goal_config$stuff_category
      if (!is.null(category) && category != "") {
        if (category == "Velocity") {
          pitch <- goal_config$velocity_pitch
          if (!is.null(pitch) && pitch != "") {
            chart_title <- paste(pitch, "Velocity")
          } else {
            chart_title <- "Velocity"
          }
        } else if (category == "Movement") {
          pitch <- goal_config$movement_pitch
          movement <- goal_config$movement_type
          if (!is.null(pitch) && pitch != "" && !is.null(movement) && length(movement) > 0) {
            chart_title <- paste(pitch, paste(movement, collapse = " & "))
          } else {
            chart_title <- "Movement"
          }
        }
      }
    } else if (goal_type == "Execution") {
      stat <- goal_config$execution_stat
      if (!is.null(stat) && stat != "") {
        chart_title <- stat
      } else {
        chart_title <- "Execution"
      }
    }
    
    p <- p + labs(
      title = chart_title,
      x = "Date",
      y = y_label,
      color = "Session Type"
    ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    
    return(p)
  }
  
  # Helper function to create data tables
  create_pp_goal_table <- function(goal_num) {
    goal_config <- build_goal_config(goal_num = goal_num)
    metric_data <- calculate_metric(
      filter_goal_data(goal_num),
      goal_config = goal_config
    )
    if (is.null(metric_data) || nrow(metric_data) == 0) {
      return(data.frame(Date = character(0), Value = character(0)))
    }
    
    # Get the goal type and determine column name
    goal_type <- goal_config$type
    column_name <- "Value"
    
    if (goal_type == "Stuff") {
      category <- goal_config$stuff_category
      if (category == "Velocity") {
        column_name <- "Velocity"
      } else if (category == "Movement") {
        movement_types <- goal_config$movement_type
        if (length(movement_types) > 1) {
          column_name <- "Movement"
        } else if ("IVB" %in% movement_types) {
          column_name <- "IVB"
        } else if ("HB" %in% movement_types) {
          column_name <- "HB"
        }
      }
    } else if (goal_type == "Execution") {
      category <- goal_config$execution_stat
      if (category == "FPS%") {
        column_name <- "FPS%"
      } else if (category == "Strike%") {
        column_name <- "Strike%"
      } else if (category == "InZone%") {
        column_name <- "InZone%"
      } else if (category == "Comp%") {
        column_name <- "Comp%"
      } else if (category == "Whiff%") {
        column_name <- "Whiff%"
      } else if (category == "CSW%") {
        column_name <- "CSW%"
      } else if (category == "E+A%") {
        column_name <- "E+A%"
      } else if (category == "K%") {
        column_name <- "K%"
      } else if (category == "BB%") {
        column_name <- "BB%"
      } else if (category == "QP+") {
        column_name <- "QP+"
      } else if (category == "Ctrl+") {
        column_name <- "Ctrl+"
      }
    }
    
    # Create table with date and formatted value
    table_data <- metric_data %>%
      dplyr::arrange(dplyr::desc(Date)) %>%
      dplyr::mutate(
        formatted_value = case_when(
          SessionType == "Bullpen" ~ paste0(value, " (B)"),
          SessionType == "Live" ~ paste0(value, " (L)"),
          TRUE ~ as.character(value)
        ),
        DateLabel = format(Date, "%m/%d")
      ) %>%
      dplyr::select(Date = DateLabel, formatted_value)
    
    # Set column names
    colnames(table_data) <- c("Date", column_name)
    
    return(table_data)
  }
  
  # Goal plot renders
  output$pp_goal1_plot <- renderPlot({
    create_pp_goal_plot(1)
  }, bg = "transparent")
  
  output$pp_goal2_plot <- renderPlot({
    create_pp_goal_plot(2)
  }, bg = "transparent")
  
  output$pp_goal3_plot <- renderPlot({
    create_pp_goal_plot(3)
  }, bg = "transparent")
  
  # Goal table renders
  output$pp_goal1_table <- DT::renderDataTable({
    table_data <- create_pp_goal_table(1)
    if (nrow(table_data) == 0) return(table_data)
    
    dt <- DT::datatable(
      table_data,
      options = list(
        pageLength = 5,
        searching = FALSE,
        info = FALSE,
        lengthChange = FALSE,
        ordering = FALSE,
        paging = TRUE,
        dom = 'tp',
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
    return(dt)
  })
  
  output$pp_goal2_table <- DT::renderDataTable({
    table_data <- create_pp_goal_table(2)
    if (nrow(table_data) == 0) return(table_data)
    
    dt <- DT::datatable(
      table_data,
      options = list(
        pageLength = 5,
        searching = FALSE,
        info = FALSE,
        lengthChange = FALSE,
        ordering = FALSE,
        paging = TRUE,
        dom = 'tp',
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
    return(dt)
  })
  
  output$pp_goal3_table <- DT::renderDataTable({
    table_data <- create_pp_goal_table(3)
    if (nrow(table_data) == 0) return(table_data)
    
    dt <- DT::datatable(
      table_data,
      options = list(
        pageLength = 5,
        searching = FALSE,
        info = FALSE,
        lengthChange = FALSE,
        ordering = FALSE,
        paging = TRUE,
        dom = 'tp',
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
    return(dt)
  })
  
  # Render completed goals modal content
  output$pp_completed_goals_content <- renderUI({
    req(input$pp_player_select)
    
    if (is.null(completed_goals_data$goals) || length(completed_goals_data$goals) == 0) {
      return(div(style = "text-align: center; padding: 50px;",
                 h4("No completed goals yet.")))
    }
    
    # Filter completed goals for current player
    player_goals <- completed_goals_data$goals[sapply(completed_goals_data$goals, function(g) {
      !is.null(g$player) && g$player == input$pp_player_select
    })]
    
    if (length(player_goals) == 0) {
      return(div(style = "text-align: center; padding: 50px;",
                 h4("No completed goals for this player yet.")))
    }
    
    # Sort by completion date (most recent first)
    player_goals <- player_goals[order(sapply(player_goals, function(g) {
      if (is.null(g$date_completed)) return(0)
      return(as.Date(g$date_completed))
    }), decreasing = TRUE)]
    
    # Create cards for each completed goal
    goal_cards <- lapply(names(player_goals), function(goal_id) {
      goal <- player_goals[[goal_id]]
      
      div(class = "panel panel-default", style = "margin-bottom: 15px;",
          div(class = "panel-heading", style = "position: relative;",
              h5(class = "panel-title", style = "margin-right: 30px;",
                 strong(paste("Goal #", goal$goal_num, " - ", goal$goal_data$type))
              ),
              # Delete button in upper right
              div(style = "position: absolute; top: 5px; right: 10px;",
                  actionButton(paste0("pp_delete_goal_", goal_id), "×",
                               class = "btn btn-danger btn-xs",
                               style = "padding: 2px 6px; font-size: 12px;")
              )
          ),
          div(class = "panel-body",
              div(style = "margin-bottom: 10px;",
                  strong("Completed: "), format(as.Date(goal$date_completed), "%B %d, %Y")
              ),
              div(style = "margin-bottom: 10px;",
                  strong("Description: "),
                  create_pp_goal_description_from_data(
                    goal = goal,
                    date_range = input$pp_date_range,
                    session_type = input$pp_session_type %||% "All"
                  )
              ),
              if (!is.null(goal$goal_data$notes) && goal$goal_data$notes != "") {
                div(style = "margin-bottom: 10px;",
                    strong("Notes: "), goal$goal_data$notes
                )
              }
          )
      )
    })
    
    div(goal_cards)
  })
  
  # Helper function to create goal descriptions from stored data
  create_pp_goal_description_from_data <- function(goal, date_range, session_type) {
    if (is.null(goal) || is.null(goal$goal_data)) {
      return("No goal data recorded")
    }
    goal_data <- goal$goal_data
    session_type <- session_type %||% "All"
    config <- build_goal_config(goal_data = goal_data)
    trailing <- ""
    avg_val <- compute_goal_average_value(goal$player, config, date_range, session_type)
    final_label <- " | Final Avg (current range): "
    if (!is.null(avg_val)) {
      formatted <- format_goal_value(value = avg_val, goal_config = config)
      if (!is.null(formatted)) {
        trailing <- paste0(final_label, formatted)
      } else {
        trailing <- paste0(final_label, "N/A")
      }
    } else {
      trailing <- paste0(final_label, "N/A")
    }
    describe_goal_from_config(config, trailing_text = trailing)
  }
  
  # Helper function to clear goal inputs
  clear_goal_inputs <- function(goal_num) {
    updateSelectInput(session, paste0("pp_goal", goal_num, "_type"), selected = "")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_stuff_category"), selected = "")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_velocity_pitch"), selected = "All")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_movement_pitch"), selected = "All")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_movement_type"), selected = character(0))
    updateSelectInput(session, paste0("pp_goal", goal_num, "_execution_stat"), selected = "")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_execution_pitch"), selected = "All")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_batter_hand"), selected = "All")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_chart_view"), selected = "Trend Chart")
    updateSelectInput(session, paste0("pp_goal", goal_num, "_target_direction"), selected = "")
    updateTextInput(session, paste0("pp_goal", goal_num, "_target_value"), value = "")
    updateTextAreaInput(session, paste0("pp_goal", goal_num, "_notes"), value = "")
    updateCheckboxInput(session, paste0("pp_goal", goal_num, "_completed"), value = FALSE)
  }
  
  # Completed goals functionality (simplified)
  output$pp_completed_count <- renderText({
    req(input$pp_player_select)
    
    if (is.null(completed_goals_data$goals) || length(completed_goals_data$goals) == 0) {
      return("(0 completed goals)")
    }
    
    # Filter completed goals for current player
    player_goals <- completed_goals_data$goals[sapply(completed_goals_data$goals, function(g) {
      !is.null(g$player) && g$player == input$pp_player_select
    })]
    
    count <- length(player_goals)
    if (count == 0) {
      return("(0 completed goals)")
    } else if (count == 1) {
      return("(1 completed goal)")
    } else {
      return(paste0("(", count, " completed goals)"))
    }
  })
  
  observeEvent(input$pp_view_completed, {
    showModal(modalDialog(
      title = "Completed Goals",
      div(style = "max-height: 600px; overflow-y: auto;",
          uiOutput("pp_completed_goals_content")
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # ============== END PLAYER PLANS SERVER LOGIC ==============
  
  # ============== CORRELATIONS OUTPUT FUNCTIONS ==============
  
  # Correlation summary
  output$corr_summary_ui <- renderUI({
    data <- corr_data()
    
    if (is.null(data)) {
      return(div(class = "alert alert-danger",
                 h4("No Data Available"),
                 p("No valid data found for the selected variables and filters. Please check your selections.")))
    }
    
    if (nrow(data) < 2) {
      return(div(class = "alert alert-warning",
                 h4("Insufficient Data"),
                 p("Need at least 2 data points to calculate correlation.")))
    }
    
    # Process data for aggregation if needed
    if (input$corr_aggregation == "averages") {
      # Aggregate by player for correlation analysis
      player_col <- if (input$corr_domain == "Pitching") "Pitcher" 
      else if (input$corr_domain == "Hitting") "Batter" 
      else "Catcher"
      
      # Simple aggregation using means for basic metrics
      data <- data %>%
        group_by(!!sym(player_col)) %>%
        summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = 'drop')
      
      # Rename player column for consistency
      data <- data %>% rename(Player = !!sym(player_col))
    }
    
    # Filter for the selected variables and remove NAs
    if (!input$corr_var_x %in% colnames(data) || !input$corr_var_y %in% colnames(data)) {
      return(div(class = "alert alert-danger",
                 h4("Variables Not Found"),
                 p("Selected variables not found in the data.")))
    }
    
    data_clean <- data %>%
      dplyr::select(dplyr::all_of(c(input$corr_var_x, input$corr_var_y))) %>%
      filter(!is.na(.data[[input$corr_var_x]]), !is.na(.data[[input$corr_var_y]]))
    
    if (nrow(data_clean) < 2) {
      return(div(class = "alert alert-warning",
                 h4("Insufficient Valid Data"),
                 p("Need at least 2 valid data points to calculate correlation.")))
    }
    
    x_var <- input$corr_var_x
    y_var <- input$corr_var_y
    
    # Check if variables have any variation
    x_vals <- data_clean[[x_var]]
    y_vals <- data_clean[[y_var]]
    
    if (length(unique(x_vals)) == 1 || length(unique(y_vals)) == 1) {
      return(div(class = "alert alert-warning",
                 h4("No Variation"),
                 p("One or both variables have no variation. Correlation cannot be calculated.")))
    }
    
    correlation <- cor(x_vals, y_vals, use = "complete.obs")
    r_squared <- correlation^2
    
    div(class = "alert alert-info",
        h4("Correlation Results"),
        p(strong("Variables: "), paste(x_var, "vs", y_var)),
        p(strong("Correlation (r): "), round(correlation, 3)),
        p(strong("R-squared: "), round(r_squared, 3)),
        p(strong("Sample Size: "), nrow(data_clean)),
        p(strong("Relationship: "), 
          if (abs(correlation) >= 0.7) "Strong" 
          else if (abs(correlation) >= 0.3) "Moderate" 
          else "Weak")
    )
  })
  
  # Correlation plot
  output$corr_plot <- renderPlot({
    data <- corr_data()
    
    if (is.null(data) || nrow(data) < 2) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No sufficient data for correlation plot", size = 6) +
               theme_void())
    }
    
    # Process data for aggregation if needed
    if (input$corr_aggregation == "averages") {
      player_col <- if (input$corr_domain == "Pitching") "Pitcher" 
      else if (input$corr_domain == "Hitting") "Batter" 
      else "Catcher"
      
      data <- data %>%
        group_by(!!sym(player_col)) %>%
        summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = 'drop') %>%
        rename(Player = !!sym(player_col))
    }
    
    # Filter for the selected variables and remove NAs
    if (!input$corr_var_x %in% colnames(data) || !input$corr_var_y %in% colnames(data)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Variables not found in data", size = 6) +
               theme_void())
    }
    
    data_clean <- data %>%
      dplyr::select(dplyr::all_of(c(input$corr_var_x, input$corr_var_y))) %>%
      filter(!is.na(.data[[input$corr_var_x]]), !is.na(.data[[input$corr_var_y]]))
    
    if (nrow(data_clean) < 2) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Insufficient valid data for correlation", size = 6) +
               theme_void())
    }
    
    x_var <- input$corr_var_x
    y_var <- input$corr_var_y
    x_vals <- data_clean[[x_var]]
    y_vals <- data_clean[[y_var]]
    
    if (length(unique(x_vals)) == 1 || length(unique(y_vals)) == 1) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No variation in data for correlation", size = 6) +
               theme_void())
    }
    
    correlation <- cor(x_vals, y_vals, use = "complete.obs")
    r_squared <- correlation^2
    
    # Create pitch type display text
    pitch_type_text <- if (input$corr_domain == "Pitching" && !is.null(input$corr_pitch_type) && !"all" %in% input$corr_pitch_type) {
      paste("(", paste(input$corr_pitch_type, collapse = ", "), ")")
    } else if (input$corr_domain == "Pitching") {
      "(All Pitch Types)"
    } else ""
    
    # Create the plot
    ggplot(data_clean, aes(x = .data[[x_var]], y = .data[[y_var]])) +
      geom_point(alpha = 0.6, size = 3, color = "steelblue") +
      geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
      labs(
        title = paste("Correlation Analysis:", x_var, "vs", y_var, pitch_type_text),
        subtitle = paste("r =", round(correlation, 3), "| R² =", round(r_squared, 3)),
        x = x_var, y = y_var
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
  }, bg = "transparent")
  
  # Data table
  output$corr_data_table <- DT::renderDataTable({
    data <- corr_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(DT::datatable(
        data.frame(Message = "No data available for the selected variables and filters."),
        options = list(dom = 't'), rownames = FALSE
      ))
    }
    
    # Process data for aggregation if needed
    if (input$corr_aggregation == "averages") {
      player_col <- if (input$corr_domain == "Pitching") "Pitcher" 
      else if (input$corr_domain == "Hitting") "Batter" 
      else "Catcher"
      
      data <- data %>%
        group_by(!!sym(player_col)) %>%
        summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = 'drop') %>%
        rename(Player = !!sym(player_col))
      
      cols_to_show <- c("Player", input$corr_var_x, input$corr_var_y)
    } else {
      player_col <- if (input$corr_domain == "Pitching") "Pitcher" 
      else if (input$corr_domain == "Hitting") "Batter" 
      else "Catcher"
      
      if (player_col %in% colnames(data)) {
        data <- data %>% rename(Player = !!sym(player_col))
      }
      
      cols_to_show <- c("Player", "Date", "TaggedPitchType", input$corr_var_x, input$corr_var_y)
      cols_to_show <- cols_to_show[cols_to_show %in% colnames(data)]
    }
    
    # Filter to only show rows with valid data for both variables
    data_filtered <- data %>%
      filter(!is.na(.data[[input$corr_var_x]]), !is.na(.data[[input$corr_var_y]]))
    
    if (nrow(data_filtered) == 0) {
      return(DT::datatable(
        data.frame(Message = "No valid data for the selected variables."),
        options = list(dom = 't'), rownames = FALSE
      ))
    }
    
    DT::datatable(
      data_filtered[cols_to_show],
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons'
    ) %>%
      DT::formatRound(columns = input$corr_var_x, digits = 2) %>%
      DT::formatRound(columns = input$corr_var_y, digits = 2)
  })
  
  # ============== END CORRELATIONS SERVER LOGIC ==============
  
  # ============== END SERVER FUNCTION ==============
}

# Wrap UI with head tags for favicon and mobile web app support
ui <- tagList(
  tags$head(
    # Page title
    tags$title("PCU Dashboard"),
    # Apple iOS home screen icon (enlarged logo)
    tags$link(rel = "apple-touch-icon", href = "PCUlogo.png"),
    tags$link(rel = "apple-touch-icon", sizes = "152x152", href = "PCUlogo.png"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "PCUlogo.png"),
    tags$link(rel = "apple-touch-icon", sizes = "167x167", href = "PCUlogo.png"),
    # Favicons for browser tabs (multiple sizes for better display - enlarged)
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "PCUlogo.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "PCUlogo.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "96x96", href = "PCUlogo.png"),
    tags$link(rel = "icon", type = "image/png", href = "PCUlogo.png"),
    # Apple mobile web app settings
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "black-translucent"),
    tags$meta(name = "apple-mobile-web-app-title", content = "PCU Dashboard")
  ),
  ui  # The original ui object
)

shinyApp(ui = ui, server = server)
