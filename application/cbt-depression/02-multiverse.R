rm(list = ls())

library(dplyr)
library(tidyr)
library(metafor)
library(purrr)
library(ggplot2)
library(flipmeta)

set.seed(2026)

dat <- readRDS("application/cbt-depression/results/cbt-dep-clean.rds")

therapies <- c(
    "CBT"
)

dat <- dat %>%
    filter(
        target_group == "Adults",
        condition %in% therapies
    )

minimum_studies <- 6

multi <- expand_grid(
    therapy = therapies,
    rho = c(0.3, 0.5, 0.7),
    tau_estimator = c("REML", "DL"),
    control = c("waitlist", "cau", "all"),
    rob = c(
        "low_only",
        "low_some_concern",
        "all"
    ),
    format = c(
        "Individual",
        "Group",
        "Guided self-help",
        "all"
    ),
    rating = c(
        "clinician",
        "self-report",
        "all"
    )
) |>
    mutate(
        specification_id = row_number(),
        rho_used = NA,
        rho_effective = NA_real_
    )


# ------------------------------------------------------------
# 3. FILTER EFFECT SIZES ACCORDING TO SPECIFICATION
# ------------------------------------------------------------

filter_effects <- function(d, specification) {
    keep <- rep(TRUE, nrow(d))

    # Therapy
    keep <- keep &
        d$condition == specification$therapy

    # Control condition
    if (specification$control == "waitlist") {
        keep <- keep &
            d$control_condition == "wl"
    }

    if (specification$control == "cau") {
        keep <- keep &
            d$control_condition == "cau"
    }

    # Risk of bias
    if (specification$rob == "low_only") {
        keep <- keep &
            d$rob == "Low"
    }

    if (specification$rob == "low_some_concern") {
        keep <- keep &
            d$rob %in%
                c(
                    "Low",
                    "Some concern"
                )
    }

    # Treatment format
    if (specification$format != "all") {
        keep <- keep &
            d$format == specification$format
    }

    # Outcome rating
    if (specification$rating != "all") {
        keep <- keep &
            d$rating == specification$rating
    }

    d[keep, ]
}

# ------------------------------------------------------------
# 4. AGGREGATE DEPENDENT EFFECT SIZES WITHIN STUDIES
# ------------------------------------------------------------

aggregate_studies <- function(d, rho) {
    effect_data <- metafor::escalc(
        yi = yi,
        vi = vi,
        data = d
    )

    aggregated <- stats::aggregate(
        effect_data,
        cluster = study_id,
        rho = rho
    )

    data.frame(
        study_id = aggregated$study_id,
        yi = as.numeric(aggregated$yi),
        vi = as.numeric(aggregated$vi)
    )
}


# ------------------------------------------------------------
# 5. FIT ALL ELIGIBLE META-ANALYTIC SPECIFICATIONS
# ------------------------------------------------------------

fitl <- fitlr <- vector(
    mode = "list",
    length = nrow(multi)
)

k_studies <- rep(
    NA_integer_,
    nrow(multi)
)

for (i in seq_len(nrow(multi))) {
    specification_i <- multi[i, ]

    # First filter according to the specification
    dat_i <- filter_effects(
        dat,
        specification_i
    )

    # rho affects the aggregation only when at least one study contributes
    # more than one usable effect size.
    usable_i <- complete.cases(
        dat_i[c("study_id", "yi", "vi")]
    )
    rho_used_i <- anyDuplicated(
        dat_i$study_id[usable_i]
    ) > 0L

    multi$rho_used[i] <- rho_used_i
    multi$rho_effective[i] <- if (rho_used_i) {
        specification_i$rho
    } else {
        NA_real_
    }

    # Number of studies before aggregation
    k_i <- dplyr::n_distinct(
        dat_i$study_id
    )

    # Specifications with fewer than 6 studies are ineligible
    if (k_i < minimum_studies) {
        next
    }

    # Then aggregate effects within study using the
    # specification-specific rho
    dat_i <- aggregate_studies(
        dat_i,
        rho = specification_i$rho
    )

    k_studies[i] <- nrow(dat_i)

    # Safety check after aggregation
    if (nrow(dat_i) < minimum_studies) {
        next
    }

    # Fit intercept-only random-effects meta-analysis
    fitl[[i]] <- metafor::rma(
        yi = yi,
        vi = vi,
        data = dat_i,
        method = specification_i$tau_estimator,
        slab = study_id
    )

    fitlr[[i]] <- metafor::robust(
        fitl[[i]],
        cluster = study_id,
        adjust = TRUE,
        clubSandwich = TRUE
    )
}


# ------------------------------------------------------------
# 6. RETAIN ONLY ELIGIBLE SPECIFICATIONS
# ------------------------------------------------------------

eligible <- !vapply(
  fitl,
  is.null,
  logical(1)
)

fitl <- fitl[eligible]
fitlr <- fitlr[eligible]
multi <- multi[eligible, ]

# ------------------------------------------------------------
# 7. IDENTIFY DUPLICATE META-ANALYTIC MODELS
# ------------------------------------------------------------

model_inputs <- function(fit) {
  list(
    slab = fit$slab,
    yi = fit$yi,
    vi = fit$vi,
    X = fit$X,
    method = fit$method,
    weighted = fit$weighted,
    weights = fit$weights,
    tau2_fixed = fit$tau2.fix,
    tau2_value = if (isTRUE(fit$tau2.fix)) fit$tau2 else NULL,
    test = fit$test,
    level = fit$level
  )
}

model_hash <- function(fit) {
  digest::digest(
    model_inputs(fit),
    algo = "sha256",
    serializeVersion = 3
  )
}

# Preserve every eligible decision path and map it to the final analysis.
specifications <- multi |>
  mutate(
    rho_requested = rho,
    model_hash = vapply(
      fitl,
      model_hash,
      character(1)
    )
  )

# Verify that equal hashes correspond to exactly identical model inputs.
input_signatures <- lapply(fitl, model_inputs)
hash_groups <- split(
  seq_along(fitl),
  specifications$model_hash
)
hash_groups_identical <- vapply(
  hash_groups,
  function(index) {
    reference <- input_signatures[[index[[1]]]]
    all(vapply(
      index,
      function(j) identical(reference, input_signatures[[j]]),
      logical(1)
    ))
  },
  logical(1)
)
stopifnot(all(hash_groups_identical))

display_choice <- function(x) {
  if (dplyr::n_distinct(x) > 1L) {
    "non_discriminating"
  } else {
    as.character(x[[1]])
  }
}

collapse_paths <- function(x) {
  paste(sort(unique(x)), collapse = " / ")
}

# "non_discriminating" means that multiple requested choices lead to the
# same final model. The *_paths columns retain those original choices.
model_labels <- specifications |>
  group_by(model_hash) |>
  summarise(
    n_paths = n(),
    control_display = display_choice(control),
    control_paths = collapse_paths(control),
    rob_display = display_choice(rob),
    rob_paths = collapse_paths(rob),
    format_display = display_choice(format),
    format_paths = collapse_paths(format),
    rating_display = display_choice(rating),
    rating_paths = collapse_paths(rating),
    rho_display = if (all(!rho_used)) {
      "non_discriminating"
    } else {
      display_choice(rho_effective)
    },
    rho_paths = collapse_paths(rho_requested),
    .groups = "drop"
  )

# Keep only the first occurrence of each identical model
unique_model <- !duplicated(specifications$model_hash)

fitl <- fitl[unique_model]
fitlr <- fitlr[unique_model]
analyses <- specifications[unique_model, ] |>
  left_join(
    model_labels,
    by = "model_hash"
  ) |>
  mutate(
    analysis_id = row_number(),
    .before = 1
  )

specifications <- specifications |>
  left_join(
    analyses |>
      select(analysis_id, model_hash),
    by = "model_hash"
  )

stopifnot(
  length(fitl) == length(fitlr),
  length(fitl) == nrow(analyses),
  !anyDuplicated(analyses$model_hash)
)

# ------------------------------------------------------------
# 8. MULTIVERSE ANALYSIS
# ------------------------------------------------------------

names(fitl) <- paste0(
  "mod",
  seq_along(multi$fitl)
)

res <- flipmeta(
  fitl,
  id = "study_id",
  B = 5000,
  extra = multi$multi,
  progress = FALSE
)

res <- p.adjust(res, method = "maxT")

multi <- list(
  res = res,
  multi = analyses,
  specifications = specifications,
  fitl = fitl,
  fitlr = fitlr
)

saveRDS(
  multi,
  "application/cbt-depression/results/cbt-dep-multi.rds"
)
