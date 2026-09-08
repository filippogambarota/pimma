rm(list = ls())

library(dplyr)
library(tidyr)
library(metafor)
library(purrr)
library(ggplot2)
library(flipmeta)

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
        specification_id = row_number()
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
multi <- multi[, colnames(multi) != "specification_id"]

multi <- list(
    multi = multi,
    fitl = fitl,
    fitlr = fitlr
)

saveRDS(multi, "application/cbt-depression/results/cbt-dep-multi.rds")
