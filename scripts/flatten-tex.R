flatten_tex <- function(
    tex_file,
    out_dir = "submission",
    overwrite = FALSE
) {
  
  tex_file <- normalizePath(tex_file, mustWork = TRUE)
  tex_dir  <- dirname(tex_file)
  
  # Output relativo alla directory del .tex
  if (!grepl("^(/|[A-Za-z]:)", out_dir)) {
    out_dir <- file.path(tex_dir, out_dir)
  }
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Leggi il .tex
  tex <- readLines(tex_file, warn = FALSE, encoding = "UTF-8")
  txt <- paste(tex, collapse = "\n")
  
  # Trova \includegraphics[...]{path}
  pattern <- "\\\\includegraphics(?:\\[[^]]*\\])?\\{([^}]+)\\}"
  
  matches <- gregexpr(pattern, txt, perl = TRUE)
  hits <- regmatches(txt, matches)[[1]]
  
  if (!length(hits)) {
    warning("No \\includegraphics dependencies found.")
  } else {
    
    paths <- sub(
      pattern,
      "\\1",
      hits,
      perl = TRUE
    )
    
    paths <- unique(paths)
    
    # Path completi delle figure, risolti rispetto al .tex
    sources <- vapply(
      paths,
      function(x) {
        path <- file.path(tex_dir, x)
        
        if (!file.exists(path)) {
          stop("File not found: ", x)
        }
        
        normalizePath(path, mustWork = TRUE)
      },
      character(1)
    )
    
    # Controlla collisioni di basename
    dest_names <- basename(sources)
    
    duplicated_names <- unique(
      dest_names[duplicated(dest_names)]
    )
    
    if (length(duplicated_names)) {
      stop(
        "Duplicate basenames detected: ",
        paste(duplicated_names, collapse = ", ")
      )
    }
    
    # Copia le figure
    copied <- file.copy(
      sources,
      file.path(out_dir, dest_names),
      overwrite = overwrite
    )
    
    if (!all(copied)) {
      stop("Some figures could not be copied.")
    }
    
    # Riscrivi i path nel .tex
    for (i in seq_along(paths)) {
      txt <- gsub(
        paste0("{", paths[i], "}"),
        paste0("{", dest_names[i], "}"),
        txt,
        fixed = TRUE
      )
    }
  }
  
  # Scrivi il nuovo .tex
  out_tex <- file.path(
    out_dir,
    basename(tex_file)
  )
  
  if (file.exists(out_tex) && !overwrite) {
    stop(
      "Output .tex already exists: ",
      out_tex,
      "\nUse overwrite = TRUE to replace it."
    )
  }
  
  writeLines(
    txt,
    out_tex,
    useBytes = TRUE
  )
  
  message("Created: ", out_dir)
  
  invisible(out_tex)
}

flatten_tex("paper/pimma.tex", out_dir = "submission", overwrite = TRUE)
