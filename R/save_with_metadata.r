
# Main API -----------------------------------------------------------

#' Save an object with rich metadata (provenance + environment)
#' @param object The R object to save (data.frame, list, model, etc.)
#' @param file Output .rds filename
#' @param description A short human-readable description
#' @param fn Optional: pass the function that produced `object` (for code capture)
#' @param extra_meta Optional named list with any additional fields you want
#' @param compress One of "gzip" (default), "xz", "bzip2", or FALSE
#' @export
save_with_metadata <- function(object,
                               file,
                               description = "",
                               fn = NULL,
                               dictionary=NULL, 
                               extra_meta = list(),
                               compress = "gzip") {
  # Call stack & call
  calls <- sys.calls()
  last_call <- if (length(calls)) calls[[length(calls)]] else NULL
  
  deparse_safe <- function(x) {
    paste(utils::capture.output(print(x)), collapse = "\n")
  }
  
  # Optional: data hash to detect downstream changes (requires digest)
  hash_object <- function(x) {
    if (!requireNamespace("digest", quietly = TRUE)) return(NA_character_)
    digest::digest(x, algo = "xxhash64")
  }
  
  # Optional: safely capture random seed
  get_seed <- function() {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      paste0(utils::head(get(".Random.seed", envir = .GlobalEnv), 6), collapse = ",")
    } else NA_character_
  }
  
    # Function code capture:
  # - If user supplies `fn`, use that
  # - Else attempt to capture the immediate calling function (sys.function(1))
  fn_obj <- NULL
  if (!is.null(fn)) {
    fn_obj <- fn
  } else {
    # Try to get the function at frame 1 (caller of this helper)
    # In some contexts this may be NULL (e.g., top-level)
    fn_obj <- tryCatch(sys.function(1), error = function(e) NULL)
  }
  
  fn_code <- if (!is.null(fn_obj)) deparse_safe(fn_obj) else NA_character_
  call_str <- if (!is.null(last_call)) deparse_safe(last_call) else NA_character_
  
  # Script path + git info
  script_path <-   ctx <- rstudioapi::getActiveDocumentContext()$path
  
  # Session info
  sess <- utils::capture.output(sessionInfo())[1:3]
  
  # Random seed (if any)
  seed <- get_seed()
  
  # Object hash (optional; helps detect changes later)
  obj_hash <- hash_object(object)
  # Build metadata list
  meta <- c(
    list(
      title       = description,
      created_at  = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      user        = Sys.info()[["user"]],
      hostname    = Sys.info()[["nodename"]],
      working_dir = normalizePath(getwd()),
      script_path = script_path,
      sessionInfo = sess,
      random_seed = seed,
      object_class = paste(class(object), collapse = "/"),
      dictionary = dictionary
      ),
    extra_meta
  )
  
  # Wrap in a container that is explicit and robust
  container <- list(
    data = object,
    metadata = meta
  )
  print(meta)
  
  saveRDS(container, file = file, compress = compress)
  invisible(container)
}
