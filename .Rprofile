# VS Code fallback shim for occasional missing `.vsc.attach()` initialization.
# This keeps workspace attach resilient when the extension startup hook is skipped.
.vsc.attach <- function() {
    if (!interactive() || Sys.getenv("TERM_PROGRAM") != "vscode") {
        return(invisible(FALSE))
    }

    # Prefer the extension-provided attach function when available.
    if ("tools:vscode" %in% search()) {
        ext_env <- as.environment("tools:vscode")
        if (exists(".vsc.attach", envir = ext_env, inherits = FALSE)) {
            ext_fun <- get(".vsc.attach", envir = ext_env, inherits = FALSE)
            return(ext_fun())
        }
    }

    # Fall back to loading vsc helpers from the newest installed vscode-R extension.
    # Match only the vscode-R extension folders (e.g. reditorsupport.r-2.8.8),
    # and exclude reditorsupport.r-syntax-* which has no R/session/vsc.R.
    ext_dirs <- Sys.glob(file.path(path.expand("~/.vscode/extensions"), "reditorsupport.r-[0-9]*"))
    if (!length(ext_dirs)) {
        message("No vscode-R extension found under ~/.vscode/extensions")
        return(invisible(FALSE))
    }

    ext_dir <- normalizePath(tail(sort(ext_dirs), 1), winslash = "/", mustWork = FALSE)
    vsc_file <- file.path(ext_dir, "R", "session", "vsc.R")
    if (!file.exists(vsc_file)) {
        message("Could not find vsc.R at: ", vsc_file)
        return(invisible(FALSE))
    }

    vsc_env <- new.env(parent = globalenv())
    assign("dir_init", dirname(vsc_file), envir = vsc_env)
    source(vsc_file, local = vsc_env)

    if (!exists("attach", envir = vsc_env, inherits = FALSE)) {
        message("vscode-R attach helper not found in: ", vsc_file)
        return(invisible(FALSE))
    }

    get("attach", envir = vsc_env, inherits = FALSE)()
}
