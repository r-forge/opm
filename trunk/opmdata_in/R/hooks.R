

.onLoad <- function(...) {
  opm.version <- utils::packageVersion("opm")
  opmdata.version <- utils::packageVersion("opmdata")
  opm.wanted <- package_version("0.7-28")
  if (opm.version < opm.wanted) {
    msg <- c(
      "",
      "PROBLEM when loading opmdata (version %s):",
      "Object 'vaas_et_al' from this version is INCOMPATIBLE with opm < %s.",
      "Please use a newer version of opm or an older version of opmdata.",
      "Alternatively, convert old-style to new-style \"OPMS\" objects via:",
      "",
      "  vaas_et_al <- as(as(vaas_et_al, \"list\"), \"OPMS\")",
      "",
      "(with opm >= %s!) before conducting any analyses with them.",
      ""
    )
    msg <- paste(msg, collapse = "\n")
    msg <- sprintf(msg, opmdata.version, opm.wanted, opm.wanted)
    packageStartupMessage(msg)
  }
}


