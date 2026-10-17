## Cobertura temporal e linhas de cada dataset lido -- tabela do anexo
## "Methodology, Definitions and Data Notes" dos relatorios mensal/anual.
## Texto da tabela em ingles (vai para o docx).
build_data_sources_table <- function(track_dt = NULL, curtl_dt = NULL, scada_dt = NULL, heartb_dt = NULL) {
  fmt <- function(x) if (length(x) == 0L || all(is.na(x))) "-" else format(x, "%Y-%m-%d %H:%M")
  one <- function(name, dt, col) {
    if (is.null(dt) || nrow(dt) == 0L) return(data.frame(Dataset = name, First = "-", Last = "-", Rows = "0", check.names = FALSE))
    data.frame(Dataset = name, First = fmt(min(dt[[col]], na.rm = TRUE)), Last = fmt(max(dt[[col]], na.rm = TRUE)),
               Rows = format(nrow(dt), big.mark = ","), check.names = FALSE)
  }
  rbind(
    one("TrackReport (tracks)", track_dt, "timestamp"),
    one("Curtailments", curtl_dt, "start"),
    one("SCADA", scada_dt, "datetime"),
    one("Heartbeats", heartb_dt, "timestamp")
  )
}
