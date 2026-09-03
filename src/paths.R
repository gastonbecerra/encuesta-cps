project_root <- here::here()

paths <- list(
  raw_500 = file.path(project_root, "data", "raw", "muestra_500"),
  raw_1000 = file.path(project_root, "data", "raw", "muestra_1000"),
  processed = file.path(project_root, "data", "processed"),
  codificacion = file.path(project_root, "data", "processed", "codificacion"),
  analysis = file.path(project_root, "analysis"),
  reports = file.path(project_root, "reports"),
  docs = file.path(project_root, "docs"),
  figures = file.path(project_root, "outputs", "figures"),
  tables = file.path(project_root, "outputs", "tables"),
  models = file.path(project_root, "outputs", "models")
)
