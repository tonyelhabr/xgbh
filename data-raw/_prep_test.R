cols_extra = NULL
f_import = magick::image_read
f_export = ggplot2::ggsave
do = TRUE
overwrite = TRUE
export = FALSE
dir = .get_dir_data()
file = NULL
ext = 'png'
sep = '_'
prefix = 'viz'
suffix = NULL
.do = list(
  agg = TRUE,
  swarm = FALSE
)
.f_import = list(
  agg = NULL,
  swarm = NULL
)
.f_export = list(
  agg = NULL,
  swarm = NULL
)
.export = list(
  agg = NULL,
  swarm = NULL
)
.overwrite = list(
  agg = NULL,
  swarm = NULL
)
.dir = list(
  agg = NULL,
  swarm = NULL
)
.file = list(
  agg = 'agg',
  swarm = 'swarm'
)
.ext = list(
  agg = NULL,
  swarm = NULL
)
.path = list(
  agg = NULL,
  swarm = NULL
)
