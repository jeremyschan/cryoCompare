read_mrc <- function(path) {
  con <- file(path, "rb"); on.exit(close(con), add = TRUE)
  hdr <- readBin(con, "integer", n = 256, size = 4L, signed = TRUE, endian = "little")
  nx <- hdr[1]; ny <- hdr[2]; nz <- hdr[3]; mode <- hdr[4]
  if (mode != 2L) stop("Only MRC mode 2 (float32) supported. Got mode=", mode,
                       ". Convert externally or provide an .rds array.")
  seek(con, where = 1024L, origin = "start")
  nvox <- as.double(nx) * ny * nz
  buf <- readBin(con, "numeric", n = nvox, size = 4L, endian = "little")
  array(buf, dim = c(nz, ny, nx))
}
