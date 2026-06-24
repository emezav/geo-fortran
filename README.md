# geo-fortran

Fortran 2003 library for reading and writing geospatial grids in multiple
formats (WGS84).  It is the Fortran counterpart of the
[geo](https://github.com/emezav/geo) C++ header-only library, replicating
the same format coverage and coordinate conventions.

**Author:** Erwin Meza Vega \<emezav@unicauca.edu.co\> \<emezav@gmail.com\>  
**License:** MIT

---

## Supported formats

| Format | Extension | Read | Write | Notes |
|--------|-----------|------|-------|-------|
| ESRI ASCII | `.asc` | ✅ | ✅ | `xllcorner`/`xllcenter` both accepted |
| ESRI BIL | `.bil` + `.hdr` | ✅ | ✅ | float32 binary, companion text header |
| Surfer ASCII | `.grd` (DSAA) | ✅ | ✅ | Golden Software ASCII grid |
| Surfer 6 Binary | `.grd` (DSBB) | ✅ | ✅ | float32, ncols/nrows as int16 (max 32767) |
| Surfer 7 Binary | `.grd` (DSRB) | ✅ | ✅ | float64 data; sectioned format |
| Plain text FRF | `.txt` / `.dat` | ✅ | ✅ | no header; south row first |
| Plain text LRF | `.txt` / `.dat` | ✅ | ✅ | no header; north row first |

---

## Coordinate convention

All grids are normalised internally to a common convention:

```
x0, y0  — lower-left CORNER of the grid (decimal degrees, WGS84)
dx, dy  — cell size in X / Y  (decimal degrees)
data(i, j) — value at column i (1..ncols), row j (1..nrows)
             i increases west → east
             j increases south → north  (j=1 = southernmost row)
```

Formats that store **node/cell-centre coordinates** (Surfer ASCII, Surfer 6,
Surfer 7) are automatically converted to the corner convention at load time
and converted back at save time.  Formats that store data **north-first**
(ESRI ASCII, ESRI BIL) are reversed in memory at load time and reversed again
at save time.

---

## Building

Requires CMake ≥ 3.18 and a Fortran 2003 compiler
(`gfortran` ≥ 4.9, `nvfortran`, or Intel `ifort`).

```bash
mkdir build && cd build
cmake ..
make
```

Build examples only, without installing:
```bash
cmake -DBUILD_EXAMPLES=ON ..
make create_grids read_grids
```

---

## Quick start

### Auto-detect and load

```fortran
use GeoFortranMod
type(GeoGrid) :: g

g = geo_load_grid('bathymetry.asc')   ! ESRI ASCII — auto-detected
g = geo_load_grid('bathymetry.bil')   ! ESRI BIL   — auto-detected
g = geo_load_grid('bathymetry.grd')   ! Surfer (any version) — magic-bytes detection

if (.not. g%loaded) stop 'failed to load grid'
write(*,*) g%ncols, 'x', g%nrows, 'cells'
write(*,*) 'zmin =', g%zmin, '  zmax =', g%zmax
```

### Plain-text grids (geometry supplied by caller)

```fortran
g = geo_load_grid('bathy.txt', &
                  txt_ncols=1800, txt_nrows=1200, &
                  txt_x0=-85.d0, txt_y0=-20.d0,  &
                  txt_dx=1.d0/240.d0, txt_dy=1.d0/240.d0, &
                  txt_nodata=-9999.d0, txt_lrf=.true.)   ! LRF = north-first
```

### Save in any format

```fortran
logical :: ok
ok = geo_save_grid(g, 'out.asc')           ! ESRI ASCII (from extension)
ok = geo_save_grid(g, 'out.grd', fmt=GEO_FMT_SURFER7)  ! explicit format
ok = geo_save_txt  (g, 'out.txt', lrf=.true.)           ! plain text, north-first
```

### Create a grid from scratch

```fortran
type(GeoGrid) :: g
g = geo_create(ncols=360, nrows=180, &
               x0=-180.d0, y0=-90.d0, dx=1.d0, dy=1.d0)
g%data = 0.0   ! fill with zeros
```

---

## Module reference

### Type: `GeoGrid`

| Member | Type | Description |
|--------|------|-------------|
| `data(ncols,nrows)` | `real(4)`, allocatable | Grid values |
| `ncols`, `nrows` | `integer(4)` | Dimensions |
| `x0`, `y0` | `real(8)` | Lower-left corner (degrees) |
| `dx`, `dy` | `real(8)` | Cell size (degrees) |
| `dx_m`, `dy_m` | `real(8)` | Cell size (metres, approx at y0) |
| `zmin`, `zmax` | `real(8)` | Data range |
| `nodata` | `real(8)` | No-data sentinel value |
| `fmt` | `integer` | `GEO_FMT_*` constant |
| `loaded` | `logical` | `.true.` after successful load |
| `path` | `character(512)` | Source file path |

### Format constants

```fortran
GEO_FMT_UNKNOWN    = 0
GEO_FMT_TXT_FRF    = 1   ! plain text, south-first
GEO_FMT_TXT_LRF    = 2   ! plain text, north-first
GEO_FMT_ESRI_ASC   = 3
GEO_FMT_ESRI_BIL   = 4
GEO_FMT_SURFER_ASC = 5
GEO_FMT_SURFER6    = 6
GEO_FMT_SURFER7    = 7
```

### Key procedures

| Procedure | Description |
|-----------|-------------|
| `geo_create(ncols,nrows,x0,y0,dx,dy[,nodata])` | Allocate an empty grid |
| `geo_clone(src)` | Copy geometry; zero data |
| `geo_detect_format(path)` | Infer `GEO_FMT_*` from extension + magic bytes |
| `geo_load_grid(path[,...])` | Auto-detect and load |
| `geo_save_grid(g,path[,fmt,txt_lrf])` | Save in specified (or auto-detected) format |
| `geo_load_esri_asc(path)` | Load ESRI ASCII |
| `geo_save_esri_asc(g,path)` | Save ESRI ASCII |
| `geo_load_esri_bil(path)` | Load ESRI BIL |
| `geo_save_esri_bil(g,path)` | Save ESRI BIL |
| `geo_load_surfer_asc(path)` | Load Surfer ASCII |
| `geo_save_surfer_asc(g,path)` | Save Surfer ASCII |
| `geo_load_surfer6(path)` | Load Surfer 6 binary |
| `geo_save_surfer6(g,path)` | Save Surfer 6 binary |
| `geo_load_surfer7(path)` | Load Surfer 7 binary |
| `geo_save_surfer7(g,path)` | Save Surfer 7 binary |
| `geo_load_txt(path,ncols,nrows,x0,y0,dx,dy,nodata[,lrf])` | Load plain text |
| `geo_save_txt(g,path[,lrf,fmt_str])` | Save plain text |
| `geo_diff(a,b[,max_col,max_row,rel_pct])` | Max absolute difference |
| `geo_haversine(lon1,lat1,lon2,lat2)` | Great-circle distance (m) |
| `geo_cell_size_meters(x0,y0,dx,dy,dx_m,dy_m)` | Cell size in metres |

---

## Integration as a submodule

```bash
# inside your project
git submodule add https://github.com/emezav/geo-fortran third_party/geo-fortran
```

In your `CMakeLists.txt`:

```cmake
add_subdirectory(third_party/geo-fortran)
target_link_libraries(my_target PRIVATE geo_fortran)
```

---

## Relation to geo (C++)

`geo-fortran` is an independent Fortran reimplementation of
[emezav/geo](https://github.com/emezav/geo).  Both libraries share the same
coordinate conventions and format semantics.  Known divergences and suggested
fixes for the C++ library are tracked in [TODO-GEO.md](TODO-GEO.md).

---

## Design notes

- **No shell dependencies.**  Unlike earlier Fortran grid modules that used
  `wc -l` / `awk` for file scanning, this library uses pure Fortran I/O.
- **Double-precision coordinates.**  `x0, y0, dx, dy` are `real(8)` to
  preserve geographic accuracy at arc-second resolution.
- **Single-precision data.**  Grid values are `real(4)` (float32), consistent
  with `geo.h` and the tsunami model solvers.  Surfer 7 float64 data is
  downcast at load time.
- **`newunit=` allocation.**  Unit numbers are allocated automatically via the
  Fortran 2008 `newunit=` specifier, avoiding hard-coded unit conflicts.
- **PRJ sidecar files.**  ESRI ASCII and BIL saves also write a `.prj` file
  (WGS84 Geographic) for direct import into GIS tools.
