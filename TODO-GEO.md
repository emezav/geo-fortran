# TODO-GEO.md — Bugs and improvements to backport to geo (C++)

This file documents issues found while implementing `geo-fortran`.
Each item is a candidate for a fix or improvement in the original
[emezav/geo](https://github.com/emezav/geo) C++ library.

---

## Bugs

### BUG-1 — Surfer ASCII: wrong `dx`/`dy` when `ncols > 1`

**File:** `geo.h`  `Surfer::loadAscii()` (or equivalent)  
**Also present in:** `tsunamin2cudafortran/grid.f90` → `LoadSurferASCIIGrid`

**Problem:**  
The Surfer ASCII format (`DSAA`) stores the X/Y extents as the coordinates of
the **centre** of the leftmost/rightmost columns and bottom/top rows — not as
grid corners.  The cell spacing between adjacent nodes is therefore:

```
dx = (xmax - xmin) / (ncols - 1)
```

Both `geo.h` and `grid.f90` compute:

```
dx = (xmax - xmin) / ncols          ← WRONG
```

This underestimates the cell size by a factor of `(ncols-1)/ncols` and shifts
`xmax` inward by one cell width, causing a systematic alignment error.

**Fix for geo.h:**
```cpp
double dx = (ncols > 1) ? (xmax - xmin) / (ncols - 1) : (xmax - xmin);
double x0 = xmin - dx / 2.0;        // corner convention
```

**Fix for grid.f90:**
```fortran
if (this%if > 1) then
  this%dx = (this%xhi - this%xlo) / real(this%if - 1)
else
  this%dx = this%xhi - this%xlo
end if
this%xlo = this%xlo - this%dx / 2.0  ! convert centre → corner
this%ylo = this%ylo - this%dy / 2.0
```

---

### BUG-2 — Surfer ASCII in `grid.f90`: inconsistent coordinate convention

**File:** `tsunamin2cudafortran/grid.f90` → `LoadSurferASCIIGrid`

**Problem:**  
`LoadEsriASCIIGrid` stores `xlo` / `ylo` as **corner** coordinates
(as specified by the ESRI ASC format).  `LoadSurferASCIIGrid` stores the raw
values from the DSAA header, which are **centre** coordinates of the edge
cells.  The `Grid` type does not document which convention is in use, so
downstream code that mixes grids from both loaders computes wrong positions
and nesting indices.

**Fix:** Normalise all loaders to store corner coordinates in `xlo`/`ylo`.
Apply the centre-to-corner correction in `LoadSurferASCIIGrid` (see BUG-1).

---

### BUG-3 — Surfer 6 Binary: coordinate convention identical to BUG-1/BUG-2

**File:** `geo.h`  `Surfer::load6()` (or equivalent)

**Problem:** Same centre-vs-corner issue as BUG-1 applies to the binary header
of Surfer 6 (`DSBB`).  The `xmin`/`xmax`/`ymin`/`ymax` fields are centres of
the edge nodes; `dx = (xmax - xmin) / (ncols - 1)`.

---

### BUG-4 — Surfer 7 Binary: ambiguous section tag "ATAD" vs "DATA"

**File:** `geo.h`  Surfer 7 loader  

**Problem:**  
`geo.h` appears to compare the data-section tag against `"ATAD"` rather than
`"DATA"`.  If this comparison is performed by reading 4 bytes into a
`char[4]` and comparing as a string, the literal bytes in the file must spell
`A-T-A-D` — which is not the standard Surfer 7 specification (which uses
`DATA`).

Likely cause: the tag was read into a `uint32_t` variable and compared as a
little-endian integer.  On a little-endian host, the bytes `D-A-T-A`
(0x44, 0x41, 0x54, 0x41) are read as the 32-bit integer `0x41544144`.
Converting that integer back to a string via `reinterpret_cast<char*>` yields
the bytes in memory order on that platform, which on a little-endian host is
still `D-A-T-A`.  The comparison with the string literal `"ATAD"` (bytes
`0x41, 0x54, 0x41, 0x44`) would therefore **fail** on standard little-endian
hardware.

`geo-fortran` reads the 4-byte tag as `character(len=4)` directly from the
stream, getting `"DATA"` in file order.  This is correct and avoids the
endian confusion.

**Recommended fix for geo.h:** compare the tag as a `char[4]` string, not as
an integer:
```cpp
char tag[4];
fread(tag, 4, 1, f);
if (memcmp(tag, "DATA", 4) == 0) { /* data section */ }
```

---

### BUG-5 — `grid.f90` `NODATA_value` case sensitivity

**File:** `tsunamin2cudafortran/grid.f90` → `LoadEsriASCIIGrid`

**Problem:**  
The NODATA keyword check is:
```fortran
if (param /= 'nodata_value' .and. param /= 'NODATA_value')
```
This rejects valid mixed-case variants such as `NODATA_VALUE` or
`Nodata_Value` that some tools write.

**Fix:** Use a case-insensitive comparison (as `geo-fortran` does with
`geo_streq`).

---

## Improvements / Design

### IMPROVE-1 — Use double precision for geographic coordinates

**Affects:** `tsunamin2cudafortran/grid.f90`

`xlo`, `ylo`, `dx`, `dy` etc. are `real` (32-bit) in the existing `GridModule`.
At fine resolutions (3 arc-second = 0.000833°) a 32-bit float retains only
~4–5 significant digits in the fractional part, introducing sub-cell
positioning errors.

`geo-fortran` uses `real(real64)` for all coordinates.

**Recommended change for grid.f90:** declare coordinate members as
`real(kind=8)` or `double precision`.

---

### IMPROVE-2 — Eliminate shell-command dependency in `ScanFile`

**File:** `tsunamin2cudafortran/grid.f90` → `ScanFile`

`ScanFile` shells out to `wc -l` and `awk` to count lines and columns.
This is non-portable (fails on Windows, some HPC clusters) and adds process
fork/exec overhead for every grid load.

`geo-fortran` counts lines and columns with pure Fortran I/O.

**Alternative approach:**  
```fortran
integer :: nlines, ios
character(len=1) :: dummy
nlines = 0
rewind(u)
do
  read(u, '(A)', iostat=ios)
  if (ios /= 0) exit
  nlines = nlines + 1
end do
```

---

### IMPROVE-3 — `newunit=` instead of hard-coded unit numbers

**File:** `tsunamin2cudafortran/grid.f90`

Unit 99 is hard-coded throughout.  If two modules happen to open unit 99
simultaneously the behaviour is undefined.  The Fortran 2008 `newunit=`
specifier allocates a fresh, unique unit automatically.

**Fix:** Replace `open(unit=99, ...)` with `open(newunit=u, ...)`.

---

### IMPROVE-4 — geo.h: single-precision data array limits Surfer 7 precision

**File:** `geo.h`

`geo.h` stores the internal data as `float*` (32-bit), but Surfer 7 files
contain 64-bit double data.  Loading a Surfer 7 file silently downcasts the
values.  For tsunami bathymetry this is generally acceptable (depths are
O(1)–O(10000) m, well within float range), but it should be documented.

`geo-fortran` makes the same tradeoff (`data(:,:)` is `real(4)`) and
documents it explicitly.

---

### IMPROVE-5 — PRJ sidecar file content

**File:** `geo.h` (ESRI savers) and `tsunamin2cudafortran/grid.f90`

The existing `.prj` writers produce a legacy plain-text projection format:
```
Projection    GEOGRAPHIC
Datum         WGS84
...
```

Modern GIS tools (QGIS ≥ 3.x, ArcGIS) expect OGC WKT format:
```
GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",
  SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]
```

`geo-fortran` writes OGC WKT.  `geo.h` and `grid.f90` should be updated to
match.

---

### IMPROVE-6 — Auto-detect `xllcenter`/`xllcorner` case-insensitively in geo.h

**File:** `geo.h`  ESRI ASCII loader

If geo.h does a case-sensitive comparison of the `xllcorner`/`xllcenter`
keyword, files from tools that write `XLLCORNER` (upper-case) will fail to
load.  `geo-fortran` lower-cases the key before comparison.

---

### IMPROVE-7 — Surfer 7: skip unknown sections instead of aborting

**File:** `geo.h`  Surfer 7 loader

The Surfer 7 format can contain optional sections (statistics, faults, etc.)
between the mandatory `GRID` and `DATA` sections.  A robust loader should skip
unknown sections by reading and discarding their `size` bytes.

`geo-fortran` implements this via a `default` case in the section-tag `select`.

---

*Last updated during geo-fortran v1.0 initial implementation — 2026-06-24*
