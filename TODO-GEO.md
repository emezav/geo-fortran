# TODO-GEO.md -- Bugs and improvements to backport

This file documents issues found while implementing geo-fortran and reviewing
the original geo C++ library and the grid.f90 module in tsunamin2cudafortran.

Items are grouped by target: the geo C++ library or grid.f90 (Fortran).

---

## Corrections to the initial analysis

The first version of this file incorrectly attributed three Surfer-related
bugs to geo.h.  After reading the actual source code the findings are:

- geo.h Surfer ASCII and Surfer 6 loader use (columns-1) in the dx formula
  and convert center to corner correctly.  No bug in geo.h.
- geo.h Surfer 7 loader writes "DATA" and compares against "data" (lowercase).
  No bug in geo.h.
- The Surfer coordinate bugs exist only in grid.f90 (tsunamin2cudafortran).

The items below reflect the corrected analysis.

---

## Bugs in grid.f90 (tsunamin2cudafortran)

### BUG-F1 -- Surfer ASCII: wrong dx/dy formula

**File:** `tsunamin2cudafortran/grid.f90` -> `LoadSurferASCIIGrid`

The Surfer ASCII header stores xmin/xmax as the X coordinates of the
centres of the leftmost and rightmost columns.  The node spacing is:

    dx = (xmax - xmin) / (ncols - 1)

The current code computes:

    this%dx = (this%xhi - this%xlo) / this%if   ! WRONG: divides by ncols

This underestimates dx by a factor of (ncols-1)/ncols and produces a small
but systematic alignment error that grows with grid size.

**Fix:**
```fortran
if (this%if > 1) then
    this%dx = (this%xhi - this%xlo) / real(this%if - 1)
else
    this%dx = this%xhi - this%xlo
end if
```

---

### BUG-F2 -- Surfer ASCII: stored coordinates are centres, not corners

**File:** `tsunamin2cudafortran/grid.f90` -> `LoadSurferASCIIGrid`

After applying the corrected formula from BUG-F1, xlo/ylo still hold the
centre coordinates read from the file, not the lower-left corner of the grid.
The ESRI ASCII loader stores corners in xlo/ylo.  This inconsistency causes
wrong results in GridNesting and any code that mixes grids from both loaders.

**Fix (after BUG-F1):**
```fortran
this%xlo = this%xlo - this%dx / 2.0
this%ylo = this%ylo - this%dy / 2.0
```

And update the Surfer ASCII save to reconstruct centres before writing:
```fortran
xmin_centre = this%xlo + this%dx / 2.0
xmax_centre = xmin_centre + (this%if - 1) * this%dx
```

---

### BUG-F3 -- NODATA_value case sensitivity in ESRI ASCII loader

**File:** `tsunamin2cudafortran/grid.f90` -> `LoadEsriASCIIGrid`

The keyword check is:
```fortran
if (param /= 'nodata_value' .and. param /= 'NODATA_value')
```

This rejects valid variants such as `NODATA_VALUE` (all caps) that some tools
write.  A case-insensitive comparison is needed.

---

## Improvements for geo.h (C++)

The following are quality improvements, not correctness bugs.  geo.h produces
correct results for the formats tested.

### IMPROVE-1 -- Single-precision data array limits Surfer 7 accuracy

geo.h stores data internally as `float*` (32-bit).  Surfer 7 files contain
64-bit double data which is silently downcast at load time.  For tsunami
bathymetry (O(1)-O(10000) m) the precision loss is acceptable, but it should
be documented in the API.  geo-fortran makes the same tradeoff (real32 data)
and documents it explicitly.

---

### IMPROVE-2 -- PRJ sidecar file uses legacy projection format

The current .prj writer produces:
```
Projection    GEOGRAPHIC
Datum         WGS84
...
```

Modern GIS tools (QGIS >= 3.x, ArcGIS) expect OGC WKT:
```
GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",...]]
```

geo-fortran writes OGC WKT.  geo.h should be updated to match.

---

### IMPROVE-3 -- Surfer 7 loader aborts on unknown sections

The Surfer 7 format allows optional sections between GRID and DATA.  The
current loader reads exactly 80 bytes for the grid section and then expects
DATA immediately.  A file with an extra section would fail to load.

geo-fortran handles this by reading section tags in a loop and skipping
unknown sections using the size field.

---

*Last updated: 2026-06-24 -- corrected initial Surfer bug analysis*
