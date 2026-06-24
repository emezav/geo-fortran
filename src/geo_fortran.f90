! =============================================================================
! geo_fortran.f90 — Geospatial grid I/O module for Fortran
! =============================================================================
!
! @file
! @brief Read and write geospatial grids in multiple formats (WGS84).
!
! @author Erwin Meza Vega <emezav@unicauca.edu.co> <emezav@gmail.com>
! @copyright MIT License
!
! Supported formats
! -----------------
!   ESRI ASCII      .asc               text header + row-major text data
!   ESRI BIL        .bil + .hdr        binary float32, companion text header
!   Surfer ASCII    .grd  (DSAA)       Golden Software ASCII grid
!   Surfer 6 Bin.   .grd  (DSBB)       Golden Software binary, float32
!   Surfer 7 Bin.   .grd  (DSRB)       Golden Software binary, float64
!   Plain text FRF  .txt/.dat          no header, first row = south row
!   Plain text LRF  .txt/.dat          no header, first row = north row
!
! Coordinate convention (internal)
! ---------------------------------
!   x0, y0  : lower-left CORNER of the grid  (decimal degrees, WGS84)
!   dx, dy  : cell size in X / Y direction    (decimal degrees)
!   data(i,j): value at column i, row j
!              i = 1 .. ncols  (west → east)
!              j = 1 .. nrows  (south → north)
!
! Formats that store coordinates as node / cell-center positions
! (Surfer ASCII, Surfer 6 Bin., Surfer 7 Bin.) are converted to corner
! convention at load time and written back as centers at save time.
!
! Formats that store data top-to-bottom (ESRI ASC, ESRI BIL) are
! reversed in memory at load time and reversed again at save time.
! =============================================================================
module GeoFortranMod

  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, &
                                            real32, real64
  implicit none
  private

  ! ---------------------------------------------------------------------------
  ! Public constants
  ! ---------------------------------------------------------------------------
  integer, parameter, public :: GEO_PATH_MAX = 512
  integer, parameter, public :: GEO_BUFLEN   = 4096

  ! Format identifiers
  integer, parameter, public :: GEO_FMT_UNKNOWN    = 0
  integer, parameter, public :: GEO_FMT_TXT_FRF    = 1  ! plain text, south-first
  integer, parameter, public :: GEO_FMT_TXT_LRF    = 2  ! plain text, north-first
  integer, parameter, public :: GEO_FMT_ESRI_ASC   = 3  ! ESRI ASCII (.asc)
  integer, parameter, public :: GEO_FMT_ESRI_BIL   = 4  ! ESRI BIL  (.bil + .hdr)
  integer, parameter, public :: GEO_FMT_SURFER_ASC = 5  ! Surfer ASCII (.grd, DSAA)
  integer, parameter, public :: GEO_FMT_SURFER6    = 6  ! Surfer 6 binary (.grd, DSBB)
  integer, parameter, public :: GEO_FMT_SURFER7    = 7  ! Surfer 7 binary (.grd, DSRB)

  ! ---------------------------------------------------------------------------
  ! GeoGrid type
  ! ---------------------------------------------------------------------------
  type, public :: GeoGrid
    real(real32), allocatable :: data(:,:)    ! data(1:ncols, 1:nrows)
    character(len=GEO_PATH_MAX) :: path = ''
    integer(int32) :: ncols  = 0             ! columns  (X / longitude direction)
    integer(int32) :: nrows  = 0             ! rows     (Y / latitude  direction)
    real(real64)   :: x0     = 0.d0          ! lower-left corner X  (degrees)
    real(real64)   :: y0     = 0.d0          ! lower-left corner Y  (degrees)
    real(real64)   :: dx     = 0.d0          ! cell width  (degrees)
    real(real64)   :: dy     = 0.d0          ! cell height (degrees)
    real(real64)   :: dx_m   = 0.d0          ! cell width  (metres, approx at y0)
    real(real64)   :: dy_m   = 0.d0          ! cell height (metres, approx at x0)
    real(real64)   :: zmin   = 0.d0
    real(real64)   :: zmax   = 0.d0
    real(real64)   :: nodata = -9999.d0
    integer        :: fmt    = GEO_FMT_UNKNOWN
    logical        :: loaded = .false.
  contains
    final :: geo_grid_destructor
  end type GeoGrid

  ! ---------------------------------------------------------------------------
  ! Public API
  ! ---------------------------------------------------------------------------
  public :: geo_create
  public :: geo_clone
  public :: geo_detect_format
  public :: geo_load_grid
  public :: geo_save_grid
  public :: geo_load_esri_asc,   geo_save_esri_asc
  public :: geo_load_esri_bil,   geo_save_esri_bil
  public :: geo_load_surfer_asc, geo_save_surfer_asc
  public :: geo_load_surfer6,    geo_save_surfer6
  public :: geo_load_surfer7,    geo_save_surfer7
  public :: geo_load_txt,        geo_save_txt
  public :: geo_diff
  public :: geo_haversine
  public :: geo_cell_size_meters

contains

  ! ===========================================================================
  ! Section 1: Constructor / Destructor / Utility
  ! ===========================================================================

  ! ---------------------------------------------------------------------------
  ! Create an empty grid with all geometry set.
  ! ---------------------------------------------------------------------------
  function geo_create(ncols, nrows, x0, y0, dx, dy, nodata) result(g)
    integer(int32), intent(in)           :: ncols, nrows
    real(real64),   intent(in)           :: x0, y0, dx, dy
    real(real64),   intent(in), optional :: nodata
    type(GeoGrid) :: g

    g%ncols = ncols
    g%nrows = nrows
    g%x0    = x0
    g%y0    = y0
    g%dx    = dx
    g%dy    = dy
    if (present(nodata)) then
      g%nodata = nodata
    else
      g%nodata = -9999.d0
    end if
    call geo_cell_size_meters(x0, y0, dx, dy, g%dx_m, g%dy_m)
    g%zmin   = 0.d0
    g%zmax   = 0.d0
    g%fmt    = GEO_FMT_UNKNOWN
    g%loaded = .false.
    allocate(g%data(ncols, nrows))
    g%data = 0.0_real32
  end function geo_create

  ! ---------------------------------------------------------------------------
  ! Clone geometry; data array is zeroed.
  ! ---------------------------------------------------------------------------
  function geo_clone(src) result(g)
    type(GeoGrid), intent(in) :: src
    type(GeoGrid) :: g
    g = src
    g%data   = 0.0_real32
    g%zmin   = 0.d0
    g%zmax   = 0.d0
    g%loaded = .false.
    g%path   = ''
  end function geo_clone

  subroutine geo_grid_destructor(g)
    type(GeoGrid) :: g
    if (allocated(g%data)) deallocate(g%data)
  end subroutine geo_grid_destructor

  ! ---------------------------------------------------------------------------
  ! Haversine distance in metres between two lon/lat points.
  ! ---------------------------------------------------------------------------
  real(real64) function geo_haversine(lon1, lat1, lon2, lat2) result(d)
    real(real64), intent(in) :: lon1, lat1, lon2, lat2
    real(real64), parameter  :: RE = 6378137.d0  ! WGS84 semi-major axis (m)
    real(real64), parameter  :: PI = acos(-1.d0)
    real(real64) :: dlon, dlat, a, c
    dlat = (lat2 - lat1) * PI / 180.d0
    dlon = (lon2 - lon1) * PI / 180.d0
    a = sin(dlat/2.d0)**2 + &
        cos(lat1*PI/180.d0) * cos(lat2*PI/180.d0) * sin(dlon/2.d0)**2
    c = 2.d0 * atan2(sqrt(a), sqrt(1.d0 - a))
    d = RE * c
  end function geo_haversine

  ! ---------------------------------------------------------------------------
  ! Compute approximate cell size in metres at position (x0, y0).
  ! ---------------------------------------------------------------------------
  subroutine geo_cell_size_meters(x0, y0, dx, dy, dx_m, dy_m)
    real(real64), intent(in)  :: x0, y0, dx, dy
    real(real64), intent(out) :: dx_m, dy_m
    dx_m = geo_haversine(x0, y0, x0 + dx, y0)
    dy_m = geo_haversine(x0, y0, x0,      y0 + dy)
  end subroutine geo_cell_size_meters

  ! ---------------------------------------------------------------------------
  ! Update zmin/zmax from data array.
  ! ---------------------------------------------------------------------------
  subroutine geo_update_stats(g)
    type(GeoGrid), intent(inout) :: g
    if (allocated(g%data) .and. g%ncols > 0 .and. g%nrows > 0) then
      g%zmin = real(minval(g%data), real64)
      g%zmax = real(maxval(g%data), real64)
    end if
  end subroutine geo_update_stats

  ! ---------------------------------------------------------------------------
  ! Return the file extension (lower-case, without leading dot).
  ! ---------------------------------------------------------------------------
  function geo_extension(path) result(ext)
    character(len=*), intent(in) :: path
    character(len=16) :: ext
    integer :: dot, i
    ext = ''
    dot = 0
    do i = len_trim(path), 1, -1
      if (path(i:i) == '.') then
        dot = i
        exit
      end if
    end do
    if (dot > 0 .and. dot < len_trim(path)) then
      ext = path(dot+1 : len_trim(path))
      call geo_to_lower(ext)
    end if
  end function geo_extension

  ! ---------------------------------------------------------------------------
  ! Replace extension: returns path with a new extension.
  ! ---------------------------------------------------------------------------
  function geo_replace_ext(path, newext) result(res)
    character(len=*), intent(in) :: path, newext
    character(len=GEO_PATH_MAX)  :: res
    integer :: dot, i
    dot = 0
    do i = len_trim(path), 1, -1
      if (path(i:i) == '.') then
        dot = i
        exit
      end if
    end do
    if (dot > 1) then
      res = path(1:dot-1) // '.' // trim(newext)
    else
      res = trim(path) // '.' // trim(newext)
    end if
  end function geo_replace_ext

  ! ---------------------------------------------------------------------------
  ! In-place lower-case conversion for a character variable.
  ! ---------------------------------------------------------------------------
  subroutine geo_to_lower(s)
    character(len=*), intent(inout) :: s
    integer :: i, c
    do i = 1, len(s)
      c = iachar(s(i:i))
      if (c >= 65 .and. c <= 90) s(i:i) = achar(c + 32)
    end do
  end subroutine geo_to_lower

  ! ---------------------------------------------------------------------------
  ! Case-insensitive string equality.
  ! ---------------------------------------------------------------------------
  logical function geo_streq(a, b)
    character(len=*), intent(in) :: a, b
    character(len=256) :: la, lb
    la = a; lb = b
    call geo_to_lower(la); call geo_to_lower(lb)
    geo_streq = (trim(la) == trim(lb))
  end function geo_streq

  ! ---------------------------------------------------------------------------
  ! Read a full text line into an allocatable string; returns ios /= 0 on EOF.
  ! ---------------------------------------------------------------------------
  subroutine geo_getline(unit, line, ios)
    integer,                       intent(in)  :: unit
    character(len=:), allocatable, intent(out) :: line
    integer,                       intent(out) :: ios
    character(len=GEO_BUFLEN) :: buf
    buf = ''
    read(unit, '(A)', iostat=ios) buf
    line = trim(buf)
  end subroutine geo_getline

  ! ---------------------------------------------------------------------------
  ! Write a PRJ sidecar file (WGS84 geographic) alongside any output grid.
  ! ---------------------------------------------------------------------------
  subroutine geo_write_prj(path)
    character(len=*), intent(in) :: path
    character(len=GEO_PATH_MAX) :: prjpath
    integer :: u, ios
    prjpath = geo_replace_ext(trim(path), 'prj')
    open(newunit=u, file=trim(prjpath), status='UNKNOWN', iostat=ios)
    if (ios /= 0) return
    ! OGC WKT for WGS84 Geographic coordinate system
    write(u,'(A)') 'GEOGCS["GCS_WGS_1984",'// &
      'DATUM["D_WGS_1984",'// &
      'SPHEROID["WGS_1984",6378137.0,298.257223563]],'// &
      'PRIMEM["Greenwich",0.0],'// &
      'UNIT["Degree",0.0174532925199433]]'
    close(u)
  end subroutine geo_write_prj

  ! ===========================================================================
  ! Section 2: Format detection
  ! ===========================================================================

  ! ---------------------------------------------------------------------------
  ! Detect format from file extension; for .grd also peek at magic bytes.
  ! Returns one of GEO_FMT_* constants.  GEO_FMT_UNKNOWN means unsupported.
  ! ---------------------------------------------------------------------------
  integer function geo_detect_format(path)
    character(len=*), intent(in) :: path
    character(len=16) :: ext
    character(len=4)  :: magic
    integer :: u, ios

    ext = geo_extension(trim(path))
    geo_detect_format = GEO_FMT_UNKNOWN

    select case (trim(ext))

    case ('asc')
      geo_detect_format = GEO_FMT_ESRI_ASC

    case ('bil')
      geo_detect_format = GEO_FMT_ESRI_BIL

    case ('grd')
      ! Distinguish DSAA / DSBB / DSRB by reading the first 4 bytes.
      open(newunit=u, file=trim(path), form='unformatted', &
           access='stream', status='old', action='read', iostat=ios)
      if (ios /= 0) return
      read(u, iostat=ios) magic
      close(u)
      if (ios /= 0) return
      select case (magic)
      case ('DSAA')
        geo_detect_format = GEO_FMT_SURFER_ASC
      case ('DSBB')
        geo_detect_format = GEO_FMT_SURFER6
      case ('DSRB')
        geo_detect_format = GEO_FMT_SURFER7
      end select

    case ('txt', 'dat')
      ! Cannot distinguish FRF / LRF from the file alone — caller decides.
      geo_detect_format = GEO_FMT_TXT_FRF

    end select
  end function geo_detect_format

  ! ---------------------------------------------------------------------------
  ! Infer format from extension only (no file read).
  ! Used when saving: the target file may not exist yet.
  ! For .grd defaults to Surfer ASCII (DSAA); for .txt defaults to FRF.
  ! ---------------------------------------------------------------------------
  integer function geo_format_from_ext(path)
    character(len=*), intent(in) :: path
    character(len=16) :: ext
    ext = geo_extension(trim(path))
    select case (trim(ext))
    case ('asc');       geo_format_from_ext = GEO_FMT_ESRI_ASC
    case ('bil');       geo_format_from_ext = GEO_FMT_ESRI_BIL
    case ('grd');       geo_format_from_ext = GEO_FMT_SURFER_ASC  ! default Surfer format
    case ('txt','dat'); geo_format_from_ext = GEO_FMT_TXT_FRF
    case default;       geo_format_from_ext = GEO_FMT_UNKNOWN
    end select
  end function geo_format_from_ext

  ! ===========================================================================
  ! Section 3: Auto-dispatch load / save
  ! ===========================================================================

  ! ---------------------------------------------------------------------------
  ! Load a grid, auto-detecting the format.
  ! For TXT formats, supply txt_ncols, txt_nrows, txt_x0, txt_y0,
  ! txt_dx, txt_dy, txt_nodata, and txt_lrf=.true. for north-first order.
  ! ---------------------------------------------------------------------------
  function geo_load_grid(path, txt_ncols, txt_nrows, txt_x0, txt_y0, &
                          txt_dx, txt_dy, txt_nodata, txt_lrf) result(g)
    character(len=*), intent(in)           :: path
    integer(int32),   intent(in), optional :: txt_ncols, txt_nrows
    real(real64),     intent(in), optional :: txt_x0, txt_y0, txt_dx, txt_dy
    real(real64),     intent(in), optional :: txt_nodata
    logical,          intent(in), optional :: txt_lrf
    type(GeoGrid) :: g
    integer :: fmt

    fmt = geo_detect_format(trim(path))

    select case (fmt)
    case (GEO_FMT_ESRI_ASC)
      g = geo_load_esri_asc(path)
    case (GEO_FMT_ESRI_BIL)
      g = geo_load_esri_bil(path)
    case (GEO_FMT_SURFER_ASC)
      g = geo_load_surfer_asc(path)
    case (GEO_FMT_SURFER6)
      g = geo_load_surfer6(path)
    case (GEO_FMT_SURFER7)
      g = geo_load_surfer7(path)
    case (GEO_FMT_TXT_FRF, GEO_FMT_UNKNOWN)
      ! TXT: caller must supply dimensions and georeferencing.
      if (.not. (present(txt_ncols) .and. present(txt_nrows) .and. &
                 present(txt_x0)    .and. present(txt_y0)    .and. &
                 present(txt_dx)    .and. present(txt_dy))) then
        write(*,'(A)') 'geo_load_grid: TXT format requires txt_ncols, &
                        &txt_nrows, txt_x0, txt_y0, txt_dx, txt_dy'
        return
      end if
      block
        logical :: lrf
        real(real64) :: nd
        lrf = .false.
        nd  = -9999.d0
        if (present(txt_lrf))    lrf = txt_lrf
        if (present(txt_nodata)) nd  = txt_nodata
        g = geo_load_txt(path, txt_ncols, txt_nrows, txt_x0, txt_y0, &
                          txt_dx, txt_dy, nd, lrf)
      end block
    end select
  end function geo_load_grid

  ! ---------------------------------------------------------------------------
  ! Save a grid in a given format.  If fmt=GEO_FMT_UNKNOWN, the format is
  ! inferred from the file extension of path.
  ! For TXT, supply txt_lrf=.true. to write north-first (LRF) order.
  ! ---------------------------------------------------------------------------
  logical function geo_save_grid(g, path, fmt, txt_lrf)
    type(GeoGrid),    intent(inout)        :: g
    character(len=*), intent(in)           :: path
    integer,          intent(in), optional :: fmt
    logical,          intent(in), optional :: txt_lrf
    integer :: effective_fmt
    logical :: lrf

    geo_save_grid = .false.
    lrf = .false.
    if (present(txt_lrf)) lrf = txt_lrf

    if (present(fmt)) then
      effective_fmt = fmt
    else
      ! Use extension-only detection for saving: target file may not exist yet.
      ! For .grd with an explicit Surfer variant, caller must pass fmt explicitly.
      effective_fmt = geo_format_from_ext(trim(path))
    end if

    select case (effective_fmt)
    case (GEO_FMT_ESRI_ASC)
      geo_save_grid = geo_save_esri_asc(g, path)
    case (GEO_FMT_ESRI_BIL)
      geo_save_grid = geo_save_esri_bil(g, path)
    case (GEO_FMT_SURFER_ASC)
      geo_save_grid = geo_save_surfer_asc(g, path)
    case (GEO_FMT_SURFER6)
      geo_save_grid = geo_save_surfer6(g, path)
    case (GEO_FMT_SURFER7)
      geo_save_grid = geo_save_surfer7(g, path)
    case (GEO_FMT_TXT_FRF, GEO_FMT_TXT_LRF)
      geo_save_grid = geo_save_txt(g, path, lrf)
    case default
      write(*,'(A,A)') 'geo_save_grid: unknown or unsupported format for ', &
                        trim(path)
    end select
  end function geo_save_grid

  ! ===========================================================================
  ! Section 4: ESRI ASCII  (.asc)
  ! ===========================================================================
  !
  ! File structure:
  !   ncols         <int>
  !   nrows         <int>
  !   xllcorner     <real>   OR  xllcenter  <real>
  !   yllcorner     <real>   OR  yllcenter  <real>
  !   cellsize      <real>
  !   NODATA_value  <real>
  !   <data rows from north (top) to south (bottom), left to right>
  ! ===========================================================================

  function geo_load_esri_asc(path) result(g)
    character(len=*), intent(in) :: path
    type(GeoGrid) :: g

    integer :: u, ios, j
    character(len=64) :: key
    real(real64) :: rval
    logical :: center_x, center_y

    center_x = .false.
    center_y = .false.

    open(newunit=u, file=trim(path), status='OLD', action='READ', iostat=ios)
    if (ios /= 0) then
      write(*,'(A,A)') 'geo_load_esri_asc: cannot open ', trim(path)
      return
    end if

    ! --- Parse header (exactly 6 mandatory key-value lines) ---
    call geo_asc_read_kv_int(u,  g%ncols,  ios); if (ios/=0) goto 900
    call geo_asc_read_kv_int(u,  g%nrows,  ios); if (ios/=0) goto 900
    call geo_asc_read_kv_real(u, g%x0,     key, ios)
    if (ios/=0) goto 900
    if (geo_streq(trim(key), 'xllcenter')) center_x = .true.
    call geo_asc_read_kv_real(u, g%y0,     key, ios)
    if (ios/=0) goto 900
    if (geo_streq(trim(key), 'yllcenter')) center_y = .true.

    ! cellsize — stored in dx (assume square cells)
    call geo_asc_read_kv_real(u, rval, key, ios); if (ios/=0) goto 900
    g%dx = rval
    g%dy = rval
    call geo_asc_read_kv_real(u, g%nodata, key, ios); if (ios/=0) goto 900

    ! Convert center to corner if needed
    if (center_x) g%x0 = g%x0 - g%dx / 2.d0
    if (center_y) g%y0 = g%y0 - g%dy / 2.d0

    call geo_cell_size_meters(g%x0, g%y0, g%dx, g%dy, g%dx_m, g%dy_m)

    allocate(g%data(g%ncols, g%nrows))

    ! Data is north-first in file; store south-first in memory (j=1 = south).
    do j = g%nrows, 1, -1
      read(u, *, iostat=ios) g%data(1:g%ncols, j)
      if (ios /= 0) then
        write(*,'(A,I0)') 'geo_load_esri_asc: read error at row ', j
        goto 900
      end if
    end do

    call geo_update_stats(g)
    g%path   = trim(path)
    g%fmt    = GEO_FMT_ESRI_ASC
    g%loaded = .true.
    close(u)
    return

900 close(u)
  end function geo_load_esri_asc

  logical function geo_save_esri_asc(g, path) result(ok)
    type(GeoGrid),    intent(inout) :: g
    character(len=*), intent(in)    :: path
    integer :: u, ios, j
    character(len=32) :: fmt

    ok = .false.
    open(newunit=u, file=trim(path), status='UNKNOWN', iostat=ios)
    if (ios /= 0) return

    call geo_update_stats(g)
    write(fmt,'("(",I0,"G16.7)")') g%ncols

    write(u,'(A,I0)')        'ncols         ', g%ncols
    write(u,'(A,I0)')        'nrows         ', g%nrows
    write(u,'(A,F0.10)')     'xllcorner     ', g%x0
    write(u,'(A,F0.10)')     'yllcorner     ', g%y0
    write(u,'(A,F0.10)')     'cellsize      ', g%dx
    write(u,'(A,G0.7)')      'NODATA_value  ', g%nodata

    ! Write north-first (j=nrows down to 1)
    do j = g%nrows, 1, -1
      write(u, trim(fmt), iostat=ios) g%data(1:g%ncols, j)
      if (ios /= 0) goto 900
    end do

    close(u)
    call geo_write_prj(trim(path))
    ok = .true.
    return
900 close(u)
  end function geo_save_esri_asc

  ! --- Internal helpers for ESRI ASC header parsing ---

  subroutine geo_asc_read_kv_int(unit, val, ios)
    integer,          intent(in)  :: unit
    integer(int32),   intent(out) :: val
    integer,          intent(out) :: ios
    character(len=:), allocatable :: line
    character(len=64) :: key
    call geo_getline(unit, line, ios)
    if (ios /= 0) return
    read(line, *, iostat=ios) key, val
    if (ios /= 0) then
      write(*,'(A,A)') 'geo_load_esri_asc: bad header line: ', trim(line)
    end if
  end subroutine geo_asc_read_kv_int

  subroutine geo_asc_read_kv_real(unit, val, actual_key, ios)
    integer,          intent(in)  :: unit
    real(real64),     intent(out) :: val
    character(len=*), intent(out) :: actual_key
    integer,          intent(out) :: ios
    character(len=:), allocatable :: line
    call geo_getline(unit, line, ios)
    if (ios /= 0) return
    read(line, *, iostat=ios) actual_key, val
    if (ios /= 0) then
      write(*,'(A,A)') 'geo_load_esri_asc: bad header line: ', trim(line)
    end if
  end subroutine geo_asc_read_kv_real

  ! ===========================================================================
  ! Section 5: ESRI BIL  (.bil + .hdr)
  ! ===========================================================================
  !
  ! .hdr (text, key-value):
  !   nrows       <int>
  !   ncols       <int>
  !   nbands      1
  !   nbits       32
  !   byteorder   I (little-endian) | M (big-endian)
  !   layout      bil
  !   ulxmap      <real>   X centre of upper-left cell
  !   ulymap      <real>   Y centre of upper-left cell
  !   xdim        <real>   cell width  (degrees)
  !   ydim        <real>   cell height (degrees)
  !   nodata      <real>
  !
  ! .bil (binary):
  !   nrows × ncols float32 values, row-major, north-first.
  ! ===========================================================================

  function geo_load_esri_bil(path) result(g)
    character(len=*), intent(in) :: path
    type(GeoGrid) :: g

    character(len=GEO_PATH_MAX) :: hdrpath
    integer :: uh, ub, ios, j
    character(len=:), allocatable :: line
    character(len=64) :: key
    real(real64) :: ulxmap, ulymap, xdim, ydim
    logical :: have_ulx, have_uly, have_xdim, have_ydim

    ulxmap=0.d0; ulymap=0.d0; xdim=0.d0; ydim=0.d0
    have_ulx=.false.; have_uly=.false.
    have_xdim=.false.; have_ydim=.false.

    hdrpath = geo_replace_ext(trim(path), 'hdr')

    open(newunit=uh, file=trim(hdrpath), status='OLD', action='READ', iostat=ios)
    if (ios /= 0) then
      write(*,'(A,A)') 'geo_load_esri_bil: cannot open header ', trim(hdrpath)
      return
    end if

    do
      call geo_getline(uh, line, ios)
      if (ios /= 0) exit
      if (len_trim(line) == 0) cycle
      read(line, *, iostat=ios) key
      if (ios /= 0) cycle
      call geo_to_lower(key)

      select case (trim(key))
      case ('nrows');   read(line, *, iostat=ios) key, g%nrows
      case ('ncols');   read(line, *, iostat=ios) key, g%ncols
      case ('nodata');  read(line, *, iostat=ios) key, g%nodata
      case ('ulxmap');  read(line, *, iostat=ios) key, ulxmap;  have_ulx  = .true.
      case ('ulymap');  read(line, *, iostat=ios) key, ulymap;  have_uly  = .true.
      case ('xdim');    read(line, *, iostat=ios) key, xdim;    have_xdim = .true.
      case ('ydim');    read(line, *, iostat=ios) key, ydim;    have_ydim = .true.
      end select
    end do
    close(uh)

    if (.not. (have_ulx .and. have_uly .and. have_xdim .and. have_ydim)) then
      write(*,'(A)') 'geo_load_esri_bil: incomplete header (missing ulxmap/ulymap/xdim/ydim)'
      return
    end if
    if (g%ncols <= 0 .or. g%nrows <= 0) then
      write(*,'(A)') 'geo_load_esri_bil: invalid dimensions in header'
      return
    end if

    g%dx = xdim
    g%dy = ydim
    ! ulxmap/ulymap are centers of the upper-left cell.
    ! x0 = left edge of leftmost column = ulxmap - xdim/2
    ! y0 = bottom edge of bottom row    = ulymap - (nrows-1)*ydim - ydim/2
    g%x0 = ulxmap - xdim / 2.d0
    g%y0 = ulymap - real(g%nrows - 1, real64) * ydim - ydim / 2.d0

    call geo_cell_size_meters(g%x0, g%y0, g%dx, g%dy, g%dx_m, g%dy_m)

    allocate(g%data(g%ncols, g%nrows))

    open(newunit=ub, file=trim(path), form='unformatted', access='stream', &
         status='OLD', action='READ', iostat=ios)
    if (ios /= 0) then
      write(*,'(A,A)') 'geo_load_esri_bil: cannot open binary ', trim(path)
      return
    end if

    ! BIL stores rows north-first; read into j = nrows..1
    do j = g%nrows, 1, -1
      read(ub, iostat=ios) g%data(1:g%ncols, j)
      if (ios /= 0) then
        write(*,'(A,I0)') 'geo_load_esri_bil: read error at row ', j
        close(ub)
        return
      end if
    end do
    close(ub)

    call geo_update_stats(g)
    g%path   = trim(path)
    g%fmt    = GEO_FMT_ESRI_BIL
    g%loaded = .true.
  end function geo_load_esri_bil

  logical function geo_save_esri_bil(g, path) result(ok)
    type(GeoGrid),    intent(inout) :: g
    character(len=*), intent(in)    :: path
    character(len=GEO_PATH_MAX) :: hdrpath
    integer :: uh, ub, ios, j
    real(real64) :: ulxmap, ulymap

    ok = .false.
    call geo_update_stats(g)

    ! Reconstruct ulxmap/ulymap (center of upper-left cell)
    ulxmap = g%x0 + g%dx / 2.d0
    ulymap = g%y0 + real(g%nrows - 1, real64) * g%dy + g%dy / 2.d0

    hdrpath = geo_replace_ext(trim(path), 'hdr')
    open(newunit=uh, file=trim(hdrpath), status='UNKNOWN', iostat=ios)
    if (ios /= 0) return

    write(uh,'(A,I0)')     'byteorder      i'
    write(uh,'(A)')        'layout         bil'
    write(uh,'(A,I0)')     'nrows          ', g%nrows
    write(uh,'(A,I0)')     'ncols          ', g%ncols
    write(uh,'(A)')        'nbands         1'
    write(uh,'(A)')        'nbits          32'
    write(uh,'(A)')        'pixeltype      float'
    write(uh,'(A,F0.10)')  'ulxmap         ', ulxmap
    write(uh,'(A,F0.10)')  'ulymap         ', ulymap
    write(uh,'(A,F0.10)')  'xdim           ', g%dx
    write(uh,'(A,F0.10)')  'ydim           ', g%dy
    write(uh,'(A,G0.7)')   'nodata         ', g%nodata
    close(uh)

    open(newunit=ub, file=trim(path), form='unformatted', access='stream', &
         status='UNKNOWN', iostat=ios)
    if (ios /= 0) return

    ! Write north-first
    do j = g%nrows, 1, -1
      write(ub, iostat=ios) g%data(1:g%ncols, j)
      if (ios /= 0) then
        close(ub)
        return
      end if
    end do
    close(ub)

    call geo_write_prj(trim(path))
    ok = .true.
  end function geo_save_esri_bil

  ! ===========================================================================
  ! Section 6: Surfer ASCII  (.grd, magic DSAA)
  ! ===========================================================================
  !
  ! File structure:
  !   DSAA
  !   <ncols> <nrows>
  !   <xmin> <xmax>      (X centres of leftmost/rightmost columns)
  !   <ymin> <ymax>      (Y centres of bottom/top rows)
  !   <zmin> <zmax>
  !   <data rows from south (bottom) to north (top), left to right>
  !
  ! Conversion to internal corner convention:
  !   dx   = (xmax - xmin) / (ncols - 1)
  !   x0   = xmin - dx / 2
  !   (same for y)
  ! ===========================================================================

  function geo_load_surfer_asc(path) result(g)
    character(len=*), intent(in) :: path
    type(GeoGrid) :: g

    integer :: u, ios, j
    character(len=:), allocatable :: line
    character(len=4) :: tag
    real(real64) :: xmin, xmax, ymin, ymax, zmin, zmax

    open(newunit=u, file=trim(path), status='OLD', action='READ', iostat=ios)
    if (ios /= 0) then
      write(*,'(A,A)') 'geo_load_surfer_asc: cannot open ', trim(path)
      return
    end if

    call geo_getline(u, line, ios); if (ios/=0) goto 900
    tag = adjustl(trim(line))
    if (tag /= 'DSAA') then
      write(*,'(A,A)') 'geo_load_surfer_asc: not a Surfer ASCII grid: ', trim(path)
      goto 900
    end if

    call geo_getline(u, line, ios); if (ios/=0) goto 900
    read(line, *, iostat=ios) g%ncols, g%nrows; if (ios/=0) goto 900

    call geo_getline(u, line, ios); if (ios/=0) goto 900
    read(line, *, iostat=ios) xmin, xmax; if (ios/=0) goto 900

    call geo_getline(u, line, ios); if (ios/=0) goto 900
    read(line, *, iostat=ios) ymin, ymax; if (ios/=0) goto 900

    call geo_getline(u, line, ios); if (ios/=0) goto 900
    read(line, *, iostat=ios) zmin, zmax; if (ios/=0) goto 900

    ! Surfer stores node centres.  Convert to corner coordinates.
    if (g%ncols > 1) then
      g%dx = (xmax - xmin) / real(g%ncols - 1, real64)
    else
      g%dx = xmax - xmin
    end if
    if (g%nrows > 1) then
      g%dy = (ymax - ymin) / real(g%nrows - 1, real64)
    else
      g%dy = ymax - ymin
    end if
    g%x0     = xmin - g%dx / 2.d0
    g%y0     = ymin - g%dy / 2.d0
    g%nodata = -9999.d0  ! Surfer ASCII has no explicit nodata; use default

    call geo_cell_size_meters(g%x0, g%y0, g%dx, g%dy, g%dx_m, g%dy_m)

    allocate(g%data(g%ncols, g%nrows))

    ! Data is south-first in file: j = 1, 2, ..., nrows
    do j = 1, g%nrows
      read(u, *, iostat=ios) g%data(1:g%ncols, j)
      if (ios /= 0) then
        write(*,'(A,I0)') 'geo_load_surfer_asc: read error at row ', j
        goto 900
      end if
    end do

    call geo_update_stats(g)
    g%path   = trim(path)
    g%fmt    = GEO_FMT_SURFER_ASC
    g%loaded = .true.
    close(u)
    return

900 close(u)
  end function geo_load_surfer_asc

  logical function geo_save_surfer_asc(g, path) result(ok)
    type(GeoGrid),    intent(inout) :: g
    character(len=*), intent(in)    :: path
    integer :: u, ios, j
    character(len=32) :: fmt
    real(real64) :: xmin, xmax, ymin, ymax

    ok = .false.
    call geo_update_stats(g)

    ! Reconstruct node-centre extents from corner coordinates
    xmin = g%x0 + g%dx / 2.d0
    xmax = xmin + real(g%ncols - 1, real64) * g%dx
    ymin = g%y0 + g%dy / 2.d0
    ymax = ymin + real(g%nrows - 1, real64) * g%dy

    open(newunit=u, file=trim(path), status='UNKNOWN', iostat=ios)
    if (ios /= 0) return

    write(fmt,'("(",I0,"G16.7)")') g%ncols

    write(u,'(A)')         'DSAA'
    write(u,'(I0,1X,I0)')  g%ncols, g%nrows
    write(u,'(G0.10,1X,G0.10)') xmin, xmax
    write(u,'(G0.10,1X,G0.10)') ymin, ymax
    write(u,'(G0.10,1X,G0.10)') g%zmin, g%zmax

    ! Write south-first
    do j = 1, g%nrows
      write(u, trim(fmt), iostat=ios) g%data(1:g%ncols, j)
      if (ios /= 0) goto 900
    end do

    close(u)
    ok = .true.
    return
900 close(u)
  end function geo_save_surfer_asc

  ! ===========================================================================
  ! Section 7: Surfer 6 Binary  (.grd, magic DSBB)
  ! ===========================================================================
  !
  ! Binary layout (little-endian):
  !   "DSBB"      char(4)
  !   ncols       int16
  !   nrows       int16
  !   xmin        float64   (X centre of leftmost  column)
  !   xmax        float64   (X centre of rightmost column)
  !   ymin        float64   (Y centre of bottom row)
  !   ymax        float64   (Y centre of top    row)
  !   zmin        float64
  !   zmax        float64
  !   data        ncols × nrows float32, south-first, left-to-right
  ! ===========================================================================

  function geo_load_surfer6(path) result(g)
    character(len=*), intent(in) :: path
    type(GeoGrid) :: g

    integer :: u, ios
    character(len=4)  :: tag
    integer(int16)    :: nc16, nr16
    real(real64)      :: xmin, xmax, ymin, ymax, zmin, zmax
    integer :: j

    open(newunit=u, file=trim(path), form='unformatted', access='stream', &
         status='OLD', action='READ', iostat=ios)
    if (ios /= 0) then
      write(*,'(A,A)') 'geo_load_surfer6: cannot open ', trim(path)
      return
    end if

    read(u, iostat=ios) tag;  if (ios/=0 .or. tag/='DSBB') goto 900
    read(u, iostat=ios) nc16; if (ios/=0) goto 900
    read(u, iostat=ios) nr16; if (ios/=0) goto 900
    read(u, iostat=ios) xmin, xmax, ymin, ymax, zmin, zmax; if (ios/=0) goto 900

    g%ncols = int(nc16, int32)
    g%nrows = int(nr16, int32)

    if (g%ncols > 1) then
      g%dx = (xmax - xmin) / real(g%ncols - 1, real64)
    else
      g%dx = xmax - xmin
    end if
    if (g%nrows > 1) then
      g%dy = (ymax - ymin) / real(g%nrows - 1, real64)
    else
      g%dy = ymax - ymin
    end if
    g%x0     = xmin - g%dx / 2.d0
    g%y0     = ymin - g%dy / 2.d0
    g%nodata = -9999.d0

    call geo_cell_size_meters(g%x0, g%y0, g%dx, g%dy, g%dx_m, g%dy_m)

    allocate(g%data(g%ncols, g%nrows))

    ! Data is south-first: j = 1 .. nrows
    do j = 1, g%nrows
      read(u, iostat=ios) g%data(1:g%ncols, j)
      if (ios /= 0) then
        write(*,'(A,I0)') 'geo_load_surfer6: read error at row ', j
        goto 900
      end if
    end do

    close(u)
    call geo_update_stats(g)
    g%path   = trim(path)
    g%fmt    = GEO_FMT_SURFER6
    g%loaded = .true.
    return

900 write(*,'(A,A)') 'geo_load_surfer6: format error in ', trim(path)
    close(u)
  end function geo_load_surfer6

  logical function geo_save_surfer6(g, path) result(ok)
    type(GeoGrid),    intent(inout) :: g
    character(len=*), intent(in)    :: path
    integer :: u, ios, j
    integer(int16) :: nc16, nr16
    real(real64)   :: xmin, xmax, ymin, ymax

    ok = .false.
    call geo_update_stats(g)

    if (g%ncols > 32767 .or. g%nrows > 32767) then
      write(*,'(A)') 'geo_save_surfer6: grid too large for Surfer 6 (max 32767)'
      return
    end if

    nc16 = int(g%ncols, int16)
    nr16 = int(g%nrows, int16)
    xmin = g%x0 + g%dx / 2.d0
    xmax = xmin + real(g%ncols - 1, real64) * g%dx
    ymin = g%y0 + g%dy / 2.d0
    ymax = ymin + real(g%nrows - 1, real64) * g%dy

    open(newunit=u, file=trim(path), form='unformatted', access='stream', &
         status='UNKNOWN', iostat=ios)
    if (ios /= 0) return

    write(u, iostat=ios) 'DSBB'
    write(u, iostat=ios) nc16, nr16
    write(u, iostat=ios) xmin, xmax, ymin, ymax, g%zmin, g%zmax

    do j = 1, g%nrows
      write(u, iostat=ios) g%data(1:g%ncols, j)
      if (ios /= 0) then
        close(u)
        return
      end if
    end do
    close(u)
    ok = .true.
  end function geo_save_surfer6

  ! ===========================================================================
  ! Section 8: Surfer 7 Binary  (.grd, magic DSRB)
  ! ===========================================================================
  !
  ! Sectioned binary format (little-endian).  Each section:
  !   tag      char(4)
  !   size     int32    number of bytes that follow in this section
  !   <size bytes of section data>
  !
  ! File header section  (tag "DSRB", size = 8):
  !   version  int32  = 1
  !   unused   int32  = 0
  !
  ! GRID section  (tag "GRID", size = 72):
  !   nrows    int32
  !   ncols    int32
  !   xLL      float64   X centre of lower-left cell
  !   yLL      float64   Y centre of lower-left cell
  !   xSize    float64   cell width   (degrees)
  !   ySize    float64   cell height  (degrees)
  !   zMin     float64
  !   zMax     float64
  !   rotation float64   (0 = north-up)
  !   noData   float64
  !
  ! DATA section  (tag "DATA", size = nrows * ncols * 8):
  !   float64 values, nrows × ncols, south-first, left-to-right
  !
  ! Unknown sections are skipped by advancing the stream position.
  ! ===========================================================================

  function geo_load_surfer7(path) result(g)
    character(len=*), intent(in) :: path
    type(GeoGrid) :: g

    integer :: u, ios
    character(len=4) :: tag
    integer(int32)   :: sec_size
    integer(int32)   :: version, unused_i
    integer(int32)   :: nr32, nc32
    real(real64)     :: xll, yll, xsize, ysize, zmin, zmax, rotation, nodata_val
    logical          :: have_grid, have_data
    integer          :: j
    integer(int64)   :: pos_before, expected_next

    have_grid = .false.
    have_data = .false.

    open(newunit=u, file=trim(path), form='unformatted', access='stream', &
         status='OLD', action='READ', iostat=ios)
    if (ios /= 0) then
      write(*,'(A,A)') 'geo_load_surfer7: cannot open ', trim(path)
      return
    end if

    ! Read file identification section: DSRB + size + version + unused
    read(u, iostat=ios) tag
    if (ios /= 0 .or. tag /= 'DSRB') then
      write(*,'(A,A)') 'geo_load_surfer7: not a Surfer 7 grid: ', trim(path)
      goto 900
    end if
    read(u, iostat=ios) sec_size   ! should be 8
    if (ios /= 0) goto 900
    read(u, iostat=ios) version, unused_i
    if (ios /= 0) goto 900

    ! Read sections until we have both GRID and DATA
    do
      read(u, iostat=ios) tag
      if (ios /= 0) exit          ! EOF

      read(u, iostat=ios) sec_size
      if (ios /= 0) goto 900

      ! Record stream position immediately after size field
      inquire(unit=u, pos=pos_before)
      expected_next = int(pos_before, int64) + int(sec_size, int64)

      select case (tag)

      case ('GRID')
        read(u, iostat=ios) nr32, nc32
        read(u, iostat=ios) xll, yll, xsize, ysize
        read(u, iostat=ios) zmin, zmax, rotation, nodata_val
        if (ios /= 0) goto 900
        g%nrows  = nr32
        g%ncols  = nc32
        g%dx     = xsize
        g%dy     = ysize
        g%x0     = xll - xsize / 2.d0
        g%y0     = yll - ysize / 2.d0
        g%nodata = nodata_val
        call geo_cell_size_meters(g%x0, g%y0, g%dx, g%dy, g%dx_m, g%dy_m)
        allocate(g%data(g%ncols, g%nrows))
        have_grid = .true.

      case ('DATA')
        if (.not. have_grid) then
          write(*,'(A)') 'geo_load_surfer7: DATA section before GRID section'
          goto 900
        end if
        ! Read float64 data, south-first; downcast to float32
        block
          real(real64), allocatable :: row64(:)
          allocate(row64(g%ncols))
          do j = 1, g%nrows
            read(u, iostat=ios) row64
            if (ios /= 0) then
              write(*,'(A,I0)') 'geo_load_surfer7: read error at row ', j
              goto 900
            end if
            g%data(1:g%ncols, j) = real(row64, real32)
          end do
          deallocate(row64)
        end block
        have_data = .true.
        exit    ! Done

      case default
        ! Skip unknown section by reading sec_size bytes
        block
          integer(int8) :: dummy
          integer :: k
          do k = 1, sec_size
            read(u, iostat=ios) dummy
            if (ios /= 0) exit
          end do
        end block

      end select
    end do

    close(u)
    if (.not. have_grid .or. .not. have_data) then
      write(*,'(A,A)') 'geo_load_surfer7: incomplete file (missing GRID or DATA): ', &
                        trim(path)
      return
    end if

    call geo_update_stats(g)
    g%path   = trim(path)
    g%fmt    = GEO_FMT_SURFER7
    g%loaded = .true.
    return

900 write(*,'(A,A)') 'geo_load_surfer7: format error in ', trim(path)
    close(u)
  end function geo_load_surfer7

  logical function geo_save_surfer7(g, path) result(ok)
    type(GeoGrid),    intent(inout) :: g
    character(len=*), intent(in)    :: path
    integer :: u, ios, j
    integer(int32) :: sec_size32
    real(real64) :: xll, yll
    real(real64), allocatable :: row64(:)

    ok = .false.
    call geo_update_stats(g)

    xll = g%x0 + g%dx / 2.d0
    yll = g%y0 + g%dy / 2.d0

    open(newunit=u, file=trim(path), form='unformatted', access='stream', &
         status='UNKNOWN', iostat=ios)
    if (ios /= 0) return

    ! File header section: DSRB + size(8) + version(1) + unused(0)
    write(u) 'DSRB'
    write(u) 8_int32
    write(u) 1_int32, 0_int32

    ! GRID section: size = 2*4 + 8*8 = 72
    write(u) 'GRID'
    write(u) 72_int32
    write(u) int(g%nrows, int32), int(g%ncols, int32)
    write(u) xll, yll, g%dx, g%dy
    write(u) g%zmin, g%zmax, 0.d0, g%nodata

    ! DATA section: size = nrows * ncols * 8
    sec_size32 = int(g%nrows, int32) * int(g%ncols, int32) * 8_int32
    write(u) 'DATA'
    write(u) sec_size32

    allocate(row64(g%ncols))
    do j = 1, g%nrows
      row64 = real(g%data(1:g%ncols, j), real64)
      write(u, iostat=ios) row64
      if (ios /= 0) then
        deallocate(row64)
        close(u)
        return
      end if
    end do
    deallocate(row64)
    close(u)
    ok = .true.
  end function geo_save_surfer7

  ! ===========================================================================
  ! Section 9: Plain text  (FRF = south-first,  LRF = north-first)
  ! ===========================================================================
  !
  ! No header.  All geometry is supplied by the caller.
  ! Values are whitespace-separated floating-point numbers.
  ! FRF: first data row in file = j=1 (south / minimum Y).
  ! LRF: first data row in file = j=nrows (north / maximum Y).
  ! ===========================================================================

  function geo_load_txt(path, ncols, nrows, x0, y0, dx, dy, nodata, lrf) result(g)
    character(len=*), intent(in)           :: path
    integer(int32),   intent(in)           :: ncols, nrows
    real(real64),     intent(in)           :: x0, y0, dx, dy
    real(real64),     intent(in)           :: nodata
    logical,          intent(in), optional :: lrf   ! .true. = north-first (LRF)
    type(GeoGrid) :: g

    integer :: u, ios, j
    logical :: north_first

    north_first = .false.
    if (present(lrf)) north_first = lrf

    g%ncols  = ncols
    g%nrows  = nrows
    g%x0     = x0
    g%y0     = y0
    g%dx     = dx
    g%dy     = dy
    g%nodata = nodata
    call geo_cell_size_meters(x0, y0, dx, dy, g%dx_m, g%dy_m)

    open(newunit=u, file=trim(path), status='OLD', action='READ', iostat=ios)
    if (ios /= 0) then
      write(*,'(A,A)') 'geo_load_txt: cannot open ', trim(path)
      return
    end if

    allocate(g%data(ncols, nrows))

    if (north_first) then
      ! LRF: first row in file = north = j=nrows
      do j = nrows, 1, -1
        read(u, *, iostat=ios) g%data(1:ncols, j)
        if (ios /= 0) then
          write(*,'(A,I0)') 'geo_load_txt: read error at row ', j
          goto 900
        end if
      end do
    else
      ! FRF: first row in file = south = j=1
      do j = 1, nrows
        read(u, *, iostat=ios) g%data(1:ncols, j)
        if (ios /= 0) then
          write(*,'(A,I0)') 'geo_load_txt: read error at row ', j
          goto 900
        end if
      end do
    end if

    close(u)
    call geo_update_stats(g)
    g%path   = trim(path)
    g%fmt    = merge(GEO_FMT_TXT_LRF, GEO_FMT_TXT_FRF, north_first)
    g%loaded = .true.
    return

900 close(u)
  end function geo_load_txt

  logical function geo_save_txt(g, path, lrf, fmt_str) result(ok)
    type(GeoGrid),    intent(inout)        :: g
    character(len=*), intent(in)           :: path
    logical,          intent(in), optional :: lrf       ! .true. = write north-first
    character(len=*), intent(in), optional :: fmt_str   ! override Fortran format
    integer :: u, ios, j
    logical :: north_first
    character(len=64) :: wfmt

    ok = .false.
    north_first = .false.
    if (present(lrf)) north_first = lrf

    if (present(fmt_str)) then
      wfmt = trim(fmt_str)
    else
      write(wfmt,'("(",I0,"G16.7)")') g%ncols
    end if

    open(newunit=u, file=trim(path), status='UNKNOWN', iostat=ios)
    if (ios /= 0) return

    if (north_first) then
      do j = g%nrows, 1, -1
        write(u, trim(wfmt), iostat=ios) g%data(1:g%ncols, j)
        if (ios /= 0) goto 900
      end do
    else
      do j = 1, g%nrows
        write(u, trim(wfmt), iostat=ios) g%data(1:g%ncols, j)
        if (ios /= 0) goto 900
      end do
    end if

    close(u)
    ok = .true.
    return
900 close(u)
  end function geo_save_txt

  ! ===========================================================================
  ! Section 10: Grid analysis utilities
  ! ===========================================================================

  ! ---------------------------------------------------------------------------
  ! Find the maximum absolute difference between two grids (same dimensions).
  ! Also returns the relative error percentage and the location (col, row).
  ! Returns 0.0 if grids match exactly; returns -1.0 on dimension mismatch.
  ! ---------------------------------------------------------------------------
  real(real64) function geo_diff(a, b, max_col, max_row, rel_pct) result(max_diff)
    type(GeoGrid), intent(in)            :: a, b
    integer,       intent(out), optional :: max_col, max_row
    real(real64),  intent(out), optional :: rel_pct
    integer :: i, j, bi, bj
    real(real64) :: diff, av, bv

    max_diff = -1.d0
    if (a%ncols /= b%ncols .or. a%nrows /= b%nrows) then
      write(*,'(A,2(I0,A,I0,1X))') &
        'geo_diff: dimension mismatch: ', a%ncols,'x',a%nrows, b%ncols,'x',b%nrows
      return
    end if

    max_diff = 0.d0
    bi = 1; bj = 1
    do j = 1, a%nrows
      do i = 1, a%ncols
        av   = real(a%data(i,j), real64)
        bv   = real(b%data(i,j), real64)
        diff = abs(bv - av)
        if (diff > max_diff) then
          max_diff = diff
          bi = i; bj = j
        end if
      end do
    end do

    if (present(max_col)) max_col = bi
    if (present(max_row)) max_row = bj
    if (present(rel_pct)) then
      av = real(a%data(bi, bj), real64)
      bv = real(b%data(bi, bj), real64)
      if (max(abs(av), abs(bv)) > 0.d0) then
        rel_pct = (max_diff / max(abs(av), abs(bv))) * 100.d0
      else
        rel_pct = 0.d0
      end if
    end if
  end function geo_diff

end module GeoFortranMod
