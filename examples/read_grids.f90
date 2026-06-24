! =============================================================================
! read_grids.f90 — Example: read grid files and print metadata + statistics.
! =============================================================================
!
! Usage:  read_grids <file> [<file> ...]
!
! For plain-text TXT files supply extra arguments:
!   read_grids myfile.txt ncols nrows x0 y0 dx dy [nodata] [lrf]
!
! The program prints metadata for each file and optionally a small excerpt
! of data values near the grid centre.
! =============================================================================
program read_grids
  use GeoFortranMod
  use, intrinsic :: iso_fortran_env, only: real32, real64
  implicit none

  integer :: nargs, iarg
  character(len=512) :: path, tmp
  type(GeoGrid) :: g
  integer :: fmt
  logical :: is_txt

  ! For TXT files
  integer(int32) :: txt_nc, txt_nr
  real(real64)   :: txt_x0, txt_y0, txt_dx, txt_nodata
  logical        :: txt_lrf

  nargs = command_argument_count()
  if (nargs < 1) then
    write(*,'(A)') 'Usage: read_grids <file> [<file> ...]'
    write(*,'(A)') '       For .txt: read_grids file.txt ncols nrows x0 y0 dx [nodata] [lrf=0|1]'
    stop
  end if

  iarg = 1
  do while (iarg <= nargs)
    call get_command_argument(iarg, path)
    iarg = iarg + 1

    fmt    = geo_detect_format(trim(path))
    is_txt = (fmt == GEO_FMT_TXT_FRF .or. fmt == GEO_FMT_UNKNOWN)

    if (is_txt) then
      ! Read extra geometry arguments for TXT
      if (nargs - iarg + 1 < 5) then
        write(*,'(A,A)') 'read_grids: TXT file requires ncols nrows x0 y0 dx: ', trim(path)
        stop
      end if
      call get_command_argument(iarg,   tmp); read(tmp,*) txt_nc;  iarg = iarg + 1
      call get_command_argument(iarg,   tmp); read(tmp,*) txt_nr;  iarg = iarg + 1
      call get_command_argument(iarg,   tmp); read(tmp,*) txt_x0;  iarg = iarg + 1
      call get_command_argument(iarg,   tmp); read(tmp,*) txt_y0;  iarg = iarg + 1
      call get_command_argument(iarg,   tmp); read(tmp,*) txt_dx;  iarg = iarg + 1

      txt_nodata = -9999.d0
      txt_lrf    = .false.

      if (iarg <= nargs) then
        call get_command_argument(iarg, tmp)
        read(tmp,*,iostat=fmt) txt_nodata   ! reuse fmt as iostat
        if (fmt == 0) iarg = iarg + 1
      end if

      if (iarg <= nargs) then
        call get_command_argument(iarg, tmp)
        read(tmp,*) fmt                      ! 0=FRF, 1=LRF
        txt_lrf = (fmt /= 0)
        iarg = iarg + 1
      end if

      g = geo_load_grid(trim(path), &
                        txt_ncols=txt_nc, txt_nrows=txt_nr, &
                        txt_x0=txt_x0, txt_y0=txt_y0, &
                        txt_dx=txt_dx, txt_dy=txt_dx, &
                        txt_nodata=txt_nodata, txt_lrf=txt_lrf)
    else
      g = geo_load_grid(trim(path))
    end if

    if (.not. g%loaded) then
      write(*,'(A,A)') 'read_grids: failed to load ', trim(path)
      cycle
    end if

    call print_grid_info(g)

  end do

contains

  subroutine print_grid_info(g)
    type(GeoGrid), intent(in) :: g
    integer :: ci, cj, w, i, j
    character(len=20) :: fmt_name

    select case (g%fmt)
    case (GEO_FMT_ESRI_ASC);   fmt_name = 'ESRI ASCII'
    case (GEO_FMT_ESRI_BIL);   fmt_name = 'ESRI BIL'
    case (GEO_FMT_SURFER_ASC); fmt_name = 'Surfer ASCII'
    case (GEO_FMT_SURFER6);    fmt_name = 'Surfer 6 Binary'
    case (GEO_FMT_SURFER7);    fmt_name = 'Surfer 7 Binary'
    case (GEO_FMT_TXT_FRF);    fmt_name = 'Text FRF'
    case (GEO_FMT_TXT_LRF);    fmt_name = 'Text LRF'
    case default;               fmt_name = 'Unknown'
    end select

    write(*,'(A)')      '-----------------------------------------------'
    write(*,'(A,A)')    'File   : ', trim(g%path)
    write(*,'(A,A)')    'Format : ', trim(fmt_name)
    write(*,'(A,I0,A,I0)') 'Size   : ', g%ncols, ' cols × ', g%nrows, ' rows'
    write(*,'(A,F12.6,A,F12.6)') 'Lon    : ', g%x0, '  ..  ', g%x0 + g%ncols*g%dx
    write(*,'(A,F12.6,A,F12.6)') 'Lat    : ', g%y0, '  ..  ', g%y0 + g%nrows*g%dy
    write(*,'(A,F12.8,A,F12.8)') 'dx     : ', g%dx, ' °   dy = ', g%dy, ' °'
    write(*,'(A,F10.2,A,F10.2)') 'dx_m   : ', g%dx_m, ' m   dy_m = ', g%dy_m, ' m'
    write(*,'(A,F12.4,A,F12.4)') 'zmin   : ', g%zmin, '  zmax = ', g%zmax
    write(*,'(A,G0.4)')           'nodata : ', g%nodata

    ! Print a 5×5 excerpt near the grid centre
    ci = max(1, g%ncols/2 - 2)
    cj = max(1, g%nrows/2 - 2)
    w  = min(5, g%ncols - ci + 1)

    write(*,'(A,2(I0,A))') 'Excerpt (col ', ci, '..', ci+w-1, ', rows near centre):'
    do j = min(cj+4, g%nrows), cj, -1
      write(*,'(A,I4,A,5F10.2)') '  row', j, ': ', g%data(ci:ci+w-1, j)
    end do
    write(*,'(A)') '-----------------------------------------------'
    write(*,*)
  end subroutine print_grid_info

end program read_grids
