! =============================================================================
! create_grids.f90 — Example: create a synthetic bathymetry grid and save
!                    it in every format supported by GeoFortranMod.
! =============================================================================
!
! Usage:  create_grids [output_dir]
!   Default output_dir = "."
!
! Produces files:
!   synth_bathy.asc      ESRI ASCII
!   synth_bathy.bil      ESRI BIL binary  (+synth_bathy.hdr)
!   synth_bathy_dsaa.grd Surfer ASCII
!   synth_bathy_dsbb.grd Surfer 6 Binary
!   synth_bathy_dsrb.grd Surfer 7 Binary
!   synth_bathy_frf.txt  Plain text, south-first (FRF)
!   synth_bathy_lrf.txt  Plain text, north-first (LRF)
!
! The synthetic grid is a simple ocean bowl:
!   depth = -1000 * (1 - ((x-cx)**2 + (y-cy)**2) / r**2)
! clipped to [-3000, 0] m.
! =============================================================================
program create_grids
  use GeoFortranMod
  use, intrinsic :: iso_fortran_env, only: real32, real64
  implicit none

  integer, parameter :: NCOLS = 120
  integer, parameter :: NROWS = 90

  ! Grid extent: small Pacific region
  real(real64), parameter :: X0      = -85.d0   ! lower-left corner lon (°)
  real(real64), parameter :: Y0      = -15.d0   ! lower-left corner lat (°)
  real(real64), parameter :: DX      = 1.d0/12.d0  ! 5-arc-minute cell (~9.25 km)
  real(real64), parameter :: NODATA  = -9999.d0

  type(GeoGrid) :: g
  integer :: i, j
  real(real64) :: cx, cy, r2, xx, yy, depth
  character(len=256) :: outdir, fpath
  logical :: ok
  integer :: nargs

  ! --- Parse optional output directory argument ---
  nargs = command_argument_count()
  if (nargs >= 1) then
    call get_command_argument(1, outdir)
  else
    outdir = '.'
  end if

  write(*,'(A,A)') 'Output directory: ', trim(outdir)

  ! --- Build synthetic grid ---
  g = geo_create(NCOLS, NROWS, X0, Y0, DX, DX, NODATA)
  cx = X0 + NCOLS * DX / 2.d0   ! centre lon
  cy = Y0 + NROWS * DX / 2.d0   ! centre lat
  r2 = (NCOLS * DX / 2.d0)**2 + (NROWS * DX / 2.d0)**2

  do j = 1, NROWS
    do i = 1, NCOLS
      xx = X0 + (i - 0.5d0) * DX - cx
      yy = Y0 + (j - 0.5d0) * DX - cy
      depth = -2500.d0 * (1.d0 - (xx**2 + yy**2) / r2)
      ! Add shallow shelf near edges
      if (abs(xx) > NCOLS*DX*0.35d0 .or. abs(yy) > NROWS*DX*0.35d0) then
        depth = max(depth, -200.d0)
      end if
      g%data(i, j) = real(max(min(depth, 0.d0), -3000.d0), real32)
    end do
  end do

  write(*,'(A,F8.1,A,F8.1)') 'Grid zmin = ', minval(g%data), '  zmax = ', maxval(g%data)
  write(*,'(A,F8.2,A,F8.2,A,F8.2,A,F8.2)') &
        'Extent: lon [', g%x0, ', ', g%x0 + g%ncols*g%dx, &
        ']  lat [', g%y0, ', ', g%y0 + g%nrows*g%dy, ']'
  write(*,'(A,F8.3,A,F8.3)') 'Cell size dx_m ≈ ', g%dx_m, '  dy_m ≈ ', g%dy_m

  ! --- ESRI ASCII ---
  fpath = trim(outdir)//'/synth_bathy.asc'
  ok = geo_save_grid(g, trim(fpath))
  call report('ESRI ASCII      ', trim(fpath), ok)

  ! --- ESRI BIL ---
  fpath = trim(outdir)//'/synth_bathy.bil'
  ok = geo_save_grid(g, trim(fpath))
  call report('ESRI BIL        ', trim(fpath), ok)

  ! --- Surfer ASCII ---
  fpath = trim(outdir)//'/synth_bathy_dsaa.grd'
  ok = geo_save_grid(g, trim(fpath))
  call report('Surfer ASCII    ', trim(fpath), ok)

  ! --- Surfer 6 Binary ---
  fpath = trim(outdir)//'/synth_bathy_dsbb.grd'
  ok = geo_save_surfer6(g, trim(fpath))
  call report('Surfer 6 Binary ', trim(fpath), ok)

  ! --- Surfer 7 Binary ---
  fpath = trim(outdir)//'/synth_bathy_dsrb.grd'
  ok = geo_save_surfer7(g, trim(fpath))
  call report('Surfer 7 Binary ', trim(fpath), ok)

  ! --- Plain text FRF (south-first) ---
  fpath = trim(outdir)//'/synth_bathy_frf.txt'
  ok = geo_save_txt(g, trim(fpath), lrf=.false.)
  call report('Text FRF        ', trim(fpath), ok)

  ! --- Plain text LRF (north-first) ---
  fpath = trim(outdir)//'/synth_bathy_lrf.txt'
  ok = geo_save_txt(g, trim(fpath), lrf=.true.)
  call report('Text LRF        ', trim(fpath), ok)

  write(*,'(A)') 'Done.'

contains

  subroutine report(label, path, ok)
    character(len=*), intent(in) :: label, path
    logical,          intent(in) :: ok
    if (ok) then
      write(*,'(A,A,A)') '  [OK]  ', label, trim(path)
    else
      write(*,'(A,A,A)') '  [ERR] ', label, trim(path)
    end if
  end subroutine report

end program create_grids
