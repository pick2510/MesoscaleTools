!
!  Description: how to get PV values.
!
!
!
program get_pv
  use grib_api
  implicit none
  integer                         :: infile
  integer                         :: igrib
  integer                         :: iret
  integer                         :: PVPresent, nb_pv, ishape
  double precision, dimension(:), allocatable :: pv
  character(len=300) :: f
  character(30) :: cval, aval, bval
  character(200) :: usage
  logical :: file_exists, found_pv
  integer :: i,c
  usage = "Create PV coefficients file for WRF"//NEW_LINE('A')//"&
  &Usage ./createECMWFcoeffs [file_with_model_levels.grb]"
  c = command_argument_count()
  if (c == 0) then
    write(*,*) usage
    call exit(-1)
  endif
  if (c /= 1) then
    write(*,*) usage
    call exit(-1)
  endif
  call get_command_argument(1, f)
  inquire(FILE=f, EXIST=file_exists)
  if (file_exists /= .TRUE.) then
        write (*,*) "File doesn't exist:", f, usage
        call exit(-1)
  endif
  found_pv = .FALSE.
  call grib_open_file(infile, f, 'r')
 
  call grib_new_from_file(infile,igrib, iret)
 
  do while  (iret /= GRIB_END_OF_FILE)
       call grib_get(igrib,'PVPresent',PVPresent)
          if (PVPresent == 1) then
             call grib_get_size(igrib,'pv',nb_pv)
             print*, "There are ", nb_pv, " PV values"
             allocate(pv(nb_pv))
             call grib_get(igrib,'pv',pv)
             !print*, "pv = ", pv
             found_pv = .TRUE.
             exit          
          else
       call grib_new_from_file(infile, igrib, iret)  
        end if 
  end do
  call grib_release(igrib)
  call grib_close_file(infile)
  if (found_pv == .FALSE.) then
          write(*,*) "No PV Values found in File"
          call exit(-1)
  end if
  ishape = size(pv)/2
  open (20, FILE="ecmwf_coeffs", action="write", status="replace")
  do i=1,ishape
      write (*, '(I0, A, F0.6, A, F0.6)') i-1, " ", pv(i), " ",  pv(i+ishape)
      write (20, '(I0, A, F0.6, A, F0.6)', advance='yes') i-1, " ", pv(i), " ",  pv(i+ishape)
  end do
  close(20)
  deallocate(pv)
end program get_pv
