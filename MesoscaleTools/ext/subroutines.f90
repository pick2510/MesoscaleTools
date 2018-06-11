module utils
contains

SUBROUTINE DINTERP3DZ(data3d, out2d, zdata, desiredloc, nx, ny, nz, missingval)
    IMPLICIT NONE

    !f2py threadsafe
    !f2py intent(in,out) :: out2d

    INTEGER, INTENT(IN) :: nx, ny, nz
    REAL(KIND=8), DIMENSION(nx,ny,nz), INTENT(IN) ::  data3d
    REAL(KIND=8), DIMENSION(nx,ny), INTENT(OUT) :: out2d
    REAL(KIND=8), DIMENSION(nx,ny,nz), INTENT(IN) :: zdata
    REAL(KIND=8), INTENT(IN) :: desiredloc
    REAL(KIND=8), INTENT(IN) :: missingval

! 

    INTEGER :: i,j,kp,ip,im
    LOGICAL :: dointerp
    REAL(KIND=8) :: w1,w2
   !  write(*,*) nx, ny, nz
    ! does vertical coordinate increase or decrease with increasing k?
    ! set offset appropriately

    ip = 0
    im = 1
    IF (zdata(1,1,1) .GT. zdata(1,1,nz)) THEN
        ip = 1
        im = 0
    END IF

    !$OMP PARALLEL DO COLLAPSE(2) PRIVATE(i,j,kp,dointerp,w1,w2) &
    !$OMP FIRSTPRIVATE(ip,im) SCHEDULE(runtime)
    DO i = 1,nx
        DO j = 1,ny
            ! Initialize to missing.  Was initially hard-coded to -999999.
            out2d(i,j) = missingval
            dointerp = .FALSE.
            kp = nz

            DO WHILE ((.NOT. dointerp) .AND. (kp >= 2))
                IF (((zdata(i,j,kp-im) < desiredloc) .AND. (zdata(i,j,kp-ip) > desiredloc))) THEN
                    w2 = (desiredloc - zdata(i,j,kp-im))/(zdata(i,j,kp-ip) - zdata(i,j,kp-im))
                    w1 = 1.D0 - w2
                    out2d(i,j) = w1*data3d(i,j,kp-im) + w2*data3d(i,j,kp-ip)
                    dointerp = .TRUE.
                END IF
                kp = kp - 1
            END DO
        END DO
    END DO
    !$OMP END PARALLEL DO

    RETURN

END SUBROUTINE DINTERP3DZ

! PORT  HERE

SUBROUTINE DZSTAG(znew, nx, ny, nz, z, nxz, nyz ,nzz, terrain)
    IMPLICIT NONE

    !f2py threadsafe
    !f2py intent(in,out) :: znew

    INTEGER, INTENT(IN) :: nx, ny, nz, nxz, nyz, nzz
    REAL(KIND=8), DIMENSION(nx,ny,nz), INTENT(OUT) :: znew
    REAL(KIND=8), DIMENSION(nxz,nyz,nzz), INTENT(IN) :: z
    REAL(KIND=8), DIMENSION(nxz,nyz), INTENT(IN) :: terrain
! NCLEND

    INTEGER :: i,j,k,ii,im1,jj,jm1

    ! check for u, v, or w (x,y,or z) staggering
    ! for x and y stag, avg z to x, y, point
    IF (nx .GT. nxz) THEN
        DO k = 1,nz
            DO j = 1,ny
                DO i = 1,nx
                    ii = MIN(i,nxz)
                    im1 = MAX(i-1,1)
                    znew(i,j,k) = 0.5D0*(z(ii,j,k) + z(im1,j,k))
                END DO
            END DO
        END DO

    ELSE IF (ny .GT. nyz) THEN
        DO k = 1,nz
            DO j = 1,NY
                jj = MIN(j,nyz)
                jm1 = MAX(j-1,1)
                DO i = 1,nx
                    znew(i,j,k) = 0.5D0*(z(i,jj,k) + z(i,jm1,k))
                END DO
            END DO
        END DO

    ! w (z) staggering
    ELSE IF (nz .GT. nzz) THEN
        DO j = 1,ny
            DO i = 1,nx
                znew(i,j,1) = terrain(i,j)
            END DO
        END DO

        DO k = 2,nz
            DO j = 1,ny
                DO i = 1,nx
                    znew(i,j,k) = znew(i,j,k-1) + 2.D0*(z(i,j,k-1) - znew(i,j,k-1))
                END DO
            END DO
        END DO

    END IF

    RETURN

    END SUBROUTINE DZSTAG


    SUBROUTINE DROTATECOORDS(ilat, ilon, olat, olon, lat_np, lon_np, lon_0, direction)

    IMPLICIT NONE

    !f2py threadsafe
    !f2py intent(out) :: olat, olon

    REAL(KIND=8), INTENT(IN) :: ilat, ilon
    REAL(KIND=8), INTENT(OUT) :: olat, olon
    REAL(KIND=8), INTENT(IN) :: lat_np, lon_np, lon_0
    INTEGER, INTENT(IN) :: direction


    !  >=0, default : computational -> geographical
    !  < 0          : geographical  -> computational

    REAL(KIND=8) :: rlat, rlon
    REAL(KIND=8) :: phi_np, lam_np, lam_0, dlam
    REAL(KIND=8) :: sinphi, cosphi, coslam, sinlam
    REAL(KIND=8), PARAMETER :: PI=3.141592653589793D0
    REAL(KIND=8), PARAMETER :: RAD_PER_DEG=PI/180.D0
    REAL(KIND=8), PARAMETER :: DEG_PER_RAD=180.D0/PI

    !convert all angles to radians
    phi_np = lat_np*RAD_PER_DEG
    lam_np = lon_np*RAD_PER_DEG
    lam_0 = lon_0*RAD_PER_DEG
    rlat = ilat*RAD_PER_DEG
    rlon = ilon*RAD_PER_DEG

    IF (direction .LT. 0) THEN
    ! the equations are exactly the same except for one
    ! small difference with respect to longitude ...
        dlam = pi - lam_0
    ELSE
        dlam = lam_np
    END IF

    sinphi = COS(phi_np)*COS(rlat)*COS(rlon - dlam) + SIN(phi_np)*SIN(rlat)
    cosphi = SQRT(1.D0 - sinphi*sinphi)
    coslam = SIN(phi_np)*COS(rlat)*COS(rlon - dlam) - COS(phi_np)*SIN(rlat)
    sinlam = COS(rlat)*SIN(rlon - dlam)

    IF (cosphi.NE.0.D0) THEN
        coslam = coslam/cosphi
        sinlam = sinlam/cosphi
    END IF

    olat = DEG_PER_RAD*ASIN(sinphi)
    olon = DEG_PER_RAD*(ATAN2(sinlam,coslam) - dlam - lam_0 + lam_np)

    RETURN

END SUBROUTINE DROTATECOORDS


  
function dinterp2d(workarr, nx, ny, nxx, nyy, x_array, y_array, x, y, ierr)
    implicit none
    integer, intent(in) :: nx, ny, nxx, nyy
    integer, intent(out) :: ierr
    double precision, intent(in) :: x,y
    integer :: i,j
    double precision :: x0,y0,x1,y1,x2,y2,t,u, denom
    double precision, dimension(nxx), intent(in) :: x_array
    double precision, dimension(nyy), intent(in) :: y_array
    double precision, dimension(nx,ny), intent(in) :: workarr
    double precision :: dinterp2d
    ierr = 0
    if (x < minval(x_array) .or. x > maxval(x_array)) then
        ierr = -1 
    else if (y < minval(y_array) .or. y > maxval(y_array)) then
        ierr = -1
    end if
    x0 = x
    y0 = y
    i = binarysearch(nxx,x_array, x0)
    j = binarysearch(nyy,y_array, y0)
    x1 = x_array(i)
    x2 = x_array(i+1)
    y1 = y_array(j)
    y2 = y_array(j+1)
!    t = (x0 - x1)/(x2-x1)
!    u = (y0 - y1)/(y2-y1)
    denom = (x2 - x1)*(y2 - y1)
    dinterp2d = (workarr(i,j)*(x2-x)*(y2-y) + workarr(i+1,j)*(x-x1)*(y2-y) + &
    workarr(i,j+1)*(x2-x)*(y-y1) + workarr(i+1, j+1)*(x-x1)*(y-y1))/denom
!    dinterp2d = (workarr(j,i)*(x2-x)*(y2-y) + workarr(j+1,i)*(x-x1)*(y2-y) + &
!    workarr(j,i+1)*(x2-x)*(y-y1) + workarr(j+1, i+1)*(x-x1)*(y-y1))/denom
 
   end function dinterp2d


    function binarysearch(length, array, value, delta)
        ! Given an array and a value, returns the index of the element that
        ! is closest to, but less than, the given value.
        ! Uses a binary search algorithm.
        ! "delta" is the tolerance used to determine if two values are equal
        ! if ( abs(x1 - x2) <= delta) then
        !    assume x1 = x2
        ! endif

        implicit none
        double precision, dimension(:), intent(in) :: array
        double precision, intent(in) :: value
        double precision, intent(in), optional :: delta
        integer, intent(in) :: length


        integer :: binarysearch

        integer :: left, middle, right
        double precision :: d

        if (present(delta) .eqv. .true.) then
            d = delta
        else
            d = 1e-9
        endif
        
        left = 1
        right = length
        do
            if (left > right) then
                exit
            endif
            middle = nint((left+right) / 2.0)
            if ( abs(array(middle) - value) <= d) then
                binarysearch = middle
                return
            else if (array(middle) > value) then
                right = middle - 1
            else
                left = middle + 1
            end if
        end do
        binarysearch = right

    end function binarysearch



    subroutine dinterp1d (data1d, coords, ncords, loc, out, ierr)
    implicit none
    integer, intent(in) :: ncords
    double precision, intent(in) :: loc
    double precision, intent(in), dimension(ncords) :: data1d, coords
    double precision, intent(out) :: out
    integer, intent(out) :: ierr
    integer :: i

    ierr  = 0
    if (loc < minval(coords) .or. loc > maxval(coords)) then
        ierr = -1 
    endif
    i = binarysearch(ncords, coords, loc)
    out = data1d(i) + (data1d(i+1)- data1d(i))/(coords(i+1) - coords(i)) * (loc - coords(i)) 
    end subroutine dinterp1d

    subroutine dinterp1darray(data1d, coords, ncords, locarr, nlocs, out, ierr)
    implicit none
    integer, intent(in) :: ncords, nlocs
    double precision, intent(in), dimension(nlocs) :: locarr
    double precision, intent(in), dimension(ncords) :: data1d, coords
    double precision, intent(out), dimension(nlocs) :: out
    double precision, dimension(nlocs) :: res
    integer, intent(out) :: ierr
    integer :: i
    ierr = 0
    if (minval(locarr) < minval(coords) .or. maxval(locarr) > maxval(coords)) then
        ierr = -1
    endif
 !$OMP PARALLEL DO PRIVATE(i) SHARED(data1d, coords, locarr, ierr, ncords)
    do i=1,nlocs
        call dinterp1d(data1d, coords, ncords, locarr(i), res(i), ierr)
    end do
 !$OMP END PARALLEL DO
    out = res
    end subroutine dinterp1darray

    end module utils