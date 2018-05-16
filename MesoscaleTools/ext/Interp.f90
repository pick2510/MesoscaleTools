! NCLFORTSTART
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

! NCLEND

    INTEGER :: i,j,kp,ip,im
    LOGICAL :: dointerp
    REAL(KIND=8) :: w1,w2

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

! PORT DZSTAG HERE

! NCLFORTSTART
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
