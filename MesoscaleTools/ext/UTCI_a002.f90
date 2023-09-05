    !~ UTCI, Version a 0.002, October 2009
    !~ Copyright (C) 2009  Peter Broede
    
    !~ Program for calculating UTCI Temperature (UTCI)
    !~ released for public use after termination of COST Action 730
    
    !~ replaces Version a 0.001, from September 2009

    !~ Version History:

    !~ pre-alpha version  October 2008:
    !~ based on calculations as presented at the MC/WG meeting in Eilat, 2008-09-10

    !~ Version a 0.001, September 2009:
    !~ Changes compared to the pre-alpha version were based on decisions after the
    !~ WG1 meeting in Stuttgart 2009-02-24:
     !~ - Changed clothing model with respect to the maximum insulation and 
       !~ to reduction of clothing insulation by wind speed
     !~ - Changed physiological model with respect to hidromeiosis
     !~ - Equivalent temperature calculated by explicit comparisons
       !~ to values of a 'Response Index' computed
       !~ as first principal component from values after 30 and 120 min of 
       !~ rectal temperature, mean skin temperature, face skin temperature,
       !~ sweat rate, wetted skin area, shivering, skin blood flow.
     !~ - as in the pre-alpha version, UTCI values are approximated by a 6th order polynomial regression function
     !~ - The limits of mean radiant temperatures were extended to 30 degC below 
       !~ and 70 degC above air temperature
      !~ Please note: The given polynomial approximation limits the application
      !~ of this procedure to values of wind speed between 0.5 and 17 m/s!
    
    !~ Version a 0.002, October 2009:
    !~ Changed ReadMe text and program messages for public release

    !~ Copyright (C) 2009  Peter Broede
   
    !~ This program is distributed in the hope that it will be useful,
    !~ but WITHOUT ANY WARRANTY; without even the implied warranty of
    !~ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

!~ Disclaimer of Warranty.

!~ THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING
!~ THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM �AS IS� WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED
!~ OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
!~ THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU
!~ ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.


!~ Limitation of Liability.

!~ IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO
!~ MODIFIES AND/OR CONVEYS THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, 
!~ INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM 
!~ (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES 
!~ OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED 
!~ OF THE POSSIBILITY OF SUCH DAMAGES.
!~ **********************************************
      program UTCI_Temperature
!~ **********************************************
     !~ --- Type input variables 
      double precision Ta,ehPa,Tmrt,va10m,RH

     !~ --- Type convention for output variables 
      double precision ET,UTCI_approx
      REAL tt,es
      integer choice,correct
     !~ DATA Input
      choice=1
    print *,' Program for calculating UTCI'
    print *,' based on calculations as presented at the MC/WG meeting'
    print *,' of COST Action 730 in Eilat, 2008-09-10'
    print *,' and decisions after the WG1 meeting in Stuttgart 2009-02-24'
    print *,' '
    print *,' alpha version released for public use after termination of COST Action 730'
    print *,' '
    print *,' Version a 0.002, October 2009'
    print *,' Copyright (C) 2009  Peter Broede'
    print *,' '
    print *,' This program is distributed in the hope that it will be useful,'
    print *,' but WITHOUT ANY WARRANTY; without even the implied warranty of'
    print *,' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  '
    print *,' '
     do while (choice .eq. 1)

10    print *,' '
	  print *,'  UTCI '
	  print *,' '
	  print *,' '
	  print *,'  Version a 0.002: P.Broede, October 22, 2009 '
      print *,' '
	  print *,' '
	  print *,'  Please enter the meteorological data:'
      print *,' '
      print '(1x,a)',' Air temperature in degree Celsius (degC)'
      DO 
        print '(1x,a,\)','                                (between -50 and +50 degC)          : '
        read *, Ta
	if  (Ta .ge. -50 .and. TA .le. 50) exit
	end do
      tt=Ta
      print '(1x,a)',' Mean radiant temperature in degree Celsius (degC): '
      DO 
        print '(1x,a,\)','   (allowed range: 30 degC below and 70 degC above air temperature) : '
        read *, Tmrt
	if  (Tmrt-Ta .ge. -30 .and. Tmrt-Ta .le. 70) exit
	end do
      print '(1x,a)',' Wind velocity (10 m above ground level) in m/s'
      DO 
        print '(1x,a,\)','          (between 0.5 and 17 m/s)       : '
        read *,va10m
	if  (va10m .ge. 0.5 .and. va10m .le. 30) exit
	end do
      do
        print '(1x,a)',' Water vapour pressure in hPa'
        print '(1x,a,\)','   (max. 50 hPa, <0>+<RETURN> for relative humidity input)          : '
        read *,ehPa
        if (ehPa .eq. 0)  then
          print '(1x,a,\)',' Relative humidity in Percent                                       : '
          read *,RH
          ehPa=es(tt)*RH/100.0D0
        else
          RH=ehPa*100.0D0/es(tt)
        end if
  	if  (ehPa .le. 50 .and. RH .le. 100 .and. ehPa .ge. 0 .and. RH .ge. 0) exit
	end do
     !~ Check INPUT
      print *,' '
      print *,' PLEASE CHECK THE INPUT VARIABLES :'
	  print *,' '
      print '(a,f10.2)','  The air temperature in degC is          : ',tt
      print '(a,f10.2)','  The mean radiant temperature in degC is : ',tmrt
      print '(a,f10.2)','  The wind velocity (10 m) in m/s is   : ',va10m
      print '(a,f10.2)','  The water vapour pressure in hPa is   : ',ehPa
      print '(a,f10.2)','  The relative humidity in percent is   : ',RH
      print *,' '
      print *,' '
      print '(1x,a,\)',' Are the values correct? (Yes=1, No=0) '
      read *,correct
      if (correct .eq. 0)  goto 10
!
!
      print *,' '
      print *,' '
      print *,'   Here is your result:		'
	  print *,' '
!
	  ET = UTCI_approx(Ta,ehPa,Tmrt,va10m)
!
      print '(a,f10.1)','  UTCI in degC is  : ',ET
!
      print*,' '
	  print*,' '
	  print*,' '
      print'(1x,a,\)',' Another UTCI calculation? (Yes=1, No=0) '
      read *, choice
     end do

!
      end

!
!~ **********************************************
      DOUBLE precision function UTCI_approx(Ta,ehPa,Tmrt,va)
!~ **********************************************
 !~ DOUBLE PRECISION Function value is the UTCI in degree Celsius
 !~ computed by a 6th order approximating polynomial from the 4 Input paramters 
 !~ 
 !~ Input parameters (all of type DOUBLE PRECISION)
 !~ - Ta       : air temperature, degree Celsius
 !~ - ehPa    : water vapour presure, hPa=hecto Pascal
 !~ - Tmrt   : mean radiant temperature, degree Celsius
 !~ - va10m  : wind speed 10 m above ground level in m/s
 !~ 
 !~  UTCI_approx, Version a 0.002, October 2009
 !~  Copyright (C) 2009  Peter Broede

      implicit none
        !~ type of input of the argument list
       DOUBLE PRECISION Ta,va,Tmrt,ehPa,Pa,D_Tmrt;
          D_TMRT=Tmrt-Ta
       	  PA = ehPa/10.0; !~ use vapour pressure in kPa
        !~ calculate 6th order polynomial as approximation     
      UTCI_approx=Ta+&
		( 6.07562052D-01 )   + &
		( -2.27712343D-02 ) * Ta + &
		( 8.06470249D-04 ) * Ta*Ta + &
		( -1.54271372D-04 ) * Ta*Ta*Ta + &
		( -3.24651735D-06 ) * Ta*Ta*Ta*Ta + &
		( 7.32602852D-08 ) * Ta*Ta*Ta*Ta*Ta + &
		( 1.35959073D-09 ) * Ta*Ta*Ta*Ta*Ta*Ta + &
		( -2.25836520D+00 ) * va + &
		( 8.80326035D-02 ) * Ta*va + &
		( 2.16844454D-03 ) * Ta*Ta*va + &
		( -1.53347087D-05 ) * Ta*Ta*Ta*va + &
		( -5.72983704D-07 ) * Ta*Ta*Ta*Ta*va + &
		( -2.55090145D-09 ) * Ta*Ta*Ta*Ta*Ta*va + &
		( -7.51269505D-01 ) * va*va + &
		( -4.08350271D-03 ) * Ta*va*va + &
		( -5.21670675D-05 ) * Ta*Ta*va*va + &
		( 1.94544667D-06 ) * Ta*Ta*Ta*va*va + &
		( 1.14099531D-08 ) * Ta*Ta*Ta*Ta*va*va + &
		( 1.58137256D-01 ) * va*va*va + &
		( -6.57263143D-05 ) * Ta*va*va*va + &
		( 2.22697524D-07 ) * Ta*Ta*va*va*va + &
		( -4.16117031D-08 ) * Ta*Ta*Ta*va*va*va + &
		( -1.27762753D-02 ) * va*va*va*va + &
		( 9.66891875D-06 ) * Ta*va*va*va*va + &
		( 2.52785852D-09 ) * Ta*Ta*va*va*va*va + &
		( 4.56306672D-04 ) * va*va*va*va*va + &
		( -1.74202546D-07 ) * Ta*va*va*va*va*va + &
		( -5.91491269D-06 ) * va*va*va*va*va*va + &
		( 3.98374029D-01 ) * D_Tmrt + &
		( 1.83945314D-04 ) * Ta*D_Tmrt + &
		( -1.73754510D-04 ) * Ta*Ta*D_Tmrt + &
		( -7.60781159D-07 ) * Ta*Ta*Ta*D_Tmrt + &
		( 3.77830287D-08 ) * Ta*Ta*Ta*Ta*D_Tmrt + &
		( 5.43079673D-10 ) * Ta*Ta*Ta*Ta*Ta*D_Tmrt + &
		( -2.00518269D-02 ) * va*D_Tmrt + &
		( 8.92859837D-04 ) * Ta*va*D_Tmrt + &
		( 3.45433048D-06 ) * Ta*Ta*va*D_Tmrt + &
		( -3.77925774D-07 ) * Ta*Ta*Ta*va*D_Tmrt + &
		( -1.69699377D-09 ) * Ta*Ta*Ta*Ta*va*D_Tmrt + &
		( 1.69992415D-04 ) * va*va*D_Tmrt + &
		( -4.99204314D-05 ) * Ta*va*va*D_Tmrt + &
		( 2.47417178D-07 ) * Ta*Ta*va*va*D_Tmrt + &
		( 1.07596466D-08 ) * Ta*Ta*Ta*va*va*D_Tmrt + &
		( 8.49242932D-05 ) * va*va*va*D_Tmrt + &
		( 1.35191328D-06 ) * Ta*va*va*va*D_Tmrt + &
		( -6.21531254D-09 ) * Ta*Ta*va*va*va*D_Tmrt + &
		( -4.99410301D-06 ) * va*va*va*va*D_Tmrt + &
		( -1.89489258D-08 ) * Ta*va*va*va*va*D_Tmrt + &
		( 8.15300114D-08 ) * va*va*va*va*va*D_Tmrt + &
		( 7.55043090D-04 ) * D_Tmrt*D_Tmrt + &
		( -5.65095215D-05 ) * Ta*D_Tmrt*D_Tmrt + &
		( -4.52166564D-07 ) * Ta*Ta*D_Tmrt*D_Tmrt + &
		( 2.46688878D-08 ) * Ta*Ta*Ta*D_Tmrt*D_Tmrt + &
		( 2.42674348D-10 ) * Ta*Ta*Ta*Ta*D_Tmrt*D_Tmrt + &
		( 1.54547250D-04 ) * va*D_Tmrt*D_Tmrt + &
		( 5.24110970D-06 ) * Ta*va*D_Tmrt*D_Tmrt + &
		( -8.75874982D-08 ) * Ta*Ta*va*D_Tmrt*D_Tmrt + &
		( -1.50743064D-09 ) * Ta*Ta*Ta*va*D_Tmrt*D_Tmrt + &
		( -1.56236307D-05 ) * va*va*D_Tmrt*D_Tmrt + &
		( -1.33895614D-07 ) * Ta*va*va*D_Tmrt*D_Tmrt + &
		( 2.49709824D-09 ) * Ta*Ta*va*va*D_Tmrt*D_Tmrt + &
		( 6.51711721D-07 ) * va*va*va*D_Tmrt*D_Tmrt + &
		( 1.94960053D-09 ) * Ta*va*va*va*D_Tmrt*D_Tmrt + &
		( -1.00361113D-08 ) * va*va*va*va*D_Tmrt*D_Tmrt + &
		( -1.21206673D-05 ) * D_Tmrt*D_Tmrt*D_Tmrt + &
		( -2.18203660D-07 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt + &
		( 7.51269482D-09 ) * Ta*Ta*D_Tmrt*D_Tmrt*D_Tmrt + &
		( 9.79063848D-11 ) * Ta*Ta*Ta*D_Tmrt*D_Tmrt*D_Tmrt + &
		( 1.25006734D-06 ) * va*D_Tmrt*D_Tmrt*D_Tmrt + &
		( -1.81584736D-09 ) * Ta*va*D_Tmrt*D_Tmrt*D_Tmrt + &
		( -3.52197671D-10 ) * Ta*Ta*va*D_Tmrt*D_Tmrt*D_Tmrt + &
		( -3.36514630D-08 ) * va*va*D_Tmrt*D_Tmrt*D_Tmrt + &
		( 1.35908359D-10 ) * Ta*va*va*D_Tmrt*D_Tmrt*D_Tmrt + &
		( 4.17032620D-10 ) * va*va*va*D_Tmrt*D_Tmrt*D_Tmrt + &
		( -1.30369025D-09 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + &
		( 4.13908461D-10 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + &
		( 9.22652254D-12 ) * Ta*Ta*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + &
		( -5.08220384D-09 ) * va*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + &
		( -2.24730961D-11 ) * Ta*va*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + &
		( 1.17139133D-10 ) * va*va*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + &
		( 6.62154879D-10 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + &
		( 4.03863260D-13 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + &
		( 1.95087203D-12 ) * va*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + &
		( -4.73602469D-12 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt + &
		( 5.12733497D+00 ) * Pa + &
		( -3.12788561D-01 ) * Ta*Pa + &
		( -1.96701861D-02 ) * Ta*Ta*Pa + &
		( 9.99690870D-04 ) * Ta*Ta*Ta*Pa + &
		( 9.51738512D-06 ) * Ta*Ta*Ta*Ta*Pa + &
		( -4.66426341D-07 ) * Ta*Ta*Ta*Ta*Ta*Pa + &
		( 5.48050612D-01 ) * va*Pa + &
		( -3.30552823D-03 ) * Ta*va*Pa + &
		( -1.64119440D-03 ) * Ta*Ta*va*Pa + &
		( -5.16670694D-06 ) * Ta*Ta*Ta*va*Pa + &
		( 9.52692432D-07 ) * Ta*Ta*Ta*Ta*va*Pa + &
		( -4.29223622D-02 ) * va*va*Pa + &
		( 5.00845667D-03 ) * Ta*va*va*Pa + &
		( 1.00601257D-06 ) * Ta*Ta*va*va*Pa + &
		( -1.81748644D-06 ) * Ta*Ta*Ta*va*va*Pa + &
		( -1.25813502D-03 ) * va*va*va*Pa + &
		( -1.79330391D-04 ) * Ta*va*va*va*Pa + &
		( 2.34994441D-06 ) * Ta*Ta*va*va*va*Pa + &
		( 1.29735808D-04 ) * va*va*va*va*Pa + &
		( 1.29064870D-06 ) * Ta*va*va*va*va*Pa + &
		( -2.28558686D-06 ) * va*va*va*va*va*Pa + &
		( -3.69476348D-02 ) * D_Tmrt*Pa + &
		( 1.62325322D-03 ) * Ta*D_Tmrt*Pa + &
		( -3.14279680D-05 ) * Ta*Ta*D_Tmrt*Pa + &
		( 2.59835559D-06 ) * Ta*Ta*Ta*D_Tmrt*Pa + &
		( -4.77136523D-08 ) * Ta*Ta*Ta*Ta*D_Tmrt*Pa + &
		( 8.64203390D-03 ) * va*D_Tmrt*Pa + &
		( -6.87405181D-04 ) * Ta*va*D_Tmrt*Pa + &
		( -9.13863872D-06 ) * Ta*Ta*va*D_Tmrt*Pa + &
		( 5.15916806D-07 ) * Ta*Ta*Ta*va*D_Tmrt*Pa + &
		( -3.59217476D-05 ) * va*va*D_Tmrt*Pa + &
		( 3.28696511D-05 ) * Ta*va*va*D_Tmrt*Pa + &
		( -7.10542454D-07 ) * Ta*Ta*va*va*D_Tmrt*Pa + &
		( -1.24382300D-05 ) * va*va*va*D_Tmrt*Pa + &
		( -7.38584400D-09 ) * Ta*va*va*va*D_Tmrt*Pa + &
		( 2.20609296D-07 ) * va*va*va*va*D_Tmrt*Pa + &
		( -7.32469180D-04 ) * D_Tmrt*D_Tmrt*Pa + &
		( -1.87381964D-05 ) * Ta*D_Tmrt*D_Tmrt*Pa + &
		( 4.80925239D-06 ) * Ta*Ta*D_Tmrt*D_Tmrt*Pa + &
		( -8.75492040D-08 ) * Ta*Ta*Ta*D_Tmrt*D_Tmrt*Pa + &
		( 2.77862930D-05 ) * va*D_Tmrt*D_Tmrt*Pa + &
		( -5.06004592D-06 ) * Ta*va*D_Tmrt*D_Tmrt*Pa + &
		( 1.14325367D-07 ) * Ta*Ta*va*D_Tmrt*D_Tmrt*Pa + &
		( 2.53016723D-06 ) * va*va*D_Tmrt*D_Tmrt*Pa + &
		( -1.72857035D-08 ) * Ta*va*va*D_Tmrt*D_Tmrt*Pa + &
		( -3.95079398D-08 ) * va*va*va*D_Tmrt*D_Tmrt*Pa + &
		( -3.59413173D-07 ) * D_Tmrt*D_Tmrt*D_Tmrt*Pa + &
		( 7.04388046D-07 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt*Pa + &
		( -1.89309167D-08 ) * Ta*Ta*D_Tmrt*D_Tmrt*D_Tmrt*Pa + &
		( -4.79768731D-07 ) * va*D_Tmrt*D_Tmrt*D_Tmrt*Pa + &
		( 7.96079978D-09 ) * Ta*va*D_Tmrt*D_Tmrt*D_Tmrt*Pa + &
		( 1.62897058D-09 ) * va*va*D_Tmrt*D_Tmrt*D_Tmrt*Pa + &
		( 3.94367674D-08 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*Pa + &
		( -1.18566247D-09 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*Pa + &
		( 3.34678041D-10 ) * va*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*Pa + &
		( -1.15606447D-10 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*Pa + &
		( -2.80626406D+00 ) * Pa*Pa + &
		( 5.48712484D-01 ) * Ta*Pa*Pa + &
		( -3.99428410D-03 ) * Ta*Ta*Pa*Pa + &
		( -9.54009191D-04 ) * Ta*Ta*Ta*Pa*Pa + &
		( 1.93090978D-05 ) * Ta*Ta*Ta*Ta*Pa*Pa + &
		( -3.08806365D-01 ) * va*Pa*Pa + &
		( 1.16952364D-02 ) * Ta*va*Pa*Pa + &
		( 4.95271903D-04 ) * Ta*Ta*va*Pa*Pa + &
		( -1.90710882D-05 ) * Ta*Ta*Ta*va*Pa*Pa + &
		( 2.10787756D-03 ) * va*va*Pa*Pa + &
		( -6.98445738D-04 ) * Ta*va*va*Pa*Pa + &
		( 2.30109073D-05 ) * Ta*Ta*va*va*Pa*Pa + &
		( 4.17856590D-04 ) * va*va*va*Pa*Pa + &
		( -1.27043871D-05 ) * Ta*va*va*va*Pa*Pa + &
		( -3.04620472D-06 ) * va*va*va*va*Pa*Pa + &
		( 5.14507424D-02 ) * D_Tmrt*Pa*Pa + &
		( -4.32510997D-03 ) * Ta*D_Tmrt*Pa*Pa + &
		( 8.99281156D-05 ) * Ta*Ta*D_Tmrt*Pa*Pa + &
		( -7.14663943D-07 ) * Ta*Ta*Ta*D_Tmrt*Pa*Pa + &
		( -2.66016305D-04 ) * va*D_Tmrt*Pa*Pa + &
		( 2.63789586D-04 ) * Ta*va*D_Tmrt*Pa*Pa + &
		( -7.01199003D-06 ) * Ta*Ta*va*D_Tmrt*Pa*Pa + &
		( -1.06823306D-04 ) * va*va*D_Tmrt*Pa*Pa + &
		( 3.61341136D-06 ) * Ta*va*va*D_Tmrt*Pa*Pa + &
		( 2.29748967D-07 ) * va*va*va*D_Tmrt*Pa*Pa + &
		( 3.04788893D-04 ) * D_Tmrt*D_Tmrt*Pa*Pa + &
		( -6.42070836D-05 ) * Ta*D_Tmrt*D_Tmrt*Pa*Pa + &
		( 1.16257971D-06 ) * Ta*Ta*D_Tmrt*D_Tmrt*Pa*Pa + &
		( 7.68023384D-06 ) * va*D_Tmrt*D_Tmrt*Pa*Pa + &
		( -5.47446896D-07 ) * Ta*va*D_Tmrt*D_Tmrt*Pa*Pa + &
		( -3.59937910D-08 ) * va*va*D_Tmrt*D_Tmrt*Pa*Pa + &
		( -4.36497725D-06 ) * D_Tmrt*D_Tmrt*D_Tmrt*Pa*Pa + &
		( 1.68737969D-07 ) * Ta*D_Tmrt*D_Tmrt*D_Tmrt*Pa*Pa + &
		( 2.67489271D-08 ) * va*D_Tmrt*D_Tmrt*D_Tmrt*Pa*Pa + &
		( 3.23926897D-09 ) * D_Tmrt*D_Tmrt*D_Tmrt*D_Tmrt*Pa*Pa + &
		( -3.53874123D-02 ) * Pa*Pa*Pa + &
		( -2.21201190D-01 ) * Ta*Pa*Pa*Pa + &
		( 1.55126038D-02 ) * Ta*Ta*Pa*Pa*Pa + &
		( -2.63917279D-04 ) * Ta*Ta*Ta*Pa*Pa*Pa + &
		( 4.53433455D-02 ) * va*Pa*Pa*Pa + &
		( -4.32943862D-03 ) * Ta*va*Pa*Pa*Pa + &
		( 1.45389826D-04 ) * Ta*Ta*va*Pa*Pa*Pa + &
		( 2.17508610D-04 ) * va*va*Pa*Pa*Pa + &
		( -6.66724702D-05 ) * Ta*va*va*Pa*Pa*Pa + &
		( 3.33217140D-05 ) * va*va*va*Pa*Pa*Pa + &
		( -2.26921615D-03 ) * D_Tmrt*Pa*Pa*Pa + &
		( 3.80261982D-04 ) * Ta*D_Tmrt*Pa*Pa*Pa + &
		( -5.45314314D-09 ) * Ta*Ta*D_Tmrt*Pa*Pa*Pa + &
		( -7.96355448D-04 ) * va*D_Tmrt*Pa*Pa*Pa + &
		( 2.53458034D-05 ) * Ta*va*D_Tmrt*Pa*Pa*Pa + &
		( -6.31223658D-06 ) * va*va*D_Tmrt*Pa*Pa*Pa + &
		( 3.02122035D-04 ) * D_Tmrt*D_Tmrt*Pa*Pa*Pa + &
		( -4.77403547D-06 ) * Ta*D_Tmrt*D_Tmrt*Pa*Pa*Pa + &
		( 1.73825715D-06 ) * va*D_Tmrt*D_Tmrt*Pa*Pa*Pa + &
		( -4.09087898D-07 ) * D_Tmrt*D_Tmrt*D_Tmrt*Pa*Pa*Pa + &
		( 6.14155345D-01 ) * Pa*Pa*Pa*Pa + &
		( -6.16755931D-02 ) * Ta*Pa*Pa*Pa*Pa + &
		( 1.33374846D-03 ) * Ta*Ta*Pa*Pa*Pa*Pa + &
		( 3.55375387D-03 ) * va*Pa*Pa*Pa*Pa + &
		( -5.13027851D-04 ) * Ta*va*Pa*Pa*Pa*Pa + &
		( 1.02449757D-04 ) * va*va*Pa*Pa*Pa*Pa + &
		( -1.48526421D-03 ) * D_Tmrt*Pa*Pa*Pa*Pa + &
		( -4.11469183D-05 ) * Ta*D_Tmrt*Pa*Pa*Pa*Pa + &
		( -6.80434415D-06 ) * va*D_Tmrt*Pa*Pa*Pa*Pa + &
		( -9.77675906D-06 ) * D_Tmrt*D_Tmrt*Pa*Pa*Pa*Pa + &
		( 8.82773108D-02 ) * Pa*Pa*Pa*Pa*Pa + &
		( -3.01859306D-03 ) * Ta*Pa*Pa*Pa*Pa*Pa + &
		( 1.04452989D-03 ) * va*Pa*Pa*Pa*Pa*Pa + &
		( 2.47090539D-04 ) * D_Tmrt*Pa*Pa*Pa*Pa*Pa + &
		( 1.48348065D-03 ) * Pa*Pa*Pa*Pa*Pa*Pa 
      return
      END


!~ **********************************************
      real FUNCTION es(ta)
!~ **********************************************
!~ calculates saturation vapour pressure over water in hPa for input air temperature (ta) in celsius according to:
!~ Hardy, R.; ITS-90 Formulations for Vapor Pressure, Frostpoint Temperature, Dewpoint Temperature and Enhancement Factors in the Range -100 to 100 �C; 
!~ Proceedings of Third International Symposium on Humidity and Moisture; edited by National Physical Laboratory (NPL), London, 1998, pp. 214-221
!~ http://www.thunderscientific.com/tech_info/reflibrary/its90formulas.pdf (retrieved 2008-10-01)
      
      implicit none
!
      real ta, tk
      INTEGER I
      REAL :: g(0:7)=(/&
				-2.8365744E3,&
				-6.028076559E3,&
				1.954263612E1,&
				-2.737830188E-2,&
				1.6261698E-5,&
				7.0229056E-10,&
				-1.8680009E-13,&
				2.7150305 /)
!      
      tk=ta+273.15 		! air temp in K
      es=g(7)*log(tk)
      do i=0,6
        es=es+g(i)*tk**(i-2)  
      end do
      es=exp(es)*0.01	! *0.01: convert Pa to hPa
!
      return
      END
