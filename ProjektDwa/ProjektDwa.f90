!  ProjektDwa.f90 
!
!  FUNCTIONS:
!  ProjektDwa - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: ProjektDwa
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program ProjektDwa
    use tests
    implicit none
    ! Variables
    integer (kind = 4) :: mode, status, i
    real (kind = 8)  :: time
    
    ! Body of ProjektDwa
    do mode = 1, 5
        call multiplySmallMatrixes(mode, status, time)
        print *, mode, status, time, "multiplySmallMatrixes"
        
        call rowMatrix(mode, status, time)
        print *, mode, status, time, 'rowMatrix'
        
        call columnMatrix(mode, status, time)
        print *, mode, status, time, 'columnMatrix'
        
        call hugeMatrix(mode, status, time)
        print *, mode, status, time, 'hugeMatrix'
        
        call badSizeMatrix(mode, status, time)
        print *, mode, status, time, 'badSizeMatrix'
        
        call squareMatrix(mode, status, time)
        print *, mode, status, time, 'squareMatrix'
    end do
    
    read (*, *) i
    

    end program ProjektDwa

