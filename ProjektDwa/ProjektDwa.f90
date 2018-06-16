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
    use MULT
    implicit none
    ! Variables
    integer (kind = 4) :: mode, status, i, j, k, l
    real (kind = 8)  :: time, start, finish
    real (kind = 8), allocatable :: first(:,:), second(:, :), multiply(:, :)
    
     Body of ProjektDwa
    do mode = 1, 5 !TESTY:
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
    
    i = 1
    do j = 1, 11
        allocate(first(i, i))
        allocate(second(i, i))
        allocate(multiply(i, i))
        do k = 1, i
            do l = 1, i
                first(k, l) = k * l * 0.12
                second(k, l) = k * l * 0.35
            end do
        end do
        do mode = 1, 5
            call cpu_time(start)
            call multiplication(first, second, multiply, status, mode)
            call cpu_time(finish)
            time = finish - start
            print *, mode, i, time
        end do
        deallocate(first)
        deallocate(second)
        deallocate(multiply)
        i = i*2
    end do
    
    read (*, *) i
    

    end program ProjektDwa

