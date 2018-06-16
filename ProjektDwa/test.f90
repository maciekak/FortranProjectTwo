module tests
    use MULT        
    contains
        subroutine checkEqual(firstMatrix, secondMatrix, status)
            implicit none
            real (kind = 8), intent(in) :: firstMatrix(:, :), secondMatrix(:, :)
            real (kind = 8) :: epsilon
            integer (kind = 4), intent(out) :: status
            integer (kind = 4) :: i, j
            status = 0
            epsilon = 1.0d-4
            if (size(firstMatrix(1, :)) .NE. size(secondMatrix(1, :)) .OR. size(firstMatrix(:, 1)) &
                .NE. size(secondMatrix(:, 1))) then
                status = 1
            else
                do i = 1, size(firstMatrix(:, 1))
                    do j = 1, size(firstMatrix(1, :))
                        if(firstMatrix(i, j) + epsilon < secondMatrix(i, j) .OR. firstMatrix(i, j) &
                            - epsilon > secondMatrix(i, j)) then
                            status = 1
                        end if
                    end do
                end do
            end if
    end subroutine checkEqual
    
        subroutine multiplySmallMatrixes(mode, status, timeDifference)
            implicit none

            integer (kind = 4), intent(in) :: mode
            integer (kind = 4), intent(out) :: status
            real (kind = 8), intent(out) :: timeDifference
            real (kind = 8) start, finish
            real (kind = 8) :: first(1, 1), second(1, 1), resultMatrix(1,1), expected(1, 1)
            
            first = 1.5
            second = 2.8
            expected = reshape([1.5 * 2.8], shape(expected))
            
            call CPU_TIME(start)
            call multiplication(first, second, resultMatrix, status, mode)
            call CPU_TIME(finish)
            timeDifference = finish - start
            
            if (status .EQ. 0) then
                call checkEqual(resultMatrix, expected, status)
            end if
        end subroutine multiplySmallMatrixes
        
        subroutine hugeMatrix(mode, status, timeDifference)
            implicit none

            integer (kind = 4), intent(in) :: mode
            integer (kind = 4), intent(out) :: status
            integer (kind = 4) :: i, j
            real (kind = 8), intent(out) :: timeDifference
            real (kind = 8) start, finish
            real (kind = 8) :: first(1000, 1500), second(1500, 2000), resultMatrix(1000,2000), expected(1000, 2000)
            
            do i = 1, size(first( :, 1))
                do j = 1, size (first(1, :))
                    first(i, j) = i * j * 0.12
                end do
            end do
            do i = 1, size(second(:, 1))
                do j = 1, size(second(1, :))
                    second(i, j) = i * j * 0.35
                end do
            end do
            
            expected = matmul(first, second)
            
            call CPU_TIME(start)
            call multiplication(first, second, resultMatrix, status, mode)
            call CPU_TIME(finish)
            timeDifference = finish - start
            
            if (status .EQ. 0) then
                call checkEqual(resultMatrix, expected, status)
            end if
        end subroutine hugeMatrix
        
        subroutine rowMatrix(mode, status, timeDifference)
            implicit none

            integer (kind = 4), intent(in) :: mode
            integer (kind = 4), intent(out) :: status
            integer (kind = 4) :: i, j
            real (kind = 8), intent(out) :: timeDifference
            real (kind = 8) start, finish
            real (kind = 8) :: first(1, 4000), second(4000, 1), resultMatrix(1,1), expected(1, 1)
            
            do i = 1, size(first( :, 1))
                do j = 1, size (first(1, :))
                    first(i, j) = i * j * 0.12
                end do
            end do
            do i = 1, size(second(:, 1))
                do j = 1, size(second(1, :))
                    second(i, j) = i * j * 0.35
                end do
            end do
            
            
            expected = matmul(first, second)
            
            call CPU_TIME(start)
            call multiplication(first, second, resultMatrix, status, mode)
            call CPU_TIME(finish)
            timeDifference = finish - start
            
            if (status .EQ. 0) then
                call checkEqual(resultMatrix, expected, status)
            end if
        end subroutine rowMatrix
        
        subroutine columnMatrix(mode, status, timeDifference)
            implicit none

            integer (kind = 4), intent(in) :: mode
            integer (kind = 4), intent(out) :: status
            integer (kind = 4) :: i, j
            real (kind = 8), intent(out) :: timeDifference
            real (kind = 8) start, finish
            real (kind = 8) :: first(4000, 1), second(1, 4000), resultMatrix(4000, 4000), expected(4000, 4000)
            
            do i = 1, size(first( :, 1))
                do j = 1, size (first(1, :))
                    first(i, j) = i * j * 0.12
                end do
            end do
            do i = 1, size(second(:, 1))
                do j = 1, size(second(1, :))
                    second(i, j) = i * j * 0.35
                end do
            end do
            
            expected = matmul(first, second)
            
            call CPU_TIME(start)
            call multiplication(first, second, resultMatrix, status, mode)
            call CPU_TIME(finish)
            timeDifference = finish - start
            
            if (status .EQ. 0) then
                call checkEqual(resultMatrix, expected, status)
            end if
        end subroutine columnMatrix
        
        subroutine badSizeMatrix(mode, status, timeDifference)
            implicit none

            integer (kind = 4), intent(in) :: mode
            integer (kind = 4), intent(out) :: status
            integer (kind = 4) :: i, j
            real (kind = 8), intent(out) :: timeDifference
            real (kind = 8) start, finish
            real (kind = 8) :: first(100, 200), second(300, 400), resultMatrix(100, 400)
            
            do i = 1, size(first( :, 1))
                do j = 1, size (first(1, :))
                    first(i, j) = i * j * 0.12
                end do
            end do
            do i = 1, size(second(:, 1))
                do j = 1, size(second(1, :))
                    second(i, j) = i * j * 0.35
                end do
            end do
        
            
            call CPU_TIME(start)
            call multiplication(first, second, resultMatrix, status, mode)
            call CPU_TIME(finish)
            timeDifference = finish - start
            
        end subroutine badSizeMatrix
        
        subroutine squareMatrix(mode, status, timeDifference)
            implicit none

            integer (kind = 4), intent(in) :: mode
            integer (kind = 4), intent(out) :: status
            integer (kind = 4) :: i, j
            real (kind = 8), intent(out) :: timeDifference
            real (kind = 8) start, finish
            real (kind = 8) :: first(400, 400), second(400, 400), resultMatrix(400, 400), expected(400, 400)
            
            do i = 1, size(first( :, 1))
                do j = 1, size (first(1, :))
                    first(i, j) = i * j * 0.12
                end do
            end do
            do i = 1, size(second(:, 1))
                do j = 1, size(second(1, :))
                    second(i, j) = i * j * 0.35
                end do
            end do
        
            expected = matmul(first, second)
            
            call CPU_TIME(start)
            call multiplication(first, second, resultMatrix, status, mode)
            call CPU_TIME(finish)
            timeDifference = finish - start
            
            if (status .EQ. 0) then
                call checkEqual(resultMatrix, expected, status)
            end if
        end subroutine squareMatrix
        
end module tests
        
                