module MULT        
    contains
        subroutine multiplication(firstMatrix, secondMatrix, multiplyResult, status, mode)
            implicit none
            real (kind = 8), intent(in) :: firstMatrix(:, :), secondMatrix
            real (kind = 8), intent(out) :: multiplyResult(:, :)
            integer (kind = 4), intent(out) :: status
            integer (kind = 4), intent(in) :: mode
            integer (kind = 4) :: firstXSize, firstYSize, firstSize, secondXSize, secondYSize, secondSize, resultXSize, resultYSize
            integer (kind = 4) :: ichunk
            real (kind = 8) :: value
            
            ichunk = 1024 ! TODO: take good ichunk
            
            firstXSize = size(firstMatrix(1, :))
            firstYSize = size(firstMatrix(:, 1))
            firstSize = size(firstMatrix)
            
            secondXSize = size(secondMatrix(1, :))
            secondYSize = size(secondMatrix(:, 1))
            secondSize = size(secondMatrix)
            
            resultXSize = size(multiplyResult(1, :))
            resultYSize = size(multiplyResult(:, 1))
            
            if(firstXSize == secondYSize .AND. firstXSize * firstYSize == firstSize .AND. secondXSize * secondYSize == secondSize .AND. firstXSize == resultXSize .AND. secondYSize == resultYSize) then
                value = 0.d0
                
                if (mode == 1 .OR. mode == 2) then
                        do i = 1, resultYSize
                            do j = 1, resultXSize
                                if (mode == 1) then
                                    do k = 1, firstXSize
                                        value = value + firstMatrix(i, k) * secondMatric(k, j)
                                    end do
                                    
                                    multiplyResult(i, j) = value
                                    value = 0.d0
                                    
                                else if (mode == 2) then
                                    multiplyResult(i, j) = dot_product(firstMatrix(i, :), secondMatrix(:, j))
                                end if
                            end do
                        end do
                else if (mode == 3 .OR. mode == 4) then
                    do i = 1, resultYSize, ichunk
                        do j = 1, resultXSize, ichunk
                            do k = i, min(i + ichunk, resultYSize)
                                do l = j, min(j + ichunk, resultXSize)
                                    if (mode == 3) then
                                        do m = 1, firstXSize
                                            value = value + firstMatrix(k, m) * secondMatrix(m, l)
                                        end do
                                        multiplyResult(k, l) = value
                                        value = 0.d0
                                        
                                    else if(mode == 4) then
                                        multiplyResult(k, l) = dot_product(firstMatrix(k, :), secondMatrix(:, l))
                                    end if
                                end do
                            end do
                        end do
                    end do
                else if (mode == 5) then
                    multiplyResult = matmul(firstMatrix, secondMatrix)
                end if
                
                status = 0
            else
                status = 1
            end if
            
        end subroutine multiplication
end module MULT