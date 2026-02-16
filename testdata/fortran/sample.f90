module math_utils
    use iso_fortran_env
    implicit none

contains

    subroutine greet(name)
        character(*), intent(in) :: name
        print *, 'Hello, ', name
    end subroutine greet

    pure integer function factorial(n) result(res)
        integer, intent(in) :: n
        integer :: i
        res = 1
        do i = 2, n
            res = res * i
        end do
    end function factorial

    real function average(arr, n)
        integer, intent(in) :: n
        real, intent(in) :: arr(n)
        average = sum(arr) / real(n)
    end function average

end module math_utils
