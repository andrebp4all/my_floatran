module quad_double_precision
    implicit none
    integer, parameter :: quad_real_kind = selected_real_kind(18, 307)

    type quad_double
        real(quad_real_kind) :: value
    end type quad_double
end module quad_double_precision

module quad_double_precision_operation
    use quad_double_precision, only : quad_double
    implicit none

    contains
    subroutine quad_sinh(x, result)
        type(quad_double), intent(in) :: x
        type(quad_double), intent(out) :: result
        result%value = sinh(x%value)
    end subroutine quad_sinh

    subroutine quad_cosh(x, result)
        type(quad_double), intent(in) :: x
        type(quad_double), intent(out) :: result
        result%value = cosh(x%value)
    end subroutine quad_cosh

    subroutine quad_tanh(x, result)
        type(quad_double), intent(in) :: x
        type(quad_double), intent(out) :: result
        result%value = tanh(x%value)
    end subroutine quad_tanh
end module quad_double_precision_operation

program test_quad_double_functions
    use quad_double_precision, only : quad_double
    use quad_double_precision_operation, only : quad_sinh, quad_cosh, quad_tanh
    implicit none

    type(quad_double) :: x, result
    real :: start, finish, elapsed

    x%value = 0.5_quad_real_kind

    call test_function('sinh', x, quad_sinh)
    call test_function('cosh', x, quad_cosh)
    call test_function('tanh', x, quad_tanh)

contains
    subroutine test_function(name, x, func)
        character(len=*), intent(in) :: name
        type(quad_double), intent(in) :: x
        interface
            subroutine func(x, result)
                import :: quad_double
                type(quad_double), intent(in) :: x
                type(quad_double), intent(out) :: result
            end subroutine func
        end interface
        type(quad_double) :: result

        call cpu_time(start)
        call func(x, result)
        call cpu_time(finish)

        elapsed = finish - start
        print *, name // '(', x%value, ') = ', result%value
        print *, 'Computation time: ', elapsed, ' seconds'
    end subroutine test_function
end program test_quad_double_functions
