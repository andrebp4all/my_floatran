module quad_double_precision
    implicit none
    integer, parameter :: quad_real_kind = selected_real_kind(18, 307)

    type quad_double
        real(quad_real_kind) :: value
    end type quad_double
end module quad_double_precision

module quad_double_precision_operation
    use quad_double_precision
    implicit none

    contains
    ! Mathematical Functions

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

    subroutine quad_tanh(a, result)
        ! Hyperbolic tangent
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%value = tanh(a%value)
    end subroutine quad_tanh


end module quad_double_precision_operation

program test_quad_double
    use quad_double_precision
    use quad_double_precision_operation
    implicit none

    type(quad_double) :: x, result_sinh, result_cosh, result_tanh
    real :: start_sinh, finish_sinh, elapsed_sinh
    real :: start_cosh, finish_cosh, elapsed_cosh
    real :: start_tanh, finish_tanh, elapsed_tanh

    ! Initialize x
    x%value = 1.0_quad_real_kind

    ! Timing the operation for sinh
    call cpu_time(start_sinh)
    call quad_sinh(x, result_sinh)
    call cpu_time(finish_sinh)
    elapsed_sinh = finish_sinh - start_sinh

    ! Timing the operation for cosh
    call cpu_time(start_cosh)
    call quad_cosh(x, result_cosh)
    call cpu_time(finish_cosh)
    elapsed_cosh = finish_cosh - start_cosh

    ! Timing the operation for tanh
    call cpu_time(start_tanh)
    call quad_tanh(x, result_tanh)
    call cpu_time(finish_tanh)
    elapsed_tanh = finish_tanh - start_tanh

    ! Output results
    print *, 'Hyperbolic sine of ', x%value, ' is ', result_sinh%value
    print *, 'Computation time for sinh: ', elapsed_sinh, ' seconds'
    print *, 'Hyperbolic cosine of ', x%value, ' is ', result_cosh%value
    print *, 'Computation time for cosh: ', elapsed_cosh, ' seconds'
end program test_quad_double
