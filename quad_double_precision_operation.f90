! included quad_double_precision
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