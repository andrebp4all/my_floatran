! included quad_double_precision
module quad_double_precision
    implicit none
    integer, parameter :: quad_real_kind = selected_real_kind(18, 307)

    type quad_double
        real(quad_real_kind) :: real_part
    end type quad_double
end module quad_double_precision

module quad_double_precision_operation
    use quad_double_precision
    use iso_c_binding, only: c_double ! compare performance
    implicit none


    ! Interface block for the GSL exponential integral Ei function
        
        interface
            ! Function for the exponential integral Ei
            real(c_double) function gsl_sf_expint_Ei(x) bind(C, name="gsl_sf_expint_Ei")
                import :: c_double
                real(c_double), intent(in) :: x
            end function gsl_sf_expint_Ei
    
            ! Function for the cosine integral Ci
            real(c_double) function gsl_sf_Ci(x) bind(C, name="gsl_sf_Ci")
                import :: c_double
                real(c_double), intent(in) :: x
            end function gsl_sf_Ci
        end interface


    contains
    ! Mathematical Functions

    subroutine quad_sinh(x, result)
        type(quad_double), intent(in) :: x
        type(quad_double), intent(out) :: result
        result%real_part = sinh(x%real_part)
    end subroutine quad_sinh
    
    subroutine quad_cosh(x, result)
        type(quad_double), intent(in) :: x
        type(quad_double), intent(out) :: result
        result%real_part = cosh(x%real_part)
    end subroutine quad_cosh

    subroutine quad_tanh(a, result)
        ! Hyperbolic tangent
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = tanh(a%real_part)
    end subroutine quad_tanh

    subroutine quad_asin(a, result)
        ! Arcsine
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = asin(a%real_part)
    end subroutine quad_asin

    subroutine quad_acos(a, result)
        ! Arccosine
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = acos(a%real_part)
    end subroutine quad_acos

    subroutine quad_atan(a, result)
        ! Arctangent
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = atan(a%real_part)
    end subroutine quad_atan

    subroutine quad_bessel_j0(a, result)
        ! Bessel function of the first kind (order 0)
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = bessel_j0(a%real_part)
    end subroutine quad_bessel_j0
    
    subroutine quad_gamma(a, result)
        ! Gamma function
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = gamma(a%real_part)
    end subroutine quad_gamma

    subroutine quad_erf(a, result)
        ! Error function (Erf)
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = erf(a%real_part)
    end subroutine quad_erf

    subroutine quad_erfc(a, result)
        ! Complementary error function (Erfc)
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = erfc(a%real_part)
    end subroutine quad_erfc
    
    subroutine quad_hypot(a, b, result)
        ! Hypotenuse function
        type(quad_double), intent(in) :: a, b
        type(quad_double), intent(out) :: result
        result%real_part = hypot(a%real_part, b%real_part)
    end subroutine quad_hypot

    subroutine quad_sqrt(a, result)
        ! Square root
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = sqrt(a%real_part)
    end subroutine quad_sqrt

    subroutine quad_log(a, result)
        ! Natural logarithm
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = log(a%real_part)
    end subroutine quad_log

    subroutine quad_log10(a, result)
        ! Base-10 logarithm
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = log10(a%real_part)
    end subroutine quad_log10

    subroutine quad_exp(a, result)
        ! Exponential function (e^x)
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = exp(a%real_part)
    end subroutine quad_exp

    subroutine quad_pow(a, b, result)
        ! Power function (a^b)
        type(quad_double), intent(in) :: a, b
        type(quad_double), intent(out) :: result
        result%real_part = a%real_part ** b%real_part
    end subroutine quad_pow

    subroutine quad_sigmoid(a, result)
        ! Sigmoid function (1 / (1 + e^(-x)))
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = 1.0_quad_real_kind / (1.0_quad_real_kind + exp(-a%real_part))
    end subroutine quad_sigmoid

    subroutine quad_relu(a, result)
        ! Rectified Linear Unit (ReLU) activation function
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = max(0.0_quad_real_kind, a%real_part)
    end subroutine quad_relu

    subroutine quad_sigmoid_derivative(a, result)
        ! Derivative of the sigmoid function (sigmoid(x) * (1 - sigmoid(x)))
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = a%real_part * (1.0_quad_real_kind - a%real_part)
    end subroutine quad_sigmoid_derivative

    subroutine quad_relu_derivative(a, result)
        ! Derivative of the ReLU activation function (1 for x > 0, 0 otherwise)
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        result%real_part = merge(1.0_quad_real_kind, 0.0_quad_real_kind, a%real_part > 0.0_quad_real_kind)
    end subroutine quad_relu_derivative
    
    subroutine quad_expint_ei(a, result)
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result

        ! Temporary variable to hold the converted value
        real(c_double) :: temp_input

        ! Convert from REAL(10) to REAL(8)
        temp_input = real(a%real_part, c_double)

        ! Call the GSL function with the correctly typed argument
        result%real_part = gsl_sf_expint_Ei(temp_input)
    end subroutine quad_expint_ei

    subroutine quad_expint_ci(a, result)
        type(quad_double), intent(in) :: a
        type(quad_double), intent(out) :: result
        real(c_double) :: temp_input

        temp_input = real(a%real_part, c_double)  ! Convert to GSL compatible type
        result%real_part = gsl_sf_Ci(temp_input)  ! Call GSL function for Ci and store result
    end subroutine quad_expint_ci

    
        
end module quad_double_precision_operation