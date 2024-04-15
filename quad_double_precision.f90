module quad_double_precision
    implicit none
    integer, parameter :: quad_real_kind = selected_real_kind(18, 307)

    type quad_double
        real(quad_real_kind) :: value
    end type quad_double
end module quad_double_precision