program test_neural_network_layer
    use quad_double_precision_operation
    implicit none

    integer, parameter :: num_inputs = 5
    type(quad_double) :: inputs(num_inputs), outputs_tanh(num_inputs)
    type(quad_double) :: outputs_sinh(num_inputs), outputs_cosh(num_inputs)
    type(quad_double) :: outputs_bessel_j0(num_inputs), outputs_expint_ei(num_inputs)
    type(quad_double) :: weights(num_inputs), biases
    real(quad_real_kind) :: performance_time
    real :: start, finish
    integer :: i

    ! Initialize weights and biases for demonstration purposes
    biases%real_part = 0.1_quad_real_kind
    do i = 1, num_inputs
        weights(i)%real_part = 0.2_quad_real_kind  ! Simple uniform weights
        inputs(i)%real_part = i * 0.1_quad_real_kind  ! Incremental inputs
    end do

    ! Compute outputs using tanh, sinh, cosh, Bessel J0, and expint_ei as activation functions
    call cpu_time(start)
    do i = 1, num_inputs
        ! Weighted input plus bias
        inputs(i)%real_part = inputs(i)%real_part * weights(i)%real_part + biases%real_part
        ! Apply tanh activation
        call quad_tanh(inputs(i), outputs_tanh(i))
        ! Also compute sinh and cosh for comparison
        call quad_sinh(inputs(i), outputs_sinh(i))
        call quad_cosh(inputs(i), outputs_cosh(i))
        ! Apply Bessel function J0
        call quad_bessel_j0(inputs(i), outputs_bessel_j0(i))
        ! Apply exponential integral Ei
        call quad_expint_ei(inputs(i), outputs_expint_ei(i))
    end do
    call cpu_time(finish)
    performance_time = finish - start

    ! Output results
    print *, 'Inputs and Outputs using Tanh, Sinh, Cosh, Bessel J0, ExpInt Ei:'
    do i = 1, num_inputs
        print *, 'Input:', i, inputs(i)%real_part, 'Tanh Output:', outputs_tanh(i)%real_part, &
                 'Sinh Output:', outputs_sinh(i)%real_part, 'Cosh Output:', outputs_cosh(i)%real_part, &
                 'Bessel J0 Output:', outputs_bessel_j0(i)%real_part, 'ExpInt Ei Output:', outputs_expint_ei(i)%real_part
    end do
    print *, 'Total computation time: ', performance_time, ' seconds'

end program test_neural_network_layer
