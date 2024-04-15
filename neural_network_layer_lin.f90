program test_neural_network_layer
    use quad_double_precision_operation
    implicit none

    ! Parameter declarations
    integer, parameter :: num_inputs = 5
    integer, parameter :: mat_size = 2
    integer :: i, n, j
    logical :: inversion_success
    real :: start, finish
    real(quad_real_kind) :: performance_time

    ! Type declarations
    type(quad_double) :: inputs(num_inputs), outputs_tanh(num_inputs)
    type(quad_double) :: outputs_sinh(num_inputs), outputs_cosh(num_inputs)
    type(quad_double) :: outputs_bessel_j0(num_inputs), outputs_expint_ei(num_inputs)
    type(quad_double) :: weights(num_inputs), biases
    type(quad_double), allocatable :: test_matrix(:,:), inverse_matrix(:,:)
    type(quad_double), allocatable :: matrix_a(:,:), matrix_b(:,:), matrix_result(:,:)

    ! Initialize weights and biases for demonstration purposes
    biases%real_part = 0.1_quad_real_kind
    do i = 1, num_inputs
        weights(i)%real_part = 0.2_quad_real_kind
        inputs(i)%real_part = i * 0.1_quad_real_kind
    end do

    ! Matrix multiplication test setup
    allocate(matrix_a(mat_size, mat_size), matrix_b(mat_size, mat_size), matrix_result(mat_size, mat_size))
    matrix_a(1, 1)%real_part = 1.0_quad_real_kind
    matrix_a(1, 2)%real_part = 2.0_quad_real_kind
    matrix_a(2, 1)%real_part = 3.0_quad_real_kind
    matrix_a(2, 2)%real_part = 4.0_quad_real_kind
    matrix_b(1, 1)%real_part = 2.0_quad_real_kind
    matrix_b(1, 2)%real_part = 0.0_quad_real_kind
    matrix_b(2, 1)%real_part = 1.0_quad_real_kind
    matrix_b(2, 2)%real_part = 2.0_quad_real_kind
    call quad_matrix_multiply(matrix_a, matrix_b, matrix_result)

    ! Matrix inversion test setup
    n = 2
    allocate(test_matrix(n, n), inverse_matrix(n, n))
    test_matrix(1, 1)%real_part = 4.0_quad_real_kind
    test_matrix(1, 2)%real_part = 0.0_quad_real_kind
    test_matrix(2, 1)%real_part = 0.0_quad_real_kind
    test_matrix(2, 2)%real_part = 3.0_quad_real_kind
    call quad_matrix_inverse(test_matrix, inverse_matrix, n, inversion_success)

    ! Compute outputs using various activation functions
    call cpu_time(start)
    do i = 1, num_inputs
        inputs(i)%real_part = inputs(i)%real_part * weights(i)%real_part + biases%real_part
        call quad_tanh(inputs(i), outputs_tanh(i))
        call quad_sinh(inputs(i), outputs_sinh(i))
        call quad_cosh(inputs(i), outputs_cosh(i))
        call quad_bessel_j0(inputs(i), outputs_bessel_j0(i))
        call quad_expint_ei(inputs(i), outputs_expint_ei(i))
    end do
    call cpu_time(finish)
    performance_time = finish - start

    ! Output results
    print *, 'Inputs and Outputs using various functions:'
    do i = 1, num_inputs
        print *, 'Input:', i, inputs(i)%real_part, 'Tanh:', outputs_tanh(i)%real_part, &
                 'Sinh:', outputs_sinh(i)%real_part, 'Cosh:', outputs_cosh(i)%real_part, &
                 'Bessel J0:', outputs_bessel_j0(i)%real_part, 'ExpInt Ei:', outputs_expint_ei(i)%real_part
    end do
    print *, 'Total computation time:', performance_time, ' seconds'

    print *, 'Matrix A:'
    do i = 1, mat_size
        print '(2F8.3)', (matrix_a(i, j)%real_part, j = 1, mat_size)
    end do

    print *, 'Matrix B:'
    do i = 1, mat_size
        print '(2F8.3)', (matrix_b(i, j)%real_part, j = 1, mat_size)
    end do

    print *, 'Result of matrix multiplication (A*B):'
    do i = 1, mat_size
        print '(2F8.3)', (matrix_result(i, j)%real_part, j = 1, mat_size)
    end do

    print *, 'Original matrix and its inverse:'
    do i = 1, n
        print '(2F8.3)', (test_matrix(i, j), j = 1, n)
    end do

    print *, 'Inverse matrix:'
    do i = 1, n
        print '(2F8.3)', (inverse_matrix(i, j), j = 1, n)
    end do

end program test_neural_network_layer
