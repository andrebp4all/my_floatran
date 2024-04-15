program test_matrix_inversion
    use iso_c_binding
    implicit none

    ! Assuming MPACK provides a Fortran interface or wrappers
    ! Include necessary module from MPACK (this is hypothetical)
    ! use mpack_module

    ! Variables
    integer, parameter :: n = 3
    integer :: info, i, j
    real(kind=8), dimension(n, n) :: A
    real(kind=8), dimension(n, n) :: invA
    integer, dimension(n) :: ipiv

    ! Initialize matrix A (example matrix)
    A = reshape([4.0d0, 2.0d0, 1.0d0, 2.0d0, 5.0d0, 1.0d0, 1.0d0, 1.0d0, 3.0d0], [n, n])

    ! Call MPACK matrix inversion routine (hypothetical routine name)
    call mpi_rgetrf(n, n, A, n, ipiv, info)
    if (info .eq. 0) then
        call mpi_rgetri(n, A, n, ipiv, invA, n, info)
    end if

    ! Check for success
    if (info .eq. 0) then
        print *, "Inverted matrix:"
        do i = 1, n
            print '(3F10.5)', invA(i, :)
        end do
    else
        print *, "Matrix inversion failed with error: ", info
    end if

end program test_matrix_inversion
