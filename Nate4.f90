!--------------------------------------------------------------------------------------!
!Constants module defines double precision and pi so that both don't need to be defined!
!repeatedly in other modules and the main code.                                        !
!The other modules and main code both use this module.                                 !
!--------------------------------------------------------------------------------------!

module constants
    implicit none
    save
    integer , parameter :: dp = selected_real_kind(15 , 300)
    real(kind = dp)     :: pi = 4.0_dp*ATAN(1.0_dp)
contains
!--------------------------------------------------------------------------------------!
!The get_inputs subroutine gets the values of N, a, t, epsilon_a, and epsilon_b from   !
!the inputs.txt file.                                                                  !
!These values will be then passed directly to the analytical and numerical solution    !
!calculators for our 1D linear system for both 1 and 2 atoms per unit cell respectively!
!--------------------------------------------------------------------------------------!
    subroutine get_inputs(N_val, a_val, t_val, epsilon_array, atoms_per_unit_cell)
        implicit none
        !All the following values outputted are constants for our system
        !N specifies the amount of iterations used to plot the values
        !(k vs E) from -Ï€/a -> Ï€/a (the 1st Brillouin zone)
        integer, intent(out)                                  :: N_val, atoms_per_unit_cell
        !a_val is the lattice spacing (the a from before)
        !t_val is the hopping energy
        !epsilon_a_val and epsilon_b_val are the on-site energies for atoms a and b respectively
        real(kind=dp), intent(out)                            :: a_val, t_val
        real(kind=dp), dimension(:), allocatable, intent(out) :: epsilon_array
        real(kind=dp), dimension(5)                           :: epsilon_array_temp
        !The following logicals (boolean values) are there
        !to check their associated values existance
        logical                                               :: inputs_file_exists, N_val_exists, a_val_exists, t_val_exists
        logical                                               :: epsilon_a_val_exists, epsilon_b_val_exists
        logical                                               :: epsilon_c_val_exists, epsilon_d_val_exists, epsilon_e_val_exists
        integer                                               :: file_unit = 15, eq_pos, istat, i
        character(len=25)                                     :: line, val_name
        real(kind=dp)                                         :: N_val_real, epsilon_a_val, epsilon_b_val
        real(kind=dp)                                         :: epsilon_c_val, epsilon_d_val, epsilon_e_val

        !Sets the exists booleans to False which only sets
        !to true if the corresponding value have been found
        N_val_exists         = .False.
        a_val_exists         = .False.
        t_val_exists         = .False.
        epsilon_a_val_exists = .False.
        epsilon_b_val_exists = .False.
        epsilon_c_val_exists = .False.
        epsilon_d_val_exists = .False.
        epsilon_e_val_exists = .False.

        !Checks whether the inputs.txt file exists
        inquire(file="inputs.txt",exist=inputs_file_exists)
        if (inputs_file_exists) then
            open (unit = file_unit , file = "inputs.txt" , status = "old" , action = "read", iostat = istat)
            if (istat /= 0) STOP "error openning inputs.txt"

            !Reads the next line in the file and saves it to the line string
            read (unit = file_unit, fmt = "(a)", iostat = istat) line

            !Exits once it's reached the end of the file (or an error occurs)
            do while (istat == 0)
                !Finds the indexed position of the = in the line
                eq_pos = index(line, "=")

                !TRIM gets rid of the trailing whitespaces
                !ADJUSTL does the same but for leading whitespaces
                val_name = ADJUSTL(TRIM(line(1:eq_pos-1)))
                !val  = ADJUSTL(TRIM(line(eq_pos+1:)))

                !Checks if there's a = in a valid spot and if the lines not a comment
                if (eq_pos>=2 .and. val_name(1:1) /= "#" .and. val_name(1:1) /= "!") then
                    select case(TRIM(val_name))
                        case("N")
                            read(line(eq_pos+1:), fmt=*, iostat=istat) N_val_real
                            if (istat /= 0) STOP "error: value of N is invalid"
                            N_val                = nint(N_val_real)
                            N_val_exists         = .True.
                        case("a")
                            read(line(eq_pos+1:), fmt=*, iostat=istat) a_val
                            if (istat /= 0) STOP "error: value of a is invalid"
                            a_val_exists         = .True.
                        case("t")
                            read(line(eq_pos+1:), fmt=*, iostat=istat) t_val
                            if (istat /= 0) STOP "error: value of t is invalid"
                            t_val_exists         = .True.
                        case("epsilon_a")
                            read(line(eq_pos+1:), fmt=*, iostat=istat) epsilon_a_val
                            if (istat /= 0) STOP "error: value of epsilon_a is invalid"
                            epsilon_a_val_exists = .True.
                        case("epsilon_b")
                            read(line(eq_pos+1:), fmt=*, iostat=istat) epsilon_b_val
                            if (istat /= 0) STOP "error: value of epsilon_b is invalid"
                            epsilon_b_val_exists = .True.
                        case("epsilon_c")
                            read(line(eq_pos+1:), fmt=*, iostat=istat) epsilon_b_val
                            if (istat /= 0) STOP "error: value of epsilon_c is invalid"
                            epsilon_c_val_exists = .True.
                        case("epsilon_d")
                            read(line(eq_pos+1:), fmt=*, iostat=istat) epsilon_b_val
                            if (istat /= 0) STOP "error: value of epsilon_d is invalid"
                            epsilon_d_val_exists = .True.
                        case("epsilon_e")
                            read(line(eq_pos+1:), fmt=*, iostat=istat) epsilon_b_val
                            if (istat /= 0) STOP "error: value of epsilon_e is invalid"
                            epsilon_e_val_exists = .True.
                        case("atoms_per_unit_cell")
                            read(line(eq_pos+1:), fmt=*, iostat=istat) atoms_per_unit_cell
                            if (istat /= 0) STOP "error: value of atoms_per_unit_cell is invalid"
                        case default
                            print "(a,a,a)", "warning: ", TRIM(val_name), " isn't a valid input"
                    end select
                end if

                !Reads the next line in the file and saves it to the line string
                read (unit = file_unit, fmt = "(a)", iostat = istat) line
            end do

            !Closes the inputs.txt file
            close (unit = file_unit , iostat = istat)
            if (istat /= 0) STOP "error closing inputs.txt"

        else
            print 10, "inputs.txt doesn't exist"
        endif

        !defines the default values of the Nate inputs if not included
        if (.not. N_val_exists) then
            print 10, "warning: resorting to default N value"
            N_val = 1000
        end if

        if (.not. a_val_exists) then
            print 10, "warning: resorting to default a value"
            a_val = 1.0_dp
        end if

        if (.not. t_val_exists) then
            print 10, "warning: resorting to default t value"
            t_val = 1.0_dp
        end if

        if (.not. epsilon_a_val_exists) then
            print 10, "warning: resorting to default epsilon_a value"
            epsilon_a_val = 0.0_dp
        end if

        if (.not. epsilon_b_val_exists) then
            print 10, "warning: resorting to default epsilon_b value"
            epsilon_b_val = 0.0_dp
        end if

        if (.not. epsilon_c_val_exists) then
            print 10, "warning: resorting to default epsilon_c value"
            epsilon_c_val = 0.0_dp
        end if

        if (.not. epsilon_d_val_exists) then
            print 10, "warning: resorting to default epsilon_d value"
            epsilon_d_val = 0.0_dp
        end if

        if (.not. epsilon_e_val_exists) then
            print 10, "warning: resorting to default epsilon_e value"
            epsilon_e_val = 0.0_dp
        end if

        !Displays the values being outputted from the subroutine
        print 10, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
        print 11, "N = ", N_val, " iterations"
        print 12, "a = ", a_val, " Å"
        print 12, "t = ", t_val, " eV"
        print 12, "epsilon_a = ", epsilon_a_val, " eV"
        print 12, "epsilon_b = ", epsilon_b_val, " eV"
        print 12, "epsilon_c = ", epsilon_c_val, " eV"
        print 12, "epsilon_d = ", epsilon_d_val, " eV"
        print 12, "epsilon_e = ", epsilon_e_val, " eV"
        print 11, "atoms_per_unit_cell = ", atoms_per_unit_cell, ""
        print 10, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

        allocate(epsilon_array(atoms_per_unit_cell), stat=istat)
        if (istat/=0) stop "error allocating epsilon_array"

        epsilon_array_temp = (/epsilon_a_val, epsilon_b_val, epsilon_c_val, epsilon_d_val, epsilon_e_val/)

        do i = 1, atoms_per_unit_cell
            epsilon_array(i) = epsilon_array_temp(i)
        end do


        !Fortran string formatting codes
        10 format (a)
        11 format (a22, I4, a)
        12 format (a22, f4.1, a)
    end subroutine get_inputs
end module constants

module analytic_subroutines
    use constants
    !Constants defined in above module used
    implicit none
    save

    contains

!--------------------------------------------------------------------------------------!
!The analytic subroutine calculates the analyic solutions for each of the systems calc-!
!ulated in this experiment.                                                            !
!All constants are kept general (other than the Broullin zone limits) so that they can !
!be read from an input file.                                                           !
!case_in defines which of the analytic solutions are being calculated. This case state-!
!ment is also kept general, but is changed in the main body of code so that both analy-!
!tic solutions are calculated each time that the code is run.                          !
!--------------------------------------------------------------------------------------!

    subroutine analytic(a_in , epsilon_a_in , epsilon_b_in , t_in , steps_in , file_unit , case_in)
        implicit none
        !a_in , epsilon_a_in , epsilon_b_in , t_in are constants in calc.
        real(kind = dp) , intent(IN)     :: a_in , epsilon_a_in , epsilon_b_in , t_in
        !epsilon_b_in ignored if only a single atom cell is being simulated
        integer , intent(IN)             :: steps_in , file_unit
        !Steps_in defines how many steps are used in calculating no. points generated for analytic solutions.
        !file unit is defined in the main body of code for consistency, is passed between each subroutine
        character(len = 20) , intent(IN) :: case_in
        real(kind = dp)                  :: k_current , k_step , k_min , k_max , energy_val
        !k_current defines current k value being iterated
        !k_step defines the step size of the iteration in k
        !k_min/k_max are hard coded constants to define the limits of the analytic calculation
        !energy_val calculates the E(k) values to be written to .csv file and plotted
        integer                          :: k_big = 0 , istat = 0
        !integers are defined to prevent errors from unassigned variables

        k_min   = -pi/a_in
        k_max   = pi/a_in
        k_step  = (k_max - k_min)/real(steps_in , kind = dp)
        k_current = k_min
        !k values are defined before iteration begins

        select case(case_in)
        !case statement defines which analytic solution is being generated
            case('single')
                do k_big = 0 , steps_in , 1
                !integer iteration used to prevent float errors in the calculations of k_current
                    energy_val = epsilon_a_in - 2.0_dp*t_in*COS(k_current*a_in)
                    !calculates E(k_current) for single atom system
                    write (unit = file_unit , fmt = * , iostat = istat) k_current , ',' , energy_val
                    !writes to file in .csv format
                    if (istat /= 0) stop "error writing double analytic file"
                    k_current = k_current + k_step
                    !updates k_current to correct value using k_step
                end do
            case('double')
                do k_big = 0 , steps_in , 1
                    energy_val = (epsilon_a_in + epsilon_b_in+((epsilon_a_in - epsilon_b_in)**2.0_dp + &
                    &8.0_dp*t_in**2.0_dp*(1.0_dp+COS(k_current*a_in)))**0.5_dp)/2.0_dp
                    !+ve version of 2 atom analytic solution
                    write (unit = file_unit , fmt = * , iostat = istat) k_current , ',' , energy_val
                    if (istat /= 0) stop "error writing double analytic file"
                    k_current = k_current + k_step
                end do

                do k_big = 0 , steps_in , 1
                    energy_val = (epsilon_a_in + epsilon_b_in-((epsilon_a_in - epsilon_b_in)**2.0_dp + &
                    &8.0_dp*t_in**2.0_dp*(1.0_dp+COS(k_current*a_in)))**0.5_dp)/2.0_dp
                    !-ve version of 2 atom analytic solution
                    write (unit = file_unit , fmt = * , iostat = istat) k_current , ',' , energy_val
                    if (istat /= 0) stop "error writing double analytic file"
                    k_current = k_current - k_step
                end do
            case default
                STOP 'invalid analytic plot'
        end select
    end subroutine analytic

!--------------------------------------------------------------------------------------!
!plotting_subroutine takes the constants from the main code and passes them to the ana-!
!lytic subroutine.                                                                     !
!This subroutine also opens and closes the .csv files around the analytic subroutines  !
!writing to them.                                                                      !
!--------------------------------------------------------------------------------------!

    subroutine plotting_subroutine(a_in , epsilon_a_in, epsilon_b_in , t_in , steps_in , filename_in , case_in)
        implicit none
        real(kind = dp) , intent(IN)            :: a_in, t_in, epsilon_a_in, epsilon_b_in
        integer , intent(IN)                    :: steps_in
        character(len = *) , intent(IN)         :: filename_in , case_in
        logical                                 :: lexist
        !lexist logical used to check system for files
        integer                                 :: file_unit = 15 , istat = 0
        !file unit defined here for use throughout file uses

        inquire(file=filename_in, exist=lexist)
        if (lexist) then                                    !using lexist logical to determine how to treat file
            print*,("old data file overwritten")            !checking to ensure that no file is ballooning in size
            open (unit = file_unit , file = filename_in , status = "replace" , action = "write" , position = "append" , &
            &iostat = istat)
            if (istat /= 0) then
                call execute_command_line("mkdir outputs")
                !making a directory (if it doesn't exist) so that python plotting script can find .csv files
                open(unit=file_unit , file=filename_in , status = "replace",action = "write", iostat = istat)
            end if
        else
            open (unit = file_unit , file = filename_in , status = "new" , action = "write" , position = "append" , &
            &iostat = istat)
            if (istat /= 0) then
                call execute_command_line("mkdir outputs")
                open(unit = file_unit , file = filename_in , status = "replace" , action = "write" , iostat = istat)
            end if
        end if

        call analytic(a_in , epsilon_a_in , epsilon_b_in , t_in , steps_in , file_unit , case_in)
        !calling subroutine to calculate and write E(k) to .csv files opened and closed within plotting_subroutine

        close (unit = file_unit , iostat = istat)
        if (istat /= 0) stop "error closing test file"

    end subroutine plotting_subroutine

end module analytic_subroutines

module zheev_module
  use constants
  implicit none

  contains

  subroutine call_zheev(A, W)
    implicit none
      character                                           :: JOBZ, UPLO
      integer                                             :: N, LDA, LWORK, INFO
      integer                                             :: IERR, IOSTAT = 0

      ! arrays and matrices
      complex(kind = dp) , dimension(:,:), intent(inout)  :: A
      real(kind = dp) , dimension(:), allocatable         :: W
      complex(kind = dp) , dimension(:), allocatable      :: WORK
      real(kind = dp) , dimension(:), allocatable         :: RWORK


      JOBZ = 'N'
      UPLO = 'U'

      N = size(A, 1)
      LDA  = max(1, N)
      LWORK = max(1, 2*N - 1)
      INFO = IOSTAT

      allocate(W(N), stat=IERR)
      if (IERR /= 0) stop "failed to allocate W"

      allocate(WORK(max(1,LWORK)), stat=IERR)
      if (IERR /= 0) stop "failed to allocate WORK"

      allocate(RWORK(max(1, 3*N - 2)), stat=IERR)
      if (IERR /= 0) stop "failed to allocate RWORK"

      call zheev(JOBZ , UPLO , N , A , LDA , W, WORK, LWORK, RWORK, INFO)

  end subroutine call_zheev

end module zheev_module

module numerical_subroutines
    use constants
    use zheev_module
    implicit none

    contains

!--------------------------------------------------------------------------------------!
!generate_eigen_matrix subroutine takes the inputs from the inputs.txt file and the    !
!current k value that is being looped through, and calculates the related eigen_matrix !
!(i.e. a matrix of dimensions size(epsilon_array_in)xsize(epsilon_array_in) which is         !
!Hermitian and will be passed into the ZHEEV function which will calculate the eigen-  !
!-values for our eigen_matrix) for our 1D linear chain system with size(epsilon_array_in) !
! atoms per unit cell                                                                  !
!--------------------------------------------------------------------------------------!
    subroutine generate_eigen_matrix(k_current, a_in, t_in, epsilon_array_in, matrix_size, eigen_matrix)
        implicit none
        real(kind=dp), intent(IN)                       :: t_in, a_in, k_current
        real(kind=dp), dimension(:), intent(IN)         :: epsilon_array_in
        integer, intent(IN)                             :: matrix_size
        complex(kind=dp), dimension(:,:), intent(OUT)   :: eigen_matrix
        integer                                         :: i, j
        complex(kind=dp)                                :: i_const = (0.0_dp,1.0_dp)

        eigen_matrix = 0.0_dp

        do i = 1, matrix_size
            do j = 1, matrix_size

                !Sets the \ diagonal to epsilons (the on-site energies)
                if (i==j) then
                    eigen_matrix(i,j) = epsilon_array_in(i)
                !Sets the values next to the epsilons to -t if they're not on the border (the intra-hopping energy)
                else if (((j==i-1) .or. (j==i+1)) .and. (j/=1 .and. j/=matrix_size) .and. (i/=1 .and. i/=matrix_size)) then
                    eigen_matrix(i,j) = -t_in
                !Sets the top right corner to the - exponential (the inter-hopping energy)
                else if (i==1 .and. j==matrix_size) then
                    eigen_matrix(i,j) = -t_in*exp(-i_const*k_current*a_in)
                !Sets the bottom left corner to the + exponential (the inter-hopping energy)
                else if (i==matrix_size .and. j==1) then
                    eigen_matrix(i,j) = -t_in*exp(i_const*k_current*a_in)
                end if

                !Adds the non-periodic terms
                if (matrix_size == 2) then
                    !Adds the -t in the top right corner (the intra-hopping energy)
                    if (i==1 .and. j==matrix_size) then
                        eigen_matrix(i,j) = eigen_matrix(i,j) - t_in
                    !Adds the -t in the bottom left corner (the intra-hopping energy)
                    else if (i==matrix_size .and. j==1) then
                        eigen_matrix(i,j) = eigen_matrix(i,j) - t_in
                    end if
                else
                    !Adds the -t left to the top right corner (the intra-hopping energy)
                    if (i==1 .and. j==matrix_size-1) then
                        eigen_matrix(i,j) = eigen_matrix(i,j) - t_in
                    !Adds the -t below the top right corner (the intra-hopping energy)
                    else if (i==2 .and. j==matrix_size) then
                        eigen_matrix(i,j) = eigen_matrix(i,j) - t_in
                    !Adds the -t above the bottom left corner (the intra-hopping energy)
                    else if (i==matrix_size-1 .and. j==1) then
                        eigen_matrix(i,j) = eigen_matrix(i,j) - t_in
                    !Adds the -t above the bottom left corner (the intra-hopping energy)
                    else if (i==matrix_size .and. j==2) then
                        eigen_matrix(i,j) = eigen_matrix(i,j) - t_in
                    end if
                end if
            end do
        end do
    end subroutine generate_eigen_matrix

    subroutine find_eigenval(a_in , epsilon_array_in , t_in, steps_in, matrix_size)
        ! Inputs  : a_in, epsilon_array_in, t_in, steps_in
        ! Outputs : none
        !
        ! Purpose : to find the numerical solutions by setting up the eigen matrix
        ! and then using zheev from LAPACK to find the eigenvalues
        implicit none
        real(kind = dp) , intent(IN)                             :: a_in, t_in
        real(kind=dp), dimension(:), intent(in)                  :: epsilon_array_in
        real(kind = dp)                                          :: k_min, k_max, k_step, k_current
        integer , intent(IN)                                     :: steps_in, matrix_size
        integer                                                  :: istat , k_big , i , j
        complex(kind = dp), dimension(matrix_size , matrix_size) :: eigen_matrix
        !complex(kind = dp)                          :: i_const = (0.0_dp , 1.0_dp)

        real(kind = dp) , dimension(:), allocatable              :: eigen_values
        integer                                                  :: file_unit_1 = 5, file_unit_2 = 10
        character(len = 40)                                      :: filename_i

        k_min     = -pi/a_in
        k_max     = pi/a_in
        k_step    = (k_max - k_min)/real(steps_in , kind = dp)
        k_current = k_min
		
		
        open (unit = file_unit_1 , file = 'outputs/double_numerical_1.csv' , status = "replace" , action = "write", iostat = istat)
        open (unit = file_unit_2, file = 'outputs/double_numerical_2.csv', status = "replace", action= "write", iostat=istat)
        if(matrix_size >= 3)then
            do i = 3 , matrix_size
                write(filename_i , fmt='(a25 , i1 , a4)') 'outputs/double_numerical_' , i , '.csv'
                open (unit = 5*i , file = filename_i , status = "replace" ,&
                &action = "write", iostat = istat)
            end do
        end if

        ! looping over the k values to find the eigenmatrix for each k value
        do k_big = 0 , steps_in , 1

          !eigen_matrix(1 , 1) = epsilon_a_in
          !eigen_matrix(2 , 2) = epsilon_b_in

          !eigen_matrix(1 , 2) = -t_in*(1.0_dp + EXP(-1.0_dp*i_const*k_current*a_in))
          !eigen_matrix(2 , 1) = -t_in*(1.0_dp + EXP(i_const*k_current*a_in))

          call generate_eigen_matrix(k_current, a_in, t_in, epsilon_array_in, matrix_size, eigen_matrix)

          call call_zheev(eigen_matrix, eigen_values)

          do j = 1 , matrix_size
            write (unit = 5*j , fmt = * , iostat = istat) k_current, ",", eigen_values(j)
          end do



!
!           print*, eigen_values
          deallocate(eigen_values)

          k_current = k_current + k_step
        end do

        do j = 1 , matrix_size
            close (unit = 5*j , iostat = istat)
            if (istat /= 0) stop "error closing test file"
        end do
		
    end subroutine find_eigenval

end module numerical_subroutines

program Nate
    use constants
    use analytic_subroutines
    use numerical_subroutines
    !subroutines module put into main code here
    implicit none
    integer                                 :: N_val, atoms_per_unit_cell
    real(kind = dp)                         :: a_val, t_val
    real(kind=dp), dimension(:),allocatable :: epsilon_array
    character(len = 30)                     :: filename1
    character(len = 30)                     :: case_type

    !a_val         = 1.0_dp
    !epsilon_a_val = 0.0_dp
    !epsilon_b_val = 0.0_dp
    !t_val         = 1.0_dp
    !N_val         = 1E3
    
	!Gets the N, a, t, epsilon_a, and epsilon_b values from the inputs.txt file
    call get_inputs(N_val, a_val, t_val, epsilon_array, atoms_per_unit_cell)

    call find_eigenval(a_val , epsilon_array , t_val, N_val, atoms_per_unit_cell)

    if (atoms_per_unit_cell == 2) then
        filename1 = 'outputs/single_analytical.csv'
        case_type = 'single'
        !case types used to define type of analytic solution wanted :- single calculated here
        call plotting_subroutine(a_val , epsilon_array(1), epsilon_array(2), t_val , N_val , filename1 , case_type)
        !analytic solution calculated and written to .csv file all through plotting_subroutine

        filename1 = 'outputs/double_analytical.csv'
        case_type = 'double'
        !double calculated here
        call plotting_subroutine(a_val , epsilon_array(1), epsilon_array(2) , t_val , N_val , filename1 , case_type)
    end if

end program Nate
