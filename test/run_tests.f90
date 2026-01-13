!!> \file run_tests.f90
!!> \brief Test program for the iterative Quicksort module.
!!
!!> \details
!!> This program exercises the generic quicksort interface from
!!> sorting_module with INTEGER and REAL arrays. It tests:
!!>  - A random array
!!>  - An already sorted array
!!>  - A reverse-sorted array
!!>  - An array with duplicate elements
!!>  - Empty and single-element arrays
!!> For each test, the array is printed before and after sorting.

program run_tests
  use sorting_module
  implicit none

  integer, parameter :: n = 10

  ! Integer test arrays
  integer :: int_random(n)
  integer :: int_sorted(n)
  integer :: int_reverse(n)
  integer :: int_duplicates(n)
  integer :: int_single(1)
  integer :: int_empty(0)

  ! Real test arrays
  real :: real_random(n)
  real :: real_sorted(n)
  real :: real_reverse(n)
  real :: real_duplicates(n)
  real :: real_single(1)
  real :: real_empty(0)

  ! Stacks for iterative Quicksort (indices)
  integer :: stack_left(n)
  integer :: stack_right(n)

  integer :: i
  real    :: tmp

  ! ------------------------
  ! Initialize integer arrays
  ! ------------------------

  ! Random integers in [0, 99]
  call random_seed()
  do i = 1, n
     call random_number(tmp)
     int_random(i) = int(100.0 * tmp)
  end do

  ! Already sorted integers
  do i = 1, n
     int_sorted(i) = i
  end do

  ! Reverse-sorted integers
  do i = 1, n
     int_reverse(i) = n - i + 1
  end do

  ! Integers with duplicates
  int_duplicates = (/ 3, 1, 2, 3, 2, 1, 3, 2, 1, 3 /)

  ! Single-element integer array
  int_single(1) = 42

  ! Empty integer array: int_empty has size 0 by declaration

  ! --------------------
  ! Initialize real arrays
  ! --------------------

  ! Random reals in [0, 1)
  do i = 1, n
     call random_number(real_random(i))
  end do

  ! Already sorted reals
  do i = 1, n
     real_sorted(i) = real(i)
  end do

  ! Reverse-sorted reals
  do i = 1, n
     real_reverse(i) = real(n - i + 1)
  end do

  ! Reals with duplicates
  real_duplicates = (/ 1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 2.0, 1.0, 1.0, 3.0 /)

  ! Single-element real array
  real_single(1) = 3.14159

  ! Empty real array: real_empty has size 0 by declaration

  ! ======================
  ! Integer array test cases
  ! ======================

  print *, '============================'
  print *, 'INTEGER ARRAY TESTS'
  print *, '============================'

  call test_int_case('Random integer array',      int_random,     stack_left, stack_right)
  call test_int_case('Sorted integer array',      int_sorted,     stack_left, stack_right)
  call test_int_case('Reverse integer array',     int_reverse,    stack_left, stack_right)
  call test_int_case('Duplicate integer array',   int_duplicates, stack_left, stack_right)
  call test_int_case('Single integer element',    int_single,     stack_left, stack_right)
  call test_int_case('Empty integer array',       int_empty,      stack_left, stack_right)

  ! ==================
  ! Real array test cases
  ! ==================

  print *, '============================'
  print *, 'REAL ARRAY TESTS'
  print *, '============================'

  call test_real_case('Random real array',        real_random,     stack_left, stack_right)
  call test_real_case('Sorted real array',        real_sorted,     stack_left, stack_right)
  call test_real_case('Reverse real array',       real_reverse,    stack_left, stack_right)
  call test_real_case('Duplicate real array',     real_duplicates, stack_left, stack_right)
  call test_real_case('Single real element',      real_single,     stack_left, stack_right)
  call test_real_case('Empty real array',         real_empty,      stack_left, stack_right)

contains

  !!> \brief Helper to print and sort an integer array test case.
  !!
  !!> \param title
  !!>   Descriptive name of the test case.
  !!> \param a
  !!>   Integer array to sort.
  !!> \param left_stack
  !!>   Stack for left indices.
  !!> \param right_stack
  !!>   Stack for right indices.
  subroutine test_int_case(title, a, left_stack, right_stack)
    character(len=*), intent(in)    :: title
    integer,          intent(inout) :: a(:)
    integer,          intent(inout) :: left_stack(:)
    integer,          intent(inout) :: right_stack(:)

    print *
    print *, '--- ', trim(title)
    print *, 'Before sorting (INTEGER):'
    call print_int_array(a)

    if (size(a) > 0) then
       call quicksort(a, left_stack, right_stack)
    end if

    print *, 'After sorting (INTEGER):'
    call print_int_array(a)
  end subroutine test_int_case


  !!> \brief Helper to print and sort a real array test case.
  !!
  !!> \param title
  !!>   Descriptive name of the test case.
  !!> \param a
  !!>   Real array to sort.
  !!> \param left_stack
  !!>   Stack for left indices.
  !!> \param right_stack
  !!>   Stack for right indices.
  subroutine test_real_case(title, a, left_stack, right_stack)
    character(len=*), intent(in)    :: title
    real,             intent(inout) :: a(:)
    integer,          intent(inout) :: left_stack(:)
    integer,          intent(inout) :: right_stack(:)

    print *
    print *, '--- ', trim(title)
    print *, 'Before sorting (REAL):'
    call print_real_array(a)

    if (size(a) > 0) then
       call quicksort(a, left_stack, right_stack)
    end if

    print *, 'After sorting (REAL):'
    call print_real_array(a)
  end subroutine test_real_case


  !!> \brief Utility to print an integer array on one line.
  !!
  !!> \param a
  !!>   Integer array to print.
  subroutine print_int_array(a)
    integer, intent(in) :: a(:)

    if (size(a) == 0) then
       print *, '[ ] (empty)'
    else
       write(*,'( "  ", *(I6) )') a
    end if
  end subroutine print_int_array


  !!> \brief Utility to print a real array on one line.
  !!
  !!> \param a
  !!>   Real array to print.
  subroutine print_real_array(a)
    real, intent(in) :: a(:)

    if (size(a) == 0) then
       print *, '[ ] (empty)'
    else
       write(*,'( "  ", *(F10.4) )') a
    end if
  end subroutine print_real_array

end program run_tests
