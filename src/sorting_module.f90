!!> \file sorting_module.f90
!!> \brief Iterative Quicksort implementation for integer and real arrays.
!!
!!> \author Candidate
!!> \date 2025
!!
!!> \details
!!> This module provides PURE, iterative Quicksort subroutines for sorting
!!> one-dimensional INTEGER and REAL arrays in ascending order.
!!> No recursion or dynamic memory allocation is used. The caller must
!!> provide integer stacks used to emulate recursion.

module sorting_module
  !!> \brief Module providing generic Quicksort routines.
  !!
  !!> \details
  !!> The generic interface \ref quicksort allows sorting of both INTEGER
  !!> and REAL arrays using the same procedure name. All subroutines are
  !!> declared PURE and require the caller to manage stack memory.

  implicit none
  private

  public :: quicksort

  !!> \brief Generic Quicksort interface for INTEGER and REAL arrays.
  interface quicksort
     module procedure quicksort_int
     module procedure quicksort_real
  end interface quicksort

contains

  !!> \brief Iterative Quicksort for INTEGER arrays.
  !!
  !!> \param a
  !!>   Integer array to be sorted in ascending order (in-place).
  !!> \param left_stack
  !!>   Integer array used as stack for left indices. Must be large enough
  !!>   to hold all subranges (a safe size is at least size(a)).
  !!> \param right_stack
  !!>   Integer array used as stack for right indices. Must match left_stack
  !!>   in size.
  pure subroutine quicksort_int(a, left_stack, right_stack)
    implicit none
    integer, intent(inout) :: a(:)
    integer, intent(inout) :: left_stack(:)
    integer, intent(inout) :: right_stack(:)

    integer :: n
    integer :: top
    integer :: l, r
    integer :: i, j
    integer :: pivot
    integer :: tmp

    n = size(a)
    if (n <= 1) return

    ! Initialize stack with the full range
    top = 1
    left_stack(top)  = 1
    right_stack(top) = n

    do while (top > 0)
       ! Pop a segment [l, r] from the stack
       l = left_stack(top)
       r = right_stack(top)
       top = top - 1

       i = l
       j = r
       pivot = a((l + r) / 2)

       ! Hoare partition scheme
       do
          do while (a(i) < pivot)
             i = i + 1
          end do
          do while (a(j) > pivot)
             j = j - 1
          end do

          if (i <= j) then
             tmp = a(i)
             a(i) = a(j)
             a(j) = tmp
             i = i + 1
             j = j - 1
          end if

          if (i > j) exit
       end do

       ! Push left subrange [l, j]
       if (l < j) then
          top = top + 1
          left_stack(top)  = l
          right_stack(top) = j
       end if

       ! Push right subrange [i, r]
       if (i < r) then
          top = top + 1
          left_stack(top)  = i
          right_stack(top) = r
       end if
    end do

  end subroutine quicksort_int


  !!> \brief Iterative Quicksort for REAL arrays.
  !!
  !!> \param a
  !!>   Real array to be sorted in ascending order (in-place).
  !!> \param left_stack
  !!>   Integer array used as stack for left indices. Must be large enough
  !!>   to hold all subranges (a safe size is at least size(a)).
  !!> \param right_stack
  !!>   Integer array used as stack for right indices. Must match left_stack
  !!>   in size.
  pure subroutine quicksort_real(a, left_stack, right_stack)
    implicit none
    real,    intent(inout) :: a(:)
    integer, intent(inout) :: left_stack(:)
    integer, intent(inout) :: right_stack(:)

    integer :: n
    integer :: top
    integer :: l, r
    integer :: i, j
    real    :: pivot
    real    :: tmp

    n = size(a)
    if (n <= 1) return

    ! Initialize stack with the full range
    top = 1
    left_stack(top)  = 1
    right_stack(top) = n

    do while (top > 0)
       ! Pop a segment [l, r] from the stack
       l = left_stack(top)
       r = right_stack(top)
       top = top - 1

       i = l
       j = r
       pivot = a((l + r) / 2)

       ! Hoare partition scheme
       do
          do while (a(i) < pivot)
             i = i + 1
          end do
          do while (a(j) > pivot)
             j = j - 1
          end do

          if (i <= j) then
             tmp = a(i)
             a(i) = a(j)
             a(j) = tmp
             i = i + 1
             j = j - 1
          end if

          if (i > j) exit
       end do

       ! Push left subrange [l, j]
       if (l < j) then
          top = top + 1
          left_stack(top)  = l
          right_stack(top) = j
       end if

       ! Push right subrange [i, r]
       if (i < r) then
          top = top + 1
          left_stack(top)  = i
          right_stack(top) = r
       end if
    end do

  end subroutine quicksort_real

end module sorting_module
