module MathsOps
  use kinds
  use IFPORT
  use, intrinsic :: iso_c_binding, only: c_size_t
  implicit none

  interface mean
    module procedure mean_SP
    module procedure mean_DP
    module procedure mean_2D_SP
    module procedure mean_2D_DP
  end interface mean

  interface median
    module procedure median_SP
    module procedure median_DP
  end interface median

  interface std
    module procedure std_SP
    module procedure std_DP
  end interface std

  interface jackknife
    module procedure jackknife_SP
    module procedure jackknife_DP
  end interface jackknife

  interface sort
    module procedure sort_int
    module procedure sort_SP
    module procedure sort_DP
  end interface sort

  interface random_index
    module procedure random_index_int
    module procedure random_index_char
    module procedure random_index_log
    module procedure random_index_SP
    module procedure random_index_DP
  end interface random_index

contains

  ! Mean functions
  real(SP) function mean_SP(array)
    real(SP), dimension(:), intent(in) :: array

    integer :: i

    mean_SP = 0.0_SP
    do i = 1, size(array)
      mean_SP = (mean_SP * real((i - 1), SP) + array(i)) / real(i, SP)
    end do

  end function mean_SP


  real(DP) function mean_DP(array)
    real(DP), dimension(:), intent(in) :: array

    integer :: i

    mean_DP = 0.0_DP
    do i = 1, size(array)
      mean_DP = (mean_DP * real((i - 1), DP) + array(i)) / real(i, DP)
    end do
  end function mean_DP

  real(SP) function mean_2D_SP(array)
    real(SP), dimension(:, :), intent(in) :: array

    integer :: i, j, c
    integer, dimension(2) :: arr_shape

    arr_shape = shape(array)

    mean_2D_SP = 0.0_SP
    c = 1
    do j = 1, arr_shape(2)
      do i = 1, arr_shape(1)
        mean_2D_SP = (mean_2D_SP * real((c - 1), SP) + array(i, j)) / real(c, SP)
        c = c + 1
      end do
    end do
  end function mean_2D_SP


  real(DP) function mean_2D_DP(array)
    real(DP), dimension(:, :), intent(in) :: array

    integer :: i, j, c
    integer, dimension(2) :: arr_shape

    arr_shape = shape(array)

    mean_2D_DP = 0.0_DP
    c = 1
    do j = 1, arr_shape(2)
      do i = 1, arr_shape(1)
        mean_2D_DP = (mean_2D_DP * real((c - 1), DP) + array(i, j)) / real(c, DP)
        c = c + 1
      end do
    end do
  end function mean_2D_DP

  ! Standard deviation functions
  real(SP) function std_SP(array, ddof)
    real(SP), dimension(:), intent(in) :: array
    integer, optional :: ddof

    ! Local vars
    real(SP) :: mean, N
    integer :: ddof_

    if( .not. present(ddof)) then
      ddof_ = 0
    else
      ddof_ = ddof
    end if

    N = real(size(array), SP)

    mean = mean_SP(array)
    std_SP = mean_SP((array(:) - mean) ** 2) * (N / (N - ddof_))
    std_SP = sqrt(std_SP)
  end function std_SP


  real(DP) function std_DP(array, ddof)
    real(DP), dimension(:), intent(in) :: array
    integer, intent(in), optional :: ddof

    ! Local vars
    real(DP) :: mean, N
    integer :: ddof_

    if( .not. present(ddof)) then
      ddof_ = 0
    else
      ddof_ = ddof
    end if
    N = real(size(array), DP)

    mean = mean_DP(array)
    std_DP = mean_DP((array(:) - mean) ** 2) * (N / (N - ddof_))
    std_DP = sqrt(std_DP)
  end function std_DP


  ! Jackknife functions
  real(SP) function jackknife_SP(array)
    real(SP), dimension(:), intent(in) :: array

    ! Local vars
    real(SP), dimension(:), allocatable :: Zi
    real(SP) :: Zbarbar, mean, N

    allocate(Zi(size(array)))
    N = real(size(array), SP)

    mean = mean_SP(array)

    Zi = (N * mean - array) / (N - 1.0_SP)

    Zbarbar = mean_SP(Zi)
    jackknife_SP = mean_SP((Zi(:) - Zbarbar) ** 2) * (N - 1.0_SP)
    jackknife_SP = sqrt(jackknife_SP)
  end function jackknife_SP


  real(DP) function jackknife_DP(array)
    real(DP), dimension(:), intent(in) :: array

    ! Local vars
    real(DP), dimension(:), allocatable :: Zi
    real(DP) :: Zbarbar, mean, N

    allocate(Zi(size(array)))
    N = real(size(array), DP)

    mean = mean_DP(array)

    Zi = (N * mean - array) / (N - 1.0_DP)

    Zbarbar = mean_DP(Zi)
    jackknife_DP = mean_DP((Zi(:) - Zbarbar) ** 2) * (N - 1.0_DP)
    jackknife_DP = sqrt(jackknife_DP)
  end function jackknife_DP


  ! Median functions
  real(DP) function median_DP(array)
    real(DP), dimension(:), intent(in) :: array

    ! Local vars
    real(DP), dimension(size(array)) :: array_copy
    integer :: array_size, midpoint

    array_size = size(array)

    array_copy = array
    call sort(array_copy)

    if(mod(array_size, 2) == 1) then
      midpoint = (array_size / 2) + 1
      median_DP = array_copy(midpoint)
    else
      midpoint = array_size / 2
      median_DP = (array_copy(midpoint) + array_copy(midpoint + 1)) / 2.0d0
    end if

  end function median_DP

  real(SP) function median_SP(array)
    real(SP), dimension(:), intent(in) :: array

    ! Local vars
    real(SP), dimension(size(array)) :: array_copy
    integer :: array_size, midpoint

    array_size = size(array)

    array_copy = array
    call sort(array_copy)

    if(mod(array_size, 2) == 1) then
      midpoint = (array_size / 2) + 1
      median_SP = array_copy(midpoint)
    else
      midpoint = array_size / 2
      median_SP = (array_copy(midpoint) + array_copy(midpoint + 1)) / 2.0d0
    end if

  end function median_SP

  subroutine find_missing_positive(A, x)
    ! Find the first missing positive integer x in array A
    integer, dimension(:, :), value :: A
    integer, intent(out) :: x

    ! Local vars
    integer, dimension(:), allocatable :: B
    integer :: i, N

    allocate(B(size(A)))

    B = reshape(A, [size(A)])
    call segregate_array(B, N)

    do i = 1, N
      if(B(i) < N) then
        B(B(i)) = -B(B(i))
      end if
    end do

    write(*,*) "B = "
    write(*,*) B

    x = findloc((B > 0), .true., dim=1)

  end subroutine find_missing_positive

  subroutine segregate_array(A, max_idx)
    ! Function to push all non-positive numbers to the back of an array
    integer, dimension(:), intent(inout) :: A
    integer, intent(out) :: max_idx

    integer :: i, j, N, temp

    N = size(A)
    j = 1

    do i = 1, N
      if(A(i) > 0) then
        temp = A(i)
        A(i) = A(j)
        A(j) = temp
        j = j + 1
      end if
    end do

    max_idx = j - 1
  end subroutine segregate_array


  subroutine sort_DP(array, order)
    real(DP), dimension(:), intent(inout) :: array
    character(len=3), optional :: order

    ! Local vars
    integer(C_SIZE_T) :: array_len, array_size
    character(len=3) :: order_

    if(present(order)) then
      order_ = order
    else
      order_ = 'inc'
    end if

    array_len = size(array)
    array_size = DP

    if(order_ == 'inc') then
      call qsort(array, array_len, array_size, order_inc_DP)
    else if(order_ == 'dec') then
      call qsort(array, array_len, array_size, order_dec_DP)
    else
      write(*,*) "Invalid sort order, aborting"
      stop
    end if

  end subroutine sort_DP


  subroutine sort_SP(array, order)
    real(SP), dimension(:), intent(inout) :: array
    character(len=3), optional :: order

    ! Local vars
    integer(C_SIZE_T) :: array_len, array_size
    character(len=3) :: order_

    if(present(order)) then
      order_ = order
    else
      order_ = 'inc'
    end if

    array_len = size(array)
    array_size = SP

    if(order_ == 'inc') then
      call qsort(array, array_len, array_size, order_inc_SP)
    else if(order_ == 'dec') then
      call qsort(array, array_len, array_size, order_dec_SP)
    else
      write(*,*) "Invalid sort order, aborting"
      stop
    end if

  end subroutine sort_SP


  subroutine sort_int(array, order)
    integer, dimension(:), intent(inout) :: array
    character(len=3), optional :: order

    ! Local vars
    integer(C_SIZE_T) :: array_len, array_size
    character(len=3) :: order_

    if(present(order)) then
      order_ = order
    else
      order_ = 'inc'
    end if

    array_len = size(array)
    array_size = kind(1)

    if(order_ == 'inc') then
      call qsort(array, array_len, array_size, order_inc_int)
    else if(order_ == 'dec') then
      call qsort(array, array_len, array_size, order_dec_int)
    else
      write(*,*) "Invalid sort order, aborting"
      stop
    end if

  end subroutine sort_int


  integer(2) function order_inc_DP(a1, a2)
    real(DP) :: a1,a2

    if(a1 < a2) then
      order_inc_DP = -1
    else if(a1 > a2) then
      order_inc_DP = 1
    else if(a1 == a2) then
      order_inc_DP = 0
    end if
  end function order_inc_DP

  integer(2) function order_dec_DP(a1, a2)
    real(DP) :: a1,a2

    if(a1 > a2) then
      order_dec_DP = -1
    else if(a1 < a2) then
      order_dec_DP = 1
    else if(a1 == a2) then
      order_dec_DP = 0
    end if

  end function order_dec_DP


  integer(2) function order_inc_SP(a1, a2)
    real(SP) :: a1,a2

    if(a1 < a2) then
      order_inc_SP = -1
    else if(a1 > a2) then
      order_inc_SP = 1
    else if(a1 == a2) then
      order_inc_SP = 0
    end if
  end function order_inc_SP


  integer(2) function order_dec_SP(a1, a2)
    real(SP) :: a1,a2

    if(a1 > a2) then
      order_dec_SP = -1
    else if(a1 < a2) then
      order_dec_SP = 1
    else if(a1 == a2) then
      order_dec_SP = 0
    end if

  end function order_dec_SP


  integer(2) function order_inc_int(a1, a2)
    integer :: a1,a2

    if(a1 < a2) then
      order_inc_int = -1
    else if(a1 > a2) then
      order_inc_int = 1
    else if(a1 == a2) then
      order_inc_int = 0
    end if
  end function order_inc_int


  integer(2) function order_dec_int(a1, a2)
    integer :: a1,a2

    if(a1 > a2) then
      order_dec_int = -1
    else if(a1 < a2) then
      order_dec_int = 1
    else if(a1 == a2) then
      order_dec_int = 0
    end if

  end function order_dec_int


  function random_index_int(array, match) result(idx)
    integer, dimension(:), intent(in) :: array
    integer, intent(in), optional :: match

    real(DP) :: rand_num
    integer :: rand_idx, idx, range
    integer :: i, counter

    call random_number(rand_num)
    if(present(match)) then
      range = count(array == match)
      ! If no matches are in the array, return an invalid index
      if(range == 0) then
        idx = -1
        return
      end if
    else
      range = size(array)
    end if

    rand_num = rand_num * range
    rand_idx = floor(rand_num) + 1

    if(present(match)) then
      counter = 1
      do i = 1, size(array)
        if((counter == rand_idx) .and. (array(i) == match)) then
          idx = i
          return
        else if(array(i) == match) then
          counter = counter + 1
        end if
      end do
    else
      idx = rand_idx
    end if

  end function random_index_int


  function random_index_char(array, match) result(idx)
    character(len=*), dimension(:), intent(in) :: array
    character(len=*), intent(in), optional:: match

    real(DP) :: rand_num
    integer :: rand_idx, idx, range
    integer :: i, counter

    call random_number(rand_num)
    if(present(match)) then
      range = count(array == match)
      ! If no matches are in the array, return an invalid index
      if(range == 0) then
        idx = -1
        return
      end if
    else
      range = size(array)
    end if

    rand_num = rand_num * range
    rand_idx = floor(rand_num) + 1

    if(present(match)) then
      counter = 1
      do i = 1, size(array)
        if((counter == rand_idx) .and. (array(i) == match)) then
          idx = i
          return
        else if(array(i) == match) then
          counter = counter + 1
        end if
      end do
    else
      idx = rand_idx
    end if

  end function random_index_char


  function random_index_log(array) result(idx)
    logical, dimension(:), intent(in) :: array

    real(DP) :: rand_num
    integer :: rand_idx, idx, range
    integer :: i, counter

    call random_number(rand_num)
    range = count(array)
    ! If no matches are in the array, return an invalid index
    if(range == 0) then
      idx = -1
      return
    end if

    rand_num = rand_num * range
    rand_idx = floor(rand_num) + 1

    if(range .ne. size(array)) then
      counter = 1
      do i = 1, size(array)
        if((counter == rand_idx) .and. array(i)) then
          idx = i
          return
        else if(array(i)) then
          counter = counter + 1
        end if
      end do
    else
      idx = rand_idx
    end if

  end function random_index_log


  function random_index_SP(array, match) result(idx)
    real(SP), dimension(:), intent(in) :: array
    real(SP), intent(in), optional:: match

    real(DP) :: rand_num
    integer :: rand_idx, idx, range
    integer :: i, counter

    call random_number(rand_num)
    if(present(match)) then
      range = count(array == match)
      ! If no matches are in the array, return an invalid index
      if(range == 0) then
        idx = -1
        return
      end if
    else
      range = size(array)
    end if

    rand_num = rand_num * range
    rand_idx = floor(rand_num) + 1

    if(present(match)) then
      counter = 1
      do i = 1, size(array)
        if((counter == rand_idx) .and. (array(i) == match)) then
          idx = i
          return
        else if(array(i) == match) then
          counter = counter + 1
        end if
      end do
    else
      idx = rand_idx
    end if

  end function random_index_SP


  function random_index_DP(array, match) result(idx)
    real(DP), dimension(:), intent(in) :: array
    real(DP), intent(in), optional:: match

    real(DP) :: rand_num
    integer :: rand_idx, idx, range
    integer :: i, counter

    call random_number(rand_num)
    if(present(match)) then
      range = count(array == match)
      ! If no matches are in the array, return an invalid index
      if(range == 0) then
        idx = -1
        return
      end if
    else
      range = size(array)
    end if

    rand_num = rand_num * range
    rand_idx = floor(rand_num) + 1

    if(present(match)) then
      counter = 1
      do i = 1, size(array)
        if((counter == rand_idx) .and. (array(i) == match)) then
          idx = i
          return
        else if(array(i) == match) then
          counter = counter + 1
        end if
      end do
    else
      idx = rand_idx
    end if

  end function random_index_DP

end module MathsOps
