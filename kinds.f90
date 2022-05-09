module kinds
  implicit none
  integer, parameter  :: K10 = selected_int_kind(10)
  integer, parameter  :: SP = kind(1.0)
  integer, parameter  :: DP = kind(1.0d0)
  integer, parameter :: dc = kind((1.0D0,1.0D0)) !! Double precision complex scalars.

  real(dp), parameter :: pi = 4.0*atan(1.0d0)
  real(DP), parameter :: tol = 1.0d-8

  ! Lattice Parameters
  integer :: nx, ny, nz, nt
  integer, parameter :: ND = 4
  integer, parameter :: NC = 3
  integer, dimension(nd) :: dims

end module kinds
