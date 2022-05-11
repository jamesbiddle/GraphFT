program test
  use graphft
  use kinds
  implicit none

  type MyType
    integer :: a
  end type MyType

  type(Graph) :: bp_graph
  type(Node), pointer :: my_node
  integer, allocatable :: connections(:, :), dists(:)
  integer :: i, n_nodes, d

  type(MyType), pointer :: t
  type(MyType), pointer :: t_ptr1, t_ptr2
  type(MyType), allocatable :: array1(:), array2(:)

  call random_seed()

  ! Simple graph
  write(*,*) "Simple graph"
  call Graph(bp_graph, 2)
  call bp_graph%add_edge(1, 2, 4)
  call bp_graph%add_edge(1, 2, 2)
  call bp_graph%add_edge(1, 2, 3)
  call bp_graph%add_node()

  call bp_graph%bp_distances(dists)
  write(*,*) "Dists:"
  write(*,*) dists
  write(*,*)

  call bp_graph%shortest_distance(1, 2, d)
  write(*,*) "Shortest distance between nodes 1 and 2 = ", d


  ! Double-touching graph
  write(*,*) "Double-touching graph"
  call Graph(bp_graph, 4)
  call bp_graph%add_edge(1, 2)
  call bp_graph%add_edge(1, 2)
  call bp_graph%add_edge(2, 1)
  call bp_graph%add_edge(2, 4)
  call bp_graph%add_edge(3, 1)
  call bp_graph%add_edge(3, 4)
  call bp_graph%add_edge(3, 4)
  call bp_graph%bp_distances(dists)
  write(*,*) "Dists:"
  write(*,*) dists
  write(*,*)

  call bp_graph%shortest_distance(1, 4, d)
  write(*,*) "Shortest distance between nodes 1 and 4 = ", d


  ! Ambiguity graph from paper
  write(*,*) "Paper ambiguity graph"
  call Graph(bp_graph, 6)
  call bp_graph%add_edge(1, 5)
  call bp_graph%add_edge(2, 5)
  call bp_graph%add_edge(5, 6)
  call bp_graph%add_edge(5, 6, 3)
  call bp_graph%add_edge(6, 4)
  call bp_graph%add_edge(6, 3, 2)

  call bp_graph%add_edge(1, 4)
  call bp_graph%add_edge(1, 4)

  call bp_graph%add_edge(2, 3)
  call bp_graph%add_edge(2, 3)

  call bp_graph%bp_distances(dists)
  write(*,*) "Dists:"
  write(*,*) dists
  write(*,*)

  ! Graph featuring a self-loop
  write(*,*) "Self-loop graph"
  call Graph(bp_graph, 3)
  call bp_graph%add_edge(1, 2)
  call bp_graph%add_edge(1, 3, 2)
  call bp_graph%add_edge(1, 3)
  call bp_graph%add_edge(2, 3)
  call bp_graph%add_edge(2, 2, 3)

  call bp_graph%bp_distances(dists)
  write(*,*) "Dists:"
  write(*,*) dists
  write(*,*)

  ! Graph containing only a touching point
  write(*,*) "Touching point only graph"
  call Graph(bp_graph, 1)
  call bp_graph%add_edge(1, 1, 4)
  call bp_graph%add_edge(1, 1, 4)

  call bp_graph%bp_distances(dists)
  if(allocated(dists)) then
    write(*,*) "Dists:"
    write(*,*) dists
    write(*,*)
  else
    write(*,*) 'Dists not allocated'
  end if

  open(101, file='test.graph', action='write', status='replace')
  call bp_graph%output_graph(101, 2, 1)
  close(101)
  ! do i = 1, bp_graph%n_nodes()
  !   call bp_graph%get_node(my_node, i, index=.true.)
  !   write(*,*) my_node%get_id(), my_node%is_bp()
  ! end do

  call bp_graph%delete_graph()
  if(allocated(dists)) deallocate(dists)
end program test
