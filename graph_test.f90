program graph_test
  use GraphFT
  use kinds
  implicit none

  type(Graph) :: test_graph
  type(Node), pointer :: test_node

  integer :: failed = 0
  integer, allocatable :: connections(:, :), expected(:, :)
  integer :: distance

  write(*,*) "Test calling functions on an empty graph"
  call empty_graph_tests()

  write(*,*)
  write(*,*) "Test adding a single node"
  call test_graph%add_node()
  call populated_graph_tests(1, 0, 0)

  write(*,*)
  write(*,*) "Test removing a single node"
  call test_graph%remove_node(1)
  write(*, '(4x,a,i0,x,a)') "Number of nodes: ", test_graph%n_nodes(), test_int(test_graph%n_nodes(), 0)

  write(*,*)
  write(*,*) "Retesting calling functions on an empty graph"
  call empty_graph_tests()

  write(*,*)
  write(*,*) "Test initialising graph with two nodes and an edge between them"
  call Graph(test_graph, 2)
  call test_graph%add_edge(1, 2)
  allocate(expected(2, 2))
  expected = 1
  expected(1, 2) = 2
  call populated_graph_tests(2, 1, 1, expected)
  deallocate(expected)
  deallocate(connections)

  write(*,*)
  write(*,*) "Test removing a node"
  call test_graph%remove_node(2)
  allocate(expected(2, 2))
  expected(1, 1) = 1
  expected(1, 2) = 0
  call populated_graph_tests(1, 0, 0, expected)
  deallocate(expected)
  deallocate(connections)

  write(*,*)
  write(*,*) "Test cleaning up a graph"
  call Graph(test_graph, 1)
  call test_graph%cleanup()
  call empty_graph_tests()

  write(*,*)
  write(*,*) "Test removing an edge"
  call Graph(test_graph, 2)
  call test_graph%add_edge(1, 2)
  call test_graph%remove_edge(1, 2)
  call populated_graph_tests(2, 0, 0)
  call test_graph%add_edge(1, 2, 3)
  call test_graph%add_edge(1, 2, 5)
  write(*,'(dt)') test_graph

  write(*,*)
  if(failed == 0) then
    write(*,*) 'Passed all tests'
  else
    write(*,'(a,i0,a)') 'Failed ', failed, ' test(s)'
  end if

contains

  subroutine empty_graph_tests()
    write(*, '(4x,a,i0,x,a)') "Number of nodes: ", test_graph%n_nodes(), test_int(test_graph%n_nodes(), 0)
    write(*, '(4x,a,i0,x,a)') "Number of edges: ", test_graph%n_edges(), test_int(test_graph%n_edges(), 0)
    write(*, '(4x,a)', advance='no') "Retrieving a node that doesn't exist: "
    call test_graph%get_node(test_node, 1)
    write(*, '(4x,a)', advance='no') "Removing a node that doesn't exist: "
    call test_graph%remove_node(1)
    write(*, '(4x,a)', advance='no') "Calculating connections: "
    call test_graph%connections(connections)
    write(*, '(4x,a)', advance='no') "Calculating shortest distance: "
    call test_graph%shortest_distance(1, 1, distance)
  end subroutine empty_graph_tests

  subroutine populated_graph_tests(expect_nodes, expect_edges, expect_node1_edges, expect_connections)
    integer, intent(in) :: expect_nodes, expect_edges, expect_node1_edges
    integer, dimension(:, :), intent(in), optional :: expect_connections

    integer :: i

    write(*, '(4x,a,i0,x,a)') "Number of nodes: ", test_graph%n_nodes(), &
        & test_int(test_graph%n_nodes(), expect_nodes)
    write(*, '(4x,a,i0,x,a)') "Number of edges: ", test_graph%n_edges(), &
        & test_int(test_graph%n_edges(), expect_edges)

    write(*, '(4x,a)') "Retrieving a node: "
    call test_graph%get_node(test_node, 1, index=.true.)
    write(*, '(4x,a,i0,a,x,a)') "Node at index 1 has: ", test_node%n_edges(), ' edges', &
        & test_int(test_node%n_edges(), expect_node1_edges)

    if(present(expect_connections)) then
      call test_graph%connections(connections)
      write(*, '(4x,a)') "Calculating connections: "

      do i = 1, size(connections, dim=2)
        write(*,'(6x,2(i0,a,i0))') connections(1, i), ': ', connections(2, i)
      end do
      write(*,'(6x,a)') test_int_2d(connections, expect_connections)
    end if

  end subroutine populated_graph_tests


  function test_int(val, expected) result(pass_str)
    integer, intent(in) :: val, expected
    character(len=8) :: pass_str

    if(val == expected) then
      pass_str = '(Passed)'
    else
      pass_str = '(Failed)'
      failed = failed + 1
    end if

  end function test_int

  function test_int_2d(val, expected) result(pass_str)
    integer, intent(in), dimension(:, :) :: val, expected
    character(len=8) :: pass_str

    if(all(val == expected)) then
      pass_str = '(Passed)'
    else
      pass_str = '(Failed)'
      failed = failed + 1
    end if

  end function test_int_2d

  function test_associated_node(val, other) result(pass_str)
    type(Node), pointer :: val
    type(Node), pointer, optional :: other
    character(len=8) :: pass_str

    type(Node), pointer :: other_

    if (present(other)) then
      if(associated(val, other)) then
        pass_str = '(Passed)'
      else
        pass_str = '(Failed)'
        failed = failed + 1
      end if
    else
      if(associated(val)) then
        pass_str = '(Passed)'
      else
        pass_str = '(Failed)'
        failed = failed + 1
      end if
    end if


  end function test_associated_node

end program graph_test
