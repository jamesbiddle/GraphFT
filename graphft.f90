module graphft
  use mathsops
  use kinds
  implicit none
  private

  type Graph
    ! Derived type that specifies a directed graph
    private
    type(NodePtr), allocatable :: nodes(:)
    type(EdgePtr), allocatable :: edges(:)
    integer :: node_idx = 0
    integer :: edge_idx = 0
  contains
    final :: finalise_graph
    procedure :: get_node
    procedure :: n_nodes
    procedure :: n_edges => n_edges_graph
    procedure :: add_node
    procedure :: remove_node
    procedure :: reindex_nodes
    procedure :: connections
    procedure :: shortest_distance
    procedure :: cleanup
    procedure, private :: update_distances
    procedure, private :: get_node_index
    procedure :: delete_graph
    procedure :: add_edge => add_edge_id, add_edge_node
    procedure :: remove_edge
    procedure, private :: remove_edge_ptr
    procedure, private :: get_edge_index => get_edge_index_graph

    generic :: write(formatted) => write_graph
    procedure :: write_graph

    ! Branching-point analysis
    procedure :: bp_distances
    procedure :: output_graph
    procedure, private :: get_distance
    procedure, private :: reset
    procedure, private :: random_bp
  end type Graph


  type Node
    private
    integer :: id
    type(EdgePtr),  dimension(:), allocatable :: edges
    logical :: visited = .false.
    integer :: distance = 0
  contains
    procedure :: get_id
    procedure :: n_edges => n_edges_node
    procedure :: edge_directions
    procedure :: weights
    procedure, private :: get_edge_index => get_edge_index_node
    final :: delete_node

    generic :: write(formatted) => write_node
    procedure :: write_node

    ! Branching point analysis
    procedure :: is_bp
    procedure, private :: random_edge
    procedure, private :: check_visited
  end type Node


  type NodePtr
    type(Node), pointer :: ptr => null()
  end type NodePtr

  type Path
    integer, allocatable :: node_ids(:)
    integer :: length = 0
  end type Path

  type Edge
    private
    type(Node), pointer :: start => null()
    type(Node), pointer :: end => null()
    integer :: weight = 1
    logical :: traversed = .false.
  contains
    generic :: write(formatted) => write_edge
    procedure :: write_edge
    ! Branching point analysis
  end type Edge


  type EdgePtr
    type(Edge), pointer :: ptr => null()
  end type EdgePtr


  ! Interfaces
  interface Graph
    module procedure :: initialise_graph
  end interface Graph

  interface Node
    module procedure :: initialise_node
  end interface Node

  interface Edge
    module procedure :: initialise_edge
  end interface Edge

  ! Exports
  public :: Node, Graph, NodePtr, EdgePtr, Edge

contains

  !!------------------------ Graph Procedures ------------------------!!

  subroutine initialise_graph(this, init_nodes)
    ! Graph constructor
    type(Graph) :: this
    integer, intent(in), optional :: init_nodes
    integer :: i

    integer :: init_nodes_

    ! Clear the graph if it has previously been utilised
    call this%delete_graph()
    this%node_idx = 0
    this%edge_idx = 0

    if(present(init_nodes)) then
      init_nodes_ = init_nodes
    else
      init_nodes_ = 0
    end if

    if(init_nodes_ < 0) then
      write(*,*) "Cannot initialise graph negative nodes"
      call exit()
    end if

    do i = 1, init_nodes_
      call this%add_node()
    end do

  end subroutine initialise_graph


  subroutine get_node(this, result_node, id, index)
    ! Retrieve a node
    ! id is a node id if index is false (default)
    ! Else it is an index in the graph nodes list
    ! Setting index = .true. allows for easy looping over nodes
    class(Graph) :: this
    integer, intent(in) :: id
    logical, optional :: index
    type(Node), pointer :: result_node

    logical :: index_
    integer :: i

    if(present(index)) then
      index_ = index
    else
      index_ = .false.
    end if

    if(index_) then
      if(id > this%n_nodes()) then
        write(*,*) 'Node index larger than the number of nodes, returning'
        result_node => null()
        return
      end if
      result_node => this%nodes(id)%ptr
      return
    else
      do i = 1, this%n_nodes()
        result_node => this%nodes(i)%ptr
        if(result_node%get_id() == id) return
      end do
    end if

    write(*,'(a,i0,a)') 'Node with id ', id, ' not found, returning'
    result_node => null()

  end subroutine get_node


  function get_node_index(this, id) result(idx)
    ! Find the index of the node with the given id
    class(Graph) :: this
    integer, intent(in) :: id
    integer :: idx

    type(Node), pointer :: result_node

    do idx = 1, this%n_nodes()
      result_node => this%nodes(idx)%ptr
      if(result_node%id == id) return
    end do
  end function get_node_index


  function n_nodes(this)
    ! Getter for number of nodes in the graph
    class(Graph), intent(in) :: this
    integer :: n_nodes

    if(allocated(this%nodes)) then
      n_nodes = size(this%nodes)
    else
      n_nodes = 0
    end if

  end function n_nodes


  function n_edges_graph(this) result(n_edges)
    ! Getter for number of edges
    class(Graph), intent(in) :: this
    integer :: n_edges

    if(allocated(this%edges)) then
      n_edges = size(this%edges)
    else
      n_edges = 0
    end if

  end function n_edges_graph


  subroutine add_node(this)
    ! Add an empty node to the graph
    class(Graph) :: this

    type(Node), pointer :: new_node
    type(NodePtr) :: new_node_ptr

    allocate(new_node)

    this%node_idx = this%node_idx + 1
    new_node = Node(this%node_idx)

    new_node_ptr%ptr => new_node
    if(allocated(this%nodes)) then
      this%nodes = [this%nodes, new_node_ptr]
    else
      this%nodes = [new_node_ptr]
    end if


  end subroutine add_node


  subroutine remove_node(this, id)
    ! Remove a node from the graph
    ! Achieved by deallocating references to the node
    class(Graph) :: this
    integer, intent(in) :: id
    type(Node), pointer :: node, target_node

    type(NodePtr), allocatable :: temp_nodes(:)
    type(EdgePtr), allocatable :: temp_edges(:)
    logical, allocatable :: mask(:)
    type(Edge), pointer :: edge
    integer :: n_edges, idx
    integer :: i, j

    call this%get_node(target_node, id)
    if( .not. associated(target_node)) then
      return
    end if

    ! Remove the node from this%nodes and deallocate it
    idx = this%get_node_index(id)
    allocate(mask(this%n_nodes()))
    mask = .true.
    mask(idx) = .false.
    call move_alloc(this%nodes, temp_nodes)
    this%nodes = pack(temp_nodes, mask)
    deallocate(temp_nodes)

    ! Remove any edges referencing the removed node
    ! Don't deallocate yet as we still need to remove from this%edges
    do i = 1, this%n_nodes()
      node => this%nodes(i)%ptr

      if(node%n_edges() > 0) then
        deallocate(mask)
        allocate(mask(node%n_edges()))
        mask = .true.

        do j = 1, node%n_edges()
          edge => node%edges(j)%ptr
          if((edge%start%id == id) .or. (edge%end%id == id)) then
            mask(j) = .false.
          end if
        end do
        call move_alloc(node%edges, temp_edges)
        node%edges = pack(temp_edges, mask)
      end if

    end do

    ! Remove any edges referencing the node and deallocate them
    if(this%n_edges() > 0) then
      deallocate(mask)
      allocate(mask(this%n_edges()))
      mask = .true.

      do i = 1, this%n_edges()
        edge => this%edges(i)%ptr
        if((edge%start%id == id) .or. (edge%end%id == id))then
          mask(i) = .false.
          deallocate(edge)
        end if
      end do
      call move_alloc(this%edges, temp_edges)
      this%edges = pack(temp_edges, mask)
    end if

    deallocate(target_node)
  end subroutine remove_node


  subroutine reindex_nodes(this)
    ! Reindex the node indices
    class(Graph) :: this

    integer :: i

    do i = 1, this%n_nodes()
      this%nodes(i)%ptr%id = i
    end do

    this%node_idx = this%n_nodes()

  end subroutine reindex_nodes

  subroutine add_edge_node(this, start_node, end_node, weight)
    ! Add edge to the graph by explicitly providing nodes
    class(Graph) :: this
    type(Node), pointer :: start_node, end_node
    integer, optional :: weight

    type(Edge), pointer :: new_edge
    type(EdgePtr) :: new_edge_ptr

    allocate(new_edge)

    if(present(weight)) then
      new_edge = Edge(start_node, end_node, weight)
    else
      new_edge = Edge(start_node, end_node)
    end if

    new_edge_ptr%ptr => new_edge

    start_node%edges = [start_node%edges, new_edge_ptr]
    end_node%edges = [end_node%edges, new_edge_ptr]
    if(allocated(this%edges)) then
      this%edges = [this%edges, new_edge_ptr]
    else
      this%edges = [new_edge_ptr]
    end if

  end subroutine add_edge_node


  subroutine add_edge_id(this, start_id, end_id, weight)
    ! Add edge to the graph by the node indices
    class(Graph) :: this
    integer, intent(in) :: start_id, end_id
    integer, optional :: weight

    type(Node), pointer :: start_node, end_node

    call this%get_node(start_node, start_id)
    call this%get_node(end_node, end_id)

    if(present(weight)) then
      call add_edge_node(this, start_node, end_node, weight)
    else
      call add_edge_node(this, start_node, end_node)
    end if
  end subroutine add_edge_id


  function get_edge_index_graph(this, find_edge) result(idx)
    ! Find a specified edge's index in the edge array
    class(Graph) :: this
    type(Edge), pointer :: find_edge
    integer :: idx

    integer :: i

    do i = 1, this%n_edges()
      if(associated(this%edges(i)%ptr, find_edge)) then
        idx = i
        return
      end if
    end do

  end function get_edge_index_graph


  subroutine remove_edge_ptr(this, del_edge)
    ! Remove a specified edge from the graph
    class(Graph) :: this
    type(Edge), pointer :: del_edge

    ! type(NodePtr), allocatable :: temp_nodes(:)
    type(EdgePtr), allocatable :: temp_edges(:)
    type(Node), pointer :: this_node
    logical, allocatable :: mask(:)
    type(Edge), pointer :: edge_ptr
    integer :: n_edges, idx
    integer :: i, j

    edge_ptr => del_edge

    ! Remove the edge from this%edges
    idx = this%get_edge_index(del_edge)
    allocate(mask(this%n_edges()))
    mask = .true.
    mask(idx) = .false.
    call move_alloc(this%edges, temp_edges)
    this%edges = pack(temp_edges, mask)
    deallocate(temp_edges)
    deallocate(mask)

    do i = 1, this%n_nodes()
      this_node => this%nodes(i)%ptr
      ! See if this node connects to this edge
      idx = this_node%get_edge_index(del_edge)
      if(idx == -1) cycle

      allocate(mask(this_node%n_edges()))
      mask = .true.
      mask(idx) = .false.
      call move_alloc(this_node%edges, temp_edges)
      this_node%edges = pack(temp_edges, mask)
      deallocate(temp_edges)
      deallocate(mask)
    end do

    deallocate(del_edge)

  end subroutine remove_edge_ptr


  subroutine remove_edge(this, start_id, end_id, weight)
    ! Remove the first edge encountered between nodes with start_id and end_id
    ! Optionally supply a weight to remove a specific edge in the case that multiple exist
    class(Graph) :: this
    integer, intent(in) :: start_id, end_id
    integer, intent(in), optional :: weight

    type(Node), pointer :: start_node, end_node
    type(Edge), pointer :: this_edge
    integer :: i

    call this%get_node(start_node, start_id)
    call this%get_node(end_node, end_id)

    do i = 1, start_node%n_edges()
      this_edge => start_node%edges(i)%ptr
      if(associated(this_edge%end, end_node)) then
        if(present(weight)) then
          if(this_edge%weight == weight) then
            call this%remove_edge_ptr(this_edge)
            return
          end if
        else
          call this%remove_edge_ptr(this_edge)
          return
        end if
      end if
    end do

  end subroutine remove_edge


  subroutine cleanup(this, reindex)
    ! Remove all nodes with no edges from the graph
    ! Optionally reindex the graph
    class(Graph) :: this
    logical, optional :: reindex

    type(Node), pointer :: this_node
    logical :: reindex_
    integer :: counter

    if(present(reindex)) then
      reindex_ = reindex
    else
      reindex_ = .false.
    end if

    counter = 1
    do while(.true.)
      this_node => this%nodes(counter)%ptr
      if(this_node%n_edges() == 0) then
        call this%remove_node(this_node%id)
      else
        counter = counter + 1
      end if

      if(counter > this%n_nodes()) exit
    end do

  end subroutine cleanup


  subroutine reset(this)
    ! Reset the visited and distance variables on the nodes
    ! Reset the traversed flag on the edge
    class(Graph) :: this
    integer :: i
    type(Node), pointer :: this_node
    type(Edge), pointer :: this_edge

    do i = 1, this%n_nodes()
      this_node => this%nodes(i)%ptr
      this_node%visited = .false.
      this_node%distance = 0
    end do

    do i = 1, this%n_edges()
      this_edge => this%edges(i)%ptr
      this_edge%traversed = .false.
    end do

  end subroutine reset


  subroutine connections(this, connections_arr)
    ! Generate a rank 2 array specifiying the number of connections from each node
    ! connections_arr(1,:) contains the node id's
    ! connections_arr(2,:) contains the corresponding counts
    class(Graph) :: this
    integer, allocatable :: connections_arr(:, :)

    type(Node), pointer :: this_node
    integer :: i

    if(this%n_nodes() == 0) then
      write(*,*) 'No nodes in graph, returning'
      return
    end if

    if(allocated(connections_arr)) deallocate(connections_arr)
    allocate(connections_arr(2, this%n_nodes()))

    do i = 1, this%n_nodes()
      this_node => this%nodes(i)%ptr
      connections_arr(1, i) = this_node%id
      connections_arr(2, i) = this_node%n_edges()
    end do

  end subroutine connections


  subroutine shortest_distance(this, id_start, id_end, d, index)
    ! Utilise djikstra's algorithm to determine the shortest distance, d, between two nodes
    ! index=.false. (default) denotes that the graph id's are supplied.
    ! index=.true. indicates that they are indices in the graph's node array
    class(Graph) :: this
    integer, intent(in) :: id_start, id_end
    integer, intent(out):: d
    logical, optional :: index

    type(Node), pointer :: start_node, target_node
    logical :: index_
    integer :: i

    if(present(index)) then
      index_ = index
    else
      index_ = .false.
    end if

    call this%reset()

    ! Set the initial sitances to infinity
    do i = 1, this%n_nodes()
      call this%get_node(target_node, i, index=.true.)
      target_node%distance = huge(i)
    end do

    ! Locate the start node
    call this%get_node(start_node, id_start, index=index_)
    if( .not. associated(start_node)) return
    start_node%distance = 0

    ! Locate the end node
    call this%get_node(target_node, id_end, index=index_)
    if( .not. associated(target_node)) return

    ! Calculate the distance
    call this%update_distances(start_node, target_node)
    d = target_node%distance

    ! If there is no path between the two nodes, return -1
    if(d == huge(i)) d = -1
    call this%reset()

  end subroutine shortest_distance


  recursive subroutine update_distances(this, current_node, target_node)
    ! Recursive implementation of Djikstra's algorithm
    class(Graph) :: this
    type(Node), pointer :: current_node, target_node

    type(Edge), pointer :: this_edge
    type(Node), pointer :: neighbour, next_node
    integer, allocatable :: directions(:)
    integer :: temp_dist
    integer :: i, c_idx, n_idx

    if(current_node%n_edges() == 0) return
    if (allocated(directions)) deallocate(directions)

    call current_node%edge_directions(directions)

    do i = 1, current_node%n_edges()
      ! Only consider outgoing edges
      if(directions(i) == 1) then
        this_edge => current_node%edges(i)%ptr
        neighbour => this_edge%end
        if(neighbour%visited) cycle

        temp_dist = current_node%distance + this_edge%weight

        if(temp_dist < neighbour%distance) neighbour%distance = temp_dist


      end if
    end do

    current_node%visited = .true.

    ! If the target node is visited then we're done
    if(target_node%visited) then
      return
    else
      ! Find the unvisited node with the shortest distance
      temp_dist = huge(i)
      next_node => null()
      do i = 1, this%n_nodes()
        call this%get_node(neighbour, i, index=.true.)
        if((neighbour%distance < temp_dist).and.( .not. neighbour%visited)) then
          next_node => neighbour
          temp_dist = neighbour%distance
        end if
      end do
      ! If all distances on unvisited nodes are infinite, return
      if( .not. associated(next_node)) return
      ! Else update the distances with the next node
      call this%update_distances(next_node, target_node)
    end if

  end subroutine update_distances


  subroutine bp_distances(this, dists)
    class(Graph), intent(in) :: this
    integer, allocatable :: dists(:)

    type(Node), pointer :: this_node, start_bp
    type(Edge), pointer :: this_edge
    type(Path), allocatable :: paths(:)
    type(Path) :: this_path
    integer, allocatable :: directions(:)
    integer, allocatable :: temp_dists(:)
    integer :: dist
    integer :: i

    if(allocated(dists)) deallocate(dists)

    ! Reset any flag variables
    call this%reset()

    ! Flag nodes with only incoming edges as visited
    do i = 1, this%n_nodes()
      this_node => this%nodes(i)%ptr
      call this_node%edge_directions(directions)
      if(all(directions == 0)) this_node%visited = .true.
    end do

    do while(.true.)
      ! Pick a random BP node
      call this%random_bp(start_bp)
      ! If no unvisited branching point can be found, we're done
      if( .not. associated(start_bp)) exit

      ! Calculate the distance from this BP
      call this%get_distance(start_bp, this_path)

      ! Store the path
      paths = [paths, this_path]
      ! Cleanup the temporary path
      deallocate(this_path%node_ids)
      this_path%length = 0
    end do

    ! If no paths were found, return
    if( .not. allocated(paths)) return

    do i = 1, size(paths)
      this_path = paths(i)
      call fix_cycles()
      paths(i) = this_path
    end do

    dists = paths%length

    if(allocated(dists)) then
      do i = 1, this%n_edges()
        this_edge => this%edges(i)%ptr
        if( .not. this_edge%traversed) then
          write(*,*) "Edge not traversed"
          write(*,*) this_edge
          call exit(1)
        end if
      end do
    end if

    call this%reset()

  contains

    subroutine fix_cycles()
      ! Scan a path for untraversed edges
      ! If any our found, follow them and add their lengths to the length of the path
      type(Node), pointer :: this_node
      type(Edge), pointer :: this_edge
      integer :: i, id

      do i = 1, size(this_path%node_ids)
        id = this_path%node_ids(i)
        call this%get_node(this_node, id)
        call dfs(this_node)
      end do

    end subroutine fix_cycles

    recursive subroutine dfs(this_node)
      ! Use a depth-first search to locate cycles and add them to the path
      type(Node), pointer :: this_node

      type(Edge), pointer :: this_edge
      integer :: i

      do i = 1, this_node%n_edges()
        this_edge => this_node%edges(i)%ptr
        ! If the edge is outgoing and not traversed, follow it
        if(( .not. this_edge%traversed) .and. (associated(this_edge%start, this_node))) then
          this_path%length = this_path%length + this_edge%weight
          this_edge%traversed = .true.
          ! If the edge goes to another node, recursively call this function
          if(( .not. associated(this_edge%end, this_node))) call dfs(this_edge%end)
        end if
      end do

    end subroutine dfs

  end subroutine bp_distances


  recursive subroutine get_distance(this, start_node, this_path)
    class(Graph), intent(in) :: this
    type(Node), pointer, intent(in):: start_node
    ! integer, intent(out) :: dist
    type(Path) :: this_path

    type(Node), pointer :: next_node
    type(Edge), pointer :: next_edge

    if(allocated(this_path%node_ids)) then
      this_path%node_ids = [this_path%node_ids, start_node%id]
    else
      this_path%node_ids = [start_node%id]
    end if

    ! Find the next edge to follow
    call start_node%random_edge(next_edge)
    ! If there isn't one, mark the node as visited and return
    if( .not. associated(next_edge)) then
      start_node%visited = .true.
      return
    end if

    ! Add the distance between the nodes and mark the edge as traversed
    ! dist = dist + next_edge%weight
    this_path%length = this_path%length + next_edge%weight
    next_edge%traversed = .true.

    ! Check if the start node has now been visited (i.e. all edges are traversed)
    call start_node%check_visited()

    ! Select the next node
    next_node => next_edge%end

    ! If the next node is a branching point, check if it is visited and return
    if(next_node%is_bp()) then
      call next_node%check_visited()
      return
    else
      ! Else continue on
      call this%get_distance(next_node, this_path)
    end if

  end subroutine get_distance

  
  subroutine random_bp(this, bp)
    ! Select a random branching point that has not been visited
    class(Graph), intent(in) :: this
    type(Node), pointer :: bp

    logical, allocatable :: mask(:)
    integer :: rand_idx, i

    allocate(mask(this%n_nodes()))

    mask = .false.
    do i = 1, this%n_nodes()
      call this%get_node(bp, i, index=.true.)
      if(bp%is_bp() .and. (.not. bp%visited)) mask(i) = .true.
    end do

    rand_idx = random_index(mask)
    if(rand_idx == -1) then
      nullify(bp)
    else
      call this%get_node(bp, rand_idx, index=.true.)
    end if
  end subroutine random_bp


  subroutine output_graph(this, unit, x, key)
    ! Output graph as a file suitable for being read into python
    class(Graph) :: this
    integer, intent(in) :: unit
    integer, intent(in), optional :: x ! Slice number
    integer, intent(in), optional :: key ! Used to distinguish separate graphs in the same slice

    ! Local vars
    type(Node), pointer :: this_node
    type(Edge), pointer :: this_edge
    integer, allocatable :: directions(:)
    integer :: i, j, dist, this_id, neighbour_id
    character(len=1) :: dir
    integer :: x_, key_

    call this%reset()

    if(present(key)) then
      key_ = key
    else
      key_ = 1
    end if

    if(present(x)) then
      x_ = x
    else
      x_ = 1
    end if

    do i = 1, this%n_nodes()
      this_node => this%nodes(i)%ptr
      call this_node%edge_directions(directions)
      do j = 1, this_node%n_edges()
        ! Only save outgoing edges or self-loops
        this_edge => this_node%edges(j)%ptr
        ! Check for traversal so that self-loops are only output once
        if((directions(j) .ne. 0) .and. ( .not. this_edge%traversed)) then
          write(unit, '(5(i8,x))') this_edge%start%id, this_edge%end%id, this_edge%weight, x_, key_
          this_edge%traversed = .true.
        end if
      end do
    end do

    call this%reset()
  end subroutine output_graph


  subroutine write_graph(this, unit, iotype, v_list, iostat, iomsg)
    class(Graph), intent(in) :: this     ! Object to write.
    integer, intent(in) :: unit         ! Internal unit to write to.
    character(*), intent(in) :: iotype  ! LISTDIRECTED or DTxxx
    integer, intent(in) :: v_list(:)    ! parameters from fmt spec.
    integer, intent(out) :: iostat      ! non zero on error, etc.
    character(*), intent(inout) :: iomsg  ! define if iostat non zero.

    ! Local vars
    type(Node), pointer :: this_node
    character(len=256) :: output
    integer :: i

    do i = 1, this%n_nodes()
      call this%get_node(this_node, i, index=.true.)
      write (unit, "(dt)", IOSTAT=iostat, IOMSG=iomsg) this_node
    end do

  end subroutine write_graph



  subroutine delete_graph(this)
    ! Graph destructor that can be called manually
    class(Graph) :: this
    integer :: i

    do i = 1, this%n_nodes()
      deallocate(this%nodes(i)%ptr)
    end do

    do i = 1, this%n_edges()
      deallocate(this%edges(i)%ptr)
    end do

    if(allocated(this%nodes)) then
      deallocate(this%nodes)
    end if

    if(allocated(this%edges))then
      deallocate(this%edges)
    end if

  end subroutine delete_graph


  subroutine finalise_graph(this)
    ! Graph destructor
    type(Graph) :: this
    integer :: i

    do i = 1, this%n_nodes()
      deallocate(this%nodes(i)%ptr)
    end do

    do i = 1, this%n_edges()
      deallocate(this%edges(i)%ptr)
    end do

    if(allocated(this%nodes)) then
      deallocate(this%nodes)
    end if

    if(allocated(this%edges))then
      deallocate(this%edges)
    end if

  end subroutine finalise_graph


  !!------------------------ Node procedures -------------------------!!

  function initialise_node(id) result(this)
    ! Node constructor
    type(Node) :: this
    integer, intent(in) :: id

    this%distance = 0
    this%visited = .false.
    this%id = id

  end function initialise_node


  function get_id(this) result(id)
    ! Getter for the node id
    class(Node), intent(in) :: this
    integer :: id

    id = this%id

  end function get_id


  function n_edges_node(this) result(n_edges)
    ! Get the number of nodes in the graph
    class(Node), intent(in) :: this
    integer :: n_edges

    if(allocated(this%edges)) then
      n_edges = size(this%edges)
    else
      n_edges = 0
    end if

  end function n_edges_node


  function get_edge_index_node(this, find_edge) result(idx)
    ! Find a specified edge's index in the edge array
    class(Node) :: this
    type(Edge), pointer :: find_edge
    integer :: idx

    integer :: i

    ! Return -1 if the edge isn't found
    idx = -1

    do i = 1, this%n_edges()
      if(associated(this%edges(i)%ptr, find_edge)) then
        idx = i
        return
      end if
    end do

  end function get_edge_index_node


  subroutine edge_directions(this, directions)
    ! Return an array identifying the direction of each edge connected to this node
    ! 0 indicates incoming edges, 1 indicates outgoing, 2 indicates a self-connection
    class(Node), intent(in), target :: this
    integer, allocatable :: directions(:)

    type(Edge), pointer :: this_edge
    integer :: i

    if(allocated(directions)) deallocate(directions)

    allocate(directions(this%n_edges()))

    do i = 1, this%n_edges()
      this_edge => this%edges(i)%ptr
      if(associated(this_edge%start, this) .and. associated(this_edge%end, this)) then
        directions(i) = 2
      else if(associated(this_edge%start, this)) then
        directions(i) = 1
      else if(associated(this_edge%end, this)) then
        directions(i) = 0
      end if
    end do

  end subroutine edge_directions


  subroutine weights(this, weight_arr)
    ! Retrieve the weights of the edges connected to this node
    class(Node) :: this
    integer, allocatable :: weight_arr(:)

    integer :: i

    if(allocated(weight_arr)) deallocate(weight_arr)
    allocate(weight_arr(this%n_edges()))

    do i = 1, this%n_edges()
      weight_arr(i) = this%edges(i)%ptr%weight
    end do

  end subroutine weights


  subroutine write_node(this, unit, iotype, v_list, iostat, iomsg)
    class(Node), intent(in) :: this     ! Object to write.
    integer, intent(in) :: unit         ! Internal unit to write to.
    character(*), intent(in) :: iotype  ! LISTDIRECTED or DTxxx
    integer, intent(in) :: v_list(:)    ! parameters from fmt spec.
    integer, intent(out) :: iostat      ! non zero on error, etc.
    character(*), intent(inout) :: iomsg  ! define if iostat non zero.

    ! Local vars
    character(len=20) :: substr
    character(len=:), allocatable :: output
    integer :: i

    write(substr, '(a,i0,a)') 'Node: ', this%id, NEW_LINE('a')
    output = trim(substr)
    do i = 1, this%n_edges()
      if(this%edges(i)%ptr%end%id == this%id) then
        write(substr, '(i0,a,i0,a,i0,a)') this%id, ' <-', this%edges(i)%ptr%weight, '- ', &
            this%edges(i)%ptr%start%id, NEW_LINE('a')
      else
        write(substr, '(i0,a,i0,a,i0,a)') this%id, ' -', this%edges(i)%ptr%weight, '-> ', &
            this%edges(i)%ptr%end%id, NEW_LINE('a')
      end if
      output = output//trim(substr)
    end do

    write (unit, "(a)", IOSTAT=iostat, IOMSG=iomsg) output
  end subroutine write_node


  logical function is_bp(this)
    class(Node) :: this
    integer :: edge_count
    edge_count = this%n_edges()
    is_bp = .false.
    if((edge_count == 3) &
        .or. (edge_count == 5) &
        .or. (edge_count == 6)) is_bp = .true.
  end function is_bp


  subroutine random_edge(this, chosen_edge)
    ! Choose a random outgoing edge from this node
    class(Node), target :: this
    type(Edge), pointer :: chosen_edge

    logical, allocatable :: mask(:)
    type(Edge), pointer :: this_edge
    integer :: edge_count, rand_idx
    integer :: i

    edge_count = this%n_edges()
    allocate(mask(edge_count))

    mask = .false.
    do i = 1, edge_count
      this_edge => this%edges(i)%ptr
      ! If the edge is traversed, skip it
      if(this_edge%traversed) cycle
      ! If the edge does not start at this node, skip it
      ! Note that due to self-loops, this is not equivalent to the edge ending at this node
      if( .not. associated(this_edge%start, this)) cycle
      mask(i) = .true.
    end do

    ! From the valid edges, choose one
    rand_idx = random_index(mask)
    if(rand_idx == -1) then
      chosen_edge => null()
    else
      chosen_edge => this%edges(rand_idx)%ptr
    end if

  end subroutine random_edge


  subroutine check_visited(this)
    ! Check if all edges from this node have been traversed
    ! Mark it as visited if they have.
    ! Note that if not all edges have been traversed but the node is marked as visited,
    ! the node will remain marked as visited.
    class(Node), target :: this

    logical :: visited
    type(Edge), pointer :: this_edge
    integer :: i

    visited = .true.
    do i = 1, this%n_edges()
      this_edge => this%edges(i)%ptr
      if( .not. this_edge%traversed) then
        visited = .false.
        return
      end if
    end do

    if( .not. this%visited) this%visited = visited

  end subroutine check_visited


  elemental subroutine delete_node(this)
    ! Node destructor
    type(Node), intent(inout) :: this

    if(allocated(this%edges)) deallocate(this%edges)

  end subroutine delete_node


  !!------------------------ Edge Procedures -------------------------!!

  function initialise_edge(start_node, end_node, weight) result(this)
    ! Edge constructor
    type(Edge) :: this
    type(Node), intent(in), target :: start_node, end_node
    integer, optional, intent(in) :: weight

    this%start => start_node
    this%end => end_node

    if(present(weight)) then
      this%weight = weight
    else
      this%weight = 1
    end if
    this%traversed = .false.

  end function initialise_edge


  subroutine write_edge(this, unit, iotype, v_list, iostat, iomsg)
    class(Edge), intent(in) :: this     ! Object to write.
    integer, intent(in) :: unit         ! Internal unit to write to.
    character(*), intent(in) :: iotype  ! LISTDIRECTED or DTxxx
    integer, intent(in) :: v_list(:)    ! parameters from fmt spec.
    integer, intent(out) :: iostat      ! non zero on error, etc.
    character(*), intent(inout) :: iomsg  ! define if iostat non zero.

    ! Local vars
    character(len=256) :: output

    write(output, '(i0,a,i0,a,i0)') this%start%id, ' -', this%weight, '-> ', this%end%id

    write (unit, "(a)", IOSTAT=iostat, IOMSG=iomsg) trim(output)
  end subroutine write_edge

end module graphft
