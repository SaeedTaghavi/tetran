module blocks
  use, intrinsic:: iso_fortran_env, only: error_unit
  use cinter, only: err
  implicit none
  private :: check_collision
  
    integer :: H,W  ! playfield height, width
    ! Playfield: 0 for blank, 1 for block
    integer, allocatable :: screen(:,:)

    character(*), parameter :: Btypes = 'ITLJSZB'
    
    ! Current x/y of the falling piece
    integer :: cur_x, cur_y
 
    ! Rotation of falling piece
    integer :: cur_rotation = 0
  
    ! Type of falling piece
    ! 0/I: Line, 1/B: Square, 2: T, 3: S, 4: Z, 5: J, 6: L
    character :: cur_type, next_type
  
  public
  ! Stores the shape of the blocks at each of their rotations

  ! LINE BLOCK
  integer, parameter :: line(4,4,0:1) = reshape( &
        [0, 0, 0, 0, &
         1, 1, 1, 1, &
         0, 0, 0, 0, &
         0, 0, 0, 0, &

         0, 0, 1, 0, &
         0, 0, 1, 0, &
         0, 0, 1, 0, &
         0, 0, 1, 0], &
         shape(line))

  ! T BLOCK
  integer, parameter :: tee(4,4,0:3) = reshape( &
        [0, 0, 0, 0, &
         1, 1, 1, 0, &
         0, 1, 0, 0, &
         0, 0, 0, 0, &

         0, 1, 0, 0, &
         1, 1, 0, 0, &
         0, 1, 0, 0, &
         0, 0, 0, 0, &

         0, 1, 0, 0, &
         1, 1, 1, 0, &
         0, 0, 0, 0, &
         0, 0, 0, 0, &

         0, 1, 0, 0, &
         0, 1, 1, 0, &
         0, 1, 0, 0, &
         0, 0, 0, 0], &
         shape(tee))

  ! L BLOCK
  integer, parameter :: ell(4,4,0:3) = reshape( &
        [0, 0, 0, 0, &
         1, 1, 1, 0, &
         1, 0, 0, 0, &
         0, 0, 0, 0, &

         1, 1, 0, 0, &
         0, 1, 0, 0, &
         0, 1, 0, 0, &
         0, 0, 0, 0, &

         0, 0, 0, 0, &
         0, 0, 1, 0, &
         1, 1, 1, 0, &
         0, 0, 0, 0, &

         0, 1, 0, 0, &
         0, 1, 0, 0, &
         0, 1, 1, 0, &
         0, 0, 0, 0], &
         shape(ell))

  ! J BLOCK
  integer, parameter :: jay(4,4,0:3) = reshape( &
        [0, 0, 0, 0, &
         1, 1, 1, 0, &
         0, 0, 1, 0, &
         0, 0, 0, 0, &

         0, 1, 0, 0, &
         0, 1, 0, 0, &
         1, 1, 0, 0, &
         0, 0, 0, 0, &

         0, 0, 0, 0, &
         1, 0, 0, 0, &
         1, 1, 1, 0, &
         0, 0, 0, 0, &

         0, 1, 1, 0, &
         0, 1, 0, 0, &
         0, 1, 0, 0, &
         0, 0, 0, 0], &
         shape(jay))

  ! S BLOCK
  integer, parameter :: ess(4,4,0:1) = reshape( &
        [0, 0, 0, 0, &
         0, 1, 1, 0, &
         1, 1, 0, 0, &
         0, 0, 0, 0, &

         1, 0, 0, 0, &
         1, 1, 0, 0, &
         0, 1, 0, 0, &
         0, 0, 0, 0], &
         shape(ess))

  ! Z BLOCK
  integer, parameter :: zee(4,4,0:1) = reshape( &
        [0, 0, 0, 0, &
         1, 1, 0, 0, &
         0, 1, 1, 0, &
         0, 0, 0, 0, &

         0, 0, 1, 0, &
         0, 1, 1, 0, &
         0, 1, 0, 0, &
         0, 0, 0, 0], &
         shape(zee))

  ! SQUARE BLOCK
  integer, parameter :: square(4,4) = reshape( &
        [0, 1, 1, 0, &
         0, 1, 1, 0, &
         0, 0, 0, 0, &
         0, 0, 0, 0], &
         shape(square))

  integer, parameter :: Ny=size(square,1), Nx=size(square,2)

contains

  ! Mutates rotation
  subroutine get_shape(block_type, rotation, bshape)
  ! FIXME: make object oriented
    integer, intent(out) :: bshape(Ny, Nx)
    integer, intent(inout) :: rotation
    character, intent(in) :: block_type
    character(80) :: errmsg


    select case (block_type)
      case ("I")
        rotation = modulo(rotation, size(line,3))
        bshape = line(:,:,rotation)
      case ("T")
        rotation = modulo(rotation, size(tee,3))
        bshape = tee(:,:,rotation)
      case ("L")
        rotation = modulo(rotation, size(ell,3))
        bshape = ell(:,:,rotation)
      case ("J")
        rotation = modulo(rotation, size(jay,3))
        bshape = jay(:,:,rotation)
      case ("S")
        rotation = modulo(rotation, size(ess,3))
        bshape = ess(:,:,rotation)
      case ("Z")
        rotation = modulo(rotation, size(zee,3))
        bshape = zee(:,:,rotation)
      case ("B")
        bshape = square
        rotation = 0
      case default
        write(errmsg,*) 'unknown shape index: ',block_type
        call err(errmsg)
    end select
  end subroutine get_shape
  
 
  impure elemental subroutine generate_next_type(next_type, Nblock)
    character, intent(out) :: next_type
    integer, intent(inout), optional :: Nblock
    real :: r

    if(present(Nblock)) Nblock = Nblock + 1  ! for game stats

    call random_number(r)

    next_type = int2block(floor(r * len(Btypes)))  ! set this line constant to debug shapes
  end subroutine generate_next_type
  
  
  impure elemental character function int2block(i) result(b)
    integer, intent(in) :: i
    b = Btypes(i+1:i+1)
  end function int2block
  
  
    subroutine move_left()
    integer :: x
    x = cur_x - 1
    if (.not. check_collision(x, cur_y, cur_rotation)) cur_x = cur_x - 1
  end subroutine move_left


  subroutine move_right()
    integer :: x
    x = cur_x + 1
    if (.not. check_collision(x, cur_y, cur_rotation)) cur_x = cur_x + 1
  end subroutine move_right


  logical function move_down() result (landed)
    integer :: y
    
    y = cur_y + 1
    if (.not. check_collision(cur_x, y, cur_rotation)) then
      cur_y = cur_y + 1
      landed = .false.
    else
      landed = .true.
    end if
  end function move_down


  subroutine rotate_piece()
    integer :: r
    r = cur_rotation + 1
    if (.not. check_collision(cur_x, cur_y, r)) cur_rotation = cur_rotation + 1
  end subroutine rotate_piece
  
  
  logical function check_collision(x, y, rotation) result (collided)
    integer, intent(in) :: x, y
    integer, intent(inout) :: rotation

    integer :: block(Ny, Nx)
    integer :: i, j, jx, iy

    collided = .false.
    call get_shape(cur_type, rotation, block)

! neither do loop is "concurrent" because of "exit" statements
    iloop: do i = 1, Ny
      iy = i + y - 2
      
      if (any(block(i,:) == 1) .and. iy >= H) then
      ! piece hit the floor
        collided = .true.
        return
      end if
      
      do j = 1, Nx
        jx = j + x - 2
        if (block(i, j) == 1) then
          ! Handling left/right boundaries
          if (jx < 0 .or. jx >= W) then
            collided = .true.
            return
          end if

          ! Other blocks
          if (iy > 0 .and. iy < H) then
            if (screen(iy + 1, jx + 1) == 1) then
              collided = .true.
              return
            end if
          end if
        end if
      end do
    end do iloop
  end function check_collision

  
  
  subroutine init_random_seed(debug)
    ! NOTE: this subroutine is replaced by "call random_init()" in Fortran 2018
    logical, intent(in), optional :: debug
    integer :: n, u,ios
    integer, allocatable :: seed(:)
    logical :: dbg
    
    character(*), parameter :: randfn = '/dev/urandom'
    
    dbg = .false.
    if (present(debug)) dbg=debug
    
    call random_seed(size=n)
    allocate(seed(n))
    
    open(newunit=u, file=randfn, access="stream", &
                 form="unformatted", action="read", status="old", iostat=ios)
    if (ios/=0) call err('failed to open random source generator file: '//randfn)
    
    read(u,iostat=ios) seed
    if (ios/=0) call err('failed to read random source generator file: '//randfn)
    
    close(u)
    
    call random_seed(put=seed)

    if (dbg) then
      call random_seed(get=seed)
      print *, 'seed:',seed
    endif
  end subroutine

end module blocks
