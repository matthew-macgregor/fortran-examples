module hunt_the_wumpus
  implicit none
  private
  ! Multi-dim array for tracking the exits for each room
  integer, dimension (20,3) :: cave
  ! Flattened array, each triplet group are the exits for a room
  ! Used to initialize the cave() array
  integer, dimension (60), parameter :: cave_data = &
    (/ &
    ! Map out the connections from each room:
       2,    5,    8,   &   ! Cx Room  1
       1,    3,   10,   &   ! Cx Room  2
       2,    4,   12,   &   ! Cx Room  3
       3,    5,   14,   &   ! Cx Room  4
       1,    4,    6,   &   ! Cx Room  5
       5,    7,   15,   &   ! Cx Room  6
       6,    8,   17,   &   ! Cx Room  7
       1,    7,    9,   &   ! Cx Room  8
       8,   10,   18,   &   ! Cx Room  9
       2,    9,   11,   &   ! Cx Room 10
      10,   12,   19,   &   ! Cx Room 11
       3,   11,   13,   &   ! Cx Room 12
      12,   14,   20,   &   ! Cx Room 13
       4,   13,   15,   &   ! Cx Room 14
       6,   14,   16,   &   ! Cx Room 15
      15,   17,   20,   &   ! Cx Room 16
       7,   16,   18,   &   ! Cx Room 17
       9,   17,   19,   &   ! Cx Room 18
      11,   18,   20,   &   ! Cx Room 19
      13,   16,   19    &   ! Cx Room 20
    /)

    integer, parameter :: arrow_start_total = 5
    ! Keep track of the hero and hazards, and their original locations as well
    integer, dimension (6) :: inhabit_locs, orig_locs

    ! Entity: hero or hazards
    enum, bind(c)
      enumerator :: Entity = 0 ! enum name
      enumerator :: You = 1, Wumpus = 2, Pit = 3, Bat = 4
    end enum

    ! Commands
    enum, bind(c)
      enumerator :: Command = 0 ! enum name
      enumerator :: Shoot, Move
    end enum

    ! Game States
    enum, bind(c)
      enumerator :: WinState = 0 ! enum name
      enumerator :: WSNewGame = 1
      enumerator :: WSContinue = 0, WSBumpedWumpus, WSYouGotWumpus, WSArrowGotYou, WSWumpusGotYou, WSFellInPit, WSGameOver
    end enum

    ! Confirmation: yes, no, unknown
    enum, bind(c)
      enumerator :: CnfConfirmation
      enumerator :: CnfNo = 0, CnfYes = 1, CnfUnknown
    end enum

    type t_hero
      integer(kind(WinState)) :: win_state = WSNewGame
      integer :: arrows
    end type t_hero

    type(t_hero) :: hero
    character*1 :: is_debug

  public :: run_game
contains
  ! Converts an integer to a hazard type enum
  integer(kind(Entity)) function get_hazard_type(hazard) result(h)
    integer, intent (in) :: hazard
    select case (hazard)
      case (1)
        h = You
      case (2)
        h = Wumpus
      case (3:4)
        h = Pit
      case (5:6)
        h = Bat
    end select
  end function

  ! Prompts the user for a yes-no response and returns an enum
  integer(kind(CnfConfirmation)) function get_yn(prompt)
    character(len = *), intent(in) :: prompt
    character answer*1
    do
      write(*, fmt="(1x,a)", advance="no") prompt // " "
      read *, answer
      if ((answer == 'n') .or. (answer == 'N')) then
        get_yn = CnfNo
        exit
      else if ((answer == 'y') .or. (answer == 'Y')) then
        get_yn = CnfYes
        exit
      end if
    end do
  end function

  ! Random number generator
  integer function get_rand(min, max) result(rint)
    integer, intent (in) :: min, max
    real                 :: rx = 0.0
    integer              :: rand_seed

    call random_seed(size = rand_seed)
    call random_number(rx)
    ! write (*,'(a,i4)')   'random (int): ', int((rx * max) + min)
    rint = int((rx * max) + min)
  end function

  ! Prompts the user to pick a command (shoot-move)
  integer(kind(Command)) function get_command() result(cmd)
    character answer*1
    cmd = Command
    do while (cmd == Command)
      write(*, fmt="(1x,a)", advance="no") "SHOOT OR MOVE (S-M) "
      read *, answer
      if ((answer == 's') .or. (answer == 'S')) then
        cmd = Shoot
      else if ((answer == 'm') .or. (answer == 'M')) then
        cmd = Move
      end if
    end do
  end function

  subroutine check_quit
    integer(kind(Command)) :: yn
    yn = get_yn("QUIT (Y-N)")
    if (yn == CnfYes) then
      call exit(0)
    end if
  end subroutine

  subroutine ask_instructions
    integer(kind(CnfConfirmation)) :: yn
    yn = get_yn("INSTRUCTIONS (Y-N)")
    if (yn == CnfNo) return
    print *, "***************************************************"
    print *, "          WELCOME TO 'HUNT THE WUMPUS'"
    print *, "***************************************************"
    print *, "  THE WUMPUS LIVES IN A CAVE OF 20 ROOMS. EACH ROOM"
    print *, "HAS 3 TUNNELS LEADING TO OTHER ROOMS. (LOOK AT A"
    print *, "DODECAHEDRON TO SEE HOW THIS WORKS-IF YOU DON'T KNOW"
    print *, "WHAT A DODECAHEDRON IS, ASK SOMEONE)"
    print *, ""
    print *, "    HAZARDS:"
    print *, " BOTTOMLESS PITS - TWO ROOMS HAVE BOTTOMLESS PITS"
    print *, "    IF YOU GO THERE< YOU FALL INTO THE PIT (& LOSE!)"
    print *, " SUPER BATS - TWO OTHER ROOMS HAVE SUPER BATS. IF YOU"
    print *, "    GO THERE, A BAT GRABS YOU AND TAKES YOU TO ANOTHER"
    print *, "    ROOM AT RANDOM. (WHICH MAY BE TROUBLESOME)"
    write(*, fmt="(1x,a)", advance="no") "HIT RETURN TO CONTINUE "
    read(*,*)
    print *, "    WUMPUS:"
    print *, "THE WUMPUS IS NOT BOTHERED BY HAZARDS (HE HAS SUCKER"
    print *, "FEET AND IS TOO BIG FOR A BAT TO LIFT). USUALLY"
    print *, "HE IS ASLEEP.  TWO THINGS WAKE HIM UP: YOU SHOOTING AN"
    print *, "ARROW OR YOU ENTERING HIS ROOM."
    print *, "    IF THE WUMPUS WAKES HE MOVES (P=.75) ONE ROOM"
    print *, "OR STAYS STILL (P=.25).  AFTER THAT, IF HE IS WHERE YOU"
    print *, "ARE, HE EATS YOU UP AND YOU LOSE!"
    print *, ""
    print *, "    YOU:"
    print *, "EACH TURN YOU MAY MOVE OR SHOOT A CROOKED ARROW"
    print *, "  MOVING:  YOU CAN MOVE ONE ROOM (THRU ONE TUNNEL)"
    print *, "  ARROWS:  YOU HAVE 5 ARROWS. YOU LOSE WHEN YOU RUN OUT"
    print *, "  EACH ARROW CAN GO FROM 1 TO 5 ROOMS. YOU AIM BY TELLING"
    print *, "  THE COMPUTER THE ROOM#S YOU WANT THE ARROW TO GO TO."
    print *, "  IF THE ARROW CAN'T GO THAT WAY (IF NO TUNNEL) IT MOVES"
    print *, "  AT RANDOM TO THE NEXT ROOM."
    print *, "    IF THE ARROW HITS THE WUMPUS, YOU WIN."
    print *, "    IF THE ARROW HITS YOU, YOU LOSE."
    write(*, fmt="(1x,a)", advance="no") "HIT RETURN TO CONTINUE"
    read(*,*)
    print *, "    WARNINGS:"
    print *, "     WHEN YOU ARE ONE ROOM AWAY FROM A WUMPUS OR HAZARD,"
    print *, "     THE COMPUTER SAYS:"
    print *, " WUMPUS:   'I SMELL A WUMPUS'"
    print *, " BAT   :   'BATS NEARBY'"
    print *, " PIT   :   'I FEEL A DRAFT'"
    print *, ""
  end subroutine ask_instructions

  ! Sets up the cave from scratch
  subroutine init_cave
    integer :: j, k, l
    l = 1
    do j = 1, 20
      do k = 1, 3
        cave(j,k) = cave_data(l)
        l = l + 1
      end do
    end do

    j = get_rand(min=1, max=10)
  end subroutine init_cave

  ! Sets up the location of inhabitants at random
  subroutine init_inhabitants
    integer :: j, k, r
    integer, dimension (6) :: used = 0
    do j = 1, 6
      100   r = get_rand(1, 20)
      ! Check for duplicates
      do k = 1, 6
        if (used(k) == r) then
          goto 100  ! goto considered harmful, lol
        end if
      end do
      ! Randomly place each inhabitant
      inhabit_locs(j) = r
      ! Save the original locations
      orig_locs(j) = inhabit_locs(j)
      used(j) = inhabit_locs(j)
    end do
  end subroutine init_inhabitants

  ! Sets up the hero
  subroutine init_hero
    hero%arrows = arrow_start_total
    hero%win_state = WSContinue
  end subroutine

  ! Initializes the game state from scratch
  subroutine init_game
    call init_cave
    call init_inhabitants
    call init_hero
  end subroutine init_game

  ! Resets the game to the previous original state
  subroutine reset_game
    integer :: i
    do i = 1, 6
      inhabit_locs(i) = orig_locs(i)
    end do
    call init_hero
  end subroutine reset_game

  ! Interactive subroutine to prompt the user whether
  ! they want to replay or start from scratch
  subroutine setup_game
    integer(kind(CnfConfirmation)) :: yn

    if (is_debug .ne. '1') then
      print *, achar(27)//"[2J" ! clear screen
    end if

    if (hero%win_state /= WSNewGame) then
      yn = get_yn("SAME SETUP (Y-N)")
    else
      yn = CnfNo ! Reset Game
    end if

    select case (yn)
      case (CnfYes)
        call reset_game
      case (CnfNo)
        call init_game
    end select
  end subroutine setup_game

  ! Tell the user about nearby hazards
  subroutine issue_hazard_warnings
    integer :: j, k
    integer(kind(Entity)) :: hazard
    ! Print location & hazard warnings
    print *, ""
    ! Hazards: 2 = wumpus, 3 & 4 = pits, 5 & 6 = bats
    do j = 2, 6
      ! Exits from this room are 1 to 3
      do k = 1, 3
        ! print *, "Checking ...", k, cave(inhabit_locs(You), k), j, inhabit_locs(j)
        ! If there's not a hazard in any of the adjoining rooms...
        if (cave(inhabit_locs(You), k) /= inhabit_locs(j)) then
          continue
        else
          hazard = get_hazard_type(j)
          select case (hazard)
            case (Wumpus)
              print *, "I SMELL A WUMPUS!"
            case (Pit)
              print *, "I FEEL A DRAFT!"
            case (Bat)
              print *, "BATS NEARBY!"
          end select
        end if
      end do
    end do
  end subroutine issue_hazard_warnings

  ! Move into a nearby room
  subroutine enter_room
    integer :: your_loc, i
    your_loc = inhabit_locs(You)
    call issue_hazard_warnings
    write(*, fmt="(1x,a,i2)", advance="no") "YOU ARE IN ROOM ", your_loc
    print *, ""
    write(*, fmt="(1x,a)", advance="no") "TUNNELS LEAD TO "
    do i = 1, 3
      write(*, fmt="(i2,2x)", advance="no") cave(your_loc, i)
    end do
    print *, ""
    print *, "ARROWS LEFT: ", hero%arrows
  end subroutine

  subroutine arrow_check_hits (room_pick)
    integer, intent(in) :: room_pick

    if (inhabit_locs(Wumpus) == room_pick) then
      print *, "AHA! YOU GOT THE WUMPUS!"
      hero%win_state = WSYouGotWumpus
    else if (inhabit_locs(You) == room_pick) then
      print *, "OUCH! ARROW GOT YOU!"
      hero%win_state = WSArrowGotYou
    end if
  end subroutine

  ! Fire an arrow through up to five rooms.
  subroutine arrow_routine
    integer :: k, j
    integer :: your_loc, wumpus_loc, num_rooms, room_pick, ioerr
    character(len=100) :: ioerrmsg
    integer, dimension(-1:5) :: room_picks ! Allowing this to start at -1 to avoid oob warning

    if (hero%arrows == 0) then
      print *, "YOU ARE OUT OF ARROWS"
      return
    end if

    do
      write(*, fmt="(1x,a,i2)", advance="no") "NO. OF ROOMS (1-5) "
      read(*, *, iostat=ioerr, iomsg=ioerrmsg) num_rooms
      if (ioerr == 0) exit
    end do

    room_loop: do k = 1, num_rooms
      read_loop: do
        write(*, fmt="(1x,a,i2)", advance="no") "ROOM # "
        read(*, *, iostat=ioerr, iomsg=ioerrmsg) room_pick
        room_picks(k) = room_pick
        if (k <= 2) then
          exit
        else if (room_picks(k) /= room_picks(k-2)) then
          exit
        end if

        print *, "ARROWS AREN'T THAT CROOKED - TRY ANOTHER ROOM"
        exit room_loop ! restart the loop
      end do read_loop
    end do room_loop

    your_loc = inhabit_locs(You)
    wumpus_loc = inhabit_locs(Wumpus)
    hero%win_state = WSContinue
    do k = 1, num_rooms
      do j = 1, 3
        if (cave(your_loc, j) == room_picks(k)) then
          call arrow_check_hits(room_picks(k))
        else
          ! Not a valid path, so fires at random
          call arrow_check_hits(cave(your_loc, get_rand(1, 3)))
        end if
      end do
    end do

    if (hero%win_state == WSContinue) then
      print *, "MISSED"
    end if

    hero%arrows = hero%arrows - 1
  end subroutine

  subroutine wumpus_move_routine
    integer :: k
    k = get_rand(1, 4)
    if (k == 4) then
      if (inhabit_locs(Wumpus) /= inhabit_locs(You)) then
        return
      end if
    else
      ! print *, "Wumpus was at ", inhabit_locs(Wumpus)
      inhabit_locs(Wumpus) = cave(inhabit_locs(Wumpus), k)
      ! print *, "Wumpus is now at ", inhabit_locs(Wumpus)
      if (inhabit_locs(Wumpus) == inhabit_locs(You)) then
        print *, "WUMPUS ENTERS THE ROOM"
        hero%win_state = WSWumpusGotYou
      end if
    end if
  end subroutine

  subroutine player_move_routine
    integer :: where_to = -1, ioerr, k, your_loc, hazard_loc
    integer(kind(Entity)) :: hazard_type
    hero%win_state = WSContinue

    do
      do
        write(*, fmt="(1x,a,i2)", advance="no") "WHERE TO "
        read(*, *, iostat=ioerr) where_to
        if (ioerr == 0 .and. where_to > 0 .and. where_to <= 20) then
          exit
        end if
      end do

      do k = 1, 3
        ! CHECK IF LEGAL MOVE
        if (cave(inhabit_locs(You), k) == where_to) then
          inhabit_locs(You) = where_to
          exit
        end if
      end do
      ! Move was successful if the location was set above.
      if (inhabit_locs(You) == where_to) then
        exit
      end if
    end do

    your_loc = inhabit_locs(You)

    do k = 2, 6
      hazard_loc = inhabit_locs(k)
      if (inhabit_locs(You) == hazard_loc) then
        hazard_type = get_hazard_type(k)
        select case (hazard_type)
          case (Wumpus)
            print *, "OOPS! BUMPED A WUMPUS!"
            hero%win_state = WSBumpedWumpus
          case (Bat)
            print *, "ZAP--SUPER BAT SNATCH! ELSEWHEREVILLE FOR YOU!"
            inhabit_locs(You) = get_rand(1, 20)
          case (Pit)
            print *, "YYYYIIIIEEEE . . . FELL IN PIT"
            hero%win_state = WSFellInPit
        end select
      end if
    end do
  end subroutine

  subroutine process_cmd
    integer(kind(Command)) :: cmd
    cmd = get_command()
    select case (cmd)
      case (Shoot)
        call arrow_routine
      case (Move)
        call player_move_routine
    end select
  end subroutine

  subroutine check_win_state
    select case (hero%win_state)
    case (WSContinue)
    case (WSArrowGotYou, WSFellInPit)
      print *, "HA HA HA - YOU LOSE!"
      hero%win_state = WSGameOver
    case (WSYouGotWumpus)
      print *, "HEE HEE HEE - THE WUMPUS'LL GET YOU NEXT TIME!!"
      hero%win_state = WSGameOver
    case (WSWumpusGotYou)
      print *, "TSK TSK TSK - WUMPUS GOT YOU!"
      hero%win_state = WSGameOver
    end select
  end subroutine

  subroutine run_game
    call get_environment_variable("DEBUG", is_debug)
    print *, is_debug
    do
      call setup_game
      print *, "[---------------------------------------]"
      print *, "             HUNT THE WUMPUS"
      print *, "[---------------------------------------]"
      print *, ""
      call ask_instructions

      do
        call enter_room
        call process_cmd
        call wumpus_move_routine
        call check_win_state
        if (hero%win_state == WSGameOver) exit ! break out of the loop
      end do

      call check_quit
    end do
  end subroutine run_game
end module hunt_the_wumpus
