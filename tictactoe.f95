! Name : Jianning Zhang
! ID: 0886809
! Course: CIS3190
! Assignment 1

program tictactoe
  implicit none

  integer :: turn,move
  character, dimension(3,3) :: tictac
  character :: winner, replay
  logical :: over

  do
    ! initial the Tic-Tac-Toe game borad
    call introGame()
    call setNewBoard(tictac)

    
    do
      !check human player move
      call playTicTacToe(move,turn,tictac)
        
      do
        ! when human player mvoe, turn = 0; when computer move, turn = 1;
        if (turn == 0) write(*,*) "After your move..."
        if (turn == 1) write(*,*) "After my move..."
        
        call showBoard(tictac)
        ! check wheter the game over
        call chkovr(tictac,over,winner)
        if (over) then 
            exit
        end if
        ! game not over, it is computer turn
        if (turn == 1) then 
            exit
        end if
        turn = 1
        call pickMove(tictac)
      end do

      call chkovr(tictac,over,winner)
      if (over) then 
        exit
      end if
    end do

    write(*,*) "The game is over!"

    if (winner == "d") then
      write(*,*) "The game is a draw."
    else
      write(*,*) "The winner is: ", winner
    end if

    ! replay tictactoe
    write(*,*) "Do you want to Play again? (y/n)"
    ! defensive input check
    do
      read(*,*) replay
      if(replay == "n") then
        write(*,*) "Close the Tic-Tac-Toe Game"
        exit
      else if(replay =="y") then
        exit
      else
        write(*,*) "Invalid input."
      end if
    end do

    if(replay == "n") then 
      exit
    end if
  end do
end

! simple introduction of Tic-Tac-Toe game.
subroutine introGame()
    implicit none
    
    write(*,*) "Play tic-tac-toe. Enter 1-9 to play"
    write(*,*) " "
    write(*,*) " 1 | 2 | 3 "
    write(*,*) "---+---+---"
    write(*,*) " 4 | 5 | 6 "
    write(*,*) "---+---+---"
    write(*,*) " 7 | 8 | 9 "
    write(*,*) " "
end

! show board after each move
subroutine showBoard(tictac)
    implicit none
    
    character, dimension(3,3) :: tictac
    write(*,*) " "
    write(*,*) " ", tictac(1,1), " | ", tictac(1,2), " | ", tictac(1,3)," "
    write(*,*) "---+---+---"
    write(*,*) " ", tictac(2,1), " | ", tictac(2,2), " | ", tictac(2,3)," "
    write(*,*) "---+---+---"
    write(*,*) " ", tictac(3,1), " | ", tictac(3,2), " | ", tictac(3,3)," "
    write(*,*) " "
end

! get human player move
subroutine getMove(turn, move)
  implicit none
  
  integer :: move, turn, error
  turn = 0
  
  !check if input is integer;
  do 
    write(*,*) "Your move? "
    read(*, *,iostat=error) move
    if(error == 0) then
      exit
    end if
    write(*,*) "Invalid input, please enter 1-9"
  end do
end

! human player move function
subroutine playTicTacToe(move, turn, tictac)
  implicit none

  integer :: move, turn
  character(1), dimension(3,3) :: tictac
  logical :: chkplay

  do 
    call getMove(turn,move)

    if (move < 1 .and. move > 9) then
      write(*,*) "Invalid input."
      cycle
    else if (chkplay(tictac,move)) then
      exit
    else 
      write(*,*) "Invalid move, box already occupied."
      cycle
    end if
  end do

  ! show where the human move at
  if (move == 1) then
    tictac(1,1) = "x"
  else if (move == 2) then 
    tictac(1,2) = "x"
  else if (move == 3) then
    tictac(1,3) = "x"
  else if (move == 4) then 
    tictac(2,1) = "x"
  else if (move == 5) then
    tictac(2,2) = "x"
  else if (move == 6) then
    tictac(2,3) = "x"
  else if (move == 7) then
    tictac(3,1) = "x"
  else if (move == 8) then
    tictac(3,2) = "x"
  else if (move == 9) then
    tictac(3,3) = "x"
  end if
end



! check if Tic-Tac-Toe is over and determine winner (if any)
subroutine chkovr(tictac, over, winner)
  implicit none


  character, dimension(3,3) :: tictac(3,3) 
  character :: winner
  logical :: over, same, dsame
  integer :: ir, ic 
  character , parameter :: blank = " ", draw = "d"

  ! assume game is over at start
  over = .true.

  ! check for a winner.
  ! check rows for a winner.
  do ir = 1, 3
    if (same(tictac(ir,1),tictac(ir,2),tictac(ir,3))) then
      winner = tictac(ir,1)
      return
    end if
  end do
  
  ! no winner by rows, check columns for a winner.
  do ic = 1, 3
    if (same(tictac(1,ic),tictac(2,ic),tictac(3,ic))) then
      winner = tictac(1,ic)
      return
    end if
  end do
  
  ! No winner by rows or columns, check diagonals.
  dsame = same(tictac(1,1),tictac(2,2),tictac(3,3)) .or. same(tictac(1,3),tictac(2,2),tictac(3,1)) 
  if (dsame) then
    winner = tictac(2,2)
    return
  end if

  ! No winner at all. See if game is a draw.
  ! Check each row for an empty space.
  do ir = 1,3
    do ic = 1,3
      if (tictac(ir,ic) == blank) then
        over = .false.
        return
      end if
    end do
  end do

  ! No blank found, game is a draw.
  winner = draw

  return    
end

! check if all moves in the same direction are same
logical function same(t1,t2,t3)
  character :: t1,t2,t3

  if(t1 == "x" .and. t2 == "x" .and. t3 == "x") then 
    same = .true.
  else if (t1 == "o" .and. t2 == "o" .and. t3 == "o") then
    same = .true.
  else
    same = .false.
  end if
end

! check human player moves.
logical function chkplay(tictac,move)
  implicit none
  
  character, dimension(3,3) :: tictac 
  integer :: move

  if (move == 1 .and. (tictac(1,1) == " ")) then 
    chkplay = .true.
  else if (move == 2 .and. (tictac(1,2) == " ")) then 
    chkplay = .true.
  else if (move == 3 .and. (tictac(1,3) == " ")) then 
    chkplay = .true.
  else if (move == 4 .and. (tictac(2,1) == " ")) then 
    chkplay = .true.
  else if (move == 5 .and. (tictac(2,2) == " ")) then 
    chkplay = .true.
  else if (move == 6 .and. (tictac(2,3) == " ")) then 
    chkplay = .true.
  else if (move == 7 .and. (tictac(3,1) == " ")) then 
    chkplay = .true.
  else if (move == 8 .and. (tictac(3,2) == " ")) then 
    chkplay = .true.
  else if (move == 9 .and. (tictac(3,3) == " ")) then 
    chkplay = .true.
  else 
    chkplay = .false.
  end if
end

! clear last game and set new borad for the new game.
subroutine setNewBoard(tictac)
  implicit none
  integer :: i, j
  character, dimension(3,3) :: tictac

  do i = 1,3
    do j = 1,3
      tictac(i,j) = " "
    end do
  end do
  return
end

! the function that computer pick next move 
subroutine pickMove(tictac)
  character, dimension(3,3) :: tictac(3,3)
  integer, dimension(3,8) :: paths = reshape((/1,2,3,4,5,6,7,8,9,1,4,7,2,5,8,3,6,9,1,5,9,3,5,7/), shape(paths))
  integer, dimension(9,2) :: board = reshape((/ 1,1,1,2,2,2,3,3,3,1,2,3,1,2,3,1,2,3 /), shape(board))
  integer, dimension(8):: pathsum
  integer:: i,j,k,x,y,randpos

  ! calculate the pathsums
  do i = 1, 8
    pathsum(i) = 0
    do j = 1, 3
      x = board(paths(j, i),1)
      y = board(paths(j, i),2)
      if (tictac(x,y) == " ") then
        k = 0
      else if (tictac(x,y) == "x") then
        k = 1
      else if (tictac(x,y) == "o") then
        k = 4
      end if
      pathsum(i) = pathsum(i) + k
    end do
  end do

  ! the algorithm that computer try to win the game.
  do i = 1, 8
    if (pathsum(i) == 8) then
      do j = 1, 3
        x = board(paths(j, i),1)
        y = board(paths(j, i),2)
        if (tictac(x,y) == " ") then
          tictac(x,y) = "o"
          return
        end if
      end do
    end if
  end do

  ! the algorithm that computer cannot win the game, do defensive move to avoid lose the game
  do i = 1, 8
    if (pathsum(i) == 2) then
      do j = 1, 3
        x = board (paths(j, i),1)
        y = board (paths(j, i),2)
        if (tictac(x,y) == " ") then
          tictac(x,y) = "o"
          return
        end if
      end do
    end if
  end do

  ! the algorithm that both computer and human player cannot win, random pick a blank space to play
  do i = 0,9
    randpos = int(rand(0)*9)+1
    x = board(randpos, 1)
    y = board(randpos, 2)
    if (tictac(x, y) == " ") then
      tictac(x,y) = "o"
      return
    end if
  end do

  return
end
