# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  new_pieces = [[[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]],
               [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]],  # long with 5
                rotations([[0,0], [0,1], [1, 1]]),
                rotations([[0, 0], [1, 0], [0,1],[1,1],[2,1]])
               ]  

  old_pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]])] # Z

  All_My_Pieces = old_pieces + new_pieces

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end


end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat = false
  end

  def rotate_180
    rotate_clockwise
    rotate_clockwise
    draw
  end

  def cheat
    if (!@cheat && @score >= 100)
      @score -= 100
      @cheat = true
    end
  end

  def next_piece
    if @cheat
      @current_block = MyPiece.new([[[0, 0]]], self) # resubmit
      @cheat = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

end

class MyTetris < Tetris
  # your enhancements here

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180}) 
    @root.bind('c', proc {@board.cheat})
  end

end


