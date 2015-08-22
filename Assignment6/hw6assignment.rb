# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece

  def initialize(point_array, board)
    super(point_array, board)
    @num_points = point_array[0].length
  end

  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece(board)
    MyPiece.new(Cheat_Piece, board)
  end

  def num_points
    @num_points
  end

  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                   rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                  [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                    [[0, 0], [0, -1], [0, 1], [0, 2]]],
                   rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                   rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                   rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                   rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                   rotations([[0,0],[1,0],[2,0],[0,1],[1,1]]),
                   [[[0,0],[1,0],[2,0],[3,0],[4,0]],
                    [[0,0],[0,1],[0,2],[0,3],[0,4]]],
                   rotations([[0,0],[0,1],[1,0]])]

  Cheat_Piece = [[0,0]]


end

class MyBoard < Board

  def initialize(game)
    super(game)
    @current_block = MyPiece.next_piece(self)
    @cheat_piece = nil
  end

  def next_piece
    if @cheat_piece.nil?
      @current_block = MyPiece.next_piece(self)
    else
      @current_block = @cheat_piece
      @cheat_piece = nil
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..@current_block.num_points - 1).each { |index|
      current = locations[index]
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]

    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def cheat
    return if !@cheat_piece.nil? || @score < 100
    @score -= 100
    @cheat_piece = MyPiece.cheat_piece(self)
  end

end

class MyTetris < Tetris

  def initialize
    super
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
 
  def key_bindings 
    super
    @root.bind('u', proc { 2.times {|i| @board.rotate_clockwise } })
    @root.bind('c', proc { @board.cheat} )
  end

end


