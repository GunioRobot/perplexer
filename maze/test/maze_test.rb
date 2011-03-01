require 'maze'
require 'test/unit'

class MazeTest < Test::Unit::TestCase
  attr_accessor :m

  def setup
    @m = Maze.new
  end

  def test_parse
    i = "There is a wall to the left, a wall ahead, and a corridor to the right."
    el,ec,er = :wall,:wall,:corridor
    al,ac,ar = @m.parse i
    assert_equal(el,al)
    assert_equal(ec,ac)
    assert_equal(er,ar)
  end

  def test_process
    #facing north
    e = [:turn_around]
    a = @m.process([:wall,:corridor,:wall])
    assert_equal(e,a)
    #facing south
    e = [:turn_around,:move]
    a = @m.process([:wall,:wall,:wall])
    assert_equal(e,a)
    #facing north, corridor to west
    e = [:turn_right,:move]
    a = @m.process([:wall,:wall,:corridor])
    assert_equal(e,a)
    p "facing west" if $DEBUG
    #facing west, all walls
    e = [:turn_around,:move]
    a = @m.process([:wall,:wall,:wall])
    assert_equal(e,a)
    "p facing east" if $DEBUG

    #facing east, corridor to east
    e = [:turn_left,:move]
    a = @m.process([:wall,:wall,:wall])
    assert_equal(e,a)

  end

  def test_create_neighbors
  end

  def test_opposite
    assert_equal :north, @m.opposite(:south)
    assert_equal :south, @m.opposite(:north)
    assert_equal :west, @m.opposite(:east)
    assert_equal :east, @m.opposite(:west)
  end

  def test_left
    assert_equal :east, @m.left(:north)
    assert_equal :north, @m.left(:west)
    assert_equal :west, @m.left(:south)
    assert_equal :south, @m.left(:east)
  end

  def test_right
    assert_equal :east, @m.right(:south)
    assert_equal :north, @m.right(:east)
    assert_equal :west, @m.right(:north)
    assert_equal :south, @m.right(:west)
  end

  def test_update_cell
  end

  def test_update_graph
  end

  def test_orient
    assert_equal [:move], @m.orient(:east, :east)
    assert_equal [:turn_left,:move], @m.orient(:north, :east)
  end
end
