require 'maze'
require 'test/unit'

class MazeTest < Test::Unit::TestCase
  attr_accessor :maze

  def setup
    @maze = Maze.new
  end

  def test_parse
    i = "There is a wall to the left, a wall ahead, and a corridor to the right."
    el,ec,er = :wall,:wall,:corridor
    al,ac,ar = @maze.parse i
    assert_equal(el,al)
    assert_equal(ec,ac)
    assert_equal(er,ar)
  end

  def test_process
    e = :f
    a = @maze.process(:wall,:corridor,:wall)
    assert_equal(e,a)
    e = :tl
    a = @maze.process(:corridor,:wall,:wall)
    assert_equal(e,a)
    e = :tr
    a = @maze.process(:wall,:wall,:corridor)
    assert_equal(e,a)
    e = :tu
    a = @maze.process(:wall,:wall,:wall)
    assert_equal(e,a)
  end
end
