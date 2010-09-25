class Node
  attr_accessor :visited, :type, :north, :south, :east, :west, :parent_direction
  def initialize e, n, w, s, p
    @visited = false
    @type = :unknown
    @east, @north, @west, @south, @parent_direction = e, n, w, s, p
  end
end
