class Node
  attr_accessor :visited, :type, :north, :south, :east, :west, :parent_direction,:unvisited_neighbors,:id,:parent

  protected
  def visitable?(node)
    !node.nil? && !node.visited && not_parent(node)
  end

  def not_parent(node)
    node != @parent
  end

  private
  @@global_id = 0

  def unique_id
    @@global_id += 1
  end

  public
  def initialize parent,p, n=nil, w=nil, s=nil, e=nil
    @id = unique_id
    p "debug:node:init[node_#{@id}]: p = #{p}, n = #{n}, w = #{w}, s = #{s}, e = #{e}" if $DEBUG
    @visited = false
    @type = :unknown
    @east, @north, @west, @south, @parent, @parent_direction = e, n, w, s, parent, p
  end

  def next_unvisited_neighbor
    r = nil
    r = @east,:east if visitable?(@east)
    r = @west,:west if visitable?(@west)
    r = @north,:north if visitable?(@north)
    r = @south,:south if visitable?(@south)
    return r
  end

  def neighbors
    n = []
    n<< @north if @north
    n<< @south if @south
    n<< @east if @east
    n<< @west if @west
    return n
  end

  def to_s
    return "[node_#{@id}][#{@parent_direction}]"
  end


end
