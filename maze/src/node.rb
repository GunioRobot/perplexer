require 'pp'
class Node
  @@global_id = 0
  attr_accessor :visited, :type, :north, :south, :east, :west, :parent_direction, :unvisited_neighbors,:id,:traversed,:parent_node

  def initialize p, n=nil, w=nil, s=nil, e=nil
    @@global_id += 1
    @id = @@global_id
    p "debug:node:init[node_#{@id}]: p = #{p}, n = #{n}, w = #{w}, s = #{s}, e = #{e}" if $DEBUG
    @visited = false
    @type = :unknown
    @east, @north, @west, @south, @parent_direction = e, n, w, s, p
    #FIXME *puke*, please tell me this will be done in a better way
    @traversed = false
    @parent_node = nil
    end

  def unvisited_neighbors?
    @unvisited_neighbors = {}
    @unvisited_neighbors[:east] = @east if !@east.nil? && !@east.visited
    @unvisited_neighbors[:north] = @north if !@north.nil? && !@north.visited
    @unvisited_neighbors[:south] = @south if !@south.nil? &&  !@south.visited
    @unvisited_neighbors[:west] = @west if !@west.nil? && !@west.visited
    @unvisited_neighbors.delete(@parent_direction)

    return nil if @unvisited_neighbors.nil? || @unvisited_neighbors.empty?
    d = @unvisited_neighbors.keys[rand(@unvisited_neighbors.keys.size)]
    n = @unvisited_neighbors.delete(d)
    p "debug:unvisited_neighbors? direction = #{d}, neighbor_parent = #{n.parent_direction}" if $DEBUG
    return n, d
  end

  #FIXME again, common problem of not using a hashmap
  def parent
    p "debug:parent:node:#{self}:parent:#{@parent_direction}" if $DEBUG
    return nil if @parent_direction.nil?
    @parent_node
  end
#  def parent=p
#    case @parent_direction
#    when :north
#      @north = p
#    when :south
#      @south = p
#    when :east
#      @east = p
#    when :west
#      @west = p
#    end
#  end

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
