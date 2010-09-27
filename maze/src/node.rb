class Node
  attr_accessor :visited, :type, :north, :south, :east, :west, :parent_direction, :unvisited_neighbors

  def initialize p, n = nil, w=nil, s=nil, e=nil
    p "debug:node:init: p = #{p}, n = #{n}, w = #{w}, s = #{s}, e = #{e}" if $DEBUG
    @visited = false
    @type = :unknown
    @east, @north, @west, @south, @parent_direction = e, n, w, s, p
    #FIXME *puke*, please tell me this will be done in a better way
    @unvisited_neighbors = {}
    @unvisited_neighbors[:east] = @east if !@east.nil? && !@east.visited
    @unvisited_neighbors[:north] = @north if !@north.nil? && !@north.visited
    @unvisited_neighbors[:south] = @south if !@south.nil? &&  !@south.visited
    @unvisited_neighbors[:west] = @west if !@west.nil? && !@west.visited
    @unvisited_neighbors.delete(@parent_direction)
  end

  def unvisited_neighbors?
    #p "debug:unvisited_neighbors? checking" if $DEBUG
    return nil if @unvisited_neighbors.nil? || @unvisited_neighbors.empty?
    d = @unvisited_neighbors.keys[rand(@unvisited_neighbors.keys.size)]
    #@unvisited_neighbors -= [n = @unvisited_neighbors[rand(@unvisited_neighbors.size)]]
    n = @unvisited_neighbors.delete(d)
    p "debug:unvisited_neighbors? d = #{d}, n = #{n}" if $DEBUG

    return n, d
  end

  #FIXME again, common problem of not using a hashmap
  def parent
    #p "debug:parent::@parent_direction = #{@parent_direction}" if $DEBUG
    return nil if @parent_direction.nil?
    return instance_variable_get("@#{@parent_direction.to_s}")
  end
end
