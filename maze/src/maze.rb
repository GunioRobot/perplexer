#!/usr/bin/env ruby
require 'node'
#turn around and see what's behind
#if I moved in my last step, then:
###this is a new cell
###if the cell is visited, a loop! return now or perish forever
###add the observations to the new cell
###attach this new cell to the 'current_cell' and make current_cell parent
###update current_cell to this cell
#else if i just turned
###this is the same cell
###add the observations if they did not exist
#now we need to check if we should move or turn
#if any side is dark, then turn
#else add all unvisited cells and select a cell that has not been visited and move
#if all cells have been visited, and we have not found a solution - then
#this is a bad path. When we enter the cell, we need to know who the parent
#cell is. Once we know this is a bad cell, we need to go to the parent
#add observation to the current cell's node
#decide if we should move or 

class Maze
  #need to have the notion of just looking, and also moving
  attr_accessor :in_file, :out_file, :root, :cell, :orientation, :action, :first_cell, :new_cell

  def initialize
    @orientation = :north
    @cell = nil
    @first_cell = false
    @new_cell = false
  end

  def init inp, out
    @in_file = open(inp, 'r+')
    @out_file = open(out, 'w+')
  end

  def receive
    r = @in_file.gets.strip
    p "debug: r = #{r}" if $DEBUG
    return r
  end

  def send o
    @out_file.write o
    @out_file.flush
  end

  def parse p
    m = p.match(/There is a (.*) to the left, a (.*) ahead, and a (.*) to the right./)
    l, c, r = m[1].to_sym, m[2].to_sym, m[3].to_sym
    p "debug: l = #{l}, c = #{c}, r = #{r}" if $DEBUG
    return l, c, r
  end

  def translate t
  end

  #determine where each cell goes
  #FIXME this is ugly as hell, lets clean it up when this starts working
  def create_neighbors l, c, r
    m = { :north => nil, :west => nil, :south => nil, :east => nil}
    m[opposite @orientation] = @cell
    #p "debug: parent cell = #{m[opposite @orientation].parent_direction}" if $DEBUG && @cell
    if @orientation == :east
      m[:south] = Node.new(:north) if l == :corridor
      m[:east] = Node.new(:west) if c == :corridor
      m[:north] = Node.new(:south) if r == :corridor
    elsif @orientation == :north
      m[:east] = Node.new(:west) if l == :corridor
      m[:north] = Node.new(:south) if c == :corridor
      m[:west] = Node.new(:east) if r == :corridor
    elsif @orientation == :south
      m[:west] = Node.new(:east) if l == :corridor
      m[:south] = Node.new(:north) if c == :corridor
      m[:east] = Node.new(:west) if r == :corridor
    elsif @orientation == :west
      m[:north] = Node.new(:south) if l == :corridor
      m[:west] = Node.new(:east) if c == :corridor
      m[:south] = Node.new(:north) if r == :corridor
    end
    p "debug:create_neighbors[node_#{@cell.id if @cell}][parent #{@cell.parent}] n = #{m[:north]} w = #{m[:west]}, s = #{m[:south]} e = #{m[:east]}" if $DEBUG
    return m[:north], m[:west], m[:south], m[:east]
  end

  def opposite o
    return :west if o == :east
    return :east if o == :west
    return :north if o == :south
    return :south if o == :north
   # p "error: could not return opposite for o = #{o}" if $DEBUG
  end
  
  def left o
    return :east if o == :north
    return :north if o == :west
    return :west if o == :south
    return :south if o == :east
  end

  def right o
    opposite left o
  end

  #FIXME hack to update the first cell only
  def update_first_cell cell, l, c, r
    cell.south = Node.new(opposite @orientation) if c == :corridor
    return cell
  end
  def update_cell c,p,n,w,s,e
   c.north = n
   c.south = s
   c.east = e
   c.west = w
   c.parent_direction = p
   return c
  end

  def update_current_cell l, c, r
    p "debug:update_current_cell l = #{l}, c = #{c}, r = #{r}" if $DEBUG
    if @cell.nil?
      p "debug:update_current_cell:first time" if $DEBUG
      @first_cell = true
      @root = @cell = Node.new(nil)
      n, w, s, e = create_neighbors(l, c, r)
      @root = @cell = update_cell(@cell,nil,n,w,s,e)
      return @cell
    elsif @first_cell == true
      p "debug:update_current_cell:updating first cell = #{@cell}" if $DEBUG
      @cell = update_first_cell @cell, l, c, r
      p "debug:update_current_cell:updated first cell = #{@cell}" if $DEBUG
      @cell.visited = true
      @first_cell = false
    elsif @new_cell == true
      p "debug:update_current_cell:creating new cell" if $DEBUG
      n, w, s, e = create_neighbors(l, c, r)
      @cell = update_cell @cell,opposite(@orientation),n, w, s, e
      p "debug:update_current_cell:created new cell = #{@cell} parent = #{@cell.parent} neighbors[#{@cell.neighbors}]" if $DEBUG
      @new_cell = false
    else
      p "debug:update_current_cell:old cell = #{@cell}" if $DEBUG
    end
    return @cell
  end
  def cell_to_visit cell
    p "debug:cell_to_visit:cell = #{@cell}" if $DEBUG
    return cell,:south if @first_cell 
    k = cell.unvisited_neighbors?
    p "debug:cell_to_visit:cell = #{k}" if $DEBUG
    if !k.nil?
      @new_cell = true
      p "debug:visiting neighbor = #{k[1]}" if $DEBUG
      return k[0], k[1] #@cell.unvisited_neighbor
    elsif cell.parent != nil
      p "debug:visiting parent = #{cell.parent_direction}" if $DEBUG
      return cell.parent, cell.parent_direction
    elsif cell.parent.nil? || cell.parent_direction.nil?
      p "warn: reached root without finding end"
      return nil,nil
    else
      p "error: reached dangling condition"
      exit
    end
  end

  #give the actions required to turn
  def orient so, eo
    return :move if eo == so
    if eo == opposite(so)
      return :turn_around,:move
    elsif eo == left(so)
      return :turn_left,:move
    elsif eo == right(so)
      return :turn_right,:move
    end
      p "error: unknown orientation for so = #{so}, eo = #{eo}" if $DEBUG
      return :error
  end

  def visit n, d
    #traverse @root
    if n == nil
      return :halt
    end
    if @first_cell
      @orientation = opposite @orientation
      return :view_back
    end
    n.visited = true
    p "debug:visiting n = #{n}, n.parent = #{n.parent}" if $DEBUG
    actions = orient @orientation,d
    @orientation = d
    p "debug:reached new cell, parent = #{@cell.parent_direction}" if $DEBUG
    actions
  end

  def process l, c, r
      cell,direction = cell_to_visit update_current_cell(l, c, r)
      actions = visit cell,direction
      @cell = cell
      return actions
  end
end

def traverse node
  n = node.neighbors
  p "visiting node[#{node}] -> neighbors #{n}"
  node.traversed = true
  n.each {|x| traverse x if !x.traversed ; x.traversed = true}
end
def main
  m = Maze.new
  m.init(ARGV[0], ARGV[1])
  while i = m.receive
    m.send m.translate m.process m.parse i
  end
end
