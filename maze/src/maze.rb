#!/usr/bin/env ruby
require 'node'

class Maze
  #need to have the notion of just looking, and also moving
  attr_accessor :in_file, :out_file, :root, :cell, :orientation, :action, :first_cell
  def initialize
    @orientation = :north
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
  def create_neighbors l, c, r
    if @cell.nil?
      return c, r, nil, l
    end
    return @cell, l, c, r
  end

  def parent_orientation
    o = @orientation
    return :west if o == :east
    return :east if o == west
    return :north if o == :south
    return :south if o == :north
  end

  def update_graph l, c, r
    n, w, s, e = create_neighbors(l, c, r)
    if @cell.nil?
      @first_cell = true
      @root = @cell = Node.new(n, w, s, e, nil)
      return view_back
    end
    if @first_cell
      update_cell c
    else
      c = Node.new(e, n, w, s, parent_direction(@orientation))
    end
    if @cell.unvisited_neighbors?
      visit @cell.unvisited_neighbor
    elsif !@cell.parent.nil?
      visit @cell.parent
    else
      p "error: reached root without finding end"
      return :halt
    end
  end

  #give the actions required to turn
  def orient so eo
    return :turn_left, :turn_right, :turn_around
  end
  def visit n, d
    actions = orient(@orientation, d)
    @orientation = d
    actions<< :move
  end

  def process l, c, r
      @actions = update_graph(l, c, r)
  end
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

  def process l, c, r
    update_tree(l, c, r)
    return :f if c == :corridor
    return :tl if l == :corridor
    return :tr if r == :corridor
    return :tu
  end
end

def main
  m = Maze.new
  m.init(ARGV[0], ARGV[1])
  while i = m.receive
    m.send m.translate m.process m.parse i
  end
end
