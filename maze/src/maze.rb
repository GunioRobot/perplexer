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
  attr_accessor :in_file, :out_file, :root, :orientation, :commands, :inp,:out,:incomplete_parent,:current_cell,:nw,:ew

  def initialize
    @orientation = :north
    @commands = []
    @incomplete_parent = true
    @nw,@ew = 0,0
  end

  def init inp, out
    p "debug: input = #{inp} output = #{out}" if $DEBUG
    @inp = inp
    @out = out
  end

  def receive count=1
    @in_file = open(@inp, 'r')
    r = ""
    count.times { |c| 
    r = @in_file.gets.strip
    p "debug: r = #{r}" if $DEBUG
    }
    @in_file.close
    return r
  end

  def send commands
    @out_file = open(@out, 'w+')
    count = 0
    commands.each {|c| 
      p "debug:send command = #{c}" if $DEBUG
      @out_file.write (c+"\n")
      @commands<< c
      count += 1
    }
    @out_file.close
    count
  end

  def parse p
    m = p.match(/There is a (.*) to the left, a (.*) ahead, and a (.*) to the right./)
    if !m.nil?
    l, c, r = m[1].to_sym, m[2].to_sym, m[3].to_sym
    p "debug: l = #{l}, c = #{c}, r = #{r}" if $DEBUG
    return l, c, r
    elsif m = p.match(/Congratulations. You did it/)
      p "done!"
      p @commands
      exit
    else
      p "fatal:unable to parse #{p}" if $DEBUG
      exit
    end
  end

  def translate actions
    t = []
    actions.each {|x| 
      case x
      when :turn_left
      t += ["TURN LEFT"]
      when :move
        t += ["MOVE"]
      when :turn_right
        t += ["TURN RIGHT"]
      when :turn_around
        t += ["TURN LEFT","TURN LEFT"]
      end
    }
    p "debug:translate #{actions} -> #{t}" if $DEBUG
    return t
  end

  def create_neighbors cell,orientation,l, c, r
    m = { :north => nil, :west => nil, :south => nil, :east => nil}
    p "debug:create_neighbors:start::[#{cell}]o[#{orientation}][parent #{cell.parent}] n = #{m[:north]} w = #{m[:west]}, s = #{m[:south]} e = #{m[:east]}" if $DEBUG
    ld,rd,od = left(orientation),right(orientation),opposite(orientation)
    m[ld] = Node.new(cell,opposite(ld)) if l == :corridor
    m[rd] = Node.new(cell,opposite(rd)) if r == :corridor
    m[orientation] = Node.new(cell,od) if c == :corridor
    p "debug:create_neighbors:end::[#{cell}]o[#{orientation}][parent #{cell.parent}] n = #{m[:north]} w = #{m[:west]}, s = #{m[:south]} e = #{m[:east]}" if $DEBUG
    return m[:north], m[:west], m[:south], m[:east]
  end

  def opposite o
    return :west if o == :east
    return :east if o == :west
    return :north if o == :south
    return :south if o == :north
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

  def update_cell c,n,w,s,e
    p "debug:update_cell[#{c}]: n = #{n} w = #{w} s = #{s} e = #{e}" if $DEBUG
   c.north = n if c.north.nil?
   c.south = s if c.south.nil?
   c.east = e if c.east.nil?
   c.west = w if c.west.nil?
   return c
  end

  def update_current_cell cell,l, c, r
    p "debug:update_current_cell cell = #{cell} visited? = #{cell.visited if cell} l = #{l}, c = #{c}, r = #{r}" if $DEBUG
    if cell.nil?
      p "debug:update_current_cell:first time" if $DEBUG
      @root = cell = Node.new(nil,nil)
    end
    if cell.parent.nil? || !cell.visited
      p "debug:update_cell:start::cell = #{cell} parent = #{cell.parent} neighbors[#{cell.neighbors}]" if !cell.nil? && $DEBUG
      n, w, s, e = create_neighbors(cell,@orientation,l, c, r)
      cell = update_cell(cell,n,w,s,e)
      p "debug:update_cell:end::cell = #{cell} parent = #{cell.parent} neighbors[#{cell.neighbors}]" if !cell.nil? && $DEBUG
      cell.visited = true
      p "debug:update_current_cell:created new cell = #{cell} parent = #{cell.parent} neighbors[#{cell.neighbors}]" if $DEBUG
    end
    return cell
  end

  attr_accessor 
  def cell_to_visit cell
    p "debug:cell_to_visit:cell = #{cell}" if $DEBUG
    if cell.parent.nil? && @incomplete_parent
      return cell,:south
    end
    k = cell.next_unvisited_neighbor
    p "debug:cell_to_visit:unvisited_neighbor = #{k}" if $DEBUG
    if !k.nil?
      p "debug:visiting neighbor = #{k[1]}" if $DEBUG
      return k[0], k[1]
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
    return [:move] if eo == so
    if eo == opposite(so)
      return [:turn_around,:move]
    elsif eo == left(so)
      return [:turn_left,:move]
    elsif eo == right(so)
      return [:turn_right,:move]
    end
      p "error: unknown orientation for so = #{so}, eo = #{eo}" if $DEBUG
      return :error
  end

  def visit cell, d
    if cell == nil
      return :halt
    end
    if cell.parent.nil? && @incomplete_parent
      @orientation = opposite @orientation
      @incomplete_parent = false
      return [:turn_around]
    end
    p "debug:visiting cell = #{cell}, cell.parent = #{cell.parent}" if $DEBUG
    actions = orient @orientation,d
    @orientation = d
    p "debug:reached new cell, parent = #{cell.parent_direction}" if $DEBUG
    actions
  end

  def process view
      l,c,r = view[0],view[1],view[2]
      cell,direction = cell_to_visit update_current_cell(@current_cell,l, c, r)
      actions = visit cell,direction
      @current_cell = cell
      p "debug:process cell = #{cell} parent = #{cell.parent if cell}" if $DEBUG
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
  count = 1
  while i = m.receive(count)
    count = m.send m.translate m.process m.parse i
  end
  p @commands
end
main if ARGV.size == 2
