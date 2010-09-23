#!/usr/bin/env ruby
require 'node'

class Maze
  attr_accessor :in_file, :out_file, :tree, :orientation
  def initialize
  end
  def init inp, out
    @in_file = open(inp, 'r+')
    @out_file = open(out, 'w+')
    @tree = Node.new
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
    return l,c,r
  end

  def translate t
  end

  def update_tree l,c,r
  end

  def process l,c,r
    update_tree(l,c,r)
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
