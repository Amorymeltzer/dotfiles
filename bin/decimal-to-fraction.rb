#!/usr/bin/env ruby
# Convert decimal to fraction
# From http://codegolf.stackexchange.com/a/17340/8946

y=gets.to_r;puts"#{y.to_i} + (#{y-y.to_i})"
