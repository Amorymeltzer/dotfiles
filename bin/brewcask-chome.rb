# brewcask-chome
# cask home and cat

command_name = ARGV.shift
cask_token = ARGV.shift

cask = Hbc.load(cask_token)

Hbc.debug = true
system "/usr/bin/open", '--', cask.homepage
cask_path = Hbc.path(cask_token)
puts File.open(cask_path) { |f| f.read }
