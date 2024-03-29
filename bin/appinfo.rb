#!/usr/bin/env ruby
require 'date'
###################################################
#                         iii          fff        #
#   aa aa pp pp   pp pp       nn nnn  ff    oooo  #
#  aa aaa ppp  pp ppp  pp iii nnn  nn ffff oo  oo #
# aa  aaa pppppp  pppppp  iii nn   nn ff   oo  oo #
#  aaa aa pp      pp      iii nn   nn ff    oooo  #
#         pp      pp                              #
###################################################
=begin appinfo
Shows keys from spotlight data for an app
usage: 'appinfo [app name]'

Keys for sizes are converted to human-readable numbers (e.g. 25.3MB)
Keys for dates are converted to localized short date format

=== Config
:show_icon: If you have imgcat or chafa installed, print out an icon
:keys:       The keys to parse and their "pretty" form for printing Output in
             the order listed
==== Default keys:
'location' => 'Location',
'kMDItemCFBundleIdentifier' => 'Bundle ID',
'kMDItemPhysicalSize' => 'Size',
'kMDItemVersion' => 'Version',
'kMDItemContentCreationDate' => 'Released',
'kMDItemAppStorePurchaseDate' => 'Purchased',
'kMDItemLastUsedDate' => 'Last Used',
'kMDItemAppStoreCategory' => 'Category',
'kMDItemCopyright' => 'Copyright'

=end

CONFIG = {
  :show_icon => true,
  :keys => {
    'location' => 'Location',
    'kMDItemCFBundleIdentifier' => 'Bundle ID',
    'kMDItemAlternateNames' => 'Alternate Names',
    'kMDItemPhysicalSize' => 'Size',
    'kMDItemVersion' => 'Version',
    'kMDItemContentCreationDate' => 'Released',
    'kMDItemAppStorePurchaseDate' => 'Purchased',
    'kMDItemLastUsedDate' => 'Last Used',
    'kMDItemAppStoreCategory' => 'Category',
    'kMDItemCopyright' => 'Copyright',
    'kMDItemExecutableArchitectures' => 'Architecture'
  }
}

def class_exists?(class_name)
  klass = Module.const_get(class_name)
  return klass.is_a?(Class)
rescue NameError
  return false
end

if class_exists? 'Encoding'
  Encoding.default_external = Encoding::UTF_8 if Encoding.respond_to?('default_external')
  Encoding.default_internal = Encoding::UTF_8 if Encoding.respond_to?('default_internal')
end

class Array
  def longest_element
    group_by(&:size).max.last[0].length
  end
end

class String
  def to_human(fmt=false)
    n = self.to_i
    count = 0
    formats = %w(B KB MB GB TB PB EB ZB YB)

    while  (fmt || n >= 1024) && count < 8
      n /= 1024.0
      count += 1
      break if fmt && formats[count][0].upcase =~ /#{fmt[0].upcase}/
    end

    format("%.2f",n) + formats[count]
  end
end

def find_app(app)
  location = nil
  narrow = ' -onlyin /System/Applications -onlyin /Applications -onlyin /Applications/Setapp -onlyin /Applications/Utilities -onlyin /Developer/Applications'
  res = `mdfind#{narrow} 'kind:app filename:"#{app}"' | grep -E '\.app$' | head -n 1`.strip
  unless res && res.length > 0
    res = `mdfind 'kind:app filename:"#{app}"' | grep -E '\.app$' | head -n 1`.strip
  end

  if class_exists? 'Encoding'
    res = res.force_encoding('utf-8')
  end

  return res && !res.empty? ? res.strip : false
end

def exec_available(cli)
  if File.exist?(File.expand_path(cli))
    File.executable?(File.expand_path(cli))
  else
    system "which #{cli}", out: File::NULL, err: File::NULL
  end
end

def show_icon(app_path)
  if CONFIG[:show_icon] && (exec_available('imgcat') || exec_available('chafa'))
    app_icon = `defaults read "#{app_path}/Contents/Info" CFBundleIconFile`.strip.sub(/(\.icns)?$/, '.icns')

    if exec_available('imgcat')
      cmd = 'imgcat'
    elsif exec_available('chafa')
      cmd = 'chafa -s 15x15 -f iterm'
    end

    res = `mkdir -p ${TMPDIR}appinfo && sips -s format png --resampleHeightWidthMax 256 "#{app_path}/Contents/Resources/#{app_icon}" --out "${TMPDIR}appinfo/#{app_icon}.png"` #  > /dev/null 2>&1

    $stdout.puts `#{cmd} "${TMPDIR}appinfo/#{app_icon}.png"  && rm "${TMPDIR}appinfo/#{app_icon}.png"`

  end
end

def parse_info(info)
  values = {}
  if class_exists? 'Encoding'
    info = info.force_encoding('utf-8')
  end

  info.gsub!(/(\S+)\s*=\s*\((.*?)\)/m) do
    m = Regexp.last_match
    val = m[2].strip.split(/\n/).delete_if { |i| i.strip.empty? }.map { |l|
      l.strip.gsub(/"/, '').sub(/,$/, '').sub(/x86_64/, 'Intel').sub(/arm64/, 'Apple Silicon')
    }.join(', ')
    val += ' (Unviversal Binary)' if val =~ /Intel/ && val =~ /Apple Silicon/
    values[m[1]] = val
    ''
  end

  info.split(/\n/).delete_if(&:empty?).each do |line|
    sp = line.split(/\s*=\s*/)
    values[sp[0]] = sp[1].gsub(/"/, '')
  end
  values
end


def get_info(appname)
  app = appname # .sub(/\.app$/,'')
  found = find_app(app)
  if found
    keys = "-name " + CONFIG[:keys].keys.join(' -name ')
    res = %x{mdls #{keys} "#{found}"}
    result = parse_info(res)
    result['location'] = found
    return result
  else
    $stdout.puts %Q{App "#{app}" not found.}
    Process.exit 1
  end
end

def info(app)
  appinfo = get_info(app)
  if appinfo && appinfo.length > 0
    show_icon(appinfo['location'])
    longest_key = CONFIG[:keys].values.longest_element
    CONFIG[:keys].each {|k,v|
      key = v
      val = appinfo[k]&.strip || 'None'
      val = case k
      when /Size$/
        val.to_human
      when /Date$/
        if appinfo[k].strip =~ /^\d{4}-\d{2}-\d{2}/
          Date.parse(val.strip).strftime('%D') rescue val
        end
      else
        val
      end

      val = val =~ /\(null\)/ ? "\033[0;36;40mUnknown\033[0m" : "\033[1;37;40m#{val}\033[0m"
      $stdout.puts "\033[0;32;40m%#{longest_key}s: %s" % [key, val]
    }
  end
end

def exit_help(code=0)
  output = <<~ENDOUT
    Shows keys from Spotlight data for an app
    Usage:

      #{File.basename(__FILE__)} [app name]
  ENDOUT
  puts output
  Process.exit code.to_i
end

if ARGV.length == 0
  exit_help(1)
elsif ARGV[0] =~ /^-?h(elp)?$/
  exit_help
else
  info(ARGV.join(" "))
end
