#!/usr/bin/env ruby
# encoding: utf-8

# Read /Applications/Setapp to get apps already installed
installed_setapp_apps = Dir.glob('/Applications/Setapp/*.app')
installed_setapp_apps.map! {|app|
  File.basename(app,'.app')
}

# Grab the All Apps page from Setapp to get all available apps
apps_page = `curl -SsL https://setapp.com/apps`
setapp_apps = apps_page.force_encoding('utf-8').scan(/<div class="all-apps-item__name">\s*(.*?)\s*<\/div>/m).map {|match|
  match[0]
}

# Read /Applications for non-Setapp apps on the system
apps = Dir.glob('/Applications/*.app')
apps.map! {|app|
  basename = File.basename(app,'.app')
  # Setapp disallows version numbers in app names. Strip them from
  # /Application apps for consistency in matching
  basename.sub!(/\s*\d+$/,'')
  basename
}

setapp_apps.sort.uniq.each {|app|
  if apps.include?(app)
    # App is on Setapp
    out = "Setapp has: #{app}"
    if installed_setapp_apps.include?(app)
      # Setapp version is installed (or at least proxied)
      out += " (Installed)"
    end
    $stdout.puts out
  end
}