#!/usr/bin/env ruby
# Modified from https://tinypng.com/developers/reference
# Make an image file tinier

require "net/https"
require "uri"

key = "asd"

File.open(File.expand_path('~/.tinypng_api_key'), "r") do |f|
  f.each_line do |line|
    key = line.chomp
  end
end

if ARGV[0].nil?
  puts "You need to specify a file to upload."
  exit!(1)
end

ARGV.each do |x|
  input = "#{x}"
  output = "tiny-output.png"

  uri = URI.parse("https://api.tinypng.com/shrink")

  http = Net::HTTP.new(uri.host, uri.port)
  http.use_ssl = true

  # Uncomment below if you have trouble validating our SSL certificate.
  # Download cacert.pem from: http://curl.haxx.se/ca/cacert.pem
  # http.ca_file = File.join(File.dirname(__FILE__), "cacert.pem")

  request = Net::HTTP::Post.new(uri.request_uri)
  request.basic_auth("api", key)

  response = http.request(request, File.binread(input))
  if response.code == "201"
    # Compression was successful, retrieve output from Location header.
    File.binwrite(output, http.get(response["location"]).body)
  else
    # Something went wrong! You can parse the JSON body for details.
    puts "Compression failed"
  end
end
