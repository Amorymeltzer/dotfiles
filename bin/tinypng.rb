require "net/https"
require "uri"

key = "<your api key>"
input = "large-input.png"
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
