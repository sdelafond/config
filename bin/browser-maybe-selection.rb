#! /usr/bin/ruby

require 'uri'

ENV['DISPLAY'] = ':0'

urlSchemes = ['http', 'ftp', 'https']

system("ps aux | grep -q '[f]irefox'")
browser = $?.exitstatus == 0 ? "iceweasel" : "chromium-browser"

rhttp = URI.regexp(urlSchemes)

selection = ARGV[1] == nil ? `xclip -o` : ARGV[1]

if ARGV[0] == nil or ARGV[0] == "default" then
  selection.gsub!(/^\+/m, '')
  case selection
  when rhttp
    URI::extract(selection, urlSchemes).each { |url|
      system("#{browser} '#{url}'")
    }
  end
elsif ARGV[0] == 'gs' then
  system("#{browser} 'http://www.google.com/search?q=#{selection}'")
elsif ARGV[0] == 'gi' then
  system("#{browser} 'http://www.google.com/images?q=#{selection}'")
elsif ARGV[0] == 'gm' then
  system("#{browser} 'http://maps.google.com/?q=#{selection}'")
end