#! /usr/bin/ruby

require 'uri'

urlSchemes = ['http', 'ftp', 'https']

browser = "iceweasel"

rhttp = URI.regexp(urlSchemes)

selection = `xclip -o`

if ARGV[0] == nil then
    selection.gsub!(/(^\+|\n)/, '')
    selection.gsub!(/http/, ' http')  
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
