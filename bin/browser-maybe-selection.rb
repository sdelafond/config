#! /usr/bin/ruby

require 'shellwords'
require 'uri'

#ENV['DISPLAY'] = ':'

urlSchemes = ['http', 'ftp', 'https']

system("pgrep -f '(firefox-bin|firefox.real)'")
browser = ($?.exitstatus == 0 && ARGV[1].nil?) ? "firefox" : "chromium"

selection = `xclip -o`

rhttp = URI.regexp(urlSchemes)

# handle mutt wrap markers
selection.gsub!(/\n\+/m, '')

if ARGV[0] == nil or ARGV[0] == "default" then
  case selection
  when rhttp
    URI::extract(selection, urlSchemes).each { |url|
      url.gsub!(/\.$/, '')
      system("#{browser} #{url.shellescape}")
    }
  end
elsif ARGV[0] == 'gs' then
  system("#{browser} 'http://www.google.com/search?q=#{selection}'")
elsif ARGV[0] == 'gi' then
  system("#{browser} 'http://www.google.com/images?q=#{selection}'")
elsif ARGV[0] == 'gm' then
  system("#{browser} 'http://maps.google.com/?q=#{selection}'")
end
