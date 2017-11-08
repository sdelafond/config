#! /usr/bin/ruby

require 'shellwords'
require 'uri'

URL_SCHEMES = ['http', 'ftp', 'https']
HTTP_REGEX = URI.regexp(URL_SCHEMES)

# handle mutt wrap markers
def open(selection, browser, mode)
  selection.gsub!(/\n\+/m, '')

  if mode == nil or mode == "default" then
    case selection
    when HTTP_REGEX
      URI::extract(selection, URL_SCHEMES).each { |url|
        url.gsub!(/\.$/, '')
        system("#{browser} #{url.shellescape}")
      }
    end
  elsif mode == 'gs' then
    system("#{browser} 'http://www.google.com/search?q=#{selection}'")
  elsif mode == 'gi' then
    system("#{browser} 'http://www.google.com/images?q=#{selection}'")
  elsif mode == 'gm' then
    system("#{browser} 'http://maps.google.com/?q=#{selection}'")
  end
end

#ENV['DISPLAY'] = ':'

system("pgrep -f '(firefox-bin|firefox.real)' > /dev/null")
browser = ($?.exitstatus == 0 && ARGV[1].nil?) ? "firefox" : "chromium"

selection = `xclip -o`

open(selection, browser, ARGV[0])

if selection.index("BEGIN PGP MESSAGE") then
  decoded = `echo '#{selection}' | gpg2 -d 2> /dev/null`.chomp
  open(decoded, browser, ARGV[0]) if not decoded.empty?
end
