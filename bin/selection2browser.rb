#! /usr/bin/ruby

require 'shellwords'
require 'uri'

## constants
URL_SCHEMES = ['http', 'ftp', 'https']
HTTP_REGEX = URI.regexp(URL_SCHEMES)
SEARCH_ENGINES = { "duckduckgo" => { :url => 'https://duckduckgo.com',
                                     :params => { :default => 'q',
                                                  :image => 'ia=images&iax=images',
                                                  :map => 'iaxm=maps' } },
                   "google"     => { :url => 'https://google.com',
                                     :params => { :default => 'q',
                                                  :image => 'tbm=isch',
                                                  :map => 'um=1' } } }

## functions
def getUrl(query, searchEngine, mode)
  se = SEARCH_ENGINES[searchEngine]
  params = se[:params]
  q = "#{params[:default]}=#{query}"
  url = "#{se[:url]}/?#{q}"

  url = "#{url}&#{extraParams}" unless mode == 'search'

  return url
end

def extractURLs(selection, searchEngine, mode)
  selection.gsub!(/\n\+/m, '') # handle mutt wrap markers

  urls = []

  if mode == nil or mode == "" then
    case selection
    when HTTP_REGEX
      URI::extract(selection, URL_SCHEMES).each { |url|
        urls << url.gsub(/\.$/, '')
      }
    when /^CVE-\d+-\d+/
      urls << "https://security-tracker.debian.org/tracker/#{selection}"
    when /^#\d+/
      urls << "https://bugs.debian.org/#{selection}"
    end
  end

  urls << getUrl(selection, searchEngine, "search") if urls.empty?

  return urls
end

def openInBrowser(url, browser)
  system("#{browser} #{url.shellescape}")
end

## main

# FIXME: --browser, --search-engine
browser = 'firefox'
searchEngine = 'duckduckgo'

selection = `xclip -o`
mode = ARGV[0]

if selection.index("BEGIN PGP MESSAGE") then
  decoded = `echo '#{selection}' | gpg2 -d 2> /dev/null`.chomp
  selection = decoded unless decoded.empty?
end

extractURLs(selection, searchEngine, mode).each do |url|
  openInBrowser(url, browser)
end
