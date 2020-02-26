#! /usr/bin/ruby

require 'json'
require 'net/http'
require 'net/https'
require 'optparse'
require 'shellwords'
require 'uri'

## constants
URL_SCHEMES = ['http', 'ftp', 'https']
HTTP_REGEX = URI.regexp(URL_SCHEMES)
SEARCH_ENGINES = { "ddg"    => { :url => 'https://duckduckgo.com',
                                 :params => { :default => 'q',
                                              :image => 'ia=images&iax=images',
                                              :map => 'iaxm=maps' } },
                   "osm"    => { :url => 'https://www.openstreetmap.org/search',
                                 :params => { :default => 'query',
                                              :image => nil,
                                              :map => '' } },
                   "google" => { :url => 'https://google.com',
                                 :params => { :default => 'q',
                                              :image => 'tbm=isch',
                                              :map => 'um=1' } } }

TRIMREAD_URL = 'https://beta.trimread.com/'
TRIMREAD_URI = URI.parse(TRIMREAD_URL)
TRIMREAD_HTTPS = Net::HTTP.start(TRIMREAD_URI.host, TRIMREAD_URI.port, :use_ssl => true)

## functions
def doTrimreadPost(url)
  req = Net::HTTP::Post.new(TRIMREAD_URI.path)
  req.set_form_data({'url' => url})
  response = TRIMREAD_HTTPS.request(req)
  exit(1) unless response.code == '200'
  return response.body
end

def trimreadUrl(body)
  body =~ %r'(#{TRIMREAD_URL}articles/\d+)'
  return $1
end

def getUrl(query, searchEngine, mode)
  if mode == 'trimread'
    return trimreadUrl(doTrimreadPost(query))
  end

  se = SEARCH_ENGINES[searchEngine]
  params = se[:params]
  q = "#{params[:default]}=#{query}"
  url = "#{se[:url]}/?#{q}"

  url = "#{url}&#{params[mode.to_sym]}" unless mode == 'search'

  return url
end

def extractURLs(selection, searchEngine, mode)
  urls = []

  if mode.nil? or mode.empty? then
    case selection
    when HTTP_REGEX
      URI::extract(selection, URL_SCHEMES).each { |url|
        urls << url.gsub(/\.$/, '')
      }
    when /^CVE-\d+-\d+/
      urls << "https://security-tracker.debian.org/tracker/#{selection}"
    when /^#\d+{6,}/
      urls << "https://bugs.debian.org/#{selection}"
    end
  end

  # use search engine if nothing was extracted
  urls << getUrl(selection, searchEngine, mode) if urls.empty?

  return urls
end

def openInBrowser(url, browser)
  system("#{browser} #{url.shellescape}")
end

def parseArgs(options)
  parser = OptionParser.new do|opts|
    opts.on('-b', '--browser b') { |b| options[:browser] = b }
    opts.on('-m', '--mode m') { |m| options[:mode] = m }
    opts.on('-s', '--search-engine s') { |s| options[:searchEngine] = s }
    opts.on('-h', '--help') { puts opts ; exit }
  end

  parser.parse!

  return options
end

def decrypt(text)
  decoded = `echo '#{text}' | gpg2 -d 2> /dev/null`.chomp
  return decoded.empty? ? text : decoded
end

## main

# CLI args
options = { :browser => 'firefox',
            :searchEngine => 'ddg',
            :mode => 'search' }
options = parseArgs(options)

# selection from clipboard
selection = `xclip -o`
selection.gsub!(/\n\+/m, '') # handle mutt wrap markers

# try decoding
selection = decrypt(selection) if selection.index("BEGIN PGP MESSAGE")

extractURLs(selection, options[:searchEngine], options[:mode]).each do |url|
  openInBrowser(url, options[:browser])
end
