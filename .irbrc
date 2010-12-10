# -*- ruby -*-
ARGV.concat ["--readline"] unless ARGV.include? "--noreadline"

def require_maybe(file)
  begin
    require file
    yield if block_given?
  rescue LoadError => e
    puts e
  end
end

require_maybe 'rubygems'
require_maybe 'wirble' do
  Wirble.init
  Wirble.colorize
end

module Readline
  module History
    LOG = "#{ENV['HOME']}/.irb-history"

    def self.write_log(line)
      File.open(LOG, 'ab') {|f| f << "#{line}\n"}
    end

    def self.start_session_log
      write_log("\n# session start: #{Time.now}\n\n")
      at_exit { write_log("\n# session stop: #{Time.now}\n") }
    end
  end

  alias :old_readline :readline
  def readline(*args)
    ln = old_readline(*args)
    begin
      History.write_log(ln)
    rescue
    end
    ln
  end
end

Readline::History.start_session_log

require 'irb/ext/save-history'
IRB.conf[:SAVE_HISTORY] = 100
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb-save-history"

IRB.conf[:PROMPT][:CUSTOM] = {
  :PROMPT_I => ">> ",
  :PROMPT_S => "%l>> ",
  :PROMPT_C => ".. ",
  :PROMPT_N => ".. ",
  :RETURN => "=> %s\n"
}
IRB.conf[:PROMPT_MODE] = :CUSTOM
IRB.conf[:AUTO_INDENT] = true

require_maybe 'awesome_print' do
  IRB::Irb.class_eval do
    def output_value
      ap @context.last_value, :multiline => false
    end
  end
end
require_maybe 'what_methods'
require_maybe 'map_by_method'

if defined? Rails
  require 'logger'
  ActiveRecord::Base::logger = Logger.new(STDOUT)
end

puts <<TRICKS
You can use these tricks:
* accessing the last returning value by _
* calling controller actions: app.get '/'; app.response
* IRB sub-sessions: irb [context], jobs, fg #, kill #, exit
* RI: [].ri_each
* YAMLize by "y obj"
TRICKS
