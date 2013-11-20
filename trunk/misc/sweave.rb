#!/usr/bin/ruby -w


################################################################################
#
# sweave.rb -- Ruby script to show problems in Sweave files.
#
# (C) 2013 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This program is distributed under the terms of the Gnu Public License V2.
# For further information, see http://www.gnu.org/licenses/gpl.html
#
################################################################################


require 'optparse'


class Sweave_Labels


  class << self


    def match str
      str.match(/^<<(.*)>>=([^\r\n]+)?$/)
    end


    private :match


    def read filename
      line_num, result = 0, []
      File.foreach filename do |line|
        line_num += 1
        found = match line
        next unless found
        result << Sweave_Label.new(found[1], filename, line_num, found[2])
      end
      new result
    end


    def filter filename
      line_num, result = 0, []
      File.foreach filename do |line|
        line_num += 1
        found = match line
        if found
          result << Sweave_Label.new(found[1], filename, line_num,
            found[2]).to_s
        else
          result << line.chomp
        end
      end
      result
    end


  end


  def initialize data
    unless data.all? {|item| item.is_a? Sweave_Label}
      raise ArgumentError, "need collection of Sweave label objects"
    end
    @data = data.to_a
  end


   def to_s
     @data.join "\n"
   end


   def figure_problems
     result, values = 0, {}
     @data.each {|item| values[item.label] = item}
     values.each_pair do |k, v|
       if k =~ /-dummy$/ and not values.has_key? k.sub(/-dummy$/, '-figure')
         yield v.msg 'dummy without figure counterpart' if block_given?
         result = false
       end
     end
     result
   end


   def duplicates
     values = Hash.new {|h, k| h[k] = []}
     @data.each {|item| values[item.label.downcase] << item}
     result = 0
     values.each_value do |items|
       next if items.length < 2
       result += 1
       if block_given?
         items.each {|item| yield item.msg("duplicate label")}
       end
     end
     result
   end


   private :duplicates, :figure_problems


   def consistent? &block
     if block_given?
       result = true
       @data.each do |item|
         result = false unless item.consistent?(&block)
       end
     else
       result = @data.all?(&:consistent?)
     end
     result & (duplicates(&block) == 0) & (figure_problems(&block) == 0)
   end


end


################################################################################


class Sweave_Label


  # Note that this does not allow the first option to be an unnamed label.
  #
  def parse_options options # :nodoc:
    Hash[options.strip.split(/\s*,\s*/).collect {|s| s.split(/\s*=\s*/)}]
  end


  private :parse_options


  def initialize options, filename, line_num, comment
    @filename, @line_num = filename.to_s, Integer(line_num)
    @options, @comment = parse_options(options), comment.to_s
    raise ArgumentError, msg("missing label") unless @options.has_key? 'label'
  end


  def to_s
    "<<#{@options.collect {|k, v| "#{k}=#{v}"}.sort.join(',')}>>=#{@comment}"
  end


  def label
    @options.fetch 'label'
  end


  def msg str
    "problem in file '#{@filename}', line #{@line_num} (#{self}): #{str}"
  end


  def method_missing name
    @options[name.to_s]
  end


  def consistent?
    result = true
    unless label =~ /^\w+(-\w+)*$/
      yield msg "malformed label" if block_given?
      result = false
    end
    if label =~ /\d$/
      yield msg "label ends in number" if block_given?
      result = false
    end
    if fig == 'TRUE'
      if @options['eval'] == 'FALSE'
        yield msg 'figure but not evaluated' if block_given?
        result = false
      end
      unless label =~ /-figure$/
        yield msg 'figure but wrong label' if block_given?
        result = false
      end
    elsif width or height
      yield msg "useless height and/or width" if block_given?
      result = false
    end
    if label =~ /-dummy$/ and @options['eval'] != 'FALSE'
      yield msg "evaluated but '-dummy' in label" if block_given?
      result = false
    end
    result
  end


end



################################################################################


help_msg = false
output = :problems


opts = OptionParser.new
opts.banner = "Show problems in Sweave files or filter them.\nOptions:"

opts.on(
    '-oMANDATORY STRING', '--output=MANDATORY STRING',
    'Main output mode (problems/labels/filter)',
    String,
    ['problems', 'labels', 'filter']
) {|v| output = v.to_sym}

opts.on(
    '-h', '--help', 'Display this help message'
) { |v| help_msg = true }


filenames = opts.parse ARGV


if filenames.empty? or help_msg
  warn opts.to_s
  exit 1
end


################################################################################


case output

when :filter
  filenames.each do |filename|
    puts Sweave_Labels.filter(filename).join("\n")
  end

when :labels
  filenames.each do |filename|
    puts Sweave_Labels.read(filename)
  end

when :problems
  problems = 0
  filenames.each do |filename|
    unless Sweave_Labels.read(filename).consistent? {|msg| warn msg}
      problems += 1
    end
  end
  if problems == 0
    print "\n*** Marvellous, no problems detected! ***\n\n"
  end
  exit problems

else
  raise ArgumentError, "unknown output mode: #{output}"

end



################################################################################



