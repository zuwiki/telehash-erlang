#!/usr/bin/env ruby
require 'rubygems'
require 'fsevents'

stream = FSEvents::Stream.watch('src') { |events|
  print `echo ; echo "**** $(date) ****"`
  print `erl -make ; erl -pa ebin -noinput -run telehash_test`
}
stream.run