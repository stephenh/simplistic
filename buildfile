require 'buildr/scala'

VERSION_NUMBER = "1.0.17-SNAPSHOT"

repositories.remote << "http://www.ibiblio.org/maven2/"
repositories.remote << "http://repo1.maven.org/maven2/"

HTTPCLIENT = 'org.apache.httpcomponents:httpclient:jar:4.1.1'
HTTPCORE = 'org.apache.httpcomponents:httpcore:jar:4.1'
COMMONSLOG = 'commons-logging:commons-logging:jar:1.1.1'

CODEC = 'commons-codec:commons-codec:jar:1.3'

SLF4J_VERSION = "1.5.6"
SLF4J = [
  "org.slf4j:slf4j-api:jar:#{SLF4J_VERSION}",
  "org.slf4j:slf4j-log4j12:jar:#{SLF4J_VERSION}",
  "org.slf4j:jcl-over-slf4j:jar:#{SLF4J_VERSION}"
]
LOG4J = "log4j:log4j:jar:1.2.15"

FAKESDB = "fakesdb:fakesdb-testing:jar:2.4"

download(artifact(FAKESDB) => 'https://github.com/downloads/stephenh/fakesdb/fakesdb-testing-2.4.jar')

desc 'Scala binding for Amazon SimpleDB'
define "simplistic" do
  project.version = VERSION_NUMBER
  project.group = "simplistic"

  compile.with HTTPCORE, HTTPCLIENT, CODEC, COMMONSLOG
  compile.using :deprecation => true,
                :other => ['-unchecked', '-Xprint-types']

  test.using :scalatest
  test.with FAKESDB, LOG4J, SLF4J

  doc.using :scaladoc

  package :jar
  package :scaladoc
  package :sources
end

