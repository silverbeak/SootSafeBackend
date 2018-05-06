FROM aglover/java8-pier
MAINTAINER Kristofer Jarl <kristofer@sootsafe.com>

WORKDIR build

ADD build/distributions/sootsafe-backend.tar /

ENTRYPOINT ["/sootsafe-backend/bin/sootsafe-backend"]
#, "-cp", ".", "-Dconfig.file=/application.conf"]
