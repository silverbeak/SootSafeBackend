FROM aglover/java8-pier
MAINTAINER Kristofer Jarl <kristofer@sootsafe.com>

WORKDIR build

RUN apt-get update
RUN apt-get install -y latexmk texlive-latex3 texlive-science texlive-latex-extra texlive-fonts-recommended
# texlive-math-extra texlive-generic-extra texlive-pictures

ADD distributions/sootsafe-backend.tar /
RUN mkdir -p temp/sootsafe
ENTRYPOINT ["/sootsafe-backend/bin/sootsafe-backend"]
# ENTRYPOINT ["nginx", "-g", "daemon off;"]
