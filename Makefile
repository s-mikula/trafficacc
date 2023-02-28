OWNER=smikula/
IMAGENAME=trafficacc
VERSIONNAME=public
DOCKERDEPS = trafficacc/app.R renv.lock

info:
	@echo "To build the docker file, say 'make docker'"

docker:	Dockerfile $(DOCKERDEPS)
	docker build -t $(OWNER)$(IMAGENAME):$(VERSIONNAME) .
