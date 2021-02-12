include Makefile.config

.PHONY: build push

build:
	docker build --build-arg BUILDHOST="$(shell hostname -f)" -t $(NS)/$(REPO):$(VERSION) .

push:
	docker push $(NS)/$(REPO):$(VERSION)

run:
	docker run --rm -it -p 8050:8050 --name $(NAME)-run -e BIND_ALL_IPS=true -v $(shell pwd)/webapp:/opt/webapp/webapp $(NS)/$(REPO):$(VERSION) python3 web.py

shell:
	docker run --rm --name $(NAME)-shell -it  -v $(shell pwd)/webapp:/opt/webapp/webapp -v $(shell pwd)/queue_model:/opt/webapp/queue_model $(NS)/$(REPO):$(VERSION) bash
