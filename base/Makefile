include Makefile.config

.PHONY: build push

build:
	docker build -t $(NS)/$(REPO):$(VERSION) .

push:
	docker push $(NS)/$(REPO):$(VERSION)

run:
	docker run --rm --name $(NAME)-shell  $(NS)/$(REPO):$(VERSION)

shell:
	docker run --rm --name $(NAME)-shell  -v $(shell pwd)/ci:/ci  -v $(shell pwd)/k8s:/k8s -it $(NS)/$(REPO):$(VERSION) bash
