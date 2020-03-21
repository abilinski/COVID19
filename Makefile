include Makefile.config

.PHONY: build push

build:
	docker build -t $(NS)/$(REPO):$(VERSION) .

push:
	docker push $(NS)/$(REPO):$(VERSION)

run:
	docker run --rm -it -p 8080:8080 --name $(NAME)-run -e PORT=8080 $(NS)/$(REPO):$(VERSION)

shell:
	docker run --rm --name $(NAME)-shell -it $(NS)/$(REPO):$(VERSION) bash
