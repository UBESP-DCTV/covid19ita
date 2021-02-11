name := covid19ita

all: update

update: build restart

restart: stop run

stop:
	docker stop $(name) || true

run:
	docker run -d --rm --name $(name) -v -p 13801:3838 $(name)

build:
	docker build -t $(name) .
