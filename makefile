name := covid19ita


update: build restart

restart: stop run

stop:
	docker stop $(name) || true

run:
	docker run -d --rm --name $(name) -v -p 13801:3838 $(name)

build:
	docker build -t $(name) .

data-update:
	R -e "remotes::install_deps()" && \
	Rscript data-raw/data-UPDATE.R && \
	git commit -am "data auto-update" && \
	git push && \
	ssh root@147.162.76.187 "R -e \"remotes::install_github('UBESP-DCTV/covid19ita')\"" && \
	echo "done"
