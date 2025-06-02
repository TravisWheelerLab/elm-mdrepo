SHELL := /bin/bash
TAG = elm-mdrepo
PLATFORM = linux/amd64

server:
	source .env && elm-land server

build:
	source $(shell pwd)/.env && elm-land build

img:
	docker build --platform $(PLATFORM) --tag $(TAG) .

run:
	docker run --platform $(PLATFORM) $(TAG)
