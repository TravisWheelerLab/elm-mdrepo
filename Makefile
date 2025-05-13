SHELL := /bin/bash

server:
	source .env && elm-land server

build:
	source $(shell pwd)/.env && elm-land build
