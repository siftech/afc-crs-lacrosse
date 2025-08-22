.PHONY: all init plan up

all: up

init:
	terraform init

plan:
	terraform plan -out main.tfplan

apply:
	terraform apply main.tfplan
	@echo "Configuring Lacrosse CRS..."
	sleep 1800
	@echo "Lacrosse CRS Configuration complete!"

up: init plan apply
