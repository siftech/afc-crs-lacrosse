# Need to update storage account name for each round
# backend does not support variables as it's loaded very early
terraform {
  backend "azurerm" {
    resource_group_name  = "tfstate-rg"
    storage_account_name = "tfstatesift2025"
    container_name       = "tfstate"
    key                  = "terraform.tfstate"
  }
}
