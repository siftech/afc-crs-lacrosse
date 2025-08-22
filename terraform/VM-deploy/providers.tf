terraform {
  required_version = ">=1.0"

  required_providers {
    azapi = {
      source  = "azure/azapi"
      version = ">=2.0.1"
    }
    azurerm = {
      source  = "hashicorp/azurerm"
      version = ">=4.7.0"
    }
    random = {
      source  = "hashicorp/random"
      version = ">=3.6.3"
    }
  }
}

provider "azurerm" {
  features {}
}
