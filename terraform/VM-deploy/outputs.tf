output "resource_group_name" {
  value = azurerm_resource_group.rg.name
}

output "bastion_host_ip" {
  value = azurerm_public_ip.bastion_public_ip.ip_address
}
