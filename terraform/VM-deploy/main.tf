resource "random_pet" "rg_name" {
  prefix = var.resource_group_name_prefix
}

resource "azurerm_resource_group" "rg" {
  location = var.resource_group_location
  name     = random_pet.rg_name.id
}

# Create virtual network
resource "azurerm_virtual_network" "vmd_terraform_network" {
  name                = "vmdVnet"
  address_space       = ["10.0.0.0/16"]
  location            = azurerm_resource_group.rg.location
  resource_group_name = azurerm_resource_group.rg.name
}

# Create subnet
resource "azurerm_subnet" "vmd_terraform_subnet" {
  name                 = "vmdSubnet"
  resource_group_name  = azurerm_resource_group.rg.name
  virtual_network_name = azurerm_virtual_network.vmd_terraform_network.name
  address_prefixes     = ["10.0.1.0/24"]
}

# Bastion subnet
resource "azurerm_subnet" "bastion_subnet" {
  name                 = "AzureBastionSubnet"  # This name must be exactly this
  resource_group_name  = azurerm_resource_group.rg.name
  virtual_network_name = azurerm_virtual_network.vmd_terraform_network.name
  address_prefixes     = ["10.0.2.0/27"]  # at least a /27 CIDR block
}

# Public IP for bastion
resource "azurerm_public_ip" "bastion_public_ip" {
  name                = "bastion-public-ip"
  resource_group_name = azurerm_resource_group.rg.name
  location            = azurerm_resource_group.rg.location
  allocation_method   = "Static"
  sku                 = "Standard"  # Required for Bastion
}

# Bastion host
resource "azurerm_bastion_host" "bastion" {
  name                = "bastion-host"
  location            = azurerm_resource_group.rg.location
  resource_group_name = azurerm_resource_group.rg.name
  sku                 = "Standard"
  tunneling_enabled   = true

  ip_configuration {
    name                 = "bastion-ip-config"
    subnet_id            = azurerm_subnet.bastion_subnet.id
    public_ip_address_id = azurerm_public_ip.bastion_public_ip.id
  }
}

# Create Network Security Group (NSG) with SSH restriction
resource "azurerm_network_security_group" "vmd_terraform_nsg" {
  name                = "vmdNetworkSecurityGroup"
  location            = azurerm_resource_group.rg.location
  resource_group_name = azurerm_resource_group.rg.name

  security_rule {
    name                       = "Allow-SSH-From-VNet"
    priority                   = 1000
    direction                  = "Inbound"
    access                     = "Allow"
    protocol                   = "Tcp"
    source_port_range          = "*"
    destination_port_range     = "22"
    source_address_prefix      = "VirtualNetwork"
    destination_address_prefix = "*"
  }
}

# Create crs network interface (without public IP)
resource "azurerm_network_interface" "crs_nic" {
  name                = "crsNIC"
  location            = azurerm_resource_group.rg.location
  resource_group_name = azurerm_resource_group.rg.name

  ip_configuration {
    name                          = "crs_nic_configuration"
    subnet_id                     = azurerm_subnet.vmd_terraform_subnet.id
    private_ip_address_allocation = "Dynamic"
  }

  accelerated_networking_enabled = true
}

# Create fb network interface (without public IP)
resource "azurerm_network_interface" "fb_nic" {
  count               = 2
  name                = "fbNIC${count.index}"
  location            = azurerm_resource_group.rg.location
  resource_group_name = azurerm_resource_group.rg.name

  ip_configuration {
    name                          = "fb_nic_configuration"
    subnet_id                     = azurerm_subnet.vmd_terraform_subnet.id
    private_ip_address_allocation = "Dynamic"
  }

  accelerated_networking_enabled = true
}

# Connect the security group to the network interface
resource "azurerm_network_interface_security_group_association" "crs_nisga" {
  network_interface_id      = azurerm_network_interface.crs_nic.id
  network_security_group_id = azurerm_network_security_group.vmd_terraform_nsg.id
}

# Connect the security group to the network interface
resource "azurerm_network_interface_security_group_association" "fb_nisga" {
  count                     = 2
  network_interface_id      = azurerm_network_interface.fb_nic[count.index].id
  network_security_group_id = azurerm_network_security_group.vmd_terraform_nsg.id
}

# Generate random text for a unique storage account name
resource "random_id" "random_id" {
  keepers = {
    # Generate a new ID only when a new resource group is defined
    resource_group = azurerm_resource_group.rg.name
  }
  byte_length = 8
}

# Create storage account for boot diagnostics
resource "azurerm_storage_account" "vmd_storage_account" {
  name                     = "diag${random_id.random_id.hex}"
  location                 = azurerm_resource_group.rg.location
  resource_group_name      = azurerm_resource_group.rg.name
  account_tier             = "Standard"
  account_replication_type = "LRS"
}

# Create NetApp Account
resource "azurerm_netapp_account" "netapp_account" {
  name                = "netappacct-${random_pet.rg_name.id}"
  location            = azurerm_resource_group.rg.location
  resource_group_name = azurerm_resource_group.rg.name
}

# Create NetApp Pool
resource "azurerm_netapp_pool" "netapp_pool" {
  name                = "netapppool"
  location            = azurerm_resource_group.rg.location
  resource_group_name = azurerm_resource_group.rg.name
  account_name        = azurerm_netapp_account.netapp_account.name
  service_level       = "Premium"
  size_in_tb          = 25
}

# Create Subnet for NetApp
resource "azurerm_subnet" "anf_delegated" {
  name                 = "anf-subnet"
  resource_group_name  = azurerm_resource_group.rg.name
  virtual_network_name = azurerm_virtual_network.vmd_terraform_network.name
  address_prefixes     = ["10.0.3.0/24"]
  delegation {
    name = "netappdelegation"

    service_delegation {
      name = "Microsoft.Netapp/volumes"
      actions = [
        "Microsoft.Network/networkinterfaces/*",
        "Microsoft.Network/virtualNetworks/subnets/join/action"
      ]
    }
  }
}

# Create NetApp Volume
resource "azurerm_netapp_volume" "netapp_volume" {
  name                = "netappvolume"
  resource_group_name = azurerm_resource_group.rg.name
  account_name        = azurerm_netapp_account.netapp_account.name
  pool_name           = azurerm_netapp_pool.netapp_pool.name
  location            = azurerm_resource_group.rg.location
  volume_path         = "netappnfs"
  protocols           = ["NFSv3"]
  service_level       = "Premium"
  subnet_id           = azurerm_subnet.anf_delegated.id
  storage_quota_in_gb = 25600
  export_policy_rule {
    rule_index        = 1
    allowed_clients   = ["0.0.0.0/0"]
    protocols_enabled = ["NFSv3"]
    unix_read_write   = true
    root_access_enabled = true  # Allow root access for proper permissions management
  }
}

# declare locals
locals {
  git_url = "https://${var.username}:${data.azurerm_key_vault_secret.lacrosse_github_pat.value}@github.com/aixcc-finals/afc-crs-lacrosse.git"
  sp_app_id = data.azurerm_key_vault_secret.sp_app_id.value
  sp_passwd = data.azurerm_key_vault_secret.sp_passwd.value
  lacrosse_home = "/home/${var.username}/git-repo/crs"
  tailscale_auth_key = data.azurerm_key_vault_secret.tailscale_auth_key.value
  tailscale_hostname = data.azurerm_key_vault_secret.tailscale_hostname.value
  gemini_api_key = data.azurerm_key_vault_secret.gemini_api_key.value
  anthropic_api_key = data.azurerm_key_vault_secret.anthropic_api_key.value
  openai_api_key = data.azurerm_key_vault_secret.openai_api_key.value
  competition_api_key_id = data.azurerm_key_vault_secret.competition_api_key_id.value
  competition_api_key_token = data.azurerm_key_vault_secret.competition_api_key_token.value
  crs_api_key_id = data.azurerm_key_vault_secret.crs_api_key_id.value
  crs_api_key_token = data.azurerm_key_vault_secret.crs_api_key_token.value
  competition_api_team_id = data.azurerm_key_vault_secret.competition_api_team_id.value
  competition_api_team_secret = data.azurerm_key_vault_secret.competition_api_team_secret.value
  competition_api_endpoint = data.azurerm_key_vault_secret.competition_api_endpoint.value
  competition_api_team_endpoint = data.azurerm_key_vault_secret.competition_api_team_endpoint.value
  azure_ssh_private_key = data.azurerm_key_vault_secret.ssh_private_key.value
  azure_ssh_public_key = data.azurerm_key_vault_secret.ssh_public_key.value
  lacrosse_github_pat = data.azurerm_key_vault_secret.lacrosse_github_pat.value
  otel_exporter_otlp_headers = data.azurerm_key_vault_secret.otel_exporter_otlp_headers.value
}

# Create virtual machine
resource "azurerm_linux_virtual_machine" "crs_vm" {
  name                  = "pixel-phantom"
  location              = azurerm_resource_group.rg.location
  resource_group_name   = azurerm_resource_group.rg.name
  network_interface_ids = [azurerm_network_interface.crs_nic.id]
  size                  = "Standard_D128ds_v6"

  os_disk {
    name                 = "crsDisk"
    caching              = "ReadWrite"
    storage_account_type = "Premium_LRS"
    # consider increasing for the next round
    disk_size_gb         = 2048
  }

  source_image_reference {
    publisher = "Canonical"
    offer     = "0001-com-ubuntu-server-jammy"
    sku       = "22_04-lts-gen2"
    version   = "latest"
  }

  computer_name  = "pixel-phantom"
  admin_username = var.username

  admin_ssh_key {
    username   = var.username
    public_key = data.azurerm_key_vault_secret.ssh_public_key.value
  }

  boot_diagnostics {
    storage_account_uri = azurerm_storage_account.vmd_storage_account.primary_blob_endpoint
  }

  # Inject cloud-init script into the VM using custom_data (must be base64-encoded)
  custom_data = base64encode(templatefile("${path.module}/setup-script.sh", {
      USERNAME = var.username
      GIT_URL = local.git_url
      SP_APP_ID = local.sp_app_id
      SP_PASSWD = local.sp_passwd
      LACROSSE_HOME = local.lacrosse_home
      TAILSCALE_AUTH_KEY = local.tailscale_auth_key
      TAILSCALE_HOSTNAME = local.tailscale_hostname
      GEMINI_API_KEY = local.gemini_api_key
      ANTHROPIC_API_KEY = local.anthropic_api_key
      OPENAI_API_KEY = local.openai_api_key
      COMPETITION_API_KEY_ID = local.competition_api_key_id
      COMPETITION_API_KEY_TOKEN = local.competition_api_key_token
      CRS_API_KEY_ID = local.crs_api_key_id
      CRS_API_KEY_TOKEN = local.crs_api_key_token
      COMPETITION_API_TEAM_ID = local.competition_api_team_id
      COMPETITION_API_TEAM_SECRET = local.competition_api_team_secret
      COMPETITION_API_ENDPOINT = local.competition_api_endpoint
      COMPETITION_API_TEAM_ENDPOINT = local.competition_api_team_endpoint
      ANF_IP   = azurerm_netapp_volume.netapp_volume.mount_ip_addresses[0]
      SSH_PRIVATE_KEY = local.azure_ssh_private_key
      SSH_PUBLIC_KEY = local.azure_ssh_public_key
      LACROSSE_GITHUB_PAT = local.lacrosse_github_pat
      OTEL_EXPORTER_OTLP_HEADERS = local.otel_exporter_otlp_headers
  }))

  depends_on = [
    azurerm_netapp_volume.netapp_volume
  ]
}

# Create fb virtual machine(s)
resource "azurerm_linux_virtual_machine" "fb_vm" {
  count                 = 2
  name                  = "pixel-phantom-fb${count.index}"
  location              = azurerm_resource_group.rg.location
  resource_group_name   = azurerm_resource_group.rg.name
  network_interface_ids = [azurerm_network_interface.fb_nic[count.index].id]
  size                  = "Standard_D128ds_v6"

  os_disk {
    name                 = "fbDisk${count.index}"
    caching              = "ReadWrite"
    storage_account_type = "Premium_LRS"
    # consider increasing for the next round
    disk_size_gb         = 2048
  }

  source_image_reference {
    publisher = "Canonical"
    offer     = "0001-com-ubuntu-server-jammy"
    sku       = "22_04-lts-gen2"
    version   = "latest"
  }

  computer_name  = "pixel-phantom-fb${count.index}"
  admin_username = var.username

  admin_ssh_key {
    username   = var.username
    public_key = data.azurerm_key_vault_secret.ssh_public_key.value
  }

  boot_diagnostics {
    storage_account_uri = azurerm_storage_account.vmd_storage_account.primary_blob_endpoint
  }

  # Inject cloud-init script into the VM using custom_data (must be base64-encoded)
  custom_data = base64encode(templatefile("${path.module}/setup-script.sh", {
      USERNAME = var.username
      GIT_URL = local.git_url
      SP_APP_ID = local.sp_app_id
      SP_PASSWD = local.sp_passwd
      LACROSSE_HOME = local.lacrosse_home
      TAILSCALE_AUTH_KEY = local.tailscale_auth_key
      TAILSCALE_HOSTNAME = local.tailscale_hostname
      GEMINI_API_KEY = local.gemini_api_key
      ANTHROPIC_API_KEY = local.anthropic_api_key
      OPENAI_API_KEY = local.openai_api_key
      COMPETITION_API_KEY_ID = local.competition_api_key_id
      COMPETITION_API_KEY_TOKEN = local.competition_api_key_token
      CRS_API_KEY_ID = local.crs_api_key_id
      CRS_API_KEY_TOKEN = local.crs_api_key_token
      COMPETITION_API_TEAM_ID = local.competition_api_team_id
      COMPETITION_API_TEAM_SECRET = local.competition_api_team_secret
      COMPETITION_API_ENDPOINT = local.competition_api_endpoint
      COMPETITION_API_TEAM_ENDPOINT = local.competition_api_team_endpoint
      ANF_IP   = azurerm_netapp_volume.netapp_volume.mount_ip_addresses[0]
      SSH_PRIVATE_KEY = local.azure_ssh_private_key
      SSH_PUBLIC_KEY = local.azure_ssh_public_key
      LACROSSE_GITHUB_PAT = local.lacrosse_github_pat
      OTEL_EXPORTER_OTLP_HEADERS = local.otel_exporter_otlp_headers
  }))

  depends_on = [
    azurerm_netapp_volume.netapp_volume
  ]
}
