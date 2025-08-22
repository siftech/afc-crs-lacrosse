data "azurerm_key_vault" "sift_key_vault" {
  name                = "SiftKeyVaultBackup"
  resource_group_name = "sift-key-vault-rg"
}

data "azurerm_key_vault_secret" "ssh_public_key" {
  name         = "VmdPublicSSHKey"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "ssh_private_key" {
  name         = "VmdPrivateSSHKey"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

# using this for AIxCC GitHub
# data "azurerm_key_vault_secret" "gh_token_aixcc" {
#   name         = "GitHubToken"
#   key_vault_id = data.azurerm_key_vault.sift_key_vault.id
# }

# using this for SIFT's Gitlab
data "azurerm_key_vault_secret" "gh_token_sift" {
  name         = "SiftGitlabLaxToken"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "lacrosse_github_pat" {
  name         = "LacrosseGithubPat"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "otel_exporter_otlp_headers" {
  name         = "OtelExporterOtlpHeaders"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "sp_app_id" {
  name         = "ServicePrincipalAppId"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "sp_passwd" {
  name         = "ServicePrincipalSecret"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "tailscale_auth_key" {
  name         = "TailscaleAuthKey"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "tailscale_hostname" {
  name         = "TailscaleHostname"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "gemini_api_key" {
  name         = "GeminiKey"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "anthropic_api_key" {
  name         = "AnthropicKey"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "openai_api_key" {
  name         = "OpenAIKey"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "competition_api_key_id" {
  name         = "CompetitionApiKeyId"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "competition_api_key_token" {
  name         = "CompetitionApiKeyToken"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "crs_api_key_id" {
  name         = "CrsApiKeyID"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "crs_api_key_token" {
  name         = "CrsApiKeyToken"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "competition_api_team_id" {
  name         = "CompetitionApiTeamID"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "competition_api_team_secret" {
  name         = "CompetitionApiTeamSecret"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "competition_api_endpoint" {
  name         = "CompetitionApiEndpoint"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}

data "azurerm_key_vault_secret" "competition_api_team_endpoint" {
  name         = "CompetitionApiTeamEndpoint"
  key_vault_id = data.azurerm_key_vault.sift_key_vault.id
}
