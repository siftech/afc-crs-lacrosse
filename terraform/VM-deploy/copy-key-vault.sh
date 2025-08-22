#!/usr/bin/env bash

# Defaults
ROUND="dev"
ORIG_KEYVAULT="SiftKeyVaultBackup"

# Parse arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    --round)
      shift
      ROUND="$1"
      ;;
    --origKeyVault)
      shift
      ORIG_KEYVAULT="$1"
      ;;
    *)
      echo "Unknown option: $1"
      echo "Usage: $0 [--round <roundValue>] [--origKeyVault <vaultName>]"
      exit 1
      ;;
  esac
  shift
done

echo "Round: $ROUND"
echo "Copying key vault: $ORIG_KEYVAULT"

NEW_KEYVAULT="SiftKeyVault${ROUND}"
echo "Name of the copied key vault: $NEW_KEYVAULT"

##############
# Make a copy of original key vault
##############

# Create a new key vault
az keyvault create --name $NEW_KEYVAULT --resource-group "sift-key-vault-rg"
KEYVAULT_ID=$(az keyvault show --name $NEW_KEYVAULT --query id -o tsv)

# Current user id
CURRENT_USER_OBJECT_ID=$(az ad signed-in-user show --query id -o tsv)

# assign Administrator role to current user
az role assignment create \
  --role "Key Vault Administrator" \
  --assignee-object-id "$CURRENT_USER_OBJECT_ID" \
  --scope "$KEYVAULT_ID" \
  --assignee-principal-type User

# Store secrets of the original key vault in a variable, JSON format
secrets=$(az keyvault secret list --vault-name $ORIG_KEYVAULT -o json)

# create a directory for secrets
mkdir secretsbackups_$ORIG_KEYVAULT

# Loop through each item in the JSON array
echo "$secrets" | jq -c '.[]' | while read -r secret_data; do
  name=$(echo "$secret_data" | jq -r '.name')
  secret_id=$(echo "$secret_data" | jq -r '.id')
  echo "Backup Secret ID:   $secret_id"
  az keyvault secret backup --file "secretsbackups_$ORIG_KEYVAULT/$name" --id $secret_id
done

# Check if role assignemnt has been made, wait until it is....
echo "Waiting for role assignment to propagate..."
for i in {1..20}; do
  ASSIGNMENT_COUNT=$(az role assignment list \
    --role "Key Vault Administrator" \
    --assignee "$CURRENT_USER_OBJECT_ID" \
    --scope "$KEYVAULT_ID" \
    --query "length(@)" \
    -o tsv)

  if [ "$ASSIGNMENT_COUNT" -gt 0 ]; then
    echo "Role assignment is recognized by Azure."
    break
  else
    echo "Role assignment not yet recognized. Retrying in 10s... ($i/20)"
    sleep 10
  fi
done

# Restore all backups in new key vault
# Loop through all files in the secretsbackups directory
for file in secretsbackups_$ORIG_KEYVAULT/*; do
  # Check if it's a file (not a directory)
  if [ -f "$file" ]; then
    echo "Restoring secret from $file into vault $NEW_KEYVAULT"
    az keyvault secret restore --file "$file" --vault-name $NEW_KEYVAULT
  fi
done

echo "*** Let's check:"

SECRET_COUNT_ORIG=$(az keyvault secret list --vault-name $ORIG_KEYVAULT --query "length(@)" -o tsv)
echo "Number of secrets in $ORIG_KEYVAULT: $SECRET_COUNT_ORIG"

SECRET_COUNT_NEW=$(az keyvault secret list --vault-name $NEW_KEYVAULT --query "length(@)" -o tsv)
echo "Number of secrets in $NEW_KEYVAULT: $SECRET_COUNT_NEW"

if [ "$SECRET_COUNT_ORIG" -ne "$SECRET_COUNT_NEW" ]; then
  echo "Error: secret counts do not match (orig $SECRET_COUNT_ORIG vs new $SECRET_COUNT_NEW)"
  exit 1
else
  echo "Success: secret counts match ($SECRET_COUNT_ORIG)"
fi
