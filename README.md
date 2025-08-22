# Lacrosse
LLM+AI Cyber Reasoning Security System.
This repositories contains SIFT's LACROSSE, the adaptive Cyber Reasoning System (CRS) as submitted to the AIxCC Final Competition (AFC).

LACROSSE is a multi-agent CRS combining fuzzing and symbolic reasoning to find and patch bugs across various C and Java software codebases.  

## Prerequisites
- Azure Subscription
- Azure CLI
- Terraform

## File Structure
The [Lacrosse source code](https://github.com/aixcc-finals/afc-crs-lacrosse) the following important directories:
- `crs` containing everything needed to run the Lacrosse system including Dockerfiles, testing suites, tools, and agents.
- `terraform` containing everything needed for terraform to deploy the Lacrosse CRS on Azure.

## Deploying Lacrosse
Before starting the Lacrosse CRS, please ensure all necessary keys and tokens are stored inside of
a reachable Azure vault and azure-bashrc.
For steps on how to do this, see [Azure Tokens](#azure-tokens) and [azure-bashrc](#azure-bashrc).

1. Clone the afc-crs-lacrosse repository.

2. Login to Azure `az login --tenant aixcc.tech` NOTE: in the CLI you might need `az login --tenant aixcc.tech --use-device-code`

3. Add the following environment variables to your `.bashrc` to allow Terraform to log in to Azure:

```
export ARM_CLIENT_ID="<appID-value>"
export ARM_CLIENT_SECRET="<password-value>"
export ARM_TENANT_ID="<tenant-id>"
export ARM_SUBSCRIPTION_ID="<subscription-id>"
```
See [Authentication using the Azure CLI](https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs/guides/azure_cli) for more information.

4. Navigate within the afc-crs-lacrosse repository to `terraform/VM-deploy`.

5. Run `make up`.

## Terraform configuration

Within the lacrosse `terraform/VM-deploy` directory, there are the following terraform configuration files:

- `main.tf` provisions 3 VMs (a CRS VM, and 2 fuzzing (FB) VMs) under machine class "Standard_D128ds_v6" for a multi-host Lacrosse CRS.
- `data.tf` highlights secrets and tokens required from the azure key vault for initializing Lacrosse.
- `backend.tf` highlights the backend storage account. _Optional_ change this to a storage account for your backups.
- `outputs.tf` saves the resource group and the bastion ip.
- `providers.tf` terraform/azure providers (i.e hashicorp).
- `ssh.tf` extra azure ssh configurations.
- `variables.tf` extra azure variable configurations.

Inside of `main.tf`, a `setup-script.sh` is executed to configure everything needed for the VMs and start
an instance of the lacrosse CRS that will begin initializing it agents.


Once the `terraform apply` has completed Lacrosse will take roughly 15 minutes to setup and
connect agents. After the agents are up and connected, lacrosse will begin working on tasks.

## Azure Tokens
`data.tf` details various keys with values stored in an Azure key vault for servicing the Lacrosse CRS.

1. Create a new [Azure Key Vault](https://learn.microsoft.com/en-us/azure/key-vault/) with the following keys:

The following keys are AIxCC specific:
- `CompetitionApiTeamEndpoint`, `CompetitionApiEndpoint` Set to the endpoint of the competition API.
- `CompetitionApiKeyId`, `CompetitionApiTeamID` Set to the ID of the competition server.
- `CompetitionApiTeamSecret`, `CompetitionApiKeyToken` Set to the secret of the compeition server.
- `CrsApiKeyID` Set to the ID of the Lacrosse CRS Api.
- `CrsApiKeyToken` Set to the secret of the Lacrosse CRS Api.
- `TailscaleAuthKey` Set to the authorization key for the TailScale network.
- `TailscaleHostname` Set to the hostname expected of the TailScale network.
- `LacrosseGitHubPat` GitHub PAT token used to pull AIxCC Finals images.
- `OtelExporterOtlpHeaders` Set to the authentication headers (base64) for Telemetry.

The following keys are Lacrosse specific:
- `ServicePrincipalAppId` Set to ID for pulling from Azure Container Registry.
- `ServicePrincipalSecret` Set to Secret for pulling from Azure Container Registry.
- `VmdPublicSSHKey` SSH public key for remote access and multi-host comms within Lacrosse.
- `VmdPrivateSSHKey` SSH private key for remote access and multi-host comms within Lacrosse.

The following keys are LLM specific:
- `GeminiKey` Set to the API key for Gemini.
- `AnthropicKey` Set to the API key for Anthropic.
- `OpenAIKey` Set to the API key for OpenAI.

## azure-bashrc
Keys need to be set in the Azure Key Vault (as described in the section above)
and included in `crs/code/tools/azure-bashrc` so that every lacrosse VM can access these shared values.

Review the azure-bashrc template at `crs/code/tools/azure-bashrc-template`.
The Terraform setup will replace the placeholder `"<values>"` defined inside the template.
Any hardcoded values including endpoints, azure api base, otlp protocol, and the lacrosse release tag may be changed to match user preferences.

The Lacrosse CRS copies from `crs/code/tools/azure-bashrc` into its VM(s) and agent(s) for use on system startup.

## Docker configurations
Terraform has all the necessary initialization material for spinning up Lacrosse's `rootless-docker` images.
Currently, the images used for the Lacrosse CRS are built and added to the Azure Registry.
During AIxCC, these images are pulled from the Azure Registry within the Terraform init.

These docker images have been added to the afc-crs-lacrosse GitHub Registry as well and will be pulled by the
Lacrosse setup system.

To build the docker images locally, instead of pulling,
1. From the lacrosse parent directory (parent to `crs/`), run `make build`

2. Navigate to `crs/code/docker/afc-crs-server` and run `DOCKER_TAG=latest make`

For more information about these images, see the `code/docker/afc-crs-server` and `code/docker/neo-fuzz-ccl` Dockerfiles.

## Destroying Lacrosse
Run `terraform plan -destroy -out main.destroy.tfplan` followed by a `terraform apply main.destroy.tfplan` to preview and commit to destroying the active CRS.
Otherwise, run `terraform destroy`.
