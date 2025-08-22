# Example Competition server at SIFT tl;dr.

Have a PAT key ...

_Snippet from readme at: https://github.com/aixcc-finals/example-crs-architecture/tree/main/example-competition-server_
### GitHub Container Registry

You must login to the GitHub Container Registry to access the example competition server container.

You will need to have a GitHub personal access token (PAT) scoped to at least `read:packages`.

To create the PAT, go to your account, `Settings` > `Developer settings` > `Personal access tokens`, and generate a Token (classic) with the scopes needed for your use case.

For this example, the `read:packages` scope is required. If you'd like, you may also enable the `repo` scope so you can use the same token for the container registry and the competition test server (see the [scantron.yaml](#scantronyaml)
section below).

Once you have your PAT, set it to an environment variable on your machine like this

```bash
export CR_PAT=YOUR_TOKEN_HERE
```

## Pull the docker image
Finally, you must login to the registry, like so:

```bash
$ echo $CR_PAT | docker login ghcr.io -u USERNAME --password-stdin
> Login Succeeded
```

To check if it succeeded, try running the following:

```bash
docker pull --platform=linux/amd64 ghcr.io/aixcc-finals/example-crs-architecture/competition-test-api:v1.2-rc1
```

Docker will store your credentials in your OS's native keystore, so you should only have to run `docker login` on subsequent logins into the GitHub Container Repository

## Check the env vars in ex-comp-server-env make sure they are set in your environment.
You will see the LACROSSE_GITHUB_PAT, set this in your env to the newly created PAT key for getting AIxCC repo stuff.
We might not want to use real creds to we...
*Set CRS_API_KEY_ID to "lax-username"*
*Set CRS_API_KEY_TOKEN to "lax-password"*
as the `run-with-docker` tool does by default. This might, however, need to also be done in `afc-start-crs-api-server` in your env.

## Use the run-with-docker script.
This will bring up a pretty minimized lightweight version of the end-to-end competition example infra.

To bring up the full infra use `docker compose up`.
This may or may not include the signoz web ui, you can check this by looking at the
"include" statement at the top of the compose.yaml

## In a new terminal on the host, use the trigger-comp-server-to-task script.
This will send a tasking payload to the running example competition server triggering it to build and send the task to our CRS.

### tl;dr

1. Get a github personal access token _classic_ with `read:packages` and `repo`. Save to env as `LACROSSE_GITHUB_PAT`.
_(PAT should be made for account with access to AIxCC stuff)_

2. Do a login via
```
echo $LACROSSE_GITHUB_PAT | docker login ghcr.io -u USERNAME --password-stdin
```

3. Pull this image via `docker pull --platform=linux/amd64 ghcr.io/aixcc-finals/example-crs-architecture/competition-test-api:v1.2-rc1`

4. Make sure you have set the correct environment vars, check for LACROSSE_GITHUB_PAT. source `ex-comp-server-env`.
Under `ex-comp-server-env` there is a `OTEL_EXPORTER_OTLP_HEADERS` which decodes to `base64 ("lax-username":"lax-password"). These should be set as default in the `tools/afc-start-crs-api-server`, so make sure you are not overriding these defaults "CRS_API_KEY_ID" and "CRS_API_KEY_TOKEN" in your environment.

You may also need to replace some hardcoded values, such as CIRCA_BASEPORT/CRS_API_PORT under the "crs: url:" flag in the scantron.yaml

5. Spin up the `final-smoketest-delta-api.prt` test. Wait for All lax agents connected and CRS API has successfully started.

6. Spin up the example-competition-server via `run-with-docker`. Wait for

7. Use the `trigger-comp-server-to-task` to trigger example-competition-server to task our crs.

8. Watch it fail/pass. Debug any issues. Monitor logs. Rinse and repeat.
