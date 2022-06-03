# doc-search

This is an proof of concept for a search engine indexing documentation from
multiple sources.

The current scope is just to provide useful examples of how to interact with the
APIs of interest (Github and Dropbox Paper for now) and how to search for the
indexed docs.

Deliberately left out:
  - build a real web app that serves the frontend and proxies queries to elastic
    search (righ not it's just a static file hitting ElasticSearch directly with
    CORs enabled)
  - automatic periodic scrawling/re-indexing of sources (right now it's just a
    command line program you have to run each time you want to index content)
  - any type of robust authorization scheme to control different access levels
    for documents.

## Development environment set up

Most of the tooling is managed via nix and direnv. If you have those two set up
you should be able to just run `direnv allow`, wait a bit and then have
everything you need to compile the indexing program and the frontend.

### Elasticsearch installation and configuration

The one notable exception is Elasticsearch, which I found quite hard to properly
install and configure through Nix. For now you'll have to download it directly
from [the official website](https://www.elastic.co/downloads/elasticsearch) and
uncompress.

Then, modify the `config/elasticsearch.yml` file as follows:

```yml
# NOTE: OF COURSE THIS IS JUST A QUICK-AND-DIRTY WAY TO GET THE INSTANCE RUNNING ON
# DEVELOPMENT. DO NOT USE THIS CONFIGURATION ON ANYTHING REMOTELY SIMILAR TO A
# PRODUCTIVE ENVIRONMENT.

# Modify this line to disable authentication
xpack.security.http.ssl:
  enabled: false
  keystore.path: certs/http.p12

# Allow connections from any host
http.host: 0.0.0.0

# Add these lines to let the frontend hit Elasticsearch directly
http.cors.enabled : true
http.cors.allow-origin: "*"
http.cors.allow-methods: OPTIONS, HEAD, GET, POST, PUT, DELETE
http.cors.allow-headers: X-Requested-With,X-Auth-Token,Content-Type,Content-Length
http.cors.allow-credentials: true
```

After that you should be able to run Elasticsearch with `./bin/elasticsearch`.

### Environment variables

Configuration is managed via environment variables. For convenience, direnv will
load a `.envrc.secret` file if present, which is ignored in Git. Make sure to
run `direnv reload` for it to pick changes to that file.

You will need:
  - `ES_HOST`, pointing to the Elasticsearch server (located at `http://localhost:9200` by default)
  - `GH_TOKEN`, with a Github API token which you can get at https://github.com/settings/tokens
  - `DROPBOX_TOKEN`, with a Dropbox API token. You can get this in two ways:
    - following the instructions from https://dropbox.tech/developers/generate-an-access-token-for-your-own-account (which require registering an app)
    - clicking the `Get Token` button in https://dropbox.github.io/dropbox-api-v2-explorer/#file_requests_list/continue (which will generate a token for the API explorer)

### Running the program

To compile the haskell program run `cabal build`.

You can run the program using `cabal run doc-search -- <ARGS>`. To index the
hardcoded sources:
  - `cabal run doc-search -- --index-github`
  - `cabal run doc-search -- --index-paper`

To run the frontend, execute `elm reactor` at the top of the repo and then visit
`http://localhost:8000/ui/src/Main.elm` in your browser.
