# obsidian-link-resolver

An Obsidian plugin that resolves all links in a vault using Obsidian's internal API. Used to produce ground-truth resolution data to validate our OCaml implementation against.

## How it works

1. Obsidian is running with the target vault open and this plugin enabled
2. Caller invokes `run.sh`, which fires an `obsidian://` URI and polls for the output file
3. Plugin walks all markdown files via `vault.getMarkdownFiles()`
4. For each link (from `metadataCache`), resolves it using `getFirstLinkpathDest` + `resolveSubpath`
5. Writes the full resolution map to the output path

## Usage

```sh
./run.sh <vault-name> <output-path> [timeout-seconds]
```

```sh
./run.sh tt /tmp/results.json
```

Prints the output path on success, exits non-zero on timeout (default 30s). Obsidian must already be running with the vault open.

## Output format

```json
{
  "results": [
    {
      "source": "Note 1.md",
      "link": "indir_same_name",
      "resolved": {
        "file": "indir_same_name.md",
        "subpath_type": null,
        "heading": null,
        "heading_level": null,
        "block_id": null
      }
    },
    {
      "source": "Note 1.md",
      "link": "Note 2#Some level 2 title",
      "resolved": {
        "file": "Note 2.md",
        "subpath_type": "heading",
        "heading": "Some level 2 title",
        "heading_level": 2,
        "block_id": null
      }
    }
  ]
}
```

`file` is null for unresolved links. `subpath_type` is null when the fragment did not resolve (fallback to file).

## Build

```sh
pnpm install
pnpm build
```

Output goes to `../data/vault/tt/.obsidian/plugins/link-resolver/main.js`.

## Alternative: obsidian-local-rest-api

[obsidian-local-rest-api](https://github.com/coddingtonbear/obsidian-local-rest-api) is a community plugin that runs an HTTPS server inside Obsidian, exposing vault metadata over HTTP. Rather than a custom URI handler + polling, you'd call a REST endpoint directly:

```sh
curl -s "https://127.0.0.1:27123/vault/..." -H "Authorization: Bearer <token>"
```

It doesn't expose link resolution directly, but it provides metadata (links, headings, blocks per file) that could be used to reconstruct resolution offline. Useful if you want to avoid writing a custom plugin entirely.
