## Generating the index locally

Generate an index with
```bash
$ GH_TOKEN="****" ./scala-cli.sh src
```

Just `./scala-cli.sh src` can work if `GH_TOKEN` is not set, but it usually
gets rate-limited by the GitHub API.

The index is written in `index.json` in the current directory.

## Use by coursier

The index generated here is now used by the `java` and `java-home`
commands of [coursier](https://get-coursier.io).

If you suspect one of those commands doesn't use a newer JVM version, pass `--update` to them,
like
```text
$ cs java --env --jvm temurin:17 --update
```

## About

Copyright (c) 2020-2022, Alexandre Archambault

Licensed under the Apache version 2 license.
