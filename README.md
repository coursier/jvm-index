# coursier jvm-index

This repository hosts and manages the JVM index used by the `cs java` and `cs java-home`
commands of [coursier](https://get-coursier.io), and more generally, the JVM management
capabilities of coursier.

## Generating the index locally

Generate an index with
```bash
$ GH_TOKEN="****" ./scala-cli.sh src
```
or
```powershell
$Env:GH_TOKEN="*****"
scala-cli src
```

Just `./scala-cli.sh src` can work if `GH_TOKEN` is not set, but it usually gets
rate-limited by the GitHub API. You can read more about creating a token
[here](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens).
Just having the `public_repo` scope will be enough for the access you need.

The index is written in `index.json` in the current directory.

## Use by coursier

The index generated here is now used by the `java` and `java-home`
commands of [coursier](https://get-coursier.io).

If you suspect one of those commands doesn't use a newer JVM version, pass `--update --ttl 0` to them,
like
```text
$ cs java --env --jvm graalvm-java17:22.0.0 --update --ttl 0
```

## About

Copyright (c) 2020-2022, Alexandre Archambault

Licensed under the Apache version 2 license.
