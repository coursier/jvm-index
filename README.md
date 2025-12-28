# coursier jvm-index

[![Update index](https://github.com/coursier/jvm-index/actions/workflows/update-index.yml/badge.svg)](https://github.com/coursier/jvm-index/actions/workflows/update-index.yml)
[![Publish](https://github.com/coursier/jvm-index/actions/workflows/publish.yml/badge.svg)](https://github.com/coursier/jvm-index/actions/workflows/publish.yml)

This repository hosts and manages the JVM index used by the `cs java` and `cs java-home`
commands of [coursier](https://get-coursier.io), and more generally, the JVM management
capabilities of coursier.

## Available JDKs

| JDK                                                                                  | id in index         |
| ------------------------------------------------------------------------------------ | ------------------- |
| [Eclipse Temurin](https://adoptium.net/temurin/releases) (recommended / default)     | `temurin`           |
| [GraalVM community](https://github.com/graalvm/graalvm-ce-builds/releases)           | `graalvm-community` |
| [Oracle JDKs](https://www.oracle.com/java/technologies/downloads/)                   | `oracle`            |
| [Azul Zulu](https://www.azul.com/downloads/?package=jdk#zulu)                        | `zulu`              |
| [bellsoft Liberica](https://bell-sw.com/pages/downloads/)                            | `liberica`          |
| [bellsoft Liberica Native Image Kit](https://bell-sw.com/liberica-native-image-kit/) | `liberica-nik`      |
| [Amazon Corretto](https://aws.amazon.com/corretto/)                                  | `corretto`          |
| [IBM Semeru](https://developer.ibm.com/languages/java/semeru-runtimes/)              | `ibm-semeru`        |
| [Microsoft OpenJDK](https://learn.microsoft.com/en-us/java/openjdk/download)         | `microsoft-openjdk` |

## Legacy JDKs

| JDK                                            | id in index |
| ---------------------------------------------- | ----------- |
| AdoptOpenJDK                                   | `adopt`     |
| Merge of AdoptOpenJDK and Eclipse Temurin JDKs | `adoptium`  |

## Index structure

The index comes in 2 shapes:
- a [Jabba](https://github.com/shyiko/jabba)-compatible single JSON file, [`index.json`](https://github.com/coursier/jvm-index/blob/master/index.json)
- per-OS and CPU architecture indices, under [`indices/`](https://github.com/coursier/jvm-index/tree/master/indices)

While the former is only available on GitHub, the latter is also pushed to Maven Central,
under the [`io.get-coursier.jvm.indices`](https://repo1.maven.org/maven2/io/get-coursier/jvm/indices/)
organization.

### Jabba-compatible index

That index consists in 4 nested JSON objects, with the successive keys of these objects being:
- the OS
  - `linux`
  - `darwin` (macOS)
  - `windows`
  - `linux-musl` (for [musl libc](https://www.musl-libc.org)-based systems, like Alpine Linux),
  - `aix`
  - `solaris`
- the CPU architecture
  - `amd64`
  - `arm64` (a.k.a. `aarch64`, Raspberry PI or Mac M1-M4 CPUs)
  - `x86` (32-bit)
  - `arm` (32-bit ARM)
  - `ppc64`
  - `ppc64le`
  - `s390x`
- the JDK name, prefixed by `jdk@`, like
  - `jdk@temurin`
  - `jdk@graalvm-community`
  - `jdk@liberica`
  - `jdk@liberica-nik`
  - `jdk@zulu`
  - `jdk@corretto`
  - `jdk@java-oracle`
  - …
- the JDK version, often prefixed with `1.`, like
  - `1.8.0-432`
  - `1.17.0.13`
  - `1.21.0.5`
  - `1.23.0.1`
  - …

The values of the fourth nested object look like
```
${TYPE}+${URL}
```

`${TYPE}` being one of
- `tgz` (for gzip-compressed tar archive JDKs, on Unixes typically)
- `zip` (for zip-compressed JDKs, on Windows typically)

`${URL}` being the URL of the JDK archive, like `https://github.com/adoptium/temurin23-binaries/releases/download/jdk-23.0.1%2B11/OpenJDK23U-jdk_aarch64_mac_hotspot_23.0.1_11.tar.gz`.

Value examples
```text
tgz+https://github.com/adoptium/temurin23-binaries/releases/download/jdk-23.0.1%2B11/OpenJDK23U-jdk_aarch64_mac_hotspot_23.0.1_11.tar.gz
zip+https://github.com/adoptium/temurin23-binaries/releases/download/jdk-23%2B37/OpenJDK23U-jdk_x64_windows_hotspot_23_37.zip
```

Example commands to inspect the index locally:
```bash
$ cat "$(cs get https://github.com/coursier/jvm-index/raw/refs/heads/master/index.json)" |
    jq 'keys'
[
  "aix",
  "darwin",
  "linux",
  "linux-musl",
  "solaris",
  "windows"
]

$ cat "$(cs get https://github.com/coursier/jvm-index/raw/refs/heads/master/index.json)" |
    jq '.darwin | keys'
[
  "amd64",
  "arm64"
]

$ cat "$(cs get https://github.com/coursier/jvm-index/raw/refs/heads/master/index.json)" |
    jq '.darwin.amd64 | keys'
[
  "jdk@corretto",
  "jdk@graalvm-community",
  "jdk@graalvm-oracle",
  "jdk@java-oracle",
  "jdk@liberica",
  "jdk@temurin",
  "jdk@zulu",
  …
]

$ cat "$(cs get https://github.com/coursier/jvm-index/raw/refs/heads/master/index.json)" |
    jq '.darwin.amd64["jdk@temurin"] | keys'
[
  "1.11.0.25",
  "1.16.0.2",
  "1.17.0.13",
  "1.18.0.2.1",
  "1.19.0.2",
  "1.20.0.2",
  "1.21.0.5",
  "1.22.0.2",
  "1.23",
  "1.23.0.1",
  "1.8.0-432",
  …
]

$ cat "$(cs get https://github.com/coursier/jvm-index/raw/refs/heads/master/index.json)" |
    jq '.darwin.amd64["jdk@temurin"]["1.23.0.1"]'
"tgz+https://github.com/adoptium/temurin23-binaries/releases/download/jdk-23.0.1%2B11/OpenJDK23U-jdk_x64_mac_hotspot_23.0.1_11.tar.gz"
```

### Per-OS and CPU architecture indices

These consist in 2 nested JSON objects, whose keys are:
- the JDK name, like
  - `temurin`
  - `graalvm-community`
  - `liberica`
  - `zulu`
  - `corretto`
  - `java-oracle`
  - …
- the JDK version, like
  - `8.0-432`
  - `17.0.13`
  - `21.0.5`
  - `23.0.1`
  - …

The values of the second nested object look like those of the [Jabba-compatible index](#jabba-compatible-index):
```
${TYPE}+${URL}
```

`${TYPE}` being one of
- `tgz` (for gzip-compressed tar archive JDKs, on Unixes typically)
- `zip` (for zip-compressed JDKs, on Windows typically)

`${URL}` being the URL of the JDK archive, like `https://github.com/adoptium/temurin23-binaries/releases/download/jdk-23.0.1%2B11/OpenJDK23U-jdk_aarch64_mac_hotspot_23.0.1_11.tar.gz`.

Value examples
```text
tgz+https://github.com/adoptium/temurin23-binaries/releases/download/jdk-23.0.1%2B11/OpenJDK23U-jdk_aarch64_mac_hotspot_23.0.1_11.tar.gz
zip+https://github.com/adoptium/temurin23-binaries/releases/download/jdk-23%2B37/OpenJDK23U-jdk_x64_windows_hotspot_23_37.zip
```

These indices live at Maven coordinates like
```text
io.get-coursier.jvm.indices:index-${OS}-${CPU}:${INDEX_VERSION}
```

`${OS}` being one of:
  - `linux`
  - `darwin` (macOS)
  - `windows`
  - `linux-musl` (for [musl libc](https://www.musl-libc.org)-based systems, like Alpine Linux),
  - `aix`
  - `solaris`

`${CPU}` being one of:
  - `amd64`
  - `arm64` (a.k.a. `aarch64`, Raspberry PI or Mac M1-M4 CPUs)
  - `x86` (32-bit)
  - `arm` (32-bit ARM)
  - `ppc64`
  - `ppc64le`
  - `s390x`

`${INDEX_VERSION}` can be found from [directory listings](https://repo1.maven.org/maven2/io/get-coursier/jvm/indices/index-darwin-arm64/)
or [`maven-metadata.xml`](https://repo1.maven.org/maven2/io/get-coursier/jvm/indices/index-darwin-arm64/maven-metadata.xml) files on Maven Central.

Example commands to inspect such an index locally:
```text
$ unzip -l "$(cs get https://repo1.maven.org/maven2/io/get-coursier/jvm/indices/index-darwin-arm64/0.0.4-70-51469f/index-darwin-arm64-0.0.4-70-51469f.jar)"
Archive:  ~/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/get-coursier/jvm/indices/index-darwin-arm64/0.0.4-70-51469f/index-darwin-arm64-0.0.4-70-51469f.jar
  Length      Date    Time    Name
---------  ---------- -----   ----
      332  11-14-2024 09:51   META-INF/MANIFEST.MF
        0  11-14-2024 09:51   META-INF/
        0  11-14-2024 09:51   coursier/
        0  11-14-2024 09:51   coursier/jvm/
        0  11-14-2024 09:51   coursier/jvm/indices/
        0  11-14-2024 09:51   coursier/jvm/indices/v1/
   157254  11-14-2024 09:51   coursier/jvm/indices/v1/darwin-arm64.json
---------                     -------
   157586                     7 files

$ unzip -p "$(cs get https://repo1.maven.org/maven2/io/get-coursier/jvm/indices/index-darwin-arm64/0.0.4-70-51469f/index-darwin-arm64-0.0.4-70-51469f.jar)" coursier/jvm/indices/v1/darwin-arm64.json |
    jq keys
[
  "corretto",
  "graalvm-community",
  "graalvm-oracle",
  "ibm-semeru",
  "java-oracle",
  "liberica",
  "temurin",
  "zulu",
  …
]

$ unzip -p "$(cs get https://repo1.maven.org/maven2/io/get-coursier/jvm/indices/index-darwin-arm64/0.0.4-70-51469f/index-darwin-arm64-0.0.4-70-51469f.jar)" coursier/jvm/indices/v1/darwin-arm64.json |
    jq '.temurin | keys'
[
  "11.0.25",
  "17.0.13",
  "18.0.2.1",
  "19.0.2",
  "20.0.2",
  "21.0.5",
  "22.0.2",
  "23.0.1",
  …
]

$ unzip -p "$(cs get https://repo1.maven.org/maven2/io/get-coursier/jvm/indices/index-darwin-arm64/0.0.4-70-51469f/index-darwin-arm64-0.0.4-70-51469f.jar)" coursier/jvm/indices/v1/darwin-arm64.json |
    jq '.temurin["23.0.1"]'
"tgz+https://github.com/adoptium/temurin23-binaries/releases/download/jdk-23.0.1%2B11/OpenJDK23U-jdk_aarch64_mac_hotspot_23.0.1_11.tar.gz"
```

## Use by coursier

### API

As of writing this (coursier `2.1.18`), coursier relies by default on the Jabba-compatible single JSON file index, but is able to use the per-OS and CPU index from Maven Central.

```scala mdoc:silent
import coursier.cache.FileCache
import coursier.jvm._
val cache = FileCache()
val jvmCache = JvmCache().withDefaultIndex
val javaHomeManager = JavaHome().withCache(jvmCache)
val javaHome = javaHomeManager.get("temurin:21")
  .unsafeRun()(cache.ec)
```

Using the per-OS and CPU index from Maven Central:
```scala mdoc:silent:reset
import coursier.cache.FileCache
import coursier.jvm._
val cache = FileCache()
val jvmCache = JvmCache().withIndexChannel(
  coursier.Resolve().finalRepositories.unsafeRun()(cache.ec),
  JvmChannel.central()
)
val javaHomeManager = JavaHome().withCache(jvmCache)
val javaHome = javaHomeManager.get("temurin:21")
  .unsafeRun()(cache.ec)
```

### CLI

The index generated here is now used by the `java` and `java-home`
commands of [coursier](https://get-coursier.io).

If you suspect one of those commands doesn't use a newer JVM version, pass `--update --ttl 0` to them,
like
```text
$ cs java --env --jvm graalvm-community:23 --update --ttl 0
```

## Use by Scala CLI

[Scala CLI](https://github.com/VirtusLab/scala-cli) relies on coursier to manage JDKs. It automatically
downloads JDKs if needed, based on the `--jvm …` option and `//> jvm …` directive.

## Use by Mill

The [Mill build tool](https://mill-build.org) allows its users to pick a JVM to compile / run / test code. As of
writing this, this feature has just been merged via [`com-lihaoyi/mill#3716`](https://github.com/com-lihaoyi/mill/pull/3716)

## How this repository works

### Workflow

The [`update-index` workflow](https://github.com/coursier/jvm-index/blob/master/.github/workflows/update-index.yml)
runs daily. It lists available JVMs from various providers, and re-generates the indices. If any
change is found, a PR for it is opened.

Maintainers need to pick the PR, check that it doesn't contain anything suspicious, approve it,
and merge it.

Upon merge, the [Jabba-compatible index](#jabba-compatible-index) is immediately up-to-date.
The [`publish` workflow](https://github.com/coursier/jvm-index/blob/master/.github/workflows/publish.yml)
runs and pushes updated [per-OS and CPU architecture indices](#per-os-and-cpu-architecture-indices)
to Maven Central. Once the `publish` workflow ran successfully, up to a few hours are sometimes needed
for the newer indices to be available on [Maven Central](https://repo1.maven.org/maven2).

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

## About

Copyright (c) 2020-2022, Alexandre Archambault

Licensed under the Apache version 2 license.
