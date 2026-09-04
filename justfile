@default:
    just --list

fmt:
    just --fmt --unstable

ws:
  websocat ws://localhost:8090/api/ws

scalafmt:
    sbtn scalafmtAll

scalafix:
    sbtn 'scalafixEnable; scalafixAll'

fix:
    just scalafmt scalafix

[confirm]
clean:
    git clean -Xdf

deps:
    sbt dependencyUpdates

grep-graal:
  cs java --jvm-index cs --available | grep -i graalvm

native-image:
  sbt 'json-rpc/nativeImage'

test-rpc:
  jq -c . rpc.json | socat - UNIX-CONNECT:/tmp/rates-scope.sock
