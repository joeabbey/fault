import os
import strutils
from json import parseJson, JsonNode
include helpers

type
  Config* = object
    name*: string
    port*: int
    debug: bool

  InternalState = enum
    idle, running, stopped

const VERSION* = "1.0.0"
let defaultPort* = 8080
var requestCount = 0

proc init*(name: string, port: int): Config =
  result = Config(name: name, port: port, debug: false)

proc parseConfig*(data: string): Config =
  let node = parseJson(data)
  result = Config(
    name: node["name"].getStr(),
    port: node["port"].getInt(),
    debug: node["debug"].getBool()
  )

func pureAdd*(x, y: int): int =
  result = x + y

method render*(self: Config): string =
  result = "Config(" & self.name & ":" & $self.port & ")"

proc internalHelper(x: int): int =
  result = x * 2

template debugLog*(msg: string) =
  when defined(debug):
    echo "[DEBUG] " & msg

macro generateField*(name: untyped) =
  discard
