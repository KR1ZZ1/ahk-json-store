# ahk-json-store

A lightweight AutoHotkey class for automatically loading, editing, and saving JSON data to disk.
`JsonStore` provides simple key-value access to a persistent JSON file; perfect for settings, state, or cache storage.

## Features

- Automatically syncs values to a JSON file on assignment
- Transparent property access (`store.username := "admin"`)
- Automatically creates missing files or initializes empty JSON
- Reset support for clearing stored data
- Pretty-printed or compact JSON output

---

## Installation

1. Download and place the folder into your AutoHotkey installation folder lib or your script lib folder
    - E.g: `C:\Program Files\AutoHotkey\lib\ahk-json-store`
2. Include the code in your script:

```ahk
#Include <ahk-json-store\json-store>
```

---

## Usage

```ahk
#Include <ahk-json-store\json-store>

settings := new JsonStore("settings.json") ; (filePath [, prettyPrint := true])

; Set values like normal properties
settings.username := "admin"
settings.exit_on_error := true

; Dumps normal properties to OutputDebug (if available)
; @see https://www.autohotkey.com/docs/v1/lib/OutputDebug.htm
settings.dump()

; Read values
MsgBox % "Exit on error: " settings.exit_on_error

; Reset the store (clears file and memory)
settings.reset()

; Store now empty
MsgBox % "After reset: " settings.username  ; Returns ""
```

---

## Related

- [JSON.ahk by cocobelgica](https://github.com/cocobelgica/AutoHotkey-JSON)
- [Console by me (KR1ZZ1)](https://github.com/KR1ZZ1/ahk-console)

---
