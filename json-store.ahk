/**
 * A lightweight AutoHotkey class for automatically loading, editing, and saving JSON data to disk.
 * Written by KR1ZZ1 @ github
 * Created: 29.05.2025 (day.month.year)
 *
 * Uses JSON.ahk by cocobelgica
 * @see https://github.com/cocobelgica/AutoHotkey-JSON
 *
 * Uses ahk-console by KR1ZZ1
 * @see https://github.com/KR1ZZ1/ahk-console
 */
class JsonStore {
    /**
     * Constructor for JsonStore.
     * Initializes a JSON store, loading or creating a JSON file.
     * @param {String} file - The path to the JSON file.
     * @param {Boolean} [prettyprint=true] - Whether to pretty print the JSON.
     */
    __new(file, prettyprint := true) {
        this._prettyprint := prettyprint
        this._filepath := file

        ; Create empty JSON object if file doesn't exist
        if (!FileExist(file)) {
            FileAppend, {}, % file
        }

        ; Read the file content
        FileRead, content, % file
        if (ErrorLevel) {
            throw Exception("Failed to read file")
        }

        ; Check if content is long enough to be valid JSON
        if (StrLen(content) < 2) {
            throw Exception("Not enough data in file to be valid JSON")
        }

        try {
            this._content := content
            currjson := JsonStore.JSON.Load(content)
            ; Validate the JSON object
            if (!currjson || !IsObject(currjson)) {
                throw Exception("Invalid/Empty JSON from file")
            }

            this._object := currjson
        } catch e {
            ; Fallback to empty object on failure
            this._object := {}
            this._content := "{}"
        }

        return this
    }

    /**
     * Getter for properties.
     * @param {String} aName - The name of the property to get.
     * @return {*} The value of the property.
     */
    __get(aName) {
        ; Special access for internal properties
        if ( aName = "_object"
          || aName = "_filepath"
          || aName = "_content"
          || aName = "_prettyprint" )
        {
            return ObjRawGet(this, aName)
        }

        ; Get from JSON object if it exists
        if (this._object.HasKey(aName)) {
            return this._object[aName]
        }

        return ""  ; Default value if not set
    }

    /**
     * Setter for properties.
     * @param {String} aName - The name of the property to set.
     * @param {*} aValue - The value to set.
     * @return {*} The value that was set.
     */
    __set(aName, aValue) {
        ; Prevent setting internal properties
        if ( aName = "_object"
          || aName = "_filepath"
          || aName = "_content"
          || aName = "_prettyprint" )
        {
            ObjRawSet(this, aName, aValue)
            return aValue
        }

        ; Only update if value is different
        if (!this._object.HasKey(aName) || this._object[aName] != aValue) {
            this._object[aName] := aValue
            this._content := JsonStore.JSON.Dump(this._object,, this._prettyprint ? 4 : "")

            ; Save to file
            If (FileExist(this._filepath)) {
                FileDelete, % this._filepath
            }
            FileAppend, % this._content, % this._filepath
        }

        return aValue
    }

    /**
     * Retrieves all the properties currently set.
     * @return {Object} The JSON object with all properties.
     */
    all() {
        return this._object
    }

    /**
     * Resets the JSON store by clearing the file and internal state.
     * @return {JsonStore} The current instance.
     */
    reset() {
        ; Check if the file exists and delete it
        If (FileExist(this._filepath)) {
            FileDelete, % this._filepath
        }

        ; Reset the internal JSON object and content
        this._object := {}
        this._content := "{}"

        ; Recreate the file with an empty JSON object
        FileAppend, % this._content, % this._filepath

        ; Return the current instance
        return this
    }

    /**
     * Dumps the current state of the JSON store to OutputDebug.
     */
    dump() {
        JsonStore.console.log(this.all())
    }

    /**
     * The console class is a wrapper for OutputDebug with Ansi color coding inspired by javascript's console methods.
     *
     * Written by krizzi @ discord
     * Created: 30.09.2023 (day.month.year)
     * Updated: 22.05.2025 (day.month.year)
     *
     * @see https://developer.mozilla.org/en-US/docs/Web/API/console
     */
    class console {
        /**
         * Prints string or array to OutputDebug
         * @param   {*} params* - Eg: `JsonStore.console.log("text", 123, [1, 2, 3])`
         * @return  {void}
         */
        log(params*) {
            local _, data

            For _,data in params {
                JsonStore.console.outputDebug(data)
            }
        }

        /**
         * Prints to OutputDebug
         * - All parameters: **Bold, Green Text**
         *
         * @param   {string|integer} params* - Eg: `JsonStore.console.success(1, 2, 3)`
         * @return  {void}
         */
        success(params*) {
            local _, string
            static Color := JsonStore.console.outputColor

            For _,string in params {
                if (IsObject(string)) {
                    throw Exception("``" A_ThisFunc "()`` cannot accept objects/arrays, use ``JsonStore.console.log()`` instead.")
                }
                OutputDebug, % Color.successStyle . string . Color.reset
            }
        }

        /**
         * Prints to OutputDebug
         * - All parameters: **Italic, Yellow Text**
         *
         * @param   {string|integer} params* - Eg: `JsonStore.console.info(1, 2, 3)`
         * @return  {void}
         */
        info(params*) {
            local _, string
            static Color := JsonStore.console.outputColor

            For _,string in params {
                if (IsObject(string)) {
                    throw Exception("``" A_ThisFunc "()`` cannot accept objects/arrays, use ``JsonStore.console.log()`` instead.")
                }
                OutputDebug, % Color.infoStyle . string . Color.reset
            }
        }

        /**
         * Accepts multiple params to print to OutputDebug
         * - First parameter: **White Text, Red Blinking Background**
         * - Additional parameters: **Bold, Red Text**
         *
         * @param   {string|integer} params* - Eg: `JsonStore.console.error("Title", "Content")`
         * @return  {void}
         */
        error(params*) {
            local index, string, str
            static Color := JsonStore.console.outputColor

            For index, string in params {
                if (IsObject(string)) {
                    throw Exception("``" A_ThisFunc "()`` cannot accept objects/arrays, use ``JsonStore.console.log()`` instead.")
                }
                str := " " string " "
                OutputDebug, % index < 2 ? Color.errorStyle . str . Color.reset
                                        : Color.errorDescriptionStyle . str . Color.reset
            }
        }

        /**
         * Accepts multiple params to print to OutputDebug
         * - First parameter: **Black Text, Yellow Background, Bold**
         * - Additional parameters: **Bold, Yellow Text**
         *
         * @param   {string|integer} params* - Eg: `JsonStore.console.warn("Title", "Content")`
         * @return  {void}
         */
        warn(params*) {
            local lengths, _, string, maxLength, index, str
            static Color := JsonStore.console.outputColor

            ; Get max length for center padding
            lengths := []
            for _,string in params {
                lengths.Push(StrLen(string))
            }
            maxLength := Round(Max(lengths*) * 10) / 10 + 10

            for index, string in params {
                if (IsObject(string)) {
                    throw Exception("``" A_ThisFunc "()`` cannot accept objects/arrays, use ``JsonStore.console.log()`` instead.")
                }
                str := JsonStore.console.padCenter(string, maxLength)
                OutputDebug, % index < 2
                    ? Color.warnStyle . str . Color.reset
                    : Color.warnDescriptionStyle . str . Color.reset
            }

            ; Append new line
            OutputDebug, % ""
        }

        /**
         * This class is used in `outputDebug()` to colorize the output.
         * [See list of ANSI color escape sequences](https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences)
         *
         * @property {string} successStyle          Color for success messages
         * @property {string} infoStyle             Color for info messages
         * @property {string} errorStyle            Color for error messages
         * @property {string} errorDescriptionStyle Color for error description
         * @property {string} warnStyle             Color for warning messages
         * @property {string} warnDescriptionStyle  Color for warning description
         * @property {string} stringColor           Color for regular strings
         * @property {string} numberColor           Color for numbers
         * @property {string} instanceColor         Color for instance descriptor
         * @property {string} functionColor         Color for functions
         * @property {string} keyColor              Color for keys
         * @property {string} reset                 Resets any ANSI styling
         */
        class outputColor {
            ; Console Class Styles
            static successStyle := "[32;1m"
            static infoStyle := "[33;3m"
            static errorStyle := "[37;41;6m"
            static errorDescriptionStyle := "[31;1m"
            static warnStyle := "[30;43;1m"
            static warnDescriptionStyle := "[33;1m"

            ; OutputDebug Styles
            static keyValueSeparator := " => "
            static stringColor := "[38;2;175;175;175m"
            static numberColor := "[36m"
            ;static normalColor := "[m"
            static instanceColor := "[37m"
            static functionColor := "[35m"
            static keyColor := "[34m"
            ;static arrowColor := "[32m"
            static reset := "[0m"
        }

        /**
         * Pads a centered string to a specified length
         *
         * @param {string}  string
         * @param {integer} length
         * @param {string}  cornerChar
         * @return {string}
         */
        padCenter(string, length, cornerChar := "") {
            local totalPadding, leftPadding, rightPadding, paddedString

            ; Calculate the total padding needed
            totalPadding := length - StrLen(string) - (StrLen(cornerChar) * 2)
            if (totalPadding <= 0) {
                return string
            }

            ; Calculate padding for the left and right sides
            leftPadding := Floor(totalPadding / 2)
            rightPadding := Ceil(totalPadding / 2)

            ; Create the padded string
            paddedString := Format("{1:" leftPadding "}", "") . string . Format("{1:-" rightPadding "}", "")

            return cornerChar . paddedString . cornerChar
        }

        /**
         * String repeater
         *
         * @param {string} string
         * @param {integer} count number of times to repeat the string
         * @return {string} * count
         */
        strRepeat(string, count) {
            local str := ""
            Loop, % count {
                str .= string
            }
            return str ? str : string
        }

        /**
         * Check if variable is given type
         *
         * @param   {variable} variable
         * @param   {null|integer|float|number|digit|xdigit|alpha|upper|lower|alnum|space|time} type
         * @return  {boolean}
         */
        type(var, type := "integer") {
            If type not in integer,float,number,digit,xdigit,alpha,upper,lower,alnum,space,time
                throw Exception("Not a valid type")

            If var is %type%
                return true
        }

        /**
         * OutputDebug wrapping to allow for strings and arrays to be sent to a debugger
         * In addition uses Ansi color coding (tested on Visual Studio Code Debug Console)
         *
         * Written by krizzi @ discord
         * Created: 30.09.2023 (day.month.year)
         * Updated: 22.05.2025 (day.month.year)
         *
         * @param {string|array}  content
         * @param {bool}          print   primarily used by this function to control when to construct the string and when to print the constructed string
         * @param {integer}       indent  primarily used to control array indentations during construction of the string
         * @return {string}       string  output of the constructed string
         */
        outputDebug(content, print := true, indent := 0) {
            local string, indentation, contentCount, key, value, endOfArray, requiredParams
            static color := JsonStore.console.outputColor

            static singleIndent := "    "

            if (print && IsObject(content) && !indent && !content.Count()) {
                if (IsFunc(content) && content.Name) {
                    OutputDebug, % color.instanceColor "function:" color.functionColor content.name "(" color.keyColor IsFunc(content) - 1 color.functionColor ")" color.reset
                    return
                }

                OutputDebug, % color.instanceColor "array:0 " color.reset "[]"
                return
            }

            ; Print when receiving string
            If (print && !IsObject(content)) {
                OutputDebug, % content
                return
            }

            string := ""
            indentation := JsonStore.console.strRepeat(singleIndent, indent)
            contentCount := ObjCount(content)
            For key, value in content {
                endOfArray := contentCount < A_Index

                ; Append new line to existing string
                if (string) {
                    string .= "`n"
                }

                ; Indentation or object type
                if (print && !string) { ; Inner Collection / Array
                    string .= color.instanceColor
                            . (A_Index = 1
                                ? (IsObject(content) && StrLen(content.__Class) ? content.__Class : "array") ":" contentCount
                                : (IsObject(value)   && StrLen(value.__Class)   ? value.__Class   : "array") ":" ObjCount(value))
                            . color.reset " [`n"
                } else if (!print) {
                    string .= indentation
                }

                ; Key for "Key => Value"
                string .= singleIndent color.keyColor key color.reset color.keyValueSeparator

                ; Value for "Key => Value"
                if (requiredParams := IsFunc(value)) { ; Functions
                    string .= color.instanceColor "function:" color.functionColor value.name "(" color.keyColor requiredParams - 1 color.functionColor ")" color.reset
                            . (!endOfArray ? ",":"")
                } else if (IsObject(value)) { ; Arrays
                    string .= color.instanceColor
                            . (StrLen(value.__Class) ? value.__Class : "array")
                            . ":" ObjCount(value)
                            . color.reset " [`n"
                            . JsonStore.console.outputDebug(value, 0, indent + 1) "`n"
                            . (!print ? indentation : "")
                            . "    ]"
                            . (!endOfArray ? ",":"")
                } else if (JsonStore.console.type(value, "number")) { ; Numbers
                    string .= color.numberColor value color.reset
                            . (!endOfArray ? ",":"")
                } else { ; Strings
                    string .= """" color.stringColor value color.reset """"
                            . (!endOfArray ? ",":"")
                }
            }

            If (print) {
                string .= "`n]"
                OutputDebug, % string
            }

            return string
        }
    }

    /**
     * Lib: JSON.ahk
     *     JSON lib for AutoHotkey.
     * Version:
     *     v2.1.3 [updated 04/18/2016 (MM/DD/YYYY)]
     * License:
     *     WTFPL [http://wtfpl.net/]
     * Requirements:
     *     Latest version of AutoHotkey (v1.1+ or v2.0-a+)
     * Installation:
     *     Use #Include JSON.ahk or copy into a function library folder and then
     *     use #Include <JSON>
     * Links:
     *     GitHub:     - https://github.com/cocobelgica/AutoHotkey-JSON
     *     Forum Topic - http://goo.gl/r0zI8t
     *     Email:      - cocobelgica <at> gmail <dot> com
     */

    /**
     * Class: JSON
     *     The JSON object contains methods for parsing JSON and converting values
     *     to JSON. Callable - NO; Instantiable - YES; Subclassable - YES;
     *     Nestable(via #Include) - NO.
     * Methods:
     *     Load() - see relevant documentation before method definition header
     *     Dump() - see relevant documentation before method definition header
     */
    class JSON {
        /**
         * Method: Load
         *     Parses a JSON string into an AHK value
         * Syntax:
         *     value := JSON.Load( text [, reviver ] )
         * Parameter(s):
         *     value      [retval] - parsed value
         *     text    [in, ByRef] - JSON formatted string
         *     reviver   [in, opt] - function object, similar to JavaScript's
         *                           JSON.parse() 'reviver' parameter
         */
        class Load extends JsonStore.JSON.Functor {
            Call(self, ByRef text, reviver:="") {
                local
                this.rev := IsObject(reviver) ? reviver : false
                ; Object keys(and array indices) are temporarily stored in arrays so that
                ; we can enumerate them in the order they appear in the document/text instead
                ; of alphabetically. Skip if no reviver function is specified.
                this.keys := this.rev ? {} : false

                static quot := Chr(34), bashq := "\" . quot
                    , json_value := quot . "{[01234567890-tfn"
                    , json_value_or_array_closing := quot . "{[]01234567890-tfn"
                    , object_key_or_object_closing := quot . "}"

                key := ""
                is_key := false
                root := {}
                stack := [root]
                next := json_value
                pos := 0

                while ((ch := SubStr(text, ++pos, 1)) != "") {
                    if InStr(" `t`r`n", ch)
                        continue
                    if !InStr(next, ch, 1)
                        this.ParseError(next, text, pos)

                    holder := stack[1]
                    is_array := holder.IsArray

                    if InStr(",:", ch) {
                        next := (is_key := !is_array && ch == ",") ? quot : json_value

                    } else if InStr("}]", ch) {
                        ObjRemoveAt(stack, 1)
                        next := stack[1]==root ? "" : stack[1].IsArray ? ",]" : ",}"

                    } else {
                        if InStr("{[", ch) {
                        ; Check if Array() is overridden and if its return value has
                        ; the 'IsArray' property. If so, Array() will be called normally,
                        ; otherwise, use a custom base object for arrays
                            static json_array := Func("Array").IsBuiltIn || ![].IsArray ? {IsArray: true} : 0

                        ; sacrifice readability for minor(actually negligible) performance gain
                            (ch == "{")
                                ? ( is_key := true
                                , value := {}
                                , next := object_key_or_object_closing )
                            ; ch == "["
                                : ( value := json_array ? new json_array : []
                                , next := json_value_or_array_closing )

                            ObjInsertAt(stack, 1, value)

                            if (this.keys)
                                this.keys[value] := []

                        } else {
                            if (ch == quot) {
                                i := pos
                                while (i := InStr(text, quot,, i+1)) {
                                    value := StrReplace(SubStr(text, pos+1, i-pos-1), "\\", "\u005c")

                                    static tail := A_AhkVersion<"2" ? 0 : -1
                                    if (SubStr(value, tail) != "\")
                                        break
                                }

                                if (!i)
                                    this.ParseError("'", text, pos)

                                value := StrReplace(value,  "\/",  "/")
                                , value := StrReplace(value, bashq, quot)
                                , value := StrReplace(value,  "\b", "`b")
                                , value := StrReplace(value,  "\f", "`f")
                                , value := StrReplace(value,  "\n", "`n")
                                , value := StrReplace(value,  "\r", "`r")
                                , value := StrReplace(value,  "\t", "`t")

                                pos := i ; update pos

                                i := 0
                                while (i := InStr(value, "\",, i+1)) {
                                    if !(SubStr(value, i+1, 1) == "u")
                                        this.ParseError("\", text, pos - StrLen(SubStr(value, i+1)))

                                    uffff := Abs("0x" . SubStr(value, i+2, 4))
                                    if (A_IsUnicode || uffff < 0x100)
                                        value := SubStr(value, 1, i-1) . Chr(uffff) . SubStr(value, i+6)
                                }

                                if (is_key) {
                                    key := value, next := ":"
                                    continue
                                }

                            } else {
                                value := SubStr(text, pos, i := RegExMatch(text, "[\]\},\s]|$",, pos)-pos)

                                static number := "number", integer :="integer"
                                if value is %number%
                                {
                                    if value is %integer%
                                        value += 0
                                }
                                else if (value == "true" || value == "false")
                                    value := %value% + 0
                                else if (value == "null")
                                    value := ""
                                else
                                ; we can do more here to pinpoint the actual culprit
                                ; but that's just too much extra work.
                                    this.ParseError(next, text, pos, i)

                                pos += i-1
                            }

                            next := holder==root ? "" : is_array ? ",]" : ",}"
                        } ; If InStr("{[", ch) { ... } else

                        is_array? key := ObjPush(holder, value) : holder[key] := value

                        if (this.keys && this.keys.HasKey(holder))
                            this.keys[holder].Push(key)
                    }

                } ; while ( ... )

                return this.rev ? this.Walk(root, "") : root[""]
            }

            ParseError(expect, ByRef text, pos, len:=1) {
                static quot := Chr(34), qurly := quot . "}"

                line := StrSplit(SubStr(text, 1, pos), "`n", "`r").Length()
                col := pos - InStr(text, "`n",, -(StrLen(text)-pos+1))
                msg := Format("{1}`n`nLine:`t{2}`nCol:`t{3}`nChar:`t{4}"
                ,     (expect == "")     ? "Extra data"
                    : (expect == "'")    ? "Unterminated string starting at"
                    : (expect == "\")    ? "Invalid \escape"
                    : (expect == ":")    ? "Expecting ':' delimiter"
                    : (expect == quot)   ? "Expecting object key enclosed in double quotes"
                    : (expect == qurly)  ? "Expecting object key enclosed in double quotes or object closing '}'"
                    : (expect == ",}")   ? "Expecting ',' delimiter or object closing '}'"
                    : (expect == ",]")   ? "Expecting ',' delimiter or array closing ']'"
                    : InStr(expect, "]") ? "Expecting JSON value or array closing ']'"
                    :                      "Expecting JSON value(string, number, true, false, null, object or array)"
                , line, col, pos)

                static offset := A_AhkVersion<"2" ? -3 : -4
                throw Exception(msg, offset, SubStr(text, pos, len))
            }

            Walk(holder, key) {
                value := holder[key]
                if IsObject(value) {
                    for i, k in this.keys[value] {
                        ; check if ObjHasKey(value, k) ??
                        v := this.Walk(value, k)
                        if (v != JSON.Undefined)
                            value[k] := v
                        else
                            ObjDelete(value, k)
                    }
                }

                return this.rev.Call(holder, key, value)
            }
        }

        /**
         * Method: Dump
         *     Converts an AHK value into a JSON string
         * Syntax:
         *     str := JSON.Dump( value [, replacer, space ] )
         * Parameter(s):
         *     str        [retval] - JSON representation of an AHK value
         *     value          [in] - any value(object, string, number)
         *     replacer  [in, opt] - function object, similar to JavaScript's
         *                           JSON.stringify() 'replacer' parameter
         *     space     [in, opt] - similar to JavaScript's JSON.stringify()
         *                           'space' parameter
         */
        class Dump extends JsonStore.JSON.Functor {
            Call(self, value, replacer:="", space:="") {
                this.rep := IsObject(replacer) ? replacer : ""

                this.gap := ""
                if (space) {
                    static integer := "integer"
                    if space is %integer%
                        Loop, % ((n := Abs(space))>10 ? 10 : n)
                            this.gap .= " "
                    else
                        this.gap := SubStr(space, 1, 10)

                    this.indent := "`n"
                }

                return this.Str({"": value}, "")
            }

            Str(holder, key) {
                value := holder[key]

                if (this.rep)
                    value := this.rep.Call(holder, key, ObjHasKey(holder, key) ? value : JSON.Undefined)

                if IsObject(value) {
                ; Check object type, skip serialization for other object types such as
                ; ComObject, Func, BoundFunc, FileObject, RegExMatchObject, Property, etc.
                    static type := A_AhkVersion<"2" ? "" : Func("Type")
                    if (type ? type.Call(value) == "Object" : ObjGetCapacity(value) != "") {
                        if (this.gap) {
                            stepback := this.indent
                            this.indent .= this.gap
                        }

                        is_array := value.IsArray
                    ; Array() is not overridden, rollback to old method of
                    ; identifying array-like objects. Due to the use of a for-loop
                    ; sparse arrays such as '[1,,3]' are detected as objects({}).
                        if (!is_array) {
                            for i in value
                                is_array := i == A_Index
                            until !is_array
                        }

                        str := ""
                        if (is_array) {
                            Loop, % value.Length() {
                                if (this.gap)
                                    str .= this.indent

                                v := this.Str(value, A_Index)
                                str .= (v != "") ? v . "," : "null,"
                            }
                        } else {
                            colon := this.gap ? ": " : ":"
                            for k in value {
                                v := this.Str(value, k)
                                if (v != "") {
                                    if (this.gap)
                                        str .= this.indent

                                    str .= this.Quote(k) . colon . v . ","
                                }
                            }
                        }

                        if (str != "") {
                            str := RTrim(str, ",")
                            if (this.gap)
                                str .= stepback
                        }

                        if (this.gap)
                            this.indent := stepback

                        return is_array ? "[" . str . "]" : "{" . str . "}"
                    }

                } else ; is_number ? value : "value"
                    return ObjGetCapacity([value], 1)=="" ? value : this.Quote(value)
            }

            Quote(string) {
                static quot := Chr(34), bashq := "\" . quot

                if (string != "") {
                    string := StrReplace(string,  "\",  "\\")
                    ; , string := StrReplace(string,  "/",  "\/") ; optional in ECMAScript
                    , string := StrReplace(string, quot, bashq)
                    , string := StrReplace(string, "`b",  "\b")
                    , string := StrReplace(string, "`f",  "\f")
                    , string := StrReplace(string, "`n",  "\n")
                    , string := StrReplace(string, "`r",  "\r")
                    , string := StrReplace(string, "`t",  "\t")

                    static rx_escapable := A_AhkVersion<"2" ? "O)[^\x20-\x7e]" : "[^\x20-\x7e]"
                    while RegExMatch(string, rx_escapable, m)
                        string := StrReplace(string, m.Value, Format("\u{1:04x}", Ord(m.Value)))
                }

                return quot . string . quot
            }
        }

        /**
         * Property: Undefined
         *     Proxy for 'undefined' type
         * Syntax:
         *     undefined := JSON.Undefined
         * Remarks:
         *     For use with reviver and replacer functions since AutoHotkey does not
         *     have an 'undefined' type. Returning blank("") or 0 won't work since these
         *     can't be distnguished from actual JSON values. This leaves us with objects.
         *     Replacer() - the caller may return a non-serializable AHK objects such as
         *     ComObject, Func, BoundFunc, FileObject, RegExMatchObject, and Property to
         *     mimic the behavior of returning 'undefined' in JavaScript but for the sake
         *     of code readability and convenience, it's better to do 'return JSON.Undefined'.
         *     Internally, the property returns a ComObject with the variant type of VT_EMPTY.
         */
        Undefined[]
        {
            get {
                static empty := {}, vt_empty := ComObject(0, &empty, 1)
                return vt_empty
            }
        }

        class Functor {
            __Call(method, ByRef arg, args*) {
                ; When casting to Call(), use a new instance of the "function object"
                ; so as to avoid directly storing the properties(used across sub-methods)
                ; into the "function object" itself.
                if IsObject(method)
                    return (new this).Call(method, arg, args*)
                else if (method == "")
                    return (new this).Call(arg, args*)
            }
        }
    }
}
