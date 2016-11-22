<Query Kind="FSharpProgram" />

// my own dumper?
module Option =
    let getOr' (lazyVal: 't Lazy) (valueOpt: 't option) = match valueOpt with |Some x -> x | None -> lazyVal.Value
    let ofObj x = if isNull <| box x then None else Some x
type System.String with
    member x.ChopEnds startCharsToSkip endCharsToSkip =
        if startCharsToSkip + endCharsToSkip < x.Length then x.Substring(startCharsToSkip, x.Length - startCharsToSkip - endCharsToSkip) else String.Empty
[<RequireQualifiedAccess>]
type FlushMode = 
    | None
    | Optimal
    | Immediate
// interfaces don't show up unless upcast so perhaps we can simulate protected
type IHideForProtectedOutputWriter
[<AbstractClass>]    
type OutputWriter(backingWriterOpt:TextWriter,flushMode,formatString:string -> bool -> string,formatObject, isTooBig) =
    inherit TextWriter()
    let maxLineLength = 100000
    let locker = Object()
    let inner:TextWriter = backingWriterOpt |> Option.ofObj |> Option.getOr' (lazy(upcast new StringWriter()))
    let mutable buffer = StringBuilder()
    let mutable currentLineLength = 0
    let mutable totalObjects = 0
    let mutable limit = false
    let mutable resultsCleared = false
    let mutable version = 0
    let mutable totalLength = 0
    let mutable flushMode: FlushMode = flushMode
    let mutable headerWritten = true
    let mutable footerWritten = false
    let mutable formattingTime:TimeSpan = TimeSpan()
    let setVersion x =
        lock locker (fun () -> version <- x)
    let rec flushBuffer partial = 
        if buffer.Length > 0 then
            let str = formatString (buffer |> string) partial
            if partial then
                inner.Write str
            elif isNull str || str.EndsWith("\r\n") then
                inner.WriteLine str
            else inner.Write str
            buffer <- StringBuilder()
            currentLineLength <- 0
            registerNewData 0 1
    and 
        registerNewData newChars newObjects =
        version <- version + 1
        totalLength <- totalLength + newChars
        totalObjects <- totalObjects + newObjects
        if flushMode = FlushMode.Optimal && buffer.Length > 4096 then
            flushBuffer true
            
    let appendBufferCore (value:string) =
        if flushMode <> FlushMode.Immediate then
            buffer.Append value |> ignore<StringBuilder>
        else
            let sw = System.Diagnostics.Stopwatch.StartNew()
            let str = formatString value true
            sw.Stop()
            inner.Write str
        registerNewData value.Length (value |> Seq.filter (fun c -> c = '\n') |> Seq.length)
        
    let appendBuffer (value:string) = 
        let mutable flag = false
        let num = value.LastIndexOf '\n'
        let appender () = 
            if value <> "\r\n" && value <> "\n" then
                if currentLineLength + value.Length <= maxLineLength then
                    flag <- num = -1 || num = value.Length - 1
                
                if not flag then
                    let strArrays = value.Split([ '\n'] |> Array.ofList)
                    let mutable flag1 = true
                    let strArrays1 = strArrays
                    strArrays1
                    |> Seq.iteri(fun i str ->
                        if not flag1 then
                            appendBufferCore "\r\n"
                        else
                            flag1 <- false
                        let mutable str1 = if str.EndsWith "\r" then str.ChopEnds 0 1 else str
                        while currentLineLength + str1.Length <= 101000 do
                            let num1 = maxLineLength - currentLineLength
                            str1.Substring(0,num1) |> appendBufferCore
                            appendBufferCore "\r\n"
                            currentLineLength <- 0
                            str1 <- str1.Substring num1
                        appendBufferCore str1
                        currentLineLength <- str1.Length
                        ()
                        
                    )
                elif num <> value.Length - 1 then
                    appendBufferCore value
                else
                    let flag2 = value.Length > 0 && value.[value.Length - 2] = '\r'
                    appendBufferCore(value.ChopEnds 0 (if flag2 then 2 else 1))
                    appendBufferCore "\r\n"
                    currentLineLength <- 0
            else
                currentLineLength <- 0
                appendBufferCore value
                
        lock locker appender
    let checkLimit () = 
        if not limit then
            lock locker (fun () ->
                if isTooBig totalLength totalObjects then
                    limit <- true
                    currentLineLength <- 0
                    appendBuffer"\r\n<limit of graph>"
                    flushBuffer false
                not limit
            )
        else
            false
    let isSimple (value:obj) =
        match value with
        | :? string
        | :? decimal
        | :? DateTime
        | :? DateTimeOffset
        | :? TimeSpan -> true
        | x -> x.GetType().IsPrimitive
    let purge() =
        if flushMode = FlushMode.None then
            inner <- new StringWriter()
            buffer.Length <- 0
            currentLineLength <- 0
            
    //new() = new OutputWriter(null, FlushMode.None)
    
    override x.Encoding with get() = Encoding.Unicode
    member val MaxDepth: int Nullable = Nullable() with get,set
    abstract Explorables:ResizeArray<obj>
    default __.Explorables = ResizeArray<_>()
    abstract Footer :string
    default __.Footer = null
    member __.FormattingTime = formattingTime
    abstract Header:string
    member __.IsAtLimit = limit
    member __.Version 
        with get() = 
            let num = lock locker (fun () -> version)
            num
    
        
            
    
type XhtmlWriter(backingWriter:TextWriter, enableExpansions:bool, isInteractive:bool, fragment:bool, writerHeader:bool) =
    inherit TextWriter()
    
let createXhtmlWriter enableExpansions maxDepth noHeader =
    
let toHtmlString enableExpansions maxDepth noHeader ([<ParamArray>] objectsToDump) =
    let mutable str:string = null
    use txtWriter = 