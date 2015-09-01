<Query Kind="FSharpProgram" />

namespace FSharpWpfMvvmTemplate.ViewModel

open System.Data
open System.Data.SqlClient
open System.Collections.ObjectModel

open FSharpWpfMvvmTemplate.ViewModel
type TableSys = {Name:string; Schema_id:int; Is_ms_shipped:bool}
type TableInfo = {Name:string; Catalog:string; Schema:string}

type SqlTableViewModel (connectionString) =
    inherit ViewModelBase()
    let dbNullToOpt x = if x = System.DBNull.Value then None else Some x
    let dbNullOrToString (x:obj) = if System.DBNull.Value.Equals(x) then null else x.ToString() 
    let objToOpt x = if x = null then None else Some x
    let get t (f:(_ -> _) option)  exe = 
        use conn = new SqlConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()
        cmd.CommandType <- CommandType.Text
        cmd.CommandText  <- t
        if f.IsSome then exe cmd |> f.Value else exe cmd
    let getScalar t f =
        get t f <| fun cmd -> cmd.ExecuteScalar()
    let getNon t f =
        get t f <| fun cmd -> cmd.ExecuteNonQuery()
    let getAll t f = 
        get t None  
        <| fun cmd -> 
                use r = cmd.ExecuteReader()
                seq{ 
                    while r.Read() do
                        let record = r :> IDataRecord
                        yield f record
                }
                |> Array.ofSeq 
    let getAllEmitted t (init:IDataRecord->System.Func<IDataRecord,obj>) = 
        get t None 
        <| fun cmd -> 
                use r = cmd.ExecuteReader()
                let loader:System.Func<IDataRecord,obj> ref= ref null
                let isFirst = ref true
                seq{ 
                    while r.Read() do
                        let record = r :> IDataRecord
                        if !isFirst then
                            isFirst := false
                            loader := init record
                        let value = !loader
                        let loadedValue = value.Invoke(record)
                        yield loadedValue
                }
                |> Array.ofSeq 
    let searchDatabases () = 
        getAll "select Name from sys.Databases" <| fun r -> r.["Name"] :?> string
    let searchTables db = 
        let query = sprintf "use %s select * from INFORMATION_SCHEMA.TABLES WHERE TABLE_CATALOG = '%s'" db db
        printfn "%s" query
        getAll query
        <| fun r ->
            {
                TableInfo.Catalog = r.["Table_Catalog"] :?> string
                Schema = r.["Table_Schema"] :?> string
                Name = r.["Table_Name"] :?> string
            }
    let createType name (r:IDataRecord)  =
//        let readNextColumn (r:IDataRecord) i = if r.FieldCount > i then dbNullOrToString r.[i] else null
       
//        match r.FieldCount  with
//        | 0 -> null :> obj
//        | 1 -> r.[0].ToString() :> obj
//        | 2 -> (r.[1].ToString(), readNextColumn r 1) :> obj
//        | 3 -> 
        let props = 
            seq{
                for i in [0..r.FieldCount-1] do
                    yield r.GetName(i),r.GetFieldType(i)
            }
            |> dict
        let loader = EmitModel.GenerateAssembly name name props

//                System.Reflection.Emit.AssemblyBuilder.DefineDynamicAssembly(new System.Reflection.AssemblyName(db+"."+table), System.Reflection.Emit.AssemblyBuilderAccess.Run)
        //printfn "%A" <| EmitModel.GenerateCSharpCode EmitModel.compileUnit
                //EmitModel.GenerateAssembly EmitModel.compileUnit
        loader
        //assembly :> obj
            
//            assembly.DefinedTypes <- [| specificType |]
//            assembly.
//        | _ -> 
//            (r.[0].ToString() :> obj, 
//                readNextColumn r 1,
//                readNextColumn r 2,
//                readNextColumn r 3,
//                readNextColumn r 4,
//                readNextColumn r 5,
//                readNextColumn r 6,
//                readNextColumn r 7)
//            :> obj
    let searchTable db table = 
        let query = sprintf "use %s select top 100 * from %s" db table
        printfn "%s" query
        let readNextColumn (r:IDataRecord) i = if r.FieldCount > i then dbNullOrToString r.[i] else null
        getAllEmitted query
        <| createType "DynamicB01"

    let databases = new ObservableCollection<string>(searchDatabases())
    let tables = new ObservableCollection<TableInfo>()
    let rows = new ObservableCollection<obj>()
    let mutable selectedDatabase :string option  = None
    let mutable selectedTable:TableInfo option = None
    let handleSearchDatabases () = 
        searchDatabases()
        |> Seq.iter databases.Add 
    let handleChangeDatabase db =
        tables.Clear()
        searchTables db
        |> Seq.iter tables.Add  
    let handleChangeTable table = 
        rows.Clear()
        searchTable selectedDatabase.Value table
        |> Seq.iter rows.Add

    static member Assembly = System.Reflection.Assembly.GetExecutingAssembly()

    new () = SqlTableViewModel( sprintf "Data Source=(local);Integrated Security=SSPI;Initial Catalog=XPEncounter;app=%s" <| SqlTableViewModel.Assembly.GetName().Name)

    member x.Databases with get () = databases
    member x.Tables with get () = tables
    member x.Rows with get() = rows
    member x.SelectedDatabase
        with get() = if selectedDatabase.IsSome then selectedDatabase.Value else null
        and set value = 
            if value <> null && selectedDatabase.IsSome && value <> selectedDatabase.Value then
                if value <> null then handleChangeDatabase value else tables.Clear()
            selectedDatabase <- if value <> null then Some value else None
            x.OnPropertyChanged "SelectedDatabase"
    member x.SelectedTable
        with get() = if selectedTable.IsSome then selectedTable.Value :> obj else null
        and set (value:obj) =
            let mappedValue = if value <> null then value :?> TableInfo |> Some else None
            if value <> null && selectedTable.IsNone || (selectedTable.IsSome && value <> (selectedTable.Value.Name :> obj)) then
                if value <> null then handleChangeTable mappedValue.Value.Name else rows.Clear()
            selectedTable <- if value <> null then value :?> TableInfo |> Some else None
            x.OnPropertyChanged "SelectedTable"
    member x.SearchDatabasesCommand = 
        new RelayCommand ((fun canExecute -> true), 
            (fun action -> handleSearchDatabases())) 
