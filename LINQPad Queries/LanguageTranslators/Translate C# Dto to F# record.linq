<Query Kind="FSharpExpression">
  <Reference>&lt;RuntimeDirectory&gt;\System.Windows.Forms.dll</Reference>
  <Reference>&lt;RuntimeDirectory&gt;\System.Web.dll</Reference>
  <Namespace>System.Web</Namespace>
</Query>

//C# to sql
//var input=LINQPad.Util.ReadLine<string>("What shall we reformat?");
let input ="""   public class Payers : INotifyPropertyChanging, INotifyPropertyChanged
                {
                    public Payers();

                    [System.Data.Linq.Mapping.ColumnAttribute]
                    public string Address1 { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute]
                    public string Address2 { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_AVID", DbType = "VarChar(20)")]
                    public string AVID { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_City", DbType = "VarChar(50)")]
                    public string City { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_ClaimsPhone", DbType = "VarChar(10)")]
                    public string ClaimsPhone { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_InNetwork", DbType = "Bit")]
                    public bool? InNetwork { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_IsChecked", DbType = "Bit")]
                    public bool? IsChecked { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_IsGroupPrint", DbType = "Bit NOT NULL")]
                    public bool IsGroupPrint { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_IsInsurance", DbType = "Bit")]
                    public bool? IsInsurance { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_IsMedicare", DbType = "Bit")]
                    public bool? IsMedicare { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_Name", DbType = "VarChar(50)")]
                    public string Name { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_PayerGroup", DbType = "VarChar(50)")]
                    public string PayerGroup { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute]
                    public int PayerID { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_State", DbType = "Char(2)")]
                    public string State { get; set; }
                    [System.Data.Linq.Mapping.AssociationAttribute(Name = "FK_Statements_Payers_PayerID", Storage = "_Statements", ThisKey = "PayerID", OtherKey = "PayerIdGroup", DeleteRule = "NO ACTION")]
                    public System.Data.Linq.EntitySet<Statements> Statements { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_X270ID", DbType = "VarChar(20)")]
                    public string X270ID { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_X276ID", DbType = "VarChar(20)")]
                    public string X276ID { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_X278ID", DbType = "VarChar(20)")]
                    public string X278ID { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_X837ID", DbType = "VarChar(20)")]
                    public string X837ID { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_X837IID", DbType = "VarChar(50)")]
                    public string X837IID { get; set; }
                    [System.Data.Linq.Mapping.ColumnAttribute(Storage = "_Zip", DbType = "VarChar(5)")]
                    public string Zip { get; set; }

                    public event PropertyChangedEventHandler PropertyChanged;
                    public event PropertyChangingEventHandler PropertyChanging;

                    protected virtual void SendPropertyChanged(string propertyName);
                    protected virtual void SendPropertyChanging();
                }"""
[
for l in input.SplitLines() do 
    if l.Contains("{ get; set; }") then
        if l.Trim().StartsWith("public ") then
            let l = l.After("public ").Trim().Before("{").Trim()
            let words = l.Split( ' ')
            let itemType = words.[0]
            let itemType = if itemType.EndsWith("?") then itemType.Before("?") + " option" else itemType
            yield words.[1] + ":" + itemType
    elif l.Contains(";") then 
        let itemType = l.Trim().Before(" ").Trim()
        let name = l.Trim().After(" ").Trim().Before(";")
        yield name+":"+itemType
]	
|> fun l -> String.Join("\r\n\t",l)