<Query Kind="FSharpProgram" />

let curry f (x,y) = f x y
let delimit (d:string) (x:string seq) = String.Join(d,x)
let text pascal humanized = sprintf """                <DataGridTextColumn Binding="{Binding %s}" Header="%s">
                    <DataGridTextColumn.HeaderTemplate>
                        <DataTemplate>
                            <StackPanel>
                                <TextBlock x:Name="txt%s" Text="{Binding Content, RelativeSource={RelativeSource Mode=TemplatedParent}}" />
                                <TextBox KeyDown="%sKeyDown"/>
                            </StackPanel>
                        </DataTemplate>
                    </DataGridTextColumn.HeaderTemplate>
                </DataGridTextColumn>""" pascal humanized pascal pascal
type Generated = {TypeName:string; Meta:string; ViewModelMatches: string list; KeyDowns:string list; Xaml:string} with
    member x.Targets = 
        let vmFn = sprintf "%sViewModel.fs" x.TypeName
        let xamlCs =  sprintf "%sList.xaml.cs" x.TypeName
        let xaml=  sprintf "%sList.xaml" x.TypeName
        sprintf "Meta -> ViewModel, Matches -> %s, KeyDowns -> %s, Xaml -> %s" vmFn xamlCs xaml
let gen typeName columns = 
    {   TypeName=typeName; 
        Meta =  sprintf "module Meta = HD.Schema.DataModels.%ss.%sHelpers.Meta" typeName typeName
        ViewModelMatches =  
            columns
            |> List.map(fst >> fun k ->
                sprintf "        | StringEqualsI (CMeta.%s) -> Searching.rMatcher(string item.%s) userValue" k k )
        KeyDowns =  
            columns 
            |> List.map(fst >> fun k -> 
                sprintf "        void %sKeyDown(object sender, KeyEventArgs e) => HandleSearchAttempt(nameof(%sRecord.%s), sender, e);" k typeName k )
        Xaml = 
            columns
            |> List.map (curry text)
            |> delimit Environment.NewLine
            |> sprintf "<DataGrid.Columns>%s</DataGrid.Columns>"
    
    }
    
let columns = Map[
                "Client",[
                    "Id", "ID"
                    "CompanyName", "Company Name"
//                    "ParentClientId", "Parent Client ID"
//                    "AllowsOffsiteData", "Allows Offsite Data"
//                    "Address1", "Address1"
//                    "Address2", "Address2"
//                    "City", "City"
//                    "State", "State"
//                    "ZIP", "Zip" ]
                ]
                "Event",[
                    "Id","ID"
                    "PrimaryTeamId","Primary Team ID"
                    "StartTime", "Start Time"
                    "EndTime", "End Time"
                ]
                "Participant",[
//                    "Id","ID"
                    "LastName", "Last Name"
                    "FirstName", "First Name"
                    "Dob", "DOB"
                    "EmployeeNumber", "Employee Number"
                    "MemberNumber", "Member Number"
                ]
]
let f k =
    gen k columns.[k] |> Dump |> ignore
f "Participant"