<Query Kind="FSharpExpression" />

// find react component state/props references
let matches pattern x = Regex.Matches(x,pattern) |> Seq.cast<Match>
let findReferences x = x |> matches @"(?:this\.)?(?:props|state)\.(\w+)" |> Seq.map (fun m -> m.Value) |> Seq.distinct
let findPropTypes x = x |> matches @"(\w+)\s*:React.PropTypes" |> Seq.map(fun m -> sprintf "props.%s" m.Groups.[1].Value) |> Seq.distinct
let findPassedProps x = x|> matches @"(\w+)\s*={" |> Seq.map (fun m -> sprintf "props.%s" m.Groups.[1].Value)

let text = """app.Inputs = props =>
{
    var cooldown = (app.getCooldown(props.cooldownCommon, props.cooldownUncommon, props.cooldownRare, props.cooldownEpic) * 100);
    var dpsHero = props.crusaders.find(cru => cru.id === props.selectedHeroId);
    var effectiveEP = calcEffectiveEP(props.sharingIsCaringLevel, props.mainDpsEP, props.dpsSlotEP);
    console.log('Inputs mainDpsEP', props.mainDpsEP, typeof(props.mainDpsEP));
    return (<table>
        <thead>
            <tr><th>Crit Chance %</th><td><TextInputUnc onChange={val => props.onCritChanceChange(val)} value={props.critChance} /></td><td>%</td><td title="D"></td><th colSpan="5">Horn and Cornucopia Trinkets</th></tr>
            <tr><th>Ability Cooldown %</th><td>{cooldown}</td><td title="C"></td><td></td><th className="rarity1 black">Common:</th><th className="rarity2 black">Uncommon:</th><th className="rarity3 black">Rare:</th><th className="rarity4 black">Epic:</th><th>Total:</th></tr>
            <tr><th>Enchantment Points on main dps</th><td>{effectiveEP}</td><th colSpan="2"></th>
                <td title="E4"><TextInputUnc type="number" value={props.cooldownCommon} onChange={props.onCooldownCommonChange} /></td>
                <td title="F4"><TextInputUnc type="number" value={props.cooldownUncommon} onChange={props.onCooldownUncommonChange} /></td>
                <td><TextInputUnc type="number" value={props.cooldownRare} onChange={props.onCooldownRareChange} /></td>
                <td><TextInputUnc type="number" value={props.cooldownEpic} onChange={props.onCooldownEpicChange} /></td>
                <td>{cooldown.toFixed(2)}</td>
            </tr>
            <tr><th>Main Dps</th><td colSpan="2"><HeroSelect crusaders={props.crusaders} selectedHeroId={props.selectedHeroId} onHeroChange={props.onHeroChange} /></td><th>Slot</th></tr>
            <tr><th>Main Crusader Enchantments</th><td>{props.mainDpsEP}</td><td /><td className="textcenter vcenter">{dpsHero? dpsHero.slot: ""}</td><th colSpan="6">Put your levels for other talents here to calculate how much you've spent.</th></tr>
            <tr><th>Alt Crusader Enchantments</th><td>{props.dpsSlotEP - props.mainDpsEP}</td><td /><td /><th>Time-O-Rama</th><th>Massive Criticals</th><th>Speed Runner</th><th>Endurance Training</th><th>Gold-o-Splosion</th><th>Sniper</th></tr>
            <tr data-row="8"><th colSpan="2" /><th /><th />
                <td title="timeORama"><TextInputUnc value={props.timeORama} onChange={props.ontimeORamaChange}/></td>
                <td title="massiveCriticals"><TextInputUnc value={props.massiveCriticals} onChange={props.onMassiveCriticalsChange}/></td>
                <td title="speedRunner"><TextInputUnc value={props.speedRunner} onChange={props.onSpeedRunnerChange}/></td>
                <td title="enduranceTraining"><TextInputUnc value={props.enduranceTraining} onChange={props.onEnduranceTrainingChange}/></td>
                <td title="goldOSplosion"><TextInputUnc value={props.goldOSplosion} onChange={props.onGoldOSplosionChange}/></td>
                <td title="sniper"><TextInputUnc value={props.sniper} onChange={props.onsniper}/></td>
            </tr>
            <tr data-row="9"><th>Every Last Cent</th><th>Spend it all</th><th>Upgrade them all</th><th>Scavenger</th><th>Speed Looter</th><th>Efficient Crusading</th></tr>
            <tr data-row="10">
                <th colSpan="4" />
                <td title="everyLastCent"><TextInputUnc value={props.everyLastCent} onChange={props.onEveryLastCent}/></td>
                <td title="spendItAll"><TextInputUnc value={props.spendItAll} onChange={props.onSpendItAll}/></td>
                <td title="upgradeThemAll"><TextInputUnc value={props.upgradeThemAll} onChange={props.onUpgradeThemAll}/></td>
                <td title="scavenger"><TextInputUnc value={props.scavenger} onChange={props.onScavenger}/></td>
                <td title="speedLooter"><TextInputUnc value={props.speedLooter} onChange={props.onSpeedLooter}/></td>
                <td title="efficientCrusading"><TextInputUnc value={props.efficientCrusading} onChange={props.onEfficientCrusading}/></td>
            </tr>
            <tr>
                </tr>
            <tr>
                <td title="z"><TextInputUnc value={props.z} onChange={props.onz}/></td>
                </tr>
        </thead>
        <tbody>
        </tbody>
        </table>
        );
};"""

// ----------------------------------------------------------------------------------------------------------------------


let propTypesDeclaration = 
    """  onCritChanceChange: React.PropTypes.func.isRequired,
    critChance:React.PropTypes.number.isRequired,
    // hero may not be selected, and don't put on required, that's an implementation detail this component can handle
    mainDpsEP:React.PropTypes.number,
    // hero may not be selected
    selectedHeroId:React.PropTypes.string,
    // hero may not be selected yet
    heroDpsId:React.PropTypes.string,
    // hero may not be selected yet
    dpsSlotOtherEP:React.PropTypes.number,
    coolDownCommon:React.PropTypes.number,
    cooldownUncommon:React.PropTypes.number,
    cooldownRare:React.PropTypes.number,
    cooldownEpic:React.PropTypes.number,
    onCooldownCommonChange: React.PropTypes.func.isRequired,
    onCooldownUncommonChange: React.PropTypes.func.isRequired,
    onCooldownRareChange: React.PropTypes.func.isRequired,
    onCooldownEpicChange: React.PropTypes.func.isRequired,
    crusaders:React.PropTypes.array.isRequired"""
let consumerDeclaration = 
    """          <TalentCalc
            sharingIsCaringLevel={+this.state.saved.sharingIsCaringLevel}
            crusaders={props.jsonData.crusaders}
            mainDpsEP={talentSelectedCrusader ? getNumberOrDefault(this.state.saved.enchantmentPoints[talentSelectedCrusader.id]): 0}
            dpsSlotEP={ talentSelectedCrusader ? +props.jsonData.crusaders.filter(cru => cru.slot == talentSelectedCrusader.slot).map(cru => this.state.saved.enchantmentPoints[cru.id] || 0).reduce((a,b) => +a + +b): 0}
            critChance={getNumberOrDefault(this.state.saved.critChance, 0)} onCritChanceChange={val => (this.changeSaveState({critChance: inspect(+val || 0, 'changeSaveState crit')}))}
            cooldownCommon={getNumberOrDefault(this.state.saved.cooldownCommon,0)} onCooldownCommonChange={val => this.changeSaveState({cooldownCommon: +val || 0})}
            cooldownUncommon={getNumberOrDefault(this.state.saved.cooldownUncommon,0)} onCooldownUncommonChange={val => this.changeSaveState({cooldownUncommon: +val || 0})}
            cooldownRare={getNumberOrDefault(this.state.saved.cooldownRare,0)} onCooldownRareChange={val => this.changeSaveState({cooldownRare: +val || 0})}
            cooldownEpic={getNumberOrDefault(this.state.saved.cooldownEpic,0)} onCooldownEpicChange={val => this.changeSaveState({cooldownEpic: +val || 0})}
            selectedHeroId={typeof(this.state.saved.talentCalcHeroId) ==="string"? this.state.saved.talentCalcHeroId : undefined} onHeroChange={val => this.changeSaveState({talentCalcHeroId:val})}"""
let refs = findReferences text
let passed = findPassedProps consumerDeclaration
let propTypeInfo = findPropTypes propTypesDeclaration
// find missing, not extra here
(refs
|> Seq.map (fun r -> 
    ((passed |> Seq.tryFind ((=) r)),r |> after "props.", propTypeInfo |> Seq.tryFind((=) r) |> Option.getOrDefault null)
    )
)
//|> dumpt "missings"
|> Seq.filter(fun (found, _, _) -> Option.isNone found)
|> Seq.map (fun (_,name,_) -> name)
|> delimit "\r\n"
|> dumpt "to copy/paste"
|> ignore
Util.HorizontalRun(false,findPassedProps consumerDeclaration ,findReferences text,findPropTypes propTypesDeclaration)