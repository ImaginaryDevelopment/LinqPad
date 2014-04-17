<Query Kind="Statements">
  <Reference>&lt;RuntimeDirectory&gt;\System.Data.Entity.Design.dll</Reference>
  <Namespace>System.Data.Entity.Design.PluralizationServices</Namespace>
  <Namespace>System.Globalization</Namespace>
</Query>

PluralizationService pluralizationService =  
    PluralizationService.CreateService( 
        new CultureInfo("en-US"));
pluralizationService.Pluralize("child").Dump();