<Query Kind="FSharpExpression" />

// create sql db on network share
// help from  https://blogs.msdn.microsoft.com/varund/2010/09/02/create-a-sql-server-database-on-a-network-shared-drive/
let statements user share dbName pwd = 
    sprintf """
    DBCC TRACEON(1807, -1)
    
    DBCC TRACESTATUS(-1)
    
    exec xp_cmdshell 'net use %s: \\xdc\sqlShare %s /user:%s' 
    
    CREATE DATABASE [%s] ON  PRIMARY 
    ( NAME = N'networked', FILENAME = N'%s:\%s.mdf' , SIZE = 3072KB , MAXSIZE = 2GB, FILEGROWTH = 1024KB ) 
    LOG ON 
    ( NAME = N'networked_log', FILENAME = N'%s:\%s_log.ldf' , SIZE = 1024KB , MAXSIZE = 1GB , FILEGROWTH = 10%%)
    go""" share pwd user dbName share dbName share dbName

// get the name the password is stored under
let userWithDomain = Util.GetPassword("domainUser")
//Util.ReadLine("password name?")
userWithDomain
|> Util.GetPassword
|> statements userWithDomain "Z" "PmNetworked"