module Fake.JiraCliHelper

open System

open System.Text.RegularExpressions

open Fake
open Fake.Git.CommandHelper

open Microsoft.FSharp.Core


type JiraCliConfig = {
            url : string
            userName : string
            password : string
        }

type private EitherBuilder<'a, 'b>() =
    member x.Bind(m,f) =
             match m with
                | Ok s -> f s
                | Error _ -> m
    member x.Return a = Ok a
    member x.ReturnFrom (o : Result<'a, 'b>) = o

    member x.Zero() = Error ""

let private errorFlow = EitherBuilder()    

type JiraCliCommands =
    CustomExec of string
    | AssignVersion of project:string * version:string * issues:string list
    | CreateVersion of project:string * version:string
    | ReleaseVersion of project:string * version:string

let private jiraCliExec config arg =
    let jiraCliExe = findToolInSubPath "JiraCli.exe" "packages"
    ProcessHelper.enableProcessTracing <- false
    let fullArgs = sprintf "-user \"%s\" -pw \"%s\" -url \"%s\" %s" config.userName config.password config.url arg
    let success = execProcess 
                    (fun p -> p.FileName <- jiraCliExe; p.Arguments <- fullArgs)
                    (TimeSpan.FromMinutes 2.)
    ProcessHelper.enableProcessTracing <- true                
    if (not success) then failwith "failed to run JiraCli"    

let private getCmdFromJiraCliCommand = function
    CustomExec arg -> arg
    | AssignVersion (project,version,issues)
        -> sprintf "-action AssignVersion -project \"%s\" -version \"%s\" -issues \"%s\"" 
            project
            version 
            (issues |> String.concat ",")
    | CreateVersion (project, version)
        -> sprintf "-action CreateVersion -project \"%s\" -version \"%s\"" project version
    | ReleaseVersion (project, version)
        -> sprintf "-action ReleaseVersion -project \"%s\" -version \"%s\"" project version


let jiraCli (cfg : JiraCliConfig->JiraCliConfig) cmd =
    let configuration =
        {
                url = ""
                userName = ""
                password = ""
        } |> cfg
    let actualCommand = cmd |> getCmdFromJiraCliCommand

    cmd
    |> getCmdFromJiraCliCommand
    |> jiraCliExec configuration

let runGitCommandOpt cmd = 
    let success, msg, err =
        cmd
        |> runGitCommand currentDirectory
    if success then
        Ok msg
    else
        Error err        

let parseJiraIdsFromCommits commits =
    let regex = "[a-zA-Z]+-\\d+"

    let matches regex input =
        Regex.Matches(input, regex) 
        |> Seq.cast<Match>
        |> Seq.groupBy (fun m -> m.Value)
        |> Seq.map fst
    
    commits
    |> String.concat " "
    |> matches regex

let getCommitsSinceLastTag() =
    errorFlow {
        let! lastTag = runGitCommandOpt "describe --tags --abbrev=0 HEAD^"
        let lastTagHead = lastTag |> Seq.head
        return! runGitCommandOpt (sprintf "log %s..HEAD --pretty=format:\"%%s\"" lastTagHead)
    }
    |> fun x -> match x with
                Ok a -> a
                | Error _ -> failwith "Unable to retrieve git commits since last tag"
