module Fake.JiraCliHelper

open System.Text.RegularExpressions

open Fake
open Fake.Git.CommandHelper


type JiraCliConfig = {
            url : string
            userName : string
            password : string
        }

type Either<'a, 'b> =
 Success of 'a
 | Failure of 'b

type private ErrorFallBackBuilder() =
    member x.Bind(v,f) = 
                match v with
                Success a -> (f v) |> Success
                | Failure b -> Failure b
    member x.Return a = Success a
    member x.ReturnFrom o = o

let private errorFlow = ErrorFallBackBuilder()

let jiraCli arg config =
    let jiraCliExe = findToolInSubPath "JiraCli.exe" "packages"
    ProcessHelper.enableProcessTracing <- false
    let fullArgs = sprintf "-user \"%s\" -pw \"%s\" -url \"%s\" %s" config.userName config.password config.url arg
    let success = execProcess 
                    (fun p -> p.FileName <- jiraCliExe; p.Arguments <- fullArgs)
                    (TimeSpan.FromMinutes 2.)
    if (not success) then failwith "failed to run JiraCli"
    ProcessHelper.enableProcessTracing <- true


let runGitCommandOpt cmd = 
    let success, msg, err =
        cmd
        |> runGitCommand currentDirectory
    if success then
        Success msg
    else
        Failure err        

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
                Success a -> a
                | Failure _ -> failwith "Unable to retrieve git commits since last tag"
