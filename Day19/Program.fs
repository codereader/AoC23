open System
open Utils
open System.Collections.Generic
open System.Diagnostics

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

(*
// Test input
lines <- @"px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}".Replace("\r\n", "\n").Split('\n')
*)

let separatorIndex = seq { 0..lines.Length - 1 } |> Seq.find (fun index -> String.IsNullOrWhiteSpace(lines[index]))

let workflowLines = lines |> Seq.take separatorIndex |> Seq.toArray
let partLines = lines |> Seq.skip (separatorIndex + 1) |> Seq.toArray

type Part = { X: int; M: int; A: int; S: int; mutable Accepted: bool option; mutable CurrentWorkflow: string }

let parts = 
    partLines
    |> Seq.map (fun line -> 
            let cls =
                line.Trim([| '}'; '{' |]).Split(',')
                |> Seq.map (fun part -> int (part.Split('=')[1]))
                |> Seq.toArray
            { X = cls[0]; M = cls[1]; A = cls[2]; S = cls[3]; Accepted = None; CurrentWorkflow = "in" }
        )
    |> Seq.toArray

type WorkflowStep = { Condition: Part -> bool; Action: Part -> unit }

type Workflow = { Name: string; Steps: WorkflowStep list }

let ParseSelector(input: string) =
    match input[0] with
    | 'x' -> (fun part -> part.X)
    | 'm' -> (fun part -> part.M)
    | 'a' -> (fun part -> part.A)
    | 's' -> (fun part -> part.S)

let CompareGreaterThan(selector, operand) =
    fun part -> selector(part) > operand

let CompareLessThan(selector, operand) =
    fun part -> selector(part) < operand

let ParseCondition(input: string) =
    let operand = int (input.Split([| '<'; '>' |])[1])
    let selector = ParseSelector(input)

    if input.Contains('>') then
        fun part -> CompareGreaterThan(selector, operand)(part)
    else
        fun part -> CompareLessThan(selector, operand)(part)

let ParseTarget(target:string) =
    if target = "A" then
        fun part ->
            printfn " -> ACCEPT"
            part.Accepted <- Some(true)
    else if target = "R" then
        fun part ->
            printfn " -> REJECT"
            part.Accepted <- Some(false)
    else
        fun part ->
            printf " -> %s" target
            part.CurrentWorkflow <- target

let ParseAction(input: string) =
    if input.Contains(':') then
        ParseTarget(input.Split(':')[1])
    else
        ParseTarget(input)

let workflows = 
    workflowLines
    |> Seq.map (fun line -> 
            let nameAndRest = line.Split('{')
            let name = nameAndRest[0]

            // s<1351:px,qqz
            // s<537:gd,x>2440:R,A
            let steps =
                nameAndRest[1].Trim('}').Split(',')
                |> Seq.map (fun stepStr ->
                    if stepStr.Contains(':') then
                        { Condition = ParseCondition(stepStr.Split(':')[0]); Action = ParseAction(stepStr) }
                    else
                        { Condition = (fun _ -> true); Action = ParseAction(stepStr) }
                )
                |> Seq.toList

            (name, { Name = name; Steps = steps })
        )
    |> Map.ofSeq

let EvaluatePart(part:Part) =
    printf "Part: %s" part.CurrentWorkflow
    while part.Accepted.IsNone do
        let workflow = workflows[part.CurrentWorkflow]
        
        let applicableStep = workflow.Steps |> Seq.find (fun step -> step.Condition(part))
        applicableStep.Action(part)

parts |> Seq.iter EvaluatePart

let partSum = 
    parts
    |> Seq.filter (fun part -> part.Accepted.Value)
    |> Seq.sumBy (fun part -> part.X + part.M + part.A + part.S)

printfn "[Part 1]: Sum of all classifications of accepted parts = %d" partSum