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

type WorkflowPart1 = { Name: string; Steps: WorkflowStep list }

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

let workflowsPart1 = 
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
        let workflow = workflowsPart1[part.CurrentWorkflow]
        
        let applicableStep = workflow.Steps |> Seq.find (fun step -> step.Condition(part))
        applicableStep.Action(part)

parts |> Seq.iter EvaluatePart

let partSum = 
    parts
    |> Seq.filter (fun part -> part.Accepted.Value)
    |> Seq.sumBy (fun part -> part.X + part.M + part.A + part.S)

printfn "[Part 1]: Sum of all classifications of accepted parts = %d" partSum

(* ============================================================================== *)

type ValidRange(min, max) =
    member this.Min = min
    member this.Max = max
    member this.IsValid =
        this.Max >= this.Min
    member this.Clone() =
        ValidRange(min, max)

type Part2(x, m, a, s, workflow, stepIndex) =
    member this.X = x
    member this.M = m
    member this.A = a
    member this.S = s
    member val CurrentWorkflow = workflow with get,set
    member val StepIndex = stepIndex with get, set

    new() =
        Part2(ValidRange(1, 4000), ValidRange(1, 4000), ValidRange(1, 4000), ValidRange(1, 4000), "in", 0)

    member this.ReplaceRange(cls: char, range) =
        match cls with
        | 'x' -> Part2(range, this.M.Clone(), this.A.Clone(), this.S.Clone(), this.CurrentWorkflow, this.StepIndex)
        | 'm' -> Part2(this.X.Clone(), range, this.A.Clone(), this.S.Clone(), this.CurrentWorkflow, this.StepIndex)
        | 'a' -> Part2(this.X.Clone(), this.M.Clone(), range, this.S.Clone(), this.CurrentWorkflow, this.StepIndex)
        | 's' -> Part2(this.X.Clone(), this.M.Clone(), this.A.Clone(), range, this.CurrentWorkflow, this.StepIndex)
        | _ -> failwith "???"

type WorkflowPart2 = { Name: string; Steps: string list }

let workflowsPart2 = 
    workflowLines
    |> Seq.map (fun line -> 
            let nameAndRest = line.Split('{')
            let name = nameAndRest[0]

            // s<1351:px,qqz
            // s<537:gd,x>2440:R,A
            let steps =
                nameAndRest[1].Trim('}').Split(',')
                |> Seq.toList

            (name, { Name = name; Steps = steps })
        )
    |> Map.ofSeq

let acceptedParts = new List<Part2>()

let IntersectRange(range: ValidRange, target: ValidRange) =
    let candidate = ValidRange(Math.Max(range.Min, target.Min), Math.Min(range.Max, target.Max))
    if candidate.IsValid then Some(candidate) else None

let SplitRange(range: ValidRange, operator: char, operand: int) =
    
    if operator = '>' then
        let matchingRange = IntersectRange(range, ValidRange(operand + 1, 4000))
        let mismatchingRange = IntersectRange(range, ValidRange(1, operand))
        (matchingRange, mismatchingRange)
    else
        let matchingRange = IntersectRange(range, ValidRange(1, operand - 1))
        let mismatchingRange = IntersectRange(range, ValidRange(operand, 4000))
        (matchingRange, mismatchingRange)

let SplitPart(part: Part2, selector: char, operator: char, operand: int) =

    let range =
        match selector with
        | 'x' -> part.X
        | 'm' -> part.M
        | 'a' -> part.A
        | 's' -> part.S
        | _ -> failwith "???"

    let (matchingRange, mismatchingRange) = SplitRange(range, operator, operand)

    (
        (if matchingRange.IsSome then Some(part.ReplaceRange(selector, matchingRange.Value)) else None),
        (if mismatchingRange.IsSome then Some(part.ReplaceRange(selector, mismatchingRange.Value)) else None)
    )


let EvaluateStep(part: Part2) =
    let workflow = workflowsPart2[part.CurrentWorkflow]
    let step = workflow.Steps[part.StepIndex]

    if step = "A" || step = "R" then
        seq { Part2(part.X, part.M, part.A, part.S, step, 0) }
    else if step.Contains(':') then
        // Evaluate the condition, this yields two branches
        let parts = step.Split(':')
        let target = parts[1]
        let conditionStr = parts[0]

        let operand = int (conditionStr.Split([| '<'; '>' |])[1])
        let operator = if step.Contains('>') then '>' else '<'

        let (matchingPart, mismatchingPart) = SplitPart(part, conditionStr[0], operator, operand)

        seq { 
            if matchingPart.IsSome then
                matchingPart.Value.CurrentWorkflow <- target
                matchingPart.Value.StepIndex <- 0
                yield matchingPart.Value

            if mismatchingPart.IsSome then
                mismatchingPart.Value.StepIndex <- mismatchingPart.Value.StepIndex + 1
                yield mismatchingPart.Value
        }
    else
        // forward to the first step of another workflow
        seq { Part2(part.X, part.M, part.A, part.S, step, 0) }

let rootPart = new Part2()

let mutable partsToInvestigate = new Stack<Part2>()

partsToInvestigate.Push(rootPart)

while partsToInvestigate.Count > 0 do
    let part = partsToInvestigate.Pop()

    for newPart in EvaluateStep(part) do

        if newPart.CurrentWorkflow = "A" then
            acceptedParts.Add(newPart)
        else if newPart.CurrentWorkflow = "R" then
            () // Reject
        else
            partsToInvestigate.Push(newPart)

printfn "[Part 2] Accepted variants: %d" acceptedParts.Count

let combinationCount =
    acceptedParts
    |> Seq.sumBy (fun part -> int64 (part.X.Max - part.X.Min + 1) *
                              int64 (part.M.Max - part.M.Min + 1) *
                              int64 (part.A.Max - part.A.Min + 1) *
                              int64 (part.S.Max - part.S.Min + 1))

printfn "[Part 2] Acceptable combinations: %d" combinationCount
