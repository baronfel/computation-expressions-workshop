(*
  This file is one part documatation, one part active samples!
  You should be able to fill in portions and run them in your FSI as we go along.
  In addition, I'll drop links to useful resources here.
*)

(* references:
    * Official docs: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions
    * The F# Spec: https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, section 6.3.10, page 67
    * Ryan Riley's presentation from openfsharp 2017: https://panesofglass.github.io/computation-expressions/#/
      * make sure to open the speaker notes (press 's' while the presentation is open) to get more detail
    * Ryan's workshop from openfsharp 2018: https://github.com/panesofglass/computation-expressions-workshop
    * Scott Wlaschin's Deep Dive Series: https://fsharpforfunandprofit.com/posts/computation-expressions-intro/
*)

(* What are Computation Expressions?

    Computation Expressions (forever after referred to as CE's) are an alternative calling syntax for methods
    on some kind of 'builder object'. This builder object exposes methods that can be called inside of `{ }`
    blocks via a special syntax, and which can be transformed/executed/run in ways that are not immediately apparent

    You've probably already used them in the form of `seq` expressions, or `async` or `task` expressions.
*)

/// A simple sequence comprehension that skips by `skip` from a `start` point
let mySeq skip start =
    let mutable v = start
    seq {
        while true do
            yield v
            v <- v + skip
    }

/// a simple wrapper that sleeps before returning an action
let delay (duration: System.TimeSpan) (action: Async<'t>) =
    async {
        do! Async.Sleep ( int duration.TotalMilliseconds)
        return! action
    }

(* How do they work?
    `async` is an instance of a `builder` class (https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/control.asyncbuilder-class-%5Bfsharp%5D) that exposes instance members.
    When the compiler sees an expression of the form
        <builder-object-instance> {
            set_of_expressions
        }
    , it translates that into calls onto the <builder-object>.
    As an example, the above translates to the following (The full table of standard 'translations' is available here: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions#creating-a-new-type-of-computation-expression)
*)

let delayExpanded (duration: System.TimeSpan) (action: Async<'t>) =
    // a delay is inserted so that none of the body of the CE is run until the whole CE is run
    async.Delay(fun () ->
        // do the sleep, then after the sleep is done
        async.Bind(Async.Sleep(int duration.TotalMilliseconds), fun () ->
            // return the result of this other async action
            async.ReturnFrom(action))
        )


(* Why use CEs?
    A CE can do a good job of hiding the guts of the underlying transformations.  Take for example a gathering of Danarys Targaryen's inner council. The council can only
    meet if all members are present
*)

let getJorah = fun () -> Some "Jorah Mormont"
let getBarristan = fun () -> Some "Barristan Selmy"
let getTyrion = fun () -> None // he's off drinking

let canCouncilMeet =
    match getJorah () with
    | Some jorah ->
        match getBarristan () with
        | Some barristan ->
            match getTyrion () with
            | Some tyrion -> true
            | None -> false
        | None -> false
    | None -> false

// It's easy to see here that there's a lot of duplicated logic if any of the members is missing.  What if we could abstract away all of the nests to get to the core logic?
// let's try defining a 'OptionBuilder' so that we could use a CE to hide the innards. Luckily the `Option` module has a lot of the implemenation already!

type OptionBuilder() =
    /// bind lets us map + flatten
    member this.Bind(optionValue: 'a option, bindFn: 'a -> 'b option) = Option.bind bindFn optionValue
    // return lets us 'lift' a value
    member this.Return(value: 'a) = Some value
    member this.ReturnFrom(value: 't option) = value
    // delay makes it so that nothing in the CE is run until the CE is evaluated.
    // This makes it so a println in an `async` block doesn't fire until the `async` is started, for example
    member this.Delay(action: unit -> _) = fun () -> action ()
    // this member un-delays a CE
    member this.Run(f: unit -> _) = f()
    member this.Combine(option, f) = Option.bind f option

let option = OptionBuilder ()

let niceCanCouncilMeet =
    option {
        printfn "boo!"
        let! jorah = getJorah ()
        let! barristan = getBarristan ()
        let! tyrion = getTyrion ()
        return true
    }

printfn "note that no printing has occurred yet!"
// we can check that they are equivalent
canCouncilMeet = (niceCanCouncilMeet |> Option.defaultValue false)
printfn "now the message should have printed"

// RECORD SCRATCH! That didn't work, the `boo!` line printed before our logging lines!

(* This is because f# is _eager_. If you really want to delay options, you need a builder that uses a delayed option type: *)

type Maybe<'a> = Maybe of (unit -> 'a option)

type MaybeBuilder() =
    member x.Bind(option, binder) = Option.bind binder option
    member x.Return(value) = Some value
    member x.ReturnFrom(Maybe value) = value ()
    member x.Delay(action) = action
    member x.Run(f) = Maybe f
    member x.Zero() = None

module Maybe =
    let run (Maybe f) = f()

let maybe = MaybeBuilder()

(* Now let's try again *)

let maybeCanCouncilMeet =
    maybe {
        printfn "boo!"
        let! jorah = getJorah ()
        let! barristan = getBarristan ()
        let! tyrion = getTyrion ()
        return true
    }

printfn "let's see if that worked"
canCouncilMeet = (maybeCanCouncilMeet |> Maybe.run |> Option.defaultValue false)
printfn "did it work?"

(*
    Well that was a nice detour into eager/delayed semantics in F#. We saw how to mimic monadic structures.
    What else can CE's do?
    What about more complex control structures, like iteration, loops, using statements?
*)

(* EventuallyBuilder *)

// Computations that can be run step by step
type Eventually<'T> =
    | Done of 'T
    | NotYetDone of (unit -> Eventually<'T>)

module Eventually =
    // The bind for the computations. Append 'func' to the
    // computation.
    let rec bind func expr =
        match expr with
        | Done value -> func value
        | NotYetDone work -> NotYetDone (fun () -> bind func (work()))

    // Return the final value wrapped in the Eventually type.
    let result value = Done value

    type OkOrException<'T> =
        | Ok of 'T
        | Exception of System.Exception

    // The catch for the computations. Stitch try/with throughout
    // the computation, and return the overall result as an OkOrException.
    let rec catch expr =
        match expr with
        | Done value -> result (Ok value)
        | NotYetDone work ->
            NotYetDone (fun () ->
                let res = try Ok(work()) with | exn -> Exception exn
                match res with
                | Ok cont -> catch cont // note, a tailcall
                | Exception exn -> result (Exception exn))

    // The delay operator.
    let delay func = NotYetDone (fun () -> func())

    // The stepping action for the computations.
    let step expr =
        match expr with
        | Done _ -> expr
        | NotYetDone func -> func ()

    // The rest of the operations are boilerplate.
    // The tryFinally operator.
    // This is boilerplate in terms of "result", "catch", and "bind".
    let tryFinally expr compensation =
        catch (expr)
        |> bind (fun res ->
            compensation();
            match res with
            | Ok value -> result value
            | Exception exn -> raise exn)

    // The tryWith operator.
    // This is boilerplate in terms of "result", "catch", and "bind".
    let tryWith exn handler =
        catch exn
        |> bind (function Ok value -> result value | Exception exn -> handler exn)

    // The whileLoop operator.
    // This is boilerplate in terms of "result" and "bind".
    let rec whileLoop pred body =
        if pred() then body |> bind (fun _ -> whileLoop pred body)
        else result ()

    // The sequential composition operator.
    // This is boilerplate in terms of "result" and "bind".
    let combine expr1 expr2 =
        expr1 |> bind (fun () -> expr2)

    // The using operator.
    let using (resource: #System.IDisposable) func =
        tryFinally (func resource) (fun () -> resource.Dispose())

    // The forLoop operator.
    // This is boilerplate in terms of "catch", "result", and "bind".
    let forLoop (collection:seq<_>) func =
        let ie = collection.GetEnumerator()
        tryFinally
            (whileLoop
                (fun () -> ie.MoveNext())
                (delay (fun () -> let value = ie.Current in func value)))
            (fun () -> ie.Dispose())

// The builder class.
type EventuallyBuilder() =
    member x.Bind(comp, func) = Eventually.bind func comp
    member x.Return(value) = Eventually.result value
    member x.ReturnFrom(value) = value
    member x.Combine(expr1, expr2) = Eventually.combine expr1 expr2
    member x.Delay(func) = Eventually.delay func
    member x.Zero() = Eventually.result ()
    member x.TryWith(expr, handler) = Eventually.tryWith expr handler
    member x.TryFinally(expr, compensation) = Eventually.tryFinally expr compensation
    member x.For(coll:seq<_>, func) = Eventually.forLoop coll func
    member x.Using(resource, expr) = Eventually.using resource expr

let eventually = new EventuallyBuilder()

let disposableLogger =
    printfn "let's do the thing!"
    { new System.IDisposable with
        member __.Dispose() =
            printfn "done doing the thing!" }

let comp =
    eventually {
        use logger = disposableLogger
        try
            for x in 1..2 do
                try
                    printfn " x = %d" x
                finally
                    printfn "at least we got there in the end"
        with
        | exn -> printfn "welp that went poorly"
        return 3 + 4
    }

let step = Eventually.step
let comp' = step comp // use statement
let comp'' = comp'
             |> step // try
             |> step // for
             |> step // try/finally
             |> step // for
             |> step // try/finally
             |> step // return/end of use


(* Other fun builders are: *)

(* TaskBuilder (https://github.com/rspeele/TaskBuilder.fs/blob/master/TaskBuilder.fs)

    This one is _great_ for C#/BCL interop
*)

#load "./.paket/load/net471/TaskBuilder.fs.fsx"

open System.Threading.Tasks
open FSharp.Control.Tasks

let myTask = task {
    do! Task.Delay 1000
    return 1 + 1
}

printfn "Task returned %d" myTask.Result


(*
    QueryBuilder (https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/query-expressions)
                 (https://github.com/Microsoft/visualfsharp/blob/master/src/fsharp/FSharp.Core/Query.fs#L52-L227)

    This one is great, it compiles down the nested builder-method chains before running the queryable and returning it as an IEnumerable
*)
let q =
    query {
        for i in 1..100000 do
        where (i % 2 <> 0)
        select (i * i)
        headOrDefault
    }


(* Saturn! (https://github.com/SaturnFramework/Saturn) *)
