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
    When the compiler sees an expression of the form <builder-object> { set_of_expressions }, it translates that into calls onto the <builder-object>.
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
// let's try defining a 'maybeBuilder' so that we could use a CE to hide the innards. Luckily the `Option` module has a lot of the implemenation already!

type maybeBuilder() = 
    /// bind lets us map + flatten
    member this.Bind(optionValue: 'a option, bindFn: 'a -> 'b option) = Option.bind bindFn optionValue
    // return lets us 'lift' a value
    member this.Return(value: 'a) = Some value
    // delay makes it so that nothing in the CE is run until the CE is evaluated.
    // This makes it so a println in an `async` block doesn't fire until the `async` is started, for example
    member this.Delay(action: unit -> 'a option) = action ()

let maybe = maybeBuilder ()

let niceCanCouncilMeet =
    maybe {
        let! jorah = getJorah ()
        let! barristan = getBarristan ()
        let! tyrion = getTyrion ()
        return true
    }
    |> Option.defaultValue false


// we can check that they are equivalent
canCouncilMeet = niceCanCouncilMeet

