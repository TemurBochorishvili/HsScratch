module List' = 

    let inline map fab ma = List.foldBack (fun a mb -> (fab a)::mb) ma []
    
    let inline result a = [ a ]

    let inline apply mfab ma = List.foldBack (fun fab mb -> List.foldBack (fun a mb -> (fab a)::mb) ma mb) mfab []

    let inline lift2 fabc ma mb = List.foldBack (fun a mc -> List.foldBack (fun b mc -> (fabc a b)::mc) mb mc) ma []

    let inline bind famb ma = List.foldBack (fun a mb -> (famb a) @ mb) ma []

    let inline empty<'a> : List<'a> = []

    let inline alternative ma1 ma2 = ma1 @ ma2


    type Builder() =
        member inline __.BindFrom(ma, fab) = map fab ma
        member inline __.Return(a) = result a
        member inline __.MergeSources(ma, mb) = lift2 (fun a b -> a, b) ma mb
        member inline __.Bind(ma, famb) = bind famb ma
        member inline __.ReturnFrom(ma) = ma


let listm = List'.Builder ()




//
let divide numerator denominator =
    match denominator with
    | 0.0 -> List'.empty
    | denominator -> List'.result (numerator / denominator)

let sqrt' number =
    match number with
    | 0.0 -> List'.result 0.0
    | number when number < 0.0 -> List'.empty
    | number -> let s = sqrt number in List'.alternative (List'.result -s) (List'.result s)


let squareEquation a b c = listm {
    let d = b * b - 4.0 * a * c

    let! sqrtd = sqrt' d

    let! x = divide (-b + sqrtd) (2.0 * a)

    return x
}

let biquadraticEquation a b c = listm {
    let! y = squareEquation a b c

    let! x = sqrt' y

    return x
}


[<EntryPoint>]
let main argv =

    printfn "%A" (biquadraticEquation 1.0 -9.0 0.0)

    0
