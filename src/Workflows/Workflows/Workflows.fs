module Workflows
open System

/// Workflow that performs arithmetic operations with accuracy given
type RoundingBuilder(accuracy: int) =
    member this.Bind(x: float, f) = f (Math.Round(x, accuracy))
    member this.Return(x: float) = Math.Round(x, accuracy)

/// Workflow that performs arithmetic operations with numbers represented in string format
type StringCalculatingBuilder() =
    member this.Bind(x: string, f) =
        try
            x |> int |> f
        with
        | :? FormatException -> None

    member this.Return(x) = Some x
