open System
open Elmish
open Elmish.WPF
open MyMainWinodw


let makeNameGenerator prefix =
  let mutable intId = 0
  fun () -> 
        intId <- (+) intId 1
        (prefix <| intId)

let createTabId = makeNameGenerator (sprintf "-%03i")

type Tab = {
    Name : string
    }

type Model =  { 
    Tabs: Tab list
    SelectedTab : Tab option
   }

let init = {
    Tabs = []
    SelectedTab = None
    }

type Msg =
  | AddTab
  | CloseTab
  | SetSelectedTab of Tab option
    
let update msg m =
  match msg with
  | AddTab -> {m with Tabs = (m.Tabs |> List.append [{Name = "Tab" + createTabId ()}])}
  | CloseTab -> match m.SelectedTab with
                |Some t -> {m with Tabs = m.Tabs |> List.filter ((<>) t)}
                |None -> m
  | SetSelectedTab t -> {m with SelectedTab = t}

let bindings () : Binding<Model, Msg> list = [
    "Tabs" |> Binding.subModelSeq((fun m -> m.Tabs),(fun s -> s), fun () -> [
                                                                                "Name" |> Binding.oneWay(fun (_, e) -> e.Name)
                                                                                "CloseTab" |> Binding.cmd(CloseTab)])
    "AddTab" |> Binding.cmd(AddTab)
    "SelectedTab" |> Binding.subModelSelectedItem("Tabs",(fun m -> m.SelectedTab),(fun t -> SetSelectedTab t))
]   

[<EntryPoint; STAThread>]
let main argv =
  Program.mkSimpleWpf (fun () -> init) update bindings
  |> Program.withConsoleTrace
  |> Program.runWindowWithConfig
    { ElmConfig.Default with LogConsole = true; Measure = true }
    (MainWindow())
