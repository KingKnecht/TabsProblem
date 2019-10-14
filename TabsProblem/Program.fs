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

let init = 
    {
        Tabs = []
        SelectedTab = None
    }
    , Cmd.none

type Msg =
  | AddTab
  | CloseTab
  | CloseTab2 of Tab
  | SetSelectedTab of Tab option
    
let update msg m =
  match msg with
  | AddTab -> {m with Tabs = (m.Tabs |> List.append [{Name = "Tab" + createTabId ()}])}, Cmd.none
  | CloseTab -> match m.SelectedTab with
                | Some t -> { m with SelectedTab = None }, Cmd.ofMsg (CloseTab2 t)
                | None -> m, Cmd.none
  | CloseTab2 tabToDelete -> {m with Tabs = m.Tabs |> List.filter ((<>) tabToDelete)}, Cmd.none
  | SetSelectedTab t -> {m with SelectedTab = t}, Cmd.none


let bindings () : Binding<Model, Msg> list = [
    "Tabs" |> Binding.subModelSeq((fun m -> m.Tabs),(fun s -> s), fun () -> [
                                                                                "Name" |> Binding.oneWay(fun (_, e) -> e.Name)
                                                                                "CloseTab" |> Binding.cmd(CloseTab)])
    "AddTab" |> Binding.cmd(AddTab)
    "SelectedTab" |> Binding.subModelSelectedItem("Tabs",(fun m -> m.SelectedTab),(fun t -> SetSelectedTab t))
]   

[<EntryPoint; STAThread>]
let main argv =
  Program.mkProgramWpf  (fun () -> init) update bindings
  |> Program.withConsoleTrace
  |> Program.runWindowWithConfig
    { ElmConfig.Default with LogConsole = true; Measure = true }
    (MainWindow())
