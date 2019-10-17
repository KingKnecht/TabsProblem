open System
open Elmish
open Elmish.WPF
open MyMainWinodw

let makeNameGenerator prefix =
    let mutable intId = 0
    fun () ->
        intId <- (+) intId 1
        prefix <| intId

let createTabId = makeNameGenerator (sprintf "-%03i")

type Tab =
    {
        Id: Guid
        Name: string
    }

type Model =
    {
        Tabs: Tab list
        SelectedTab: Guid option
    }

let init =
    {
        Tabs = []
        SelectedTab = None
    }

type Msg =
    | AddTab
    | CloseTab of tabId: Guid
    | SelectTab of tabId: Guid option

let update msg m =
    match msg with
    | AddTab ->
            let newGuid = Guid.NewGuid ()
            let newTab = { Id = newGuid; Name = "Tab" + createTabId () }
            { m with Tabs = List.append m.Tabs [ newTab ]; SelectedTab = Some newGuid }
    | CloseTab tabId ->
        let newTabs = m.Tabs |> List.filter (fun t -> t.Id <> tabId)
        // Logic for selected tab: Keep existing selected if not removed. If
        // selected tab is removed, select next tab in list, or the new last tab
        // if we removed the last tab, or no tab if empty
        let newSelectedTabId =
          if m.SelectedTab <> Some tabId then m.SelectedTab
          else
            let selectedIdx = m.Tabs |> List.findIndex (fun t -> t.Id = tabId)
            newTabs
            |> List.tryItem selectedIdx
            |> Option.orElse (newTabs |> List.tryLast)
            |> Option.map (fun t -> t.Id)
        { m with Tabs = newTabs; SelectedTab = newSelectedTabId }
    | SelectTab t ->
        { m with SelectedTab = t }

let bindings () : Binding<Model, Msg> list =
    [
    "Tabs" |> Binding.subModelSeq((fun m -> m.Tabs), (fun s -> s), fun () ->
        [
            "Id" |> Binding.oneWay (fun (_, t) -> t.Id)
            "Name" |> Binding.oneWay (fun (_, t) -> t.Name)
            "Close" |> Binding.cmd (fun (_, t) -> CloseTab t.Id)
        ])
    "AddTab" |> Binding.cmd AddTab
    "SelectedTab" |> Binding.twoWayOpt((fun m -> m.SelectedTab), SelectTab)
    ]

[<EntryPoint; STAThread>]
let main argv =
    Program.mkSimpleWpf (fun () -> init) update bindings
    |> Program.withConsoleTrace
    |> Program.runWindowWithConfig
        { ElmConfig.Default with LogConsole = true; Measure = true }
        (MainWindow())
