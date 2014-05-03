open System
open System.Windows
open FsXaml

type App = XAML<"App.xaml">

[<STAThread>]
[<EntryPoint>]
let main _ = 
    App().CreateRoot().Run()
