namespace HouseHunter

open System
open System.Collections.ObjectModel
open System.Diagnostics
open System.Windows
open System.Windows.Data
open System.Threading
open FsWpf
open FSharp.ViewModule.Core.ViewModel

type PropertyViewModel(property) as self = 
    inherit ViewModelBase()

    let openInBrowserCommand = 
        self.Factory.CommandSync <| fun () ->
            Process.Start property.Url |> ignore

    member x.Property = property

    member x.OpenInBrowserCommand = openInBrowserCommand

type MainWindowViewModel() as self = 
    inherit ViewModelBase()

    let properties = ObservableCollection<_>()
    let view = CollectionViewSource.GetDefaultView(properties) :?> ListCollectionView

    let textMatchesQuery (query:string) (text:string) = 
        query.Split([|'&'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.forall (fun query -> query.Split([|'|'|], StringSplitOptions.RemoveEmptyEntries)
                                      |> Array.map String.trim
                                      |> Array.filter ((<>) "")
                                      |> Array.exists (fun query -> text.IndexOf(query, StringComparison.OrdinalIgnoreCase) >= 0))
    
    let propertyMatchesQuery query (property:Property) = 
        property.GetSearchableContent()
        |> List.exists (textMatchesQuery query)

    let updateCount() = 
        self.Count <- sprintf "%d/%d" view.Count properties.Count

    let setFilter() =
        view.Filter <- fun property -> 
            let property = (property :?> PropertyViewModel).Property
            let showListing = 
                property.Photos.Length >= self.MinPhotos &&
                property.Price >= self.MinPrice &&
                property.Price <= self.MaxPrice &&
                propertyMatchesQuery self.Search property &&
                not (propertyMatchesQuery self.NegativeSearch property)
            showListing
        updateCount()

    let refreshFilter() =
        view.Refresh()
        updateCount()

    let context = SynchronizationContext.Current

    let addProperty property = async {
        do! Async.SwitchToContext context
        properties.Add (PropertyViewModel property)
        updateCount()
        do! Async.SwitchToThreadPool()
    }

    let bulkAddProperties propertyBatch = 
        for property in propertyBatch do
            properties.Add (PropertyViewModel property)

    let crawler = Crawler(addProperty, bulkAddProperties, [ Zoopla.T() ])

    let currentCts : CancellationTokenSource option ref = ref None

    let onStarted cts =
        currentCts := Some cts
        self.IsRunning <- true

    let onStopped() = 
        currentCts := None
        self.IsRunning <- false

    let startStopCommand = 
        self.Factory.CommandSync <| fun () ->
            if self.IsRunning then
                currentCts.Value.Value.Cancel()
                currentCts.Value.Value.Dispose()
                onStopped()
            else
                let cts = new CancellationTokenSource()
                let computation = async {
                    do! crawler.Crawl (self.MinPrice, self.MaxPrice, self.MinBeds, self.MaxBeds)
                    onStopped()
                }
                onStarted cts
                Async.Start(computation, cts.Token)

    let count = self.Factory.Backing(<@ self.Count @>, "0/0")
    let isRunning = self.Factory.Backing(<@ self.IsRunning @>, false)
    let minPrice = self.Factory.Backing(<@ self.MinPrice @>, 1000M)
    let maxPrice = self.Factory.Backing(<@ self.MaxPrice @>, 1600M)
    let minBeds = self.Factory.Backing(<@ self.MinBeds @>, 1)
    let maxBeds = self.Factory.Backing(<@ self.MaxBeds @>, 3)
    let minPhotos = self.Factory.Backing(<@ self.MinPhotos @>, 3)
    let search = self.Factory.Backing(<@ self.Search @>, "")
    let negativeSearch = self.Factory.Backing(<@ self.NegativeSearch @>, "stratford | woolwich | croydon | peckham")
    let isMock = self.Factory.Backing(<@ self.IsMock @>, false)

    do 
        Application.Current.Exit.Add <| fun _ -> if not (self.IsMock) then crawler.SaveState()
        setFilter()

    member x.Properties = properties
    member x.Count with get() = count.Value and set value = count.Value <- value
    member x.IsRunning with get() = isRunning.Value and set value = isRunning.Value <- value
    member x.MinPrice with get() = minPrice.Value and set value = minPrice.Value <- value
    member x.MaxPrice with get() = maxPrice.Value and set value = maxPrice.Value <- value
    member x.MinBeds with get() = minBeds.Value and set value = minBeds.Value <- value
    member x.MaxBeds with get() = maxBeds.Value and set value = maxBeds.Value <- value
    member x.MinPhotos with get() = minPhotos.Value and set value = minPhotos.Value <- value; refreshFilter()
    member x.Search with get() = search.Value and set value = search.Value <- value; refreshFilter()
    member x.NegativeSearch with get() = negativeSearch.Value and set value = negativeSearch.Value <- value; refreshFilter()

    member x.StartStopCommand = startStopCommand

    member x.IsMock with get() = isMock.Value and set value = isMock.Value <- value; if value then properties.Clear(); properties.Add (PropertyViewModel crawler.MockProperty)

// TODO:
//links (floorplan)
//distance to
//TFL Zone
//new/favorites/deleted
//save state

type ListConverter() = 
    inherit ConverterToStringMarkupExtension<string list>()
    override x.Convert list = 
        list
        |> String.concat Environment.NewLine

type ListWithIndentationConverter() = 
    inherit ConverterToStringMarkupExtension<string list>()
    override x.Convert list = 
        list
        |> List.map ((+) "    ")
        |> String.concat Environment.NewLine