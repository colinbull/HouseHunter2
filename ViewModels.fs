namespace HouseHunter

open System
open System.Collections.ObjectModel
open System.Diagnostics
open System.IO  
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Documents
open System.Threading
open HtmlAgilityPack
open FsWpf
open FSharp.ViewModule.Core.ViewModel
open Newtonsoft.Json

type Status =
    | New
    | Shortlisted
    | Discarded

type PropertyViewModel(property:Property, status, onStatusChanged) as self = 
    inherit ViewModelBase()

    let status = ref status

    let changeStatus newStatus =
        onStatusChanged !status newStatus self
        status := newStatus

    let addToShortlistCommand = 
        self.Factory.CommandSyncChecked(
            (fun () -> changeStatus Status.Shortlisted),
            (fun () -> !status <> Status.Shortlisted)) 

    let discardCommand = 
        self.Factory.CommandSyncChecked(
            (fun () -> changeStatus Status.Discarded),
            (fun () -> !status <> Status.Discarded)) 

    let openInBrowserCommand = 
        self.Factory.CommandSyncChecked(
            (fun () -> Process.Start property.Url |> ignore),
            (fun () -> !status = Status.Shortlisted)) 

    let selectCommand =
        self.Factory.CommandSyncChecked(
            (fun () -> if addToShortlistCommand.CanExecute()
                       then addToShortlistCommand.Execute() 
                       else openInBrowserCommand.Execute()),
            (fun () -> addToShortlistCommand.CanExecute() || openInBrowserCommand.CanExecute())) 
    
    let commuteDuration = self.Factory.Backing(<@ self.CommuteDuration @>, None)

    member x.CommuteDuration with get() : int option = commuteDuration.Value and set value = commuteDuration.Value <- value

    member x.Property = property

    member x.SelectCommand = selectCommand
    member x.DiscardCommand = discardCommand

    static member GetProperty(x:PropertyViewModel) = x.Property
    static member GetUrl(x:PropertyViewModel) = x.Property.Url

type PropertiesViewModel() =
    
    let newProperties = ObservableCollection<_>()
    let shortlistedProperties = ObservableCollection<_>()
    let discardedProperties = ObservableCollection<_>()

    let getCollection status =
        match status with
        | New -> newProperties
        | Shortlisted -> shortlistedProperties
        | Discarded -> discardedProperties

    let onStatusChanged oldStatus newStatus propertyViewModel =
        let removed = (getCollection oldStatus).Remove propertyViewModel
        assert removed
        (getCollection newStatus).Add propertyViewModel

    let add status property = 
        let propertyViewModel = PropertyViewModel(property, status, onStatusChanged)
        (getCollection status).Add propertyViewModel
        propertyViewModel

    let stateFilename = "properties.json"
    let stateFilename2 = "properties.bin"

    let loadState() =
        if File.Exists stateFilename then
            let newProps, shortlistedProps, discardedProps =
                JsonConvert.DeserializeObject<_>(File.ReadAllText stateFilename)
            List.iter (add Status.New >> ignore) newProps
            List.iter (add Status.Shortlisted >> ignore) shortlistedProps
            List.iter (add Status.Discarded >> ignore) discardedProps

    let saveState() = 
        let state =
            Seq.map PropertyViewModel.GetProperty newProperties,
            Seq.map PropertyViewModel.GetProperty shortlistedProperties,
            Seq.map PropertyViewModel.GetProperty discardedProperties
        File.WriteAllText(stateFilename, JsonConvert.SerializeObject state)

    do loadState()

    member x.NewProperties = newProperties
    member x.ShortlistedProperties = shortlistedProperties
    member x.DiscardedProperties = discardedProperties

    member x.SeenPropertyUrls =
        Seq.concat
            [ Seq.map PropertyViewModel.GetUrl newProperties
              Seq.map PropertyViewModel.GetUrl shortlistedProperties
              Seq.map PropertyViewModel.GetUrl discardedProperties ]
        |> Seq.toList

    member x.TotalCount = 
        newProperties.Count + shortlistedProperties.Count + discardedProperties.Count

    member x.Add property = add Status.New property

    member x.SaveState() = saveState()

type MainWindowViewModel() as self = 
    inherit ViewModelBase()

    let propertiesViewModel = PropertiesViewModel()

    let newPropertiesView = CollectionViewSource.GetDefaultView(propertiesViewModel.NewProperties) :?> ListCollectionView
    do 
        newPropertiesView.IsLiveFiltering <- Nullable true
        newPropertiesView.LiveFilteringProperties.Add("CommuteDuration")

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
        self.CountStr <- sprintf "%d/%d" newPropertiesView.Count propertiesViewModel.TotalCount

    let setFilter() =
        newPropertiesView.Filter <- fun property -> 
            let propertyViewModel = property :?> PropertyViewModel
            let property = propertyViewModel.Property
            let showListing = 
                property.Photos.Length >= self.MinPhotos &&
                property.Price >= self.MinPrice &&
                property.Price <= self.MaxPrice &&
                propertyMatchesQuery self.Search property &&
                (self.NegativeSearch = "" || not (propertyMatchesQuery self.NegativeSearch property)) &&
                match propertyViewModel.CommuteDuration with
                | None -> true
                | Some duration -> duration <= self.MaxCommuteDuration
            showListing
        updateCount()

    let refreshFilter() =
        newPropertiesView.Refresh()
        updateCount()

    let context = SynchronizationContext.Current

    let calcCommuteDistance workLocationLatLong (propertyViewModel:PropertyViewModel) = async {
        let! duration = GoogleMapsQuery.GetCommuteDuration propertyViewModel.Property.LatLong workLocationLatLong
        propertyViewModel.CommuteDuration <- duration
    }

    let addProperty property = async {
        do! Async.SwitchToContext context
        let propertyViewModel = propertiesViewModel.Add property
        updateCount()
        do! Async.SwitchToThreadPool()
        match self.WorkLocationLatLong with
        | None -> ()
        | Some workLocationLatLong -> 
            calcCommuteDistance workLocationLatLong propertyViewModel |> Async.Start
    }

    let propertySites = [ Zoopla.T() :> IPropertySite ]
    let crawler = Crawler(propertiesViewModel.SeenPropertyUrls, addProperty, propertySites)

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

    let updateWorkLocationLatLong() =
        async {
            let! latLong = LatLong.fromAddress self.WorkLocation
            if Some latLong <> self.WorkLocationLatLong then
                self.WorkLocationLatLong <- Some latLong
                for property in propertiesViewModel.NewProperties do
                    property.CommuteDuration <- None
                    calcCommuteDistance latLong property |> Async.Start                    
        } |> Async.Start

    let countStr = self.Factory.Backing(<@ self.CountStr @>, "0/0")
    let isRunning = self.Factory.Backing(<@ self.IsRunning @>, false)
    let minPrice = self.Factory.Backing(<@ self.MinPrice @>, 1000M)
    let maxPrice = self.Factory.Backing(<@ self.MaxPrice @>, 1600M)
    let minBeds = self.Factory.Backing(<@ self.MinBeds @>, 1)
    let maxBeds = self.Factory.Backing(<@ self.MaxBeds @>, 3)
    let minPhotos = self.Factory.Backing(<@ self.MinPhotos @>, 3)
    let search = self.Factory.Backing(<@ self.Search @>, "")
    let negativeSearch = self.Factory.Backing(<@ self.NegativeSearch @>, "stratford | woolwich | croydon | peckham")
    let workLocation = self.Factory.Backing(<@ self.WorkLocation @>, "London Victoria")
    let workLocationLatLong = self.Factory.Backing(<@ self.WorkLocationLatLong @>, None)
    let maxCommuteDuration = self.Factory.Backing(<@ self.MaxCommuteDuration @>, 45)

    let isMock = self.Factory.Backing(<@ self.IsMock @>, false)

    do 
        Application.Current.Exit.Add <| fun _ -> 
            if not (self.IsMock) then 
                propertiesViewModel.SaveState()
        updateWorkLocationLatLong()
        setFilter()

    member x.NewProperties = propertiesViewModel.NewProperties
    member x.ShortlistedProperties = propertiesViewModel.ShortlistedProperties
    member x.DiscardedProperties = propertiesViewModel.DiscardedProperties
    
    member x.CountStr with get() = countStr.Value and set value = countStr.Value <- value
    member x.IsRunning with get() = isRunning.Value and set value = isRunning.Value <- value
    member x.MinPrice with get() = minPrice.Value and set value = minPrice.Value <- value; refreshFilter()
    member x.MaxPrice with get() = maxPrice.Value and set value = maxPrice.Value <- value; refreshFilter()
    member x.MinBeds with get() = minBeds.Value and set value = minBeds.Value <- value; refreshFilter()
    member x.MaxBeds with get() = maxBeds.Value and set value = maxBeds.Value <- value; refreshFilter()
    member x.MinPhotos with get() = minPhotos.Value and set value = minPhotos.Value <- value; refreshFilter()
    member x.Search with get() = search.Value and set value = search.Value <- value; refreshFilter()
    member x.NegativeSearch with get() = negativeSearch.Value and set value = negativeSearch.Value <- value; refreshFilter()
    member x.WorkLocation with get() = workLocation.Value and set value = workLocation.Value <- value; updateWorkLocationLatLong()
    member x.WorkLocationLatLong with get() = workLocationLatLong.Value and set value = workLocationLatLong.Value <- value
    member x.MaxCommuteDuration with get() = maxCommuteDuration.Value and set value = maxCommuteDuration.Value <- value; refreshFilter()

    member x.StartStopCommand = startStopCommand

    member x.IsMock
        with get() = isMock.Value 
        and set value = 
            isMock.Value <- value
            if value then                
                let doc = 
                    Path.Combine(__SOURCE_DIRECTORY__, "property.html")
                    |> File.ReadAllText
                    |> HtmlDocument.Parse                
                propertiesViewModel.Add(propertySites.[0].ParsePropertyPage doc Property.Mock) |> ignore

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

type HyperlinkConverter() =
    inherit ConverterMarkupExtension<string*string, TextBlock>()
    override x.Convert arg =
        let text, url = arg
        let hyperLink = Hyperlink(Run(text), NavigateUri = Uri url)
        hyperLink.RequestNavigate.Add <| fun _ ->
            Process.Start url |> ignore
        TextBlock(hyperLink)
