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
            (fun () -> 
                Process.Start property.Url |> ignore
                Process.Start (GoogleMapsQuery property.LatLong).Url |> ignore),
            (fun () -> !status = Status.Shortlisted)) 

    let selectCommand =
        self.Factory.CommandSyncChecked(
            (fun () -> if addToShortlistCommand.CanExecute()
                       then addToShortlistCommand.Execute() 
                       else openInBrowserCommand.Execute()),
            (fun () -> addToShortlistCommand.CanExecute() || openInBrowserCommand.CanExecute())) 
    
    let commuteDuration = self.Factory.Backing(<@ self.CommuteDuration @>, None)

    member x.CommuteDuration with get() = commuteDuration.Value and set value = commuteDuration.Value <- value

    member x.Property = property

    member x.Photos =
        x.Property.Photos @ [ "https://maps.googleapis.com/maps/api/staticmap?zoom=15&size=500x250&sensor=false&markers=" + x.Property.LatLong.ToString()
                              "https://maps.googleapis.com/maps/api/staticmap?zoom=12&size=500x250&sensor=false&markers=" + x.Property.LatLong.ToString() ]

    member x.SelectCommand = selectCommand
    member x.DiscardCommand = discardCommand

type PropertiesViewModel() as self =
    inherit ViewModelBase()
    
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
        self.TotalCount <- self.TotalCount + 1
        propertyViewModel

    let stateFilename = "properties.json"

    let loadState() =
        let add status (property, commuteDuration) =
            let propertyViewModel = add status property
            propertyViewModel.CommuteDuration <- commuteDuration
        if File.Exists stateFilename then
            let newProps, shortlistedProps, discardedProps =
                JsonConvert.DeserializeObject<_>(File.ReadAllText stateFilename)
            List.iter (add Status.New) newProps
            List.iter (add Status.Shortlisted) shortlistedProps
            List.iter (add Status.Discarded) discardedProps

    let saveState() = 
        let getState (propertyViewModel:PropertyViewModel) = 
            propertyViewModel.Property, propertyViewModel.CommuteDuration
        let state =
            Seq.map getState newProperties,
            Seq.map getState shortlistedProperties,
            Seq.map getState discardedProperties
        File.WriteAllText(stateFilename, JsonConvert.SerializeObject state)

    let totalCount = self.Factory.Backing(<@ self.TotalCount @>, 0)

    member x.NewProperties = newProperties
    member x.ShortlistedProperties = shortlistedProperties
    member x.DiscardedProperties = discardedProperties

    member x.TotalCount with get() = totalCount.Value and set value = totalCount.Value <- value

    member x.SeenPropertyUrls =
        let getUrl (propertyViewModel:PropertyViewModel) =
            propertyViewModel.Property.Url
        Seq.concat
            [ Seq.map getUrl newProperties
              Seq.map getUrl shortlistedProperties
              Seq.map getUrl discardedProperties ]
        |> Seq.toList

    member x.Add property = 
        add Status.New property

    member x.LoadState() = loadState()
    member x.SaveState() = saveState()

type MainWindowViewModel(propertiesViewModel:PropertiesViewModel) as self = 
    inherit ViewModelBase()

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
                | Some (_, duration) -> duration <= self.MaxCommuteDuration
            showListing

    let calcCommuteDistance workLocationLatLong (propertyViewModel:PropertyViewModel) = async {
        let! duration = GoogleMapsQuery.GetCommuteDuration propertyViewModel.Property.LatLong workLocationLatLong
        propertyViewModel.CommuteDuration <- Some (workLocationLatLong, duration)
    }

    let updateWorkLocationLatLong() =
        async {
            let! newWorkLocationLatLong = LatLong.fromAddress self.WorkLocation
            if Some newWorkLocationLatLong <> self.WorkLocationLatLong then
                self.WorkLocationLatLong <- Some newWorkLocationLatLong
                for property in propertiesViewModel.NewProperties do
                    match property.CommuteDuration with
                    | Some (workLocationLatLong, _) when workLocationLatLong = newWorkLocationLatLong -> ()
                    | _ ->
                        property.CommuteDuration <- None
                        calcCommuteDistance newWorkLocationLatLong property |> Async.Start
        } |> Async.Start

    let context = SynchronizationContext.Current

    let addProperty property = async {
        do! Async.SwitchToContext context
        let propertyViewModel = propertiesViewModel.Add property
        do! Async.SwitchToThreadPool()
        match self.WorkLocationLatLong with
        | None -> ()
        | Some workLocationLatLong -> 
            calcCommuteDistance workLocationLatLong propertyViewModel |> Async.Start
    }

    let crawler = Crawler(propertiesViewModel.SeenPropertyUrls, addProperty, [ Zoopla.T() ])

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

    do 
        setFilter()
        updateWorkLocationLatLong()

    member x.NewPropertiesView = newPropertiesView
    member x.Properties = propertiesViewModel
    
    member x.IsRunning with get() = isRunning.Value and set value = isRunning.Value <- value
    member x.MinPrice with get() = minPrice.Value and set value = minPrice.Value <- value; newPropertiesView.Refresh()
    member x.MaxPrice with get() = maxPrice.Value and set value = maxPrice.Value <- value; newPropertiesView.Refresh()
    member x.MinBeds with get() = minBeds.Value and set value = minBeds.Value <- value; newPropertiesView.Refresh()
    member x.MaxBeds with get() = maxBeds.Value and set value = maxBeds.Value <- value; newPropertiesView.Refresh()
    member x.MinPhotos with get() = minPhotos.Value and set value = minPhotos.Value <- value; newPropertiesView.Refresh()
    member x.Search with get() = search.Value and set value = search.Value <- value; newPropertiesView.Refresh()
    member x.NegativeSearch with get() = negativeSearch.Value and set value = negativeSearch.Value <- value; newPropertiesView.Refresh()
    member x.WorkLocation with get() = workLocation.Value and set value = workLocation.Value <- value; updateWorkLocationLatLong()
    member x.WorkLocationLatLong with get() = workLocationLatLong.Value and set value = workLocationLatLong.Value <- value
    member x.MaxCommuteDuration with get() = maxCommuteDuration.Value and set value = maxCommuteDuration.Value <- value; newPropertiesView.Refresh()

    member x.StartStopCommand = startStopCommand

    new() =
        let propertiesViewModel = PropertiesViewModel()
        propertiesViewModel.LoadState()
        Application.Current.Exit.Add <| fun _ -> propertiesViewModel.SaveState()
        MainWindowViewModel(propertiesViewModel)

type MockMainWindowViewModel() = 
    inherit MainWindowViewModel(MockMainWindowViewModel.GetMockData())

    static member private GetMockData() =
        let propertiesViewModel = PropertiesViewModel()
        let doc = 
            Path.Combine(__SOURCE_DIRECTORY__, "property.html")
            |> File.ReadAllText
            |> HtmlDocument.Parse
        let mockProperty = (Zoopla.T() :> IPropertySite).ParsePropertyPage doc Property.Mock
        let propertyViewModel = propertiesViewModel.Add mockProperty
        propertyViewModel.CommuteDuration <- Some (LatLong.parse "-1" "-1", 25)
        propertiesViewModel

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
