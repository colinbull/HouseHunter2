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
open FSharp.RegexProvider
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
                Process.Start(property.Url) |> ignore
                Process.Start(self.MapUrl:string) |> ignore
                match self.DirectionsUrl1 with
                | Some url -> Process.Start(url:string) |> ignore
                | None -> ()
                match self.DirectionsUrl2 with
                | Some url -> Process.Start(url:string) |> ignore
                | None -> ()),
            (fun () -> !status = Status.Shortlisted)) 

    let selectCommand =
        self.Factory.CommandSyncChecked(
            (fun () -> if addToShortlistCommand.CanExecute()
                       then addToShortlistCommand.Execute() 
                       else openInBrowserCommand.Execute()),
            (fun () -> addToShortlistCommand.CanExecute() || openInBrowserCommand.CanExecute())) 
    
    let commuteDuration1 = self.Factory.Backing(<@ self.CommuteDuration1 @>, None)
    let commuteDuration2 = self.Factory.Backing(<@ self.CommuteDuration2 @>, None)

    do self.DependencyTracker.AddPropertyDependencies(<@@ self.DirectionsUrl1 @@>, [ <@@ self.CommuteDuration1 @@> ])
    do self.DependencyTracker.AddPropertyDependencies(<@@ self.DirectionsUrl2 @@>, [ <@@ self.CommuteDuration2 @@> ])

    static let bedroomMatcher = new Regex<"(?<Bedrooms>\d{1,3}) bedroom">()
    let numBedrooms = 
        lazy 
            bedroomMatcher.Match(property.Name).Bedrooms.Value 
            |> Int32.TryParse 
            |> Option.fromTryParse 
            |> Option.get 0

    member x.CommuteDuration1 with get() = commuteDuration1.Value and set value = commuteDuration1.Value <- value
    member x.CommuteDuration2 with get() = commuteDuration2.Value and set value = commuteDuration2.Value <- value

    member x.NumBedrooms = numBedrooms.Value

    member x.Property = property

    member x.Photos =
        x.Property.Photos @ [ "https://maps.googleapis.com/maps/api/staticmap?zoom=15&size=500x250&sensor=false&markers=" + x.Property.LatLong.ToString() + "&key=AIzaSyD_FxyVWxPqbfT3zMle7OtDx_vF-LnD97I"
                              "https://maps.googleapis.com/maps/api/staticmap?zoom=12&size=500x250&sensor=false&markers=" + x.Property.LatLong.ToString() + "&key=AIzaSyD_FxyVWxPqbfT3zMle7OtDx_vF-LnD97I" ]

    member x.MapUrl =
        (GoogleMapsQuery x.Property.LatLong).Url

    member x.DirectionsUrl1 = 
        match x.CommuteDuration1 with
        | Some (latLong, _) -> Some <| GoogleMapsDirectionsAt9amNextWorkDay(x.Property.LatLong, latLong).Url
        | None -> None        

    member x.DirectionsUrl2 = 
        match x.CommuteDuration2 with
        | Some (latLong, _) -> Some <| GoogleMapsDirectionsAt9amNextWorkDay(x.Property.LatLong, latLong).Url
        | None -> None        

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
        let add status (property, commuteDuration1, commuteDuration2) =
            let propertyViewModel = add status property
            propertyViewModel.CommuteDuration1 <- commuteDuration1
            propertyViewModel.CommuteDuration2 <- commuteDuration2
        if File.Exists stateFilename then
            let newProps, shortlistedProps, discardedProps =
                JsonConvert.DeserializeObject<_>(File.ReadAllText stateFilename)
            List.iter (add Status.New) newProps
            List.iter (add Status.Shortlisted) shortlistedProps
            List.iter (add Status.Discarded) discardedProps

    let saveState() = 
        let getState (propertyViewModel:PropertyViewModel) = 
            propertyViewModel.Property, propertyViewModel.CommuteDuration1, propertyViewModel.CommuteDuration2
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
        newPropertiesView.LiveFilteringProperties.Add("CommuteDuration1")
        newPropertiesView.LiveFilteringProperties.Add("CommuteDuration2")

    let textMatchesQuery (query:string) (text:string) = 
        query.Split([|'&'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.forall (fun query -> query.Split([|'|'|], StringSplitOptions.RemoveEmptyEntries)
                                      |> Array.map String.trim
                                      |> Array.filter ((<>) "")
                                      |> Array.exists (fun query -> text.IndexOf(query, StringComparison.OrdinalIgnoreCase) >= 0))
    
    let propertyMatchesQuery query (property:Property) = 
        property.GetSearchableContent()
        |> List.exists (textMatchesQuery query)

    let acceptableCommuteDuration (propertyViewModel:PropertyViewModel) =
        match (self.WorkLocationLatLong1, propertyViewModel.CommuteDuration1), 
              (self.WorkLocationLatLong2, propertyViewModel.CommuteDuration2) with 
        | (Some _, Some (_, duration1)), (Some _, Some (_, duration2)) -> duration1 <= self.MaxCommuteDuration || duration2 <= self.MaxCommuteDuration
        | (Some _, Some (_, duration1)), _                             -> duration1 <= self.MaxCommuteDuration
        | _                            , (Some _, Some (_, duration2)) ->                                         duration2 <= self.MaxCommuteDuration
        | _ -> true

    let setFilter() =
        newPropertiesView.Filter <- fun property -> 
            let propertyViewModel = property :?> PropertyViewModel
            let property = propertyViewModel.Property
            let showListing = 
                property.Photos.Length >= self.MinPhotos
                && property.Price >= self.MinPrice
                && property.Price <= self.MaxPrice
                && propertyViewModel.NumBedrooms >= self.MinBeds
                && propertyViewModel.NumBedrooms <= self.MaxBeds
                && acceptableCommuteDuration propertyViewModel
                && propertyMatchesQuery self.Search property
                && (self.NegativeSearch = "" || not (propertyMatchesQuery self.NegativeSearch property))
            showListing

    let calcCommuteDuration1 latLong (propertyViewModel:PropertyViewModel) = async {
        let! duration = GoogleMapsQuery.GetCommuteDuration propertyViewModel.Property.LatLong latLong
        propertyViewModel.CommuteDuration1 <- Some (latLong, duration)
    }

    let calcCommuteDuration2 latLong (propertyViewModel:PropertyViewModel) = async {
        let! duration = GoogleMapsQuery.GetCommuteDuration propertyViewModel.Property.LatLong latLong
        propertyViewModel.CommuteDuration2 <- Some (latLong, duration)
    }

    let context = SynchronizationContext.Current

    let updateWorkLocationLatLong1() =
        async {
            if String.IsNullOrWhiteSpace self.WorkLocation1 then
                self.WorkLocationLatLong1 <- None
                do! Async.SwitchToContext context
                newPropertiesView.Refresh()
                do! Async.SwitchToThreadPool()
            else
                try
                    let! newLatLong = LatLong.fromAddress self.WorkLocation1
                    if Some newLatLong <> self.WorkLocationLatLong1 then
                        self.WorkLocationLatLong1 <- Some newLatLong
                        for property in propertiesViewModel.NewProperties do
                            match property.CommuteDuration1 with
                            | Some (latLong, _) when latLong = newLatLong -> ()
                            | _ ->
                                property.CommuteDuration1 <- None
                                calcCommuteDuration1 newLatLong property |> Async.Catch |> Async.Ignore |> Async.Start
                with _ ->
                    self.WorkLocationLatLong1 <- None
        } |> Async.Catch |> Async.Ignore |> Async.Start

    let updateWorkLocationLatLong2() =
        async {
            if String.IsNullOrWhiteSpace self.WorkLocation2 then
                self.WorkLocationLatLong2 <- None
                do! Async.SwitchToContext context
                newPropertiesView.Refresh()
                do! Async.SwitchToThreadPool()
            else
                try
                    let! newLatLong = LatLong.fromAddress self.WorkLocation2
                    if Some newLatLong <> self.WorkLocationLatLong2 then
                        self.WorkLocationLatLong2 <- Some newLatLong
                        for property in propertiesViewModel.NewProperties do
                            match property.CommuteDuration2 with
                            | Some (latLong, _) when latLong = newLatLong -> ()
                            | _ ->
                                property.CommuteDuration2 <- None
                                calcCommuteDuration2 newLatLong property |> Async.Catch |> Async.Ignore |> Async.Start
                with _ ->
                    self.WorkLocationLatLong2 <- None
        } |> Async.Catch |> Async.Ignore |> Async.Start

    let addProperty property = async {
        do! Async.SwitchToContext context
        let propertyViewModel = propertiesViewModel.Add property
        do! Async.SwitchToThreadPool()
        match self.WorkLocationLatLong1 with
        | None -> ()
        | Some latLong -> 
            calcCommuteDuration1 latLong propertyViewModel |> Async.Catch |> Async.Ignore |> Async.Start
        match self.WorkLocationLatLong2 with
        | None -> ()
        | Some latLong -> 
            calcCommuteDuration2 latLong propertyViewModel |> Async.Catch |> Async.Ignore |> Async.Start
    }

    let crawler = Crawler(propertiesViewModel.SeenPropertyUrls, addProperty, [ Zoopla.T(); RightMove.T() ])

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

    let saveStateCommand =
        self.Factory.CommandSync <| fun () -> propertiesViewModel.SaveState()

    let isRunning = self.Factory.Backing(<@ self.IsRunning @>, false)
    let minPrice = self.Factory.Backing(<@ self.MinPrice @>, 1000M)
    let maxPrice = self.Factory.Backing(<@ self.MaxPrice @>, 1500M)
    let minBeds = self.Factory.Backing(<@ self.MinBeds @>, 2)
    let maxBeds = self.Factory.Backing(<@ self.MaxBeds @>, 3)
    let minPhotos = self.Factory.Backing(<@ self.MinPhotos @>, 3)
    let search = self.Factory.Backing(<@ self.Search @>, "wooden floor")
    let negativeSearch = self.Factory.Backing(<@ self.NegativeSearch @>, "stratford | woolwich | croydon | peckham")
    let workLocation1 = self.Factory.Backing(<@ self.WorkLocation1 @>, "London Victoria")
    let workLocation2 = self.Factory.Backing(<@ self.WorkLocation2 @>, "London Bridge Station")
    let workLocationLatLong1 = self.Factory.Backing(<@ self.WorkLocationLatLong1 @>, None)
    let workLocationLatLong2 = self.Factory.Backing(<@ self.WorkLocationLatLong2 @>, None)
    let maxCommuteDuration = self.Factory.Backing(<@ self.MaxCommuteDuration @>, 30)

    do 
        setFilter()
        updateWorkLocationLatLong1()
        updateWorkLocationLatLong2()

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
    member x.WorkLocation1 with get() = workLocation1.Value and set value = workLocation1.Value <- value; updateWorkLocationLatLong1()
    member x.WorkLocation2 with get() = workLocation2.Value and set value = workLocation2.Value <- value; updateWorkLocationLatLong2()
    member x.WorkLocationLatLong1 with get() = workLocationLatLong1.Value and set value = workLocationLatLong1.Value <- value
    member x.WorkLocationLatLong2 with get() = workLocationLatLong2.Value and set value = workLocationLatLong2.Value <- value
    member x.MaxCommuteDuration with get() = maxCommuteDuration.Value and set value = maxCommuteDuration.Value <- value; newPropertiesView.Refresh()

    member x.StartStopCommand = startStopCommand
    member x.SaveStateCommand = saveStateCommand

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
        propertyViewModel.CommuteDuration1 <- Some (LatLong.Default, 25)
        propertyViewModel.CommuteDuration2 <- Some (LatLong.Default, 30)
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

type HyperlinkConverterWithParameter() =
    inherit ConverterWithParameterMarkupExtension<string, string, TextBlock>()
    override x.Convert url text =
        if url = null then null else
        let hyperLink = Hyperlink(Run(text), NavigateUri = Uri url)
        hyperLink.RequestNavigate.Add <| fun _ ->
            Process.Start url |> ignore
        TextBlock(hyperLink)
