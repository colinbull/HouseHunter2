namespace HouseHunter

open System
open System.IO
open FSharp.Data
open HtmlAgilityPack
open Nessos.FsPickler

type LatLong = 
    { Lat : float
      Long : float } 
    static member parse lat long =
        { Lat = Double.Parse lat
          Long = Double.Parse long }
    static member fromAddress str = async {
        let url = "http://maps.googleapis.com/maps/api/geocode/xml?sensor=false&address=" + (Uri.EscapeDataString str)
        let! geocodeResponse = XmlProvider<"http://maps.googleapis.com/maps/api/geocode/xml?sensor=false&address=london">.AsyncLoad url
        let location = geocodeResponse.Results.[0].Geometry.Location
        return { Lat = double location.Lat
                 Long = double location.Lat } }
    override x.ToString() = 
        sprintf "%f,%f" x.Lat x.Long

type Property =
    { Url : string
      Name : string
      Description : string
      Address : string
      LatLong : LatLong
      Price : decimal
      Photos : string list
      Features : string list
      Nearby : string list
      Phone : string
      AddedOn : string }

    override x.ToString() = sprintf "%A" x

    member x.SearchableContent = [ x.Name; x.Description; x.Address ] @ x.Features @ x.Nearby

    static member Mock = 
        { Url = ""
          Name = ""
          Description = ""
          Address = ""
          LatLong = LatLong.parse "-1" "-1"
          Price = 1000M
          Photos = []
          Features = []
          Nearby = [ "London Victoria (0.1 miles)" ]
          Phone = "02080000000"
          AddedOn = "3rd May 2014" }

type IPropertySite =

    abstract GetFirstListingPage : minPrice:decimal * maxPrice:decimal * minBeds:int * maxBeds:int -> string
    abstract ParseListingPage : doc:HtmlDocument -> properties:Property seq * nextPageUrl:string option
    abstract ParsePropertyPage : doc:HtmlDocument -> property:Property -> Property

type Crawler(addProperty, bulkAddProperties, propertySites) = 

    let allProperties = ref Map.empty
    
    let add property = async {
        
        lock allProperties <| fun () ->
            allProperties := Map.add property.Url property !allProperties

        do! addProperty property
    }

    let fsp = FsPickler()
    let stateFilename = "properies.bin"

    let loadState() =
        if File.Exists stateFilename then
            try
                using (File.OpenRead stateFilename) <| fun stream ->
                    allProperties := fsp.Deserialize<_>(stream)
            with _ -> ()
            !allProperties |> Map.toList |> List.map snd |> bulkAddProperties 

    let saveState() = 
        lock allProperties <| fun () ->
            using (File.OpenWrite stateFilename) <| fun stream ->
                fsp.Serialize(stream, !allProperties)

    do loadState()
    
    let rec processPage (propertySite:IPropertySite) url = async {

        Console.WriteLine(sprintf "%s" url)
    
        let! html = Http.AsyncRequestString url
        let doc = HtmlDocument.Parse html
    
        let properties, nextPageUrl = propertySite.ParseListingPage doc

        let childJobs = properties |> Seq.map (fun property -> async {

            let url = property.Url
            Console.WriteLine(sprintf "%s" url)

            let! html = Http.AsyncRequestString url
            let doc = HtmlDocument.Parse html
                    
            let property = propertySite.ParsePropertyPage doc property
            do! add property
        })

        for job in childJobs do
            do! job |> Async.StartChild |> Async.Ignore
        
        match nextPageUrl with
        | Some nextPageUrl -> return! processPage propertySite nextPageUrl
        | None -> ()
    }

    member x.Crawl args =
        propertySites
        |> List.map (fun (propertySite:IPropertySite) -> propertySite.GetFirstListingPage args |> processPage propertySite)
        |> Seq.toArray
        |> Async.Parallel
        |> Async.Ignore

    member x.MockProperty =
        let doc = 
            Path.Combine(__SOURCE_DIRECTORY__, "property.html")
            |> File.ReadAllText
            |> HtmlDocument.Parse
        propertySites.[0].ParsePropertyPage doc Property.Mock

    member x.SaveState() =
        saveState()

