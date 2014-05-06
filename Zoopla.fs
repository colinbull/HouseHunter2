#if INTERACTIVE
#I @"packages\FSharp.Data.2.0.8\lib\net40"
#I @"packages\HtmlAgilityPack.1.4.6\lib\Net40"
#I @"packages\RegexProvider.0.0.2\lib\net40"
#r "FSharp.Data.dll"
#r "System.Xml.Linq.dll"
#r "HtmlAgilityPack.dll"
#r "RegexProvider.dll"
#load "Utils.fs"
#load "DataModels.fs"
#else
module Zoopla
#endif

open System
open System.Web
open FSharp.Data
open FSharp.RegexProvider
open HtmlAgilityPack
open HouseHunter

let baseUrl = "http://www.zoopla.co.uk"
    
let adjustToRange possiblePrices minPrice maxPrice =
    possiblePrices |> Seq.skipWhile (fun x -> x < minPrice) |> Seq.takeWhile (fun x -> x <= maxPrice) |> Seq.pairwise
    
let getFirstListingPages minPrice maxPrice minBeds maxBeds = [
    let possiblePrices = [100..100..1000] @ [1250..250..5000] @ [5500..500..10000] @ [12500..2500..20000] @ [25000] |> List.map decimal
    for minPrice, maxPrice in adjustToRange possiblePrices minPrice maxPrice do
        let minPrice = minPrice |> sprintf "&price_min=%M"
        let maxPrice = maxPrice |> sprintf "&price_max=%M"
        for beds in minBeds..maxBeds do
            let minBeds = beds |> sprintf "&beds_min=%d"
            let maxBeds = beds |> sprintf "&beds_max=%d"
            yield sprintf "%s/to-rent/property/london/?include_retirement_homes=false&price_frequency=per_month&q=London&results_sort=newest_listings&search_source=home&page_size=100%s%s%s%s"
                          baseUrl minPrice maxPrice minBeds maxBeds ]    
    
let getListingItems (doc:HtmlDocument) =
    doc.DocumentNode.Descendants("li")
    |> Seq.filter (hasAttr "itemtype" "http://schema.org/Place")
    
let getNextListingPage (doc:HtmlDocument) =
    doc.DocumentNode.Descendants("div")
    |> Seq.filter (hasClass "paginate bg-muted")
    |> Seq.collect (elements "a")
    |> Seq.tryFind (innerText >> (=) "Next")
    |> Option.map (attr "href" >> HttpUtility.HtmlDecode >> ((+) baseUrl))
    
let pcmMatcher = new Regex<"(?<PCM>\d{1,3}(?:[,]\d{3})*)\s*pcm">()
    
let parseListingItem (li:HtmlNode) = 
    
    let props = getMicrodataProperties li
    
    let propertyUrl = props.["url", ""]?href
    
    let name = props.["name", ""] |> innerText |> String.removeNewlines
    let description =  props.TryFind("description", "") |> Option.mapString innerText
    
    let address = props.["streetAddress", ""] |> innerText
    let latLong = LatLong.parse props.["latitude", ""]?content props.["longitude", ""]?content
        
    let price = 
        li.Descendants("a")
        |> Seq.find (hasClass "listing-results-price text-price")
        |> innerText
        |> (fun str -> pcmMatcher.Match(str).PCM.Value)
        |> Decimal.TryParse
        |> Option.fromTryParse
        |> Option.get 0M
        
    let mainPhotoSmall = props.["photo", ""]?src
        
    let nearbyLookup = 
        [ "nearby_stations_schools_london_underground_station", "Tube"
          "nearby_stations_schools_london_dlr_station", "DLR"
          "nearby_stations_schools_london_overground_station", "Overground"
          "nearby_stations_schools_national_rail_station", "Train"
          "nearby_stations_schools_uk_primary_school", "Primary School"
          "nearby_stations_schools_uk_secondary_school", "Secondary School"
          "nearby_stations_schools_uk_airport", "Airport"
          "nearby_stations_schools_uk_ferry_port", "Ferry"
          "nearby_stations_schools_uk_primary_and_secondary_school", "Primary and Secondary School" ]

    let getNearbyItem li =
        (li |> innerText |> String.removeNewlines,
         li.Elements("span"))
        ||> Seq.fold (fun text span -> 
            nearbyLookup
            |> List.tryPick (fun (key, value) -> if hasClass ("interface " + key) span
                                                 then Some (" [" + value + "]") 
                                                 else None)
            |> Option.map ((+) text)
            |> Option.get text)

    let nearby =
        li.Descendants("div")
        |> Seq.tryFind (hasClass "nearby_stations_schools clearfix")
        |> Option.mapList (descendants "li" >> Seq.map getNearbyItem >> Seq.toList)
    
    let phone = 
        props.TryFind("telephone", "")
        |> Option.mapString innerText
    
    let addedOn =
        li.Descendants("strong")
        |> Seq.find (hasClass "listing_sort_copy")
        |> innerText
        |> String.removeNewlines
        |> String.remove "Added on "
    
    { Url = baseUrl + propertyUrl
      Name = name
      Description = description
      Address = address
      LatLong = latLong
      Price = price
      Photos = [ mainPhotoSmall ]
      Links = []
      Features = []
      Nearby = nearby
      Phone = phone
      AddedOn = addedOn }

#if INTERACTIVE
let li = 
    getFirstListingPage (Some 1000M) (Some 1600M) (Some 1) (Some 3) 
    |> Http.RequestString 
    |> HtmlDocument.Parse
    |> getListingItems 
    |> Seq.head

let property = parseListingItem li

let doc = 
    property.Url 
    |> Http.RequestString 
    |> HtmlDocument.Parse
#endif

let parsePropertyPage (doc:HtmlDocument) (property:Property) = 

    let props = getMicrodataProperties doc.DocumentNode

    let name = props.["name", ""] |> innerText |> String.removeNewlines
    let description = props.TryFind("description", "") |> Option.mapString innerText

    let address = props.["streetAddress", ""] |> innerText

    let photos = 
        doc.DocumentNode.Descendants("a")
        |> Seq.filter (hasClass "images-thumb")
        |> Seq.map (attr "data-photo")
        |> Seq.toList

    let featureItems = 
        doc.DocumentNode.Descendants("h3")
        |> Seq.where (hasText "Property info" >>||>> hasText "Property features")
        |> Seq.collect (followingSibling "ul" >> elements "li")

    let featureText = ResizeArray<_>()
    let featurePhotos = ResizeArray<_>()
    let featureLinks = ResizeArray<_>()

    for feature in featureItems do
        let a = element "a" feature
        if a = null then
            featureText.Add (innerText feature)
        else
            let href = a?href
            if href.EndsWith(".jpg") || href.EndsWith("png") then
                featurePhotos.Add href
            else
                featureLinks.Add (innerText feature, href)

    { property with
        Name = name
        Description = description
        Address = address
        Photos = photos @ (Seq.toList featurePhotos)
        Links = Seq.toList featureLinks
        Features = Seq.toList featureText }

#if INTERACTIVE
parsePropertyPage doc property
#endif

type T() = 

    interface IPropertySite with

        member x.GetFirstListingPages(minPrice, maxPrice, minBeds, maxBeds) =
            getFirstListingPages minPrice maxPrice minBeds maxBeds

        member x.ParseListingPage(doc) = 
            let properties = 
                doc
                |> getListingItems
                |> Seq.choose (Option.fromTry "listingItem" parseListingItem)
            let nextPageUrl = 
                getNextListingPage doc
            properties, nextPageUrl

        member x.ParsePropertyPage doc property =
            parsePropertyPage doc property
