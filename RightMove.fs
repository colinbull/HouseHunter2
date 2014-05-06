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
module RightMove
#endif

open System
open System.Web
open FSharp.Data
open FSharp.RegexProvider
open HtmlAgilityPack
open HouseHunter

let baseUrl = "http://www.rightmove.co.uk"
    
let adjustToRange possiblePrices minPrice maxPrice =
    possiblePrices |> Seq.skipWhile (fun x -> x < minPrice) |> Seq.takeWhile (fun x -> x <= maxPrice) |> Seq.pairwise
    
let getFirstListingPages minPrice maxPrice minBeds maxBeds = [
    let possiblePrices = 
        [100..50..500] @ [600..100..1200] @ [1250] @ [1300..100..1500] @ [1750..250..3000] @ [3500..500..7000] @ [8000..1000..10000] @ [12500..2500..20000] @ [25000..5000..40000]
        |> List.map decimal
    for minPrice, maxPrice in adjustToRange possiblePrices minPrice maxPrice do
        let minPrice = minPrice |> sprintf "&minPrice=%M"
        let maxPrice = maxPrice |> sprintf "&maxPrice=%M"
        for beds in minBeds..maxBeds do
            let minBeds = beds |> sprintf "&minBedrooms=%d"
            let maxBeds = beds |> sprintf "&maxBedrooms=%d"
            yield baseUrl
                  + "/property-to-rent/find.html?locationIdentifier=REGION%5E87490&retirement=false&sortType=6&numberOfPropertiesPerPage=50"
                  + minPrice 
                  + maxPrice 
                  + minBeds
                  + maxBeds ]

let getListingItems (doc:HtmlDocument) =
    doc.DocumentNode.Descendants("li")
    |> Seq.filter (hasAttr "name" "summary-list-item")
    
let getNextListingPage (doc:HtmlDocument) =
    doc.DocumentNode.Descendants("div")
    |> Seq.filter (hasId "pagenavigation")
    |> Seq.collect (elements "a")
    |> Seq.tryFind (innerText >> (=) "next")
    |> Option.map (attr "href" >> HttpUtility.HtmlDecode >> ((+) baseUrl))
    
let pcmMatcher = new Regex<"(?<PCM>\d{1,3}(?:[,]\d{3})*)\s*pcm">()

let parseListingItem (li:HtmlNode) = 
    
    let price = 
        li.Descendants("p")
        |> Seq.find (hasClass "price-new")
        |> innerText
        |> (fun str -> pcmMatcher.Match(str).PCM.Value)
        |> Decimal.TryParse
        |> Option.fromTryParse
        |> Option.get 0M

    let photo = 
        li.Descendants("img")
        |> Seq.find (hasClass "largephoto" >>||>> hasClass "largephoto single-image" >>||>> hasClass "fixedPic")
            
    let mainPhotoSmall = photo?src
        
    let propertyUrl = photo.ParentNode?href
    
    let addressSpan =
        li.Descendants("span")
        |> Seq.find (hasClass "displayaddress")

    let name = 
        addressSpan 
        |> precedingSibling "span"
        |> innerText

    let address =
        addressSpan
        |> innerText

    let description =
        li.Descendants("p")
        |> Seq.find (hasClass "description")
        |> innerText
        |> String.remove "More details &rsaquo;"
        |> String.trim

    { Url = baseUrl + propertyUrl
      Name = name
      Description = description
      Address = address
      LatLong = LatLong.Default
      Price = price
      Photos = [ mainPhotoSmall ]
      Links = []
      Features = []
      Nearby = []
      Phone = ""
      AddedOn = "" }

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

    let description =
        doc.DocumentNode.Descendants("div")
        |> Seq.where (hasClass "propertyDetailDescription")
        |> Seq.map innerText
        |> Seq.reduce (+)
    
    let addedOn = 
        doc.DocumentNode.Descendants("strong")
        |> Seq.tryFind (hasText "First Listed on Rightmove:")
        |> Option.mapString (fun x -> x.NextSibling |> innerText)

    let phone = 
        doc.DocumentNode.Descendants("span")
        |> Seq.find (hasClass "number")
        |> innerText

    let photos = 
        doc.DocumentNode.Descendants("img")
        |> Seq.where (hasClass "imageinnernojs")
        |> Seq.map (attr "src")
        |> Seq.toList

    let features = 
        doc.DocumentNode.Descendants("ul")
        |> Seq.tryFind (hasClass "keyfeatures")
        |> Option.mapList (elements "li" >> Seq.map innerText >> Seq.toList)

    let getLink a =
        innerText a |> String.trim |> String.remove "__",
        baseUrl + a?href

    let links = 
        doc.DocumentNode.Descendants("div")
        |> Seq.tryFind (hasId "agentloadedmedia")
        |> Option.mapList (descendants "a" >> Seq.map getLink >> Seq.toList)

    let epc =
        doc.DocumentNode.Descendants("img")
        |> Seq.tryFind (hasClass "epcgraph thumbnail")
        |> Option.map (attr "src" >> String.remove "_max_135x100")

    let photos = 
        match epc with
        | Some epc -> photos @ [ epc ]
        | None -> photos

    let getLocationFromGoogleMapsUrl (url:string) = 
        let url = url.Substring(url.IndexOf("center=") + "center=".Length)
        let coords = url.Substring(0, url.IndexOf("&")).Split ','
        LatLong.parse coords.[0] coords.[1]

    let latLong = 
        doc.DocumentNode.Descendants("a")
        |> Seq.find (hasId "minimapwrapper")
        |> element "img"
        |> attr "src"
        |> getLocationFromGoogleMapsUrl

    let nearbyLookup = 
        [ "London Underground logo", "Tube"
          "Light Railway Station logo", "DLR"
          "London Overground logo", "Overground"
          "National Train Station logo", "Train" ]

    let getNearbyItem div =
        (div |> innerText |> String.removeNewlines,
         div.Elements("img"))
        ||> Seq.fold (fun text img -> 
            nearbyLookup
            |> List.tryPick (fun (key, value) -> if img?alt = key
                                                 then Some (" [" + value + "]") 
                                                 else None)
            |> Option.map ((+) text)
            |> Option.get text)

    let nearby =
        doc.DocumentNode.Descendants("div")
        |> Seq.where (hasClass "stations")
        |> Seq.map (getNearbyItem)
        |> Seq.distinct
        |> Seq.toList

    { property with
        Description = description
        LatLong = latLong
        Photos = photos
        Links = links
        Features = features
        Nearby = nearby
        Phone = phone
        AddedOn = addedOn }

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
                |> Seq.where (descendants "span" >> Seq.exists (hasClass "propertyUnavailable") >> not)
                |> Seq.choose (Option.fromTry "listingItem" parseListingItem)
            let nextPageUrl = 
                getNextListingPage doc
            properties, nextPageUrl

        member x.ParsePropertyPage doc property =
            parsePropertyPage doc property
