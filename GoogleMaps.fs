namespace HouseHunter

open System
open FSharp.Data

module private DateHelpers =

    let bankHolidays = 
        [1,1,14; 18,4,14; 21,4,14; 5,5,14; 26,5,14; 25,8,14; 25,12,14; 26,12,14
         1,1,15;  3,4,15;  6,4,15; 4,5,15; 25,5,15; 31,8,15; 25,12,15; 28,12,15]
        |> Seq.map (fun (day, month, year) -> DateTime(2000 + year, month, day, 9, 0, 0))
        |> Set.ofSeq

    let nextWorkDayAt9am = 
        let mutable nextWorkDay = DateTime.Now.AddDays(1.)
        nextWorkDay <- new DateTime(nextWorkDay.Year, nextWorkDay.Month, nextWorkDay.Day, 9, 0, 0)
        while nextWorkDay.DayOfWeek = DayOfWeek.Saturday || nextWorkDay.DayOfWeek = DayOfWeek.Sunday || bankHolidays.Contains nextWorkDay do
            nextWorkDay <- nextWorkDay.AddDays(1.)
        nextWorkDay

type GoogleMapsQuery =     
    | GoogleMapsQuery of center:LatLong
    | GoogleMapsDirections of source:LatLong * destination:LatLong * date:DateTime
    | GoogleMapsDirectionsAt9amNextWorkDay of source:LatLong * destination:LatLong
    
    member x.Url =
        let baseUrl = "http://maps.google.co.uk/"
        match x with
        | GoogleMapsQuery center -> sprintf "%s?q=%O" baseUrl center
        | GoogleMapsDirectionsAt9amNextWorkDay(source, destination) -> GoogleMapsDirections(source, destination, DateHelpers.nextWorkDayAt9am).Url
        | GoogleMapsDirections(source, destination, date) -> 
            sprintf "%s?dirflg=r&ttype=arr&time=%s&date=%s&saddr=%O&daddr=%O" 
                    baseUrl 
                    (date.ToString("h:mmtt"))
                    (date.ToString("dd/MM/yy"))
                    source
                    destination

    static member GetCommuteDuration homeLocation workLocation = async {
        let url = GoogleMapsDirectionsAt9amNextWorkDay(homeLocation, workLocation).Url + "&output=dragdir"
        let! str = Http.AsyncRequestString url
        let pos = str.IndexOf("tooltipHtml:\"") + "tooltipHtml:\"".Length
        let str = str.Substring(pos)
        let pos = str.IndexOf("\"")
        let str = str.Substring(0, pos)
        let parts = str.Trim().Replace("(", null).Replace(")", null).Replace("min", null).Replace("hour", null).Replace("sec", null).Replace("s", null).Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        if parts.Length = 1 then
            return Int32.Parse parts.[0]
        elif parts.Length = 2 then
            let hours = Int32.Parse parts.[0]
            let minutes = Int32.Parse parts.[1]
            return hours * 60 + minutes
        else
            failwithf "Unexpected result: %s" str
            return Unchecked.defaultof<_>
    }
