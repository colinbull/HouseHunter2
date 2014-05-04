namespace global

open HtmlAgilityPack
open FSharp.RegexProvider

module Option = 
    
    let get defaultValue opt = defaultArg opt defaultValue
    let mapString f = Option.map f >> get ""
    let mapSprintf formatStr = mapString (sprintf formatStr)
    let mapList f = Option.map f >> get []

module String =
    
    let inline trim (s:string) = s.Trim()
    let inline replace (value:string) replacement (str:string) = str.Replace(value, replacement)
    let inline remove toRemove (s:string) =  s |> replace toRemove ""
    let private wsRegex = new Regex<"\s+">()
    let removeNewlines (s:string) = wsRegex.Replace(s |> trim |> replace "\r" " " |> replace "\n"  " ", " ")

module HtmlAgilityPack = 

    type HtmlDocument with 

        static member Parse(html) = 
            let doc = new HtmlDocument()
            doc.LoadHtml(html) 
            doc

    type HtmlNode with 
    
        member x.FollowingSibling name = 
            let sibling = x.NextSibling
            if sibling = null then
                null
            elif sibling.Name = name then
                sibling
            else 
                sibling.FollowingSibling name
    
        member x.FollowingSiblings name = seq {
            let sibling = x.NextSibling
            if sibling <> null then
                if sibling.Name = name then
                    yield sibling
                yield! sibling.FollowingSiblings name
        }

        member x.PrecedingSibling name = 
            let sibling = x.PreviousSibling
            if sibling = null then
                null
            elif sibling.Name = name then
                sibling
            else 
                sibling.PrecedingSibling name
    
        member x.PrecedingSiblings name = seq {
            let sibling = x.PreviousSibling
            if sibling <> null then
                if sibling.Name = name then
                    yield sibling
                yield! sibling.PrecedingSiblings name
        }

    let parent (node:HtmlNode) = 
        node.ParentNode

    let element name (node:HtmlNode) = 
        node.Element name

    let elements name (node:HtmlNode) = 
        node.Elements name

    let descendants name (node:HtmlNode) = 
        node.Descendants name

    let descendantsAndSelf name (node:HtmlNode) = 
        node.DescendantsAndSelf name

    let ancestors name (node:HtmlNode) = 
        node.Ancestors name

    let ancestorsAndSelf name (node:HtmlNode) = 
        node.AncestorsAndSelf name

    let followingSibling name (node:HtmlNode) = 
        node.FollowingSibling name

    let followingSiblings name (node:HtmlNode) = 
        node.FollowingSiblings name

    let precedingSibling name (node:HtmlNode) = 
        node.PrecedingSibling name

    let precedingSiblings name (node:HtmlNode) = 
        node.PrecedingSiblings name

    let hasTagName tagName (node : HtmlNode) = 
        node.Name.ToLowerInvariant() = tagName

    let private trimAndUnescape (text:string) = 
        text
        |> String.replace "&nbsp;" " "
        |> String.replace "&amp;" "&"
        |> String.replace "&quot;" "\""
        |> String.replace "apos;" "'"
        |> String.replace "&lt;" "<"
        |> String.replace "&gt;" ">"
        |> String.trim

    let innerText (node : HtmlNode) = 
        node.Descendants()
        |> Seq.filter (fun n -> not (n.HasChildNodes))
        |> Seq.map (fun n -> if hasTagName "br" n then "\n" else n.InnerText)
        |> String.concat ""
        |> trimAndUnescape

    let inline attr name (node:HtmlNode) = 
        node.GetAttributeValue(name, "")

    let inline (?) (node:HtmlNode) name = 
        attr name node

    let inline hasAttr name value node = 
        attr name node = value

    let inline hasId value node = 
        hasAttr "id" value node

    let inline hasClass value node = 
        hasAttr "class" value node

    let inline hasText value (node:HtmlNode) = 
        innerText node = value

    let getMicrodataProperties (node:HtmlNode) = 
        node.Descendants()
        |> Seq.filter (attr "itemprop" >> (<>) "")
        |> Seq.map (fun x -> (x?itemprop, x?itemtype), x)
        |> Map.ofSeq
    
    let getMetaProperties (node:HtmlNode) = 
        node.Descendants("meta")
        |> Seq.filter (attr "property" >> (<>) "")
        |> Seq.map (fun x -> x?property, x?content)
        |> Map.ofSeq
    