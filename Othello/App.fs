open Suave
open Suave.Successful
open Suave.Filters
open Suave.Operators
open Suave.RequestErrors
open Suave.Http
open Suave.Web
open Newtonsoft.Json

let mutable turn = 1;
let mutable winner = 0;
let mutable finish = false;
//black = 2 white = 1

let table = 
    [|
        [|0;0;0;0;0;0;0;0|]
        [|0;0;0;0;0;0;0;0|]
        [|0;0;0;0;0;0;0;0|]
        [|0;0;0;1;2;0;0;0|]
        [|0;0;0;2;1;0;0;0|]
        [|0;0;0;0;0;0;0;0|]
        [|0;0;0;0;0;0;0;0|]
        [|0;0;0;0;0;0;0;0|]  
    |];

let turnGame id = 
    request (fun tg ->
        if id = turn then
            OK (sprintf "Your Turn")
        else
            OK (sprintf "Not Your Turn")
    )
type GameClass = { Turn:int; Game:int[][]; Finish:bool; Winner:int}

let getGame table turn fini gagnant = 
    JsonConvert.SerializeObject({Turn=turn; Game=table; Finish=fini; Winner=gagnant})

let Retourner ligne colonne sensL sensC color =
    let mutable stop = false
    let mutable continuer = true
    let mutable l_tmp = ligne + sensL
    let mutable c_tmp = colonne + sensC
    
    while (l_tmp > 0) && (c_tmp > 0) && (l_tmp < 8) && (c_tmp < 8) && continuer = true do
        if table.[l_tmp].[c_tmp] = color then
            stop <- true
            continuer <- false
        elif table.[l_tmp].[c_tmp] = 0 then
            continuer <- false
        
        c_tmp <- c_tmp + sensC    
        l_tmp <- l_tmp + sensL

        if stop = true then
            c_tmp <- c_tmp - sensC;
            l_tmp <- l_tmp - sensL;
            while c_tmp <> colonne || l_tmp <> ligne do
                table.[l_tmp].SetValue(color, c_tmp)
                c_tmp <- c_tmp - sensC
                l_tmp <- l_tmp - sensL

let isFinish:bool =
    let mutable fin = true
    for i in 0 .. table.Length - 1 do
        for j in 0 .. table.[i].Length - 1 do
            if table.[i].[j] = 0 then
                fin <- false
    
    if fin then 
        finish <- true

    fin

let getWinner = 
    let mutable j1 = 0
    let mutable j2 = 0
    for i in 0 .. table.Length - 1 do
        for j in 0 .. table.[i].Length - 1 do
            if table.[i].[j] = 1 then 
                j1 <- j1 + 1
            elif table.[i].[j] = 2 then
                j2 <- j2 + 1
    if j1 > j2 then
        winner <- 1
    elif j2 > j1 then
        winner <- 2
    elif j2 = j1 && isFinish = true then
        winner <- 3
    else
        winner <- 0

        

let GameIt color ligne colonne = 
    request (fun tg -> 

        if isFinish = true then
            getWinner
        elif colonne >= 0 && colonne < 8 && ligne >= 0 && ligne < 8 && table.[ligne].[colonne] = 0 then
            table.[ligne].SetValue(color, colonne);
            Retourner ligne colonne 1   0 color
            Retourner ligne colonne -1  0 color
            Retourner ligne colonne 0   1 color
            Retourner ligne colonne 0  -1 color
            Retourner ligne colonne -1  1 color
            Retourner ligne colonne -1 -1 color
            Retourner ligne colonne 1   1 color
            Retourner ligne colonne 1  -1 color

            if turn = 1 then
                turn <- 2
            else
                turn <- 1
            
        OK(sprintf "%s" (getGame table turn finish winner))
    )
    

let browse =
    request (fun r ->
        match r.queryParam "genre" with
        | Choice1Of2 genre -> OK (sprintf "Genre: %s" genre)
        | Choice2Of2 msg -> BAD_REQUEST msg)

let webPart = 
    choose [
        path "/" >=> (OK "Home")
        path "/get" >=> (OK (sprintf "%s" (getGame table turn finish winner)))
        pathScan "/put/%d/%d/%d"  (fun (color, ligne, col) -> GameIt color ligne col)
    ]
startWebServer defaultConfig webPart

   // pathScan "/store/details/%s/%d" (fun (a, id) -> OK (sprintf "Artist: %s; Id: %d" a id))