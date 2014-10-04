open System.Windows.Forms
open System.Drawing

let rand = System.Random()

let width, height = 700,700      
let form = new Form(Width = width, Height = height)
let box = new PictureBox(BackColor = Color.Transparent, Dock = DockStyle.Fill)
form.Show()

type EdgeType = Top | Bottom | Left | Right

type Box(row:int,col:int) =
    member x.Row = row
    member x.Column = col

type Player = {
    Color: Color
}
with member p.Brush = new SolidBrush(p.Color)

let p1 = {Player.Color = Color.LightBlue}
let p2 = {Player.Color = Color.Pink}

let drawEdge (g:Graphics) (pen: Pen) edgeType i j =
    let startDotI,startDotJ,endDotI,endDotJ =
        match edgeType with
        | Top -> i,j,i,j+1
        | Bottom -> i+1,j,i+1,j+1
        | Left -> i,j,i+1,j
        | Right -> i,j+1,i+1,j+1
    g.DrawLine(pen,Point(50 + 50*startDotJ,50 + 50*startDotI),Point(50 + 50*endDotJ, 50 + 50*endDotI))

let fillBox (g:Graphics) (player:Player) (box:Box) =
    g.FillRectangle(player.Brush,50 + 50*box.Column, 50 + 50*box.Row, 50, 50)

let b1 = Box(4,4)

type Move = {Player:Player; Box:Box; EdgeType:EdgeType}

type GameState = {
    Size : int
    Horizontals : bool [] []
    Verticals : bool [] []
    BoxOwners : Player option [] []
}
with
    static member Fresh(size:int) =
        let vs = [|for _ in [1..size-1] do
                    yield [| for _ in [1..size] do yield false|]|]
        let hs = [|for _ in [1..size] do
                    yield [| for _ in [1..size-1] do yield false|]|]
        let bo = [|for _ in [1..size-1] do 
                    yield [|for _ in [1..size-1] do yield None |]|]
        {Horizontals = hs; Verticals = vs; Size = size; BoxOwners = bo}
    member this.tryCapture (player:Player) (box:Box) =

        let i,j = box.Row, box.Column
        printfn "(%d,%d)" i j
        if i < this.Size && j < this.Size then
            if this.Verticals.[i-1].[j-1] && this.Verticals.[i-1].[j]
                    && this.Horizontals.[i-1].[j-1] && this.Horizontals.[i].[j-1] then
                this.BoxOwners.[i-1].[j-1] <- Some player
    member this.ApplyMove (move:Move) =
        let i,j = move.Box.Column, move.Box.Row
        let trySetEdge edgeType i j =
            let edgeArray, i', j' = 
                match edgeType with
                | Left -> this.Verticals, i-1, j-1
                | Right -> this.Verticals, i-1, j
                | Top -> this.Horizontals, i-1, j-1
                | Bottom -> this.Horizontals, i, j-1
            if edgeArray.Length < i' 
                || edgeArray.[i'].Length < j' 
                || edgeArray.[i'].[j'] then failwith "bad move"
            else
                edgeArray.[i'].[j'] <- true
                let adjacents = 
                    match edgeType with
                    | Left    when j > 1           -> [Box(i,j-1)]
                    | Right when j+1 < this.Size -> [Box(i,j+1)]
                    | Top   when i > 1           -> [Box(i-1,j)]
                    | Bottom  when i+1 < this.Size -> [Box(i+1,j)]
                    | _                           -> []
                    @ [Box(i,j)]
                adjacents |> Seq.iter (this.tryCapture move.Player)
                 
        trySetEdge move.EdgeType move.Box.Row move.Box.Column
        this
    static member FromMoves n (moves:Move seq) =
        moves
        |> Seq.map (fun m -> (fun (gs:GameState) -> gs.ApplyMove m))
        |> Seq.reduce (>>)
        <| GameState.Fresh(n)
    member this.Draw() = 
        let image = new Bitmap(width, height)
        image.MakeTransparent()
        let graphics = Graphics.FromImage(image)
        let brush = new SolidBrush(Color.FromArgb(0, 0, 0))
        let pen = new Pen(brush,3.f)

        this.BoxOwners
        |> Seq.iteri (fun i row ->
            row|> Seq.iteri (fun j p ->
                match p with
                | Some player -> fillBox graphics player <| Box(i+1,j+1)
                | None -> ()))

        this.Verticals 
        |> Seq.iteri (fun i row ->
            row |> Seq.iteri (fun j b -> 
                if b then drawEdge graphics pen EdgeType.Left (i+1) (j+1))) 

        this.Horizontals
        |> Seq.iteri (fun i row ->
            row |> Seq.iteri (fun j b -> 
                if b then drawEdge graphics pen EdgeType.Top (i+1) (j+1)))

        for i in [1..this.Size] do
            for j in [1..this.Size] do
                graphics.FillEllipse(brush, Rectangle(Point(45+50 * i,45+50*j), Size(10,10)))

        box.Image <- image
        form.Controls.Add(box) 
        box.BringToFront()
        box.Refresh()

type Ai = GameState -> Move option

let randomAi (gs:GameState) =
    
    let verticalMoves = 
        gs.Verticals 
            |> Array.mapi (fun i col -> 
                col |> Array.mapi (fun j b -> if not b then Some(i,j) else None))
            |> Array.concat
            |> Array.choose (fun o ->
                match o with
                | Some(i,j) when i+1 < gs.Size -> Some {Move.Player = p1; EdgeType = Left; Box=Box(i+1,j+1)}
                | Some(i,j) -> Some {Player=p1; EdgeType=Right; Box=Box(i+1,j)}
                | None -> None
            )

    let horizontalMoves = 
        gs.Horizontals 
        |> Array.mapi (fun i col -> 
            col |> Array.mapi (fun j b -> if not b then Some(i,j) else None))
        |> Array.concat
        |> Array.choose (fun o ->
            match o with
            | Some(i,j) when j+1 < gs.Size -> Some {Move.Player = p1; EdgeType = Top; Box=Box(i+1,j+1)}
            | Some(i,j) -> Some {Player=p1; EdgeType=Bottom; Box=Box(i,j+1)}
            | None -> None
        )

    let allMoves = Array.append verticalMoves horizontalMoves |> List.ofArray

    match allMoves.Length with
    | 0 -> None
    | l -> Some allMoves.[rand.Next(0,l-1)]
//
//[
//    1,1,Right,p1
//    1,1,Top,p1
//    1,1,Left,p1
//    1,1,Bottom,p1
//    //2,2,Left,p1
//    //3,3,Bottom,p2
//    //4,4,Right,p1
//    //8,8,Top,p1
//    //9,9,Top,p2
//    //9,9,Left,p2
//    //9,9,Bottom,p2
//    //9,9,Right,p2
//    8,9,Top,p1
//    8,9,Left,p1
//    9,9,Top,p1
//    8,9,Right,p1
//
//]
//|> Seq.map (fun (i,j,et,p) ->{Move.Box = Box(i,j); Move.EdgeType=et; Move.Player=p})
//|> GameState.FromMoves 10
//|> (fun x -> x.Draw())

let rec evokeAi (ai:Ai) (gs:GameState) =
    gs.Draw()
    match ai gs with
    | None -> ()
    | Some mv -> 
        let gs' = gs.ApplyMove(mv)
        System.Threading.Thread.Sleep(50)
        evokeAi ai gs'

evokeAi randomAi <| GameState.Fresh(10)