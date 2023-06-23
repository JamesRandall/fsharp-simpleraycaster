module App.Game

open App.Model
open Browser.Types
open Fable.Core.JsInterop
open App.Render
open Browser


let sampleLevel = [                            //
  [ 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 2 ; 2 ; 2 ; 2 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 2 ; 0 ; 0 ; 2 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 2 ; 0 ; 0 ; 2 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 2 ; 0 ; 0 ; 2 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ] // 6
  [ 1 ; 0 ; 2 ; 2 ; 2 ; 2 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 3 ; 3 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 3 ; 3 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 1 ]
  [ 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ]
]

let private initialGameState =
  { Map = sampleLevel |> Map.mapFromColorIndexes
    Player = { Score = 0<points> ; Health = 100<hp> }
    Camera = {
      Position = { vX = 12. ; vY = 6. }
      Direction = { vX = -1. ; vY = 0. }
      Plane = { vX = 0. ; vY = 0.666 }
    }
    ControlState = ControlState.None
  }

let private updateFrame game frameTime =
  let (|IsActive|_|) controlState game = if game.ControlState &&& controlState > ControlState.None then Some () else None 
  let frameMultiplier = float frameTime / 1000. 
  let movementSpeed = 5.0 * frameMultiplier // squares per second
  let rotationSpeed = 3.0 * frameMultiplier // radians per second
  let posX = game.Camera.Position.vX
  let posY = game.Camera.Position.vY
  let dirX = game.Camera.Direction.vX
  let dirY = game.Camera.Direction.vY
  let move speed inputGame =
    let newCameraPosition =
      { inputGame.Camera.Position with
          vX = if game.Map.[int posY].[int (posX + dirX * speed)] = Cell.Empty then posX + (dirX * speed) else posX 
          vY = if game.Map.[int (posY + dirY * speed)].[int posX] = Cell.Empty then posY + (dirY * speed) else posY
      }
    { inputGame with Camera = { inputGame.Camera with Position = newCameraPosition } }
  let strafe speed inputGame =
    let strafeDirection = game.Camera.Direction.CrossProduct
    let strafeX = strafeDirection.vX
    let strafeY = strafeDirection.vY
    let newCameraPosition =
      { inputGame.Camera.Position with
          vX = if game.Map.[int posY].[int (posX + strafeX * speed)] = Cell.Empty then posX + (strafeX * speed) else posX 
          vY = if game.Map.[int (posY + strafeY * speed)].[int posX] = Cell.Empty then posY + (strafeY * speed) else posY
      }
    { inputGame with Camera = { inputGame.Camera with Position = newCameraPosition } }
  let rotate inputGame =
    let rotationMultiplier =
      match inputGame with
      | IsActive ControlState.TurningRight -> -1.
      | IsActive ControlState.TurningLeft -> 1.
      | _ -> 0.
    let planeX = game.Camera.Plane.vX
    let planeY = game.Camera.Plane.vY
    let newDirX = dirX * cos(rotationMultiplier * rotationSpeed) - dirY * sin(rotationMultiplier * rotationSpeed)
    let newDirY = dirX * sin(rotationMultiplier * rotationSpeed) + dirY * cos(rotationMultiplier * rotationSpeed)
    let newPlaneX = planeX * cos(rotationMultiplier * rotationSpeed) - planeY * sin(rotationMultiplier * rotationSpeed)
    let newPlaneY = planeX * sin(rotationMultiplier * rotationSpeed) + planeY * cos(rotationMultiplier * rotationSpeed)
    { inputGame with
        Camera =
          { inputGame.Camera with
              Direction = { vX = newDirX  ; vY = newDirY}
              Plane = { vX = newPlaneX ;  vY = newPlaneY }
          }
    }

  game
  |> (fun g -> match g with | IsActive ControlState.Forward -> move movementSpeed g | _ -> g)
  |> (fun g -> match g with | IsActive ControlState.Backward -> move (-movementSpeed/2.) g | _ -> g)
  |> (fun g -> match g with | IsActive ControlState.StrafingLeft -> strafe -movementSpeed g | _ -> g)
  |> (fun g -> match g with | IsActive ControlState.StrafingRight -> strafe movementSpeed g | _ -> g)
  |> (fun g -> match g with | IsActive ControlState.TurningLeft | IsActive ControlState.TurningRight -> rotate g | _ -> g)
   
  
  
let private renderScene (context:CanvasRenderingContext2D) game =
  context.save ()
  context?imageSmoothingEnabled <- false
  context.lineCap <- "square"
  context.translate(0.5,0.5)
  context.font <- "30px Consolas, Menlo, monospace"
  
  clearCanvas context
  
  let width = context.canvas.width
  let height = context.canvas.height
    
  let posX = game.Camera.Position.vX
  let posY = game.Camera.Position.vY
  {(0.)..(width-1.)}
  |> Seq.iter(fun screenX ->
    // gives us the x position in a range of -1 to 1
    let cameraX = (2. * screenX / width) - 1.
    // calculate the direction of the ray as a vector
    let rayDirection = {
      vX = game.Camera.Direction.vX + game.Camera.Plane.vX * cameraX
      vY = game.Camera.Direction.vY + game.Camera.Plane.vY * cameraX
    }
    let mapX = int posX
    let mapY = int posY
    // setup our starting values for DDA
    let deltaDistX = if rayDirection.vX = 0. then System.Double.MaxValue else abs (1. / rayDirection.vX)
    let deltaDistY = if rayDirection.vY = 0. then System.Double.MaxValue else abs (1. / rayDirection.vY)
    let stepX, initialSideDistX =
      if rayDirection.vX < 0. then
        -1,(posX - float mapX) * deltaDistX
      else
        1,(float mapX + 1.0 - posX) * deltaDistX
    let stepY, initialSideDistY =
      if rayDirection.vY < 0. then
        -1,(posY - float mapY)*deltaDistY
      else
        1,(float mapY + 1.0 - posY)*deltaDistY
    // perform DDA
    // as we go we track which side of the wall we are hitting so we can show an alternating colour
    // (or later a specific texture)
    let _, sideDistX, sideDistY, hitMapX, hitMapY, side =
      Seq.initInfinite (fun _ -> 0)
      |> Seq.scan(fun (isHit, castSideDistX, castSideDistY, castMapX, castMapY, castSide) _ ->
        if isHit then
          // bail out of the loop on a hit
          (isHit, castSideDistX, castSideDistY, castMapX, castMapY, castSide)
        else
          // otherwise move to the next grid square
          let newSideDistX, newSideDistY, newMapX, newMapY, newSide =
            if castSideDistX < castSideDistY then
              castSideDistX + deltaDistX, castSideDistY, castMapX + stepX, castMapY, 0
            else
              castSideDistX, castSideDistY + deltaDistY, castMapX, castMapY + stepY, 1
          // if we hit a grid square that isn't empty then we're done - we've hit a wall (or something solid!)
          let newIsHit = game.Map.[newMapY].[newMapX] <> Cell.Empty
          newIsHit, newSideDistX, newSideDistY, newMapX, newMapY, newSide
      ) (false, initialSideDistX, initialSideDistY, mapX, mapY, 0)
      |> Seq.skipWhile (fun (isHit, _, _, _, _, _) -> not isHit)
      |> Seq.head
    // now we need to draw the vertical line in the column - the height of the line is based on the
    // perpendicular distance to the wall
    let perpendicularWallDistance = if side = 0 then (sideDistX - deltaDistX) else (sideDistY - deltaDistY)
    let lineHeight = height / perpendicularWallDistance
    let startY = max (-lineHeight/2. + height/2.) 0.
    let endY = min (lineHeight/2. + height/2.) (height-1.)
    
    // just handling color maps at the moment
    match game.Map.[hitMapY].[hitMapX] with
    | Cell.WithColor color ->
      verticalLine context (if side = 1 then adjustColor color -30 else color) screenX startY endY
    | _ -> ()
  )
  context.restore ()

let init (canvas:HTMLCanvasElement) =
  let context = canvas.getContext_2d()
  let gameLoop (game:Game) (frameTime:float<ms>) =
    renderScene context game
    let updatedGameState = updateFrame game frameTime
    updatedGameState
    
  let updateControlState game controlState =
    { game with ControlState = game.ControlState ^^^ controlState }

  gameLoop,updateControlState,initialGameState
  