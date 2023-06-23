module App.Model

[<Measure>] type hp
[<Measure>] type points
[<Measure>] type ms

[<RequireQualifiedAccess>]
type Cell =
  | WithColor of hexColor:int
  | WithTexture of textureIndex:int
  | Empty
  
type Vector2D =
  { vX: float
    vY: float
  }
  member x.CrossProduct = { vX = x.vY ; vY = -x.vX }
  
type ControlState =
  | None          = 0b000000
  | Forward       = 0b000001
  | TurningLeft   = 0b000010
  | TurningRight  = 0b000100
  | StrafingLeft  = 0b001000
  | StrafingRight = 0b010000
  | Backward      = 0b100000

type Player =
  { Score: int<points>
    Health: int<hp>
  }

type Camera =
  { Position: Vector2D
    Direction: Vector2D
    Plane: Vector2D
  }

type Game =
  { Map: Cell list list
    Player: Player
    Camera: Camera
    ControlState: ControlState
  }
  