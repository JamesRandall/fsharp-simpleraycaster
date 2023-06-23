module App.Map

open App.Model

let mapFromColorIndexes source =
  let colours =
    [ 0xDC2626 // red
      0x16A34A // green
      0x2563EB // blue
      0xCA8A04 // yellow
      0x4B5563 // gray      
    ]
    
  source
  |> List.map(fun row ->
    row |> List.map(fun colourIndex ->
      if colourIndex > 0 then
        Cell.WithColor colours.[colourIndex-1]
      else
        Cell.Empty
    )  
  )

