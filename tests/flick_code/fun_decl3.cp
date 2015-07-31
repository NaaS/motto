# like fun_decl2 but also includes state and exceptions

# Function with both channels and parameters
fun F : ([[integer]/[integer]] c, boolean/boolean b; x : integer, y : [boolean]) -> (boolean)
  local x : boolean := True
  local y : boolean := False
  local w := True
  local z := False
  global x1 : boolean := True
  global y1 : boolean := False
  global w1 := True
  global z1 := False
  True
  except Bla : <>

# Function without channels
fun F : (x : integer, y : [boolean]) -> (boolean)
  local x : boolean := True
  local y : boolean := False
  local w := True
  local z := False
  global x1 : boolean := True
  global y1 : boolean := False
  global w1 := True
  global z1 := False
  True
  except Bla : <>

#Function without parameters
fun F : ([[integer]/[integer]] c, boolean/boolean b) -> (boolean)
  local x : boolean := True
  local y : boolean := False
  local w := True
  local z := False
  global x1 : boolean := True
  global y1 : boolean := False
  global w1 := True
  global z1 := False
  True
  except Bla : <>
