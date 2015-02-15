# NOTE this differs from proc_decl.cp only in the last line: i'm testing the use
#      of indentation for backeting expressions.

proc P : (integer/integer c)
  local x : boolean := True
  local y : boolean := False
  local w := True
  local z := False
  global x1 : boolean := True
  global y1 : boolean := False
  global w1 := True
  global z1 := False
  <>
  except Bla : <>
  except Bla2 :
    <>

