#Check that
# -  we cannot have more than one candidate resolution for each name
# -  forbid all kinds of shadowing -- even across namespaces

type bla : record
  x : string

#type bla2 : record
#  x : integer

type abl : record
  abc : integer
  a' : <integer * string * boolean>

fun F : (a : <integer>, b : type bla, c : type abl) -> ()
#  let x = "test" # this is shadowed below!
#  let x = 4  # This doesn't interfere with constant x -- different namespace
  a.1 # This doesn't interfere with fieldname (constant) a -- different namespace
  b.x = "is this an int?"
#  a.3 # this results in a type error
#  x + 3 # Since "x" is taken to be bla's field, and isn't be a bound variable
         # (since it cannot be both!) then compiler complains that "x" binding
         # is missing. Then when you add a binding for "x", it tells you that
         # "x" already has meaning in the symbol table.
  c.abc
  c.a'.3
  (c.a').3
#  [] typed integer # this should result in a type error
  [] typed [integer]
  [] typed [[integer]]
  [] typed [integer] typed [integer]
#  [] typed [integer] typed [integer] typed [[integer]] # this cannot be accepted
  [] typed [integer] typed [integer] typed [integer]
#  <> typed integer # this should result in a type error
  @: show_symbol_table :@ typed <>
  @:
    show_symbol_table
    show_symbol_table
  :@ typed <>
#  @:
#    print "Here"
#    show symbol_table
#  :@ typed <>

#  This doesn't work -- the indentation in this block fails the parser:
#  @:show_symbol_table
#  show_symbol_table:@ typed <>
  <>

#  The example below is illegal: labels need to be literals.
#  c.(d.4)
