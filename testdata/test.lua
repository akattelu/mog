

local int = 42
local float = 3.14
local negative = -99

local str = "hello world"
local empty_str = ""

local bool_true = true
local bool_false = false

local nothing = nil


global_var = 100

local x = 10

local a, b, c = 1

local result = math.sqrt(16)


local neg = -x

local inverted = not true

local len = #"test"

local bitnot = ~255


local sum = 1 + 2
local diff = 10 - 5
local prod = 3 * 4
local quot = 20 / 4
local floor_div = 21 // 4
local mod = 10 % 3
local pow = 2 ^ 8

local band = 15 & 7
local bor = 8 | 4
local bxor = 12 ~ 3
local lshift = 1 << 4
local rshift = 32 >> 2

local eq = 5 == 5
local neq = 5 ~= 6
local lt = 3 < 5
local lte = 3 <= 3
local gt = 7 > 2
local gte = 7 >= 7

local land = true and false
local lor = true or false

local concat = "hello" .. " " .. "world"


if x > 0 then
  print("positive")
end

if x < 0 then
  print("negative")
else
  print("non-negative")
end


do
  local scoped = 123
  print(scoped)
end

while x > 0 do
  x = x - 1
end

repeat
  x = x + 1
until x >= 10

for i = 1, 10 do
  print(i)
end

for i = 1, 10, 2 do
  print(i)
end

for k, v in pairs(table) do
  print(k, v)
end

for line in io.lines(), io.read() do
  print(line)
end

for i = 1, 100 do
  if i == 50 then
    break
  end
end


function add(x, y)
  return x + y
end

local function subtract(x, y)
  return x - y
end

function vararg_func(...)
  return ...
end

function mixed_params(a, b, ...)
  local sum = a + b
  return sum
end

function no_params()
  print("hello")
end

local anon = function(x)
  return x * 2
end


function with_return()
  return 42
end

function bare_return()
  return;
end


local empty = {}

local array = {1, 2, 3, 4, 5}

local record = {x = 10, y = 20, z = 30}

local computed = {[key] = value, ["dynamic"] = 123}

local mixed = {
  1,
  2,
  x = 10,
  [expr] = val,
  3
}


local elem = array[1]
local nested = matrix[i][j]

local field = obj.x
local chain = foo.bar.baz

local combo = obj.field[index].nested


print("hello")
math.max(1, 2, 3)

noargs()

config {
  debug = true,
  level = 5
}

require "module"


obj:method(arg1, arg2)

obj:configure {
  option = "value"
}

str:match "pattern"

obj:first():second():third()


print("side effect")

obj:update()


local complex = (1 + 2) * 3 - 4 / 2

local nested_call = outer(inner(value))

if (x > 0 and y > 0) or z == 0 then
  print("complex condition")
end

local vtable = {
  add = function(a, b) return a + b end,
  sub = function(a, b) return a - b end
}

function make_adder(x)
  return function(y)
    return x + y
  end
end


function single(x)
  return x
end

function multiple(x, y, z)
  return x
end

if false then end
if true then else end

while false do
end

do
end

function outer()
  local function inner()
    if true then
      for i = 1, 10 do
        while i < 5 do
          repeat
            local x = i
          until true
        end
      end
    end
  end
end
