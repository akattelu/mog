local x = 42
local y = 100

$printf("x hex: %lx, x int: %d,  y hex: %lx, y int: %d \n", x, x, y, y)

local z = true
local f = false
$printf("z hex %lx, z true boolean value is %d, f hex %lx, f false boolean value is %d\n", z, z, f, f)

if (z) then
  $puts("[PASS] unboxed true boolean value from double d")
else
  $puts("[FAIL] unboxed false boolean value from double d")
end

if (f) then
  $puts("[FAIL] unboxed true boolean value from double d")
else
  $puts("[PASS] unboxed false boolean value from double d")
end
