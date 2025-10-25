$puts("[TEST] Do-End Block")
do
	local x = 1
	$puts("[PASS] Do block executes with local variable")
end
$puts("[PASS] Control returns after do block")
$puts("")

$puts("[TEST] While Loop - Condition False (Never Executes)")
while (0) do
	$puts("[FAIL] While loop with false condition should not execute")
end
$puts("[PASS] While loop skipped when condition is false")
$puts("")

$puts("[TEST] While Loop - Condition True (Finite via Decrement)")
local y = 10
while (y) do
	$printf("[PASS] While loop on iteration %d/%d\n", y, 10)
	y = y - 1
end
$puts("")

$puts("[TEST] Repeat-Until Loop (Always Executes Once)")
$puts("[NOTE] This test is failing")
repeat
	$puts("[PASS] Repeat loop body executed at least once")

until (1)
$puts("[PASS] Repeat loop exited after condition was true")
$puts("")

$puts("[TEST] Do Block with Multiple Locals and Operations")
do
	local a = 10
	local b = 20
	local c = a + b
	if (c == 30) then
		$puts("[PASS] Multiple locals in do block work correctly")
	else
		$puts("[FAIL] Multiple locals calculation incorrect")
	end
end
$puts("")

$puts("[TEST] Nested Do Blocks")
do
	local outer = 1
	$puts("[PASS] Outer do block")
	do
		local inner = 2
		$puts("[PASS] Inner do block")
	end
	$puts("[PASS] Back in outer do block")
end
$puts("")

$puts("[TEST] Repeat with Immediate Exit")
local counter = 0
repeat
	local counter = counter + 1
	$puts("[PASS] Repeat executed once before checking condition")
until (counter > 0)
$puts("[PASS] Repeat exited correctly")
$puts("")
