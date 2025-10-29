
local x = true
local y = false

$puts("[TEST] If/Then/Else/End True Case")
if (x) then
	$puts("[PASS] Then block evaluates when condition is truthy")
else
	$puts("[FAIL] Else block evaluated when condition was truthy")
end
$puts("[PASS] Returns control to end block once")
$puts("")

$puts("[TEST] If/Then/Else/End False Case")
if (y) then
	$puts("[FAIL] Then block evaluated when condition was falsy")
else
	$puts("[PASS] Else block evaluated when condition was falsy")
end
$puts("[PASS] Returns control to end block twice")
$puts("")

$puts("[TEST] If/Then/End")
if (x) then
	$puts("[PASS] Then block evaluates when condition is truthy")
end
$puts("[PASS] Returns control to end block thrice")
$puts("")

$puts("[TEST] If/Then/End Nested")
if (x) then
	if (y == 0) then
		$puts("[PASS] Then block evaluates when both conditions are truthy")
	end
end
$puts("[PASS] Returns control to end block thrice")
$puts("")
