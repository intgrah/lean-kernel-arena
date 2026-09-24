-- Kernel Nat-literal extension on large numerals.
-- Small `2 + 2` can also reduce by unfolding `Nat.add` to `Nat.succ` chains;
-- these values make that impractical, so a checker has to use the literal ops.

theorem natAddLit : Nat.add 123456789 987654321 = 1111111110 := rfl

theorem natSubLit : Nat.sub 1000000 1000001 = 0 := rfl

theorem natBleLit : Nat.ble 1000000 1000001 = true := rfl
