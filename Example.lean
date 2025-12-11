variable {p q r : Prop}

theorem imp_trans (hpq : p → q) (hqr : q → r) : p → r := by
  intro h
  apply hqr
  apply hpq
  apply h

theorem or_comm' (h : p ∨ q) : q ∨ p := by
  rcases h
  right
  assumption
  left
  assumption
