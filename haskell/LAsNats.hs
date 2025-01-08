module LAsNats where
import ThanosRepo.ExNat as N

data LAsNats = LNats N.Nat
  deriving (Show, Eq, Ord)

{--toLAN :: [N.Nat] -> LAsNats
toLAN ns = LNats (foldl (N.<*>) (S O) (zipWith (\p e -> p (N.<^>) (e(N.<+>)(S O))) primos ns))

primos :: [N.Nat]
primos = S(S O) : eulers [(S(S(S O))),(S(S(S(S(S O)))))..]
  where eulers (p:xs) = p : eulers (xs `minus` map (p(N.<*>)) (p:xs))--}
