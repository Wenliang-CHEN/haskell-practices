module TowerOfHanoi where
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi layerCount start temp dest
    | layerCount <= 0 = []
    | layerCount == 1 = [(start, dest)]
    | layerCount == 2 = [(start, temp), (start, dest), (temp, dest)]
    | layerCount > 2 = hanoi (layerCount - 1) start dest temp ++ [(start, dest)] ++ hanoi (layerCount - 1) temp start dest