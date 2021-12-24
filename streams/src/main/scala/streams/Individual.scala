package streams

class IndividualTask {
    def toList(range: Seq[Int], k: Int): List[BigInt] = {
        def func = new PartialFunction[(Int, Int), BigInt] {
            def apply(params: (Int, Int)) = if (params._1 > params._2) fact(params._1, 1) else params._2
            def isDefinedAt(params: (Int, Int)): Boolean = {
                val x = params._1
                val k = params._2
                x > 0 && (x > k || (x > 1 && x < k) ) 
            }
        }

        def fact(x: BigInt, res: BigInt): BigInt = {
            if (x > 0) fact(x - 1, res * x)
            else res
        }

        if (range.isEmpty) List()
        else {
            if (!func.isDefinedAt(range.head, k)) toList(range.tail, k)
            else func(range.head, k) :: toList(range.tail, k)
        }
    }

    def testFunctions() {
        val lst = toList(-10 to 20, 5)
        println(lst)
        println(lst.forall(x => x % 2 == 0))
        println(lst.filter(x => x / 5 == 0))
        println(lst.sortBy(x => -x))
        println(lst.map(x => -x))
        println(lst.reduceLeft((x1,x2) => x1 + x2))
    }
}
