module Utils.Misc


module Array = 
    let shuffle (input: 'a []) =
        let rgen = System.Random()
        let input = Array.copy input
        let l = input.Length
        for i in (l-1) .. -1 .. 1 do
            let temp = input.[i]
            let j = rgen.Next(0, i+1)
            input.[i] <- input.[j]
            input.[j] <- temp
        input