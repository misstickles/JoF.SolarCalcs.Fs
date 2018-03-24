namespace JoF.SolarCalcs.Fs.Library.Standard

module SunCalcs =

    open JoF.SolarCalcs.Fs.Library.Standard.Math

    type Location = {
        RightAscension: double
        Declination: double
        Distance: double
    }

    let FundamentalArguments jd: Location =
        let jdc = jd / 36525.

        let L = 0.779072 + 0.00273790931 * jd
        let L = L - floor L
                * 2. * Pi
        let G = 0.993126 + 0.0027377785 * jd
        let G = G - floor G
                * 2. * Pi

        let v = 0.39785 * sin(L)
                - 0.01000 * sin(L - G)
                + 0.00333 * sin(L + G)
                - 0.00021 * jdc * sin(L)
        let u = 1. - 0.03349 * cos(G)
                - 0.00014 * cos(2. * L)
                + 0.00008 * cos(L)
        let w = -0.00010 - 0.04129 * sin(2. * L)
                + 0.03211 * sin(G)
                + 0.00104 * sin(2. * L - G)
                - 0.00035 * sin(2. * L + G)
                - 0.00008 * jdc * sin(G)
        
        // TODO: these are the same as for moon (and probably planets!)
        let mutable s = w / sqrt(u - v * v)

        let ra = L + atan(s / sqrt (1. - s * s))

        s <- v / sqrt u
        
        let dec = atan(s / sqrt(1. - s * s))

        let distance = 1.00021 * sqrt u

        { RightAscension = ra; Declination = dec; Distance = distance }