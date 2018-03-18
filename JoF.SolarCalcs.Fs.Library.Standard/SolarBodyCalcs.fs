namespace JoF.SolarCalcs.Fs.Library.Standard

module SolarBodyCalcs = 

    open Dates
    open System

    let Radians = Math.PI / 180.
    let Degrees = 180. / Math.PI

    type Planet = 
        | Mercury = 0
        | Venus = 1
        | Earth = 2
        | Mars = 4
        | Jupiter = 8
        | Saturn = 16
        | Uranus = 32
        | Neptune = 64
        | Pluto = 128

    type Ephemerides(a: double, e: double, i: double, w: double, o: double, m: double,
                        n: double, ec: double, p: double) =
        member this.a = a
        member this.e = e
        member this.i = i
        member this.w = w
        member this.o = o
        member this.m = m
        member this.n = n
        member this.ec = ec
        member this.p = p

    // TODO: snuk into OO territory here...
    let Ephemerides (p: Planet) : Ephemerides =
        match p with
        | Planet.Mercury -> new Ephemerides(0.38710, 0.20563, 7.005, 29.125, 48.331, 174.795, 4.092317, 0.37073, 77.456)
        | Planet.Venus -> new Ephemerides(0.72333, 0.00677, 3.395, 54.884, 76.680, 50.416, 1.602136, 0.72330, 131.564)
        | Planet.Earth -> new Ephemerides(1.00000, 0.01671, 0.000, 288.064, 174.873, 357.529, 0.985608, 0.99972, 102.937)
        | Planet.Mars -> new Ephemerides(1.52368, 0.09340, 1.850, 286.502, 49.558, 19.373, 0.524039, 1.51039, 336.060)
        | Planet.Jupiter -> new Ephemerides(5.20260, 0.04849, 1.303, 273.867, 100.464, 20.020, 0.083056, 5.19037, 14.331)
        | Planet.Saturn -> new Ephemerides(9.55491, 0.05551, 2.489, 339.391, 113.666, 317.021, 0.033371, 9.52547, 93.057)
        | Planet.Uranus -> new Ephemerides(19.21845, 0.04630, 0.773, 98.999, 74.006, 141.050, 0.011698, 19.17725, 173.005)
        | Planet.Neptune -> new Ephemerides(30.11039, 0.00899, 1.770, 276.340, 131.784, 256.225, 0.005965, 30.10796, 48.124)
        | Planet.Pluto -> new Ephemerides(39.543, 0.2490, 17.140, 113.768, 110.307, 14.882, 0.003964, 37.09129, 224.075)
        | _ -> new Ephemerides(0., 0., 0., 0., 0., 0., 0., 0., 0.)

    let MeanAnomoly (date: DateTime, planet: Planet) =
        // M = M0 + n(j2000)
        let jd = JulianDate2000 date
        let p = Ephemerides planet
        (p.m + p.n * jd) % 360.

    let TrueAnomoly (M: double, e: double) =
        0

    let DistanceToSun (planet: Planet, v: double) =
        // r = a(1-e2) / (1+ecosv)
        let p = Ephemerides planet
        (p.a * (1. - Math.Pow(p.e, 2.))) / (1. + p.e * cos v)
    
    let HeliocentricEclipticCoords =
        0

    let GeocentricEclipticCoords =
        0
    
    let GeocentricEclipticLongitude =
        0
    
    let GeocentricEclipticLatitude =
        0
    
    let RightAscension =
        0
    
    let Declination =
        0

    let HourAngle =
        0

    let Height =
        0

    let Azimuth =
        0

    let Elongation =
        0

