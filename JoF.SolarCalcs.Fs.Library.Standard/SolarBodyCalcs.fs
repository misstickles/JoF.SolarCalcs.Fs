namespace JoF.SolarCalcs.Fs.Library.Standard

// http://aa.quae.nl/en/reken/hemelpositie.html
module SolarBodyCalcs = 

    open Dates
    open System
    open JoF.SolarCalcs.Fs.Library.Standard.Math

    let epsilon = 23.4397

    type Coords = {
        X: double
        Y: double
        Z: double
    }

    type EquatorialCoords = {
        RightAscension: double
        Declination: double
    }

    type Position = {
        Latitude: double
        Longitude: double
        Distance: double
    }

    type CelestialPosition = {
        Altitude: double
        Azimuth: double
    }

    type RiseSet = {
        Rise: double
        Set: double
        Transit: double
    }

    type PlanetData = {
        RiseTime: double
        SetTime: double
        TransitTime: double
        RiseAzimuth: double
        SetAzimuth: double
        TransitAltitude: double
        Distance: double
        RightAscension: double
        Declination: double
    }

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
        member this.a = a   // semi-major axis, au
        member this.e = e   // eccentricity of orbit
        member this.i = i   // inclination of the orbit (tilt), deg
        member this.w = w   // omega, argument of perihelion, deg
        member this.o = o   // large omega ecliptic longitude of the ascending node of the orbit, deg
        member this.m = m   // mean anmaly, M0, at specific date, d0, deg
        member this.n = n   // degrees per day
        member this.ec = ec // au a(1 - e^2)
        member this.p = p   // deg

    // TODO: snuk into OO territory here...
    // orbital elements at utc 1/1/2000, 2451545, relative to earth
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

    // M
    let MeanAnomoly (date: DateTime) (planet: Planet) =
        // n = 0.9856076686 / (a^(3/2)) - or from table?
        // M = M0 + n(j2000)
        let jd = JulianDate2000 date
        let p = Ephemerides planet
        CheckInRange (p.m + p.n * jd) 360.
    
    // v (nu) from M and e
    // TODO: http://aa.quae.nl/en/reken/hemelpositie.html#1_2
    let TrueAnomoly (date: DateTime) (planet: Planet) =
        let m = MeanAnomoly date planet
        let e = (Ephemerides planet).e
        // TODO: this is just MeanAnomoly
        let E = Kepler e m
        CheckInRange (atan2 (sqrt (1. - e * e) * sin E) (cos E - e) * Degrees) 360.

    // distance, r from a, e, v
    let DistanceToSun (date: DateTime) (planet: Planet) =
        // r = a(1-e2) / (1+ecosv)
        let v = TrueAnomoly date planet
        let p = Ephemerides planet
        p.ec / (1. + p.e * dcos v)
    
    // rectangular (cartesian) coordinates; xplanet, yplanet, zplanet from v, r, w o, 
    let HeliocentricEclipticCoords (date: DateTime) (planet: Planet) =
        let p = Ephemerides planet
        let r = DistanceToSun date planet
        let v = TrueAnomoly date planet
        let x = r * (dcos p.o * dcos (p.w + v) - dsin p.o * dcos p.i * dsin (p.w + v))
        let y = r * (dsin p.o * dcos (p.w + v) + dcos p.o * dcos p.i * dsin (p.w + v))
        let z = r * dsin p.i * dsin (p.w + v)
        { X = x; Y = y; Z = z }

    // rectangular coordinates from xplanet, yplanet, zplanet and xearth, yearth, zearth
    let GeocentricEclipticalCoords (date: DateTime) (planet: Planet) =
        let earth = HeliocentricEclipticCoords date Planet.Earth
        let p = HeliocentricEclipticCoords date planet
        { X = p.X - earth.X; Y = p.Y - earth.Y; Z = p.Z - earth.Z } // au
 
    // lambda/beta/deltta from x, y, z
    let GeocentricEclipticalPosition (date: DateTime) (planet: Planet) =
        let coords = GeocentricEclipticalCoords date planet
        let delta = sqrt (coords.X * coords.X + coords.Y * coords.Y + coords.Z * coords.Z)
        let lambda = datan2 coords.Y coords.X * Degrees
        let beta = asin (coords.Z / delta) * Degrees
        { Latitude = CheckInRange beta 360.; Longitude = CheckInRange lambda 360.; Distance = delta }

    // alpha/delta from lambda, beta, epsilon (obliquity)
    let EquatorialCoordinates (date: DateTime) (planet: Planet) =
        let position = GeocentricEclipticalPosition date planet
        let beta, lambda = position.Latitude, position.Longitude
        let delta = asin (dsin beta * dcos epsilon + dcos beta * dsin epsilon * dsin lambda) * Degrees
        let alpha = atan2 (dsin lambda * dcos epsilon - dtan beta * dsin epsilon) (dcos lambda) * Degrees
        { RightAscension = alpha; Declination = delta }

    // H from alpha and theta (sidereal time)
    let HourAngle (date: DateTime) (planet: Planet) long =
        let lmst = LocalMeanSiderealTime date long
        let ra = (EquatorialCoordinates date planet).RightAscension
        lmst - ra

    let Refraction altitude =
        (0.017 / dtan(altitude + (10.26 / (altitude + 5.10))))

    // h/A from H, delta and geographical lat (phi)
    let CelestialPosition (date: DateTime) (planet: Planet) latitude longitude =
        let dec = (EquatorialCoordinates date planet).Declination
        let ha = HourAngle date planet longitude
        let h = asin (dsin latitude * dsin dec + dcos latitude * dcos dec * dcos ha) * Degrees
        let az = (atan2 (dsin ha) (dcos ha * dsin latitude - dtan dec * dcos latitude) + Pi) * Degrees
        { Altitude = (h + Refraction h); Azimuth = az }

    // psi from geocentric ecliptic coords of sun and planet
    let Elongation =
        0

    let Transit (date: DateTime) (planet: Planet) longitude =
        let ra = (EquatorialCoordinates date planet).RightAscension
        let m = MeanAnomoly date Planet.Earth
        // let p = (Ephemerides Planet.Earth).p
        // (ra + longitude - m - p) / 15.
        // TODO: how does that equate to this??
        (ra + longitude - m) / 15. + 17.1375

    let RiseSetTimes (date: DateTime) (planet: Planet) latitude longitude =
        let h = 0.
        let dec = (EquatorialCoordinates date planet).Declination
        let hHorizon = acos ((dsin h - dsin latitude * dsin dec) / (dcos latitude * dcos dec)) * Degrees
        let transit = Transit date planet longitude
        { Rise = CheckInRange (transit - (hHorizon / 15.)) 24.; Set = CheckInRange (transit + (hHorizon / 15.)) 24.; Transit = transit }

    let PlanetData (date: DateTime) (planet: Planet) latitude longitude =
        let times = RiseSetTimes date planet latitude longitude
        let riseAz = (CelestialPosition (Converter.DecimalTimeToDate date times.Rise) planet latitude longitude).Azimuth
        let setAz = (CelestialPosition (Converter.DecimalTimeToDate date times.Set) planet latitude longitude).Azimuth
        let transitAlt = (CelestialPosition (Converter.DecimalTimeToDate date times.Transit) planet latitude longitude).Altitude
        let eqCoords = EquatorialCoordinates date planet

        { RiseTime = times.Rise; SetTime = times.Set; TransitTime = times.Transit;
            RiseAzimuth = riseAz; SetAzimuth = setAz; TransitAltitude = transitAlt;
            Distance = DistanceToSun date planet; RightAscension = eqCoords.RightAscension;
            Declination = eqCoords.Declination }
