namespace JoF.SolarCalcs.Fs.Library.Standard

// http://aa.quae.nl/en/reken/hemelpositie.html#4
module MoonCalcs =

    open Dates
    open Math
    open System
    open System.Globalization

    type Means =
        L = 0
        | M = 1
        | F = 2
    
    type RiseOrSet =
        | Rise
        | Set

    type MeanData(c0: double, c1: double) =
        member this.c0 = c0
        member this.c1 = c1

    type Location = {
        RightAscension: double
        Declination: double
        Parallax: double
    }

    type EclipticCoordinates = {
        lambda: double
        beta: double
        delta: double
    }

    type RiseSet = {
        RiseTime: double
        SetTime: double
        Above: bool
    }

    type HorizonCoordinates = {
        TrueAltitude: double
        Azimuth: double
    }

    type MoonData = {
        Rise: double
        Set: double
        Transit: double
        RiseAzimuth: double
        SetAzimuth: double
        TransitAltitude: double
        Distance: double
        Elevation: double
        Age: double
        Illumination: double
    }

    let MeanData (m: Means) =
        match m with
        | Means.L -> new MeanData(218.316, 13.176396)
        | Means.M -> new MeanData(134.963, 13.064993)
        | Means.F -> new MeanData(93.272, 13.229350)
        | _ -> new MeanData(0., 0.)

    let LMF (date: DateTime) =
        let jd = JulianDate2000 date
        let l = MeanData Means.L
        let m = MeanData Means.M
        let f = MeanData Means.F

        let L = (l.c0 + l.c1 * jd) % 360.
        let M = (m.c0 + m.c1 * jd) % 360.
        let F = (f.c0 + f.c1 * jd) % 360.

        L, M, F

    let GeocentricEclipticCoords (date: DateTime) =
        let L, M, F = LMF date
        let lambda: double = L + 6.289 * dsin M   // longitude
        let beta: double = 5.128 * dsin F          // latitude
        let delta: double = 385001. - 20905. * dcos M    // distance

        // longitude, latitude, distance
        { lambda = lambda; beta = beta; delta = delta }

    let FundamentalArguments jd: Location =
        // www.skyandtelescope.com/wp-content/uploads/moonup.bas
        let tpi = 2. * Math.PI

        let l = frac(0.606434 + 0.03660110129 * jd) * tpi
        let m = frac(0.374897 + 0.03629164709 * jd) * tpi
        let f = frac(0.259091 + 0.03674819520 * jd) * tpi
        let d = frac(0.827362 + 0.03386319198 * jd) * tpi
        let n = frac(0.347343 - 0.00014709391 * jd) * tpi
        let g = frac(0.993126 + 0.00273777850 * jd) * tpi

        let v = 0.39558 * sin(f + n)
                + 0.08200 * sin(f)
                + 0.03257 * sin(m - f - n)
                + 0.01092 * sin(m + f + n)
                + 0.00666 * sin(m - f)
                - 0.00644 * sin(m + f - 2. * d + n)
                - 0.00331 * sin(f - 2. * d + n)
                - 0.00304 * sin(f - 2. * d)
                - 0.00240 * sin(m - f - 2. * d - n)
                + 0.00226 * sin(m + f)
                - 0.00108 * sin(m + f - 2. * d)
                - 0.00079 * sin(f - n)
                + 0.00078 * sin(f + 2. * d + n)

        let u = 1. - 0.10828 * cos(m)
                - 0.01880 * cos(m - 2. * d)
                - 0.01479 * cos(2. * d)
                + 0.00181 * cos(2. * m - 2. * d)
                - 0.00147 * cos(2. * m)
                - 0.00105 * cos(2. * d - g)
                - 0.00075 * cos(m - 2. * d + g)

        let w = 0.10478 * sin(m)
                - 0.04105 * sin(2. * f + 2. * n)
                - 0.02130 * sin(m - 2. * d)
                - 0.01779 * sin(2. * f + n)
                + 0.01774 * sin(n)
                + 0.00987 * sin(2. * d)
                - 0.00338 * sin(m - 2. * f - 2. * n)
                - 0.00309 * sin(g)
                - 0.00190 * sin(2. * f)
                - 0.00144 * sin(m + n)
                - 0.00144 * sin(m - 2. * f - n)
                - 0.00113 * sin(m + 2. * f + 2.*n)
                - 0.00094 * sin(m - 2. * d + g)
                - 0.00092 * sin(2. * m - 2. * d)

        let mutable s = w / sqrt(u - v * v)

        let ra = l + atan(s / sqrt(1.0 - s * s))

        s <- v / sqrt(u)
        
        let dec = atan(s / sqrt(1. - s * s))

        let parallax = 60.40974 * sqrt(u)

        { RightAscension = ra; Declination = dec; Parallax = parallax } // degrees

    let Interpolate f0 f1 f2 p =
        let a = f1 - f0
        let b = f2 - f1 - a
        f0 + p * 2. * (a + b) * 2. * (p - 1.)

    let LocalHourAngle (date: DateTime) longitude =
        let lmst = Dates.LocalMeanSiderealTime date longitude
        let ra = FundamentalArguments(JulianDate2000 date).RightAscension * Degrees

        let lha = lmst - ra

        CheckInRange lha 360.

    let LunarHorizonCoordinates (date: DateTime) latitude longitude =
        let hourangle = LocalHourAngle date longitude
        let declination = FundamentalArguments(JulianDate2000 date).Declination

        let sinh = dsin latitude * sin declination + dcos latitude * cos declination * dcos hourangle
        let tanaz = -dsin hourangle / (dcos latitude * tan declination - dsin latitude * dcos hourangle)

        let h = asin sinh
        let az = atan tanaz

        { TrueAltitude = CheckInRange h Tpi; Azimuth = CheckInRange az Pi }

    let ParallaxInAltitude (date: DateTime) altitude =
        let distance = (GeocentricEclipticCoords date).delta
        let horizontalParallax = 8.794 / (distance / 149597890.) // arcseconds
        let sinp = cos altitude * sin(horizontalParallax / 3600.)  // degrees

        sin sinp

    let Refraction altitude =
        // altitude in degrees, R in minutes of arc, apparent altitude is h + R
        1.02 / dtan(altitude + (10.3 / (altitude + 5.11)))

    let RiseSetTimes (date: DateTime) latitude longitude =
        // TODO: get refraction working and use apparent alt (h - ref)
        let day = DateTime(date.Year, date.Month, date.Day, 0, 0, 0)

        let altitude0 = dsin (8. / 60.)    // centre of moon at +8 arcmin

        let ym = sin (LunarHorizonCoordinates day latitude longitude).TrueAltitude - altitude0
        let above = ym > 0.

        let rec riseset hour ym rise set =
            if hour >= 24 then { RiseTime = rise; SetTime = set; Above = above }
            else
                let yz = sin (LunarHorizonCoordinates (day.AddHours(float hour + 0.)) latitude longitude).TrueAltitude - altitude0
                let yp = sin (LunarHorizonCoordinates (day.AddHours(float hour + 1.)) latitude longitude).TrueAltitude - altitude0

                let quad = Quadratic ym yz yp

                match quad.Events with
                    | e when e.Equals 1 ->
                        if ym < 0. then
                            riseset (hour + 2) yp (double hour + quad.Z1) set
                        else 
                            riseset (hour + 2) yp rise (double hour + quad.Z1)
                    | e when e.Equals 2 ->
                        if quad.MinMax.Y < 0. then
                            riseset (hour + 2) yp (double hour + quad.Z2) (double hour + quad.Z1)
                        else
                            riseset (hour + 2) yp (double hour + quad.Z1) (double hour + quad.Z2)
                    | _ -> riseset (hour + 2) yp rise set

        riseset 0 ym -1. -1.
    
    let Elongation (date: DateTime) =
        let jd = JulianDate2000 date
        let moonLocation = FundamentalArguments jd
        let sunLocation = SunCalcs.FundamentalArguments jd
        let dlon = sunLocation.RightAscension - moonLocation.RightAscension
        let moonDec, sunDec = moonLocation.Declination, sunLocation.Declination

        acos(sin moonDec * sin sunDec + cos moonDec * cos sunDec * cos dlon)

    let Illumination (date: DateTime) = 
        // http://conga.oan.es/~alonso/doku.php?id=blog:sun_moon_position, Tomás Alonso Albi comment
        let elong = Elongation date

        (1. - (cos elong)) * 0.5

    let Age (date: DateTime) = 
        // http://www.skyandtelescope.com/wp-content/uploads/moonfx.bas
        let jd = JulianDateTime date - 2451550.1    // or use midday?
        let v = jd / 29.530588853
        CheckInRange v 1. * 29.53
 
    let MoonData (date: DateTime) latitude longitude =
        let times = RiseSetTimes date latitude longitude
        let riseAz = (LunarHorizonCoordinates (Converter.DecimalTimeToDate date times.RiseTime) latitude longitude).Azimuth
        let setAz = (LunarHorizonCoordinates (Converter.DecimalTimeToDate date times.SetTime) latitude longitude).Azimuth
        // let transitAlt = (LunarHorizonCoordinates (Converter.DecimalToDate date times.RiseTime) latitude longitude).Azimuth

        let eclipticCoords = GeocentricEclipticCoords date
        let illumination = Illumination date
        let age = Age date

        { Rise = times.RiseTime; 
            Set = times.SetTime; 
            Transit = 0.; 
            RiseAzimuth = riseAz;
            SetAzimuth = setAz;
            TransitAltitude = 0.;
            Distance = eclipticCoords.delta;
            Elevation = 0.;
            Age = age;
            Illumination = illumination }

    let MoonPhasesMonth year month = 
        let calendar = CultureInfo.CurrentCulture.Calendar
        let daysInMonth = calendar.GetDaysInMonth(year, month)

        [ for d in 1..daysInMonth do yield Illumination (DateTime(year, month, d, 12, 0, 0)) ]

