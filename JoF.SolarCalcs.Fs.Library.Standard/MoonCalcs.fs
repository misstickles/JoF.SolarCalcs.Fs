﻿namespace JoF.SolarCalcs.Fs.Library.Standard

// http://aa.quae.nl/en/reken/hemelpositie.html#4
module MoonCalcs =

    open Dates
    open Math
    open System

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

    type HorizonCoordinates = {
        TrueAltitude: double
        Azimuth: double
    }

    type Point = {
        X: double
        Y: double
    }
    type QuadEvent = {
        Events: int
        MinMax: Point
        Z1: double
        Z2: double
    }

    type MoonData = {
        Rise: double
        Set: double
        Transit: DateTime
        RiseAzimuth: double
        SetAzimuth: double
        TransitAzimuth: double
        Distance: double
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

    let SunPosition (jd: Double) =
        0.

    //let Position (jd: Double, sunPosition: Double) =
     //   let mutable ms = 0.985647332099 * jd - 3.762863
     //   ms <- if ms < 0. then dsin(ms + 360.) else dsin(ms)

     //   let mutable l = 13.176396 * jd + 64.975464

     //   let mm = l - 0.1114041 * j - 349.383063
    //let i = (mm/360).toNumber()
    //let mm -= i * 360.0
    //let n = 151.950429 - 0.0529539 * j
    //let i = (n / 360).toNumber()
    //let n -= i * 360.0
    //let ev = 1.2739 * dsin(2 * (l - ls) - mm)
    //let ae = 0.1858 * ms
    //let mm += ev - ae - 0.37 * ms
    //let ec = 6.2886 * dsin mm
    //let l += ev + ec - ae + 0.214 * dsin(2 * mm)
    //l = 0.6583 * dsin(2 * (l - ls)) + l

    //l
        //0.

    //let Phase (date: DateTime) =
    //    let jd = JulianDate2000 date
    //    let sunPosition = SunPosition jd
    //    let moonPosition = Position(jd, sunPosition)

    //    let x = (((1. - dcos(moonPosition - sunPosition) / 2.) * 1000.) + 0.5) / 10.

    //    let position = moonPosition - sunPosition

    //    let t = if position < 0. then position + 180. else position

    //    if t > 180. then x * -1. else x
    
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

    let Location jd =
        [0; 1; 2] |> List.map(fun x -> FundamentalArguments(jd + float x / 2.))
    
    let Interpolate f0 f1 f2 p =
        let a = f1 - f0
        let b = f2 - f1 - a
        f0 + p * 2. * (a + b) * 2. * (p - 1.)

    let Quadratic ym yz yp = 
        // max/min point = xe, xy
        // ym yz yp = y points
        // z1 z2 = x where parabola crosses zero (roots of quadratic)
        // nz = number of roots (0, 1, 2) within interval [-1,1]
        let nz = 0;
        let a = 0.5 * (ym + yp) - yz;
        let b = 0.5 * (yp - ym);
        let c = yz;
        let xe = -b / (2. * a);
        let ye = (a * xe + b) * xe + c;
        let dis = b * b - 4. * a * c;
        if dis > 0. then
            let dx = 0.5 * sqrt dis / abs a
            let z1 = xe - dx
            let z2 = xe + dx
            let nz = if abs z1 <= 1.0 then nz + 1 else nz
            let nz = if abs z2 <= 1.0 then nz + 1 else nz
            let z1 = if z1 < -1. then z2 else z1
            { Events = nz; MinMax = { X = xe; Y = ye }; Z1 = z1; Z2 = z2 }
        else
            { Events = 0; MinMax = { X = xe; Y = ye }; Z1 = 0.; Z2 = 0. }

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

        let altitude0 = sin (8. / 60. * Radians)    // centre of moon at +8 arcmin

        let ym = (sin (LunarHorizonCoordinates day latitude longitude).TrueAltitude) - altitude0
        let above = ym > 0.

        let rec riseset hour ym rise set =
            if hour > 24 then rise, set, above
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

        riseset 0 ym 0. 0.
