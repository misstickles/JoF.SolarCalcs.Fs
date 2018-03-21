namespace JoF.SolarCalcs.Fs.Library.Standard

module SunCalcs =

    open System
    open Dates
    open Math

    let Degrees = 180. / Math.PI

    let L (date : DateTime) =
        let jc = JulianCentury2000 date
        (280.46646 + jc * (36000.76983 + jc * 0.0003032)) % 360.

    let G (date : DateTime) =
        let jc = JulianCentury2000 date
        357.52911 + jc * (35999.05029 - 0.0001537 * jc)

    let e (date : DateTime) =
        let jc = JulianCentury2000 date
        0.016708634 - jc * (0.000042037 + 0.0000001267 * jc)

    let SunEquationOfCentre (date : DateTime) =
        let jc = JulianCentury2000 date
        let g = G date
        dsin(g) * (1.914602 - jc * (0.004817 + 0.000014 * jc)) 
            + dsin(2. * g) * (0.019993 - 0.000101 * jc)
            + dsin(3. * g) * 0.000289

    let SunTrueLongitude (date : DateTime) = 
        L date + SunEquationOfCentre date

    let sunTrueAnomoly (date : DateTime) = 
        G date + SunEquationOfCentre date

    let R (date : DateTime) =
        let ecc = (e date)
        (1.000001018 * (1. - ecc * ecc)) / (1. + ecc * dcos(sunTrueAnomoly date))

    let lambda (date : DateTime) =
        let jc = JulianCentury2000 date
        (SunTrueLongitude date) - 0.00569 - 0.00478 * dsin(125.04 - 1934.136 * jc)

    let MeanObliquityCorrection (date : DateTime) =
        let jc = JulianCentury2000 date
        23. + (26. + ((21.448 - jc * (46.815 + jc * (0.00059 - jc * 0.001813)))) / 60.0) / 60.0

    let epsilon (date : DateTime) =
        let jc = JulianCentury2000 date
        (MeanObliquityCorrection date) + 0.00256 * dcos(125.04 - 1934.136 * jc)

    let RightAscension (date : DateTime) =
        let l = lambda date
        let e = epsilon date
        atan2 (dcos e * dsin l) (dcos l) * Degrees

    let Declination (date : DateTime) = 
        let e = epsilon date
        let l = lambda date
        asin(dsin e * dsin l) * Degrees

    let E (date : DateTime) =
        let e2 = (epsilon date) / 2.
        let e = (e date)
        let y = tan(e2) * tan(e2)
        let l = (L date)
        let g = (G date)

        4. * (
            y * dsin(2. * l)
            - 2. * e * dsin(g)
            + 4. * e * y * dsin(g) * cos(2. * l)
            - 0.5 * y * y * dsin(4. * l)
            - 1.25 * e * e * dsin(2. * g)) * Degrees

    let SolarNoon longitude E =
        (720. - 4. * longitude - E) / 1440.0

    let HourAngleSunrise (date : DateTime) latitude =
        let l = latitude
        let d = (Declination date) 
        (acos(dcos 90.833 / (dcos l * dcos d) - dtan l * dtan d)) * Degrees

    let SunRise (date : DateTime) latitude longitude =
        let ha = HourAngleSunrise date latitude
        let noon = SolarNoon longitude (E date)
        (noon - ha * 4. / 1440.0) * 24.0

    let SunSet (date : DateTime) latitude longitude =
        let ha = HourAngleSunrise date latitude
        let noon = SolarNoon longitude (E date)
        (noon + ha * 4. / 1440.0) * 24.0

    let DayLength (date : DateTime) latitude =
        8. * HourAngleSunrise date latitude

    let TrueSolarTime (date : DateTime) longitude =
        let e = E date
        let ut = double date.Hour / 24.
        (ut * 1440. + e + 4. * longitude) % 1440.

    let HourAngle (date : DateTime) longitude =
        let time = (TrueSolarTime date longitude) / 4.
        match time < 0. with
        | true -> time + 180.
        | false -> time - 180.

    let SolarZenithAngle (date : DateTime) latitude longitude =
        let delta = Declination date
        let ha = HourAngle date longitude
        let lat = latitude
        (acos(dsin lat * dsin delta + dcos lat * dcos delta * dcos ha)) * Degrees

    let SolarElevationAngle (date : DateTime) latitude longitude =
        90. - (SolarZenithAngle date latitude longitude)

    let AtmosphericRefraction (date : DateTime) latitude longitude = 
        let elevAngle = SolarElevationAngle date latitude longitude
        let angle = elevAngle
        let ref = 
            match elevAngle with
            | x when x > 85. -> 0.
            | x when x > 5. -> 58.1 / dtan angle - 0.07 / (dtan angle ** 3.) + 0.000086 / (dtan angle ** 5.0)
            | x when x > -0.575 -> 1735. + elevAngle * (-518.2 + elevAngle * (103.4 + elevAngle * (-12.79 + elevAngle * 0.711)))
            | _ -> -20.772 / dtan angle
        ref / 3600.

    let SolarElevationCorrected (date : DateTime) latitude longitude = 
        (SolarElevationAngle date latitude longitude) + (AtmosphericRefraction date latitude longitude)

    let SolarAzimuthAngle (date : DateTime) latitude longitude =
        let ha = HourAngle date longitude
        let zenith = (SolarZenithAngle date latitude longitude)
        let delta = (Declination date)
        let lat = latitude
        let az =
            if ha > 0. then
                (acos(((dsin lat * cos zenith) - dsin delta) / (cos lat * dsin zenith)) + 180.) * Degrees
            else
                540. - (acos(((dsin lat * dcos zenith) - dsin delta) / (cos lat * dsin zenith))) * Degrees
        az % 360.
