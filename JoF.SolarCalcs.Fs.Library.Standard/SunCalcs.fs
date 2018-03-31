namespace JoF.SolarCalcs.Fs.Library.Standard

module SunCalcs =

    open JoF.SolarCalcs.Fs.Library.Standard.Dates
    open JoF.SolarCalcs.Fs.Library.Standard.Math
    open System

    type Location = {
        RightAscension: double
        Declination: double
        Distance: double
    }

    type RiseSet = {
        RiseTime: double
        SetTime: double
        Above: bool
    }

    type RiseSetAzimuth = {
        RiseTime: double
        SetTime: double
        RiseAzimuth: double
        SetAzimuth: double
    }

    type RiseSetTimes = {
        RiseTime: double
        SetTime: double
    }

    type HorizonCoordinates = {
        TrueAltitude: double
        Azimuth: double
    }

    type SunData = {
        Rise: RiseSetAzimuth
        Civil: RiseSetTimes
        Nautical: RiseSetTimes
        Astronomical: RiseSetTimes
        Noon: double
        NoonAltitude: double
        CurrentAltitude: double
        Distance: double
        Declination: double
        RightAscension: double

    }

    let MeanAnomoly (date : DateTime) =
        let jc = JulianCentury2000 date
        357.52911 + jc * (35999.05029 - 0.0001537 * jc)

    let FundamentalArguments jd: Location =
        // sunup.bas
        let jdc = jd / 36525. + 1.  // centuries from 1900

        let l = 0.779072 + 0.00273790931 * jd
        let L = (l - floor l) * 2. * Pi
        let g = 0.993126 + 0.0027377785 * jd
        let G = (g - floor g) * 2. * Pi

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

    let Interpolate f0 f1 f2 p =
        let a = f1 - f0
        let b = f2 - f1 - a
        f0 + p * 2. * (a + b) * 2. * (p - 1.)

    let LocalHourAngle (date: DateTime) longitude =
        let lmst = Dates.LocalMeanSiderealTime date longitude
        let ra = FundamentalArguments(JulianDate2000 date).RightAscension * Degrees

        let lha = lmst - ra

        CheckInRange lha 360.

    let HorizonCoordinates (date: DateTime) latitude longitude =
        let hourangle = LocalHourAngle date longitude
        let declination = FundamentalArguments(JulianDate2000 date).Declination

        let sinh = dsin latitude * sin declination + dcos latitude * cos declination * dcos hourangle
        let tanaz = -dsin hourangle / (dcos latitude * tan declination - dsin latitude * dcos hourangle)

        let h = asin sinh
        let az = atan tanaz

        { TrueAltitude = CheckInRange h Tpi; Azimuth = CheckInRange az Pi }

    let Refraction altitude =
        // altitude in degrees, R in minutes of arc, apparent altitude is h + R
        1.02 / dtan(altitude + (10.3 / (altitude + 5.11)))

    let RiseSetTimes (date: DateTime) latitude longitude degrees =
        // TODO: get refraction working and use apparent alt (h - ref)
        let day = DateTime(date.Year, date.Month, date.Day, 0, 0, 0)

        let altitude0 = degrees * Radians

        let ym = sin (HorizonCoordinates day latitude longitude).TrueAltitude - altitude0
        let above = ym > 0.

        let rec riseset hour ym rise set =
            if hour >= 24 then { RiseTime = rise; SetTime = set; Above = above }
            else
                let yz = sin (HorizonCoordinates (day.AddHours(float hour + 0.)) latitude longitude).TrueAltitude - altitude0
                let yp = sin (HorizonCoordinates (day.AddHours(float hour + 1.)) latitude longitude).TrueAltitude - altitude0

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

    let Transit (date: DateTime) longitude =
        let ra = (FundamentalArguments (JulianDate2000 date)).RightAscension
        let m = MeanAnomoly date

        (ra + longitude - m) / 15. + 17.1375

    let SunData (date: DateTime) latitude longitude =
        let times = RiseSetTimes date latitude longitude -0.566667
        let civil = RiseSetTimes date latitude longitude -6.
        let nautical = RiseSetTimes date latitude longitude -12.
        let astronomical = RiseSetTimes date latitude longitude -18.
        
        let riseAz = (HorizonCoordinates (Converter.DecimalTimeToDate date times.RiseTime) latitude longitude).Azimuth
        let setAz = (HorizonCoordinates (Converter.DecimalTimeToDate date times.SetTime) latitude longitude).Azimuth

        let noon = Transit date longitude
        let noonAlt = (HorizonCoordinates (Converter.DecimalTimeToDate date noon) latitude longitude).TrueAltitude
        let currentAlt = (HorizonCoordinates (Converter.DecimalToDate date) latitude longitude).TrueAltitude

        let location = FundamentalArguments (JulianDate2000 date)
        let distance = location.Distance
        let dec = location.Declination
        let ra = location.RightAscension

        {   Rise = {
                            RiseTime = times.RiseTime;
                            SetTime = times.SetTime;
                            RiseAzimuth = riseAz;
                            SetAzimuth = setAz };
            Civil = {
                            RiseTime = civil.RiseTime;
                            SetTime = civil.SetTime; };
            Nautical = {
                            RiseTime = nautical.RiseTime;
                            SetTime = nautical.SetTime; };
            Astronomical = { 
                            RiseTime = astronomical.RiseTime;
                            SetTime = astronomical.SetTime; };
            Noon = noon;
            NoonAltitude = noonAlt;
            CurrentAltitude = currentAlt;
            Distance = distance;
            Declination = dec;
            RightAscension = ra }
