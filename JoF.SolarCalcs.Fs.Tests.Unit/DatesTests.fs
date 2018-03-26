namespace JoF.SolarCalcs.Fs.Tests.Unit

module DatesTests =
    open System;
    open Xunit;
    open JoF.SolarCalcs.Fs.Library.Standard

    let AssertDelta exp act delta =
        let low = exp - delta
        let high = exp + delta
        Assert.InRange(act, low, high)

    let myDate = DateTime.Parse "18 February 2017, 16:00:00"

    [<Fact>]
    let ``JulianDateTime myBirthday isCorrect``() =
        AssertDelta 2457803.166666667 (Dates.JulianDateTime myDate) 0.00000001

    [<Fact>]
    let ``JulianCentury2000 myBirthday isCorrect``() =
        AssertDelta 0.17133927 (Dates.JulianCentury2000 myDate) 0.00000001

    [<Fact>]
    let ``JulianDateTime isCorrect``() =
        let d = DateTime.Parse "8 March 2018, 14:00:00"
        AssertDelta 2458186.083333 (Dates.JulianDateTime d) 0.000001

    [<Fact>]
    let ``GreenwichMeanSiderealTime March2018 isCorrect``() =
        let d = DateTime.Parse "8 March 2018, 14:00:00"
        AssertDelta 16.226928 (Dates.GreenwichMeanSiderealTime d) 0.00001

    [<Fact>]
    let ``LocalMeanSiderealTime March2018 isCorrect``() =
        let d = DateTime.Parse "8 March 2018, 14:00:00"
        let long = -0.1
        AssertDelta 16.126928 (Dates.LocalMeanSiderealTime d long) 0.00001

    [<Fact>]
    let ``LocalMeanSiderealTime May1991 isCorrect``() =
        let d = DateTime.Parse "19 May 1991, 13:00:00"
        let long = 10.
        AssertDelta 81.698 (Dates.LocalMeanSiderealTime d long) 0.00014

    [<Fact>]
    let ``LocalMeanSiderealTime March2018 hmsIsCorrect``() =
        let d = DateTime.Parse "8 March 2018, 14:00:00"
        let long = -0.1
        let lmst = Dates.LocalMeanSiderealTime d long
        let hms = Converter.DegreesToHms lmst
        Assert.Equal(1, hms.Hour)
        AssertDelta 4. (float hms.Minute) 2.
        AssertDelta 30.4604 hms.Second 0.01

    [<Fact>]
    let ``GreenwichMeanSiderealTime May1991 isCorrect``() =
        // http://aa.usno.navy.mil/siderealtime?year=2018&month=3&day=8&hr=14&min=0&sec=0.0&intv_mag=1.0&intv_unit=1&reps=5&place=&lon_sign=-1&lon_deg=0.1&lon_min=&lon_sec=&lat_sign=1&lat_deg=51&lat_min=&lat_sec=
        let d = DateTime.Parse "19 May 1991, 13:00:00"
        AssertDelta 71.698 (Dates.GreenwichMeanSiderealTime d) 0.00014

    [<Fact>]
    let ``Easter March2018 isCorrect``() =
        let d = DateTime.Parse "10 March 2018, 14:00:00"
        let easter = Dates.Easter d.Year
        Assert.Equal(4, easter.Month)
        Assert.Equal(1, easter.Day)

    [<Fact>]
    let ``Easter March2016 isCorrect``() =
        let d = DateTime.Parse "10 March 2016, 14:00:00"
        let easter = Dates.Easter d.Year
        Assert.Equal(3, easter.Month)
        Assert.Equal(27, easter.Day)

    [<Fact>]
    let ``Dates NextSunday isCorrect``() =
        let d = DateTime.Parse "10 March 2018, 14:00:00"
        let next = Dates.NextSunday d
        Assert.Equal (11, next.Day)

    [<Fact>]
    let ``Dates NextSunday myBirthday2010 isCorrect``() =
        let d = DateTime.Parse "18 February 2010, 14:00:00"
        let next = Dates.NextSunday d
        Assert.Equal (21, next.Day)
