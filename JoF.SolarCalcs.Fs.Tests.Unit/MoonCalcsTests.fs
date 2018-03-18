namespace JoF.SolarCalcs.Fs.Tests.Unit

module MoonCalcsTests = 
    
    open System
    open Xunit
    open JoF.SolarCalcs.Fs.Library.Standard

    let myDate = DateTime.Parse "18 February 2017, 16:00:00"

    let AssertDelta exp act delta =
        let low = exp - delta
        let high = exp + delta
        Assert.InRange(act, low, high)

    [<Fact>]
    let ``Moon Position OneJan2004 IsCorrect``() =
        let d = DateTime.Parse "1 January 2004, 00:00:00"
        let coords = MoonCalcs.GeocentricEclipticCoords d
        AssertDelta 26.78 coords.lambda 0.005
        AssertDelta -2.19 coords.beta 0.2
        AssertDelta 400136. coords.delta 0.2

    [<Fact>]
    let ``Moon Position``() =
        // https://www.satellite-calculations.com/Satellite/suncalc.htm
        let d = DateTime.Parse "8 March 2018, 14:00:00"
        let coords = MoonCalcs.GeocentricEclipticCoords d
        AssertDelta 248.143 coords.lambda 0.3
        AssertDelta 4.787 coords.beta 0.1
        AssertDelta 400233.798 coords.delta 1000.

    [<Fact>]
    let ``Moon Location May1991 isCorrect``() =
        let d = DateTime.Parse "19 May 1991, 13:00:00"
        let jdz = Dates.JulianDate2000 d
        let loc = MoonCalcs.Location jdz |> Seq.item 0
        let ra, dec, p = loc.RightAscension, loc.Declination, loc.Parallax
        AssertDelta 133.44 (ra * Math.Degrees) 0.2
        AssertDelta 15.53 (dec * Math.Degrees) 0.2
        //AssertDelta 0.8 p 1.0
     
    [<Fact>]
    let ``Moon LHA May1991 isCorrect``() =
        let d = DateTime.Parse "19 May 1991, 13:00:00"
        let lha = MoonCalcs.LocalHourAngle d 10.
        AssertDelta 308.25 lha 0.04

    [<Fact>]
    let ``Moon HorizonCoordinates March2018 isCorrect``() =
        // http://aa.usno.navy.mil/cgi-bin/aa_altazw.pl?form=2&body=10&year=2018&month=3&day=8&intv_mag=10&place=&lon_sign=-1&lon_deg=0&lon_min=6&lat_sign=1&lat_deg=51&lat_min=0&tz=0&tz_sign=-1
        let d = DateTime.Parse "8 March 2018, 14:00:00"
        let lat, long = 51., -0.1
        let coords = MoonCalcs.LunarHorizonCoordinates d lat long
        AssertDelta 29.6 (coords.TrueAltitude * Math.Degrees) 0.001
        AssertDelta 211.6 (coords.Azimuth * Math.Degrees) 0.04

    [<Fact>]
    let ``Moon HorizonCoordinates May1991 isCorrect``() =
        let d = DateTime.Parse "19 May 1991, 13:00:00"
        let lat, long = 50., 10.
        let coords = MoonCalcs.LunarHorizonCoordinates d lat long
        AssertDelta 36.100 (coords.TrueAltitude * Math.Degrees) 0.001
        AssertDelta 110.6 (coords.Azimuth * Math.Degrees) 0.04

    [<Fact>]
    let ``Moon ParallaxInAltitude May1991 isCorrect``() =
        let d = DateTime.Parse "19 May 1991, 13:00:00"
        let lat, long = 10., 50.
        let coords = MoonCalcs.LunarHorizonCoordinates d lat long
        let parallax = MoonCalcs.ParallaxInAltitude d coords.TrueAltitude
        AssertDelta 0.8 parallax 1.
    
    [<Fact>]
    let ``Moon Refraction May1991 isCorrect``() =
        let d = DateTime.Parse "19 May 1991, 13:00:00"
        let lat, long = 10., 50.
        let coords = MoonCalcs.LunarHorizonCoordinates d lat long
        let r = MoonCalcs.Refraction coords.TrueAltitude
        AssertDelta 0.0004 r 0.0001
    
    [<Fact>]
    let ``Moon RiseSetTimes March2018_8 areCorrect``() =
        let d = DateTime.Parse "8 March 2018, 14:00:00"
        let lat, long = 51., -0.1
        let utrise, utset, rise, sett, above = MoonCalcs.RiseSetTimes d lat long
        AssertDelta 0. (float utrise.Hour) 0.
        AssertDelta 18. (float utrise.Minute) 5.
        AssertDelta 9. (float utset.Hour) 0.
        AssertDelta 50. (float utset.Minute) 5.

    [<Fact>]
    let ``Moon RiseSetTimes March2018_10 areCorrect``() =
        let d = DateTime.Parse "10 March 2018, 14:00:00"
        let lat, long = 51., -0.1
        let utrise, utset, rise, sett, above = MoonCalcs.RiseSetTimes d lat long
        AssertDelta 2. (float utrise.Hour) 0.
        AssertDelta 17. (float utrise.Minute) 5.
        AssertDelta 11. (float utset.Hour) 0.
        AssertDelta 5. (float utset.Minute) 5.

    [<Fact>]
    let ``Moon RiseSetTimes Feb1998_18 areCorrect``() =
        // https://www.timeanddate.com/moon/uk/london?month=2&year=1998
        let d = DateTime.Parse "18 February 1998, 14:00:00"
        let lat, long = 51., -0.1
        let utrise, utset, rise, sett, above = MoonCalcs.RiseSetTimes d lat long
        Assert.Equal(false, rise)
        AssertDelta 10. (float utset.Hour) 0.
        AssertDelta 04. (float utset.Minute) 5.
