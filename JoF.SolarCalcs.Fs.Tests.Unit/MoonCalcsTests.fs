namespace JoF.SolarCalcs.Fs.Tests.Unit

module MoonCalcsTests = 
    
    open System
    open Xunit
    open JoF.SolarCalcs.Fs.Library.Standard
    open JoF.SolarCalcs.Fs.Library.Standard.Converter

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
    let ``Moon Position March2018_8 isCorrect``() =
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
        let loc = MoonCalcs.FundamentalArguments jdz
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
    let ``Moon HorizonCoordinates March2018_10 isCorrect``() =
        // http://aa.usno.navy.mil/cgi-bin/aa_altazw.pl?form=2&body=10&year=2018&month=3&day=8&intv_mag=10&place=&lon_sign=-1&lon_deg=0&lon_min=6&lat_sign=1&lat_deg=51&lat_min=0&tz=0&tz_sign=-1
        let d = DateTime.Parse "10 March 2018, 2:20:00"
        let lat, long = 51., -0.1
        let coords = MoonCalcs.LunarHorizonCoordinates d lat long
        AssertDelta 122.6 (coords.Azimuth * Math.Degrees) 2.5
        AssertDelta 0. (coords.TrueAltitude * Math.Degrees) 2.

    [<Fact>]
    let ``Moon HorizonCoordinates May1991 isCorrect``() =
        let d = DateTime.Parse "19 May 1991, 13:00:00"
        let lat, long = 50., 10.
        let coords = MoonCalcs.LunarHorizonCoordinates d lat long
        AssertDelta 110.6 (coords.Azimuth * Math.Degrees) 0.04
        AssertDelta 35.300 (coords.TrueAltitude * Math.Degrees) 1.

    [<Fact>]
    let ``Moon ParallaxInAltitude May1991 isCorrect``() =
        let d = DateTime.Parse "19 May 1991, 13:00:00"
        let lat, long = 50., 10.
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
    let ``Moon RiseSetTimes March2018_7 areCorrect``() =
        let d = DateTime.Parse "7 March 2018, 14:00:00"
        let lat, long = 51., -0.1
        let rise, set, above = MoonCalcs.RiseSetTimes d lat long
        let utRise, utSet = DecimalToHms rise, DecimalToHms set
        AssertDelta -1. (float utRise.Hour) 0.
        AssertDelta -1. (float utRise.Minute) 5.
        AssertDelta 9. (float utSet.Hour) 0.
        AssertDelta 17. (float utSet.Minute) 5.

    [<Fact>]
    let ``Moon RiseSetTimes March2018_10 areCorrect``() =
        let d = DateTime.Parse "10 March 2018, 14:00:00"
        let lat, long = 51., -0.1
        let rise, set, above = MoonCalcs.RiseSetTimes d lat long
        let utRise, utSet = DecimalToHms rise, DecimalToHms set
        AssertDelta 2. (float utRise.Hour) 0.
        AssertDelta 20. (float utRise.Minute) 5.
        AssertDelta 11. (float utSet.Hour) 0.
        AssertDelta 2. (float utSet.Minute) 5.
        Assert.False above

    [<Fact>]
    let ``Moon RiseSetTimes Feb1998_18 areCorrect``() =
        // https://www.timeanddate.com/moon/uk/london?month=2&year=1998
        let d = DateTime.Parse "18 February 1998, 14:00:00"
        let lat, long = 51., -0.1
        let rise, set, above = MoonCalcs.RiseSetTimes d lat long
        let utRise, utSet = DecimalToHms rise, DecimalToHms set
        AssertDelta 10. (float utSet.Hour) 0.
        AssertDelta 04. (float utSet.Minute) 5.

    [<Fact>]
    let ``Moon Illumination March2018_10 isCorrect``() =
        let d = DateTime.Parse "10 March 2018, 8:00:00"
        let ill = MoonCalcs.Illumination d
        AssertDelta 0.42 ill 0.006

    [<Fact>]
    let ``Moon Illumination March2018_8 isCorrect``() =
        let d = DateTime.Parse "8 March 2018, 8:00:00"
        let ill = MoonCalcs.Illumination d
        AssertDelta 0.61 ill 0.02
       
    [<Fact>]
    let ``Moon Age myBirthday2017 isCorrect``() =
        let d = DateTime.Parse "18 February 2017, 16:00:00"
        let age = MoonCalcs.Age d
        AssertDelta 21.86 age 0.26

    [<Fact>]
    let ``Moon Age March2018_8 isCorrect``() =
        let d = DateTime.Parse "8 March 2018, 14:00:00"
        let age = MoonCalcs.Age d
        AssertDelta 21.25 age 0.26

    [<Fact>]
    let ``Moon Distance March2018_8 isCorrect``() =
        let d = DateTime.Parse "8 March 2018, 14:00:00"
        let age = MoonCalcs.Age d
        AssertDelta 405439. age 100.
