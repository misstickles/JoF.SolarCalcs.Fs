namespace JoF.SolarCalcs.Fs.Tests.Unit

module SunCalcsTests = 
    open Xunit
    open System
    open JoF.SolarCalcs.Fs.Library.Standard
    open JoF.SolarCalcs.Fs.Library.Standard.SunCalcs

    let au = 149597870.7

    // TODO: RA tests have wrong actual

    let AssertDelta exp act delta =
        let low = exp - delta
        let high = exp + delta
        Assert.InRange(act, low, high)

    [<Fact>]
    let ``Sun RightAscensionSun myBirthday isCorrect``() =
        let d = DateTime.Parse "18 February 2017, 16:00:00"
        let jd = Dates.JulianDate2000 d
        let location = SunCalcs.FundamentalArguments jd
        AssertDelta -0.203107 (location.RightAscension) 0.001

    [<Fact>]
    let ``Sun Declination myBirthday isCorrect``() =
        let d = DateTime.Parse "18 February 2017, 16:00:00"
        let jd = Dates.JulianDate2000 d
        let location = SunCalcs.FundamentalArguments jd
        AssertDelta -11.486667 (location.Declination * Math.Degrees) 0.1

    [<Fact>]
    let ``Sun RightAscensionSun 2018March_10 isCorrect``() =
        let d = DateTime.Parse "10 March 2018, 14:00:00"
        let jd = Dates.JulianDate2000 d
        let location = SunCalcs.FundamentalArguments jd
        AssertDelta 23.368611 (location.RightAscension) 0.001

    [<Fact>]
    let ``Sun Declination 2018March_29 isCorrect``() =
        let d = DateTime.Parse "29 March 2018, 00:00:00"        
        let jd = Dates.JulianDate2000 d
        let location = SunCalcs.FundamentalArguments jd
        AssertDelta 0.05722546 (location.Declination) 0.001

    [<Fact>]
    let ``Sun Distance myBirthday isCorrect``() =
        // 147.868 mil km
        let d = DateTime.Parse "18 February 2017, 16:00:00"
        let jd = Dates.JulianDate2000 d
        let location = FundamentalArguments jd
        AssertDelta 147868000. (location.Distance * au) 2000000.

    [<Fact>]
    let ``Sun Distance 2018March_10 isCorrect``() =
        // 147.868 mil km
        let d = DateTime.Parse "10 March 2018, 14:00:00"
        let jd = Dates.JulianDate2000 d
        let location = FundamentalArguments jd
        AssertDelta 148576000. (location.Distance * au) 2000000.

    [<Fact>]
    let ``Sun RiseSetTimes 2018March_29 isCorrect``() =
        let d = DateTime.Parse "29 March 2018, 00:00:00"
        let times = SunCalcs.RiseSetTimes d 51. -0.1 -0.566667
        let rise = Converter.DecimalToHms times.RiseTime
        AssertDelta 5. (float rise.Hour) 0.
        AssertDelta 43. (float rise.Minute) 5.

    [<Fact>]
    let ``Sun RiseSetTimes 2018March_10 isCorrect``() =
        let d = DateTime.Parse "10 March 2018, 14:00:00"
        let times = SunCalcs.RiseSetTimes d 51. -0.1 -0.566667
        let rise = Converter.DecimalToHms times.RiseTime
        let set = Converter.DecimalToHms times.SetTime
        AssertDelta 6. (float rise.Hour) 0.
        AssertDelta 25. (float rise.Minute) 5.
        AssertDelta 17. (float set.Hour) 0.
        AssertDelta 56. (float set.Minute) 5.

    [<Fact>]
    let ``Sun SunData 2018March_29 isCorrect``() =
        let d = DateTime.Parse "29 March 2018, 9:12:31"
        let times = SunCalcs.SunData d 51. -0.1
        let rise = Converter.DecimalToHms times.Rise.RiseTime
        let set = Converter.DecimalToHms times.Rise.SetTime
        AssertDelta 5. (float rise.Hour) 0.
        AssertDelta 43. (float rise.Minute) 5.
        AssertDelta 18. (float set.Hour) 0.
        AssertDelta 28. (float set.Minute) 5.
