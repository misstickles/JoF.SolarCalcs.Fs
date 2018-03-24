namespace JoF.SolarCalcs.Fs.Tests.Unit

module SunCalcsTests = 
    open Xunit
    open System
    open JoF.SolarCalcs.Fs.Library.Standard
    open JoF.SolarCalcs.Fs.Library.Standard.SunCalcs

    let au = 149597870.7

    let AssertDelta exp act delta =
        let low = exp - delta
        let high = exp + delta
        Assert.InRange(act, low, high)

    [<Fact>]
    let ``Sun RightAscensionSun myBirthday isCorrect``() =
        let d = DateTime.Parse "18 February 2017, 16:00:00"
        let jd = Dates.JulianDate2000 d
        let location = SunCalcs.FundamentalArguments jd
        AssertDelta 22.136389 (location.RightAscension) 0.001

    [<Fact>]
    let ``Sun Declination myBirthday isCorrect``() =
        let d = DateTime.Parse "18 February 2017, 16:00:00"
        let jd = Dates.JulianDate2000 d
        let location = SunCalcs.FundamentalArguments jd
        AssertDelta -11.486667 (location.Declination) 0.1

    [<Fact>]
    let ``Sun RightAscensionSun 2018March_10 isCorrect``() =
        let d = DateTime.Parse "10 March 2018, 14:00:00"
        let jd = Dates.JulianDate2000 d
        let location = SunCalcs.FundamentalArguments jd
        AssertDelta 23.368611 (location.RightAscension) 0.001

    [<Fact>]
    let ``Sun Declination 2018March_10 isCorrect``() =
        let d = DateTime.Parse "10 March 2018, 14:00:00"        
        let jd = Dates.JulianDate2000 d
        let location = SunCalcs.FundamentalArguments jd
        AssertDelta -4.080556 (location.Declination) 0.1

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



