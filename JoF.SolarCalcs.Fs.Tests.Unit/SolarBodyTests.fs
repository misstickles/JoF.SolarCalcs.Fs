namespace JoF.SolarCalcs.Fs.Tests.Unit

module SolarBodyTests =
    open System
    open Xunit
    open JoF.SolarCalcs.Fs.Library.Standard.SolarBodyCalcs

    let AssertDelta exp act delta =
        let low = exp - delta
        let high = exp + delta
        Assert.InRange(act, low, high)

    let myDate = DateTime.Parse "1 January 2004, 00:00:00"

    [<Fact>]
    let ``PlanetJupiter MeanAnomoly isCorrect``() =
        let j = MeanAnomoly(myDate, Planet.Jupiter)
        AssertDelta 141.324 j 0.001

    [<Fact>]
    let ``PlanetEarth MeanAnomoly isCorrect``() =
        let e = MeanAnomoly(myDate, Planet.Earth)
        AssertDelta 357.009 e 0.001

    [<Fact>]
    let ``PlanetJupiter DistanceToSun isCorrect``() =
        let e = DistanceToSun(Planet.Jupiter, 1.)
        AssertDelta 5.40406 e 0.0001

    [<Fact>]
    let ``PlanetEarth DistanceToSun isCorrect``() =
        let e = DistanceToSun(Planet.Earth, 1.)
        AssertDelta 0.98331 e 0.00001


