namespace JoF.SolarCalcs.Fs.Tests.Unit

module SunCalcsTests =
    open System
    open Xunit
    open JoF.SolarCalcs.Fs.Library.Standard

    let AssertDelta exp act delta =
        let low = exp - delta
        let high = exp + delta
        Assert.InRange(act, low, high)

    let myDate = DateTime.Parse "18 February 2017, 16:00:00"
    let lat = 51.
    let lon = 0.
    let tz = 0.

    [<Fact>]
    let ``Sun MeanLongitude myBirthday isCorrect``() =
        AssertDelta 328.8119 (SunCalcs.L myDate) 0.0001

    [<Fact>]
    let ``Sun MeanAnomoly myBirthday isCorrect``() =
        AssertDelta 6525.58 (SunCalcs.G myDate) 0.0001

    [<Fact>]
    let ``Sun OrbitalEccentricityOfEarth myBirthday isCorrect``() =
        AssertDelta 0.016701 (SunCalcs.e myDate) 0.000001

    [<Fact>]
    let ``Sun Distance myBirthday isCorrect``() =
        AssertDelta 0.988456 (SunCalcs.R myDate) 0.000001

    [<Fact>]
    let ``Sun ApparentLongitude myBirthday isCorrect``() =
        AssertDelta 330.1912 (SunCalcs.lambda myDate) 0.0001

    [<Fact>]
    let ``Sun ObliquityCorrection myBirthday isCorrect``() =
        AssertDelta 23.43477 (SunCalcs.epsilon myDate) 0.005

    [<Fact>]
    let ``Sun RightAscensionSun myBirthday isCorrect``() =
        AssertDelta -27.7288 (SunCalcs.RightAscension myDate) 0.001

    [<Fact>]
    let ``Sun Declination myBirthday isCorrect``() =
        AssertDelta -11.4026 (SunCalcs.Declination myDate) 0.01

    [<Fact>]
    let ``Sun EquationOfTime myBirthday isCorrect``() =
        AssertDelta -13.8903 (SunCalcs.E myDate) 0.01
    
    [<Fact>]
    let ``Sun SolarNoon myBirthday isCorrect``() =
        AssertDelta 0.51 (SunCalcs.SolarNoon lon (SunCalcs.E myDate) tz) 0.001

    [<Fact>]
    let ``Sun SolarNoon myBirthday_BST isCorrect``() =
        AssertDelta 0.551 (SunCalcs.SolarNoon lon (SunCalcs.E myDate) (tz + 1.0)) 0.001

    [<Fact>]
    let ``Sun HourAngleSunrise myBirthday isCorrect``() =
        AssertDelta 76.96823 (SunCalcs.HourAngleSunrise myDate lat) 0.003

    [<Fact>]
    let ``Sun Rise myBirthday isCorrect``() =
        AssertDelta 7.11028984 (SunCalcs.SunRise myDate lat lon tz) 0.01

    [<Fact>]
    let ``Sun Set myBirthday isCorrect``() =
        AssertDelta 17.362720104 (SunCalcs.SunSet myDate lat lon tz) 0.0002

    [<Fact>]
    let ``Sun TrueSolarTime myBirthday isCorrect``() =
        AssertDelta 946.1097 (SunCalcs.TrueSolarTime myDate lon tz) 0.004

    [<Fact>]
    let ``Sun HourAngle myBirthday isCorrect``() =
        AssertDelta 56.52743 (SunCalcs.HourAngle myDate lon tz) 0.001

    [<Fact>]
    let ``Sun SolarZenithAngle myBirthday isCorrect``() =
        AssertDelta 79.24559 (SunCalcs.SolarZenithAngle myDate lat lon tz) 0.002

    [<Fact>]
    let ``Sun SolarElevationAngle myBirthday isCorrect``() =
        AssertDelta 10.75441 (SunCalcs.SolarElevationAngle myDate lat lon tz) 0.002

    [<Fact>]
    let ``Sun AtmosphericRefraction myBirthday isCorrect``() =
        AssertDelta 0.082229 (SunCalcs.AtmosphericRefraction myDate lat lon tz) 0.0001

    [<Fact>]
    let ``Sun SolarElevationCorrected myBirthday isCorrect``() =
        AssertDelta 10.83664 (SunCalcs.SolarElevationCorrected myDate lat lon tz) 0.002

    [<Fact>]
    let ``Sun SolarAzimuthAngle myBirthday isCorrect``() =
        AssertDelta 236.3362 (SunCalcs.SolarAzimuthAngle myDate lat lon tz) 0.00001
