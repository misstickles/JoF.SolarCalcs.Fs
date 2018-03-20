namespace JoF.SolarCalcs.Fs.Tests

module MathTests =
    open JoF.SolarCalcs.Fs.Library.Standard.Math
    open Xunit

    [<Fact>]
    let ``Math CheckInRange360 isCorrect``() =
        let x = CheckInRange 400. 360.
        Assert.Equal(x, 40.)

    [<Fact>]
    let ``Math CheckInRangePi isCorrect``() =
        let x = CheckInRange (-2. * Pi + 2.) Pi
        Assert.Equal(x, 2.)

    [<Fact>]
    let ``Math CheckInRangeMinus40 isCorrect``() =
        let x = CheckInRange -40. 360.
        Assert.Equal(x, 320.)

