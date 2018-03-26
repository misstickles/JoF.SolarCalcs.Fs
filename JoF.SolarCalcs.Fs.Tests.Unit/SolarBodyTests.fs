namespace JoF.SolarCalcs.Fs.Tests.Unit

module SolarBodyTests =
    open System
    open Xunit
    open JoF.SolarCalcs.Fs.Library.Standard.Converter
    open JoF.SolarCalcs.Fs.Library.Standard.SolarBodyCalcs

    let AssertDelta exp act delta =
        let low = exp - delta
        let high = exp + delta
        Assert.InRange(act, low, high)

    let myDate = DateTime.Parse "1 January 2004, 00:00:00"

    [<Fact>]
    let ``Planet MeanAnomolyJupiter isCorrect``() =
        let m = MeanAnomoly myDate Planet.Jupiter
        AssertDelta 141.324 m 0.001

    [<Fact>]
    let ``Planet MeanAnomolyEarth isCorrect``() =
        let m = MeanAnomoly myDate Planet.Earth
        AssertDelta 357.009 m 0.001

    [<Fact>]
    let ``Planet TrueAnomolyJupiter isCorrect``() =
        let v = TrueAnomoly myDate Planet.Jupiter
        AssertDelta 144.637 v 0.001

    [<Fact>]
    let ``Planet TrueAnomolyEarth isCorrect``() =
        let v = TrueAnomoly myDate Planet.Earth
        AssertDelta 356.907 v 0.001

    [<Fact>]
    let ``Planet DistanceToSunJupiter isCorrect``() =
        let e = DistanceToSun myDate Planet.Jupiter
        AssertDelta 5.40406 e 0.0001

    [<Fact>]
    let ``Planet DistanceToSunEarth isCorrect``() =
        let e = DistanceToSun myDate Planet.Earth
        AssertDelta 0.98331 e 0.00001
     
    [<Fact>]
    let ``Planet HeliocentricEclipticCoordsJupiter isCorrect``() =
        let coords = HeliocentricEclipticCoords myDate Planet.Jupiter
        AssertDelta -5.04289 coords.X 0.001
        AssertDelta 1.93965 coords.Y 0.001
        AssertDelta 0.10478 coords.Z 0.001

    [<Fact>]
    let ``Planet HeliocentricEclipticCoordsEarth isCorrect``() =
        let coords = HeliocentricEclipticCoords myDate Planet.Earth
        AssertDelta -0.16811 coords.X 0.001
        AssertDelta 0.96884 coords.Y 0.001
        AssertDelta 0. coords.Z 0.001

    [<Fact>]
    let ``Planet GeocentricEclipticCoordsJupiter isCorrect``() =
        let coords = GeocentricEclipticalCoords myDate Planet.Jupiter
        AssertDelta -4.87477 coords.X 0.001
        AssertDelta 0.97081 coords.Y 0.001
        AssertDelta 0.10478 coords.Z 0.001

    [<Fact>]
    let ``Planet GeocentricEclipticPositionJupiter isCorrect``() =
        let position = GeocentricEclipticalPosition myDate Planet.Jupiter
        AssertDelta 4.97161 position.Distance 0.001
        AssertDelta 168.737 position.Longitude 0.001
        AssertDelta 1.208 position.Latitude 0.001

    [<Fact>]
    let ``Planet EquatorialCoordinatesJupiter isCorrect``() =
        let position = EquatorialCoordinates myDate Planet.Jupiter
        AssertDelta 170.120 position.RightAscension 0.001 // 11h 20m 29s
        AssertDelta 5.567 position.Declination 0.001

    [<Fact>]
    let ``Planet HourAngleJupiter isCorrect``() =
        let ha = HourAngle myDate Planet.Jupiter -5.
        AssertDelta -65.174 ha 0.001   // -4h 20m 42s

    [<Fact>]
    let ``Planet HourAngleVenus March2018_8 isCorrect``() =
        let ha = HourAngle myDate Planet.Venus -0.1
        AssertDelta 13. ha 0.001

    [<Fact>]
    let ``Planet CelestialPositionJupiter isCorrect``() =
        let position = CelestialPosition myDate Planet.Jupiter 51. -5.
        AssertDelta 19.500 position.Altitude 0.001
        AssertDelta -73.383 position.Azimuth 0.001

    [<Fact>]
    let ``Planet CelestialPositionVenus March2018_8 isCorrect``() =
        let d = DateTime.Parse "8 March 2018, 7:03:00"
        let position = CelestialPosition d Planet.Venus 51. -5.
        //AssertDelta 19.500 position.Altitude 0.001
        AssertDelta 38.7 position.Azimuth 0.1

    [<Fact>]
    let ``Planet RefractionJupiter isCorrect``() =
        let ref = Refraction (CelestialPosition myDate Planet.Jupiter 51. -5.).Altitude
        AssertDelta 0.05 ref 0.017

    [<Fact>]
    let ``Planet TransitJupiter isCorrect``() =
        let transit = Transit myDate Planet.Jupiter -5.
        let t = DecimalToHms transit
        AssertDelta 4. (float t.Hour) 0.
        AssertDelta 20. (float t.Minute) 5.

    [<Fact>]
    let ``Planet TransitMercury March2018_8 isCorrect``() =
        let d = DateTime.Parse "8 March 2018, 16:00:00"
        let transit = Transit d Planet.Mercury -0.1
        let t = DecimalToHms transit
        AssertDelta 13. (float t.Hour) 0.
        AssertDelta 9. (float t.Minute) 5.

    [<Fact>]
    let ``Planet RistSetJupiter isCorrect``() =
        let times = RiseSetTimes myDate Planet.Jupiter 52. -5.
        let r = DecimalToHms times.Rise
        let s = DecimalToHms times.Set
        AssertDelta 21. (float r.Hour) 0.
        AssertDelta 52. (float r.Minute) 5.
        AssertDelta 10. (float s.Hour) 0.
        AssertDelta 49. (float s.Minute) 5.

    [<Fact>]
    let ``Planet RistSetVenus March2018_8 isCorrect``() =
        let d = DateTime.Parse "8 March 2018, 16:00:00"
        let times = RiseSetTimes d Planet.Venus 51. -0.1
        let r = DecimalToHms times.Rise
        let s = DecimalToHms times.Set
        AssertDelta 7. (float r.Hour) 0.
        AssertDelta 3. (float r.Minute) 5.
        AssertDelta 19. (float s.Hour) 0.
        AssertDelta 07. (float s.Minute) 5.

    [<Fact>]
    let ``Planet RistSetMercury March2018_8 isCorrect``() =
        let d = DateTime.Parse "8 March 2018, 16:00:00"
        let times = RiseSetTimes d Planet.Mercury 51. -0.1
        let r = DecimalToHms times.Rise
        let s = DecimalToHms times.Set
        AssertDelta 6. (float r.Hour) 0.
        AssertDelta 55. (float r.Minute) 5.
        AssertDelta 19. (float s.Hour) 0.
        AssertDelta 23. (float s.Minute) 5.

    [<Fact>]
    let ``Planet RistSetVenus MyBirthday2017 isCorrect``() =
        // https://www.timeanddate.com/astronomy/night/
        let d = DateTime.Parse "18 February 2017, 16:00:00"
        let times = RiseSetTimes d Planet.Venus 51. -0.1
        let r = DecimalToHms times.Rise
        let s = DecimalToHms times.Set
        AssertDelta 7. (float r.Hour) 0.
        AssertDelta 51. (float r.Minute) 5.
        AssertDelta 21. (float s.Hour) 0.
        AssertDelta 17. (float s.Minute) 5.


