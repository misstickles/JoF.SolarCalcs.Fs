namespace JoF.SolarCalcs.Fs.Library.Standard

module Math = 

    open System

    let Radians = Math.PI / 180.
    let Degrees = 180. / Math.PI
    let Pi = Math.PI
    let Tpi = 2. * Pi

    let dsin x =
        sin(Radians * x)
        
    let dcos x =
        cos(Radians * x)

    let dtan x =
        tan(Radians * x)

    let frac x: Double =
        x - floor x
    
    let CheckInRange x range =
        let y = x % range
        if y < 0. then y + range else y