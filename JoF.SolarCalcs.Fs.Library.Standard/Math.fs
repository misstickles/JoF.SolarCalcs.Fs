namespace JoF.SolarCalcs.Fs.Library.Standard

module Math = 

    open System

    type Point = {
        X: double
        Y: double
    }

    type QuadEvent = {
        Events: int
        MinMax: Point
        Z1: double
        Z2: double
    }

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

    let Quadratic ym yz yp = 
        // max/min point = xe, xy
        // ym yz yp = y points
        // z1 z2 = x where parabola crosses zero (roots of quadratic)
        // nz = number of roots (0, 1, 2) within interval [-1,1]
        let nz = 0;
        let a = 0.5 * (ym + yp) - yz;
        let b = 0.5 * (yp - ym);
        let c = yz;
        let xe = -b / (2. * a);
        let ye = (a * xe + b) * xe + c;
        let dis = b * b - 4. * a * c;
        if dis > 0. then
            let dx = 0.5 * sqrt dis / abs a
            let z1 = xe - dx
            let z2 = xe + dx
            let nz = if abs z1 <= 1.0 then nz + 1 else nz
            let nz = if abs z2 <= 1.0 then nz + 1 else nz
            let z1 = if z1 < -1. then z2 else z1
            { Events = nz; MinMax = { X = xe; Y = ye }; Z1 = z1; Z2 = z2 }
        else
            { Events = 0; MinMax = { X = xe; Y = ye }; Z1 = 0.; Z2 = 0. }

