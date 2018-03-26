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

    let datan2 x y =
        atan2 (x * Radians) (y * Radians)

    let dasin x =
        asin (x * Radians)

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

    let Kepler eccentricity meanAnomoly =
        let maxIterations, epsilon = 10, 0.00001

        let e = meanAnomoly * Radians
        let m = if eccentricity < 0.8 then e else Pi

        let rec kepl found iter ecc m e0 =
            if found || iter > maxIterations then
                e0
            else
                // TODO: !!!! find a delta to compare!!
                let e1 = e0 + ((m + ecc * sin e0 - e0) / (1. - ecc * cos e0))
                kepl false (iter + 1) ecc m e1

        kepl false 0 eccentricity m e

        //let rec kepl found iter ecc m e0 =
        //    if found || iter > maxIterations then 
        //        e0
        //    else
        //        let delta = e0 - ecc * sin m - m
        //        let e1 = (cos e0 - e) / (1. - ecc * cos e0)
        //        kepl (abs delta <= epsilon) (iter + 1) ecc m e1

        //kepl false 0 eccentricity m e

//let m = meanAnomoly / 360.
        //let m = 2. * Pi * (m - floor m)

        //let e0 = if eccentricity < 0.8 then m else Pi
        //let e1 = e0 - eccentricity * sin m - m

        //let rec kep found iter ecc m e0 e1 =
        //    if found || (iter > maxIterations) then e0
        //    else
        //        let e2 = e0 - (e1 / (1. - eccentricity * cos e0))
        //        let e3 = e2 - eccentricity * sin e0 - m
        //        kep (abs e3 <= delta) (iter + 1) ecc m e2 e3
        
        //kep false 0 eccentricity m e0 e1