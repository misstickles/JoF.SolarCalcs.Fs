namespace JoF.SolarCalcs.Fs.Library.Standard

module Converter =
    open JoF.SolarCalcs.Fs.Library.Standard.Math

    type HMS = {
        Hour: int
        Minute: int
        Second: double
        Days: double
    }

    let DegreesToHms d =
        let day = floor (d / 360.)
        let h = floor(d / 15.)
        let m = floor((d / 15. - h) * 60.)
        let s = (((d / 15. - h) * 60.) - m) * 60.

        { Hour = int h; Minute = int m; Second = s; Days = day}
    
    let DecimalToHms d =
        let day = floor (d / 24.)
        let h = floor d
        let m = frac d * 60.
        let s = frac m * 3600.

        { Hour = int h; Minute = int (floor m); Second = s; Days = day }
