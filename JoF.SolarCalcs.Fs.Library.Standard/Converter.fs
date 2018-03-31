namespace JoF.SolarCalcs.Fs.Library.Standard

module Converter =
    open JoF.SolarCalcs.Fs.Library.Standard.Math
    open System

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

    let DecimalTimeToDate (date: DateTime) (time: double) =
        let day = floor (time / 24.)
        let t = time - day * 24.
        let h = floor t
        let m = frac t * 60.
        let s = frac m * 60.

        let d = date.AddDays(day)

        DateTime(d.Year, d.Month, d.Day, int h, int (floor m), int s)
    
    let DecimalToDate (date: DateTime) =
        let h = date.Hour
        let m = frac (float date.Minute) * 60.
        let s = frac m * 60.

        DateTime(date.Year, date.Month, date.Day, int h, int (floor m), int s)
