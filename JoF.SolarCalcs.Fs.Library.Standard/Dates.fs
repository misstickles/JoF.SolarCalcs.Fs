namespace JoF.SolarCalcs.Fs.Library.Standard

module Dates =

    open System

    let SECONDS_IN_DAY = 86400.

    type DayMonth = {
        Day: int
        Month: int
    }

    let JulianDateTime (date : DateTime) = 
        let hour = double date.Hour
        let minute = double date.Minute
        let second = double date.Second

        let a = (date.Month - 14) / 12
        let year = date.Year + 4800 + a

        // http://aa.usno.navy.mil/faq/docs/JD_Formula.php
        let jdn = date.Day - 32075 + 1461 * year / 4 + 367 * (date.Month - 2 - a * 12) / 12 - 3 * ((date.Year + 4900 + a) / 100) / 4
        let jd = double jdn + ((hour - 12.) / 24.) + (minute / 1440.) + (second / 86400.)
        jd

    let JulianDate2000 (date : DateTime) = (JulianDateTime date) - 2451545.

    let JulianCentury2000 (date : DateTime) = (JulianDate2000 date) / 36525.

    let GreenwichMeanSiderealTime (date: DateTime) =
        let jd2000 = JulianDate2000 date
        let jdc2000 = JulianCentury2000 date

        let theta0 = 280.46061837 
                    + 360.98564736629 * jd2000
                    + 0.000387933 * jdc2000 * jd2000
                    - 0.0000000258333 * jdc2000 * jdc2000 * jdc2000

        match theta0 with 
            | x when x < 0. -> theta0 % 360. + 360.
            | _ -> theta0 % 360.

    let LocalMeanSiderealTime (date: DateTime) longitude =
        (GreenwichMeanSiderealTime date) + longitude % 360.
    
    let NextSunday (date: DateTime) =
        let addDays = Math.CheckInRange (float (DayOfWeek.Sunday - date.DayOfWeek)) 7.

        date.AddDays(addDays)

    let Easter year =
        // https://en.wikipedia.org/wiki/Computus#Adaptation_for_Western_Easter_of_Meeus'_Julian_algorithm
        let a = year % 19
        let b = (11 * a + 5) % 30
        let c = if b.Equals 0 || (b.Equals 1 && a > 10) then b + 1 else b
        let month = if c >= 1 && c <= 19 then 4 else 3
        let day = (50 - c) % 31 + 1

        NextSunday (DateTime (year, month, day))


