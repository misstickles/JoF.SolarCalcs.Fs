namespace JoF.SolarCalcs.Fs.Library.Standard

module Types =

    open System

    type RiseSet = 
        {
            Rise: DateTime option
            Set: DateTime option
            Transit: DateTime option
        }
    
    type DawnDusk =
        {
            Dawn: DateTime option
            Dusk: DateTime option
            Transit: DateTime option
        }

    type BodyData =
        {
            Body: RiseSet
            Azimuth: RiseSet option
            Uptime: double
        }

    type SunData = 
        {
            // inherit BodyData

            Astronomical: DawnDusk option
            Civil: DawnDusk option
            Nautical: DawnDusk option
        }
