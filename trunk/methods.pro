
;******************************************************************
;   methods.pro

;   Copyright (c) 2001-2007, Scott D. Peckham
;   Created:   Dec 2001 - Jan 2002
;   Modified:  Jan 2003, Feb 2004, Jul 2005
;   Modified:  Jan 2006  (Richards Eqn method)
;   Modified:  Feb 2007  (Diffusive Wave method)

;   Notes:     Later have these read from text config files.

;******************************************************************

;   Stop_Methods      (function)
;   Snow_Methods      (function)
;   ET_Methods        (function)
;   Precip_Methods    (function)
;   Infil_Methods     (function)
;   GW_Methods        (function)
;   Overland_Methods  (function)
;   Channel_Methods   (function)
;   Sediment_Methods  (function)
;   Diversion_Methods (function, Feb 2004, Jul 2005)

;******************************************************************
function Stop_Methods 
 
stop_methods = ['Run until Q drops to P% of Q_peak', $
                'Run for a specified time (model)', $
                'Run for a specified number of steps' ]
                ;** 'Run for a specified time (real)']
                ;** 'Run until Q reaches a given value']
npad = 2
stop_methods = Str_Pad(stop_methods, npad)

RETURN, stop_methods
END;    Stop_Methods
;******************************************************************
function Snow_Methods

snow_methods = ['None', 'Degree-Day', 'Energy Balance']
npad = 2
snow_methods = Str_Pad(snow_methods, npad)

RETURN, snow_methods
END;    Snow_Methods
;******************************************************************
function ET_Methods

ET_methods = ['None', 'Priestley-Taylor', 'Energy Balance']
npad = 2
ET_methods = Str_Pad(ET_methods, npad)

RETURN, ET_methods
END;    ET_Methods
;******************************************************************
function Precip_Methods

precip_methods = ['None', $
                  'Uniform in space, given durations', $
                  'General data types']

                  ;*** 'Grid sequence file, fixed timestep', $
                  ;*** 'Grid sequence file, given durations']
npad = 2
precip_methods = Str_Pad(precip_methods, npad)

RETURN, Precip_methods
END;    Precip_Methods
;******************************************************************
function Infil_Methods

infil_methods = ['None', $
                 '100% infiltration until (h >= z)',$
                 'Simple Green-Ampt, one event', $
                 'Smith-Parlange 3-param., one event', $
                 'Richards 1D Equation, 6 layers']
                 ;**** 'Beven Exponential K, single event']  ;(no GUI yet)
npad = 2
infil_methods = Str_Pad(infil_methods, npad)

RETURN, Infil_methods
END;    Infil_Methods
;******************************************************************
function GW_Methods

GW_methods = ["None", $
              "Darcy's Law, Surface-parallel layers"]
              ;*** "Darcy's Law, Uniform layers", $
              ;*** "Darcy's Law, Variable K"]
npad = 2
GW_methods = Str_Pad(GW_methods, npad)

RETURN, GW_methods
END;    GW_Methods
;******************************************************************
function Overland_Methods

overland_methods = ['None', 'Kinematic Wave, Manning friction']
             ;**** 'Kinematic Wave, Law of Wall friction']
npad = 2
overland_methods = Str_Pad(overland_methods, npad)

RETURN, overland_methods
END;    Overland_Methods
;******************************************************************
function Channel_Methods

channel_methods = ['None', $
                   'Kinematic Wave, Manning friction', $
                   'Kinematic Wave, Law of Wall friction', $
                   'Diffusive Wave, Manning friction', $
                   'Diffusive Wave, Law of Wall friction', $
                   'Dynamic Wave, Manning friction', $
                   'Dynamic Wave, Law of Wall friction']
npad = 2
channel_methods = Str_Pad(channel_methods, npad)

RETURN, channel_methods
END;    Channel_Methods
;******************************************************************
function Sediment_Methods

sediment_methods = ['None', 'Slope-Area Power Law']
npad = 2
sediment_methods = Str_Pad(sediment_methods, npad)

RETURN, sediment_methods
END;    Sediment_Methods
;******************************************************************
function Diversion_Methods

diversion_methods = ['None', $
                     'Sources, Sinks or Canals' ]
npad = 2
diversion_methods = Str_Pad(diversion_methods, npad)

RETURN, diversion_methods
END;    Diversion_Methods
;******************************************************************

