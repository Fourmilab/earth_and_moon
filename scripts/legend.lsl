/*
                      Fourmilab Earth and Moon

                           by John Walker
                      https://www.fourmilab.ch/
                       fourmilab in Second Life

                    Generate floating text legend
                    and assorted utility services

        This is broken out into a separate script to evade the
        64 Kb memory limit on a single script.

        This program is licensed under a Creative Commons
        Attribution-ShareAlike 4.0 International License.
            http://creativecommons.org/licenses/by-sa/4.0/
        Please see the License section in the "Fourmilab
        Earth and Moon Help" notecard included in the object
        for details.
    */

    //  Link message command codes

//  integer LM_INIT = 0;                // Initialise internal state
    integer LM_EARTH_LEGEND = 1;        // Update Earth legend
    integer LM_MOON_LEGEND = 2;         // Update Moon legend
    integer LM_STAT = 3;                // Print status
    integer LM_MESSAGE = 4;             // Print canned message
    integer LM_DIALOGUE = 5;            // Display dialogue
    integer LM_LEGEND_VIS = 6;          // Set legend visibility
//  integer LM_UPDATE_MODEL = 7;        // Update Earth and Moon model
//  integer LM_STAT_MODEL = 8;          // Request status from Update Model
    integer LM_RESET = 9;               // Reset scripts

    //  Link indices for child prims of linked object set

//  integer BASE = 1;
//  integer DAY_SIDE = 2;
    integer NIGHT_SIDE = 3;
//  integer MOON_GLOBE = 4;
    integer MOON_NIGHT = 5;

    float JulianCentury = 36525.0;      // Days in Julian century
    float SynMonth = 29.53058868;       // Synodic month (mean time from new Moon to new Moon)
    float MoonSemiMaj = 384401.0;       // Semi-major axis of Moon's orbit
    float moonSubtends = 0.5181;        // Moon's angular size at MoonSemiMaj distance from Earth
    float EarthRad = 6378.14;           // Earth's equatorial radius, km (IAU 1976)

    //  Special characters
    string U_deg;                       // U+00B0 Degree Sign
    string U_white_circle;              // U+25CB White Circle
    string U_black_circle;              // U+25CF Black Circle
    string U_left_half_black_circle;    // U+25D6 Left Half Black Circle
    string U_right_half_black_circle;   // U+25D7 Right Half Black Circle

    //  Cache for previously computed Moon phases
    list phaseJD = [];                  // Julian day of phases
    string phaseDates = "";             // Primate readable phase table

    //  State for legend update rate limiting
    float LegendMinInterval = 0.5;
    float moonLegendTimeLast = 0;
    float earthLegendTimeLast = 0;

    //  FIXANGLE  --  Range reduce an angle in degrees

    float fixangle(float a) {
        return a - (360.0 * llFloor(a / 360.0));
    }

    /*  JYEARL  --  Convert Julian date/time list to year, month, day,
                    which are returned as a list.  */

    list jyearl(list tdl) {
        float a;
        float alpha;
        integer yy;
        integer mm;

        float td = llList2Float(tdl, 0);
        float tdf = llList2Float(tdl, 1);

        tdf += 0.5;                     // Adjust for Julian date changes at noon
        if (tdf > 1.0) {                // If this advanced day
            td++;                       //    then bump the integral day
            tdf -= 1;                   //    and decrement fractional day
        }

        float z = td;
        float f = tdf;

        if (z < 2299161.0) {
            a = z;
        } else {
            alpha = llFloor((z - 1867216.25) / 36524.25);
            a = z + 1 + alpha - llFloor(alpha / 4);
        }

        float b = a + 1524;
        float c = llFloor((b - 122.1) / 365.25);
        float d = llFloor(365.25 * c);
        float e = llFloor((b - d) / 30.6001);

        if (e < 14) {
            mm = (integer) (e - 1);
        } else {
            mm = (integer) (e - 13);
        }

        if (mm > 2) {
            yy = (integer) (c - 4716);
        } else {
            yy = (integer) (c - 4715);
        }

        return [
                 yy,                                            // year
                 mm,                                            // month
                 (integer) (b - d - llFloor(30.6001 * e) + f)   // day
               ];
    }

    /*  JHMS  --  Convert Julian time to hour, minutes, and seconds,
                  returned as a list.  */

    list jhms(float j) {
        j += 0.5;                 // Astronomical to civil
        integer ij = (integer) ((j - llFloor(j)) * 86400.0);
        return [
                    (ij / 3600),        // hours
                    ((ij / 60) % 60),   // minutes
                    (ij % 60)           // seconds
               ];
    }

    /*  JDCOMP  --  Compare two Julian day and fraction pairs.
                    Returns -1 if a < b, 0 if a == b, 1 if a > b.  */

    integer jdcomp(float jda, float jdfa, float jdb, float jdfb) {
        integer result = 0;
        if (jda == jdb) {
            if (jdfa < jdfb) {
                result = -1;
            } else {
                result = 1;
            }
        } else if (jda < jdb) {
            result = -1;
        } else {
            result = 1;
        }
        return result;
    }

    /*  MEANPHASE  --  Calculates time of the mean new Moon for a given
                       base date.  The argument k to this function is the
                       precomputed synodic month index, given by:

                          k = (year - 1900) * 12.3685

                       where year is expressed as a year and fractional year.  */

    list meanphase(float sdate, float sdatef, float k) {
        // Time in Julian centuries from 1900 January 0.5
        float t = ((sdate - 2415020.0) + sdatef) / JulianCentury;
        float t2 = t * t;               // Square
        float t3 = t2 * t;              // Cube

        // All of this faffing about is to avoid single-precision loss of accuracy
        float nt1k = 0.75933 + (SynMonth * k);
        float nt1w = llFloor(nt1k);
        float nt1f = (nt1k - nt1w) + (0.0001178 * t2)
                                   - (0.000000155 * t3)
                                   + (0.00033 * llSin((166.56 + (132.87 * t) -
                                        (0.009173 * t2)) * DEG_TO_RAD));
        nt1w += 2415020.0;

        return [ nt1w, nt1f ];
    }

    /*  TRUEPHASE  --  Given a K value used to determine the mean phase of
                       the new Moon, and a phase selector (0.0, 0.25, 0.5,
                       0.75), obtain the true, corrected phase time.

                       When computing this in double precision, this is all very
                       straightforward.  It is only when confronted with LSL
                       bronze age single precision floating point that the
                       algorithm explodes into a fractal universe of cruft.
                       Faced with, at most, nine decimal places of precision
                       (and that's on a nice day, with the wind blowing in
                       the right direction), we must separate the whole and
                       fractional parts of the expression and carefully evaluate
                       the fractional part to avoid loss of precision, then
                       propagate any whole part of the fraction after applying all
                       of the corrections back to the whole part.  Positional
                       astronomy is not this complicated; what you're seeing are
                       the (unnecessary and obsolete) limitations of Second Life's
                       scripting language.  */

    list truephase(float k, float phase) {
        k += phase;                     // Add phase to new moon time
        float t = k / 1236.85;          /* Time in Julian centuries from
                                           1900 January 0.5 */
        float t2 = t * t;               // Square for frequent use
        float t3 = t2 * t;              // Cube for frequent use
        float ptwhole = 2415020.0;      // Whole part of JD base
        float nSynMon = SynMonth * k;   // Synodic months in days
        if (llFabs(nSynMon) >= 1) {
            ptwhole += llFloor(nSynMon);
            nSynMon -= llFloor(nSynMon);
        }

        float pt = 0.75933              // Mean time of phase
             + nSynMon
             + 0.0001178 * t2
             - 0.000000155 * t3
             + 0.00033 * llSin((166.56 + 132.87 * t - 0.009173 * t2) * DEG_TO_RAD);

        float m = fixangle(359.2242              // Sun's mean anomaly
            + fixangle(29.10535608 * k)
            - 0.0000333 * t2
            - 0.00000347 * t3);
        float mprime = fixangle(306.0253         // Moon's mean anomaly
            + fixangle(385.81691806 * k)
            + 0.0107306 * t2
            + 0.00001236 * t3);
        float f = fixangle(21.2964               // Moon's argument of latitude
            + fixangle(390.67050646 * k)
            - 0.0016528 * t2
            - 0.00000239 * t3);

        m *= DEG_TO_RAD;
        mprime *= DEG_TO_RAD;
        f *= DEG_TO_RAD;

        if ((phase < 0.01) || (llFabs(phase - 0.5) < 0.01)) {

           // Corrections for New and Full Moon

           pt +=     (0.1734 - 0.000393 * t) * llSin(m)
                    + 0.0021 * llSin(2 * m)
                    - 0.4068 * llSin(mprime)
                    + 0.0161 * llSin(2 * mprime)
                    - 0.0004 * llSin(3 * mprime)
                    + 0.0104 * llSin(2 * f)
                    - 0.0051 * llSin(m + mprime)
                    - 0.0074 * llSin(m - mprime)
                    + 0.0004 * llSin(2 * f + m)
                    - 0.0004 * llSin(2 * f - m)
                    - 0.0006 * llSin(2 * f + mprime)
                    + 0.0010 * llSin(2 * f - mprime)
                    + 0.0005 * llSin(m + 2 * mprime);
        } else if ((llFabs(phase - 0.25) < 0.01 || (llFabs(phase - 0.75) < 0.01))) {
           pt +=     (0.1721 - 0.0004 * t) * llSin(m)
                    + 0.0021 * llSin(2 * m)
                    - 0.6280 * llSin(mprime)
                    + 0.0089 * llSin(2 * mprime)
                    - 0.0004 * llSin(3 * mprime)
                    + 0.0079 * llSin(2 * f)
                    - 0.0119 * llSin(m + mprime)
                    - 0.0047 * llSin(m - mprime)
                    + 0.0003 * llSin(2 * f + m)
                    - 0.0004 * llSin(2 * f - m)
                    - 0.0006 * llSin(2 * f + mprime)
                    + 0.0021 * llSin(2 * f - mprime)
                    + 0.0003 * llSin(m + 2 * mprime)
                    + 0.0004 * llSin(m - 2 * mprime)
                    - 0.0003 * llSin(2 * m + mprime);
           if (phase < 0.5) {
                // First quarter correction
                pt += 0.0028 - 0.0004 * llCos(m) + 0.0003 * llCos(mprime);
            } else {
                // Last quarter correction
                pt += -0.0028 + 0.0004 * llCos(m) - 0.0003 * llCos(mprime);
            }
        }

        if (llFabs(pt) >= 1) {
            ptwhole += llFloor(pt);
            pt -= llFloor(pt);
        }
        return [ ptwhole, pt ];
    }

    /*   PHASEHUNT  --  Find time of phases of the Moon which surround the
                        current date.  Five phases are found, starting and
                        ending with the new Moons which bound the current
                        lunation.  */

    list phasehunt(float sdate, float sdatef) {
        float adate = sdate - 45;

        list ymd = jyearl([ sdate + sdatef ]);
        float k1 = llFloor((llList2Float(ymd, 0) +
            ((llList2Float(ymd, 1) - 1) * (1.0 / 12.0)) - 1900) * 12.3685);

        list nt1 = meanphase(adate, sdatef, k1);
        adate = llList2Float(nt1, 0);
        float adatef = llList2Float(nt1, 1);

        /*  What's this all about, you ask?  (Or, if you have a sense
            of self-preservation and value your sanity, you don't.)
            Well, we want to show a table of the dates and times of
            the phases of the Moon around the current date.  In order
            to do this, given that the functions we have available
            provide precisely the inverse information of what we need,
            we jump back a synodic month in time from the current
            date, then step forward, computing the nearest full Moons
            before and after that date, until we find those which bracket
            it.  Now we know the lunation we're in, and we can directly
            compute the dates of the enclosing new Moons and the quarter
            and full Moon of the lunation.  */

        integer hunting = TRUE;
        while (hunting) {
            adate += SynMonth;
            float k2 = k1 + 1;
            list nt2 = meanphase(adate, adatef, k2);
            list tp1 = truephase(k1, 0.0);
            list tp2 = truephase(k2, 0.0);
            /*  Welcome to gnarly multiple-precision tests due to the
                need to break up Julian dates and fractions into two
                floats to work around LSL single precision.  */
            if (((llList2Float(tp1, 0) < sdate) ||
                ((llList2Float(tp1, 0) == sdate) &&
                 (llList2Float(tp1, 1) <= sdatef)))
                &&
                ((llList2Float(tp2, 0) > sdate) ||
                ((llList2Float(tp2, 0) == sdate) &&
                 (llList2Float(tp2, 1) > sdatef)))) {
                hunting = FALSE;
            } else {
                nt1 = nt2;
                k1 = k2;
            }
        }

        /*  Return dates of last new Moon, first quarter, full,
            third quarter, and next new Moon.  */

        return truephase(k1, 0.0) +
               truephase(k1, 0.25) +
               truephase(k1, 0.5) +
               truephase(k1, 0.75) +
               truephase(k1 + 1, 0.0);
    }

    /*  editJDtoUTC  --  Edit a Julian date pair list to UTC date and time.
                         Displays the time as hours and minutes, rounded
                         to the nearest minute.  */

    string zerofill(integer n, integer places) {        // Integer to string with zero fill
        string sn = (string) n;
        while (llStringLength(sn) < places) {
            sn = "0" + sn;
        }
        return sn;
    }

    string editJDtoUTC(list jd) {
        list utctime = jhms(llList2Float(jd, 1));

        //  Round to nearest minute
        if (llList2Integer(utctime, 2) >= 30) {
            integer utchour = llList2Integer(utctime, 0);
            integer utcmin = llList2Integer(utctime, 1) + 1;
            if (utcmin >= 60) {
                utcmin -= 60;
                utchour++;
                if (utchour >= 24) {
                    utchour -= 24;
                    jd = llListReplaceList(jd, [  llList2Float(jd, 0) + 1 ], 0, 0);
                }
            }
            utctime = [ utchour, utcmin, 0 ];
        }

        list utcdate = jyearl(jd);
        string textutc = zerofill(llList2Integer(utcdate, 0), 4) +
                   "-" + zerofill(llList2Integer(utcdate, 1), 2) +
                   "-" + zerofill(llList2Integer(utcdate, 2), 2) +
                   " " + zerofill(llList2Integer(utctime, 0), 2) +
                   ":" + zerofill(llList2Integer(utctime, 1), 2);
        return textutc;
    }

    //  editJDtoDec  --  Edit a Julian date pair list to a decimal Julian date

    string editJDtoDec(list jd) {
//      if (llFabs(llList2Float(jd, 1)) >= 1) {
//          llOwnerSay("Unnormalised JD: " + llList2CSV(jd));
//      }
        string textjd = (string) llList2Float(jd, 0) + " " + (string) llList2Float(jd, 1);
        list ljd = llParseString2List(textjd, [".", " "], [" "]);
        textjd = llList2String(ljd, 0) + "." + llList2String(ljd, 3);

        return textjd;
    }

    //  EPL  --  Form English plural, if required

    string EPL(integer n) {
        if (n == 1) {
            return "";
        }
        return "s";
    }

    /*  EDFV  --  Edit a string representation of a floating
                  point number discarding insignificant trailing
                  zeroes and decimal.  */

    string edfv(string s) {
        integer busy = TRUE;

        while (busy && (llGetSubString(s, -1, -1) == "0")) {
            if (llGetSubString(s, -2, -2) == ".") {
                s = llGetSubString(s, 0, -3);
                busy = FALSE;
            } else {
                s = llGetSubString(s, 0, -2);
            }
        }
        return s;
    }

    //  EDFLS  --  Edit a comma-separated set of floats into a format string

    string edfls(string s, string v) {
        integer n;

        list l = llParseString2List(v, [", ", " "], [""]);
        while ((n = llSubStringIndex(s, "%f")) >= 0) {
            s = llDeleteSubString(s, n, n + 1);
            s = llInsertString(s, n, edfv(llList2String(l, 0)));
            l = llDeleteSubList(l, 0, 0);
        }

        return s;
    }

    //  EDFLF  --  Edit a list of floats into a format string

    string edflf(string s, list l) {
        return edfls(s, llList2CSV(l));
    }

    //  EDFLO  --  Edit a LSL object containing floats into a format string

    string edflo(string s, string v) {
        //  Deletes leading and trailing brackets and calls edflf()
        return edfls(s, llDeleteSubString(llDeleteSubString(v, -1, -1), 0, 0));
    }

    //  EDDEC  --  Edit float with specified number of decimal places.

    string eddec(float f, integer d) {
        if (d == 0) {
            //  Special case: round to integer with no decimals
            return (string) ((integer) llRound(f));
        }

        float scale = llPow(10, d);
        string fs = (string) (llRound(f * scale) / scale);  // Round to specified number of places
        integer dp = llSubStringIndex(fs, ".");     // Position of decimal point in string
        return llGetSubString(fs, 0, dp + d);       // Return number with specified decimal places
    }

    //  EDDEG  --  Edit degrees and minutes.

    string eddeg(float ds) {
        string result;
        float d = llFabs(ds + (30.0 / 3600.0));  // Round to nearest minute

        string sign;
        if (ds < 0) {
            sign = "-";
        } else {
            sign = "";
        }
        result = sign + ((string) ((integer) d)) + U_deg;

        d = 60 * (d - ((integer) d));
        if ((llFloor(d)) > 0) {
            result = result + ((string) llFloor(d)) + "'";
        }
        return result;
    }

    //  EDLAT  --  Edit latitude.

    string edlat(float lat) {
        string result;
        float ulat = fixangle(lat);

        if (ulat >= 180) {
            ulat = -(360 - ulat);
        }
        result = eddeg(llFabs(ulat));
        if (ulat < 0) {
            result += "S";
        } else {
            result += "N";
        }
        return result;
    }

    //  EDLON  --  Edit longitude.

    string edlon(float lon) {
        string result;
        float ulon = fixangle(lon);

        if (ulon >= 180) {
            ulon = -(360 - ulon);
        }
        result = eddeg(llFabs(ulon));
        if (ulon < 0) {
            result += "E";
        } else {
            result += "W";
        }
        return result;
    }

    /*  BUILDLEGEND  --  Compose the legend which is displayed as floating
                         text above the Earth globe.  */

    string buildLegend(list jd, list sun, list moon) {
        string result;

        //  Format the UTC date and time

        string textutc = editJDtoUTC(jd);

        //  Format the Julian date as a decimal number

        string textjd = editJDtoDec(jd);

        //  Format the subsolar point as latitude and longitude

        float sunlat = llList2Float(sun, 0);
        float sunlong = fixangle(llList2Float(sun, 1));
        if (sunlong >= 180) {
            sunlong = -(360 - sunlong);
        }
        string textsun = edlat(sunlat) + " " + edlon(sunlong);

        //  Format the sublunar point as latitude and longitude

        float moonlat = llList2Float(moon, 0);
        float moonlong = fixangle(llList2Float(moon, 1));
        if (moonlong >= 180) {
            moonlong = -(360 - moonlong);
        }
        string textmoon = edlat(moonlat) + " " + edlon(moonlong);

        result = textutc + " UTC\n" +
                 "JD " + textjd + "\n" +
                 "Sun " + textsun + "\n" +
                 "Moon " + textmoon;
        return result;
    }

    //  BUILDMOONLEGEND  --  Build legend for Moon information.

    string buildMoonLegend(list args) {

        list phaseNames = [ U_white_circle + " New",
                            U_right_half_black_circle + " First Q",
                            U_black_circle + " Full",
                            U_left_half_black_circle + " Last Q",
                            U_white_circle +  " New" ];

        /*  The items in the argument list are all floats, with
            indices as follows:

                0 - 1:  Julian day and fraction
                2 - 5:  Sun position: RA, Dec, RV, Solar longitude
                6 - 10: Moon position: RA, Dec, RV, Ecliptic longitude, Ecliptic latitude  */

        //  Age of moon in degrees
        float moonAge = llList2Float(args, 9) - llList2Float(args, 5);
        //  Moon phase (fraction illuminated)
        float moonPhase = (1 - llCos(moonAge * DEG_TO_RAD)) / 2;
        //  Moon age in days and fraction
        float moonDays = SynMonth * (fixangle(moonAge) / 360.0);
        //  Moon angular diameter
        float moonRV = llList2Float(args, 8);
        float moonDFrac = moonRV / MoonSemiMaj;
        float moonAng = moonSubtends / moonDFrac;

        /*  We only recompute the table of Moon phases if the
            present date is outside the range of phases we have
            already computed.  */

        float jd = llList2Float(args, 0);
        float jdf = llList2Float(args, 1);
        if ((llGetListLength(phaseJD) != 10) ||
            (jdcomp(jd, jdf, llList2Float(phaseJD, 0), llList2Float(phaseJD, 1)) < 0) ||
            (jdcomp(jd, jdf, llList2Float(phaseJD, 8), llList2Float(phaseJD, 9)) > 0)) {
            //  Build table of phases
            phaseJD = phasehunt(llList2Float(args, 0), llList2Float(args, 1));

            integer i;
            phaseDates = "";
            for (i = 0; i < 10; i += 2) {
                phaseDates += "\n" + llList2String(phaseNames, i / 2) +
                    "  " + editJDtoUTC(llList2List(phaseJD, i, i + 1));
            }
        }

        integer aom_d = (integer) moonDays;
        integer aom_h = (integer) (24 * (moonDays - llFloor(moonDays)));
        integer aom_m = ((integer) (1440 * (moonDays - llFloor(moonDays)))) % 60;
        return "Moon phase: " + (string) ((integer) llRound(moonPhase * 100)) +
                                "%\n" +
               "Age: " + (string) aom_d + " day" + EPL(aom_d) + ", " +
                         (string) aom_h + " hour" + EPL(aom_h) + ", " +
                         (string) aom_m + " minute" + EPL(aom_m) + "\n" +
               "Distance: " + (string) llRound(moonRV) + " km, " +
                              eddec(moonRV / EarthRad, 1) + " Earth radii\n" +
               "Subtends: " + eddec(moonAng, 4) + U_deg +
               phaseDates;
    }

    //  setLegendVisibility  --  Set visibility of Earth and Moon legends

    integer showEarthLegend = TRUE;     // Show floating text above the Earth globe ?
    integer showMoonLegend = TRUE;      // Show floating text above the Moon globe ?

    setLegendVisibility(key id, integer action) {
        if (action & 1) {       // 1: Error
            llRegionSayTo(id, PUBLIC_CHANNEL, "Usage: Set Legend [Earth/Moon/Both] On/Off");
        } else {
            list lclear = [ PRIM_TEXT, "", <0, 0, 0>, 0] ;
            if (action & 4) {    // 4: Earth
                if (action & 2) {   // 2: On/off
                    showEarthLegend = TRUE;
                } else {
                    showEarthLegend = FALSE;
                    //  Clear any existing legend
                    llSetLinkPrimitiveParamsFast(NIGHT_SIDE, lclear);
                }
            }
            if (action & 8) {    // 8: Moon
                if (action & 2) {   // 2: On/off
                    showMoonLegend = TRUE;
                } else {
                    showMoonLegend = FALSE;
                    //  Clear any existing legend
                    llSetLinkPrimitiveParamsFast(MOON_NIGHT, lclear);
                }
            }
        }
    }

    //  statSitter  --  Add status for sit target

    string statSitter(integer target, string tname) {
        key sitter;
        if ((sitter = llAvatarOnLinkSitTarget(target)) != NULL_KEY) {
            string aname = llGetUsername(sitter);
            if (aname == "") {
                aname = (string) sitter;    // Can't find name.  Show key instead
            }
            return "\nSitting on " + tname + ": " + aname;
        }
        return "";
    }

    //  showStatus  --  Show status via chat reply to requester

    showStatus(key id, list args) {
        string s = "Status:\n";
        integer mFree = llList2Integer(args, 2);
        integer mUsed = llList2Integer(args, 3);
        integer mFreeModel = llList2Integer(args, 26);
        integer mUsedModel = llList2Integer(args, 27);
        s +=
            "Main script.  Free memory: " + (string) mFree +
            "  Used memory: " + (string) mUsed + " (" +
            (string) ((integer) llRound((mUsed * 100.0) / (mUsed + mFree))) + "%)\n" +
            "Update Model script.  Free memory: " + (string) mFreeModel +
            "  Used memory: " + (string) mUsedModel + " (" +
            (string) ((integer) llRound((mUsedModel * 100.0) / (mUsedModel + mFreeModel))) + "%)\n";

        mFree = llGetFreeMemory();
        mUsed = llGetUsedMemory();

        integer LunatBase = 2423436;    /* Base date for E. W. Brown's numbered
                                           series of lunations (1923 January 16) */
        integer lunation = (integer) llFloor(((llList2Float(args, 4) - LunatBase) +
            llList2Float(args, 5)) / SynMonth) + 1;

        s +=
            "Legend script.  Free memory: " + (string) mFree +
            "  Used memory: " + (string) mUsed + " (" +
            (string) ((integer) llRound((mUsed * 100.0) / (mUsed + mFree))) + "%)\n" +

            edflo("Position: <%f, %f, %f>  ", llList2String(args, 0)) +
            edflo("Rotation: <%f, %f, %f>  ",
                (string) (llRot2Euler((rotation) llList2String(args, 1)) * RAD_TO_DEG)) +
            "\n" +

            "Julian day " + editJDtoDec(llList2List(args, 4, 5)) +
            "  " + editJDtoUTC(llList2List(args, 4, 5)) + " UTC\n" +

            edflf("Sun position: RA %f  Dec %f  RV %f  SLong %f\n", llList2List(args, 6, 9)) +

            edflf("Subsolar on Earth: Lat %f  Long %f  Alt %f\n", llList2List(args, 10, 12)) +

            edflf("Moon position: RA %f Dec %f RV %f Ecl lat %f Ecl long %f\n",
                llList2List(args, 13, 17)) +

            edflf("Lunar librations: Lat %f  Long %f\n", llList2List(args, 18, 19)) +

            edflf("Sublunar on Earth: Lat %f  Long %f  Alt %f\n", llList2List(args, 20, 22)) +

            edflf("Subsolar on Moon: Lat %f  Long %f  Alt %f\n", llList2List(args, 23, 25)) +

            "Lunation: " + (string) lunation;

        s += statSitter(NIGHT_SIDE, "Earth");
        s += statSitter(MOON_NIGHT, "Moon");

        llRegionSayTo(id, PUBLIC_CHANNEL, s);
    }

    //  showDialogue  --  Show dialogue when touched

    showDialogue(key toucher, list args) {
        integer dialogueChannel = llList2Integer(args, 0);
        list dialogueButtons = [];          // Dialogue buttons

        if (llList2Integer(args, 1)) {      // spinning
            dialogueButtons += [ "Stop spin" ];
        } else {
            dialogueButtons += [ "Spin" ];
        }
        if ((llList2Integer(args, 2) == 2) ||
            (llList2Integer(args, 2) == 0)) { // timeMode
            dialogueButtons += [ "Real Time" ];
        } else {
            dialogueButtons += [ "Animate" ];
        }
        dialogueButtons += [ "Step" ];
        if (llList2Integer(args, 3)) {      // showEarthLegend
            dialogueButtons += [ "Legend Earth Off" ];
        } else {
            dialogueButtons += [ "Legend Earth On" ];
        }
        if (llList2Integer(args, 4)) {      // showMoonLegend
            dialogueButtons += [ "Legend Moon Off" ];
        } else {
            dialogueButtons += [ "Legend Moon On" ];
        }
        dialogueButtons += [ "Help" ];
        llDialog(toucher, " ", dialogueButtons, dialogueChannel);
    }

    /*  Canned messages.  We declare these in global context so
        we don't need to initialise them on the stack every time
        we need the list and run the risk of blowing the memory
        limit on a dynamic call.  */

    list messages = [
        "Unknown access restriction.  Valid: public, group, owner.",    // 0
        "Usage: Set Animation rate/step time",  // 1
        "Usage: Set base on/off",   // 2
        "Invalid channel.", // 3
        "Usage: set date yyyy-mm-dd hh:mm:ss / jjjjjjjj.ffff",  // 4
        "Usage: set animation step [n] [unit]  Units: minute, hour, day, week, month, lunation", // 5
        "Usage: Set terminator soft/sharp", // 6
        "Usage: Set Night [Earth/Moon/Both] On/Off",    // 7
        "Unknown variable.  Valid: access, animation, base, cardinal, channel, date, fixed, inclination, legend, moon, moondist, night, terminator", // 8
        "Invalid command.  Valid: Animate, Freeze, Help, Real, Reset, Set, Spin, Status, Step, Stop"  // 9
    ];

    default {
        state_entry() {

            /*  Initialise special characters (defining them this way
                allows our script to be pure US_ASCII, which allows it
                to be edited by any text editor without worrying about
                character encodings.  */

            U_deg = llUnescapeURL("%C2%B0");                        // U+00B0 Degree Sign
            U_white_circle = llUnescapeURL("%E2%97%8B");            // U+25CB White Circle
            U_black_circle = llUnescapeURL("%E2%97%8F");            // U+25CF Black Circle
            U_left_half_black_circle = llUnescapeURL("%E2%97%96");  // U+25D6 Left Half Black Circle
            U_right_half_black_circle = llUnescapeURL("%E2%97%97"); // U+25D7 Right Half Black Circle
       }

       /*   The link_message() event receives requests to update the
            legend from the updateModel() function in the main
            script.  */

        link_message(integer sender, integer num, string str, key id) {

            //  LM_EARTH_LEGEND (1): Update Earth legend

            if (num == LM_EARTH_LEGEND) {
                if (showEarthLegend) {      // Avoid possible race condition
                    float timeNow = llGetTime();
                    if (((timeNow - earthLegendTimeLast) >= LegendMinInterval) ||
                        (timeNow < earthLegendTimeLast)) {
                        earthLegendTimeLast = timeNow;

                        list args = llJson2List(str);

                        /*  The items in the argument list are all floats, with
                            indices as follows:

                                0 - 1:  Julian day and fraction
                                2 - 4:  Subsolar latitude, longitude, altitude
                                5 - 7:  Sublunar latitude, longitude, altitude  */

                        list jd = llList2List(args, 0, 1);
                        list sun = llList2List(args, 2, 4);
                        list moon = llList2List(args, 5, 7);

                        string l = buildLegend(jd, sun, moon);
                        llSetLinkPrimitiveParamsFast(NIGHT_SIDE, [ PRIM_TEXT, l, <0, 1, 0>, 1] );
                    }
                }

            //  LM_MOON_LEGEND (2): Update Moon legend

            } else if (num == LM_MOON_LEGEND) {
                if (showMoonLegend) {       // Avoid possible race condition
                    float timeNow = llGetTime();
                    if (((timeNow - moonLegendTimeLast) >= LegendMinInterval) ||
                        (timeNow < moonLegendTimeLast)) {
                        moonLegendTimeLast = timeNow;
                        list args = llJson2List(str);

                        string l = buildMoonLegend(args);
                        llSetLinkPrimitiveParamsFast(MOON_NIGHT, [ PRIM_TEXT, l, <0, 1, 0>, 1] );
                    }
                }

            //  LM_STAT (3): Display script status

            } else if (num == LM_STAT) {
                list args = llJson2List(str);
                showStatus(id, args);

            //  LM_MESSAGE (4): Print canned message

            } else if (num == LM_MESSAGE) {
                string msg = llList2String(messages, (integer) str);
                if (id == NULL_KEY) {
                    //  No known sender.  Say in nearby chat.
                    llSay(PUBLIC_CHANNEL, msg);
                } else {
                    llRegionSayTo(id, PUBLIC_CHANNEL, msg);
                }

            //  LM_MESSAGE (5): Display dialogue

            } else if (num == LM_DIALOGUE) {
                list args = llJson2List(str);
                showDialogue(id, args);

            //  LM_LEGEND_VIS (6): Set legend visibility

            } else if (num == LM_LEGEND_VIS) {
                setLegendVisibility(id, (integer) str);

            //  LM_RESET (9): Reset script

            } else if (num == LM_RESET) {
                llResetScript();
            }
        }
    }
