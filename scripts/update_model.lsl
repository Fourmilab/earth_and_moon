    /*              Fourmilab Earth and Moon

                        Update Model

                         by John Walker
                    https://www.fourmilab.ch/
                    fourmilab in Second Life

        This program is licensed under a Creative Commons
        Attribution-ShareAlike 4.0 International License.
            http://creativecommons.org/licenses/by-sa/4.0/
        Please see the License section in the "Fourmilab
        Earth and Moon Help" notecard included in the object
        for details.

        The algorithms for positional astronomy in this program
        will look substantially different from what you'll find
        in standard references such as Jean Meeus's
        "Astronomical Algorithms".  The Second Life scripting
        language (LSL) implements only single-precision floating
        point, while accurate numerical astronomy requires
        double precision.  In the following code, we re-write
        the order of computations and in some cases use less
        general algorithms which suffer less from the loss of
        precision in single-precision floating-point arithmetic.

        If you notice some inaccuracy in the results of this
        program and try to fix it by going back to the standard
        double-precision expressions, you'll probably be sorely
        disappointed.  The code here has been carefully crafted
        to wring the most precision possible out of the limited
        accuracy available in LSL floating point arithmetic.  */

    //  Astronomical constants

    float J2000 = 2451545.0;            // Julian day of J2000 epoch
    float JulianCentury = 36525.0;      // Days in Julian century
    float SunSMAX = 149598022.291;      // Semi-major axis of Earth's orbit
    float MoonSemiMaj = 384401.0;       // Semi-major axis of Moon's orbit
    float EarthRad = 6378.14;           // Earth's equatorial radius, km (IAU 1976)
    float MoonRad = 1737.4;             // Moon's equatorial radius, km
    float meinc = 1.54242;              /* Inclination of the mean lunar equator
                                           to the ecliptic (IAU) */

    //  Model definition constants

    float MASK_SCALE = 1.03;            // Scale factor for day and night alpha map

    //  Parameters from main script

    integer showEarthLegend = TRUE;     // Show floating text above the Earth globe ?
    integer showMoonLegend = TRUE;      // Show floating text above the Moon globe ?
    integer cardinal = TRUE;            // Show cardinal points on textures
    integer fixSun = TRUE;              // Fix Sun, rotate Earth ?
    integer inclineEarth = TRUE;        // If Sun fixed, incline Earth ?
    float moonMeanDist = 2;             // Moon's mean display distance in globe radii

//  integer showEarth = FALSE;          // Hide and don't update Earth (for making Moon demos)

    //  Internal state variables

    integer wasFixed = -1;              // Last displayed value of fixSun
    integer currentMonth = -1;          // Current month texture applied to day sphere

    integer sitterEarth = 0;            // Link of avatar sitting on Earth
    integer sitterMoon = 0;             // Link of avatar sitting on Moon

    //  Link indices for child prims of linked object set

//    integer BASE = 1;
    integer DAY_SIDE = 2;
    integer NIGHT_SIDE = 3;
    integer MOON_GLOBE = 4;
    integer MOON_NIGHT = 5;

    //  Link message command codes

    integer LM_INIT = 0;                // Initialise internal state
    integer LM_EARTH_LEGEND = 1;        // Update Earth legend
    integer LM_MOON_LEGEND = 2;         // Update Moon legend
    integer LM_STAT = 3;                // Print status
//    integer LM_MESSAGE = 4;             // Print canned message
//    integer LM_DIALOGUE = 5;            // Display dialogue
//    integer LM_LEGEND_VIS = 6;          // Set legend visibility
    integer LM_UPDATE_MODEL = 7;        // Update Earth and Moon model
    integer LM_STAT_MODEL = 8;          // Request status from Update Model
    integer LM_RESET = 9;               // Reset scripts

    //  FIXANGLE  --  Range reduce an angle in degrees

    float fixangle(float a) {
        return a - (360.0 * llFloor(a / 360.0));
    }

    /*  JYEARL  --  Convert Julian date/time list to year,
                    month, day, which are returned as a list.  */

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

    /*  GMSTX  --  Calculate Greenwich Mean Sidereal Time for a
                   given instant expressed as a Julian date and
                   fraction.  We use a less general expression
                   than in Earth and Moon Viewer because it
                   yields more accurate results when computed in
                   the single-precision floating point of LSL.  */

    float gmstx(float jd, float jdf) {
        /*  See simplified formula in:
                https://aa.usno.navy.mil/faq/docs/GAST.php  */
        float D = (jd - 2451545) + jdf;
        float GMST = 18.697374558 + 24.06570982441908 * D;
        GMST -= 24.0 * (llFloor(GMST / 24.0));
        return GMST;
    }

    /*  KEPLER  --  Solve the equation of Kepler.  */

    float kepler(float m, float ecc) {
        float e;
        float delta;
        float EPSILON = 1E-6;

        e = m = m * DEG_TO_RAD;
        do {
            delta = e - ecc * llSin(e) - m;
            e -= delta / (1 - ecc * llCos(e));
        } while (llFabs(delta) > EPSILON);
        return e;
    }

    /*  OBLIQEQ  --  Calculate the obliquity of the ecliptic for
                     a given Julian date.  This uses Laskar's
                     tenth-degree polynomial fit (J. Laskar,
                     Astronomy and Astrophysics, Vol. 157, page
                     68 [1986]) which is accurate to within 0.01
                     arc second between AD 1000 and AD 3000, and
                     within a few seconds of arc for +/-10000
                     years around AD 2000.  If we're outside the
                     range in which this fit is valid (deep
                     time) we simply return the J2000 value of
                     the obliquity, which happens to be almost
                     precisely the mean.  */

    float obliqeq(float jd, float jdf) {
        /*  These terms were originally specified in arc
            seconds.  In the interest of efficiency, we convert
            them to degrees by dividing by 3600 and round to
            nine significant digits, which is the maximum
            precision of the single-precision floats used by
            LSL.  */
        list oterms = [
            -1.30025833,
            -0.000430555556,
             0.555347222,
            -0.0142722222,
            -0.0693527778,
            -0.0108472222,
             0.00197777778,
             0.00774166667,
             0.00160833333,
             0.000680555556
        ];

        //  Again, we evaluate a number specified as 23d26'21".448 as degrees
        float eps = 23.4392911;
        float u;
        float v;
        integer i;

        v = u = ((jd - J2000) / (JulianCentury * 100)) + (jdf / (JulianCentury * 100));

        if (llFabs(u) < 1.0) {
            for (i = 0; i < 10; i++) {
                eps += llList2Float(oterms, i) * v;
                v *= u;
            }
        }
        return eps;
    }

    /*  ECLIPTOEQ  --  Convert celestial (ecliptical) longitude
                       and latitude into right ascension (in
                       degrees) and declination.  We must supply
                       the time of the conversion in order to
                       compensate correctly for the varying
                       obliquity of the ecliptic over time.  */

    list ecliptoeq(float jd, float jdf, float Lambda, float Beta) {

        // Obliquity of the ecliptic
        float eps = obliqeq(jd, jdf) * DEG_TO_RAD;

        Lambda *= DEG_TO_RAD;
        Beta *= DEG_TO_RAD;
        return [
            fixangle(llAtan2((llCos(eps) * llSin(Lambda) -
                    (llTan(Beta) * llSin(eps))), llCos(Lambda)) * RAD_TO_DEG),
                llAsin((llSin(eps) * llSin(Lambda) * llCos(Beta)) +
                    (llSin(Beta) * llCos(eps))) * RAD_TO_DEG
               ];
    }

    /*  SUNPOS  --  Calculate position of the Sun.  JD and JDF
                    are the Julian date and fraction of the
                    instant for which the position is desired
                    and APPARENT should be nonzero if the
                    apparent position (corrected for nutation
                    and aberration) is desired. The Sun's
                    co-ordinates are returned in a list
                    containing the right ascension, declination,
                    radius vector, and solar longitude.  */

    list sunpos(float jd, float jdf, integer apparent) {

        /* Time, in Julian centuries of 36525 ephemeris days,
           measured from the epoch 1900 January 0.5 ET. */

        float t = ((jd - 2415020.0) / JulianCentury) + (jdf / JulianCentury);
        float t2 = t * t;
        float t3 = t2 * t;

        /* Geometric mean longitude of the Sun, referred to the
           mean equinox of the date. */

        float l = fixangle(279.69668 + 36000.76892 * t + 0.0003025 * t2);

        // Sun's mean anomaly

        float m = fixangle(358.47583 + 35999.04975 * t - 0.000150 * t2 - 0.0000033 * t3);

        // Eccentricity of the Earth's orbit

        float e = 0.01675104 - 0.0000418 * t - 0.000000126 * t2;

        // Eccentric anomaly

        float ea = kepler(m, e);

        // True anomaly

        float v = fixangle(2 * llAtan2(llSqrt((1 + e) / (1 - e)) * llTan(ea / 2), 1) * RAD_TO_DEG);

        // Sun's true longitude

        float theta = l + v - m;

        // Obliquity of the ecliptic

        float eps = obliqeq(jd, jdf);

        // Corrections for Sun's apparent longitude, if desired

        if (apparent) {
           float omega = fixangle(259.18 - 1934.142 * t) * DEG_TO_RAD;
           theta = theta - 0.00569 - 0.00479 * llSin(omega);
           eps += 0.00256 * llCos(omega);
        }

        // Compute Sun's longitude and radius vector

        float sunpos_SLONG = theta;
        float sunpos_RV = (1.0000002 * (1 - (e * e))) / (1 + e * llCos(v * DEG_TO_RAD));

        // Determine solar co-ordinates

        eps *= DEG_TO_RAD;
        theta *= DEG_TO_RAD;
        float sunpos_RA = fixangle(llAtan2(llCos(eps) *
            llSin(theta), llCos(theta)) * RAD_TO_DEG);
        float sunpos_DEC = llAsin(llSin(eps) * llSin(theta)) * RAD_TO_DEG;

        return [ sunpos_RA, sunpos_DEC, sunpos_RV, sunpos_SLONG ];
    }

    /*  SUBSOLAR  --  Compute latitude and longitude of subsolar
                      point.  Returns a list containing the
                      latitude, longitude, and the distance from
                      the Earth's surface at the subsolar point
                      to the centre of the Sun.  If you have
                      previously called sunpos() for jd+jdf, you
                      can pass the result list as argument sun
                      to avoid recomputation.  Otherwise, pass
                      [] to recompute sunpos().  */

    list subsolar(float jd, float jdf, list sun) {
        if (sun == []) {
            sun = sunpos(jd, jdf, 0);
        }
        float gt = gmstx(jd, jdf);

        float subslong = (gt * 15) - llList2Float(sun, 0);
        if (subslong > 180) {
            subslong = -(360 - subslong);
        } else if (subslong < -180) {
            subslong += 360;
        }

        return [ llList2Float(sun, 1),      // Latitude
                 subslong,                  // Longitude
                 llList2Float(sun, 2) * SunSMAX - EarthRad  // Altitude
                ];
    }

    /*  LOWMOON  --  Low-precision calculation of the position
                     of the Moon. We return a list containing
                     the right ascension and declination of the
                     Moon, its radius vector (distance between
                     the centres of the Earth and Moon) and, for
                     those who need them, the ecliptic longitude
                     and latitude of the Moon before
                     transformation to equatorial co-ordinates.  */

    list lowmoon(float jd, float jdf) {

        // Elements of the Moon's orbit

        float Epoch = 2444238.5;    // Epoch: 1980-01-01 00:00 UTC
        float l0 = 64.975464;       // Moon's mean longitude
        float P0 = 349.383063;      // Mean longitude of the perigee
        float N0 = 151.950429;      // Mean longitude of the node
        float i  = 5.145396;        // Inclination
        float e  = 0.054900;        // Eccentricity
        float a  = MoonSemiMaj;     // Semi-major axis

        //  Elements of the Sun's apparent orbit

        float Epg = 278.833540;     // Ecliptic longitude
        float Omg = 282.596403;     // Ecliptic longitude of perigee
        float Es = 0.016718;        // Eccentricity

        float D = (jd - Epoch) + jdf;

        //              For the Sun

        float Ns = ((360.0 / 365.2422) * D);    // Circular orbit position
        float M = fixangle(Ns + Epg - Omg);     // Mean anomaly
        float sEc = kepler(M, Es);              // Solve equation of Kepler
        sEc = llSqrt((1 + Es) / (1 - Es)) * llTan(sEc / 2);
        sEc = 2 * llAtan2(sEc, 1) * RAD_TO_DEG; // True anomaly
        float Las = fixangle(sEc + Omg);        // Sun's geocentric ecliptic longitude

        //              For the Moon

        float l = fixangle((13.1763966 * D) + l0);      // Mean longitude
        float Mm = fixangle(l - (0.1114041 * D) - P0);  // Mean anomaly
        float N = fixangle(N0 - 0.0529539 * D);         // Ascending node mean longitude
        float C = l - Las;                              // Correction for evection
        float Ev = 1.2739 * llSin(((2 * C) - Mm) * DEG_TO_RAD);  // Evection
        float Ae = 0.1858 * llSin(M * DEG_TO_RAD);      // Annual equation
        float A3 = 0.37 * llSin(M * DEG_TO_RAD);        // Third correction

        float Mpm = fixangle(Mm + Ev - Ae - A3);        // Corrected anomaly M'm
        float Ec = fixangle(6.2886 * llSin(Mpm * DEG_TO_RAD));   // Correction for equation of the centre
        float A4 = 0.214 * llSin(2 * Mpm * DEG_TO_RAD); // Fourth correction
        float lp = fixangle(l + Ev + Ec - Ae + A4);     // Corrected longitude
        float V = fixangle(0.6583 * llSin((2 * (lp - Las)) * DEG_TO_RAD));  // Variation

        float lpp = fixangle(lp + V);                   // True orbital longitude
        float Np = fixangle(N - (0.16 * llSin(M * DEG_TO_RAD)));    // Corrected longitude of the node
        float lm = (llAtan2(llSin((lpp - Np) * DEG_TO_RAD) * llCos(i * DEG_TO_RAD), // Ecliptic latitude
            llCos((lpp - Np) * DEG_TO_RAD)) * RAD_TO_DEG) + Np;
        float bm = llAsin(llSin(fixangle(lpp - Np) * DEG_TO_RAD) * llSin(i * DEG_TO_RAD)) * RAD_TO_DEG;

        float Alm;                                      // Right ascension
        float Bem;                                      // Declination
        list ecql = ecliptoeq(jd, jdf, lm, bm);
        Alm = llList2Float(ecql, 0);
        Bem = llList2Float(ecql, 1);

        float Rh = (a * (1 - (e * e))) /           // Radius vector (km)
            (1 + (e * llCos(fixangle(Mpm + Ec) * DEG_TO_RAD)));

        return [ Alm, Bem, Rh, lm, bm ];
    }

    /*  SUBLUNAR  --  Compute latitude and longitude of sublunar
                      point.  Returns a list containing the
                      latitude, longitude, and the distance from
                      the Earth's surface at the sublunar point
                      to the centre of the Moon.  If you have
                      previously called lowmoon() with jd+jdf,
                      you can pass the result list in as
                      argument moon to avoid recomputation.
                      Otherwise, pass [] to recompute here.  */

    list sublunar(float jd, float jdf, list moon) {
        if (moon == []) {
            moon = lowmoon(jd, jdf);
        }
        float gt = gmstx(jd, jdf);
        float moonlong = (gt * 15) - llList2Float(moon, 0);
        if (moonlong > 180) {
            moonlong = -(360 - moonlong);
        } else if (moonlong < -180) {
            moonlong += 360;
        }

        return [ llList2Float(moon, 1),     // Latitude
                 moonlong,                  // Longitude
                 llList2Float(moon, 2)      // Altitude
               ];
    }

    /*  LIBRATION  --  Calculate the optical librations of the
                       Moon for a given date.  This is
                       particularly sensitive to floating point
                       precision and will disagree substantially
                       (although not visibly at the scale of the
                       model) from computation done in double
                       precision.  A list returned containing
                       the latitude and longitude of the
                       libration.  */

    list libration(float jd, float jdf, float mlong, float mlat) {
        float t2;
        float t3;

        float t = ((jd - J2000) / JulianCentury) + (jdf / JulianCentury);

        float t4 = t * (t3 = t * (t2 = t * t));
        float arglat = 93.2720993 + 483202.0175273 * t - 0.0034029 * t2 -
                    t3 / 3526000.0 + t4 / 863310000.0;
        arglat = fixangle(arglat);
        float lanode = 125.0445550 - 1934.1361849 * t + 0.0020762 * t2 +
                    t3 / 467410.0 - t4 / 60616000.0;
        float W = fixangle(mlong - lanode);
        W *= DEG_TO_RAD;
        mlat *= DEG_TO_RAD;
        float meincR = meinc * DEG_TO_RAD;
        float A = llAtan2(
                      (llSin(W) * llCos(mlat) * llCos(meincR) -
                          llSin(mlat) * llSin(meincR)),
                      (llCos(W) * llCos(mlat))
                   ) * RAD_TO_DEG;

        float lo = A - arglat;
        lo = lo - llFloor((lo / 90) + 0.5) * 90;
        float libLong = lo;
        float libLat = llSin(-(llSin(W) * llCos(mlat) * llSin(meincR)) -
                            llSin(mlat) * llCos(meincR)) * RAD_TO_DEG;

        return [ libLat, libLong ];
    }

    /*  MOONSUBSOLAR  --  Calculate the position and radius
                          vector of the Sun in selenographic
                          co-ordinates. IMPORTANT!!!  Note that
                          selenographic longitudes are, for
                          historical reasons, backwards with
                          respect to terrestrial longitudes:
                          positive toward the East in terms of
                          the current convention for
                          selenographic co-ordinates.  If you
                          have previously called sunpos() and
                          lowmoon() for jd+jdf, you can pass
                          their result lists in as arguments sp
                          and lwm to avoid recomputing them
                          here.  Otherwise, pass [] and they
                          will be computed.  */

    list moonsubsolar(float jd, float jdf, list sp, list lwm) {
        float t2;
        float t3;

        if (sp == []) {
            sp = sunpos(jd, jdf, FALSE);
        }
        float sunra = llList2Float(sp, 0);
        float sundec = llList2Float(sp, 1);
        float sunrv = llList2Float(sp, 2);
        float sunlong = llList2Float(sp, 3);

        float subslong = (gmstx(jd, jdf) * 15) - sunra;
        if (subslong > 180) {
            subslong = -(360 - subslong);
        } else if (subslong < -180) {
            subslong += 360;
        }

        if (lwm == [] ) {
            lwm = lowmoon(jd, jdf);
        }
        float mrv = llList2Float(lwm, 2);
        float mlong = llList2Float(lwm, 3);
        float mlat = llList2Float(lwm, 4);
        float srvkm = sunrv * SunSMAX;
        float lambdaH = fixangle(sunlong + 180.0 + (mrv / srvkm) *
                    57.296 * llCos(mlong * DEG_TO_RAD) * llSin((sundec - mlat) * DEG_TO_RAD));
        float betaH = fixangle((mrv / srvkm) * mlat);

        float t = ((jd - J2000) + jdf) / JulianCentury;

        float t4 = t * (t3 = t * (t2 = t * t));
        float arglat = fixangle(93.2720993 + 483202.0175273 * t - 0.0034029 * t2 -
                    t3 / 3526000.0 + t4 / 863310000.0);
        float lanode = fixangle(125.0445550 - 1934.1361849 * t + 0.0020762 * t2 +
                    t3 / 467410.0 - t4 / 60616000.0);

        float W = fixangle(lambdaH - lanode);
        W *= DEG_TO_RAD;
        betaH *= DEG_TO_RAD;
        float meincR = meinc * DEG_TO_RAD;
        float A = llAtan2(
                      (llSin(W) * llCos(betaH) * llCos(meincR) -
                          llSin(betaH) * llSin(meincR)) ,
                      (llCos(W) * llCos(betaH))
               ) * RAD_TO_DEG;
        float sunLong = fixangle(A - arglat);
        float sunLat = llAsin(-(llSin(W) * llCos(betaH) * llSin(meincR)) -
                            llSin(betaH) * llCos(meincR)) * RAD_TO_DEG;
        float sunRV = srvkm; /*** Need to transform to
                            rectangular co-ordinates and
                            calculate actual Sun/Moon distance. */
        return [ sunLat, sunLong, sunRV ];
    }


    /*  UPDATEMODEL  --  Update the model to the current real time
                         or simulated date.  This consists of:

                            1.  Replace the texture map on the
                                illuminated Earth globe if the month
                                has changed since the last update.
                            2.  Calculate position of the Sun
                            3.  Calculate the position of the Moon.
                            4.  Adjust the position of the Moon globe
                                (and night side mask) to the Moon's
                                current position.
                            5.  Calculate lunar librations.
                            6.  Rotate the Moon globe so the current
                                centre point of the disc points toward
                                the centre of the Earth.
                            7.  Adjust the night side maps of the
                                Earth and Moon globes to point away
                                from the subsolar point on each body.
    */

    updateModel(float jd, float jdf) {
        list timenow = [ jd, jdf ];

        //  Compute Sun position (sub-solar point on Earth) from time

        list sp = sunpos(jd, jdf, 0);
        list sunpos = subsolar(jd, jdf, sp);

        /*  Determine the current month.  If it differs from the
            texture currently applied to the day globe, retrieve
            the correct texture from the inventory of the base
            and install on the day globe.  */

        integer monthNow;
        if (cardinal) {
            monthNow = 99;              // Code for cardinal point texture
        } else {
            monthNow = llList2Integer(jyearl(timenow), 1);
        }
        if (monthNow != currentMonth) {
            string monthtex;
            if (monthNow == 99) {       // Displaying the cardinal points ?
                monthtex = "CP";
            } else {
                monthtex = (string) monthNow;
                if (llStringLength(monthtex) == 1) {
                    monthtex = "0" + monthtex;
                }
            }
            monthtex = "Earth_Day_" + monthtex;
//          if (showEarth) {
            llSetLinkTexture(DAY_SIDE, monthtex, ALL_SIDES);
//          } else {
//              llSetLinkAlpha(DAY_SIDE, 0, ALL_SIDES);
//              llSetLinkAlpha(NIGHT_SIDE, 0, ALL_SIDES);
//          }
            currentMonth = monthNow;
        }

        list lm = lowmoon(jd, jdf);
        list lib = libration(jd, jdf,
            llList2Float(lm, 3), llList2Float(lm, 4));
        //  Adjust libration longitude for the way lunar longitudes go
        lib = [ llList2Float(lib, 0), -llList2Float(lib, 1) ];

        /*  Now yaw and pitch the Earth night side alpha mask so
            that it is oriented opposite the subsolar point.
            This will subdue the night side of the globe.  */

        float Ezang = (90 - llList2Float(sunpos, 1)) * DEG_TO_RAD;
        float Exang = llList2Float(sunpos, 0) * DEG_TO_RAD;
        if (fixSun) {

            //  Sun is fixed: Earth moves

            Ezang = llList2Float(sunpos, 1) * DEG_TO_RAD;
            vector ErotatedZ = <0, 0, 1> * llEuler2Rot(<Exang, 0, 0>);
//          if (showEarth) {
            if (inclineEarth) {

                //  Night side mask stays vertical, Earth inclines

                llSetLinkPrimitiveParamsFast(NIGHT_SIDE, [ PRIM_ROT_LOCAL,
                    //  Rotate to put North up
                    llEuler2Rot(<0, -PI_BY_TWO, 0>)
                    *
                    llEuler2Rot(<0, 0, PI>)
                ]);

                llSetLinkPrimitiveParamsFast(DAY_SIDE, [ PRIM_ROT_LOCAL,
                    //  Rotate to put North up
                    llEuler2Rot(<0, PI_BY_TWO, 0>)
                    *
                    //  Tilt to move north pole toward or away from Sun
                    llEuler2Rot(<Exang, 0, 0>)
                    *
                    //  Turn to aim subsolar longitude at Sun
                    llAxisAngle2Rot(ErotatedZ, Ezang)
                ]);

            } else {

                //  Night side mask inclines based on subsolar latitude

                llSetLinkPrimitiveParamsFast(NIGHT_SIDE, [ PRIM_ROT_LOCAL,
                    //  Rotate to put North up
                    llEuler2Rot(<0, -PI_BY_TWO, 0>)
                    *
                    //  Turn to point day side toward the Sun
                    llEuler2Rot(<0, 0, PI>)
                    *
                    //  Tilt to point subsolar latitude toward Sun
                    llAxisAngle2Rot(<-1, 0, 0>, Exang)
                ]);
                llSetLinkPrimitiveParamsFast(DAY_SIDE, [ PRIM_ROT_LOCAL,
                    //  Rotate to put North up
                    llEuler2Rot(<0, PI_BY_TWO, 0>)
                    *
                    //  Rotate so subsolar longitude points at Sun
                    llEuler2Rot(<0, 0, Ezang>)
                ]);
            }
//          }

        } else {

            //  Earth is fixed: Sun moves

            vector ErotatedX = <-1, 0, 0> * llEuler2Rot(<0, 0, Ezang>);
//          if (showEarth) {
            llSetLinkPrimitiveParamsFast(NIGHT_SIDE, [ PRIM_ROT_LOCAL,
                //  Rotate to put North up
                llEuler2Rot(<0, -PI_BY_TWO, 0>)
                *
                //  Turn to align longitude toward Earth
                llEuler2Rot(<0, 0, Ezang>)
                *
                //  Tilt to point latitude toward Earth
                llAxisAngle2Rot(ErotatedX, -Exang)
            ]);
            //  Re-establish standard rotation if Earth newly fixed
            if (wasFixed != fixSun) {
                llSetLinkPrimitiveParamsFast(DAY_SIDE, [ PRIM_ROT_LOCAL,
                    //  Rotate to put North up
                    llEuler2Rot(<0, PI_BY_TWO, 0>)
                    *
                    llEuler2Rot(<0, 0, -PI_BY_TWO>)
                ]);
            }
//          }
        }
        wasFixed = fixSun;          // Update last fixed setting

        //  Centre of Earth globe
        vector earthGlobeP = llList2Vector(llGetLinkPrimitiveParams(DAY_SIDE,
            [ PRIM_POS_LOCAL ]), 0);

        /*  If an avatar is sitting on the Earth, rotate along with
            the Earth, as defined by Ezang, which is used above to
            rotate the globe or the terminator mask, according to
            which is fixed to diurnal motion.  We override the position
            of the avatar to be atop the sphere, avoiding bias
            introduced by rotation of the initial sit target as part
            of the NIGHT_SIDE.  */

        if (sitterEarth) {
            vector earthGlobeS = llList2Vector(llGetLinkPrimitiveParams(DAY_SIDE,
                [ PRIM_SIZE ]), 0);
            llSetLinkPrimitiveParamsFast(sitterEarth,
                [ PRIM_POS_LOCAL, earthGlobeP + <0, 0, earthGlobeS.z> ]);
            llSetLinkPrimitiveParamsFast(sitterEarth,
                [ PRIM_ROT_LOCAL, llEuler2Rot(<0, 0, Ezang>) ]);
        }

        /*  Next, we move on to the Moon.  Calculate its
            position and move the Moon globe and shadow mask to
            their proper positions with respect to the Earth.  */

        list moonp = sublunar(jd, jdf, lm);
        //  Distance to Moon as fraction of Earth globe size
        vector globePos = llList2Vector(llGetLinkPrimitiveParams(DAY_SIDE, [ PRIM_SIZE ]), 0);
        float globeSize = globePos.x / 2;

        /*  Display Moon at distance from Earth scaled by radius
            vector over semi-major axis of orbit.  */
        float moonDist = globeSize * moonMeanDist * (llList2Float(lm, 2) / MoonSemiMaj);

        float longang = fixangle(180 - llList2Float(moonp, 1)) * DEG_TO_RAD;
        float latang = llList2Float(moonp, 0) * DEG_TO_RAD;
        if (fixSun) {
            //  Sun is fixed: adjust Moon latitude by Earth subsolar latitude
            longang = (longang + Ezang) + PI_BY_TWO;
        }

        /*  If the Sun is fixed and we're inclining the Earth to
            show the seasons (as opposed to having the Sun bob
            up and down with respect to the Earth), we need to
            further rotate the Moon's position around the X
            axis to take the inclination of the Earth globe
            into account.  */

        rotation moonRVx = ZERO_ROTATION;
        if (fixSun && inclineEarth) {
            moonRVx = llEuler2Rot(<Exang, 0, 0>);
        }

        /*  Translate, rotate, and scale an X axis unit vector
            into the Moon's position relative to the centre of
            the Earth globe.  */

        vector moonRV = earthGlobeP +
            (<1, 0, 0>                      // X axis unit vector
             *
             llEuler2Rot(<0, -latang, 0>)   // Tilt by sublunar latitude
             *
             llEuler2Rot(<0, 0, longang>)   // Turn to sublunar longitude
             *
             moonRVx                        // Account for Earth inclination if Sun fixed
             *
             moonDist                       // Scale to Moon distance from Earth
            );

        /*  If an avatar is sitting on the Moon, update their position
            based upon the relative motion of the Moon in the last step.  */
        if (sitterMoon) {
            vector moonRVold = llList2Vector(llGetLinkPrimitiveParams(MOON_NIGHT,
                [ PRIM_POS_LOCAL ]), 0);
            vector moonSold =  llList2Vector(llGetLinkPrimitiveParams(sitterMoon,
                [ PRIM_POS_LOCAL ]), 0);
            llSetLinkPrimitiveParamsFast(sitterMoon,
                [ PRIM_POS_LOCAL, moonSold + (moonRV - moonRVold) ]);
        }

        //  Move the Moon to its new position
        llSetLinkPrimitiveParamsFast(MOON_GLOBE, [ PRIM_POS_LOCAL, moonRV ]);
        llSetLinkPrimitiveParamsFast(MOON_NIGHT, [ PRIM_POS_LOCAL, moonRV ]);

        /*  Rotate Moon globe so centre of near side (adjusted
            for libration) points to the centre of the Earth.  */

        vector EarthCtrg = llList2Vector(llGetLinkPrimitiveParams(DAY_SIDE,
            [ PRIM_POS_LOCAL ]), 0);
        vector MoonCtrg = llList2Vector(llGetLinkPrimitiveParams(MOON_GLOBE,
            [ PRIM_POS_LOCAL ]), 0);
        vector mevgn = llVecNorm(EarthCtrg - MoonCtrg);

        //  Compute angle to tilt Moon around X to point at Earth's Z

        float xang = -llSin(mevgn.z) + (llList2Float(lib, 0) * DEG_TO_RAD);

        /*  Compute angle to rotate Moon about Z to point at
            Earth. Note that the following code is full of fudge
            factors needed to accommodate the initial rotation
            of the Moon so that its texture points the right way
            before we rotate it.  */

        float zang = llAtan2(mevgn.y, mevgn.x) + (llList2Float(lib, 1) * DEG_TO_RAD);
        zang += PI_BY_TWO;

        vector rotatedX = <-1, 0, 0> * llEuler2Rot(<0, 0, zang>);
        llSetLinkPrimitiveParamsFast(MOON_GLOBE, [ PRIM_ROT_LOCAL,
            //  Rotate to put North up
            llEuler2Rot(<0, PI_BY_TWO, 0>)
            *
            //  Turn to align longitude toward Earth
            llEuler2Rot(<0, 0, zang>)
            *
            //  Tilt to point latitude toward Earth
            llAxisAngle2Rot(rotatedX, -xang)
        ]);

        //  Rotate Moon day and night mask based on Moon subsolar point

        list moonss = moonsubsolar(jd, jdf, sp, lm);
        llSetLinkPrimitiveParamsFast(MOON_NIGHT, [ PRIM_ROT_LOCAL,
            //  Rotate to put North up
            llEuler2Rot(<0, PI_BY_TWO, 0>)
            *
            //  Turn to align with Earth, then turn to subsolar longitude
            llEuler2Rot(<0, 0, zang + (llList2Float(moonss, 1) * DEG_TO_RAD) - PI>)
            *
            //  Tilt to align with Earth, plus (tiny) subsolar latitude
            llAxisAngle2Rot(rotatedX, -xang + (llList2Float(moonss, 0) * DEG_TO_RAD))
        ]);

        //  If enabled, show floating text legend above the Earth and Moon globes

        if (showEarthLegend) {
            llMessageLinked(LINK_THIS, LM_EARTH_LEGEND,
                llList2Json(JSON_ARRAY, timenow + sunpos + moonp), NULL_KEY);
        }

        if (showMoonLegend) {
            llMessageLinked(LINK_THIS, LM_MOON_LEGEND,
                llList2Json(JSON_ARRAY, timenow + sp + lm), NULL_KEY);
        }
    }

    //  initModel  --  Initialise the model when the main script resets

    initModel() {
        wasFixed = -1;              // Last displayed value of fixSun
        currentMonth = -1;          // Current month texture applied to day sphere

        /*  Automatically scale the Moon globe and mask based upon
            the size of the Earth globe and the ratio of radii of
            the two bodies.  If the user prefers a different sized
            globe, they can just rescale the visible Earth globe and
            everything will be rescaled accordingly.  */

        vector globeSize = llList2Vector(llGetLinkPrimitiveParams(DAY_SIDE,
            [ PRIM_SIZE ]), 0);
        float moonSize = (globeSize.z * MoonRad) / EarthRad;
        llSetLinkPrimitiveParamsFast(MOON_GLOBE,
            [ PRIM_SIZE, < moonSize, moonSize, moonSize >]);
        llSetLinkPrimitiveParamsFast(MOON_NIGHT,
            [ PRIM_SIZE, < moonSize * MASK_SCALE,
                            moonSize * MASK_SCALE,
                            moonSize * MASK_SCALE >]);

        /*  "Assume the Earth is a sphere."  If the user has
            fat-fingered the shape of the globe (by editing the
            object with "Edit linked"), we'll now put it back to
            the Z extent of the day globe. This should undo many
            cases of corruption.  This also scales up the night
            mask based upon the size of the day side,
            guaranteeing the relationship remains the same if
            the user resizes the object.  */

        /* Disable this once we're in production.
        llSetLinkPrimitiveParamsFast(DAY_SIDE,
            [ PRIM_SIZE, < globeSize.z, globeSize.z, globeSize.z >]);
        llSetLinkPrimitiveParamsFast(NIGHT_SIDE,
            [ PRIM_SIZE, < globeSize.z * MASK_SCALE,
                globeSize.z * MASK_SCALE, globeSize.z * MASK_SCALE >]);
        /* */

        list clearLegend =  [PRIM_TEXT, "", <0, 0, 0>, 0 ];
        llSetLinkPrimitiveParamsFast(NIGHT_SIDE, clearLegend);  // Clear existing Earth legend
        llSetLinkPrimitiveParamsFast(MOON_NIGHT, clearLegend);  // Clear existing Moon legend

        /*  Place sit targets on the Earth and Moon's terminator
            masks so avatars can sit on each independently.  The
            sit positions are pure hackwork.  */

        llLinkSitTarget(NIGHT_SIDE,
            <globeSize.z, 0, -((globeSize.z / 2) - 0.4)>,
            llEuler2Rot(<0, PI_BY_TWO, PI_BY_TWO>));
        llLinkSitTarget(MOON_NIGHT,
            <-moonSize * 2, moonSize, -moonSize>,
            llEuler2Rot(<0, -PI_BY_TWO, PI_BY_TWO>));

    }

    //  showModelStatus  --  Obtain status of model as of last update

    showModelStatus(key id, list args) {
        float jd = llList2Float(args, 4);
        float jdf = llList2Float(args, 5);
        list sunp = sunpos(jd, jdf, 0);
        list sunsub = subsolar(jd, jdf, sunp);
        list lm = lowmoon(jd, jdf);
        list lib = libration(jd, jdf, llList2Float(lm, 3), llList2Float(lm, 4));
        list moonp = sublunar(jd, jdf, lm);
        list moonss = moonsubsolar(jd, jdf, sunp, lm);

        //  Append locally-computed status to overall status
        args +=
                                                    // 0: Root prim position
                                                    // 1: Root prim rotation
                                                    // 2: Main script free memory
                                                    // 3: Main script used memory
                                                    // 4-5: Last displayed date and time
                sunp +                              // 6-9: Sun position (RA, DEC, RV, SLONG)
                sunsub +                            // 10-12: Subsolar position on Earth (Lat, Long, Alt)
                lm +                                // 13-17: Lunar position (RA, DEC, RV, Ecl lat, Ecl long)
                lib +                               // 18-19: Lunar librations (Lat, Long)
                moonp +                             // 20-22: Sublunar position on Earth (Lat, Long, Alt)
                moonss +                            // 23-25: Subsolar position on Moon (Lat, Long, Alt)
                [ llGetFreeMemory(),                // 26: Update Model script free memory
                  llGetUsedMemory() ]               // 27: Update Model script used memory
        ;

        //  Pass on to the Legend script to display status
        llMessageLinked(LINK_THIS, LM_STAT, llList2Json(JSON_ARRAY, args), id);
    }

    //  sitterLink  --  Determine link number of seated avatar

    integer sitterLink(key k) {
        integer l;

        integer nPrims = llGetNumberOfPrims();
        for (l = 1; l <= nPrims; l++) {
            if (llGetLinkKey(l) == k) {
                return l;
            }
        }
        return 0;               // Can't find avatar (maybe stood up)
    }

    /*  DEFAULT  --  This is the default state handler.  This is
                     presently the only state we have.  */

    default {

        state_entry() {
        }


       /*   The link_message() event receives requests
            from the updateModel() function in the main
            script.  */

        link_message(integer sender, integer num, string str, key id) {

            //  LM_INIT (0): Initialise internal state

            if (num == LM_INIT) {
                initModel();

            //  LM_UPDATE_MODEL (7): Update Earth and Moon model

            } else if (num == LM_UPDATE_MODEL) {
                list args = llJson2List(str);

                /*  The items in the argument list have
                    indices as follows:

                        0 - 1:  Julian day and fraction
                        2:  showEarthLegend
                        3:  showMoonLegend
                        4:  cardinal
                        5:  fixSun
                        6:  inclineEarth
                        7:  moonMeanDist
                */

                list jd = llList2List(args, 0, 1);
                showEarthLegend = llList2Integer(args, 2);
                showMoonLegend = llList2Integer(args, 3);
                cardinal = llList2Integer(args, 4);
                fixSun = llList2Integer(args, 5);
                inclineEarth = llList2Integer(args, 6);
                moonMeanDist = llList2Integer(args, 7);

                updateModel(llList2Float(jd, 0), llList2Float(jd, 1));

            //  LM_STAT_MODEL (8): Request status of model

            } else if (num == LM_STAT_MODEL) {
                list args = llJson2List(str);
                showModelStatus(id, args);

            //  LM_RESET (9): Reset script

            } else if (num == LM_RESET) {
                llResetScript();
            }
        }

        //  The changed() message tells us when somebody sits or stands up

        changed(integer change) {
            if (change & CHANGED_LINK) {
                key k;
                sitterEarth = sitterMoon = 0;
                if ((k = llAvatarOnLinkSitTarget(NIGHT_SIDE)) != NULL_KEY) {
                    sitterEarth = sitterLink(k);
                }
                if ((k = llAvatarOnLinkSitTarget(MOON_NIGHT)) != NULL_KEY) {
                    sitterMoon = sitterLink(k);
                }
            }
        }
    }
