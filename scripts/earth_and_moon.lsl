    /*              Fourmilab Earth and Moon

                         by John Walker
                    https://www.fourmilab.ch/
                    fourmilab in Second Life

        This program is licensed under a Creative Commons
        Attribution-ShareAlike 4.0 International License.
            http://creativecommons.org/licenses/by-sa/4.0/
        Please see the License section in the "Fourmilab
        Earth and Moon Help" notecard included in the object
        for details.  */

    integer commandChannel = 2173;      // Command channel in chat
    integer commandH;                   // Handle for command channel
    key whoDat = NULL_KEY;              // Avatar who sent command
    integer restrictAccess = 2;         // Access restriction: 0 none, 1 group, 2 owner
    integer DialogueChannel = -982449714; // Base of dialogue reply channel
    //  Product's information page on the Marketplace
    string productInfoPage = "https://marketplace.secondlife.com/p/Fourmilab-Earth-and-Moon/18452910";

    float MoonSemiMaj = 384401.0;       // Semi-major axis of Moon's orbit
    float EarthRad = 6378.14;           // Earth's equatorial radius, km (IAU 1976)
    integer spinning = FALSE;           // Is the globe spinning ?
    integer showEarthLegend = TRUE;     // Show floating text above the Earth globe ?
    integer showMoonLegend = TRUE;      // Show floating text above the Moon globe ?
    integer UPDATE_TERMINATOR = 60;     // How often do we update the terminator's position ?
    float updateAnimation = 0.1;        // How often do we update when animating ?
    list timeani;                       // Animation date and time
    float anistep = 0.01;               // Animation step in days
    float moonMeanDist = 2;             // Moon's mean display distance in globe radii
    integer timeMode = 1;               // Time mode:  0 = Stop, 1 = Real time, 2 = Animate, 3 = Step
    integer cardinal = TRUE;            // Show cardinal points on textures
    integer fixSun = TRUE;              // Fix Sun, rotate Earth ?
    integer inclineEarth = TRUE;        // If Sun fixed, incline Earth ?
    integer showMoon = TRUE;            // Show the Moon ?
    integer showNight = TRUE;           // Are we showing night sides ?

    key toucher;                        // ID of user who touched us
    integer dialogueChannel;            // Channel we use to communicate with dialogue
    integer listenH;                    // Handle for listening to dialogue responses
    integer listening = FALSE;          // Do we have an active listener ?
    float listenTimer;                  // Timeout on dialogue listener

    //  Link indices for child prims of linked object set

    integer BASE = 1;
    integer DAY_SIDE = 2;
    integer NIGHT_SIDE = 3;
    integer MOON_GLOBE = 4;
    integer MOON_NIGHT = 5;

    //  Link message command codes

    integer LM_INIT = 0;                // Initialise internal state
//    integer LM_EARTH_LEGEND = 1;      // Update Earth legend
//    integer LM_MOON_LEGEND = 2;       // Update Moon legend
//    integer LM_STAT = 3;              // Print status
    integer LM_MESSAGE = 4;             // Print canned message
    integer LM_DIALOGUE = 5;            // Display dialogue
    integer LM_LEGEND_VIS = 6;          // Set legend visibility
    integer LM_UPDATE_MODEL = 7;        // Update Earth and Moon model
    integer LM_STAT_MODEL = 8;          // Request status from Update Model
    integer LM_RESET = 9;               // Reset scripts

    string helpFileName = "Fourmilab Earth and Moon Help";  // Help file name

    /*  EDFV  --  Edit a string representation of a floating
                  point number discarding insignificant trailing
                  zeroes and decimal.  */

    string edfv(float f) {
        string s = (string) f;
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

    /*  JDATE  --   Compute Julian date and fraction from UTC
                    date and time. Warning: the mon argument is
                    a month index (0-11) as used in the Unix
                    struct tm, not a month number as in ISO
                    8601.  We return the whole day number and
                    fraction as separate floats in a list
                    because a single precision float doesn't
                    have sufficient significant digits to
                    represent Julian day and fraction.  */

    list jdate(integer year, integer mon, integer mday,
                integer hour, integer min, integer sec) {
        /*  Algorithm as given in Meeus, Astronomical Algorithms 2nd ed.,
            Chapter 7, page 61, with serious hackery to cope with the
            limits of single precision floats.  */

        integer a;
        integer b;

        integer m = mon + 1;
        integer y = year;

        if (m <= 2) {
            y--;
            m += 12;
        }

        /* Determine whether date is in Julian or Gregorian calendar based on
           canonical date of calendar reform. */

        if ((year < 1582) || ((year == 1582) && ((mon < 9) ||
            ((mon == 9) && (mday < 5))))) {
            b = 0;
        } else {
            a = y / 100;
            b = 2 - a + (a / 4);
        }

        float dwhole = llFloor(365.25 * (y + 4716)) + llFloor(30.6001 * (m + 1)) +
            mday + b - 1524;
        float dfrac = ((sec + 60 * (min + (60 * hour))) / 86400.0) - 0.5;
        if (dfrac < 0) {
            dwhole--;
            dfrac += 1;
        }

        return [ dwhole, dfrac ];
    }

    //  JDSTAMP  --  Get Julian date from an llGetTimestamp() string

    list jdstamp(string s) {
        list t = llParseString2List(s, ["-", "T", ":", "."], []);

        return jdate(llList2Integer(t, 0), llList2Integer(t, 1) - 1, llList2Integer(t, 2),
                     llList2Integer(t, 3), llList2Integer(t, 4), llList2Integer(t, 5));
    }

    /*  syncTimer  --  Set the timer so that its next tick will be
                       at a time which is an even multiple of the
                       timerInt argument.  */

    syncTimer(integer timerInt) {
        integer ut = llGetUnixTime();                   // Unix time in seconds
        integer nextTimer = timerInt - (ut % timerInt); // Seconds until next even interval
        llSetTimerEvent(nextTimer);
    }

    /*  UPDATEMODEL  --  Update the model to the current real time
                         or simulated date.  All of the actual work
                         is performed in the Update Model script.
                         We determine the date and time for which
                         the model should be displayed and invoke
                         that script's LM_UPDATE_MODEL task,
                         passing the date, time, and parameters
                         to be used in the display.  */

    updateModel() {

        /*  Obtain time for computations.  If we're animating, take
            the next animation step and return the simulated
            animation time.  Otherwise, get the real time as
            a Julian day and fraction from jdstamp().  */

        list timenow;
        if (timeMode != 1) {
            //  Using simulated time (stopped or animating)
            float anidat = llList2Float(timeani, 0);
            float anitime = llList2Float(timeani, 1);
            if ((timeMode == 2) || (timeMode == 3)) {
                anitime += anistep;
                if (anitime > 1.0) {
                    float anidays = llFloor(anitime);
                    anidat += anidays;
                    anitime -= anidays;
                }
                if (timeMode == 3) {        // If we are single-stepping...
                    timeMode = 0;           // ...stop updates after this one
                }
            }
            timeani = [ anidat, anitime ];
            timenow = timeani;
        } else {
            //  Using real time
            timenow = jdstamp(llGetTimestamp());
            timeani = timenow;              // Set as default animation time
        }

        llMessageLinked(LINK_THIS, LM_UPDATE_MODEL,
            llList2Json(JSON_ARRAY,
                [   llList2Float(timenow, 0), llList2Float(timenow, 1),
                    showEarthLegend,
                    showMoonLegend,
                    cardinal,
                    fixSun,
                    inclineEarth,
                    moonMeanDist
                ]), NULL_KEY);
    }

    //  SETROTATION  --  Start or stop a slow constant rotation of the globe

    setRotation(integer spin) {
        llTargetOmega(<0, 0, 1>, TWO_PI / 60 , spin);
        /*  What's all this, you ask?  Well, you see, when you
            Omega rotate a non-physical prim, the operation is
            performed entirely locally, in the viewer.
            Apparently, then, after stopping the rotation, if
            you want to explicitly rotate the prim (or in this
            case, a linked object) to a fixed location, such as
            the starting point, the rotation is ignored (my
            guess is because the server doesn't know the prim
            has been rotated by the viewer).  So, what we have
            to do is a little jiggle of a local rotation to
            persuade the server that it has moved, and then do
            the actual rotation to put it where we want it.  Oh,
            and one more thing: that little jiggle can't be a
            llSetLinkPrimitiveParamsFast()--it has to the full
            one that waits 200 milliseconds because apparently
            the fast variant is too fast for the server to twig
            to the fact that you've rotated it with Omega.  */
        if (!spin) {
            llSetLinkPrimitiveParams(LINK_ROOT,
                [ PRIM_ROT_LOCAL, llEuler2Rot(<0, 0,  PI_BY_TWO + 0.001>) ]);
            llSetLinkPrimitiveParamsFast(LINK_ROOT,
                [ PRIM_ROTATION, llEuler2Rot(<0, 0, PI_BY_TWO>) ]);
        }
    }

    //  tawk  --  Send a message to the interacting user in chat

    tawk(string msg) {
        if (whoDat == NULL_KEY) {
            //  No known sender.  Say in nearby chat.
            llSay(PUBLIC_CHANNEL, msg);
        } else {
            llRegionSayTo(whoDat, PUBLIC_CHANNEL, msg);
        }
    }

    //  tawkr  --  Send a canned message from those in auxiliary script

    tawkr(integer msgn) {
        llMessageLinked(LINK_THIS, LM_MESSAGE, (string) msgn, whoDat);
    }

    //  checkAccess  --  Check if user has permission to send commands

    integer checkAccess(key id) {
        return (restrictAccess == 0) ||
               ((restrictAccess == 1) && llSameGroup(id)) ||
               (id == llGetOwner());
    }

    /*  parseGlobeMode  --  Parse arguments for commands which turn
                            modes on or off for either or both globes.
                            This is used in commands like:
                                Set Legend [Earth/Moon/Both] on/off
                            It is passed the arguments for the command
                            and the index of the selector arguments.  It
                            returns a bit-coded integer as follows:
                                1   Error: do nothing
                                2   On/Off mode
                                4   Act on Earth globe
                                8   Act on Moon globe       */

    integer parseGlobeMode(list args, integer argn, integer an) {
        integer goof = FALSE;
        string which;
        string what;

        if (argn >= (an + 1)) {
            which = llList2String(args, an);
            if ((which == "on") || (which == "off")) {
                what = which;
                which = "both";
            } else if (argn >= (an + 2)) {
                what = llList2String(args, an + 1);
                if (((which != "earth") && (which != "moon") && (which != "both")) ||
                    ((what != "on") && (what != "off"))) {
                    goof = TRUE;
                }
            } else {
                goof = TRUE;
            }
        } else {
            goof = TRUE;
        }
        integer action = 1;
        if (!goof) {
            action = 0;
            if ((which == "earth") || (which == "both")) {
                if (what == "on") {
                    action = action | 6;
                } else {
                    action = action | 4;
                }
            }
            if ((which == "moon") || (which == "both")) {
                if (what == "on") {
                    action = action | 10;
                } else {
                    action = action | 8;
                }
            }
        }
        return action;
    }

    /*  parseTimeUnit  --  Parse a unit of time, returning multiple
                           of days or -1 if invalid.  */

    float parseTimeUnit(string suname) {
        float sunit = -1;

        string suabbr = llGetSubString(suname, 0, 0);
        string suabbr2 = llGetSubString(suname, 0, 1);

        if (suabbr == "d") {            // day: 1
            sunit = 1;
        } else if (suabbr == "h") {     // hour: 0.041666667
            sunit = 0.041666667;
        } else if (suabbr == "l") {     // lunation: 29.53058868 (synodic month)
            sunit = 29.53058868;
        } else if (suabbr2 == "mi") {   // minute: 0.00069444444
            sunit = 0.00069444444;
        } else if (suabbr2 == "mo") {   // month: 30
            sunit = 30;
        } else if (suabbr == "w") {     // week: 7
            sunit = 7;
        }
        return sunit;
    }

    //  processCommand  --  Process command from local chat or dialogue

    processCommand(key id, string message) {

        if (!checkAccess(id)) {
            llRegionSayTo(id, PUBLIC_CHANNEL,
                "You do not have permission to control this object.");
            return;
        }

        whoDat = id;                                // Direct chat output to sender of command

        string lmessage = llToLower(llStringTrim(message, STRING_TRIM));
        list args = llParseString2List(lmessage, [" "], []);    // Command and arguments
        string command = llList2String(args, 0);    // The command

        /*  Hack to transform abbreviated commands from the
            dialogue into full commands parsed below.  */

        if (command == "legend") {
            command = "set";
            args = [ command ] + args;
        }

        integer argn = llGetListLength(args);       // Number of arguments

        tawk(message);                  // Echo command to sender

        //  Animate                     Animate forward in time for debugging

        if (command == "animate") {
            if (timeMode == 1) {
                //  Start animation at current time
                timeani = jdstamp(llGetTimestamp());
            }
            timeMode = 2;
            llSetTimerEvent(updateAnimation);

        //  Freeze                      Stop the clock and periodic updates

        } else if (command == "freeze") {
            timeMode = 0;
            syncTimer(UPDATE_TERMINATOR);   // Keep timer running to handle timeouts

        //  Help                        Give user the help notecard

        } else if (command == "help") {
                llGiveInventory(id, helpFileName);

        //  Real [time]                 Return to real time display

        } else if ((command == "real") || (command == "realtime")) {
            timeMode = 1;               // Set real time mode
            updateModel();
            syncTimer(UPDATE_TERMINATOR);

        //  Reset                       Perform a hard restart (reset script)

        } else if (command == "reset") {
            llResetScript();            // Note that all global variables are re-initialised
            llMessageLinked(LINK_THIS, LM_RESET, "", NULL_KEY); // Reset other scripts

        //  Set                         Set parameter

        } else if (command == "set") {
            string param = llList2String(args, 1);
            string svalue = llList2String(args, 2);
            float value = (float) svalue;

            //  Set Access who              Restrict chat command access to public/group/owner

            if (param == "access") {
                if (svalue == "public") {
                    restrictAccess = 0;
                } else if (svalue == "group") {
                    restrictAccess = 1;
                } else if (svalue == "owner") {
                    restrictAccess = 2;
                } else {
                    tawkr(0);
                }

            //  Set Animation rate t        Animation update rate, t = seconds
            //  Set Animation step [n] [unit] Animation step, default n = 1, unit = day

            } else if (param == "animation") {
                if (svalue == "rate") {
                    updateAnimation = (float) llList2String(args, 3);
                    if (updateAnimation < 0.1) {
                        updateAnimation = 0.1;
                    } else if (updateAnimation > 10) {
                        updateAnimation = 10;
                    }
                   tawk("Animation rate: " + edfv(updateAnimation) + " seconds");
                } else if (svalue == "step") {
                    string astep = llList2String(args, 3);
                    float sunit = 1;        // Default unit: 1 day
                    if (argn >= 5) {
                        sunit = parseTimeUnit(llList2String(args, 4));
                    }
                    string step1 = llGetSubString(astep, 0, 0);
                    if (llSubStringIndex("0123456789", step1) >= 0) {
                        anistep = ((float) astep) * sunit;
                    } else {
                        anistep = parseTimeUnit(astep);
                    }
                    if (anistep < 0) {
                        tawkr(5);
                    } else {
                        if (anistep < 0.01) {
                            anistep = 0.01;
                        } else if (anistep > 366) {
                            anistep = 366;
                        }
                        tawk("Animation step: " + edfv(anistep) + " days");
                    }
                } else {
                    tawkr(1);
                }

            //  Set Base on/off                 Show / hide base

            } else if (param == "base") {
                if (svalue == "off") {
                    llSetLinkAlpha(BASE, 0, ALL_SIDES);
                } else if (svalue == "on") {
                    llSetLinkAlpha(BASE, 1, ALL_SIDES);
                } else {
                    tawkr(2);
                }

            //  Set Cardinal on/off             Show cardinal points on Earth, Moon, night side

            } else if (param == "cardinal") {
                cardinal = svalue == "on";
                string mTexture = "LRO_100m";
                string tTexture = "terminator_sharp";
                if (cardinal) {
                    mTexture = "Moon_cardinal_points";
                    tTexture = "terminator_cardinal_points";
                }
                llSetLinkPrimitiveParams(MOON_GLOBE, [ PRIM_TEXTURE, ALL_SIDES,
                    mTexture,
                    < 1, 1, 0 >,
                    < 0, 0, 0 >,
                    -PI_BY_TWO ]);
                list targs = [ PRIM_TEXTURE, ALL_SIDES,
                               tTexture,
                               < 1, 1, 0 >,
                               < 0.25, 0, 0 >,
                               PI_BY_TWO ];
                llSetLinkPrimitiveParams(NIGHT_SIDE, targs);
                llSetLinkPrimitiveParams(MOON_NIGHT, targs);
                updateModel();

            /*  Set Channel n                   Change command channel.  Note that
                                                the channel change is lost on a
                                                script reset.  */

            } else if (param == "channel") {
                integer newch = (integer) svalue;
                if ((newch < 2)) {
                    tawkr(3);               // Invalid channel
                } else {
                    llListenRemove(commandH);
                    commandChannel = newch;
                    commandH = llListen(commandChannel, "", NULL_KEY, "");
                    tawk("Listening on chat /" + (string) commandChannel + ".");
                }

            //  Set Date yyyy-mm-dd hh:mm:ss    Set date to civil date
            //  Set Date jjjjjjjjjj.ffff        Set date to Julian day

            } else if (param == "date") {
                integer goof = FALSE;
                if (argn >= 3) {
                    string td = llList2String(args, 2);
                    if (llSubStringIndex(td, "-") >= 0) {
                        list ymd = llParseString2List(td, ["-"], []);
                        integer yyyy;
                        integer mm;
                        integer dd;
                        integer HH = 0;
                        integer MM = 0;
                        integer SS = 0;
                        if (llGetListLength(ymd) >= 3) {
                            yyyy = (integer) llList2String(ymd, 0);
                            mm = (integer) llList2String(ymd, 1);
                            dd = (integer) llList2String(ymd, 2);
                            if (argn >= 4) {
                                string hm = llList2String(args, 3);
                                if (llSubStringIndex(hm, ":") >= 0) {
                                    list hms = llParseString2List(hm, [":"], []);
                                    HH = (integer) llList2String(hms, 0);
                                    MM = (integer) llList2String(hms, 1);
                                    SS = (integer) llList2String(hms, 2);
                                } else {
                                    goof = TRUE;
                                }
                            }
                        } else {
                            goof = TRUE;
                        }
                        if (!goof) {
                            timeani = jdate(yyyy, mm - 1, dd, HH, MM, SS);
                        }
                    } else if (llSubStringIndex(td, ".") >= 0) {
                        list jf = llParseString2List(td, ["."], []);
                        timeani = [ (float) llList2String(jf, 0),
                                    (float) ("0." + llList2String(jf, 1)) ];
                    } else {
                        goof = TRUE;
                    }
                } else {
                    goof = TRUE;
                }
                if (goof) {
                    tawkr(4);   // "Usage: set date yyyy-mm-dd hh:mm:ss / jjjjjjjj.ffff"
                } else {
                    timeMode = 0;
                    updateModel();
                }

            //  Set Fixed [Earth/Sun]                       Fix Earth or Sun in constant position

            } else if (param == "fixed") {
                fixSun = svalue == "sun";
                updateModel();

            //  Set Inclination [Earth/Sun]                 Set whether Earth or Sun inclines in Fixed

            } else if (param == "inclination") {
                inclineEarth = svalue == "earth";
                updateModel();

            //  Set Legend [Earth/Moon/Both] on/off         Display/hide floating text legend

            } else if (param == "legend") {
                integer action = parseGlobeMode(args, argn, 2);

                /*  Update our local lcopy of legend visibility.
                    This keeps us from sending unnecessary legend
                    update messages when legends are hidden.  */
                if (action & 4) {               // Earth
                    showEarthLegend = (action & 2) != 0;
                }
                if (action & 8) {               // Moon
                    showMoonLegend = (action & 2) != 0;
                }

                /*  Legend visibility is handled in the Legend script,
                    avoiding race conditions which might occur if we
                    performed it here.  */
                llMessageLinked(LINK_THIS, LM_LEGEND_VIS,
                        (string) action, whoDat);

                //  If either legend now on, update the model
                if (action & 2) {
                    updateModel();
                }

            //  Set Moon on/off             Show or hide the Moon

            } else if (param == "moon") {
                showMoon = svalue == "on";
                float alpha = 0;
                if (showMoon) {
                    alpha = 1;
                }
                list cargs = [ PRIM_COLOR, ALL_SIDES, <1, 1, 1>, alpha ];
                llSetLinkPrimitiveParamsFast(MOON_GLOBE, cargs);
                if (showNight) {
                    llSetLinkPrimitiveParamsFast(MOON_NIGHT, cargs);
                }

            //  Set Moondist radii/real     Set moon distance from globe in Earth radii

            } else if (param == "moondist") {
                vector globePos = llList2Vector(llGetLinkPrimitiveParams(DAY_SIDE,
                    [ PRIM_SIZE ]), 0);
                float globeSize = globePos.x / 2;   // Radius of Earth globe
                if (svalue == "real") {
                    value =  MoonSemiMaj / EarthRad;
                } else if (value < 1.5) {
                    value = 1.5;
                }
                //  Enforce limit so object doesn't exceed 64 metres
                if ((value * globeSize) > 64) {
                    value = 64 / globeSize;
                }
                moonMeanDist = value;
                updateModel();

            //  Set Night [Earth/Moon/Both] on/off      Display/hide night side mask

            } else if (param == "night") {
                integer action = parseGlobeMode(args, argn, 2);

                if (action & 1) {       // 1: Error
                    tawkr(7);           // "Usage: Set Night [Earth/Moon/Both] On/Off"
                } else {
                    float alpha = 0;
                    if (action & 2) {   // 2: On/off
                        alpha = 1;
                    }
                    showNight = alpha == 1;
                    list cargs = [ PRIM_COLOR, ALL_SIDES, <1, 1, 1>, alpha ];
                    if (action & 4) {   // 4: Earth
                        llSetLinkPrimitiveParamsFast(NIGHT_SIDE, cargs);
                    }
                    if ((action & 8) && showMoon) { // 8: Moon
                        llSetLinkPrimitiveParamsFast(MOON_NIGHT, cargs);
                    }
                }

                //  Set Terminator sharp/soft   Set terminator edge sharp or blurred

                } else if (param == "terminator") {
                    string ttexture = "";
                    if (svalue == "sharp") {
                        ttexture = svalue;
                    } else if (svalue == "soft") {
                        ttexture = "v40";
                    }
                    if (ttexture == "") {
                        tawkr(6);   // "Usage: Set terminator soft/sharp"
                    } else {
                        list targs = [ PRIM_TEXTURE, ALL_SIDES,
                                       "terminator_" + ttexture,
                                       < 1, 1, 0 >,
                                       < 0.25, 0, 0 >,
                                       PI_BY_TWO ];
                        llSetLinkPrimitiveParams(NIGHT_SIDE, targs);
                        llSetLinkPrimitiveParams(MOON_NIGHT, targs);
                    }

            } else {
                tawkr(8);   /* "Unknown variable.  Valid: access, animation,
                                base, cardinal, channel, date, fixed, inclination,
                                legend, moon, moondist, night, terminator" */
            }

        //  Spin                        Spin the globe

        } else if (command == "spin") {
            spinning = TRUE;
            setRotation(spinning);

        //  Stat                        Print current status

        } else if (llGetSubString(command, 0, 3) == "stat") {
            list statl = [
                llGetRootPosition(),                // 0: Root prim position
                llGetRootRotation(),                // 1: Root prim rotation
                llGetFreeMemory(),                  // 2: Main script free memory
                llGetUsedMemory() ] +               // 3: Main script used memory
                timeani                             // 4-5: Last displayed date and time
            ;
            /*  Pass on to Build Model, which will append its status
                and then forward to Legend to actually display it.  */
            llMessageLinked(LINK_THIS, LM_STAT_MODEL,
                llList2Json(JSON_ARRAY, statl), whoDat);

        //  Step                        Stop the clock and make one animation step

        } else if (command == "step") {
            timeMode = 3;
            syncTimer(UPDATE_TERMINATOR);   // Keep timer running to handle timeouts
            updateModel();

        //  Stop                        Stop spinning the globe

        } else if (command == "stop") {
            spinning = FALSE;
            setRotation(spinning);

        //  Test                        Run user-defined test

/*
        } else if (command == "test") {
            //  Add code here to run tests
             integer which = (integer) llList2String(args, 1);
*/

        } else {
                //  Report invalid command to whomever sent it
                tawkr(9);   /* "Invalid command.  Valid: Animate, Freeze, Help,
                                Real, Reset, Set, Spin, Status, Step, Stop" */
        }

        whoDat = NULL_KEY;          // This conversation is complete
    }

    /*  DEFAULT  --  This is the default state handler.  This is
                     presently the only state we have.  */

    default {

        /* At state_entry we define a channel to talk to the
           dialogue, set the globe's rotation state, and start
           the timer which will periodically update the position
           of the terminator. */

        state_entry() {

            //  Notify the other scripts to re-initialise themselves
            llMessageLinked(LINK_THIS, LM_INIT, "", NULL_KEY);

            dialogueChannel = DialogueChannel ^
                (integer) ("0x" + llGetSubString((string) llGetKey(), -7, -1));
            setRotation(spinning);
            updateModel();                      // Update model when loaded...
            syncTimer(UPDATE_TERMINATOR);       // ...and every UPDATE_TERMINATOR thereafter
            commandH = llListen(commandChannel, "", NULL_KEY, "");
            llOwnerSay("Listening on chat /" + (string) commandChannel + ".");
        }

        /* When touched, display the control dialogue.  The dialogue
           is only displayed if the toucher has permission to send
           commands in chat. */

        touch_start(integer num_detected) {
            toucher = llDetectedKey(0);         // Get ID of who touched us
            if (checkAccess(toucher)) {
                if (listening) {
                    llListenRemove(listenH);    // Remove any pre-existing listener
                }
                //  The dialogue is generated in the Legend script to save memory
                llMessageLinked(LINK_THIS, LM_DIALOGUE,
                    llList2Json(JSON_ARRAY,
                        [   dialogueChannel,    // 0
                            spinning,           // 1
                            timeMode,           // 2
                            showEarthLegend,    // 3
                            showMoonLegend ]),  // 4
                        toucher);

                listenH = llListen(dialogueChannel, "", toucher, "");
                listening = TRUE;
                listenTimer = llGetTime() + 60; // Set timeout to remove listener
            } else {
                /*  If touched by somebody without access, direct them to
                    the Marketplace listing so they can get their own.  */
                llLoadURL(toucher, llGetObjectName(), productInfoPage);
            }
        }

        /* The listen event receives and processes messages from
           the dialogue and local chat.  After processing a
           dialogue message, it removes the listener.  */

        listen(integer channel, string name, key id, string message) {
            if (channel == dialogueChannel) {
                llListenRemove(listenH);
                listening = FALSE;
            }
            processCommand(id, message);
        }

        /* The timer event has three functions.  In normal operation,
           it updates the position of the night/day terminator.  When
           running an animation, it advances the simulated time and
           updates the model for each animation step.  When the
           dialogue is displayed, it serves as a timeout to get rid
           of an orphaned dialogue when the user does not respond
           or simply walks away. */

        timer() {
            if (listening) {
                if (llGetTime() > listenTimer) {
                    listening = FALSE;
                    llListenRemove(listenH);
                    /* Unfortunately, there's no way to get rid of the
                       dialogue.  We'll just ignore any messages from it. */
                }
            }

            if (timeMode != 0) {
                updateModel();      // Periodic update of the model
            }
            if (timeMode == 2) {
                llSetTimerEvent(updateAnimation);
            } else {
                syncTimer(UPDATE_TERMINATOR);
            }
        }
    }
