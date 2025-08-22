/* debug.h 	 - D. Musliner
 - defines simple macros for debug output
 - the ndebug* macros always print out.. so you write test printouts
	using ndebugs and turn them into optional debug stuff by removing
	initial 'n'.
 - if OPTIMIZE is defined, they are all null, for speed.
*/

#ifndef __DEBUG_H
#define __DEBUG_H

EXTERNAL int debug;	/* if nonzero, print out debug stuff */
EXTERNAL int quiet;	/* if nonzero, dont print out information */

// for the cdebug* options of varying verbosity
#define lowverbose 1
#define midverbose 2
#define highverbose 3

#define fatal_error(X) {fprintf(stdout,"RTS Error: %s\n",X);       \
                        fflush(stdout); return(-1); }

// these are "always debug" statements that never get optimized away
#define adebugs(X) { if (debug) { fprintf(stdout,"RTS [%d]: %s\n",simtime,X); fflush(stdout);}}
#define adebugd(X) { if (debug) { fprintf(stdout,"RTS [%d]: %d\n",simtime,X); fflush(stdout);}}
#define adebugsd(X,Y) { if (debug) { fprintf(stdout,"RTS [%d]: %s: %d\n",simtime,X,Y); fflush(stdout);}}
#define adebugsl(X,Y) { if (debug) { fprintf(stdout,"RTS [%d]: %s: %ld\n",simtime,X,Y); fflush(stdout);}}
#define adebugsdd(X,Y,Z) { if (debug) { fprintf(stdout,"RTS [%d]: %s: %d %d\n",simtime,X,Y,Z); fflush(stdout);}}
#define adebugss(X,Y) { if (debug) { fprintf(stdout,"RTS [%d]: %s: %s\n",simtime,X,Y); fflush(stdout);}}
#define adebugsf(X,Y) { if (debug) { fprintf(stdout,"RTS [%d]: %s: %f\n",simtime,X,Y); fflush(stdout);}}

// these are "always debug" statements that never get optimized away and never check the debug flag
#define andebugs(X) { fprintf(stdout,"RTS [%d]: %s\n",simtime,X); fflush(stdout); }
#define andebugd(X) { fprintf(stdout,"RTS [%d]: %d\n",simtime,X); fflush(stdout); }
#define andebugsd(X,Y) { fprintf(stdout,"RTS [%d]: %s: %d\n",simtime,X,Y); fflush(stdout); }
#define andebugsdd(X,Y,Z) { fprintf(stdout,"RTS [%d]: %s: %d %d\n",simtime,X,Y,Z); fflush(stdout);}
#define andebugsl(X,Y) { fprintf(stdout,"RTS [%d]: %s: %ld\n",simtime,X,Y); fflush(stdout); }
#define andebugss(X,Y) { fprintf(stdout,"RTS [%d]: %s: %s\n",simtime,X,Y); fflush(stdout); }
#define andebugsf(X,Y) { fprintf(stdout,"RTS [%d]: %s: %f\n",simtime,X,Y); fflush(stdout); }

#ifndef OPTIMIZE

#define debugs(X) { if (debug) { fprintf(stdout,"RTS [%d]: %s\n",simtime,X); fflush(stdout);}}
#define debugd(X) { if (debug) { fprintf(stdout,"RTS [%d]: %d\n",simtime,X); fflush(stdout);}}
#define debugsd(X,Y) { if (debug) { fprintf(stdout,"RTS [%d]: %s: %d\n",simtime,X,Y); fflush(stdout);}}
#define debugsl(X,Y) { if (debug) { fprintf(stdout,"RTS [%d]: %s: %ld\n",simtime,X,Y); fflush(stdout);}}
#define debugsdd(X,Y,Z) { if (debug) { fprintf(stdout,"RTS [%d]: %s: %d %d\n",simtime,X,Y,Z); fflush(stdout);}}
#define debugss(X,Y) { if (debug) { fprintf(stdout,"RTS [%d]: %s: %s\n",simtime,X,Y); fflush(stdout);}}
#define debugsf(X,Y) { if (debug) { fprintf(stdout,"RTS [%d]: %s: %f\n",simtime,X,Y); fflush(stdout);}}

#define debugformat(S,X) (S ## EXPAND_AND_QUOTE(X))

#define debugsc(X,Y) { if (debug) { fprintf(stdout,"RTS [%d]: %s:",simtime,X); fprintf(stdout,COUNTER_PRINTOP,Y); fflush(stdout);}}

#define ndebugsc(X,Y) { if (debug) { fprintf(stdout,"RTS [%d]: %s:",simtime,X); fprintf(stdout,COUNTER_PRINTOP,Y); fflush(stdout);}}

//#define debugsc(X,Y) { if (debug) { fprintf(stdout,debugformat("RTS [%d]: %s:",COUNTER_PRINTOP),simtime,X,Y); fflush(stdout);}}
//#define ndebugsc(X,Y) { if (debug) { fprintf(stdout,debugformat("RTS [%d]: %s:",COUNTER_PRINTOP),simtime,X,Y); fflush(stdout);}}

//#define debugsc(X,Y) { if (debug) { fprintf(stdout,"RTS [%d]: %s:" ## QUOTE(COUNTER_PRINTOP),simtime,X,Y); fflush(stdout);}}
//#define ndebugsc(X,Y) { if (debug) { fprintf(stdout,"RTS [%d]: %s:" ## QUOTE(COUNTER_PRINTOP),simtime,X,Y); fflush(stdout);}}

 // these are same as above but with 'n' beforehand, meaning 'never check the debug flag, just print'
 // these are useful when prototyping code that you want to see debugging for without turning on everything else,
 // and then when it is stable, you can easily just delete the 'n' to make them silent most of the time, but
 // still in-place for future debugging.
#define ndebugs(X) { fprintf(stdout,"RTS [%d]: %s\n",simtime,X); fflush(stdout); }
#define ndebugd(X) { fprintf(stdout,"RTS [%d]: %d\n",simtime,X); fflush(stdout); }
#define ndebugsd(X,Y) { fprintf(stdout,"RTS [%d]: %s: %d\n",simtime,X,Y); fflush(stdout); }
#define ndebugsdd(X,Y,Z) { fprintf(stdout,"RTS [%d]: %s: %d %d\n",simtime,X,Y,Z); fflush(stdout);}
#define ndebugsl(X,Y) { fprintf(stdout,"RTS [%d]: %s: %ld\n",simtime,X,Y); fflush(stdout); }
#define ndebugss(X,Y) { fprintf(stdout,"RTS [%d]: %s: %s\n",simtime,X,Y); fflush(stdout); }
#define ndebugsf(X,Y) { fprintf(stdout,"RTS [%d]: %s: %f\n",simtime,X,Y); fflush(stdout); }

//if the passed in value is > whatever debug is set as print it out
#define cdebugs(X,C) { if (debug >= C) { fprintf(stdout,"RTS [%d]: %s\n",simtime,X); fflush(stdout);}}
#define cdebugd(X,C) { if (debug >= C) { fprintf(stdout,"RTS [%d]: %d\n",simtime,X); fflush(stdout);}}
#define cdebugsd(X,Y,C) { if (debug >= C) { fprintf(stdout,"RTS [%d]: %s: %d\n",simtime,X,Y); fflush(stdout);}}
#define cdebugsdd(X,Y,Z,C) { if (debug >= C) { fprintf(stdout,"RTS [%d]: %s: %d %d\n",simtime,X,Y,Z); fflush(stdout);}}
#define cdebugss(X,Y,C) { if (debug >= C) { fprintf(stdout,"RTS [%d]: %s: %s\n",simtime,X,Y); fflush(stdout);}}
#define cdebugsf(X,Y,C) { if (debug >= C) { fprintf(stdout,"RTS [%d]: %s: %f\n",simtime,X,Y); fflush(stdout);}}

#else

#define debugs(X)
#define debugd(X)
#define debugsd(X,Y)
#define debugss(X,Y)
#define debugsf(X,Y)
#define ndebugs(X)
#define ndebugd(X)
#define ndebugsd(X,Y)
#define ndebugss(X,Y)
#define ndebugsf(X,Y)
#define cdebugs(X,C)
#define cdebugd(X,C)
#define cdebugsd(X,Y,C)
#define cdebugsdd(X,Y,Z,C)
#define cdebugss(X,Y,C)
#define cdebugsf(X,Y,C)


#endif /* OPTIMIZE */

#endif /* __DEBUG_H */
