/* defs.h 	 - D. Musliner
   - global defs for RTS

   ********************************************************************
   *******   Remember to put EXTERNAL in front of declarations   *******
   ********************************************************************
*/

EXTERNAL void terminate_rts();

#ifndef __DEFS_H
#define __DEFS_H

// Note cannot include primitives.h here b/c things like matchmaker include it, and if they use
// a macro like FAIL (as a code label for goto) and then the domain (like jjsim-generated one) uses FAIL
// as a feature/value, it will bork the build in really obscure way.
//#include "primitives.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ---------------------------------------------------------------
  - useful macros
--------------------------------------------------------------- */
/* #define PI 3.14159 */

#include <limits.h>

#define deg2rad(X) ((X)*PI/180.0)
#define rad2deg(X) (180.0*(X)/PI)

#define QUOTE(str) #str
#define EXPAND_AND_QUOTE(str) QUOTE(str)

#define MAX(x,y) ( (x) > (y) ? (x) : (y) )
#define MIN(x,y) ( (x) < (y) ? (x) : (y) )
#define ABS(x) ( (x) > 0 ? (x) : -(x) )
#define DISTANCE(x0,y0,x,y) (sqrt(((x)-(x0))*((x)-(x0))+((y)-(y0))*((y)-(y0))))

#define TRUE 1
#define FALSE 0

// we need C99 not old GCC to get ULLONG_MAX, but if we are in gcc then we can try to define it.


/*
#ifndef __LONG_MAX__
#if __WORDSIZE == 64
#define __LONG_MAX__ 9223372036854775807L
#else
#define __LONG_MAX__ 2147483647L
# endif // __WORDSIZE
#endif
//#undef LONG_MIN
//#define LONG_MIN (-LONG_MAX-1L)
#ifndef LONG_MAX
#define LONG_MAX __LONG_MAX__
*/

/* Minimum and maximum values a `signed long long int' can hold.  */
#ifndef __LONG_LONG_MAX__
#define __LONG_LONG_MAX__ 9223372036854775807LL
#endif

#ifndef LLONG_MAX
#define LLONG_MAX __LONG_LONG_MAX__
#endif

#ifndef ULLONG_MAX 
#ifdef ULONG_LONG_MAX
#define ULLONG_MAX ULONG_LONG_MAX
#else
#define ULLONG_MAX (LLONG_MAX * 2ULL + 1)
#endif
#endif


#if ( __WORDSIZE == 64 )	
#define COUNTER_TYPE unsigned long long int
#define COUNTER_MAX ULLONG_MAX	
#define COUNTER_PRINTOP "%llu"
#elif ( __WORDSIZE == 32 )
#define COUNTER_TYPE unsigned long int
#define COUNTER_MAX ULONG_MAX
#define COUNTER_PRINTOP "%lu"
#else
#define COUNTER_TYPE unsigned int
#define COUNTER_MAX UINT_MAX
#define COUNTER_PRINTOP "%ud"
#endif

/* ---------------------------------------------------------------
This COUNTER stuff automates two-level counters so that we can count up to really high
numbers and print them out, to support very long duration runs, even on lower-wordsize machines.
When you DEFCOUNTER(foo) it creates both foo and foo2, the modulo-COUNTER_MAX "second level" counter.
--------------------------------------------------------------- */



#define DEFCOUNTER(x) COUNTER_TYPE x, x##2;
#define RESETCOUNTER(x) {x=0; x##2=0;}
#define INCF(x) if (x==COUNTER_MAX) {x=0;x##2++;} else x++;
// Note these two only allow values <COUNTER_MAX in the addend.. you cannot add another COUNTER, that need ADDCOUNTERS
#define COUNTERINCF(orig,addend) if (orig > 0 && addend > COUNTER_MAX - orig) { orig##2++; orig+=(addend-(COUNTER_MAX-orig)); } else orig+=addend;
#define INCFCOUNTER(orig,addend) if (orig > 0 && addend > COUNTER_MAX - orig) { orig##2++; orig+=(addend-(COUNTER_MAX-orig)); } else orig+=addend;
#define ADDCOUNTERS(orig,addend) {orig##2+=addend##2; if (orig > 0 && addend > COUNTER_MAX - orig) { orig##2++; orig+=(addend-(COUNTER_MAX-orig)); } else orig+=addend;}
#define COPYCOUNTER(from,to) {to=from;to##2=from##2;}
#define SETCOUNTER(to,from) {to=from;to##2=from##2;}	// same as above w/ args reversed
#define PRINTCOUNTER(x) printf(COUNTER_PRINTOP,x)
#define DUMPCOUNTER(s,x) {printf("%s: ",s); if (x##2>0) {PRINTCOUNTER(x##2); printf("*2^%d plus ", __WORDSIZE); PRINTCOUNTER(x); printf("\n");} else {PRINTCOUNTER(x); printf("\n");}}
#define COUNTERLT(a,b) ((a##2<b##2) || (a##2==b##2 && a<b))
#define COUNTERLTE(a,b) ((a##2<b##2) || (a##2==b##2 && a<=b))
#define COUNTERGT(a,b) ((a##2>b##2) || (a##2==b##2 && a>b))
#define COUNTERGTE(a,b) ((a##2>b##2) || (a##2==b##2 && a>=b))

#include "debug.h"

/* ---------------------------------------------------------------
  - socket communication stuff.
--------------------------------------------------------------- */
#define MAX_HOSTNAME_LENGTH 100
#define MAX_FILE_NAME_LENGTH 100

#define MAXLEN 10000

EXTERNAL char buffer[MAXLEN];

#ifdef ACM
EXTERNAL char acm_buffer[MAXLEN];

/* Holds pending content from the ACM socket. Repeatedly passed to
   newread_socket_msg. */
EXTERNAL char pending_acm_msg[MAXLEN];

EXTERNAL char acmname[50];	/* ugh.. hack global */

EXTERNAL struct collaborator *acm_collab;

#define DEFAULT_FLTSIM_PORT 4333
EXTERNAL int flight_port, flight_socket;

EXTERNAL char plane_color[80];

EXTERNAL int flyrelative_mode;
EXTERNAL char flyrelative_name[80];
EXTERNAL float flyrelative_x;
EXTERNAL float flyrelative_y;
EXTERNAL float flyrelative_z;

EXTERNAL char platform_name[80];
EXTERNAL char weapon_type[80];
EXTERNAL char target1_name[80];
EXTERNAL char target2_name[80];
EXTERNAL char target3_name[80];
EXTERNAL char target4_name[80];

EXTERNAL char popup1_name[80];
EXTERNAL char popup2_name[80];
EXTERNAL char popup3_name[80];
EXTERNAL char popup4_name[80];

	/* gross hackery to allow jamming location pre-specd in config */
EXTERNAL float jampoint_x;
EXTERNAL float jampoint_y;

EXTERNAL int jamming;	/* should we use round robin jamming? */

EXTERNAL int num_decoys;
EXTERNAL int cur_decoy_index;

EXTERNAL char ACMhostname[MAX_HOSTNAME_LENGTH];

#endif // ACM

EXTERNAL int master;	/* am I the master of the team ? */


#define MAX_COMPOSITE_TAPS 20

EXTERNAL int standalone; 	/* flag: should we connect to AIS at all? */
EXTERNAL char tapsfilename[MAX_FILE_NAME_LENGTH];
EXTERNAL int polling;  /* flag: should the RTS poll the domain? */
EXTERNAL int comm; 	/* flag: should we connect to comm server? */
EXTERNAL int heartbeats; 	/* flag: should we init heartbeats? */
EXTERNAL int tracer_mode; 	/* flag: should we connect/talk to MscTracer tracer for live sequence diagram? */

EXTERNAL char rts_name[80];
EXTERNAL int listen_port;
EXTERNAL int port_offset;	/* 5 in old RTS; soon will be 5 or 7 depending on whether in odd/even handoff */
EXTERNAL int handoff_offset;	/* should we call an old RTS and do a handoff (and what port offset is he at) */
EXTERNAL char handoff_hostname[MAX_HOSTNAME_LENGTH];

// now in debug.h
//EXTERNAL int quiet;	/* if nonzero, dont print anything out */

EXTERNAL int myindex;	/* if there are N players accding to config, this is
			 which one of the N I am... index starting at zero */
		/* used to compute which listenport this RTS should open */

  /* if mm_hostname is NULL then don't bother connecting to it */
EXTERNAL char *mm_hostname;
EXTERNAL int mm_port;

	/* nonzero if we want to use the RTS graphics monitoring stuff */
EXTERNAL int graphics;


	/* nonzero if we want to use the schedule pair stuff,
	where newest of an odd/even pair of schedule indexes is run whenever
	either is swapped to (3 & 4 are first pair...) */
EXTERNAL int schedpair_mode;

	// nonzero if we want to have input sensor threads to the pushcache append SNAPSHOT to each of
	// their atomic updates of one or more PC sensor buffers.
EXTERNAL int snapshot_mode;

	/* nonzero if we want to run iftime taps in slacktime left by
	   a guar'd tap that doesnt use all its scheduled wcet */
EXTERNAL int run_iftimes_in_slack;

	/* the RTS can get requests to send state back up to AMP.
	   If handle_state_requests mode is on, these are handled immediately
	   when received, inside the msg-from-AMP parser which is in rts.lex.
	   The bad part about that is that the time to collect up and send the
	   state (equiv to sum of all sensor prims plus transmit overhead)
	   should be glomped into the get_new_schedule() primitive... but is
	   not at this point.
	   If handle_state_requests mode is off, then you have to put in an
	   explicit TAP saying if (STATE_REQUESTED) then SEND_STATE_TO_AMP
	   and the parser just sets the flag state_requested when it
	   sees SEND_STATE, which makes timing model hold up better and
	   allows the AMP to reason about the time reqd to collect/send state.
	   Note there would still be interactions between how often
	   get_new_schedule() is run (hence parsing the state request and
	   setting flag) and how often the explicit send-state TAP is run.
	*/
EXTERNAL int handle_state_requests, state_requested_flag;

EXTERNAL int handle_handoff_requests, handoff_requested_flag;


EXTERNAL int refinhibit;

	/* max number of argument indices given to a TAP action */
#define MAX_ACTION_ARGS 2

EXTERNAL char socc_server[20];
EXTERNAL char confidencelevel[20];

/* ---------------------------------------------------------------
  TAP structure, filled by downloading from AIS.
	- action slot is also used as count of composite elements.
--------------------------------------------------------------- */
struct TAP
{
    struct node *test_node;
    int action, action_args[MAX_ACTION_ARGS];
    int oneshot;	/* if 0, not a oneshot.  If 1, a oneshot not fired.
				if 2, a oneshot that has been fired */
    int arg_count;
    int wcet; 
#ifdef COMPOSITE_TAPS
    int composite;
    int taps[MAX_COMPOSITE_TAPS];
#endif // COMPOSITE_TAPS
    //COUNTER_TYPE last_time_fired;		// time this was last fired.  
    DEFCOUNTER(last_time_fired);		// time this was last fired.  
    // index of this TAP in tap array, for use as backpointer to record
     int index;
} ;

EXTERNAL struct TAP *new_tap, *cur_tap;

/* ---------------------------------------------------------------
 - new schedules are read into a specified element of the tap_schedules[] array.
 - TAP definitions are read in from AIS and loaded into slots of the tap_schedule's taps[] array
 - pointers to these arrays are used in the parsing functions, and these
	pointers are swapped in the executive to exchange which is active and which is not.
 - schedules are then arrays of indices into these tap arrays.
 - iftime taps are in the same arrays, with their schedule in separate array.
--------------------------------------------------------------- */

#define MAX_TAPS 50

EXTERNAL struct TAP *running_taps, *new_taps;
EXTERNAL int new_tap_index, new_sched_index, cur_sched_index,
		new_iftime_index, cur_iftime_index;

EXTERNAL int iftime_server_index ;

#define MAX_SCHED 100

EXTERNAL int *running_sched, *new_sched;

EXTERNAL int *running_iftime, *new_iftime;


struct tap_schedule
  {
    struct TAP taps[MAX_TAPS];	/* the array of TAPs */
    int sched[MAX_SCHED];	/* the array of TAP indices */
    int iftime[MAX_SCHED];	/* the array of iftime TAP indices */
  } ;

EXTERNAL int have_guaranteed_taps;	/* nonzero if there are any TAPs
					on non-iftime sched of current sched */

#define MAX_TAP_SCHEDULES 20
EXTERNAL struct tap_schedule tap_schedules[MAX_TAP_SCHEDULES];

EXTERNAL int tap_schedule_valid[MAX_TAP_SCHEDULES];

EXTERNAL int tap_schedule_timestamps[MAX_TAP_SCHEDULES];
EXTERNAL int tap_schedule_timestamp;

EXTERNAL int cur_tap_schedule_index, new_tap_schedule_index, schedule_index_changed;

#ifdef PLEXIL_RTS
EXTERNAL int stop_plexil_rts;
/* for plexil-rts schedules */
EXTERNAL char plexil_plan_files[MAX_SCHED][MAX_FILE_NAME_LENGTH];
EXTERNAL int plexil_rts_initialized;
EXTERNAL int old_plexil_plan_id;
#endif

#ifdef TIMINGINFO
/* for plexil-rts timing stats */
#ifdef PLEXIL_RTS
EXTERNAL FILE *plexil_parser_timing_stream;
//#define MAX_PARSER_TIMINGS 1000
//EXTERNAL long plexil_parser_timings[MAX_PARSER_TIMINGS];
//EXTERNAL int current_plexil_parser_timing_idx;

//struct plexil_primitive_timer {
//char primitive_name[MAXLEN];
//long primitive_run_time;
//};
EXTERNAL FILE *plexil_primitive_timing_stream;
//#define MAX_PRIMITIVE_TIMINGS 10000
//EXTERNAL struct plexil_primitive_timer plexil_primitive_timings[MAX_PRIMITIVE_TIMINGS];
//EXTERNAL int current_primitive_timing_idx;

EXTERNAL FILE *plexil_telemetry_timing_stream;
//#define MAX_TELEMETRY_TIMINGS 10000
//EXTERNAL struct plexil_primitive_timer plexil_telemetry_timings[MAX_TELEMETRY_TIMINGS];
//EXTERNAL int current_telemetry_timing_idx;

//#define MAX_PLAN_SWAP_TIMINGS 10000
EXTERNAL FILE *plexil_plan_swap_timing_stream;
//EXTERNAL long plexil_plan_swap_timings[MAX_PLAN_SWAP_TIMINGS];
//EXTERNAL int current_plan_swap_timing_idx;
EXTERNAL struct timeval last_plexil_plan_end_time;
EXTERNAL struct timeval plexil_plan_start_swap_time;
//EXTERNAL long plexil_new_plan_received_timings[MAX_PLAN_SWAP_TIMINGS];
EXTERNAL struct timeval plexil_new_plan_received_time;
//EXTERNAL int current_plexil_new_plan_received_timing_idx;
EXTERNAL unsigned long plan_swap_count;
#endif // PLEXIL_RTS
#endif // TIMINGINFO

/* ---------------------------------------------------------------
 2-dimension linked list node definition for the test syntax downloaded
 from the AIS; the RTS will perform a recursive descent execution of the
 AND/OR tree represented by these nodes.

  example:  (AND (OR foo roo) doo)      translates into

   AND  -->  OR  --> foo                where horiz links are 'member' links
             |        |                 and vertical links are 'sibling' links
             V        V
             doo     roo


 2/1   new format...
	(OR (AND (ftr val) (ftr val)...) ... )
--------------------------------------------------------------- */

#define AND 10
#define OR 11
#define NOT 12
#define PRIMITIVE 13
#define NOTYPE 0

/* - type slot is one of above values.
   - function_index slot is index of test primitive to run and compare its
	output w/ the value slot.  */

struct node
  {
        int type, function_index, value;
        struct node *sibling;
        struct node *member;
  };

typedef struct node *nodeptr;

EXTERNAL nodeptr headnode;

/* ---------------------------------------------------------------
  - defs for array of primitive functions accessed by indices, for TAP
	schedule download
--------------------------------------------------------------- */

// MAX_PRIMITIVES is auto-defined in primitives.h, from parse_trans

#ifndef MAX_PRIMITIVES
#define MAX_PRIMITIVES 100
#endif

EXTERNAL char *sourceips[MAX_PRIMITIVES];
EXTERNAL char sourceiplist[MAX_PRIMITIVES];
EXTERNAL int numbersourceips;

typedef int (*pointer_to_int_function_no_params)();

EXTERNAL pointer_to_int_function_no_params primitives[MAX_PRIMITIVES];
EXTERNAL int wcet[MAX_PRIMITIVES];

//EXTERNAL void write_msg_to_ais(char * msg);

//EXTERNAL void execute_tap_action(struct TAP *);	/* comes from parse_trans-generated primdefs.c */
EXTERNAL int really_execute_primitive(int);	/* comes from parse_trans-generated primdefs.c */

/* ----------------------------------------------------------------
  - defs for the array of sensor results used by a TAP SCHEDULE
    that includes multiple calls to a single sensor.

  - Added by:  D. Apostal, L. Forrence (SCC)  09/14/2001
----------------------------------------------------------------- */

#define NULL_SENSOR -1

EXTERNAL int sensor_cache[MAX_PRIMITIVES+1];

/* ---------------------------------------------------------------
 - prim_name[] maps #defined primitive indices to a descriptive string, for
	use in debug output.
 - value_name[] does same for values, but must subtract VAL_INDEX_OFFSET
 - 6/30/00 added this for help debugging; can remove for efficiency
if we want, poss w/ makefile option and #ifdefs...
--------------------------------------------------------------- */

EXTERNAL const char *prim_name[MAX_PRIMITIVES];
EXTERNAL const char *value_name[MAX_PRIMITIVES];

/* --------------------------------------------------------------- */

EXTERNAL int new_schedule_ready;	/* is next schedule fully downloaded? */
EXTERNAL int run_current_schedule;  /* should we keep running cur TAP sched? */
EXTERNAL int halt;			/* RTS total stop flag */

EXTERNAL int event_flag;           /* 1=Detect "bad" states; 0=already found */

/* FIXME uses of all of these below should be protected against overflow */

EXTERNAL DEFCOUNTER(taps_run);
EXTERNAL DEFCOUNTER(taps_fired);
EXTERNAL DEFCOUNTER(iftime_taps_run);
EXTERNAL DEFCOUNTER(total_taps_run);
EXTERNAL DEFCOUNTER(total_taps_fired);
EXTERNAL DEFCOUNTER(total_iftime_taps_run);
EXTERNAL DEFCOUNTER(total_schedules_run);

//EXTERNAL COUNTER_TYPE taps_run, taps_fired,
//EXTERNAL COUNTER_TYPE taps_fired,
	 //iftime_taps_run, total_taps_run, total_taps_fired,
 	 //total_iftime_taps_run, total_schedules_run;

EXTERNAL DEFCOUNTER(most_recent_data_update);

EXTERNAL char *cp;			/* vbles for lex string I/O fctns */
EXTERNAL int cpsize;

#define DEFAULT_COMM_PORT 5438
EXTERNAL int ais_socket,ais_port,comm_port,comm_socket;
EXTERNAL int handoff_socket;  // overloaded: if you are a new RTS, this is socket to old; and vice versa
EXTERNAL char ais_hostname[MAX_HOSTNAME_LENGTH];
EXTERNAL char rts_hostname[MAX_HOSTNAME_LENGTH];

EXTERNAL char configFName[MAX_FILE_NAME_LENGTH];
#define BUF_LENGTH 100000   /* size (chars) of TAP schedule input sock_buffer and p-c input buffer*/
#define END_CHAR '#'	   /* char flagging end of new TAP schedule */

EXTERNAL char sock_buffer[BUF_LENGTH], ais_pending[BUF_LENGTH],
	out_buffer[BUF_LENGTH];

/* --------------------------------------------------------------- */

#define TIME_TO_FIND_IFTIME 100

        /* this is an estimate of the overhead the RTS imposes on running
                each TAP ... the time to select & start executing it, and the
                timing overhead and minimal decision stuff to not run any
                iftime TAPs */
#define DEFAULT_RTS_OVERHEAD 0


EXTERNAL int rts_overhead;

/* ---------------------------------------------------------------
 - delay macro used to slow down RTS primitives when necessary (as in X demo)
 - we do not use usleep() anymore, b/c that leads to a Unix swapout that
	gives a min delay of 20000 usecs, regardless of param given to usleep.
--------------------------------------------------------------- */

EXTERNAL int delay_counter;

#define DELAY(X) for (delay_counter=0;delay_counter < (X); delay_counter++);

/* ---------------------------------------------------------------
 *    - defs used to support the -t[iming] option for
 *      benchmarking primitives.
 * -------------------------------------------------------------- */

EXTERNAL int timing;
EXTERNAL int prim_iterations;

/* ---------------------------------------------------------------
 * MICA OEP specific stuff.
 * -------------------------------------------------------------- */
	// sim time, rounded, filled in on each cycle of RTS.
EXTERNAL int simtime;

EXTERNAL int loitertime;	// set this to nonzero to make Uav sit around
		// note it wont alter trajectory,
		// can be set in config...

/* ---------------------------------------------------------------
 * Sleep time in microseconds after each TAP polling loop iteration
 * -------------------------------------------------------------- */

 EXTERNAL unsigned int tap_loop_sleep_time;

/* ---------------------------------------------------------------
 * semaphore stuff for proper startup of push-cache threads
 * -------------------------------------------------------------- */

#include "semaphore.h"
EXTERNAL Semaphore ready_sem;
EXTERNAL int push_cache_sockets;
EXTERNAL int push_cache_initialized;
#define PUSH_CACHE_PORT_OFFSET 1
#define WEB_GUI_PORT_OFFSET 2
#define SDT_PORT_OFFSET 3	// deprecated
#define SIM_PORT_OFFSET 3

#endif /* __DEFS_H */


