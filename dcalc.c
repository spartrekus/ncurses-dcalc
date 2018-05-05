/* Platform independent parts of DCALC - Reverse Polish Notation (RPN)
 * calculator.

    Contact info:
    bhepple@freeshell.org
    http://sedumi.freeshell.org/unix

    $Id: dcalc.c,v 1.10 2002/05/21 07:27:16 bhepple Exp $

    Copyright (C) 1999-2002 Bob Hepple

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; see the file COPYING.  If not, write to
    the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
    Boston, MA 02111-1307, USA.

 */

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h> /* for pipe() */
#include <sys/types.h> /* for wait */
#include <sys/wait.h> /* for wait */
#include <time.h> /* for localtime */
#include <float.h> /* for DBL_MAX */

extern int errno;

#ifdef HAVE_CONFIG_H /* gtk & wx */
#  include "config.h"
#endif

#include "dcalc.h"

#ifdef HAS_ALGEBRAIC_MODE
/* for yacc parser: */
char *parserPointer;
extern void yyparse();
long    xSave, ySave, zSave, tSave, uSave, lSave;  /* Shadow of Integer stack */
double  xfSave,yfSave,zfSave,tfSave,ufSave,lfSave; /* Shadow of floating point stack */
char last_eval[200];
int    algebraic_mode = 0;
#endif

long    xiReg, yiReg, ziReg, tiReg, liReg;  /* Integer stack */
long    reg[NUMREGS];   /* Integer registers */

double  xfReg,yfReg,zfReg,tfReg,lfReg; /* Floating point stack */
double regf[NUMREGS],  /* Floating point registers                  */
    pi,             /* The usual pi.                             */
    d_to_r,         /* Degrees to radians factor                 */
    r_to_d;         /* Radians to degrees factor                 */
/*                                           */
BOOLEAN entering,       /* True if we are still entering into X      */
    lift_needed,    /* True if the stack needs lifting before    */
    /* more characters are put into X            */
    invert,         /* Invert flag - causes the next operation   */
    /* to be inversed eg arcsin() instead of sin */
    degree,         /* True if numbers are in degrees; else      */
    /* radians                                   */
    was_initialised = 0;

/* To make sure program is only initialised once */
BOOLEAN
last_was_fin_key;/* True if last operation was financial      */
BOOLEAN finPayAt0 = 0;

char inbuf[45],      /* Where typed characters are put            */
    *inptr,         /* Pointer into inbuf                        */
    outbuf[45],     /* General buffer used to format output      */
    zero_string[] = "0",
    div_by_zero[] = "Division by 0!";

int decplaces,	/* number of decimal places		     */
    max_digits;     /* Maximum number of digits displayable      */

int	mode, intmode, floatmode;

typedef unsigned char byte;

#ifdef EBOOKMAN
#define DIALOG_LETTERS
#define DIALOG_NUMBERS
#else
#define DIALOG_LETTERS "(Y or N)"
#define DIALOG_NUMBERS "[0-9]"
#endif

/* function prototypes: */
static void echo_char(byte c);
static void save_x(void );
#if 0
static long round(double f);
#endif
static void process_digit(COMMAND c);
static void factors(long orig);

int 
isInverted() {
    int retval = invert;

    invert = FALSE;
    print_inv();
    clear_msg();
    return retval;
}

static void fmt_ip(char *str, int x) {
    int negative, i1,i2,i3,i4;

    if (x < 0) {
	x = x & 0x7fffffff;
	negative = 1;
    } else {
	negative = 0;
    }
    i4 = x & 0xff;
    x = x >> 8;
    i3 = x & 0xff;
    x = x >> 8;
    i2 = x & 0xff;
    x = x >> 8;
    if (negative)
	x = x | 0x80;
    i1 = x & 0xff;
    sprintf(str, "%03d.%03d.%03d.%03d", i1, i2, i3, i4);
}

void fmt_bin(char *str, long x) {
    char 	inverted[50],
	reverted[50],
	*s, *t;
    BOOLEAN negative;
    
    if (x < 0) {
	negative = TRUE;
	x &= 0x7FFFFFFF;
    } else
	negative = FALSE;
    s = inverted;
    while (x != 0) {
	*(s++) = '0' + (char) (x % 2);
	x /= 2;
    }
    *s = ASCIIZ;
    if (negative) {
	while (strlen(inverted) < 31) {
	    *(s++) = '0';
	    *s = ASCIIZ;
	}
	*(s++) = '1';
    }
    if (s == inverted) /* x==0 */
	strcpy(str, zero_string);
    else {
	t = reverted;
	while (s != inverted)
	    *(t++) = *(--s);
	*t = ASCIIZ;
	sprintf(str, "%32.32s", reverted);
    }
}

void fmt_base(char *s, long x, double xf) {
    byte 	format[20];
    byte 	*p;
    union 	frig {
	long j;
	byte c[4];
    } frigger;
    int 	i, leading_zero;
    
    if (mode == PROG) {
	switch (intmode) {
	case ASCIIM:
	    frigger.j = x;
	    for (i = 0; i < 4; i++) {
#if REVERSE_BYTES
		format[3 - i] = frigger.c[i];
#else
		format[i] = frigger.c[i];
#endif
	    }
	    format[4] = 0;
#if PCSPECIFIC
	    p = format;
	    for (i = 0; i < 4; i++, p++)
		if (*p == 0)
		    *s++ = ' ';
		else
		    *s++ = *p;
	    *s = 0;
#else
	    /* Convert to canonical form */
	    p = format;
	    leading_zero = 1;
	    for (i = 0; i < 4; i++, p++) {
		if ((i < 3) && (*p == 0) && leading_zero)
		    continue;
		leading_zero = 0;
		if (*p & 0x80) {
		    strcpy(s, "a-");
		    s += 2;
		    *p &= 0x7f;
		}
		switch (*p) {
		case 0:
		    strcpy(s, "^spc");
		    s += 4;
		    break;
		case 7:
		    strcpy(s, "bell");
		    s += 4;
		    break;
		case 8:
		    strcpy(s, "bs");
		    s += 2;
		    break;
		case 9:
		    strcpy(s, "tab");
		    s += 3;
		    break;
		case 10:
		    strcpy(s, "lf");
		    s += 2;
		    break;
		case 13:
		    strcpy(s, "cr");
		    s += 2;
		    break;
		case 27:
		    strcpy(s, "esc");
		    s += 3;
		    break;
		case 32:
		    strcpy(s, "spc");
		    s += 3;
		    break;
		case 0x7F:
		    strcpy(s, "del");
		    s += 3;
		    break;
		default:
		    if (*p < 32) {
			*s++ = '^';
			*s++ = *p+0x60;
		    } else
			*s++ = *p;
		}
		*s++ = ' ';
	    }
	    *s = 0;
#endif
	    break;
	
	case BIN:
	    fmt_bin(s, x);
	    break;
	
	case IP:
	    fmt_ip(s, x);
	    break;

	case OCT:
	    if (x == 0L)
		strcpy(s, zero_string);
	    else
		sprintf(s, "%32.1lo", x);
	    break;
	case DEC:
	    if (x == 0L)
		strcpy(s, zero_string);
	    else
		sprintf(s, "%32.1ld", x);
	    break;
	case HEX:
	    if (x == 0L)
		strcpy(s, zero_string);
	    else
		sprintf(s, "%32.1lx", x);
	    break;
	}
    } else {
	if (xf == 0.0) {
	    strcpy(s, "0.0");
	    return;
	}

	if (floatmode == SCI) {
	    sprintf(format, "%%32.%dE", decplaces);
	    sprintf(s, format, xf);
	} else if (floatmode == ENG) {
	    double mant;
	    int isNeg = 0, exp;
	    char *e, outb[80];

	    if (xf < 0.0)
		isNeg = 1;
	    mant = fabs(xf);
	    exp = log10(mant);
	    exp = exp / 3;
	    exp = exp * 3;
	    mant = mant * pow(0.1, (double) exp);
	    while (mant < 0.1) {
		mant = mant * 1000.0;
		exp = exp - 3;
	    }
	    if (isNeg)
		mant = -mant;
	    sprintf(outb, "%32.2E", pow(10.0, exp));
	    e = strchr(outb, 'E');
	    if (e) {
		sprintf(format, "%%32.%df%%s", decplaces);
		sprintf(s, format, mant, e);
	    } else {
		sprintf(format, "%%32.%dg", decplaces);
		sprintf(s, format, xf);
	    }
	} else { /* FIX */
	    double lowerLimit = pow(10.0, -decplaces);
#ifdef EBOOKMAN
	    double upperlimit = 1.0e15 / pow(10.0, decplaces);
#else
	    double upperlimit = 1.0e25 / pow(10.0, decplaces);
#endif
	    if ((fabs(xf) > upperlimit) || (fabs(xf) < lowerLimit)) {
		sprintf(format, "%%32.%dE", decplaces);
	    } else {
		sprintf(format, "%%32.%df", decplaces);
	    }
	    sprintf(s, format, xf);
	}
    }
}

void prep_for_output(char *string) {
    /* Put suitable separators in string */
    char 	*s, *d, sep = 0, *ft, rhs[45], lhs[45], scratch[45], prefix[3];
    int 	len, interval = 0, count, negative;
    
    prefix[0] = 0;
    if (mode == PROG) {
	switch (intmode) {
	case ASCIIM:
	    return; /* Nasty to return here - its easy to miss it when reading! */
	case IP:
	    return; /* all done by fmt_ip */
	case BIN:
	    sep = SPACE;
	    interval = 8;
	    break;
	case OCT:
	    sep = SPACE;
	    interval = 3;
	    strcpy(prefix, "0");
	    break;
	case DEC:
	    sep = COMMA;
	    interval = 3;
	    break;
	case HEX:
	    sep = SPACE;
	    interval = 2;
	    strcpy(prefix, "0x");
	    break;
	}
    } else { /* FIX or ENG */
	sep = COMMA;
	interval = 3;
    }
    
    /* Find first non-blank ... */
    s = string;
    while (*s && isspace(*s)) s++;
    if (*s == '-') {
	negative = TRUE;
	*s++ = SPACE;
    } else
	negative = FALSE;
    
    /* move rhs to safe keeping - fraction or exponent part */
    ft = NULL;
    *rhs = 0;
    if ((mode != PROG) && ((ft = strchr(s, '.')) ||
			   (ft = strchr(s, 'E')) ||
			   (ft = strchr(s, 'e')))) {
	strcpy(rhs, ft);
	*ft = 0;
    }
    
    /* add separators to the integer part ... */
    strcpy(lhs, s);
    len = strlen(s);
    if (len > interval) {
	count = 0;
	d = scratch;
	s = s + len - 1;
	while (--len >= 0) {
	    if (count++ == interval) {
		*d++ = sep;
		count = 1;
	    }
	    *d++ = *s--;
	}
	*d = 0;
	s = scratch + strlen(scratch) - 1;
	d = lhs;
	while (s >= scratch)
	    *d++ = *s--;
	*d = 0;
    }
    
    /* Now reassemble ... */
    d = string;
    if (negative) {
	*d++='-';
    }
    *d = 0;
    strcpy(d, prefix);
    strcat(d, lhs);
    strcat(d, rhs);
}

long 
asc2int(char *s) {
    union 	frig {
	long j;
	byte c[4];
    } frigger;
    int 	i;
    
    for (i = 0; i < 4; i++)
#if REVERSE_BYTES
	frigger.c[3 - i] = *s++;
#else
    frigger.c[i] = *s++;
#endif
    return(frigger.j);
}

#if 0
long 
bin2int(char *s) {
    char 	*t;
    long 	accum;
    
    accum = 0;
    t = s;
    while (*t)
	accum = 2 * accum + (*(t++) - '0');
    return(accum);
}
#endif

long 
ip2int(char *s) {
    int index, i[4], leftBitSet = 0;
    long retval;

    if (!s || !*s)
	return 0;

    for (index = 0; index < 4; index++) {
	char *dot = strchr(s, '.');
	i[index] = 0;
	if (dot)
	    *dot = 0;
	if (strlen(s)) {
	    i[index] = atoi(s);
	    i[index] &= 0xff;
	    s += strlen(s);
	}
	if (dot) {
	    *dot = '.';
	    s = dot + 1;
	}
    }
    if (i[0] > 0x7f) {
	leftBitSet = 1;
	i[0] &= 0x7f;
    }
    retval = i[0];
    retval = (retval << 8) | i[1];
    retval = (retval << 8) | i[2];
    retval = (retval << 8) | i[3];
    if (leftBitSet)
	retval |= 0x80000000;
    return retval;
}

/* Note that strtol appears to have a bug - it cannot accept negative
 * hex, octal or binary numbers e.g. 0xffffffff, which is -1, hence
 * the special processing */
void
parseBuffer(char *buf, long *longResult, double *doubleResult) {
    if (mode == PROG) {
	long temp = 0;
	int isNeg = 0;
	char *s, *d;

	/* remove leading 0's unless all are 0 */
	d = buf;
	s = buf;
	while (*s == '0')
	    s++;
	if (s != d) {
	    while (*s) 
		*d++ = *s++;
	    *d = 0;
	}

	switch (intmode) {
	case ASCIIM:
	    temp = asc2int(buf);
	    break;
	case IP:
	    temp = ip2int(buf);
	    break;
	case BIN:
	    if (strlen(buf) == 32 && *buf == '1') {
		d = buf; s= d + 1; while (*s) *d++=*s++; *d=0;
		isNeg = 1;
	    }
	    temp = strtol(buf, NULL, 2);
	    break;
	case OCT:
	    if (strlen(buf) == 11) {
		if (*buf == '2') {
		    d = buf; s= d + 1; while (*s) *d++=*s++; *d=0;
		    isNeg = 1;
		}
		if (*buf == '3') {
		    *buf = '1';
		    isNeg = 1;
		}
	    }
	    temp = strtol(buf, NULL, 8);
	    break;
	case DEC:
	    temp = strtol(buf, NULL, 10);
	    break;
	case HEX:
	    if (strlen(buf) == 8) {
		char b[2];
		int leftDigit;
		b[0] = *buf;
		b[1] = 0;
		leftDigit = strtol(b, NULL, 16);
		if (leftDigit & 8) {
		    *buf = (leftDigit & 7) + '0';
		    isNeg = 1;
		}
	    }
	    temp = strtol(buf, NULL, 16);
	    break;
	}
	*longResult = isNeg? temp|0x80000000: temp;
    } else
	*doubleResult = strtod(buf, NULL);
}

void stop_entering() {

    if (entering) {
	entering = FALSE;
	lift_needed = TRUE;
	parseBuffer(inbuf, &xiReg, &xfReg);
	inptr = inbuf;
	*inptr = 0;
    }
}

void push(long inx) {
    stop_entering();
    tiReg = ziReg;     
    ziReg = yiReg;     
    yiReg = xiReg;     
    xiReg = inx;   
    lift_needed = TRUE;
}

void pushf(double inxf) {
    stop_entering();
    tfReg = zfReg;
    zfReg = yfReg;
    yfReg = xfReg;
    xfReg = inxf;
    lift_needed = TRUE;
}

long pop() {
    long retval;
    stop_entering();
    retval = xiReg;    
    xiReg = yiReg;        
    yiReg = ziReg;        
    ziReg = tiReg;
    return retval;
}

double popf() {
    double retval;
    stop_entering();
    retval = xfReg;    
    xfReg = yfReg;        
    yfReg = zfReg;        
    zfReg = tfReg;
    return retval;
}

int
liftStack() {
    long 	temp;
    double 	tempf;

    if (lift_needed) {
	if (mode == PROG) {
	    temp = xiReg;
	    push(temp); 
	} else {
	    tempf = xfReg;
	    pushf(tempf);
	}
	dispnums();
	lift_needed = FALSE;
	return 1;
    } else
	return 0;
}

static void echo_char(byte c) {
    /* echo 'c' back to the screen and log in the input buffer */
    char 	*p;
    
    if ((c == BACKSPACE) && (mode != PROG || intmode != ASCIIM)) {
	if (entering && (inptr > inbuf)) {
	    inptr--;
	    *inptr = ASCIIZ;
	} else {
	    if (mode == PROG)
		xiReg = 0;
	    else
		xfReg = 0.0;
	    inptr = inbuf;
	    *inptr = ASCIIZ;
	    entering = TRUE;
	    dispnums();
	}

    } else /* Neither backspace or ASCII mode */ {
	if (!entering) {
	    liftStack();
	    entering = TRUE;
	    for (inptr = inbuf; inptr < inbuf + 4; inptr++)
		*inptr = ASCIIZ;
	    inptr = inbuf;
	}
	
	if (inptr < inbuf + max_digits)
	    if (mode == PROG && intmode == ASCIIM) {
		for (p = inbuf; p < inbuf + 3; p++)
		    *p = *(p+1);
		inbuf[3] = c;
		inptr++;
	    } else {
		*inptr++ = c;
		*inptr = ASCIIZ;
	    }
	else {
	    put_a_char(BELL);
	    msg("Too many digits!");
	}
    }
    
    if (mode == PROG && intmode == ASCIIM) {
	xiReg = asc2int(inbuf);
	fmt_base(outbuf, xiReg, xfReg);
    } else {
	strcpy(outbuf, inbuf);
    }
    prep_for_output(outbuf);
    print_x(outbuf);
}

void display() {
    prinbase();
    print_deg();
    dispnums();
    dispregs();
    invert = FALSE;
    print_inv();
}

static void save_x() {
    stop_entering();
    liReg = xiReg;
    lfReg = xfReg;
}

#if 0
static long round(double f) {
    BOOLEAN	negative;
    long 	ret;
    
    negative = FALSE;
    if (f < 0.0) {
	negative = TRUE;
	f = -f;
    }
    ret = floor(f + 0.5);
    if (negative)
	ret = -ret;
    return(ret);
}
#endif

int
exec_asciim() {
    isInverted();
    last_was_fin_key = 0;
    if (mode != PROG || intmode != ASCIIM) {
	long tempx;
	tempx = pop();
	push(tempx);
	mode = PROG;
	intmode = ASCIIM;
	msg("ASCII mode");
	os_raw_mode(1);
	max_digits = 4;

	dispnums();
	dispregs();
	prinbase();
    }
    return 0;
}
	
static
void change_floatmode(COMMAND c) {
    last_was_fin_key = 0;
    if (c != floatmode) {
	char buf[80];

	floatmode = (c == SCIFORMAT? SCI: c);
	stop_entering();
	os_raw_mode(0);
	max_digits = 20;
	sprintf(buf, "%s notation", 
		(c==ENG)? "Engineering": 
		(c==SCIFORMAT)? "Scientific":
		"Fixed point");
	msg(buf);

	dispnums();
	dispregs();
	prinbase();
    }
}

int
exec_eng() { 
    isInverted();
    last_was_fin_key = 0; 
    change_floatmode(ENG); 
    return 0;
}

int
exec_fix() { 
    isInverted();
    last_was_fin_key = 0; 
    change_floatmode(FIX); 
    return 0;
}

int
exec_sciformat() { 
    isInverted();
    last_was_fin_key = 0; 
    change_floatmode(SCIFORMAT); 
    return 0;
}
	
int
exec_places() {
    long tempx;

    isInverted();
    last_was_fin_key = 0;
    if (((tempx = places("Enter number of decimal places")) >= 0) &&
	(tempx != decplaces)) {
	char buf[80];

	stop_entering();
	decplaces = tempx;
	os_raw_mode(0);
	max_digits = 20;
	sprintf(buf, "Places set to %d", decplaces);
	msg(buf);

	dispnums();
	dispregs();
	prinbase();
    }
    return 0;
}
	
int
exec_ip() {
    isInverted();
    last_was_fin_key = 0;
    if (mode != PROG || intmode != IP) {
	long tempx;
	tempx = pop();
	push(tempx);
	mode = PROG;
	os_raw_mode(0);
	max_digits = 15;
	intmode = IP;
	msg("IP address mode");

	dispnums();
	dispregs();
	prinbase();
    }
    return 0;
}
	
int
exec_bin() {
    isInverted();
    last_was_fin_key = 0;
    if (mode != PROG || intmode != BIN) {
	long tempx;
	tempx = pop();
	push(tempx);
	mode = PROG;
	os_raw_mode(0);
	max_digits = 32;
	intmode = BIN;
	msg("Binary mode");

	dispnums();
	dispregs();
	prinbase();
    }
    return 0;
}
	
int
exec_oct() {
    isInverted();
    last_was_fin_key = 0;
    if (mode != PROG || intmode != OCT) {
	long tempx;
	tempx = pop();
	push(tempx);
	intmode = OCT;
	mode = PROG;
	os_raw_mode(0);
	max_digits = 11;
	msg("Octal mode");

	dispnums();
	dispregs();
	prinbase();
    }
    return 0;
}
	
int
exec_dec() {
    isInverted();
    last_was_fin_key = 0;
    if (mode != PROG || intmode != DEC) {
	long tempx;
	tempx = pop();
	push(tempx);
	mode = PROG;
	intmode = DEC;
	os_raw_mode(0);
	max_digits = 10;
	msg("Decimal mode");

	dispnums();
	dispregs();
	prinbase();
    }
    return 0;
}
	
int
exec_hex() {
    isInverted();
    last_was_fin_key = 0;
    if (mode != PROG || intmode != HEX) {
	long tempx;
	tempx = pop();
	push(tempx);
	mode = PROG;
	intmode = HEX;
	os_raw_mode(0);
	max_digits = 8;
	msg("Hexadecimal mode");

	dispnums();
	dispregs();
	prinbase();
    }
    return 0;
}

static	
void change_fin_sci_stat(COMMAND c) {
    last_was_fin_key = 0;
    if (mode != c) {
	char buf[80];
	mode = c;
	stop_entering();
	os_raw_mode(0);
	max_digits = 20;
	sprintf(buf, "%s mode", (c==FIN)? "Financial": (c==SCI)? "Scientific": "Statistics");
	if (c == FIN) {
	    if (floatmode != FIX)
		exec_fix();
	    if (decplaces != 2)
		decplaces = 2;
	    dispnums();
	}
	msg(buf);

	dispnums();
	dispregs();
	prinbase();
    }
}

int
exec_fin() { 
    isInverted();
    change_fin_sci_stat(FIN); 
    return 0;
}

int
exec_sci() { 
    isInverted();
    change_fin_sci_stat(SCI); 
    return 0;
}

int
exec_stat() { 
    isInverted();
    change_fin_sci_stat(STAT); 
    return 0;
}

int
exec_prog() {
    isInverted();
    last_was_fin_key = 0;
    if (mode != PROG) {
	mode = PROG;
	stop_entering();
	os_raw_mode(0);
	max_digits = 10;
	msg("Programmers mode");
    }

    dispnums();
    dispregs();
    prinbase();
    return 0;
}

static void addPrime(long cand, long *primes, int *primeMultiplier, int *numPrimes) {
    if (*numPrimes == 0 ) {
	*numPrimes = 1;
	primes[0] = cand;
	primeMultiplier[0] = 1;
    } else {
	if (primes[*numPrimes - 1] == cand) {
	    primeMultiplier[*numPrimes - 1] = primeMultiplier[*numPrimes - 1] + 1;
	} else {
	    if (*numPrimes == KMaxPrimes) {
		msg("Too many primes");
		return;
	    }
	    primes[*numPrimes] = cand;
	    primeMultiplier[*numPrimes] = 1;
	    (*numPrimes)++;
	}
    }
}

static
void factors(long orig) {
    long 	target;
    int		i, isNeg = 0;
    char	msgbuf[100];
    long	cand, rem, term;
    long 	primes[KMaxPrimes];
    int 	primeMultiplier[KMaxPrimes], numPrimes;

    if (orig < 0) {
	isNeg = 1;
	orig = -orig;
    }

    if (orig < 2) {
	msg("No prime factors");
	return;
    }

    target = orig;
    term = sqrt((double) target) + 1;
    numPrimes = 0;
    for (i = 0; i < KMaxPrimes; i++) {
	primes[i] = 0;
	primeMultiplier[i] = 0;
    }

    // check for 2's
    cand = 2;
    while (cand < term) {
	rem = target / cand;
	if (rem * cand == target) {
	    target = rem;
	    term = sqrt(target) + 1;
	    addPrime(cand, primes, primeMultiplier, &numPrimes);
	} else
	    break;
    }

    // check odd numbers
    cand = 3;
    while (cand < term) {
	rem = target / cand;
	if (rem * cand == target) {
	    target = rem;
	    term = sqrt(target)+1;
	    addPrime(cand, primes, primeMultiplier, &numPrimes);
	} else
	    cand += 2;
    }
    if (target > 1)
	addPrime(target, primes, primeMultiplier, &numPrimes);

    if (numPrimes == 1 && primeMultiplier[0] == 1) {
	sprintf(msgbuf, "%ld is prime", orig);
    } else {
#ifdef EBOOKMAN
	msgbuf[0] = 0;
#else
	sprintf(msgbuf,"%ld=", orig);
#endif
	for (i=0, target = 1; i < numPrimes; i++) {
	    char *m = msgbuf;

	    m += strlen(msgbuf);
#ifdef DEBUG
	    {
		int j;
		for (j = 0; j < primeMultiplier[i]; j++)
		    target *= primes[i];
	    }
#endif
	    sprintf(m, "%ld", primes[i]);
	    if (primeMultiplier[i] > 1) {
		m += strlen(m);
		sprintf(m, "^%d", primeMultiplier[i]);
	    }
	    if (i < numPrimes - 1) {
		strcat(m, "×");
	    }
	}
    }
    msg(msgbuf);

#ifdef DEBUG
    if (target != orig) { // ERRORCHECKING IN BETA ONLY!!!
	msg("Error!");
    }
#endif
}

static int power(int x, int n) {
    int i, p;
    switch (x) {
    case 0:
	return 0;
    case 1:
	return 1;
    default:
	switch (n) {
	case 0:
	    return 1;
	case 1:
	    return x;
	default:
	    if (n < 0 || n > 32) {
		msg("Overflow");
		return 0;
	    }
	    p = 1;
	    for (i = 1; i <= n; ++i)
		p *= x;
	    return p;
	}
    }
}

/* static void arith_int(COMMAND c) { */
int
exec_chs() {    
    long	tempx;
    double	tempxf;
    char 	*c_ptr, c_buf[45];

    isInverted();
    last_was_fin_key = 0;
    if (mode == PROG) {
	if (entering) {
	    c_ptr = inbuf;
	    strcpy(c_buf, c_ptr);
	    *c_ptr = ASCIIZ;
	    c_ptr = c_buf;
	    if (*c_ptr == '-')
		c_ptr++;
	    else
		strcat(inbuf, "-");
	    strcat(inbuf, c_ptr);
	    inptr = &inbuf[strlen(inbuf)];
	} else {
	    tempx = pop(); 
	    push(-tempx); 
	}
    } else {
	if (entering) {
	    if ((c_ptr = strchr(inbuf, 'E')))
		c_ptr++;
	    else
		c_ptr = inbuf;
	    strcpy(c_buf, c_ptr);
	    *c_ptr = ASCIIZ;
	    c_ptr = c_buf;
	    if (*c_ptr == '-')
		c_ptr++;
	    else
		strcat(inbuf, "-");
	    strcat(inbuf, c_ptr);
	    inptr = &inbuf[strlen(inbuf)];
	} else {
	    tempxf = popf();
	    pushf(-tempxf);
	}
    }
    dispnums();
    return 0;
}

/* operations on X only */
int
exec_not() { 
    long	tempx;
    double	tempxf;

    isInverted();
    save_x();
    last_was_fin_key = 0;
    if (mode == PROG) {
	tempx = pop();
	tempx = ~tempx;
	push(tempx);
    } else {
	tempxf = popf();
	pushf(tempxf);
    }
    dispnums();
    return 0;
}

int
exec_shiftl() {
    long	tempx;
    double	tempxf;

    if (isInverted())
	return exec_shiftr();
    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	tempx = tempx << 1;
	push(tempx);
    } else {
	tempxf = popf();
	pushf(tempxf);
    }
    dispnums();
    return 0;
}

int
exec_shiftr() {
    long	tempx;
    double	tempxf;

    if (isInverted())
	return exec_shiftl();
    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	tempx = tempx >> 1;
	push(tempx);
    } else {
	tempxf = popf();
	pushf(tempxf);
    }
    dispnums();
    return 0;
}

int
exec_primef() {
    long	tempx;
    double	tempxf;

    isInverted();
    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	factors(tempx);
	push(tempx);
    } else {
	tempxf = popf();
	pushf(tempxf);
    }
    dispnums();
    return 0;
}
	
int
exec_sqr() {
    long	tempx;
    double	tempxf;

    if (isInverted())
	return exec_root();

    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	tempx = tempx * tempx;
	push(tempx);
    } else {
	tempxf = popf();
	tempxf *= tempxf;
	pushf(tempxf);
    }
    dispnums();
    return 0;
}
		
int
exec_root() {
    double	tempxf;

    if (isInverted())
	return exec_sqr();

    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempxf = pop();
    } else
	tempxf = popf();

    tempxf = sqrt(tempxf);
    if (mode == PROG)
	push((long)(tempxf+0.5));
    else
	pushf(tempxf);

    dispnums();
    return 0;
}
	
int
exec_cube() {
    long	tempx;
    double	tempxf;

    if (isInverted())
	return exec_croot();

    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	tempx = tempx * tempx * tempx;
	push(tempx);
    } else {
	tempxf = popf();
	tempxf = tempxf * tempxf * tempxf;
	pushf(tempxf);
    }
    dispnums();
    return 0;
}
		
int
exec_croot() {
    double	tempxf;

    if (isInverted())
	return exec_cube();

    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempxf = pop();
    } else
	tempxf = popf();

    tempxf = pow(tempxf, 1.0 / 3.0);
    if (mode == PROG)
	push((long)(tempxf+0.5));
    else
	pushf(tempxf);

    dispnums();
    return 0;
}
	
int
exec_reci() {
    long	tempx;
    double	tempxf;

    isInverted();
    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	tempx = 0;
	push(tempx);
    } else {
	tempxf = popf();
	if (tempxf == 0.0)
	    msg(div_by_zero);
	else {
	    tempxf = 1.0 / tempxf;
	}
	pushf(tempxf);
    }
    dispnums();
    return 0;
}

/* X & Y operations */
int
exec_plus() {
    long	tempx, tempy;
    double	tempxf, tempyf;

    if (isInverted())
	return exec_minus();

    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	tempy = pop(); 
	tempx = tempx + tempy;
	push(tempx);
    } else {
	tempxf = popf();
	tempyf = popf();
	tempxf = tempxf + tempyf;
	pushf(tempxf);
    }
    dispnums();
    return 0;
}

/* case '-': */
int
exec_minus() {
    long	tempx, tempy;
    double	tempxf, tempyf;

    if (isInverted())
	return exec_plus();

    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	tempy = pop(); 
	tempx = tempy - tempx;
	push(tempx);
    } else {
	tempxf = popf();
	tempyf = popf();
	tempxf = tempyf - tempxf;
	pushf(tempxf);
    }
    dispnums();
    return 0;
}

/* case '*': */
int
exec_mult() {
    long	tempx, tempy;
    double	tempxf, tempyf;

    if (isInverted())
	return exec_divide();

    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	tempy = pop(); 
	tempx = tempx * tempy;
	push(tempx);
    } else {
	tempxf = popf();
	tempyf = popf();
	tempxf = tempxf * tempyf;
	pushf(tempxf);
    }
    dispnums();
    return 0;
}

/* case '/': */
int
exec_divide() {
    long	tempx, tempy;
    double	tempxf, tempyf;

    if (isInverted())
	return exec_mult();

    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	tempy = pop(); 
	if (tempx == 0) {
	    push(tempy);
	    msg(div_by_zero);
	} else {
	    if (tempx != 0)
		tempx = tempy / tempx;
	    else
		tempx = 0;
	}
	push(tempx);
    } else {
	tempxf = popf();
	tempyf = popf();
	if (tempxf == 0.0) {
	    pushf(tempyf);
	    msg(div_by_zero);
	} else {
	    tempxf = tempyf / tempxf;
	}
	pushf(tempxf);
    }
    dispnums();
    return 0;
}

/* case '^': */
int
exec_xor() {
    long	tempx, tempy;

    isInverted();
    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	tempy = pop(); 
	tempx ^= tempy;
	push(tempx);
    } else {
    }
    dispnums();
    return 0;
}

/* case '|': */
int
exec_or() {
    long	tempx, tempy;

    isInverted();
    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	tempy = pop(); 
	tempx |= tempy;
	push(tempx);
    } else {
    }
    dispnums();
    return 0;
}

/* case '&': */
int
exec_and() {
    long	tempx, tempy;

    isInverted();
    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	tempy = pop(); 
	tempx &= tempy;
	push(tempx);
    } else {
    }
    dispnums();
    return 0;
}

/* case '<': */
int
exec_shiftyl() {
    long	tempx, tempy;

    if (isInverted())
	return exec_shiftyr();

    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	tempy = pop(); 
	tempy <<= tempx;
	tempx = tempy;
	push(tempx);
    } else {
    }
    dispnums();
    return 0;
}

/* case '>': */
int
exec_shiftyr() {
    long	tempx, tempy;

    if (isInverted())
	return exec_shiftyl();

    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	tempy = pop(); 
	tempy >>= tempx;
	tempx = tempy;
	push(tempx);
    } else {
    }
    dispnums();
    return 0;
}

int
exec_modulus() {
    long	tempx, tempy;

    isInverted();
    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	if (tempx == 0) {
	    push(tempx);
	    msg(div_by_zero);
	} else {
	    tempy = pop(); 
	    tempy %= tempx;
	    tempx = tempy;
	    push(tempx);
	}
    } else {
    }
    dispnums();
    return 0;
}

int
exec_ytox() {
    long	tempx, tempy;
    double	tempxf, tempyf;

    isInverted();
    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
	tempx = pop();
	tempy = pop(); 
	if (tempx < 0) {
	    msg("X cannot be < 0");
	    push(tempy);
	} else
	    tempx = power(tempy, tempx);
	push(tempx);
    } else {
	tempxf = popf();
	tempyf = popf();
	tempxf = pow(tempyf, tempxf);
	pushf(tempxf);
    }
    dispnums();
    return 0;
}

int
exec_percent() {
    double	tempxf, tempyf;

    isInverted();
    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
    } else {
	tempxf = popf();
	tempyf = popf();
	pushf(tempyf); /* push y back on stack for future ops */
	if (isInverted()) {
	    if (tempxf == 0.0) {
		msg(div_by_zero);
		pushf(tempxf);
	    } else {
		tempxf = tempyf * 100.0 / tempxf;
	    }
	} else {
	    tempxf = tempxf * tempyf / 100.0;
	}
	pushf(tempxf);
    }
    dispnums();
    return 0;
}

int
exec_percentch() {
    double	tempxf, tempyf;

    isInverted();
    last_was_fin_key = 0;
    save_x();
    if (mode == PROG) {
    } else {
	tempxf = popf();
	tempyf = popf();
	pushf(tempyf); /* push y back on stack for future ops */
	if (isInverted()) {
	    if (tempxf == 0.0) {
		msg(div_by_zero);
		pushf(tempxf);
	    } else {
		tempxf = tempyf * (1.0 + tempxf / 100.0);
	    }
	} else {
	    tempxf = 100.0 * (tempxf - tempyf) / tempyf;
	}
	pushf(tempxf);
    }
    dispnums();
    return 0;
}

/* static void scientific(COMMAND c) { */
    
int
exec_sin() {
    double	tempxf;
    if (mode == PROG)
	return 0;

    last_was_fin_key = 0;
    save_x();
    tempxf = popf();

    if (isInverted()) {
	tempxf = asin(tempxf);
	if (degree)
	    tempxf *= r_to_d;
    } else {
	if (degree)
	    tempxf = sin(d_to_r * tempxf);
	else
	    tempxf = sin(tempxf);
    }
    pushf(tempxf);
    dispnums();
    return 0;
}

int
exec_asin() {
    invert = TRUE;
    return(exec_sin());
}

int
exec_cos() {
    double	tempxf;
    if (mode == PROG)
	return 0;

    last_was_fin_key = 0;
    save_x();
    tempxf = popf();

    if (isInverted()) {
	tempxf = acos(tempxf);
	if (degree)
	    tempxf *= r_to_d;
    } else {
	if (degree)
	    tempxf = cos(d_to_r * tempxf);
	else
	    tempxf = cos(tempxf);
    }
    pushf(tempxf);
    dispnums();
    return 0;
}

int
exec_acos() {
    invert = TRUE;
    return(exec_cos());
}
	
int
exec_tan() {
    double	tempxf;
    if (mode == PROG)
	return 0;

    last_was_fin_key = 0;
    save_x();
    tempxf = popf();

    if (isInverted()) {
	tempxf = atan(tempxf);
	if (degree)
	    tempxf *= r_to_d;
    } else {
	if (degree)
	    tempxf = tan(d_to_r * tempxf);
	else
	    tempxf = tan(tempxf);
    }
    pushf(tempxf);
    dispnums();
    return 0;
}

int
exec_atan() {
    invert = TRUE;
    return(exec_tan());
}
	
int
exec_sinh() {
    double	tempxf;
    if (mode == PROG)
	return 9;

    last_was_fin_key = 0;
    save_x();
    tempxf = popf();

    if (isInverted()) {
	tempxf = log(tempxf+sqrt(tempxf*tempxf + 1));
    } else {
	tempxf = 0.5*(exp(tempxf) - exp(-tempxf));
    }
    pushf(tempxf);
    dispnums();
    return 0;
}

int
exec_asinh() {
    invert = TRUE;
    return(exec_sinh());
}
	
int
exec_cosh() {
    double	tempxf;
    if (mode == PROG)
	return 0;

    last_was_fin_key = 0;
    save_x();
    tempxf = popf();

    if (isInverted()) {
	if (tempxf < 1) {
	    msg("Illegal value");
	    pushf(tempxf);
	} else {
	    tempxf = log(tempxf+sqrt(tempxf*tempxf - 1));
	}
    } else {
	tempxf = 0.5*(exp(tempxf) + exp(-tempxf));
    }
    pushf(tempxf);
    dispnums();
    return 0;
}

int
exec_acosh() {
    invert = TRUE;
    return(exec_cosh());
}
	
int
exec_tanh() {
    double	tempxf;
    if (mode == PROG)
	return 0;

    last_was_fin_key = 0;
    save_x();
    tempxf = popf();

    if (isInverted()) {
	if (tempxf == 1) {
	    msg("Illegal value");
	    pushf(tempxf);
	} else {
	    tempxf = 0.5*log((tempxf+1)/(1-tempxf));
	}
    } else {
	double p;
	p = exp(2*tempxf);
	tempxf = (p-1)/(p+1);
    }
    pushf(tempxf);
    dispnums();
    return 0;
}

int
exec_atanh() {
    invert = ~invert;
    return(exec_tanh());
}
	
int
exec_etox() {
    if (mode == PROG)
	return 0;

    if (isInverted())
	return exec_loge();

    last_was_fin_key = 0;
    save_x();
    pushf(exp(popf()));
    dispnums();
    return 0;
}
	
int
exec_loge() {
    if (mode == PROG)
	return 0;

    if (isInverted())
	return exec_etox();

    last_was_fin_key = 0;
    save_x();
    pushf(log(popf()));
    dispnums();
    return 0;
}

int
exec_10tox() {
    if (mode == PROG)
	return 0;

    if (isInverted())
	return exec_log10();

    last_was_fin_key = 0;
    save_x();
    pushf(pow(10.0, popf()));
    dispnums();
    return 0;
}

int
exec_log10() {
    if (mode == PROG)
	return 0;

    if (isInverted())
	return exec_10tox();

    last_was_fin_key = 0;
    save_x();
    pushf(log10(popf()));
    dispnums();
    return 0;
}
	
int
exec_frc() {
    double	tempxf;
    long tempy = 0;

    isInverted();
    if (mode == PROG)
	return 0;

    last_was_fin_key = 0;
    save_x();
    tempxf = popf();

    if (tempxf < 0.0) {
	tempy = 1;
	tempxf = -tempxf;
    }
    tempxf = tempxf - floor(tempxf);
    if (tempy)
	tempxf = -tempxf;
    pushf(tempxf);
    dispnums();
    return 0;
}
	
int
exec_int() {
    double	tempxf;
    long tempy = 0;

    isInverted();
    if (mode == PROG)
	return 0;

    last_was_fin_key = 0;
    save_x();
    tempxf = popf();

    if (tempxf < 0.0) {
	tempy = 1;
	tempxf = -tempxf;
    }
    tempxf = (double) floor(tempxf);
    if (tempy)
	tempxf = -tempxf;
    pushf(tempxf);
    dispnums();
    return 0;
}

int
exec_hms() { 
    double	tempxf;
    int sign = 0;
    if (mode == PROG)
	return 0;

    last_was_fin_key = 0;
    save_x();
    tempxf = popf();

    if (tempxf < 0.0) {
	sign = 1;
	tempxf = -tempxf;
    }

    if (isInverted()) { /* convert h.mmss to hours */
	long h, m, s;

	h = tempxf;
	tempxf = (tempxf - h) * 100 + 1E-10;
	m = tempxf;
	tempxf = (tempxf - m) * 100 + 1E-10;
	s = tempxf;
	tempxf = h + m / 60.0 + s / 3600.0;
	msg("HH.MMSS -> hours");
    } else { /* convert hours to h.mmss */
	long h, m, s;

	h = tempxf;
	tempxf = (tempxf - h) * 60  + 1E-10;
	m = tempxf;
	s = (tempxf - m) * 60 + 1E-10;
	tempxf = h + m / 100.0 + s / 10000.0;
	if (decplaces < 4)
	    decplaces = 4;
	if (floatmode != FIX)
	    exec_fix();
	msg("hours -> HH.MMSS");
    }
    if (sign)
	tempxf = -tempxf;
    pushf(tempxf);
    dispnums();
    return 0;
}

int
exec_dtor() {
    double	tempxf;
    if (mode == PROG)
	return 0;

    last_was_fin_key = 0;
    save_x();
    tempxf = popf();

    if (isInverted()) { 
	tempxf *= r_to_d;
	msg("radians to degrees");
    } else {
	tempxf *= d_to_r;
	msg("degrees to radians");
    }
    pushf(tempxf);
    dispnums();
    return 0;
}

int
exec_rtod() {
    invert = ~invert;
    return(exec_dtor());
}
		
int
exec_rtop() {
    double	tempxf, tempyf;
    if (mode == PROG)
	return 0;

    last_was_fin_key = 0;
    save_x();
    tempxf = popf();

    tempyf = popf();
    if (isInverted()) { /*  convert (r, theta) to (x, y) */
	double theta, r;

	theta = tempyf;
	if (degree)
	    theta = d_to_r * theta;

	r = tempxf;
	tempyf = r * sin(theta);
	tempxf = r * cos(theta);
	msg("polar to rectangular");
    } else { /*  convert (x, y) to (r, theta) */
	double theta, r;

	r = sqrt(tempxf * tempxf + tempyf * tempyf);
	theta = atan2(tempyf, tempxf);
	if (degree)
	    theta = r_to_d * theta;
	tempyf = theta;
	tempxf = r;
	msg("rectangular to polar");
    }
    pushf(tempyf);
    pushf(tempxf);
    dispnums();
    return 0;
}

int
exec_ptor() {
    invert = ~invert;
    return(exec_rtop());
}
	
static double fact(double from, double to) {
    double total, n, m;

    n = floor(from);
    m = floor(to);
    if (n <= 0 || m <= 0)
	return(1.0);

    total = 1.0;
    while (n > m) {
	total = total * n;
	n = n - 1;
    }
    return total;
}

/* static void summation(COMMAND c) { */
    
int
exec_fact() {
    double	tempxf;

    if (mode == PROG)
	return 0;

    isInverted();
    last_was_fin_key = 0;
    save_x();
    tempxf = popf();
    if (tempxf < 0) {
	msg("Illegal value");
	pushf(tempxf);
    } else {
	pushf(fact(tempxf, 1));
	dispnums();
	    
    }
    return 0;
}

int
exec_sum() {
    double	tempxf, tempyf;

    if (isInverted())
	return exec_sumr();

    last_was_fin_key = 0;
    if (mode == PROG)
	return 0;

    save_x();
    tempxf = popf();
    tempyf = popf();
    pushf(tempyf);
    pushf(tempxf);
    regf[STAT_NUM_REG]++;
    regf[STAT_SUMX_REG] += tempxf;
    regf[STAT_SUMX2_REG] += tempxf * tempxf;
    regf[STAT_SUMY_REG] += tempyf;
    regf[STAT_SUMY2_REG] += tempyf * tempyf;
    regf[STAT_SUMXY_REG] += tempxf * tempyf;
    pushf(regf[STAT_NUM_REG]);
    dispreg(STAT_NUM_REG);
    dispreg(STAT_SUMX_REG);
    dispreg(STAT_SUMY_REG);
    dispreg(STAT_SUMX2_REG);
    dispreg(STAT_SUMY2_REG);
    dispreg(STAT_SUMXY_REG);
    dispnums();
    msg("X & Y accumulated");
    return 0;
}
	
int
exec_sumr() {
    double	tempxf, tempyf;

    if (isInverted())
	return exec_sum();

    last_was_fin_key = 0;
    if (mode == PROG)
	return 0;

    if (regf[STAT_NUM_REG] > 0) {
	save_x();
	tempxf = popf();
	tempyf = popf();
	pushf(tempyf);
	pushf(tempxf);
	regf[STAT_NUM_REG]--;
	regf[STAT_SUMX_REG] -= tempxf;
	regf[STAT_SUMX2_REG] -= tempxf * tempxf;
	regf[STAT_SUMY_REG] -= tempyf;
	regf[STAT_SUMY2_REG] -= tempyf * tempyf;
	regf[STAT_SUMXY_REG] -= tempxf * tempyf;
	pushf(regf[STAT_NUM_REG]);
	dispreg(STAT_NUM_REG);
	dispreg(STAT_SUMX_REG);
	dispreg(STAT_SUMY_REG);
	dispreg(STAT_SUMX2_REG);
	dispreg(STAT_SUMY2_REG);
	dispreg(STAT_SUMXY_REG);
	dispnums();
	msg("X & Y decremented");
    }
    return 0;
}
	
int
exec_sum0() {
    isInverted();
    last_was_fin_key = 0;
    if (mode == PROG)
	return 0;
    save_x();
    regf[STAT_NUM_REG] = 0.0;
    regf[STAT_SUMX_REG]  = 0.0;
    regf[STAT_SUMX2_REG]  = 0.0;
    regf[STAT_SUMY_REG]  = 0.0;
    regf[STAT_SUMY2_REG]  = 0.0;
    regf[STAT_SUMXY_REG]  = 0.0;
    dispreg(STAT_NUM_REG);
    dispreg(STAT_SUMX_REG);
    dispreg(STAT_SUMY_REG);
    dispreg(STAT_SUMX2_REG);
    dispreg(STAT_SUMY2_REG);
    dispreg(STAT_SUMXY_REG);
    dispnums();
    msg("Accumulator registers cleared");
    return 0;
}
	
int
exec_nstat() {
    isInverted();
    last_was_fin_key = 0;
    if (mode == PROG)
	return 0;
    pushf(regf[STAT_NUM_REG]);
    msg("Number of entries");
    dispnums();
    return 0;
}
	
int
exec_means() { /* mean of x & y */
    double	tempxf, tempyf;

    isInverted();
    last_was_fin_key = 0;
    if (mode == PROG)
	return 0;

    if (regf[STAT_NUM_REG] == 0.0)
	msg("Nothing accumulated yet!");
    else {
	tempyf = regf[STAT_SUMY_REG] / regf[STAT_NUM_REG];
	pushf(tempyf);
	tempxf = regf[STAT_SUMX_REG] / regf[STAT_NUM_REG];
	pushf(tempxf);
	dispnums();
	msg("Mean of X's and of Y's");
    }
    return 0;
}
	
int
exec_mean() {
    double	tempxf;

    isInverted();
    last_was_fin_key = 0;
    if (mode == PROG)
	return 0;

    if (regf[STAT_NUM_REG] == 0.0)
	msg(div_by_zero);
    else {
	tempxf = regf[STAT_SUMX_REG] / regf[STAT_NUM_REG];
	pushf(tempxf);
	dispnums();
	msg("Mean of X's");
    }
    return 0;
}
	
int
exec_meany() {
    double tempyf;

    isInverted();
    last_was_fin_key = 0;
    if (mode == PROG)
	return 0;

    if (regf[STAT_NUM_REG] == 0.0)
	msg(div_by_zero);
    else {
	tempyf = regf[STAT_SUMY_REG] / regf[STAT_NUM_REG];
	pushf(tempyf);
	dispnums();
	msg("Mean of Y's");
    }
    return 0;
}
	
int
exec_s_dev() {
    double	tempxf, tempyf;

    isInverted();
    last_was_fin_key = 0;
    if (mode == PROG)
	return 0;

    /*  s.dev = sqrt((sum:(x^2) - (sum:(x))^2 /n) / n-1) */
    if (regf[STAT_NUM_REG] == 0.0)
	msg(div_by_zero);
    else {
	tempyf = sqrt((regf[STAT_SUMY2_REG] - regf[STAT_SUMY_REG]*regf[STAT_SUMY_REG]/regf[STAT_NUM_REG])/(regf[STAT_NUM_REG] - 1));
	pushf(tempyf);
	tempxf = sqrt((regf[STAT_SUMX2_REG] - regf[STAT_SUMX_REG]*regf[STAT_SUMX_REG]/regf[STAT_NUM_REG])/(regf[STAT_NUM_REG] - 1));
	pushf(tempxf);
	dispnums();
	msg("Std Dev of X's & Y's");
    }
    return 0;
}

int
exec_perm() {
    double	tempxf, tempyf;

    isInverted();
    last_was_fin_key = 0;
    if (mode == PROG)
	return 0;
    save_x();
    tempxf = popf();
    tempyf = popf();
    if (tempxf < 0 || tempyf < 0 || tempyf < tempxf) {
	msg("Bad values");
	pushf(tempyf);
	pushf(tempxf);
    } else {
	char buf[80];
	double n, r;

	n = floor(tempyf+.5);
	r = floor(tempxf+.5);
	pushf(fact(n, n - r));	
	dispnums();
	sprintf(buf, "Permutations (%.0f, %.0f)", r, n);
	msg(buf);
    }
    return 0;
}

int
exec_comb() {
    double	tempxf, tempyf;

    isInverted();
    last_was_fin_key = 0;
    if (mode == PROG)
	return 0;
    save_x();
    tempxf = popf();
    tempyf = popf();
    if (tempxf < 0 || tempyf < 0 || tempyf < tempxf) {
	msg("Bad values");
	pushf(tempyf);
	pushf(tempxf);
    } else {
	double n, r;
	char buf[80];

	n = floor(tempyf+.5);
	r = floor(tempxf+.5);
	if (n-r > r)
	    pushf(tempxf = fact(n, n-r) / fact(r, 1));
	else
	    pushf(tempxf = fact(n, r) / fact(n-r, 1));
	dispnums();
	sprintf(buf, "Combinations (%.0f, %.0f)", r, n);
	msg(buf);
    }
    return 0;
}

int
exec_lr() {
    last_was_fin_key = 0;
    if (mode == PROG)
	return 0;
    if (isInverted()) {
	char buf[80];

	sprintf(buf, "Linear Regression formula is: Y= %f*X + %f",
		regf[STAT_SLOPE], regf[STAT_Y_INT]);
	msg(buf);
    } else {
	if (regf[STAT_NUM_REG] < 2)
	    msg("Not enough data points");
	else {
	    double detx, dety, a, b, r2;

	    detx = regf[STAT_SUMX2_REG]*regf[STAT_NUM_REG] - 
		regf[STAT_SUMX_REG]*regf[STAT_SUMX_REG];
	    dety = regf[STAT_SUMY2_REG]*regf[STAT_NUM_REG] - 
		regf[STAT_SUMY_REG]*regf[STAT_SUMY_REG];
	    a = (regf[STAT_SUMXY_REG]*regf[STAT_NUM_REG] - 
		 regf[STAT_SUMX_REG]*regf[STAT_SUMY_REG])/detx;
	    b = (regf[STAT_SUMX2_REG]*regf[STAT_SUMY_REG] - 
		 regf[STAT_SUMX_REG]*regf[STAT_SUMXY_REG])/detx;
	    r2 = a*a*detx/dety;
	    regf[STAT_SLOPE] = a;
	    regf[STAT_Y_INT] = b;
	    regf[STAT_R2] = b;
	    pushf(r2);
	    pushf(b);
	    pushf(a);
	    dispnums();
	    msg("Slope (X), Y-intersect (Y) & correlation (Z) calculated");
	}
    }
    return 0;
}
		
int
exec_calcy() {
    double	tempxf, tempyf;

    last_was_fin_key = 0;
    if (mode == PROG)
	return 0;
    if (isInverted()) {
	tempyf = popf();
	tempxf = (tempyf -  regf[STAT_Y_INT]) / regf[STAT_SLOPE];
	msg("X calculated from Y");
    } else {
	tempxf = popf();
	tempyf = regf[STAT_SLOPE] * tempxf + regf[STAT_Y_INT];
	msg("Y calculated from X");
    } 
    pushf(tempyf);
    pushf(tempxf);
    dispnums();
    return 0;
}

static int dayFunction(double f) {
    long y, m, d, x, z;
	
    f += 0.00005; /* round up */
    y = f;
    f = (f - y) * 100;
    m = f;
    f = (f - m) * 100;
    d = f;
    if (m <= 2) {
	x = 0;
	z = y - 1;
    } else {
	x = (long) (0.4*m + 2.3);
	z = y;
    }
    return 365*y + 31 *(m - 1) + d + (int)(z/4) - x;
}

static int dayFunction30(double f, int i) {
    static int was30or31;
    int y, m, d, a1;
	
    f += 0.00005; /* round up */
    y = f;
    f = (f - y) * 100;
    m = f;
    f = (f - m) * 100;
    d = f;
	
    if (i == 1) {
	if (d == 31) {
	    a1 = 30;
	} else {
	    a1 = d;
	}
	if (d == 30 || d == 31) {
	    was30or31 = 1;
	} else {
	    was30or31 = 0;
	}
    } else {
	if (d == 31) {
	    if (was30or31) {
		a1 = 30;
	    } else {
		a1 = d;
	    }
	} else {
	    a1 = d;
	}
    }
    return 360*y + 30*m + a1;
}

/* static void financial(COMMAND c) { */
    /*
     * Financial function register allocation ...
     *
     * 5 n
     * 6 interest
     * 7 present value
     * 8 payment
     * 9 future value
     *
     * Formulae are :
     *
     * 0 = PVAL + (1 + I.S) PMT (1 - (1+I)**-N ) / I + FVAL * (1+I)**-N

     New Formula from psion version:
     y=Fval + Pval*(1 + i)**n + (1 + s*i)*pmt*((1 + i)**n - 1)/i
     y'=pmt*(1 + (1 + i)^(n-1) * n * (pval* i^2 / pmt - (1+i)/n + i + s*i^2)) / i^2
     *
     * where:
     *
     * N    = number of periods
     * PVAL = present value
     * I    = interest rate (as a fraction - user sees a percent)
     * PMT  = payment
     * FVAL = future value
     * S    = payment mode factor. 0 for payment at end of period, 1 for start.
     */

static double
calc_fval(double pval, 
	  double n, 
	  double pmt, 
	  double interest) {

    double tempxf, tempyf;

    if (fabs(interest) < 1.0e-13) {
	tempxf = -(pval + (n + finPayAt0) * pmt);
    } else {
	tempyf = pow(1.0 + interest, n);
#if 0
	if (errno) {
	    printf("Error in calc_fval: pow(): interest=%.4f%% n=%.2f\n", interest*100.0, n);
	    exit(1);
	}
#endif
	tempxf = -(pval*tempyf + (1 + finPayAt0*interest)*pmt*(tempyf - 1.0)/interest);
    }
    return tempxf;
}

static double
calc_pval(double fval, 
	  double n, 
	  double pmt, 
	  double interest) {

    double tempxf, tempyf;

    if (fabs(interest) < 1.0e-13) {
	tempxf = -(fval + (n + finPayAt0)*pmt);
    } else {
	tempyf = pow(1.0 + interest, -n);
#if 0
	if (errno) {
	    printf("Error in calc_pval: pow(): interest=%.4f%% n=%.2f\n", interest*100.0, n);
	    exit(1);
	}
#endif
	tempxf = -(fval*tempyf + (1 + finPayAt0*interest)*pmt*(1.0 - tempyf)/interest);
    }
    return tempxf;
}

static double
calc_n(double fval, 
       double pval, 
       double pmt, 
       double interest) {

    double tempxf, tempyf;

    if (fabs(interest) < 1.0e-13)
	tempxf = -(fval + pval)/pmt - finPayAt0;
    else {
	tempyf = 1.0 + interest*finPayAt0;
	tempxf = log((tempyf*pmt/interest - fval)/(pval + tempyf*pmt/interest))/log(1.0 + interest);
    }
    return tempxf;
}

static double
calc_pmt(double fval, 
	 double pval, 
	 double n, 
	 double interest) {

    double tempxf, tempyf;

    if (fabs(interest) < 1.0e-13) {
	tempxf = -(fval + pval)/(n + finPayAt0);
    } else {
	tempyf = pow(1.0 + interest, n);
#if 0
	if (errno) {
	    printf("Error in calc_pmt: pow(): interest=%.4f%% n=%.2f\n", interest*100.0, n);
	    exit(1);
	}
#endif
	tempxf = -interest*(fval + pval*tempyf)/((1.0 + interest*finPayAt0)*(tempyf - 1.0));
    }
    return tempxf;
}

int
exec_pval() {
    double	tempxf;
    double 	interest, n, pmt, fval;

    if (mode == PROG)
	return 0;

    if (isInverted()) {
	pushf(regf[FIN_PVAL_REG]);
	dispnums();
	msg("Present value recalled");
    } else if (!last_was_fin_key) {
	save_x();
	tempxf = popf();
	pushf(tempxf);
	regf[FIN_PVAL_REG] = tempxf;
	dispreg(FIN_PVAL_REG);
	dispnums();
	last_was_fin_key = TRUE;
	msg("Present value entered");
    } else {
	n = regf[FIN_NUM_REG];
	interest = regf[FIN_INT_REG] / 100.0;
	pmt = regf[FIN_PMT_REG];
	fval = regf[FIN_FVAL_REG];
	tempxf = calc_pval(fval, n, pmt, interest);
	pushf(tempxf);
	regf[FIN_PVAL_REG] = tempxf;
	dispreg(FIN_PVAL_REG);
	dispnums();
	msg("Present value computed");
    }
    return 0;
}
	
int
exec_fval() {
    double	tempxf;
    double 	interest, n, pval, pmt;

    if (mode == PROG)
	return 0;

    if (isInverted()) {
	pushf(regf[FIN_FVAL_REG]);
	dispnums();
	msg("Future value recalled");
    } else if (!last_was_fin_key) {
	save_x();
	tempxf = popf();
	pushf(tempxf);
	regf[FIN_FVAL_REG] = tempxf;
	dispreg(FIN_FVAL_REG);
	dispnums();
	last_was_fin_key = TRUE;
	msg("Final value entered");
    } else {
	n = regf[FIN_NUM_REG];
	interest = regf[FIN_INT_REG] / 100.0;
	pval = regf[FIN_PVAL_REG];
	pmt = regf[FIN_PMT_REG];
	tempxf = calc_fval(pval, n, pmt, interest);
	pushf(tempxf);
	regf[FIN_FVAL_REG] = tempxf;
	dispreg(FIN_FVAL_REG);
	dispnums();
	msg("Final value calculated");
    }
    return 0;
}
	
int
exec_pmt() {
    double	tempxf;
    double 	interest, n, pval, fval;

    if (mode == PROG)
	return 0;

    if (isInverted()) {
	pushf(regf[FIN_PMT_REG]);
	dispnums();
	msg("Payment recalled");
    } else if (!last_was_fin_key) {
	save_x();
	tempxf = popf();
	pushf(tempxf);
	regf[FIN_PMT_REG] = tempxf;
	dispreg(FIN_PMT_REG);
	dispnums();
	last_was_fin_key = TRUE;
	msg("Payment entered");
    } else {
	n = regf[FIN_NUM_REG];
	interest = regf[FIN_INT_REG] / 100.0;
	pval = regf[FIN_PVAL_REG];
	fval = regf[FIN_FVAL_REG];
	tempxf = calc_pmt(fval, pval, n, interest);
	pushf(tempxf);
	regf[FIN_PMT_REG] = tempxf;
	dispreg(FIN_PMT_REG);
	dispnums();
	msg("Payment calculated");
    }
    return 0;
}

static double 
newton_raphson_interest(double fval, 
			double pval, 
			double n, 
			double pmt, 
			double guess,
			int *converged) {

    double epsilon, last_epsilon, fy, dy, tempyf, intst;
    int count, epsilon_increasing;

    count = epsilon_increasing = 0;
    last_epsilon = epsilon = 1000.0;

    intst = 0.0;
    if (guess == 0.0)
	guess = 0.0001;

    if (fabs(fval - pval - n * pmt) > 1.0e-13) {
	while ((count < 10000) && (fabs(epsilon) > 1.0e-15)) {
	    tempyf = pow(1.0 + guess, n);
#if 0
	    if (errno) {
		printf("Error in calc_intst: pow(): guess=%.4f%% n=%.2f\n", guess*100.0, n);
		exit(1);
	    }
#endif
	    fy = fval + pval*tempyf + (1.0 + finPayAt0*guess)*pmt*(tempyf - 1.0)/guess;

	    tempyf = pow(1.0 + guess, n-1);
#if 0
	    if (errno) {
		printf("Error in calc_intst: dy: pow(): guess=%.4f%% n=%.2f\n", guess*100.0, n);
		exit(1);
	    }
#endif
	    dy = (((pval + pmt*(double)finPayAt0)*n*guess*guess + (n - 1.0)*guess*pmt - pmt)*tempyf + pmt)/(guess*guess);

	    epsilon=fy/dy;
	    guess -= epsilon;
	    if (fabs(epsilon) > fabs(last_epsilon))
		epsilon_increasing++;
	    else
		epsilon_increasing = 0;
	    if (epsilon_increasing > 5) { /* give up */
		*converged = 0;
		return intst;
	    }
	    last_epsilon = epsilon;
	    count++;
	}
	intst = guess;
    }
    *converged = 1;
    return intst;
}

/*
 * this is a financial calculation after all, so it is fair to assume
 * that interest is 0-1000% and that either pval or fval is
 * non-zero. Also n>=1.
 *
 * The initial guess must not blow up pow(1+guess,n) so initial guess < pow(L,1/n)-1
 */
static double
calc_intst(double fval, 
	   double pval, 
	   double n, 
	   double pmt,
	   int *converged) {

    int count = 0, watch_pval;
    double guess, lower_guess, upper_guess, error, intst;
    double calculated_val, good_enough_guess, val;

#define MAX_GUESSES 50
#define MAX_INTEREST_RATE 1.0e10 /* financial calculation, right? */
  
    lower_guess = intst = 0.0;
    upper_guess = pow(DBL_MAX,1.0/n)-1.0;
	
    if (upper_guess > MAX_INTEREST_RATE)
	upper_guess = MAX_INTEREST_RATE;

    /* keep an eye on pval if fval is zero: */
    watch_pval = (fabs(fval) < 1.0e-11);
    if (watch_pval && fabs(pval) < 1.0e-11) {
	*converged = 0;
	return intst;
    }

    val = watch_pval? pval: fval;
    good_enough_guess = fabs(val) * 0.40;

    /* guess high in the 'normal' case to avoid exploring the looney
	 * rates. The calculated guess (pval != 0) is naturally high and
	 * therefore a good guess
	 */
    if (fabs(pval) < 1.0e-11)
	guess = 0.20;
    else {
	guess = pow(-(fval + pmt*n)/pval, 1.0/n) - 1.0;
	guess *= 3.0; /* guess very high */
	if (guess > upper_guess)
	    guess = upper_guess;
    }
#if 0
    if (errno) {
	printf("Error in guess_interest: pow()\n");
	exit(1);
    }
#endif

    while (count < MAX_GUESSES) {
	calculated_val = watch_pval? calc_pval(fval, n, pmt, guess):
	    calc_fval(pval, n, pmt, guess);
	error = fabs(calculated_val - val);

	if (error < good_enough_guess) {
	    intst = newton_raphson_interest(fval, pval, n, pmt, guess, converged);

	    if (!*converged) {
		*converged = 0;
		return intst;
	    }

	    calculated_val = watch_pval? calc_pval(fval, n, pmt, intst):
		calc_fval(pval, n, pmt, intst);

	    if (fabs(val - calculated_val) > (fabs(val) / 1000.0)) {
		*converged = 0;
		return intst;
	    }
				
	    *converged = 1;
	    return intst;

	}

	if (fabs(upper_guess - lower_guess) < 1.0e-15) {
	    *converged = 0;
	    break;
	}

	/* need a new guess ... */
	if (calculated_val < val)
	    upper_guess = guess;
	else	
	    lower_guess = guess;
	count++;
	guess = (upper_guess + lower_guess) / 2.0;
    }

    intst = newton_raphson_interest(fval, pval, n, pmt, guess, converged);

    if (!*converged) {
	return intst;
    }

    calculated_val = watch_pval? calc_pval(fval, n, pmt, intst):
	calc_fval(pval, n, pmt, intst);

    if (fabs(val - calculated_val) > (fabs(val) / 1000.0)) {
	*converged = 0;
	return intst;
    }

    *converged = 1;
    return intst;
}
	
int
exec_intst() {
    double	tempxf;
    double 	n, pval, pmt, fval, intst;
    int converged;

    if (mode == PROG)
	return 0;

    if (isInverted()) {
	pushf(regf[FIN_INT_REG]);
	dispnums();
	msg("Interest recalled");
    } else if (!last_was_fin_key) {
	save_x();
	tempxf = popf();
	pushf(tempxf);
	regf[FIN_INT_REG] = tempxf;
	dispreg(FIN_INT_REG);
	dispnums();
	last_was_fin_key = TRUE;
	msg("Interest (%) entered");
    } else {
	n = regf[FIN_NUM_REG];
	pval = regf[FIN_PVAL_REG];
	pmt = regf[FIN_PMT_REG];
	fval = regf[FIN_FVAL_REG];
	intst = 0.0;
	if (fabs(fval + pval + (n + finPayAt0)*pmt) == 0.0) {
	    tempxf = 0.0;
	    converged = 1;
	} else {
	    /* use Newton-Raphson iteration ...
	       if we are seeking roots of f(x)=0 then
	       x_n+1 = x_n - f(x_n) / f'(x_n)

	       The tricky part is the initial guess. The relationship
	       is very unstable in 'i' and you have to get quite close
	       to have a chance of converging.

	    */

	    intst = calc_intst(fval, pval, n, pmt, &converged);
	}
	if (converged) {
	    tempxf = intst * 100.0; /* Convert to percent */
	    pushf(tempxf);
	    regf[FIN_INT_REG] = tempxf;
	    dispreg(FIN_INT_REG);
	    dispnums();
	    msg("Interest (%) computed");
	} else
	    msg("Calculation does not converge!\007");
    }
    return 0;
}
	
int
exec_nfin() {
    double	tempxf;
    double 	interest, pval, pmt, fval;

    if (mode == PROG)
	return 0;

    if (isInverted()) {
	pushf(regf[FIN_NUM_REG]);
	dispnums();
	msg("# payments recalled");
    } else if (!last_was_fin_key) {
	save_x();
	tempxf = popf();
	pushf(tempxf);
	regf[FIN_NUM_REG] = tempxf;
	dispreg(FIN_NUM_REG);
	dispnums();
	last_was_fin_key = TRUE;
	msg("# payments entered");
    } else {
	interest = regf[FIN_INT_REG] / 100.0;
	pval = regf[FIN_PVAL_REG];
	pmt = regf[FIN_PMT_REG];
	fval = regf[FIN_FVAL_REG];
	tempxf = calc_n(fval, pval, pmt, interest);
	pushf(tempxf);
	regf[FIN_NUM_REG] = tempxf;
	dispreg(FIN_NUM_REG);
	dispnums();
	msg("# payments computed");
    }
    return 0;
}

int
exec_fclr() {
    int c;

    isInverted();
    if (mode == PROG)
	return 0;

    c = dialog("Clear all finance registers?" DIALOG_LETTERS);
    if (toupper(c) == 'Y') {
	int i;
	for (i=5; i< 10; i++)
	    regf[i] = 0;
	msg("Fin registers cleared");
    }
    return 0;
}

int
exec_begin() {
    isInverted();

    if (mode == PROG)
	return 0;

    if (finPayAt0) {
	finPayAt0 = 0;
	msg("Annuity in arrears");
    } else {
	finPayAt0 = 1;
	msg("Annuity in advance");
    }
    return 0;
}

int
exec_dys() {
    double	tempxf, tempyf;
    if (mode == PROG)
	return 0;

    save_x();
    tempxf = popf();
    tempyf = popf();
    if (isInverted()) {
	tempyf = dayFunction30(tempxf,1);
	tempxf = dayFunction30(tempyf,2) - tempyf;
    } else {
	tempxf = dayFunction(tempyf) - dayFunction(tempxf);
    }
    msg("days between two dates calculated");
    if (decplaces < 4)
	decplaces = 4;
    if (floatmode != FIX)
	exec_fix();
    pushf(tempxf);
    dispnums();
    return 0;
}

int
exec_tdy() {
    double	tempxf;
    struct tm *tmbuf;
    time_t t;

    isInverted();
    if (mode == PROG)
	return 0;
    save_x();

    t = time(NULL);
    tmbuf = localtime(&t);
    tempxf = tmbuf->tm_year + 1900;
    tempxf += (tmbuf->tm_mon + 1) / 100.0;
    tempxf += tmbuf->tm_mday / 10000.0;
    pushf(tempxf);
    if (decplaces != 4)
	decplaces = 4;
    if (floatmode != FIX)
	exec_fix();
    dispnums();
    msg("Today's date");
    return 0;
}

int
exec_times12() {
    double	tempxf;

    if (isInverted())
	return exec_divide12();

    if (mode == PROG)
	return 0;
    save_x();

    tempxf = popf();
    tempxf *= 12.0;
    pushf(tempxf);
    dispnums();
    return 0;
}

int
exec_divide12() {
    double	tempxf;

    if (isInverted())
	return exec_times12();

    if (mode == PROG)
	return 0;

    save_x();
    tempxf = popf();
    tempxf /= 12.0;
    pushf(tempxf);
    dispnums();
    return 0;
}


static void process_digit(COMMAND c) {
    switch (c) {
    case BACKSPACE:
    case '0':
    case '1':
	echo_char(c);
	break;
	
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
	if (mode == PROG && intmode == BIN)
	    put_a_char(BELL);
	else
	    echo_char(c);
	break;
	
    case '8':
    case '9':
	if ((mode == PROG) && ((intmode == BIN) || (intmode == OCT)))
	    put_a_char(BELL);
	else
	    echo_char(c);
	break;
	
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'F':
	if (mode != PROG || intmode != HEX)
	    put_a_char(BELL);
	else
	    echo_char(c);
	break;
	
    case 'E':
	if (mode == PROG) {
	    if (intmode == HEX) {
		echo_char(c);
	    } else {
		put_a_char(BELL);
	    }
	} else {
	    if (strchr(inbuf, 'E') == NULL) {
		if (strlen(inbuf) == 0)
		    echo_char('1');
		echo_char(c);
	    } else {
		msg("Can't put another E in the number!");
		put_a_char(BELL);
	    }
	}
	break;
	
    case '.':
	if (mode == PROG) { /* ASCIIM never reaches here! */
	    if (intmode == IP) {
		echo_char('.');
	    } else {
		put_a_char(BELL);
	    }
	} else if (entering && strchr(inbuf, '.'))
	    put_a_char(BELL);
	else
	    echo_char(c);
	break;
	
    default:
	put_a_char(BELL);
	break;
    }
}

/* trim all white space from buf (for units) */
void trim(char *s) {
    char *d = s;
    while (*s) {
	if (!isspace(*s))
	    *d++ = *s;
	s++;
    }
    *d = 0;
}

void convertX(char *from, char *to) {
#ifdef unix
    int fd[2], pid;
#endif

    if (mode == PROG) {
	msg("No conversions in PROG mode");
	return;
    }

    trim(from);
    trim(to);

    stop_entering();

#ifdef unix
    if (pipe(fd) < 0)
	msg("Can't open a pipe.");
    else {
	double oldX = xfReg;
	char *argv[5];
	char fromarg[80];

	if (fabs(oldX) < 0.0001)
	    oldX = 1.0;

	sprintf(fromarg, "%f %s", oldX, from);
	argv[0] = "units";
	argv[1] = "-o %.20g";
	argv[2] = fromarg;
	argv[3] = to;
	argv[4] = 0;
	if ((pid = vfork()) == -1) {
	    msg("Fork failed.");
	} else if (pid == 0) { /* child - put stdout on pipe */
	    close(fd[0]); /* child does not need to read from pipe */
	    close(1); /* set stdout to the pipe */
	    dup(fd[1]);
	    close(fd[1]);
	    execvp(argv[0], argv);
	    _exit(1); /* abandon hope all ye who get here */
	} else { /* PARENT - read from pipe */
	    FILE *p;
	    close(fd[1]); /* parent does not write to pipe */
	    if ((p = fdopen(fd[0], "r")) == NULL) {
		msg("Can't read from pipe");
	    } else {
		double newX;
		int numRead = 0, status, bytesRead = 0;
		char inbuf[80];

		while (fgets(inbuf, 80, p) != NULL) {
		    bytesRead += strlen(inbuf);
		    if (!numRead)
			numRead = sscanf(inbuf, " * %lf", &newX);
		}
		wait(&status);
		if (numRead == 1 && 
		    WIFEXITED(status) && 
		    WEXITSTATUS(status) == 0) {
		    double xf;

		    xf = popf();
		    pushf(newX);
		    if (fabs(xf) < 0.0001)
			sprintf(inbuf, "Conversion factor %s to %s", from, to);
		    else
			sprintf(inbuf, "Converted %s to %s", from, to);
		    msg(inbuf);
		    dispnums();
		} else if (bytesRead)
		    /* Note: if you are tempted to catch units's error
		       message be aware that as of version 1.55 it
		       does not use stderr! */
		    msg("Bad units");
		else
		    msg("Can't find the units program - please install it!");

		fclose(p); /* no need to close fd[1] too */
	    }
	}
    }
#else
    msg("Conversions not supported here");
#endif
}

/* void storeOp(int op, int i) { */

static void
stoplus(int i) {
    if ((i >= 0) && (i < NUMREGS)) {
	char buf[80], *s;
	if (mode == PROG)
	    reg[i] += xiReg;
	else
	    regf[i] += xfReg;
	strcpy(buf, "Added to R");
	dispreg(i);
	s = buf + strlen(buf);
	sprintf(s, "%d", i);
	msg(buf);
    }
}

static void
stominus(int i) {

    if ((i >= 0) && (i < NUMREGS)) {
	char buf[80], *s;
	if (mode == PROG)
	    reg[i] -= xiReg;
	else
	    regf[i] -= xfReg;
	strcpy(buf, "Subtracted from R");
	dispreg(i);
	s = buf + strlen(buf);
	sprintf(s, "%d", i);
	msg(buf);
    }
}

static void
stotimes(int i) {
    if ((i >= 0) && (i < NUMREGS)) {
	char buf[80], *s;
	if (mode == PROG)
	    reg[i] *= xiReg;
	else
	    regf[i] *= xfReg;
	strcpy(buf, "Multiplied into R");
	dispreg(i);
	s = buf + strlen(buf);
	sprintf(s, "%d", i);
	msg(buf);
    }
}

static void
stodivide(int i) {
    if ((i >= 0) && (i < NUMREGS)) {
	char buf[80], *s;
	if (mode == PROG)
	    reg[i] /= xiReg;
	else
	    regf[i] /= xfReg;
	strcpy(buf, "Divided into R");
	dispreg(i);
	s = buf + strlen(buf);
	sprintf(s, "%d", i);
	msg(buf);
    }
}

int
exec_store() {
    int i, j;

    isInverted();
    last_was_fin_key = 0;
    stop_entering();
    i = store("Store in register [0-9] (or [+-*/] reg.)");
    j = i;
    if ((j=='+') || (j=='-') || (j=='*') || (j=='/')) {
	char buf[80];
	sprintf(buf,"Store %c in register [0-9]", j);
	i = store(buf);
	if (isdigit(i))
	    i -= '0';
	else
	    return 0;
    } else {
	j = 0;
	if (isdigit(i))
	    i -= '0';
	else
	    return 0;
    }
    stop_entering();

    if ((i >= 0) && (i < NUMREGS)) {
	char buf[80], *s;
	switch (j) {
	case '+': stoplus(i); break;
	case '-': stominus(i); break;
	case '*': stotimes(i); break;
	case '/': stodivide(i); break;
	case 0:
	    if (mode == PROG)
		reg[i] = xiReg;
	    else
		regf[i] = xfReg;
	    strcpy(buf, "Stored in R");
	    dispreg(i);
	    s = buf + strlen(buf);
	    sprintf(s, "%d", i);
	    msg(buf);
	    break;
	}
    }
    return 0;
}

int
exec_stoplus() {
    int i;

    isInverted();
    last_was_fin_key = 0;
    stop_entering();
    i = store("Store + in register [0-9]");
    if (isdigit(i))
	i -= '0';
    else
	return 0;
    stop_entering();

    stoplus(i);
    return 0;
}

int
exec_stominus() {
    int i, j;

    isInverted();
    last_was_fin_key = 0;
    stop_entering();
    i = store("Store in register [0-9] (or [+-*/] reg.)");
    j = i;
    if ((j=='+') || (j=='-') || (j=='*') || (j=='/'))
	i = store("Store in register [0-9]");
    else {
	j = 0;
	if (isdigit(i))
	    i -= '0';
	else
	    return 0;
    }
    stop_entering();
    stominus(i);
    return 0;
}

int
exec_stomultiply() {
    int i, j;

    isInverted();
    last_was_fin_key = 0;
    stop_entering();
    i = store("Store in register [0-9] (or [+-*/] reg.)");
    j = i;
    if ((j=='+') || (j=='-') || (j=='*') || (j=='/'))
	i = store("Store in register [0-9]");
    else {
	j = 0;
	if (isdigit(i))
	    i -= '0';
	else
	    return 0;
    }
    stop_entering();
    stotimes(i);
    return 0;
}

int
exec_stodivide() {
    int i, j;

    isInverted();
    last_was_fin_key = 0;
    stop_entering();
    i = store("Store in register [0-9] (or [+-*/] reg.)");
    j = i;
    if ((j=='+') || (j=='-') || (j=='*') || (j=='/'))
	i = store("Store in register [0-9]");
    else {
	j = 0;
	if (isdigit(i))
	    i -= '0';
	else
	    return 0;
    }
    stop_entering();
    stodivide(i);
    return 0;
}

int
exec_rolldown() {
    long tempx;
    double tempxf;

    isInverted();
    last_was_fin_key = 0;
    if (mode == PROG) {
	tempx = pop(); 
	tiReg = tempx;
    } else {
	tempxf = popf();
	tfReg = tempxf;
    }
    dispnums();
    return 0;
}

int
exec_clx() {
    long tempx;
    double tempxf;

    isInverted();
    last_was_fin_key = 0;
    if (mode == PROG) {
	liReg = pop(); 
	tempx = 0;
	push(tempx); 
    } else {
	lfReg = popf();
	tempxf = 0.0;
	pushf(tempxf);
    }
    lift_needed = FALSE;
    dispnums();
    return 0;
}

/* Clear stack (INV = clear Registers) */
int
exec_clr() {
    int c;

    if (isInverted()) {
	c = dialog("Clear all registers?" DIALOG_LETTERS);
	if (toupper(c) == 'Y') {
	    int i;
	    for (i=0; i< NUMREGS; i++)
		if (mode == PROG)
		    reg[i] = 0;
		else
		    regf[i] = 0;
	    msg("Registers cleared");
	}
    } else {
	last_was_fin_key = 0;
	if (mode == PROG) {
	    xiReg = yiReg = ziReg = tiReg = liReg = 0;
	} else {
	    xfReg = yfReg = zfReg = tfReg = lfReg = 0;
	}
	lift_needed = FALSE;
	dispnums();
    }
    return 0;
}

int
exec_toggle_deg() {
    isInverted();
    last_was_fin_key = 0;
    degree = !degree;
    print_deg();
    return 0;
}

int
exec_degree() {
    isInverted();
    last_was_fin_key = 0;
    degree = 1;
    print_deg();
    msg("Angles are in degrees");
    return 0;
}

int
exec_radian() {
    isInverted();
    last_was_fin_key = 0;
    degree = 0;
    print_deg();
    msg("Angles are in radians");
    return 0;
}

int
exec_e() {
    double tempxf;

    isInverted();
    last_was_fin_key = 0;
    tempxf = exp(1.0);
    pushf(tempxf);
    dispnums();
    return 0;
}

int
exec_pi() {
    long tempx;
    double tempxf;

    isInverted();
    last_was_fin_key = 0;
    tempxf = pi;
    tempx = tempxf;
    pushf(tempxf);
    dispnums();
    return 0;
}

int
exec_enter() {
    long tempx;
    double tempxf;

    isInverted();
    last_was_fin_key = 0;

    if (mode == PROG) {
	tempx = pop(); 
	push(tempx); 
	push(tempx); 
    } else {
	tempxf = popf();
	pushf(tempxf);
	pushf(tempxf);
    }
    lift_needed = FALSE;
    dispnums();
    return 0;
}

#ifdef HAS_ALGEBRAIC_MODE

/* this is for algebraic method only - it all gets popped up one register */
static void
saveStack() {
    xfSave = xfReg;
    yfSave = xfReg;
    zfSave = yfReg;
    tfSave = zfReg;
    ufSave = tfReg;
    lfSave = lfReg;
    xSave = xiReg;
    ySave = xiReg;
    zSave = yiReg;
    tSave = ziReg;
    uSave = tiReg;
    lSave = liReg;
}

int
exec_rpn() {
    isInverted();
    last_was_fin_key = 0;
    algebraic_mode = 0;
    return 0;
}

int
exec_algebraic() {
    isInverted();
    last_was_fin_key = 0;
    algebraic_mode = 1;
    return 0;
}

int
exec_eval() {
    char expr[80];

    isInverted();
    last_was_fin_key = 0;
    if ((eval(last_eval, expr) == 0)) {
	stop_entering();
	strcpy(last_eval, expr);
	parserPointer = last_eval;
	saveStack();
	yyparse();
	dispnums();
    }
    return 0;
}

int
exec_equals() {
    isInverted();
    /* last_was_fin_key = 0; */
    lift_needed = FALSE;
    if (strlen(last_eval) == 0) {
	return 0;
    } else { /* add a missing closing brace: */
	int leftBraces = 0, rightBraces = 0;
	char *s = last_eval;
	while (s && *s && (s = strchr(s, '('))) {
	    leftBraces++;
	    s++;
	}
	s = last_eval;
	while (s && *s && (s = strchr(s, ')'))) {
	    rightBraces++;
	    s++;
	}
	while (rightBraces < leftBraces) {
	    strcat(last_eval, ")");
	    rightBraces++;
	}
    }
    
    parserPointer = last_eval;
    saveStack();
    yyparse();
    dispnums();
    return 0;
}

int
exec_leftbrace() {
    isInverted();
    last_was_fin_key = 0;
    lift_needed = FALSE;
    dispnums();
    return 0;
}

int
exec_rightbrace() {
    isInverted();
    last_was_fin_key = 0;
    lift_needed = FALSE;
    dispnums();
    return 0;
}
#endif

int
exec_lastx() {
    isInverted();
    last_was_fin_key = 0;
    if (mode == PROG)
	push(liReg); 
    else
	pushf(lfReg);
    dispnums();
    return 0;
}

int
exec_quit() {
    extern int is_resident;
    int c = QUIT;

    isInverted();
    last_was_fin_key = 0;
    if (is_resident) {
	c = QUIT;
    } else {

#ifndef HAVE_CONFIG_H /* gtk & EBOOKMAN */
	c = dialog("Really quit?" DIALOG_LETTERS);
	if (toupper(c) == 'Y') {
	    c = QUIT;
	    terminate_dcalc();
	} else
	    c = 0;
	clear_msg();
#else
	terminate_dcalc();
#endif
    }
    return c;
}

/* For evaluation in algebraic expressions */
int
exec_rcl() {
    int i;
    i = (mode == PROG)? pop(): popf();
    (mode == PROG)? push(reg[i]): pushf(regf[i]);
    return 0;
}

int
exec_recall() {
    int i;

    isInverted();
    last_was_fin_key = 0;

    i = recall("Recall from register" DIALOG_NUMBERS);
	
    if (isdigit(i)) {
	i -= '0';
	if ((i >= 0) && (i < NUMREGS)) {
	    char buf[80];

#ifdef HAS_ALGEBRAIC_MODE
	    if (algebraic_mode) {
		sprintf(buf, "RCL(%d)", i);
		add_x(buf);
	    } else
#endif
	    {
		(mode == PROG)? push(reg[i]): pushf(regf[i]);
		dispnums();
	    }
	    sprintf(buf, "Recalled from R%d", i);
	    msg(buf);
	} else
	    msg("index out of bounds");
    }
    return 0;
}

int
exec_xny() {
    long tempx, tempy;
    double tempxf, tempyf;

    isInverted();
    stop_entering();
    last_was_fin_key = 0;
    if (mode == PROG) {
	tempx = pop(); 
	tempy = pop(); 
	push(tempx);   
	push(tempy);   
    } else {
	tempxf = popf();
	tempyf = popf();
	pushf(tempxf);  
	pushf(tempyf);  
    }
    dispnums();
    return 0;
}

int
exec_xnl() {
    long tempx;
    double tempxf;

    isInverted();
    stop_entering();
    last_was_fin_key = 0;
    if (mode == PROG) {
	tempx = xiReg;
	xiReg = liReg;
	liReg = tempx;
    } else {
	tempxf = xfReg;
	xfReg = lfReg;
	lfReg = tempxf;
    }
    dispnums();
    return 0;
}

int
exec_xnt() {
    long tempx;
    double tempxf;

    isInverted();
    stop_entering();
    last_was_fin_key = 0;
    if (mode == PROG) {
	tempx = xiReg;
	xiReg = tiReg;
	tiReg = tempx;
    } else {
	tempxf = xfReg;
	xfReg = tfReg;
	tfReg = tempxf;
    }
    dispnums();
    return 0;
}

int
exec_xnz() {
    long tempx;
    double tempxf;

    isInverted();
    stop_entering();
    last_was_fin_key = 0;
    if (mode == PROG) {
	tempx = xiReg;
	xiReg = ziReg;
	ziReg = tempx;
    } else {
	tempxf = xfReg;
	xfReg = zfReg;
	zfReg = tempxf;
    }
    dispnums();
    return 0;
}

int
exec_help() {
    isInverted();
    last_was_fin_key = 0;
    pop_up_help();
    return 0;
}

int
exec_reg() {
    isInverted();
    last_was_fin_key = 0;
    pop_up_reg();
    return 0;
}

int
exec_inv() {
    last_was_fin_key = 0;
    invert = !invert;
    print_inv();
    clear_msg();
    return 0;
}

int process(COMMAND c) {
    clear_msg();

#ifdef HAS_ALGEBRAIC_MODE
    if (algebraic_mode)
	if (c >= ' ' && c <= '~') {
	    last_was_fin_key = 0;
	    echo_char(c);
	    return 0;
	} /* else a command */
#endif

    /* ASCII MODE - almost any character can be echo'ed */
    if ((c < MIN_COMMAND) && (mode == PROG && intmode == ASCIIM)) {
	last_was_fin_key = 0;
	echo_char(c); /* An ordinary character was typed */
	return 0;
    }
    
    if (c == INV) {
	exec_inv();
	return 0;
    }    
    
    if (c == BACKSPACE && isInverted()) {
	exec_clx();
	return 0;
    }

    if (((c >= '0') && (c <= '9')) ||
	((c >= 'A') && (c <= 'F')) ||
	(c == BACKSPACE) ||
	(c == '.')) {
	process_digit(c);
	last_was_fin_key = 0;
	return 0;
    }
    
    switch (c) {
    case CHS:		exec_chs(); 		break;
    case PLUS:		exec_plus();		break;
    case MINUS:		exec_minus();		break;
    case DIVIDE:	exec_divide();		break;
    case MULT:		exec_mult();		break;
    case dAND:		exec_and();		break;
    case dOR:		exec_or();		break;
    case dXOR:		exec_xor();		break;
    case dNOT:		exec_not();		break;
    case MODULUS:	exec_modulus();		break;
    case PERCENT:	exec_percent();		break;
    case PERCENTCH:	exec_percentch();	break;
    case YTOX:		exec_ytox();		break;
    case SHIFTL:	exec_shiftl();		break;
    case SHIFTR:	exec_shiftr();		break;
    case SHIFTYL:	exec_shiftyl();		break;
    case SHIFTYR:	exec_shiftyr();		break;
    case PRIMEF:	exec_primef();		break;
    case RECI:		exec_reci();		break;
    case SQR:		exec_sqr();		break;
    case ROOT:		exec_root();		break;
    case CUBE:		exec_cube();		break;
    case CROOT:		exec_croot();		break;
    case PVAL:		exec_pval();		break;
    case FVAL:		exec_fval();		break;
    case PMT:		exec_pmt();		break;
    case INTST:		exec_intst();		break;
    case FCLR:		exec_fclr();		break;
    case NFIN:		exec_nfin();		break;
    case TIMES12:	exec_times12();		break;
    case DIVIDE12:	exec_divide12();	break;
    case BEGIN:		exec_begin();		break;
    case DYS:		exec_dys();		break;
    case TDY:		exec_tdy();		break;	
    case ASCIIM:	exec_asciim();		break;	
    case BIN:		exec_bin();		break;
    case IP:		exec_ip();		break;
    case OCT:		exec_oct();		break;
    case DEC:		exec_dec();		break;
    case HEX:		exec_hex();		break;
    case FIX:		exec_fix();		break;
    case ENG:		exec_eng();		break;
    case SCIFORMAT:	exec_sciformat();	break;
    case PLACES:	exec_places();		break;	
    case FIN:		exec_fin();		break;
    case STAT:		exec_stat();		break;
    case SCI:		exec_sci();		break;
    case PROG:		exec_prog();		break;
    case ROLLDOWN:	exec_rolldown();	break;
    case CLX:		exec_clx();		break;
    case CLR:		exec_clr();		break;
    case DEGREE:	exec_degree();		break;
    case RADIAN:	exec_radian();		break;
    case E:		exec_e();		break;
    case PI:		exec_pi();		break;
    case SIN:		exec_sin();		break;	
    case COS:		exec_cos();		break;
    case TAN:		exec_tan();		break;
    case SINH:		exec_sinh();		break;
    case COSH:		exec_cosh();		break;
    case TANH:		exec_tanh();		break;
    case LOGE:		exec_loge();		break;
    case LOG10:		exec_log10();		break;
    case FRC:		exec_frc();		break;
    case INT:		exec_int();		break;
    case ETOX:		exec_etox();		break;
    case HMS:		exec_hms();		break;
    case RTOP:		exec_rtop();		break;
    case DTOR:		exec_dtor();		break;
    case SUMR:		exec_sumr();		break;	
    case SUM0:		exec_sum0();		break;
    case NSTAT:		exec_nstat();		break;
    case MEAN:		exec_mean();		break;
    case MEANS:		exec_means();		break;
    case MEANY:		exec_meany();		break;
    case S_DEV:		exec_s_dev();		break;
    case SUM:		exec_sum();		break;
    case FACT:		exec_fact();		break;
    case COMB:		exec_comb();		break;
    case PERM:		exec_perm();		break;
    case LR:		exec_lr();		break;
    case CALCY:		exec_calcy();		break;
    case ENTER:		exec_enter();		break;
    case LASTX:		exec_lastx();		break;
    case QUIT:		c = exec_quit();	break;
    case RECALL:	exec_recall();		break;
    case STORE:		exec_store();		break;
    case STOPLUS:	exec_stoplus();		break;
    case STOMINUS:	exec_stominus();	break;
    case STOMULTIPLY:	exec_stomultiply();	break;
    case STODIVIDE:	exec_stodivide();	break;
    case XNY:		exec_xny();		break;
    case XNL:		exec_xnl();		break;
    case XNT:		exec_xnt();		break;	
    case XNZ:		exec_xnz();		break;
    case HELP:		exec_help();		break;
    case REGISTER:	exec_reg();		break;
#ifdef HAS_ALGEBRAIC_MODE
    case RPN:		exec_rpn();		break;
    case ALGEBRAIC:	exec_algebraic();	break;
    case EVAL:		exec_eval();		break;
    case EQUALS:	exec_equals();		break;
#endif
    case NOP:
	break;

    default:
	last_was_fin_key = 0;
	put_a_char(BELL);
	break;
    }
    return c;
}

#ifndef EBOOKMAN
static void getReg(char *b) {
    long l;
    int i;

    if (sscanf(b, "%d %ld", &i, &l))
	if (i >= 0 && i < NUMREGS)
	    reg[i] = l;
}

static void getRegf(char *b) {
    double l;
    int i;

    if (sscanf(b, "%d %lg", &i, &l))
	if (i >= 0 && i < NUMREGS)
	    regf[i] = l;
}

static void getMode(char *b) {
    mode = 
	(strcmp(b, "sci")==0)  ? SCI:
	(strcmp(b, "fin")==0)  ? FIN:
	(strcmp(b, "stat")==0) ? STAT:
	(strcmp(b, "prog")==0) ? PROG: SCI;
}

static void getIntmode(char *b) {
    intmode = 
	strcmp(b, "asci")==0? ASCIIM:
	strcmp(b, "ip")==0? IP:
	strcmp(b, "bin")==0? BIN:
	strcmp(b, "oct")==0? OCT:
	strcmp(b, "dec")==0? DEC:
	strcmp(b, "hex")==0? HEX: DEC;
}

static void getFloatmode(char *b) {
    floatmode = 
	strcmp(b, "FIX")==0? FIX:
	strcmp(b, "ENG")==0? ENG: 
	strcmp(b, "SCI")==0? SCI: FIX;
}
#endif	

static void readSettings() {
#ifdef EBOOKMAN
    readGuiSettings(NULL);
    return;
#else
    FILE *f = NULL;
    char *homeDir = NULL;
    char fileName[200], buf[80];

    homeDir = getenv("HOME");
    if (homeDir) {
	strcpy(fileName, homeDir);
	if (fileName[strlen(fileName) - 1] != '/')
	    strcat(fileName, "/");
	strcat(fileName, ".dcalcrc");
	if ((f = fopen(fileName, "r")) != NULL) {
	    while (fgets(buf, 80, f) != NULL) {
		char *b = buf, *e;

		if (*b && buf[strlen(buf) - 1] == '\n')
		    buf[strlen(buf) - 1] = 0;

		if (*b == '#')
		    continue;

		while (!isspace(*b))
		    b++;
		*b++ = 0;
		if (strcmp(buf, "x") == 0) xiReg = atol(b);
		if (strcmp(buf, "y") == 0) yiReg = atol(b);
		if (strcmp(buf, "z") == 0) ziReg = atol(b);
		if (strcmp(buf, "t") == 0) tiReg = atol(b);
		if (strcmp(buf, "l") == 0) liReg = atol(b);
		if (strcmp(buf, "xf") == 0) xfReg = atof(b);
		if (strcmp(buf, "yf") == 0) yfReg = atof(b);
		if (strcmp(buf, "zf") == 0) zfReg = atof(b);
		if (strcmp(buf, "tf") == 0) tfReg = atof(b);
		if (strcmp(buf, "lf") == 0) lfReg = atof(b);
		if (strcmp(buf, "reg") == 0) getReg(b);
		if (strcmp(buf, "regf") == 0) getRegf(b);
		if (strcmp(buf, "angularMode") == 0) degree = atoi(b);
		if (strcmp(buf, "places") == 0) decplaces = atoi(b);
		if (strcmp(buf, "mode") == 0) getMode(b);
		if (strcmp(buf, "intmode") == 0) getIntmode(b);
		if (strcmp(buf, "floatmode") == 0) getFloatmode(b);
		if (strcmp(buf, "gui") == 0) readGuiSettings(b);
		if (strcmp(buf, "annuitymode") == 0) finPayAt0 = atol(b);
#ifdef HAS_ALGEBRAIC_MODE
		if (strcmp(buf, "algebraicmode") == 0) algebraic_mode = atol(b);
		if (strcmp(buf, "lasteval") == 0) {
		    e = ++b;
		    while (*e && *e != '"')
			e++;
		    *e = 0;
		    strcpy(last_eval, b);
		}
#endif
	    }
	    fclose(f);
	}
    }
#endif
}

void saveSettings(void) {
#ifdef EBOOKMAN
    stop_entering();
    saveGuiSettings(NULL);
    return;
#else
    FILE *f = NULL;
    char *homeDir = NULL;
    char fileName[200], buf[80];
    char lFormat[] = "%s %.25lg\n";
    char dFormat[] = "%s %ld\n";
    char iFormat[] = "%s %d\n";
    int i;

    stop_entering();
    homeDir = getenv("HOME");
    if (homeDir) {
	strcpy(fileName, homeDir);
	if (fileName[strlen(fileName) - 1] != '/')
	    strcat(fileName, "/");
	strcat(fileName, ".dcalcrc");
	if ((f = fopen(fileName, "w")) != NULL) {
	    fprintf(f, "# %s\n", "DCALC Version " VERSION " - by Bob Hepple");
	    fprintf(f, "# don't edit this file - dcalc will overwrite it\n");
	    fprintf(f, dFormat, "x", xiReg);
	    fprintf(f, dFormat, "y", yiReg);
	    fprintf(f, dFormat, "z", ziReg);
	    fprintf(f, dFormat, "t", tiReg);
	    fprintf(f, dFormat, "l", liReg);
	    fprintf(f, lFormat, "xf", xfReg);
	    fprintf(f, lFormat, "yf", yfReg);
	    fprintf(f, lFormat, "zf", zfReg);
	    fprintf(f, lFormat, "tf", tfReg);
	    fprintf(f, lFormat, "lf", lfReg);
	    for (i = 0; i < NUMREGS; i++) {
		sprintf(buf, "reg %d", i);
		fprintf(f, dFormat, buf, reg[i]);
	    }
	    for (i = 0; i < NUMREGS; i++) {
		sprintf(buf, "regf %d", i);
		fprintf(f, lFormat, buf, regf[i]);
	    }
	    fprintf(f, iFormat, "angularMode", degree);
	    fprintf(f, iFormat, "places", decplaces);
	    fprintf(f, "mode %s\n", 
		    mode==SCI? "sci":
		    mode==FIN? "fin":
		    mode==STAT? "stat":
		    mode==PROG? "prog":"sci");
	    fprintf(f, "intmode %s\n", 
		    intmode==ASCIIM? "asci":
		    intmode==IP? "ip":
		    intmode==BIN? "bin":
		    intmode==OCT? "oct":
		    intmode==DEC? "dec":
		    intmode==HEX? "hex": "dec");
	    fprintf(f, "floatmode %s\n",
		    floatmode==FIX? "FIX": "ENG");
	    saveGuiSettings(f);
	    /* Rev 2.7 on: */
	    fprintf(f, "annuitymode %d\n", finPayAt0? 1: 0);
	    /* Rev 2.8 on: */
#ifdef HAS_ALGEBRAIC_MODE
	    fprintf(f, "algebraicmode %d\n", algebraic_mode? 1: 0);
	    fprintf(f, "lasteval \"%s\"\n", last_eval);
#endif
	    fclose(f);
	}
    }
#endif
}

void initialise(void) {
    int	i;
    
    if (!was_initialised) {
	was_initialised = 1;
	os_init();
	
	xiReg = yiReg = ziReg = tiReg = liReg = 0;
	xfReg = yfReg = zfReg = tfReg = lfReg = 0.0;
	decplaces = 2;
#if EBOOKMAN
	mode = FIN;
#else
	mode = SCI;
#endif
	intmode = DEC;
	floatmode = FIX;
	entering = FALSE;
	lift_needed = TRUE;
	invert = FALSE;
	degree = TRUE;
	last_was_fin_key = FALSE;
	inptr = inbuf;
	*inptr = ASCIIZ;
	max_digits = 20;
	pi = 4.0 * atan(1.0);
	d_to_r = pi / 180.0;
	r_to_d = 180.0 / pi;
	finPayAt0 = 0;
#ifdef HAS_ALGEBRAIC_MODE
	last_eval[0] = 0;
#endif

	for (i = 0; i < NUMREGS; i++) {
	    reg[i] = 0;
	    regf[i] = 0.0;
	}

	readSettings();
	if ((mode == PROG) && (intmode == ASCIIM))
	    os_raw_mode(1);
    }
}

extern void terminate_dcalc() {
    saveSettings();
    os_term();
}

/* For emacs: */

/* Local Variables: */
/* eval:(setq tab-width 8) */
/* End: */
