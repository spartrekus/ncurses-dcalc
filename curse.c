/*
 * DCALC: CURSES specific parts of DCALC. 

    Contact info:
    bhepple@freeshell.org
    http://sedumi.freeshell.org/unix

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

#if NCURSES
#include <ncurses/curses.h>
#else
#include <curses.h>
#endif
#include <ctype.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>

#include "dcalc.h"

#define CTRL(x) ('x'&037)

WINDOW	*xwin, *regwin, *helpwin, *fkwin, *msgwin, *curwin;

#define XWIN_LINES	4
#define XWIN_COLS	WIDTH+3
#define MSGWIN_LINES	3
#define MSGWIN_COLS	MSG_SIZE+2
#define FKWIN_LINES	2
#define FKWIN_COLS	80
#define REGWIN_LINES	16
#define REGWIN_COLS	40
#define HELPWIN_LINES	16
#define HELPWIN_COLS	36
#define NUMFK 5

#if PCSPECIFIC
#define F1KEY	073
#define F2KEY	074
#define F3KEY	075
#define F4KEY	076
#define F5KEY	077
#define F6KEY	010
#define F7KEY	0101
#define F8KEY	0102
#define F9KEY	0103
#define F10KEY	0104
#define HOMEKEY	0107
#define ENDKEY  0117
#define UPKEY	0110
#define LEFTKEY	0113
#define RIGHTKEY 0115
#define DOWNKEY	0120
#define ROLLKEY	0121
#define DELKEY	0123
#define NEXTKEY	0121
#define PREVKEY	0111
#endif /* PCSPECIFIC */

/* screen addresses ... */
#define LEFTSIDE	0
#define START_X		2
#define STARTL_Y	10
#define STARTT_Y	11
#define STARTZ_Y	12
#define STARTY_Y	13
#define STARTX_Y	1
#define BASE_X		0
#define BASE_Y		0
#define FK_X		0
#define FK_Y		0
#define INV_X		9
#define INV_Y		0
#define TMODE_X		19
#define TMODE_Y		0
#define INTLINEX	2
#define INTLINEY	11
#define ERRORX		0
#define ERRORY		0
#define WIDTH		36

#define F1	0x340
#define F2	0x341
#define F3	0x342
#define F4	0x343
#define F5	0x344
#define F6	0x345
#define F7	0x346
#define F8	0x347
#define F9	0x350
#define F10	0x351

int	help_on = 0,
	reg_on  = 0,
	row, col,
	is_resident = 0, /* Never a TSR! */
	fk_set;         /* The current set of function key labels    */

#ifdef PC
chtype	boxcharh	= '\315',
	boxcharv	= '\272',
	boxcharul	= '\218',
	boxcharur	= '\191',
	boxcharll	= '\192',
	boxcharlr	= '\217';
#else /* UNIX */
#if 0 /* I can't get this to work for Sun - the terminfo entry is inadequate */
chtype	boxcharh	= ACS_HLINE,
	boxcharv	= ACS_VLINE,
	boxcharul	= ACS_ULCORNER,
	boxcharur	= ACS_URCORNER,
	boxcharll	= ACS_LLCORNER,
	boxcharlr	= ACS_LRCORNER;
#else
chtype	boxcharh	= '-',
	boxcharv	= '|',
	boxcharul	= '+',
	boxcharur	= '+',
	boxcharll	= '+',
	boxcharlr	= '+';
#endif
#endif

static jmp_buf 		save;

extern int entering, fk_set;

/* FUNCTION KEY DEFINITIONS */

struct funckey {
	char label[8];
	COMMAND function;
};

struct funckey func[NUMFK][8] = {
    "Float  ", FIX,
    "Engin. ", ENG,
    "Binary ", BIN,
    "Octal  ", OCT,
    "Dec    ", DEC,
    "Hex    ", HEX,
    "Ascii  ", ASCIIM,
    "HELP   ", HELP,
    
    "Sin    ", SIN,
    "Cos    ", COS,
    "Tan    ", TAN,
    "Log E  ", LOGE,
    "Log 10 ", LOG10,
    "       ", ASCIIZ,
    "Inverse", INV,
    "HELP   ", HELP,
    
    "Square ", SQR,
    "S Root ", ROOT,
    "Cube   ", CUBE,
    "C root ", CROOT,
    "FRC    ", FRC,
    "INT    ", INT,
    "       ", ASCIIZ,
    "HELP   ", HELP,
    
    "Sum    ", SUM,
    "Mean   ", MEAN,
    "S Dev  ", S_DEV,
    "N      ", NSTAT,
    "Clear  ", SUM0,
    "Sum-   ", SUMR,
    "       ", ASCIIZ,
    "HELP   ", HELP,
    
    "N      ", NFIN,
    "I      ", INTST,
    "P Val  ", PVAL,
    "PMT    ", PMT,
    "F Val  ", FVAL,
    "       ", ASCIIZ,
    "Inverse", INV,
    "HELP   ", HELP,
};

void saveGuiSettings(FILE *f) {}
void readGuiSettings(char *b) {}

int wnselect(x)
    WINDOW	*x;
{
    curwin = x;
    return(1);
}

gotoxy(x, y)
    int	x, y;
{
    row = y;
    col = x;
    wmove(curwin, y + 1, x + 1);
}

int usage()
{
}

void print_string(char *s) {
    unsigned char d;
    
    while (d = *s++) {
		if (d > 126) {
			wattron(curwin, A_STANDOUT);
			d -= 127;
			if (d < 32)
				d = '?';
			waddch(curwin, d);
			wattroff(curwin, A_STANDOUT);
		} else {
			if (d < 32)
				d = '?';
			waddch(curwin, d);
		}
    }
}

void mybox(win, maxx, maxy)
    WINDOW 	*win;
    int		maxx, maxy;
{
#ifdef PCSPECIFIC
    box(win, boxcharv, boxcharh);
    mvwaddch(win, 0, 	  		0, 		   	boxcharul);
    mvwaddch(win, 0, 	  		maxx,		 	boxcharur);
    mvwaddch(win, maxy,	 		0, 		   	boxcharll);
    mvwaddch(win, maxy, 		maxx, 			boxcharlr);
#else
    box(win, 0, 0);
#endif
}

void msg(s)
    char	*s;
{
    char 	outbuf[45];
    
    wnselect(msgwin);
    mybox(msgwin, MSGWIN_COLS - 1, MSGWIN_LINES - 1);
    gotoxy(ERRORX, ERRORY);
    strcpy(outbuf, s);
    while (strlen(outbuf) < MSG_SIZE - 1)
		strcat(outbuf, " ");
    outbuf[MSG_SIZE - 1] = '\0';
    print_string(outbuf);
    dispnums();
}

void
clear_msg() {
    werase(msgwin);
}

void put_a_char(COMMAND c) {
    /* Only used to write to the X reg or ring the bell */
    if (c == BELL)
		beep();
    else {
		wnselect(xwin);
		waddch(curwin, c);
    }
}

COMMAND get_a_char(d)
    COMMAND	*d;
{
    /* get a character with no echo, but with wait.
     */
    COMMAND c;
    int	meta_flag = 0;
    
    *d = 0;
    refresh();
    wrefresh(regwin);
    wrefresh(fkwin);
    wrefresh(msgwin);
    wrefresh(helpwin);
    wrefresh(xwin);
    c = 0;
    do {
		c = getch(); /* The _only_ place input is received */
		if (c == '\033') { /* Escape */
			c = 0;
			meta_flag = 1;
		}
    } while (c == 0);
    
    if (meta_flag) {
		if ((c >= '0') && (c <= '9'))
			c = KEY_F0 + c - '0';
		else
			c |= 0x80;
		meta_flag = 0;
    }
    
    if (!(c & 0xFF80) && isascii(c) && (mode != PROG || intmode != ASCIIM))
		c = toupper(c);
    return((COMMAND)c);
}

static int get_int(char *s) {
    int	retval, d = 0;
    
    msg(s);
    retval = get_a_char(&d);
    clear_msg();
    return(retval);
}

int places(char *buf) {
	int retval;

	retval = get_int(buf);
	if (isdigit(retval))
		return retval - '0';
	return -1;
}	

int recall(char *buf) {
	int retval;

	retval = get_int(buf);
	switch (retval) {
	case '+':
	case '-':
	case '/':
	case '*':
		return retval;
	}
	if (isdigit(retval))
		return retval;
	return -1;
}

int store(char *buf) {
	int retval;

	retval = get_int(buf);
	switch (retval) {
	case '+':
	case '-':
	case '/':
	case '*':
		return retval;
	}
	if (isdigit(retval))
		return retval;
	return -1;
}

void print_fk(i)
    int	i;
{
    int 	j;
    
    wnselect(fkwin);
    for (j = 0; j < 8; j++) {
		gotoxy(FK_X + j * 10, FK_Y);
		wprintw(curwin, "%d:", j + 1);
		wattrset(curwin, A_REVERSE);
		wprintw(curwin, "%s", func[i][j].label);
		wattrset(curwin, 0);
		waddch(curwin, ' ');
    }
}

/* d is always 0 for curses */
COMMAND map_key_to_func(c, d)
    COMMAND 	c, d;
{
    if (mode == PROG && intmode == BIN)
		if ((c&0xff80) == 0) /* Not a function key */
			return(c);
    
    if ((mode = PROG && intmode == HEX) && 
		(((c >= 'a') && (c <= 'f')) || 
		 ((c >= 'A') && (c <= 'F'))))
		return(c);

    if ((c >= KEY_F(1)) && (c <= KEY_F(8)))
		return(func[fk_set][c - KEY_F(1)].function);
    else switch (c) {
    case '\027': /* ^W = f1 */
		return(func[fk_set][0].function);
    case '\005': /* ^E = f2 */
		return(func[fk_set][1].function);
    case '\022': /* ^R = f3 */
		return(func[fk_set][2].function);
    case '\024': /* ^T = f4 */
		return(func[fk_set][3].function);
    case '\031': /* ^Y = f5 */
		return(func[fk_set][4].function);
    case '\025': /* ^U = f6 */
		return(func[fk_set][5].function);
    case '\011': /* ^I = f7 */
		return(func[fk_set][6].function);
    case '\017': /* ^O = f8 */
		return(func[fk_set][7].function);
	
    case 'D':
    case 'd':
    case KEY_DOWN:
		return(ROLLDOWN);
	
    case KEY_F(10):
    case KEY_NPAGE:
    case '\016': /* ^N */
		return(NEXTKEYS);
	
    case KEY_F(9):
    case KEY_PPAGE:
    case '\020': /* ^P */
		return(PREVKEYS);
	
    case KEY_DL:
    case KEY_DC:
    case KEY_EOL:
    case KEY_EOS:
    case KEY_CLEAR:
    case 'C':
    case 'c':
		return(CLX);
	
    case '\004':	/* ^D */     
    case 'Q':
    case 'q':   
    case ESCAPE:
		return(QUIT);
	
    case KEY_UP:
    case '\n':
    case '\r':
		return(ENTER);
	
    case '\014': /* ^L */
		clearok(curscr, TRUE); /* refresh display */
		return(NOP);

    case KEY_BACKSPACE:
		return(BACKSPACE);

    case ',':
		return(CHS);

    case '~':
		return(dNOT);

    case '+':
		return PLUS;

    case '-':
		return MINUS;

    case '*':
		return MULT;

    case '/':
		return DIVIDE;

    case '%':
		return PERCENT;

    case '^':
		return dXOR;

    case '|':
		return dOR;

    case '&':
		return dAND;

    case '<':
		return SHIFTL;

    case '>':
		return SHIFTR;

    case '#':
		return MODULUS;

	case '$':
		return PLACES;

    case 'L':
		return LASTX;

	case 'M':
		if (degree)
			return RADIAN;
		else
			return DEGREE;

    case 'P':
		return PI;

	case 'R':
		return RECALL;

	case 'S':
		return STORE;

	case 'T':
		return XNT;

    case 'Y':
		return YTOX;

	case 'X':
		return XNY;

	case 'Z':
		return XNZ;

	case '@':
		return REGISTER;

	case 'H':
		return HELP;

    default:
		return(c);
    }
}

void print_attr(s)
    char	*s;
{
    int	standing_out = 0;
    
    while (*s) {
		if (*s == '\2') {
			if (!standing_out) {
				wattron(curwin, A_STANDOUT);
				standing_out = 1;
			}
			waddch(curwin, *++s);
		} else {
			if (standing_out) {
				wattroff(curwin, A_STANDOUT);
				standing_out = 0;
			}
			waddch(curwin, *s);
		}
		s++;
    }
    wattroff(curwin, A_STANDOUT);
}

/* Pop up the three essential windows */
void pop_up_windows()
{
    xwin    = newwin(XWIN_LINES, XWIN_COLS, REGWIN_LINES + MSGWIN_LINES , 2);
    msgwin  = newwin(MSGWIN_LINES, MSGWIN_COLS, REGWIN_LINES, 1);
    fkwin   = newwin(FKWIN_LINES, FKWIN_COLS, LINES - 2, 0);
    regwin  = newwin(REGWIN_LINES, REGWIN_COLS, 0, 1);
    helpwin = newwin(HELPWIN_LINES, HELPWIN_COLS, 0, 42);
    mybox(xwin, XWIN_COLS - 1, XWIN_LINES - 1);
    wmove(xwin, 0, 2);
    wattron(xwin, A_REVERSE);
    wprintw(xwin, "DCALC - Use \'H\' for Help");
    wattroff(xwin, A_REVERSE);
    mybox(msgwin, MSGWIN_COLS - 1, MSGWIN_LINES - 1);
}

void
pop_up_help() {
    if (help_on) {
		werase(helpwin);
		help_on = 0;
    } else {
		help_on = 1;
		wnselect(helpwin);
		gotoxy(0, 0);
		print_attr("\2^\2D Exit DCALC      \2X x<>y");
		gotoxy(0, row + 1);
		print_attr(" \2$ Places          \2Y y^x");
		gotoxy(0, row + 1);
		print_attr(" \2M Deg/Rad   \2P Pi  \2% x % of y");
		gotoxy(0, row + 1);
		print_attr(" \2C CLX             \2# y MODULO x");
		gotoxy(0, row + 1);
		print_attr(" \2D ROLL DOWN       \2& x AND y");
		gotoxy(0, row + 1);
		print_attr(" \2, CHS             \2| x OR y ");
		gotoxy(0, row + 1);
		print_attr(" \2L Recall Last x   \2^ x XOR y");
		gotoxy(0, row + 1);
		print_attr(" \2S Store           \2~ complement x");
		gotoxy(0, row + 1);
		print_attr(" \2R Recall    \2>,\2< y shifted x bits");
		gotoxy(0, row + 1);
		print_attr(" \2T x<>t            \2Z x<>z");
		gotoxy(0, row + 1);
		print_attr("\2^\2P Previous keys   \2^\2N Next keys");
		gotoxy(0, row + 1);
		print_attr("\2^\2W  \2^\2E  \2^\2R  \2^\2T  \2^\2Y  \2^\2U  \2^\2I  \2^\2O ");
		gotoxy(0, row + 1);
		print_attr("f1  f2  f3  f4  f5  f6  f7  f8 ");
		gotoxy(0, row + 1);
		print_attr("'Esc 1' = f1, etc.");
		mybox(helpwin, HELPWIN_COLS - 1, HELPWIN_LINES - 1);
		wmove(helpwin, 0, 2);
		wattron(helpwin, A_REVERSE);
		wprintw(helpwin, "DCALC: \'H\' turns HELP window off");
		wattroff(helpwin, A_REVERSE);
    }
}

void
pop_up_reg() {
    if (reg_on) {
		wattrset(regwin, 0);
		werase(regwin);
		reg_on = 0;
    } else {
		reg_on = 1;
		mybox(regwin, REGWIN_COLS - 1, REGWIN_LINES - 1);
		wmove(regwin, 0, 2);
		wattron(regwin, A_REVERSE);
		wprintw(regwin,	"DCALC: \'@\' turns registers off");
		wattroff(regwin, A_REVERSE);
		wattron(regwin, A_UNDERLINE);
		dispnums();
		dispregs();
    }
}

xwin_select()
{
    return(wnselect(xwin));
}

reg_select()
{
    if (reg_on) {
		wnselect(regwin);
		return(1);
    } else
		return(0);
}

void
os_init() {
    initscr();
    cbreak();
    nonl();
    noecho();
    keypad(stdscr, TRUE);
    pop_up_windows();
}

void os_raw_mode(int i) {}

void
os_term() {
    endwin();
}

void
matherr(int x) {
    int	set_signals(),
		result;
    
    msg("Math error!");
    set_signals();
    longjmp(save, 1);
    return; /* Never gets here - but, just in case! */
}

set_signals()
{
    signal(SIGFPE, matherr);
    signal(SIGINT, SIG_IGN);
}

dcalc() {
    COMMAND	c, d;
    
    fk_set = 0;
    initialise();
    display();
    pop_up_help();
    pop_up_reg();
    msg(DEF_SIG(VERSION));
    print_fk(fk_set);

    setjmp(save);
    set_signals();
    do {
		xwin_select();
		gotoxy(STARTX_Y, WIDTH);
		c = get_a_char(&d);
		clear_msg();
		c = map_key_to_func(c, d);
		switch (c) {
	
		case NEXTKEYS:
			fk_set++;
			if (fk_set == NUMFK)
				fk_set = 0;
			invert = FALSE;
			print_inv();
			print_fk(fk_set);
			break;
	
		case PREVKEYS:
			fk_set--;
			if (fk_set < 0)
				fk_set = NUMFK - 1;
			invert = FALSE;
			print_inv();
			print_fk(fk_set);
			break;

		default:
			c = process(c);
		}
		setjmp(save);
    } while (c != QUIT);
    
    terminate_dcalc();
}

void print_base(x, xf)
    long	x;
    double	xf;
{
    char 	outbuf[45];

    fmt_base(outbuf, x, xf);
    prep_for_output(outbuf);
	while (strlen(outbuf) < WIDTH)
		strcat(outbuf, " ");
    print_string(outbuf);
}
  
void dispreg(i)
    int	i;
{
    char 	outbuf[45];

    if (reg_select()) {
		gotoxy(LEFTSIDE, i);
		outbuf[0] = '0' + i;
		outbuf[1] = '\0';
		print_string(outbuf);
		gotoxy(START_X, i);
		print_base(reg[i], regf[i]);
    }
}

void dispregs()
{
    int i;
    
    for (i = 0; i < NUMREGS; i++)
		dispreg(i);
}

void prinbase()
{
    char 	outbuf[45];

    xwin_select();
    gotoxy(BASE_X, BASE_Y);
	if (mode == PROG)
		switch (intmode) {
		case ASCIIM:
			print_string("Ascii   ");
			break;
	
		case BIN:
			print_string("Binary  ");
			break;
	
		case OCT:
			print_string("Octal   ");
			break;
	
		case DEC:
			print_string("Decimal ");
			break;
	
		case HEX:
			print_string("Hex     ");
			break;
		} else {
			if (floatmode == ENG)
				sprintf(outbuf, "Engin. %d", decplaces);
			else
				sprintf(outbuf, "Float  %d", decplaces);
			print_string(outbuf);
		}
}

void print_inv()
{
    xwin_select();
    gotoxy(INV_X, INV_Y);
    if (invert)
		print_string("Inv   ");
    else
		print_string("      ");
}

void print_deg()
{
    xwin_select();
    gotoxy(TMODE_X, TMODE_Y);
    if (degree)
		print_string("Degrees");
    else
		print_string("Radians");
}

void print_x(char *buf) 
{
    gotoxy(LEFTSIDE, STARTX_Y);
	while (strlen(buf) < WIDTH)
		strcat(buf, " ");
    print_string(buf);
}

void dispnums() {
    char 	outbuf[45];

    xwin_select();
    gotoxy(LEFTSIDE, STARTX_Y);
    if (entering) {
		strcpy(outbuf, inbuf);
		prep_for_output(outbuf);
		while (strlen(outbuf) < WIDTH)
			strcat(outbuf, " ");
		print_string(outbuf);
    } else
		print_base(xiReg, xfReg);
    
    if (reg_select()) {
		gotoxy(LEFTSIDE, STARTT_Y);
		print_string("T");
		gotoxy(START_X, STARTT_Y);
		print_base(tiReg, tfReg);
	
		gotoxy(LEFTSIDE, STARTZ_Y);
		print_string("Z");
		gotoxy(START_X, STARTZ_Y);
		print_base(ziReg, zfReg);
	
		gotoxy(LEFTSIDE, STARTY_Y);
		print_string("Y");
		gotoxy(START_X, STARTY_Y);
		print_base(yiReg, yfReg);
	
		gotoxy(LEFTSIDE, STARTL_Y);
		print_string("L");
		gotoxy(START_X, STARTL_Y);
		print_base(liReg, lfReg);
    }
}

int dialog(char *buf) {
	COMMAND c;

	msg(buf);
	return(get_a_char(&c));
}

int
main(int argc, char *argv[]) {
    dcalc();
}
