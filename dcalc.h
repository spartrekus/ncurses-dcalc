/* $Id: dcalc.h,v 1.9 2002/05/21 07:27:16 bhepple Exp $ */

/* These flags control compilation. Define either PC, OPENVIEW or UNIX.
 * For PCs define either CURSES or PCSPECIFIC.
 * For the PC, the best combination is PC and PCSPECIFIC although PC
 * and CURSES allows the curses version to be tested.
 *
 * For non-OPENVIEW UNIX, you must have CURSES set.
 * 
 * 1999.2.3 - the PC version needs 'Turbo C Tools version 5.00' by Blaise
 * Computing, which is unlikely to still be available. IT also needs the
 * PC-dependent source 'ibm.c'. This was last compiled by me in 1994 (???)
 * with Borland's Turbo-C 1.5. As this is just too hard and unlikely to
 * be of further interest, I will no longer support the DOS version.
 * If this is a problem, please contact me:

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

#ifdef PC
#undef   CURSES
#undef EBOOKMAN
#undef UNIX
#define  PCSPECIFIC      1
/* Set this if you want a TSR function with mouse capture */
#undef	 TSR
#endif

#ifdef EBOOKMAN
#undef UNIX
#undef CURSES
#undef PC
#define HAVE_CONFIG_H 1
#endif

#ifndef HAVE_CONFIG_H
#  define VERSION "2.12"
#endif

/* If you set PCSPECIFIC then you need to compile and link dcalc.c with
 * ibm.c as well as the special version of ivctrl.obj
 * (munged to support the 101-key keyboard) as well as Blaise's tct_t1s.lib.
 * Compile everything with TurboC 1.5 in the small model.
 *
 * For CURSES, you need to compile and link curse.c and dcalc.c with the curses
 * library.
 *
 * For OPENVIEW, you need to compile and link openview.c, ov_ui.c, ov_stubs.c
 * and dcalc.c with the appropriate libraries.
 */

/* End of flags. The previous flags are all you should have to touch. */
/**********************************************************************/

#ifdef UNIX
#define CURSES		1
#undef PC
#undef EBOOKMAN
#undef  PCSPECIFIC
#undef  OPENVIEW
#endif

#ifdef __TURBOC__
#define REVERSE_BYTES	1
#else
#define REVERSE_BYTES	0
#endif

#undef REVERSE_BYTES
#define REVERSE_BYTES	1

#define ASCIIZ '\0'
#define SPECIAL ASCIIZ

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

/* Keys we are interested in ... */
#define BELL	7
#define SPACE	' '
#define COMMA   ','
#define BACKSPACE 8
#define ESCAPE	27

#define COMMAND int

/* COMMANDS (in addition to 'normal' keys ... */
#define MIN_COMMAND 0x200
#define ASCIIM		0x200
#define	DEGREE		0x201
#define	RADIAN		0x202
#define MODE		0x203
#define	PROG		0x204
#define	SCI		0x205
#define	FIN		0x206
#define	STAT		0x207
#define	PLACES		0x208
#define FIX		0x209
#define ENG  		0x20a	
#define SCIFORMAT	0x20b
#define BIN  		0x20c	
#define OCT  		0x20d
#define DEC  		0x210
#define HEX  		0x211
#define IP  		0x212
#define INV  		0x213   
#define SIN  		0x214
#define COS  		0x215
#define TAN  		0x216
#define SINH 		0x217
#define COSH 		0x218
#define TANH 		0x219
#define LOGE 		0x220
#define LOG10		0x221
#define SQR  		0x222
#define ROOT 		0x223
#define CUBE 		0x224
#define CROOT		0x225
#define FRC  		0x226
#define INT  		0x227
#define ETOX		0x228
#define RECI		0x229
#define PI		0x22a
#define E		0x22b
#define HMS		0x22c
#define RTOP		0x230
#define DTOR		0x231
#define SUM  		0x232
#define SUM0 		0x233
#define SUMR 		0x234
#define MEAN 		0x235
#define MEANY		0x236
#define MEANS		0x237
#define S_DEV		0x238
#define NSTAT		0x239
#define FACT		0x23a
#define PERM		0x23b
#define COMB		0x240
#define LR		0x241
#define CALCY		0x242
#define NFIN 		0x243
#define INTST		0x244
#define PMT  		0x245
#define PVAL 		0x246
#define FVAL 		0x247
#define FCLR		0x248
#define BEGIN		0x249
#define DYS		0x24a
#define TDY		0x250	
#define TIMES12		0x251
#define DIVIDE12	0x252
#define	LENGTH		0x253   
#define	AREA		0x254
#define	VOLUME		0x255
#define	MASS		0x256
#define	SPEED		0x257
#define	FUEL		0x260
#define	PRES		0x261
#define	TEMP		0x262
#define dAND		0x263
#define dOR		0x264
#define dNOT		0x265
#define dXOR		0x266
#define SHIFTYL 	0x267
#define SHIFTYR 	0x268
#define SHIFTL		0x280
#define SHIFTR		0x281
#define PRIMEF		0x282
#define PLUS		0x283
#define MINUS		0x284
#define MULT		0x285
#define DIVIDE		0x286
#define PERCENT		0x287	
#define PERCENTCH 	0x288
#define MODULUS 	0x290
#define	CHS		0x291
#define YTOX		0x292
#define ROLLDOWN    	0x293
#define CLX         	0x294
#define QUIT	    	0x295
#define NEXTKEYS    	0x296
#define PREVKEYS    	0x297
#define ENTER	    	0x298
#define	XNL	    	0x299
#define	XNT	    	0x29a
#define	XNZ	    	0x29b
#define	XNY	    	0x29c
#define	LASTX	    	0x29d
#define STORE	    	0x29e
#define STOPLUS	    	0x29f
#define STOMINUS    	0x2a0
#define STOMULTIPLY 	0x2a1
#define STODIVIDE   	0x2a2
#define RECALL	    	0x2b0 
#define COPY	    	0x2b1 
#define	PASTE	    	0x2b2
#define CLU	    	0x2b3
#define CLR	    	0x2b4
#define HELP	 	0x2b5  
#define REGISTER 	0x2b6
#define EXPLAIN  	0x2b7
#define CVT		0x2b8
#define RPN		0x2ba
#define ALGEBRAIC	0x2bb
#define POPUP_REG	0x2bc
#define EVAL		0x2bd
#define EQUALS		0x2be
#define LEFTBRACE	0x2bf
#define RIGHTBRACE	0x2c0
#define NOP	 	0x2d0 /* must be the last in the list */
/* ... END OF COMMANDS */

#define BOOLEAN int

#define NUMREGS 10
#define STAT_R2		1
#define STAT_Y_INT	2
#define STAT_SLOPE	3
#define STAT_NUM_REG	4
#define STAT_SUMX_REG	5
#define STAT_SUMY_REG	6
#define STAT_SUMX2_REG	7
#define STAT_SUMY2_REG	8
#define STAT_SUMXY_REG	9
#define FIN_NUM_REG	5
#define FIN_INT_REG	6
#define FIN_PVAL_REG	7
#define FIN_PMT_REG	8
#define FIN_FVAL_REG	9
#define MSG_SIZE 	42
#define KMaxPrimes 	9

#define DEF_SIG(x) "DCALC Version " x " - by Bob Hepple"

/* Function prototypes: */

/* imported by dcalc - supplied by curses, hpcalc, ibm etc */
#ifdef __cplusplus
extern "C" {
#endif
    extern void print_string(char *buf);
    extern void prinbase(void);
    extern void print_inv(void);
    extern void print_deg(void);
    extern void print_x(char *buf);
#if HAS_ALGEBRAIC_MODE
    extern void add_x(char *buf);
#endif
    extern void dispnums(void);
    extern void dispreg(int);
    extern void dispregs(void);
    extern void put_a_char(int i);
    extern int get_a_char(int *);
    extern void msg(char *buf);
    extern void clear_msg(void);
    extern void pop_up_reg(void);
    extern void pop_up_help(void);
    extern void os_raw_mode(int);
    extern void os_init(void);
    extern void os_term(void);
    extern int dialog(char *buf);
    extern int places(char *buf);
    extern int eval(char *, char *);
    extern int store(char *buf);
    extern int recall(char *buf);
    extern void saveGuiSettings(FILE *f);
    extern void readGuiSettings(char *b);
#ifdef __cplusplus
}
#endif

/* exported by dcalc.c: */
#ifdef __cplusplus
extern "C" {
#endif
    extern void fmt_bin(char *str, long x);
    extern void fmt_base(char *s, long x, double xf);
    extern void prep_for_output(char *string);
    extern void parseBuffer(char *buf, long *longResult, double *doubleResult);
    extern void initialise(void);
    extern int process(COMMAND c);
    extern void terminate_dcalc(void);
    extern void display(void);
    extern int base, decplaces, entering, lift_needed, invert, degree, mode, intmode, floatmode, finPayAt0, algebraic_mode, last_was_fin_key;
    extern long xiReg, yiReg, ziReg, tiReg, liReg, reg[NUMREGS];
    extern double xfReg, yfReg, zfReg, tfReg, lfReg, regf[NUMREGS];
    extern char inbuf[45];
    extern void convertX(char *from, char *to);
    extern void trim(char *s);
    extern long asc2int(char *s);
    extern long bin2int(char *s);
    extern long ip2int(char *s);
    extern void push(long inx);
    extern void pushf(double inx);
    extern void saveSettings(void);
    extern void stop_entering(void);
    extern long pop(void);
    extern double popf(void);
    extern int isInverted(void);
    extern int liftStack(void);

#ifdef HAS_ALGEBRAIC_MODE
/* for the algebraic parser: */
    extern double  xfSave,yfSave,zfSave,tfSave,ufSave,lfSave;
    extern long xSave, ySave, zSave, tSave, uSave, lSave;
    extern char last_eval[200];
#endif

    extern int exec_chs(void); 		
    extern int exec_plus(void);		
    extern int exec_minus(void);		
    extern int exec_divide(void);		
    extern int exec_mult(void);		
    extern int exec_and(void);		
    extern int exec_or(void);		
    extern int exec_xor(void);		
    extern int exec_not(void);		
    extern int exec_modulus(void);		
    extern int exec_percent(void);		
    extern int exec_percentch(void);	
    extern int exec_ytox(void);		
    extern int exec_shiftl(void);		
    extern int exec_shiftr(void);		
    extern int exec_shiftyl(void);		
    extern int exec_shiftyr(void);		
    extern int exec_primef(void);		
    extern int exec_reci(void);		
    extern int exec_sqr(void);		
    extern int exec_root(void);		
    extern int exec_cube(void);		
    extern int exec_croot(void);		
    extern int exec_pval(void);		
    extern int exec_fval(void);		
    extern int exec_pmt(void);		
    extern int exec_intst(void);		
    extern int exec_fclr(void);		
    extern int exec_nfin(void);		
    extern int exec_times12(void);		
    extern int exec_divide12(void);	
    extern int exec_begin(void);		
    extern int exec_dys(void);		
    extern int exec_tdy(void);			
    extern int exec_asciim(void);			
    extern int exec_bin(void);		
    extern int exec_ip(void);		
    extern int exec_oct(void);		
    extern int exec_dec(void);		
    extern int exec_hex(void);		
    extern int exec_fix(void);		
    extern int exec_eng(void);		
    extern int exec_sciformat(void);	
    extern int exec_places(void);			
    extern int exec_fin(void);		
    extern int exec_stat(void);		
    extern int exec_sci(void);		
    extern int exec_prog(void);		
    extern int exec_rolldown(void);	
    extern int exec_clx(void);		
    extern int exec_clr(void);		
    extern int exec_degree(void);		
    extern int exec_radian(void);		
    extern int exec_e(void);		
    extern int exec_pi(void);		
    extern int exec_sin(void);			
    extern int exec_asin(void);			
    extern int exec_cos(void);		
    extern int exec_acos(void);		
    extern int exec_tan(void);		
    extern int exec_atan(void);		
    extern int exec_sinh(void);		
    extern int exec_asinh(void);		
    extern int exec_cosh(void);		
    extern int exec_acosh(void);		
    extern int exec_tanh(void);		
    extern int exec_atanh(void);		
    extern int exec_loge(void);		
    extern int exec_log10(void);		
    extern int exec_frc(void);		
    extern int exec_int(void);		
    extern int exec_etox(void);		
    extern int exec_10tox(void);		
    extern int exec_hms(void);		
    extern int exec_rtop(void);		
    extern int exec_ptor(void);		
    extern int exec_rtod(void);		
    extern int exec_dtor(void);		
    extern int exec_sumr(void);			
    extern int exec_sum0(void);		
    extern int exec_nstat(void);		
    extern int exec_mean(void);		
    extern int exec_means(void);		
    extern int exec_meany(void);		
    extern int exec_s_dev(void);		
    extern int exec_sum(void);		
    extern int exec_fact(void);		
    extern int exec_comb(void);		
    extern int exec_perm(void);		
    extern int exec_lr(void);		
    extern int exec_calcy(void);		
    extern int exec_enter(void);		
    extern int exec_lastx(void);		
    extern int exec_quit(void);	
    extern int exec_recall(void);		
    extern int exec_rcl(void);		
    extern int early_exec_recall(void);		
    extern int exec_store(void);		
    extern int exec_stoplus(void);		
    extern int exec_stominus(void);	
    extern int exec_stomultiply(void);	
    extern int exec_stodivide(void);	
    extern int exec_xny(void);		
    extern int exec_xnl(void);		
    extern int exec_xnt(void);			
    extern int exec_xnz(void);		
    extern int exec_help(void);		
    extern int exec_reg(void);		
    extern int exec_equals(void);		
    extern int exec_leftbrace(void);		
    extern int exec_rightbrace(void);		
    extern int exec_toggle_deg(void);		
    extern int exec_inv(void);		
#ifdef HAS_ALGEBRAIC_MODE
    extern int exec_rpn(void);
    extern int exec_algebraic(void);
    extern int exec_eval(void);
#endif

#ifdef __cplusplus
}
#endif
/* For emacs: */

/* Local Variables: */
/* eval:(setq tab-width 8) */
/* End: */
