/**********************************************************************/
/*   ____  ____                                                       */
/*  /   /\/   /                                                       */
/* /___/  \  /                                                        */
/* \   \   \/                                                         */
/*  \   \        Copyright (c) 2003-2013 Xilinx, Inc.                 */
/*  /   /        All Right Reserved.                                  */
/* /---/   /\                                                         */
/* \   \  /  \                                                        */
/*  \___\/\___\                                                       */
/**********************************************************************/


#include "iki.h"
#include <string.h>
#include <math.h>
#ifdef __GNUC__
#include <stdlib.h>
#else
#include <malloc.h>
#define alloca _alloca
#endif
/**********************************************************************/
/*   ____  ____                                                       */
/*  /   /\/   /                                                       */
/* /___/  \  /                                                        */
/* \   \   \/                                                         */
/*  \   \        Copyright (c) 2003-2013 Xilinx, Inc.                 */
/*  /   /        All Right Reserved.                                  */
/* /---/   /\                                                         */
/* \   \  /  \                                                        */
/*  \___\/\___\                                                       */
/**********************************************************************/


#include "iki.h"
#include <string.h>
#include <math.h>
#ifdef __GNUC__
#include <stdlib.h>
#else
#include <malloc.h>
#define alloca _alloca
#endif
typedef void (*funcp)(char *, char *);
extern int main(int, char**);
extern void execute_26(char*, char *);
extern void execute_27(char*, char *);
extern void execute_28(char*, char *);
extern void execute_136(char*, char *);
extern void execute_137(char*, char *);
extern void execute_138(char*, char *);
extern void execute_139(char*, char *);
extern void execute_140(char*, char *);
extern void execute_141(char*, char *);
extern void execute_83(char*, char *);
extern void execute_84(char*, char *);
extern void execute_85(char*, char *);
extern void execute_86(char*, char *);
extern void execute_87(char*, char *);
extern void execute_88(char*, char *);
extern void execute_89(char*, char *);
extern void execute_90(char*, char *);
extern void execute_91(char*, char *);
extern void execute_32(char*, char *);
extern void execute_33(char*, char *);
extern void execute_40(char*, char *);
extern void execute_41(char*, char *);
extern void execute_42(char*, char *);
extern void execute_60(char*, char *);
extern void execute_61(char*, char *);
extern void execute_62(char*, char *);
extern void execute_80(char*, char *);
extern void execute_81(char*, char *);
extern void execute_82(char*, char *);
extern void execute_44(char*, char *);
extern void execute_46(char*, char *);
extern void execute_48(char*, char *);
extern void execute_50(char*, char *);
extern void execute_52(char*, char *);
extern void execute_54(char*, char *);
extern void execute_56(char*, char *);
extern void execute_58(char*, char *);
extern void execute_64(char*, char *);
extern void execute_66(char*, char *);
extern void execute_68(char*, char *);
extern void execute_70(char*, char *);
extern void execute_72(char*, char *);
extern void execute_74(char*, char *);
extern void execute_76(char*, char *);
extern void execute_78(char*, char *);
extern void vhdl_transfunc_eventcallback(char*, char*, unsigned, unsigned, unsigned, char *);
extern void transaction_5(char*, char*, unsigned, unsigned, unsigned);
funcp funcTab[47] = {(funcp)execute_26, (funcp)execute_27, (funcp)execute_28, (funcp)execute_136, (funcp)execute_137, (funcp)execute_138, (funcp)execute_139, (funcp)execute_140, (funcp)execute_141, (funcp)execute_83, (funcp)execute_84, (funcp)execute_85, (funcp)execute_86, (funcp)execute_87, (funcp)execute_88, (funcp)execute_89, (funcp)execute_90, (funcp)execute_91, (funcp)execute_32, (funcp)execute_33, (funcp)execute_40, (funcp)execute_41, (funcp)execute_42, (funcp)execute_60, (funcp)execute_61, (funcp)execute_62, (funcp)execute_80, (funcp)execute_81, (funcp)execute_82, (funcp)execute_44, (funcp)execute_46, (funcp)execute_48, (funcp)execute_50, (funcp)execute_52, (funcp)execute_54, (funcp)execute_56, (funcp)execute_58, (funcp)execute_64, (funcp)execute_66, (funcp)execute_68, (funcp)execute_70, (funcp)execute_72, (funcp)execute_74, (funcp)execute_76, (funcp)execute_78, (funcp)vhdl_transfunc_eventcallback, (funcp)transaction_5};
const int NumRelocateId= 47;

void relocate(char *dp)
{
	iki_relocate(dp, "xsim.dir/funcionamiento_completo_TB_behav/xsim.reloc",  (void **)funcTab, 47);
	iki_vhdl_file_variable_register(dp + 21528);
	iki_vhdl_file_variable_register(dp + 21584);


	/*Populate the transaction function pointer field in the whole net structure */
}

void sensitize(char *dp)
{
	iki_sensitize(dp, "xsim.dir/funcionamiento_completo_TB_behav/xsim.reloc");
}

void simulate(char *dp)
{
		iki_schedule_processes_at_time_zero(dp, "xsim.dir/funcionamiento_completo_TB_behav/xsim.reloc");
	// Initialize Verilog nets in mixed simulation, for the cases when the value at time 0 should be propagated from the mixed language Vhdl net
	iki_execute_processes();

	// Schedule resolution functions for the multiply driven Verilog nets that have strength
	// Schedule transaction functions for the singly driven Verilog nets that have strength

}
#include "iki_bridge.h"
void relocate(char *);

void sensitize(char *);

void simulate(char *);

extern SYSTEMCLIB_IMP_DLLSPEC void local_register_implicit_channel(int, char*);
extern void implicit_HDL_SCinstatiate();

extern SYSTEMCLIB_IMP_DLLSPEC int xsim_argc_copy ;
extern SYSTEMCLIB_IMP_DLLSPEC char** xsim_argv_copy ;

int main(int argc, char **argv)
{
    iki_heap_initialize("ms", "isimmm", 0, 2147483648) ;
    iki_set_sv_type_file_path_name("xsim.dir/funcionamiento_completo_TB_behav/xsim.svtype");
    iki_set_crvs_dump_file_path_name("xsim.dir/funcionamiento_completo_TB_behav/xsim.crvsdump");
    void* design_handle = iki_create_design("xsim.dir/funcionamiento_completo_TB_behav/xsim.mem", (void *)relocate, (void *)sensitize, (void *)simulate, 0, isimBridge_getWdbWriter(), 0, argc, argv);
     iki_set_rc_trial_count(100);
    (void) design_handle;
    return iki_simulate_design();
}
