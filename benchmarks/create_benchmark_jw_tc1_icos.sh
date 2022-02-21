#!/bin/bash
#-----------------------------------------------------------#
#   Creates namelist/streams for a certain grid/parameters
#   Creates a folder for the test to be run
#   
#-----------------------------------------------------------#

#Main directory for MPAS - do not put / in the end !!!
WORK_DIR=$MPAS_DIR
BENCH_DIR=$WORK_DIR/benchmarks

#---------------------------------
# Set here the desired parameters
#---------------------------------

#Grid
GRD_NAME="mpas-a-test_mpas"
#GRD_NAME="HR95xxx"
GRD_DIR=${WORK_DIR}"/grids/utilities/jigsaw/"${GRD_NAME}
#GRD_DIR=${MPAS_DIR}"/grids/"x1.10242

#Init
BENCH_NAME="jw_tc1_icos"
BENCH_DIR=${BENCH_DIR}/${BENCH_NAME}

LEVELS=20     #Vertical levels
TC=1          #Test case JW 1,2, 3
HCM=0         #1=HCm, 0=HCt

#Output and Diagnostics
USE_SEP_FILES=0
USE_REDUCED_OUT=1

#Parameters Runtime 
#Remmember to pre-set in namelist:
# 1=true, 0=false

# Time
DT=120.0
RUN_NML="config_run_duration = '1_00:00:00'"
OUT_INT='output_interval="1_00:00:00"'

#Filters and diffusion
SMAG=120000.	#Smagorinsky horizontal diffusion length (set to mean grid length)
SMDV=0.1	#3d div damp coef (non-dimensional - default =0.1)
APVM=0.5	#Vorticicy filter (default=0.5)
HOLS=1		#Use A. Gassmann Hollingsworth correction

#Consistent scheme options
CONS=0		#Full consistent scheme (overwrites options bellow)
KPER=0		#Perot kinetic energy
KRBF=0		#RBF kinetic energy
BAED=0		#Edge interpolation with barycentric coords
BAVT=0		#Vertex interpolation with barycentric interpolation
PERP=0		#Consistent perpendicular term

#####------------------FUNCTIONS TO LOAD VARS INTO NMLIST AND STREAMS----------------#########################

#set vars
function setup_vars (){
	echo
	NAME=${GRD_NAME}

	#INIT
	TC_NML="config_init_case = "${TC}
	NAME=${NAME}.tc${TC}
	
	LEVELS_NML="config_nvertlevels = "${LEVELS}
	NAME=${NAME}.lv${LEVELS}
	
	
	if [ $HCM -eq 1 ] ;then 
	    HCM_NAME=".hcm"
	    HCM_NML="config_hcm_staggering = true "
	    NAME=${NAME}.hcm
	else
	    HCM_NAME=""
	    HCM_NML="config_hcm_staggering = false "	
    	    NAME=${NAME}.hct
	fi
	INIT_NAME=${NAME}.init
	SURF_NAME=${NAME}.sfc_update

	#ATMOSPHERE
	DT_NML="config_dt = "${DT}
	NAME=${NAME}.dt${DT}
	SMAG_NML="config_len_disp            = "${SMAG}
	NAME=${NAME}.smag${SMAG}
	SMDV_NML="config_smdiv = "${SMDV}
	NAME=${NAME}smdv${SMDV}
	APVM_NML="config_apvm_upwinding = "${APVM}
	NAME=${NAME}.apvm${APVM}
	
	if [ $CONS -eq 1 ] ;then 
		CONS_NML="config_consistent_scheme = true"
		NAME=${NAME}.consist
		
		KPER_NML="config_KE_vecrecon_perot = true"
		KRBF_NML="config_KE_vecrecon_rbf = false"
		BAED_NML="config_bary_interpol_edge = true"
		BAVT_NML="config_bary_interpol_vertex = true"
		PERP_NML="config_consist_perp = true"
	else
		CONS_NML="config_consistent_scheme = false"	
		if [ $KPER -eq 1 ] ;then 
			KPER_NML="config_KE_vecrecon_perot = true"
			KRBF_NML="config_KE_vecrecon_rbf = false"
			NAME=${NAME}.kper
		elif [ $KRBF -eq 1 ] ;then 
			KPER_NML="config_KE_vecrecon_perot = false"
			KRBF_NML="config_KE_vecrecon_rbf = true"			
			NAME=${NAME}.krbf
		else
			KPER_NML="config_KE_vecrecon_perot = false"
			KRBF_NML="config_KE_vecrecon_rbf = false"			
			NAME=${NAME}.ktrsk
		fi
		if [ $PERP -eq 1 ] ;then 
			PERP_NML="config_consist_perp = true"
			NAME=${NAME}.cperp
		else
			PERP_NML="config_consist_perp = false"
			NAME=${NAME}.ctrsk			
		fi
		if [ $BAED -eq 1 ] ;then 
			BAED_NML="config_bary_interpol_edge = true"
			NAME=${NAME}.baed
		else
			BAED_NML="config_bary_interpol_edge = false"
		fi
		if [ $BAVT -eq 1 ] ;then 
			BAVT_NML="config_bary_interpol_vertex = true"
			NAME=${NAME}.bavt
		else
			BAVT_NML="config_bary_interpol_vertex = false"
		fi
	fi
	if [ $HOLS -eq 1 ] ;then 
		HOLS_NML="config_hollingsworth = true"
		NAME=${NAME}.hols
	else
		HOLS_NML="config_hollingsworth = false"			
	fi
	echo "Test case name:"
	echo $NAME
	echo ""
	INIT_DIR=${BENCH_DIR}"/"${NAME}"/init"
	RUN_DIR=${BENCH_DIR}"/"${NAME}"/run"

	#Graph path
	GRAPH_PATH="config_block_decomp_file_prefix = '${GRD_DIR}/${GRD_NAME}.graph.info.part.'"
	PHYSICS_NML="config_physics_suite = 'none'"
	if [ $USE_SEP_FILES -eq 1 ] ;then 
		DATE_NAME='.$Y-$M-$D_$h.$m.$s'
	else
		DATE_NAME=''

	fi
}



#NAMELIST.INIT
function namelist_init_atmosphere (){
	#backup original
	cp $MPAS_DIR/src/core_init_atmosphere/default_inputs/namelist.init_atmosphere namelist.init_atmosphere
	echo
	echo "Setup for namelist.init_atmosphere"
	#Write namelist.init
	sed -i "s/config_init_case.*/${TC_NML//\//\/}/" namelist.init_atmosphere
	echo ${TC_NML}
	sed -i "s/config_hcm_staggering.*/${HCM_NML//\//\/}/" namelist.init_atmosphere
	echo ${HCM_NML}
	sed -i "s/config_nvertlevels.*/${LEVELS_NML//\//\/}/" namelist.init_atmosphere
	echo ${LEVELS_NML}
	sed -i "s+config_block_decomp_file_prefix.*+${GRAPH_PATH//\//\/}+" namelist.init_atmosphere
	echo ${GRAPH_PATH//\//\/}
	}

#STREAMS.INIT
function streams_init_atmosphere (){
	cp $MPAS_DIR/src/core_init_atmosphere/default_inputs/streams.init_atmosphere streams.init_atmosphere
	echo
	echo "Setup for streams.init_atmosphere"
	INPUT="                  filename_template='${GRD_DIR}/${GRD_NAME}.grid.nc'"
	awk -v var="$INPUT" '{ if ( NR == 4 ) { print var;} else {print $0;} }'  streams.init_atmosphere > streams.init_atmosphere.tmp
	cp streams.init_atmosphere.tmp streams.init_atmosphere
	echo ${INPUT}
	
	OUTPUT="                  filename_template='${INIT_DIR}/${INIT_NAME}.nc'"
	awk -v var="$OUTPUT" '{ if ( NR == 9 ) { print var;} else {print $0;} }'  streams.init_atmosphere > streams.init_atmosphere.tmp
	cp streams.init_atmosphere.tmp streams.init_atmosphere
	echo ${OUTPUT}

	SURFACE="                  filename_template='${INIT_DIR}/${SURF_NAME}.nc'"
	awk -v var="$SURFACE" '{ if ( NR == 15 ) { print var;} else {print $0;} }'  streams.init_atmosphere > streams.init_atmosphere.tmp
	cp streams.init_atmosphere.tmp streams.init_atmosphere
	echo ${OUTPUT}

	}
	

#clobber mode="append"

#NAMELIST ATMOSPHERE
function namelist_atmosphere (){
	#backup original
	cp $MPAS_DIR/default_inputs/namelist.atmosphere namelist.atmosphere	
	echo 
	echo "Setup for namelist.atmosphere:"
	#Write namelist.init
	sed -i "s/config_dt.*/${DT_NML}/" namelist.atmosphere
	echo $DT_NML
	sed -i "s/config_run_duration.*/${RUN_NML}/" namelist.atmosphere
	echo $RUN_NML
	sed -i "s/config_len_disp.*/${SMAG_NML}/" namelist.atmosphere
	echo $SMAG_NML
	sed -i "s/config_apvm_upwinding.*/${APVM_NML}/" namelist.atmosphere
	echo $APVM_NML
	sed -i "s/config_hcm_staggering.*/${HCM_NML}/" namelist.atmosphere
	echo ${HCM_NML//\//\/}
	sed -i "s/config_consistent_scheme.*/${CONS_NML}/" namelist.atmosphere
	echo ${CONS_NML//\//\/}
	sed -i "s/config_KE_vecrecon_perot.*/${KPER_NML}/" namelist.atmosphere
	echo ${KPER_NML//\//\/}
	sed -i "s/config_KE_vecrecon_rbf.*/${KRBF_NML}/" namelist.atmosphere
	echo ${KRBF_NML//\//\/}
	sed -i "s/config_bary_interpol_edge.*/${BAED_NML}/" namelist.atmosphere
	echo ${BAED_NML//\//\/}
	sed -i "s/config_bary_interpol_vertex.*/${BAVT_NML}/" namelist.atmosphere
	echo ${BAVT_NML//\//\/}
	sed -i "s/config_consist_perp.*/${PERP_NML}/" namelist.atmosphere
	echo ${PERP_NML//\//\/}
	sed -i "s/config_hollingsworth.*/${HOLS_NML}/" namelist.atmosphere
	echo ${HOLS_NML}
	sed -i "s+config_block_decomp_file_prefix.*+${GRAPH_PATH//\//\/}+" namelist.atmosphere
	echo ${GRAPH_PATH//\//\/}
	sed -i "s+config_physics_suite.*+${PHYSICS_NML//\//\/}+" namelist.atmosphere
	echo ${PHYSICS_NML//\//\/}
	}

#STREAMS.ATM
function streams_atmosphere (){

	cp $MPAS_DIR/default_inputs/streams.atmosphere streams.atmosphere
	echo 
	echo "Setup for streams.atmosphere:"
	
	INPUT="                  filename_template='${INIT_DIR}/${INIT_NAME}.nc'"
	echo $INPUT
	awk -v var="$INPUT" '{ if ( NR == 4 ) { print var;} else {print $0;} }'  streams.atmosphere > streams.atmosphere.tmp
	cp streams.atmosphere.tmp streams.atmosphere
	
	OUTPUT="        filename_template='${RUN_DIR}/${NAME}${DATE_NAME}.out.nc'"
	echo $OUTPUT
	#	$Y-$M-$D_$h.$m.$s
	awk -v var="$OUTPUT" '{ if ( NR == 15 ) { print var;} else {print $0;} }'  streams.atmosphere > streams.atmosphere.tmp
	cp streams.atmosphere.tmp streams.atmosphere
	
	DIAG="        filename_template='${RUN_DIR}/${NAME}${DATE_NAME}.diag.nc'"
	echo $DIAG
	#	$Y-$M-$D_$h.$m.$s
	awk -v var="$DIAG" '{ if ( NR == 23 ) { print var;} else {print $0;} }'  streams.atmosphere > streams.atmosphere.tmp
	cp streams.atmosphere.tmp streams.atmosphere

	#sed -i 's+output_interval=[\"]*[\"]+${OUT_INT}+g' streams.atmosphere
	#echo ${OUT_INT}
	
	#Copy stream_lists
	cp $MPAS_DIR/default_inputs/stream_list.atmosphere.diagnostics stream_list.atmosphere.diagnostics
	cp $MPAS_DIR/default_inputs/stream_list.atmosphere.output stream_list.atmosphere.output
	cp $MPAS_DIR/default_inputs/stream_list.atmosphere.surface stream_list.atmosphere.surface
	
	#if [ $USE_REDUCED_OUT -eq 1 ] ;then 
	#	cp stream_list.atmosphere.output.reduced stream_list.atmosphere.output		
	#else
	#	cp stream_list.atmosphere.output.orig stream_list.atmosphere.output
	#fi	
	
	}
	

#####------------------MAIN ---------------------------------#########################	


if [[ -z $1 ]];
then 
    echo "Please provide an argument with 1, for init_atmosphere, or 0 for atmosphere"
    exit 0
else
    echo "Parameter passed = $1"
fi

#setup vars - sets up $NAME and parameters for namelists/streams
setup_vars

init=$1

if [[ $init -eq 1 ]]; then
	echo $INIT_DIR
	#Iinit directory structure
	mkdir -p ${INIT_DIR}
	cd ${INIT_DIR}
	echo "dir created"

	#Run directory structure
	#mkdir -p ${NAME}
	#cd ${NAME}
	ln -sf ${MPAS_DIR}/init_atmosphere_model 
	echo "link created"

	# Setup initial conditions
	namelist_init_atmosphere
	streams_init_atmosphere

	#Clean folder
	rm -rf stream*.tmp

else

	mkdir -p ${RUN_DIR}
	cd ${RUN_DIR}

	ln -sf ${MPAS_DIR}/atmosphere_model 

	# Setup run_time conditions
	namelist_atmosphere
	streams_atmosphere

	#Clean folder
	rm -rf stream*.tmp

fi


