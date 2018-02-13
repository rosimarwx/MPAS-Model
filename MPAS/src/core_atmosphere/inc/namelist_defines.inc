   subroutine atm_setup_nmlrec_nhyd_model(configPool, unitNumber, dminfo)
      use mpas_derived_types
      use mpas_dmpar
      use mpas_pool_routines
      use mpas_io_units
      implicit none
      type (mpas_pool_type), intent(inout) :: configPool
      integer, intent(in) :: unitNumber
      type (dm_info), intent(in) :: dminfo
      type (mpas_pool_type), pointer :: recordPool
      integer :: ierr

      character (len=StrKIND) :: config_time_integration = 'SRK3'
      integer :: config_time_integration_order = 2
      real (kind=RKIND) :: config_dt = 720.000000
      character (len=StrKIND) :: config_calendar_type = 'gregorian'
      character (len=StrKIND) :: config_start_time = '2010-10-23_00:00:00'
      character (len=StrKIND) :: config_stop_time = 'none'
      character (len=StrKIND) :: config_run_duration = '5_00:00:00'
      logical :: config_split_dynamics_transport = .true.
      integer :: config_number_of_sub_steps = 2
      integer :: config_dynamics_split_steps = 3
      real (kind=RKIND) :: config_h_mom_eddy_visc2 = 0.000000
      real (kind=RKIND) :: config_h_mom_eddy_visc4 = 0.000000
      real (kind=RKIND) :: config_v_mom_eddy_visc2 = 0.000000
      real (kind=RKIND) :: config_h_theta_eddy_visc2 = 0.000000
      real (kind=RKIND) :: config_h_theta_eddy_visc4 = 0.000000
      real (kind=RKIND) :: config_v_theta_eddy_visc2 = 0.000000
      character (len=StrKIND) :: config_horiz_mixing = '2d_smagorinsky'
      real (kind=RKIND) :: config_len_disp = 120000.000000
      real (kind=RKIND) :: config_visc4_2dsmag = 0.050000
      real (kind=RKIND) :: config_del4u_div_factor = 10.000000
      integer :: config_w_adv_order = 3
      integer :: config_theta_adv_order = 3
      integer :: config_scalar_adv_order = 3
      integer :: config_u_vadv_order = 3
      integer :: config_w_vadv_order = 3
      integer :: config_theta_vadv_order = 3
      integer :: config_scalar_vadv_order = 3
      logical :: config_scalar_advection = .true.
      logical :: config_positive_definite = .false.
      logical :: config_monotonic = .true.
      real (kind=RKIND) :: config_coef_3rd_order = 0.250000
      real (kind=RKIND) :: config_smagorinsky_coef = 0.125000
      logical :: config_mix_full = .true.
      real (kind=RKIND) :: config_epssm = 0.100000
      real (kind=RKIND) :: config_smdiv = 0.100000
      real (kind=RKIND) :: config_apvm_upwinding = 0.500000
      logical :: config_h_ScaleWithMesh = .true.
      integer :: config_num_halos = 2

      namelist /nhyd_model/ &
         config_time_integration, &
         config_time_integration_order, &
         config_dt, &
         config_calendar_type, &
         config_start_time, &
         config_stop_time, &
         config_run_duration, &
         config_split_dynamics_transport, &
         config_number_of_sub_steps, &
         config_dynamics_split_steps, &
         config_h_mom_eddy_visc2, &
         config_h_mom_eddy_visc4, &
         config_v_mom_eddy_visc2, &
         config_h_theta_eddy_visc2, &
         config_h_theta_eddy_visc4, &
         config_v_theta_eddy_visc2, &
         config_horiz_mixing, &
         config_len_disp, &
         config_visc4_2dsmag, &
         config_del4u_div_factor, &
         config_w_adv_order, &
         config_theta_adv_order, &
         config_scalar_adv_order, &
         config_u_vadv_order, &
         config_w_vadv_order, &
         config_theta_vadv_order, &
         config_scalar_vadv_order, &
         config_scalar_advection, &
         config_positive_definite, &
         config_monotonic, &
         config_coef_3rd_order, &
         config_smagorinsky_coef, &
         config_mix_full, &
         config_epssm, &
         config_smdiv, &
         config_apvm_upwinding, &
         config_h_ScaleWithMesh, &
         config_num_halos
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, nhyd_model, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_char(dminfo, config_time_integration)
         call mpas_dmpar_bcast_int(dminfo, config_time_integration_order)
         call mpas_dmpar_bcast_real(dminfo, config_dt)
         call mpas_dmpar_bcast_char(dminfo, config_calendar_type)
         call mpas_dmpar_bcast_char(dminfo, config_start_time)
         call mpas_dmpar_bcast_char(dminfo, config_stop_time)
         call mpas_dmpar_bcast_char(dminfo, config_run_duration)
         call mpas_dmpar_bcast_logical(dminfo, config_split_dynamics_transport)
         call mpas_dmpar_bcast_int(dminfo, config_number_of_sub_steps)
         call mpas_dmpar_bcast_int(dminfo, config_dynamics_split_steps)
         call mpas_dmpar_bcast_real(dminfo, config_h_mom_eddy_visc2)
         call mpas_dmpar_bcast_real(dminfo, config_h_mom_eddy_visc4)
         call mpas_dmpar_bcast_real(dminfo, config_v_mom_eddy_visc2)
         call mpas_dmpar_bcast_real(dminfo, config_h_theta_eddy_visc2)
         call mpas_dmpar_bcast_real(dminfo, config_h_theta_eddy_visc4)
         call mpas_dmpar_bcast_real(dminfo, config_v_theta_eddy_visc2)
         call mpas_dmpar_bcast_char(dminfo, config_horiz_mixing)
         call mpas_dmpar_bcast_real(dminfo, config_len_disp)
         call mpas_dmpar_bcast_real(dminfo, config_visc4_2dsmag)
         call mpas_dmpar_bcast_real(dminfo, config_del4u_div_factor)
         call mpas_dmpar_bcast_int(dminfo, config_w_adv_order)
         call mpas_dmpar_bcast_int(dminfo, config_theta_adv_order)
         call mpas_dmpar_bcast_int(dminfo, config_scalar_adv_order)
         call mpas_dmpar_bcast_int(dminfo, config_u_vadv_order)
         call mpas_dmpar_bcast_int(dminfo, config_w_vadv_order)
         call mpas_dmpar_bcast_int(dminfo, config_theta_vadv_order)
         call mpas_dmpar_bcast_int(dminfo, config_scalar_vadv_order)
         call mpas_dmpar_bcast_logical(dminfo, config_scalar_advection)
         call mpas_dmpar_bcast_logical(dminfo, config_positive_definite)
         call mpas_dmpar_bcast_logical(dminfo, config_monotonic)
         call mpas_dmpar_bcast_real(dminfo, config_coef_3rd_order)
         call mpas_dmpar_bcast_real(dminfo, config_smagorinsky_coef)
         call mpas_dmpar_bcast_logical(dminfo, config_mix_full)
         call mpas_dmpar_bcast_real(dminfo, config_epssm)
         call mpas_dmpar_bcast_real(dminfo, config_smdiv)
         call mpas_dmpar_bcast_real(dminfo, config_apvm_upwinding)
         call mpas_dmpar_bcast_logical(dminfo, config_h_ScaleWithMesh)
         call mpas_dmpar_bcast_int(dminfo, config_num_halos)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record nhyd_model'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        config_time_integration = ', trim(config_time_integration)
            write(stderrUnit,*) '        config_time_integration_order = ', config_time_integration_order
            write(stderrUnit,*) '        config_dt = ', config_dt
            write(stderrUnit,*) '        config_calendar_type = ', trim(config_calendar_type)
            write(stderrUnit,*) '        config_start_time = ', trim(config_start_time)
            write(stderrUnit,*) '        config_stop_time = ', trim(config_stop_time)
            write(stderrUnit,*) '        config_run_duration = ', trim(config_run_duration)
            write(stderrUnit,*) '        config_split_dynamics_transport = ', config_split_dynamics_transport
            write(stderrUnit,*) '        config_number_of_sub_steps = ', config_number_of_sub_steps
            write(stderrUnit,*) '        config_dynamics_split_steps = ', config_dynamics_split_steps
            write(stderrUnit,*) '        config_h_mom_eddy_visc2 = ', config_h_mom_eddy_visc2
            write(stderrUnit,*) '        config_h_mom_eddy_visc4 = ', config_h_mom_eddy_visc4
            write(stderrUnit,*) '        config_v_mom_eddy_visc2 = ', config_v_mom_eddy_visc2
            write(stderrUnit,*) '        config_h_theta_eddy_visc2 = ', config_h_theta_eddy_visc2
            write(stderrUnit,*) '        config_h_theta_eddy_visc4 = ', config_h_theta_eddy_visc4
            write(stderrUnit,*) '        config_v_theta_eddy_visc2 = ', config_v_theta_eddy_visc2
            write(stderrUnit,*) '        config_horiz_mixing = ', trim(config_horiz_mixing)
            write(stderrUnit,*) '        config_len_disp = ', config_len_disp
            write(stderrUnit,*) '        config_visc4_2dsmag = ', config_visc4_2dsmag
            write(stderrUnit,*) '        config_del4u_div_factor = ', config_del4u_div_factor
            write(stderrUnit,*) '        config_w_adv_order = ', config_w_adv_order
            write(stderrUnit,*) '        config_theta_adv_order = ', config_theta_adv_order
            write(stderrUnit,*) '        config_scalar_adv_order = ', config_scalar_adv_order
            write(stderrUnit,*) '        config_u_vadv_order = ', config_u_vadv_order
            write(stderrUnit,*) '        config_w_vadv_order = ', config_w_vadv_order
            write(stderrUnit,*) '        config_theta_vadv_order = ', config_theta_vadv_order
            write(stderrUnit,*) '        config_scalar_vadv_order = ', config_scalar_vadv_order
            write(stderrUnit,*) '        config_scalar_advection = ', config_scalar_advection
            write(stderrUnit,*) '        config_positive_definite = ', config_positive_definite
            write(stderrUnit,*) '        config_monotonic = ', config_monotonic
            write(stderrUnit,*) '        config_coef_3rd_order = ', config_coef_3rd_order
            write(stderrUnit,*) '        config_smagorinsky_coef = ', config_smagorinsky_coef
            write(stderrUnit,*) '        config_mix_full = ', config_mix_full
            write(stderrUnit,*) '        config_epssm = ', config_epssm
            write(stderrUnit,*) '        config_smdiv = ', config_smdiv
            write(stderrUnit,*) '        config_apvm_upwinding = ', config_apvm_upwinding
            write(stderrUnit,*) '        config_h_ScaleWithMesh = ', config_h_ScaleWithMesh
            write(stderrUnit,*) '        config_num_halos = ', config_num_halos
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record nhyd_model.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'config_time_integration', config_time_integration)
      call mpas_pool_add_config(configPool, 'config_time_integration_order', config_time_integration_order)
      call mpas_pool_add_config(configPool, 'config_dt', config_dt)
      call mpas_pool_add_config(configPool, 'config_calendar_type', config_calendar_type)
      call mpas_pool_add_config(configPool, 'config_start_time', config_start_time)
      call mpas_pool_add_config(configPool, 'config_stop_time', config_stop_time)
      call mpas_pool_add_config(configPool, 'config_run_duration', config_run_duration)
      call mpas_pool_add_config(configPool, 'config_split_dynamics_transport', config_split_dynamics_transport)
      call mpas_pool_add_config(configPool, 'config_number_of_sub_steps', config_number_of_sub_steps)
      call mpas_pool_add_config(configPool, 'config_dynamics_split_steps', config_dynamics_split_steps)
      call mpas_pool_add_config(configPool, 'config_h_mom_eddy_visc2', config_h_mom_eddy_visc2)
      call mpas_pool_add_config(configPool, 'config_h_mom_eddy_visc4', config_h_mom_eddy_visc4)
      call mpas_pool_add_config(configPool, 'config_v_mom_eddy_visc2', config_v_mom_eddy_visc2)
      call mpas_pool_add_config(configPool, 'config_h_theta_eddy_visc2', config_h_theta_eddy_visc2)
      call mpas_pool_add_config(configPool, 'config_h_theta_eddy_visc4', config_h_theta_eddy_visc4)
      call mpas_pool_add_config(configPool, 'config_v_theta_eddy_visc2', config_v_theta_eddy_visc2)
      call mpas_pool_add_config(configPool, 'config_horiz_mixing', config_horiz_mixing)
      call mpas_pool_add_config(configPool, 'config_len_disp', config_len_disp)
      call mpas_pool_add_config(configPool, 'config_visc4_2dsmag', config_visc4_2dsmag)
      call mpas_pool_add_config(configPool, 'config_del4u_div_factor', config_del4u_div_factor)
      call mpas_pool_add_config(configPool, 'config_w_adv_order', config_w_adv_order)
      call mpas_pool_add_config(configPool, 'config_theta_adv_order', config_theta_adv_order)
      call mpas_pool_add_config(configPool, 'config_scalar_adv_order', config_scalar_adv_order)
      call mpas_pool_add_config(configPool, 'config_u_vadv_order', config_u_vadv_order)
      call mpas_pool_add_config(configPool, 'config_w_vadv_order', config_w_vadv_order)
      call mpas_pool_add_config(configPool, 'config_theta_vadv_order', config_theta_vadv_order)
      call mpas_pool_add_config(configPool, 'config_scalar_vadv_order', config_scalar_vadv_order)
      call mpas_pool_add_config(configPool, 'config_scalar_advection', config_scalar_advection)
      call mpas_pool_add_config(configPool, 'config_positive_definite', config_positive_definite)
      call mpas_pool_add_config(configPool, 'config_monotonic', config_monotonic)
      call mpas_pool_add_config(configPool, 'config_coef_3rd_order', config_coef_3rd_order)
      call mpas_pool_add_config(configPool, 'config_smagorinsky_coef', config_smagorinsky_coef)
      call mpas_pool_add_config(configPool, 'config_mix_full', config_mix_full)
      call mpas_pool_add_config(configPool, 'config_epssm', config_epssm)
      call mpas_pool_add_config(configPool, 'config_smdiv', config_smdiv)
      call mpas_pool_add_config(configPool, 'config_apvm_upwinding', config_apvm_upwinding)
      call mpas_pool_add_config(configPool, 'config_h_ScaleWithMesh', config_h_ScaleWithMesh)
      call mpas_pool_add_config(configPool, 'config_num_halos', config_num_halos)

   end subroutine atm_setup_nmlrec_nhyd_model


   subroutine atm_setup_nmlrec_damping(configPool, unitNumber, dminfo)
      use mpas_derived_types
      use mpas_dmpar
      use mpas_pool_routines
      use mpas_io_units
      implicit none
      type (mpas_pool_type), intent(inout) :: configPool
      integer, intent(in) :: unitNumber
      type (dm_info), intent(in) :: dminfo
      type (mpas_pool_type), pointer :: recordPool
      integer :: ierr

      real (kind=RKIND) :: config_zd = 22000.000000
      real (kind=RKIND) :: config_xnutr = 0.200000

      namelist /damping/ &
         config_zd, &
         config_xnutr
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, damping, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_real(dminfo, config_zd)
         call mpas_dmpar_bcast_real(dminfo, config_xnutr)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record damping'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        config_zd = ', config_zd
            write(stderrUnit,*) '        config_xnutr = ', config_xnutr
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record damping.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'config_zd', config_zd)
      call mpas_pool_add_config(configPool, 'config_xnutr', config_xnutr)

   end subroutine atm_setup_nmlrec_damping


   subroutine atm_setup_nmlrec_io(configPool, unitNumber, dminfo)
      use mpas_derived_types
      use mpas_dmpar
      use mpas_pool_routines
      use mpas_io_units
      implicit none
      type (mpas_pool_type), intent(inout) :: configPool
      integer, intent(in) :: unitNumber
      type (dm_info), intent(in) :: dminfo
      type (mpas_pool_type), pointer :: recordPool
      integer :: ierr

      character (len=StrKIND) :: config_restart_timestamp_name = 'restart_timestamp'
      integer :: config_pio_num_iotasks = 0
      integer :: config_pio_stride = 1

      namelist /io/ &
         config_restart_timestamp_name, &
         config_pio_num_iotasks, &
         config_pio_stride
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, io, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_char(dminfo, config_restart_timestamp_name)
         call mpas_dmpar_bcast_int(dminfo, config_pio_num_iotasks)
         call mpas_dmpar_bcast_int(dminfo, config_pio_stride)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record io'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        config_restart_timestamp_name = ', trim(config_restart_timestamp_name)
            write(stderrUnit,*) '        config_pio_num_iotasks = ', config_pio_num_iotasks
            write(stderrUnit,*) '        config_pio_stride = ', config_pio_stride
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record io.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'config_restart_timestamp_name', config_restart_timestamp_name)
      call mpas_pool_add_config(configPool, 'config_pio_num_iotasks', config_pio_num_iotasks)
      call mpas_pool_add_config(configPool, 'config_pio_stride', config_pio_stride)

   end subroutine atm_setup_nmlrec_io


   subroutine atm_setup_nmlrec_decomposition(configPool, unitNumber, dminfo)
      use mpas_derived_types
      use mpas_dmpar
      use mpas_pool_routines
      use mpas_io_units
      implicit none
      type (mpas_pool_type), intent(inout) :: configPool
      integer, intent(in) :: unitNumber
      type (dm_info), intent(in) :: dminfo
      type (mpas_pool_type), pointer :: recordPool
      integer :: ierr

      character (len=StrKIND) :: config_block_decomp_file_prefix = 'x1.40962.graph.info.part.'
      integer :: config_number_of_blocks = 0
      logical :: config_explicit_proc_decomp = .false.
      character (len=StrKIND) :: config_proc_decomp_file_prefix = 'graph.info.part.'

      namelist /decomposition/ &
         config_block_decomp_file_prefix, &
         config_number_of_blocks, &
         config_explicit_proc_decomp, &
         config_proc_decomp_file_prefix
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, decomposition, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_char(dminfo, config_block_decomp_file_prefix)
         call mpas_dmpar_bcast_int(dminfo, config_number_of_blocks)
         call mpas_dmpar_bcast_logical(dminfo, config_explicit_proc_decomp)
         call mpas_dmpar_bcast_char(dminfo, config_proc_decomp_file_prefix)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record decomposition'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        config_block_decomp_file_prefix = ', trim(config_block_decomp_file_prefix)
            write(stderrUnit,*) '        config_number_of_blocks = ', config_number_of_blocks
            write(stderrUnit,*) '        config_explicit_proc_decomp = ', config_explicit_proc_decomp
            write(stderrUnit,*) '        config_proc_decomp_file_prefix = ', trim(config_proc_decomp_file_prefix)
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record decomposition.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'config_block_decomp_file_prefix', config_block_decomp_file_prefix)
      call mpas_pool_add_config(configPool, 'config_number_of_blocks', config_number_of_blocks)
      call mpas_pool_add_config(configPool, 'config_explicit_proc_decomp', config_explicit_proc_decomp)
      call mpas_pool_add_config(configPool, 'config_proc_decomp_file_prefix', config_proc_decomp_file_prefix)

   end subroutine atm_setup_nmlrec_decomposition


   subroutine atm_setup_nmlrec_restart(configPool, unitNumber, dminfo)
      use mpas_derived_types
      use mpas_dmpar
      use mpas_pool_routines
      use mpas_io_units
      implicit none
      type (mpas_pool_type), intent(inout) :: configPool
      integer, intent(in) :: unitNumber
      type (dm_info), intent(in) :: dminfo
      type (mpas_pool_type), pointer :: recordPool
      integer :: ierr

      logical :: config_do_restart = .false.
      logical :: config_do_DAcycling = .false.

      namelist /restart/ &
         config_do_restart, &
         config_do_DAcycling
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, restart, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_logical(dminfo, config_do_restart)
         call mpas_dmpar_bcast_logical(dminfo, config_do_DAcycling)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record restart'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        config_do_restart = ', config_do_restart
            write(stderrUnit,*) '        config_do_DAcycling = ', config_do_DAcycling
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record restart.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'config_do_restart', config_do_restart)
      call mpas_pool_add_config(configPool, 'config_do_DAcycling', config_do_DAcycling)

   end subroutine atm_setup_nmlrec_restart


   subroutine atm_setup_nmlrec_printout(configPool, unitNumber, dminfo)
      use mpas_derived_types
      use mpas_dmpar
      use mpas_pool_routines
      use mpas_io_units
      implicit none
      type (mpas_pool_type), intent(inout) :: configPool
      integer, intent(in) :: unitNumber
      type (dm_info), intent(in) :: dminfo
      type (mpas_pool_type), pointer :: recordPool
      integer :: ierr

      logical :: config_print_global_minmax_vel = .true.
      logical :: config_print_detailed_minmax_vel = .false.
      logical :: config_print_global_minmax_sca = .false.

      namelist /printout/ &
         config_print_global_minmax_vel, &
         config_print_detailed_minmax_vel, &
         config_print_global_minmax_sca
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, printout, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_logical(dminfo, config_print_global_minmax_vel)
         call mpas_dmpar_bcast_logical(dminfo, config_print_detailed_minmax_vel)
         call mpas_dmpar_bcast_logical(dminfo, config_print_global_minmax_sca)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record printout'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        config_print_global_minmax_vel = ', config_print_global_minmax_vel
            write(stderrUnit,*) '        config_print_detailed_minmax_vel = ', config_print_detailed_minmax_vel
            write(stderrUnit,*) '        config_print_global_minmax_sca = ', config_print_global_minmax_sca
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record printout.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'config_print_global_minmax_vel', config_print_global_minmax_vel)
      call mpas_pool_add_config(configPool, 'config_print_detailed_minmax_vel', config_print_detailed_minmax_vel)
      call mpas_pool_add_config(configPool, 'config_print_global_minmax_sca', config_print_global_minmax_sca)

   end subroutine atm_setup_nmlrec_printout


   subroutine atm_setup_nmlrec_IAU(configPool, unitNumber, dminfo)
      use mpas_derived_types
      use mpas_dmpar
      use mpas_pool_routines
      use mpas_io_units
      implicit none
      type (mpas_pool_type), intent(inout) :: configPool
      integer, intent(in) :: unitNumber
      type (dm_info), intent(in) :: dminfo
      type (mpas_pool_type), pointer :: recordPool
      integer :: ierr

      character (len=StrKIND) :: config_IAU_option = 'off'
      real (kind=RKIND) :: config_IAU_window_length_s = 21600.000000

      namelist /IAU/ &
         config_IAU_option, &
         config_IAU_window_length_s
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, IAU, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_char(dminfo, config_IAU_option)
         call mpas_dmpar_bcast_real(dminfo, config_IAU_window_length_s)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record IAU'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        config_IAU_option = ', trim(config_IAU_option)
            write(stderrUnit,*) '        config_IAU_window_length_s = ', config_IAU_window_length_s
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record IAU.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'config_IAU_option', config_IAU_option)
      call mpas_pool_add_config(configPool, 'config_IAU_window_length_s', config_IAU_window_length_s)

   end subroutine atm_setup_nmlrec_IAU


   subroutine atm_setup_nmlrec_physics(configPool, unitNumber, dminfo)
      use mpas_derived_types
      use mpas_dmpar
      use mpas_pool_routines
      use mpas_io_units
      implicit none
      type (mpas_pool_type), intent(inout) :: configPool
      integer, intent(in) :: unitNumber
      type (dm_info), intent(in) :: dminfo
      type (mpas_pool_type), pointer :: recordPool
      integer :: ierr

      character (len=StrKIND) :: input_soil_data = 'STAS'
      integer :: input_soil_temperature_lag = 140
      integer :: num_soil_layers = 4
      integer :: months = 12
      integer :: noznlev = 59
      integer :: naerlev = 29
      integer :: camdim1 = 4
      logical :: config_frac_seaice = .true.
      logical :: config_sfc_albedo = .true.
      logical :: config_sfc_snowalbedo = .true.
      logical :: config_sst_update = .false.
      logical :: config_sstdiurn_update = .false.
      logical :: config_deepsoiltemp_update = .false.
      logical :: config_o3climatology = .false.
      logical :: config_equinox = .false.
      logical :: config_microp_re = .false.
      logical :: config_ysu_pblmix = .false.
      integer :: config_n_microp = 1
      character (len=StrKIND) :: config_radtlw_interval = '00:30:00'
      character (len=StrKIND) :: config_radtsw_interval = '00:30:00'
      character (len=StrKIND) :: config_conv_interval = 'none'
      character (len=StrKIND) :: config_pbl_interval = 'none'
      character (len=StrKIND) :: config_camrad_abs_update = '06:00:00'
      character (len=StrKIND) :: config_greeness_update = '24:00:00'
      character (len=StrKIND) :: config_bucket_update = 'none'
      character (len=StrKIND) :: config_physics_suite = 'mesoscale_reference'
      character (len=StrKIND) :: config_microp_scheme = 'suite'
      character (len=StrKIND) :: config_convection_scheme = 'suite'
      character (len=StrKIND) :: config_lsm_scheme = 'suite'
      character (len=StrKIND) :: config_pbl_scheme = 'suite'
      character (len=StrKIND) :: config_gwdo_scheme = 'suite'
      character (len=StrKIND) :: config_radt_cld_scheme = 'suite'
      character (len=StrKIND) :: config_radt_lw_scheme = 'suite'
      character (len=StrKIND) :: config_radt_sw_scheme = 'suite'
      character (len=StrKIND) :: config_sfclayer_scheme = 'suite'
      integer :: config_gfconv_closure_deep = 0
      integer :: config_gfconv_closure_shallow = 8
      real (kind=RKIND) :: config_bucket_radt = 1000000000.000000
      real (kind=RKIND) :: config_bucket_rainc = 100.000000
      real (kind=RKIND) :: config_bucket_rainnc = 100.000000
      logical :: config_oml1d = .false.
      real (kind=RKIND) :: config_oml_hml0 = 30.000000
      real (kind=RKIND) :: config_oml_gamma = 0.140000
      real (kind=RKIND) :: config_oml_relaxation_time = 864000.000000

      namelist /physics/ &
         input_soil_data, &
         input_soil_temperature_lag, &
         num_soil_layers, &
         months, &
         noznlev, &
         naerlev, &
         camdim1, &
         config_frac_seaice, &
         config_sfc_albedo, &
         config_sfc_snowalbedo, &
         config_sst_update, &
         config_sstdiurn_update, &
         config_deepsoiltemp_update, &
         config_o3climatology, &
         config_equinox, &
         config_microp_re, &
         config_ysu_pblmix, &
         config_n_microp, &
         config_radtlw_interval, &
         config_radtsw_interval, &
         config_conv_interval, &
         config_pbl_interval, &
         config_camrad_abs_update, &
         config_greeness_update, &
         config_bucket_update, &
         config_physics_suite, &
         config_microp_scheme, &
         config_convection_scheme, &
         config_lsm_scheme, &
         config_pbl_scheme, &
         config_gwdo_scheme, &
         config_radt_cld_scheme, &
         config_radt_lw_scheme, &
         config_radt_sw_scheme, &
         config_sfclayer_scheme, &
         config_gfconv_closure_deep, &
         config_gfconv_closure_shallow, &
         config_bucket_radt, &
         config_bucket_rainc, &
         config_bucket_rainnc, &
         config_oml1d, &
         config_oml_hml0, &
         config_oml_gamma, &
         config_oml_relaxation_time
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, physics, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_char(dminfo, input_soil_data)
         call mpas_dmpar_bcast_int(dminfo, input_soil_temperature_lag)
         call mpas_dmpar_bcast_int(dminfo, num_soil_layers)
         call mpas_dmpar_bcast_int(dminfo, months)
         call mpas_dmpar_bcast_int(dminfo, noznlev)
         call mpas_dmpar_bcast_int(dminfo, naerlev)
         call mpas_dmpar_bcast_int(dminfo, camdim1)
         call mpas_dmpar_bcast_logical(dminfo, config_frac_seaice)
         call mpas_dmpar_bcast_logical(dminfo, config_sfc_albedo)
         call mpas_dmpar_bcast_logical(dminfo, config_sfc_snowalbedo)
         call mpas_dmpar_bcast_logical(dminfo, config_sst_update)
         call mpas_dmpar_bcast_logical(dminfo, config_sstdiurn_update)
         call mpas_dmpar_bcast_logical(dminfo, config_deepsoiltemp_update)
         call mpas_dmpar_bcast_logical(dminfo, config_o3climatology)
         call mpas_dmpar_bcast_logical(dminfo, config_equinox)
         call mpas_dmpar_bcast_logical(dminfo, config_microp_re)
         call mpas_dmpar_bcast_logical(dminfo, config_ysu_pblmix)
         call mpas_dmpar_bcast_int(dminfo, config_n_microp)
         call mpas_dmpar_bcast_char(dminfo, config_radtlw_interval)
         call mpas_dmpar_bcast_char(dminfo, config_radtsw_interval)
         call mpas_dmpar_bcast_char(dminfo, config_conv_interval)
         call mpas_dmpar_bcast_char(dminfo, config_pbl_interval)
         call mpas_dmpar_bcast_char(dminfo, config_camrad_abs_update)
         call mpas_dmpar_bcast_char(dminfo, config_greeness_update)
         call mpas_dmpar_bcast_char(dminfo, config_bucket_update)
         call mpas_dmpar_bcast_char(dminfo, config_physics_suite)
         call mpas_dmpar_bcast_char(dminfo, config_microp_scheme)
         call mpas_dmpar_bcast_char(dminfo, config_convection_scheme)
         call mpas_dmpar_bcast_char(dminfo, config_lsm_scheme)
         call mpas_dmpar_bcast_char(dminfo, config_pbl_scheme)
         call mpas_dmpar_bcast_char(dminfo, config_gwdo_scheme)
         call mpas_dmpar_bcast_char(dminfo, config_radt_cld_scheme)
         call mpas_dmpar_bcast_char(dminfo, config_radt_lw_scheme)
         call mpas_dmpar_bcast_char(dminfo, config_radt_sw_scheme)
         call mpas_dmpar_bcast_char(dminfo, config_sfclayer_scheme)
         call mpas_dmpar_bcast_int(dminfo, config_gfconv_closure_deep)
         call mpas_dmpar_bcast_int(dminfo, config_gfconv_closure_shallow)
         call mpas_dmpar_bcast_real(dminfo, config_bucket_radt)
         call mpas_dmpar_bcast_real(dminfo, config_bucket_rainc)
         call mpas_dmpar_bcast_real(dminfo, config_bucket_rainnc)
         call mpas_dmpar_bcast_logical(dminfo, config_oml1d)
         call mpas_dmpar_bcast_real(dminfo, config_oml_hml0)
         call mpas_dmpar_bcast_real(dminfo, config_oml_gamma)
         call mpas_dmpar_bcast_real(dminfo, config_oml_relaxation_time)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record physics'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        input_soil_data = ', trim(input_soil_data)
            write(stderrUnit,*) '        input_soil_temperature_lag = ', input_soil_temperature_lag
            write(stderrUnit,*) '        num_soil_layers = ', num_soil_layers
            write(stderrUnit,*) '        months = ', months
            write(stderrUnit,*) '        noznlev = ', noznlev
            write(stderrUnit,*) '        naerlev = ', naerlev
            write(stderrUnit,*) '        camdim1 = ', camdim1
            write(stderrUnit,*) '        config_frac_seaice = ', config_frac_seaice
            write(stderrUnit,*) '        config_sfc_albedo = ', config_sfc_albedo
            write(stderrUnit,*) '        config_sfc_snowalbedo = ', config_sfc_snowalbedo
            write(stderrUnit,*) '        config_sst_update = ', config_sst_update
            write(stderrUnit,*) '        config_sstdiurn_update = ', config_sstdiurn_update
            write(stderrUnit,*) '        config_deepsoiltemp_update = ', config_deepsoiltemp_update
            write(stderrUnit,*) '        config_o3climatology = ', config_o3climatology
            write(stderrUnit,*) '        config_equinox = ', config_equinox
            write(stderrUnit,*) '        config_microp_re = ', config_microp_re
            write(stderrUnit,*) '        config_ysu_pblmix = ', config_ysu_pblmix
            write(stderrUnit,*) '        config_n_microp = ', config_n_microp
            write(stderrUnit,*) '        config_radtlw_interval = ', trim(config_radtlw_interval)
            write(stderrUnit,*) '        config_radtsw_interval = ', trim(config_radtsw_interval)
            write(stderrUnit,*) '        config_conv_interval = ', trim(config_conv_interval)
            write(stderrUnit,*) '        config_pbl_interval = ', trim(config_pbl_interval)
            write(stderrUnit,*) '        config_camrad_abs_update = ', trim(config_camrad_abs_update)
            write(stderrUnit,*) '        config_greeness_update = ', trim(config_greeness_update)
            write(stderrUnit,*) '        config_bucket_update = ', trim(config_bucket_update)
            write(stderrUnit,*) '        config_physics_suite = ', trim(config_physics_suite)
            write(stderrUnit,*) '        config_microp_scheme = ', trim(config_microp_scheme)
            write(stderrUnit,*) '        config_convection_scheme = ', trim(config_convection_scheme)
            write(stderrUnit,*) '        config_lsm_scheme = ', trim(config_lsm_scheme)
            write(stderrUnit,*) '        config_pbl_scheme = ', trim(config_pbl_scheme)
            write(stderrUnit,*) '        config_gwdo_scheme = ', trim(config_gwdo_scheme)
            write(stderrUnit,*) '        config_radt_cld_scheme = ', trim(config_radt_cld_scheme)
            write(stderrUnit,*) '        config_radt_lw_scheme = ', trim(config_radt_lw_scheme)
            write(stderrUnit,*) '        config_radt_sw_scheme = ', trim(config_radt_sw_scheme)
            write(stderrUnit,*) '        config_sfclayer_scheme = ', trim(config_sfclayer_scheme)
            write(stderrUnit,*) '        config_gfconv_closure_deep = ', config_gfconv_closure_deep
            write(stderrUnit,*) '        config_gfconv_closure_shallow = ', config_gfconv_closure_shallow
            write(stderrUnit,*) '        config_bucket_radt = ', config_bucket_radt
            write(stderrUnit,*) '        config_bucket_rainc = ', config_bucket_rainc
            write(stderrUnit,*) '        config_bucket_rainnc = ', config_bucket_rainnc
            write(stderrUnit,*) '        config_oml1d = ', config_oml1d
            write(stderrUnit,*) '        config_oml_hml0 = ', config_oml_hml0
            write(stderrUnit,*) '        config_oml_gamma = ', config_oml_gamma
            write(stderrUnit,*) '        config_oml_relaxation_time = ', config_oml_relaxation_time
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record physics.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'input_soil_data', input_soil_data)
      call mpas_pool_add_config(configPool, 'input_soil_temperature_lag', input_soil_temperature_lag)
      call mpas_pool_add_config(configPool, 'num_soil_layers', num_soil_layers)
      call mpas_pool_add_config(configPool, 'months', months)
      call mpas_pool_add_config(configPool, 'noznlev', noznlev)
      call mpas_pool_add_config(configPool, 'naerlev', naerlev)
      call mpas_pool_add_config(configPool, 'camdim1', camdim1)
      call mpas_pool_add_config(configPool, 'config_frac_seaice', config_frac_seaice)
      call mpas_pool_add_config(configPool, 'config_sfc_albedo', config_sfc_albedo)
      call mpas_pool_add_config(configPool, 'config_sfc_snowalbedo', config_sfc_snowalbedo)
      call mpas_pool_add_config(configPool, 'config_sst_update', config_sst_update)
      call mpas_pool_add_config(configPool, 'config_sstdiurn_update', config_sstdiurn_update)
      call mpas_pool_add_config(configPool, 'config_deepsoiltemp_update', config_deepsoiltemp_update)
      call mpas_pool_add_config(configPool, 'config_o3climatology', config_o3climatology)
      call mpas_pool_add_config(configPool, 'config_equinox', config_equinox)
      call mpas_pool_add_config(configPool, 'config_microp_re', config_microp_re)
      call mpas_pool_add_config(configPool, 'config_ysu_pblmix', config_ysu_pblmix)
      call mpas_pool_add_config(configPool, 'config_n_microp', config_n_microp)
      call mpas_pool_add_config(configPool, 'config_radtlw_interval', config_radtlw_interval)
      call mpas_pool_add_config(configPool, 'config_radtsw_interval', config_radtsw_interval)
      call mpas_pool_add_config(configPool, 'config_conv_interval', config_conv_interval)
      call mpas_pool_add_config(configPool, 'config_pbl_interval', config_pbl_interval)
      call mpas_pool_add_config(configPool, 'config_camrad_abs_update', config_camrad_abs_update)
      call mpas_pool_add_config(configPool, 'config_greeness_update', config_greeness_update)
      call mpas_pool_add_config(configPool, 'config_bucket_update', config_bucket_update)
      call mpas_pool_add_config(configPool, 'config_physics_suite', config_physics_suite)
      call mpas_pool_add_config(configPool, 'config_microp_scheme', config_microp_scheme)
      call mpas_pool_add_config(configPool, 'config_convection_scheme', config_convection_scheme)
      call mpas_pool_add_config(configPool, 'config_lsm_scheme', config_lsm_scheme)
      call mpas_pool_add_config(configPool, 'config_pbl_scheme', config_pbl_scheme)
      call mpas_pool_add_config(configPool, 'config_gwdo_scheme', config_gwdo_scheme)
      call mpas_pool_add_config(configPool, 'config_radt_cld_scheme', config_radt_cld_scheme)
      call mpas_pool_add_config(configPool, 'config_radt_lw_scheme', config_radt_lw_scheme)
      call mpas_pool_add_config(configPool, 'config_radt_sw_scheme', config_radt_sw_scheme)
      call mpas_pool_add_config(configPool, 'config_sfclayer_scheme', config_sfclayer_scheme)
      call mpas_pool_add_config(configPool, 'config_gfconv_closure_deep', config_gfconv_closure_deep)
      call mpas_pool_add_config(configPool, 'config_gfconv_closure_shallow', config_gfconv_closure_shallow)
      call mpas_pool_add_config(configPool, 'config_bucket_radt', config_bucket_radt)
      call mpas_pool_add_config(configPool, 'config_bucket_rainc', config_bucket_rainc)
      call mpas_pool_add_config(configPool, 'config_bucket_rainnc', config_bucket_rainnc)
      call mpas_pool_add_config(configPool, 'config_oml1d', config_oml1d)
      call mpas_pool_add_config(configPool, 'config_oml_hml0', config_oml_hml0)
      call mpas_pool_add_config(configPool, 'config_oml_gamma', config_oml_gamma)
      call mpas_pool_add_config(configPool, 'config_oml_relaxation_time', config_oml_relaxation_time)

   end subroutine atm_setup_nmlrec_physics


   subroutine atm_setup_nmlrec_soundings(configPool, unitNumber, dminfo)
      use mpas_derived_types
      use mpas_dmpar
      use mpas_pool_routines
      use mpas_io_units
      implicit none
      type (mpas_pool_type), intent(inout) :: configPool
      integer, intent(in) :: unitNumber
      type (dm_info), intent(in) :: dminfo
      type (mpas_pool_type), pointer :: recordPool
      integer :: ierr

      character (len=StrKIND) :: config_sounding_interval = 'none'

      namelist /soundings/ &
         config_sounding_interval
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, soundings, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_char(dminfo, config_sounding_interval)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record soundings'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        config_sounding_interval = ', trim(config_sounding_interval)
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record soundings.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'config_sounding_interval', config_sounding_interval)

   end subroutine atm_setup_nmlrec_soundings


