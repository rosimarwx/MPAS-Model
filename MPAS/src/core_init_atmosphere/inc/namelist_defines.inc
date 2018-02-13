   subroutine init_atm_setup_nmlrec_nhyd_model(configPool, unitNumber, dminfo)
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

      integer :: config_init_case = 7
      character (len=StrKIND) :: config_calendar_type = 'gregorian'
      character (len=StrKIND) :: config_start_time = '2010-10-23_00:00:00'
      character (len=StrKIND) :: config_stop_time = '2010-10-23_00:00:00'
      integer :: config_theta_adv_order = 3
      real (kind=RKIND) :: config_coef_3rd_order = 0.250000
      integer :: config_num_halos = 2

      namelist /nhyd_model/ &
         config_init_case, &
         config_calendar_type, &
         config_start_time, &
         config_stop_time, &
         config_theta_adv_order, &
         config_coef_3rd_order, &
         config_num_halos
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, nhyd_model, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_int(dminfo, config_init_case)
         call mpas_dmpar_bcast_char(dminfo, config_calendar_type)
         call mpas_dmpar_bcast_char(dminfo, config_start_time)
         call mpas_dmpar_bcast_char(dminfo, config_stop_time)
         call mpas_dmpar_bcast_int(dminfo, config_theta_adv_order)
         call mpas_dmpar_bcast_real(dminfo, config_coef_3rd_order)
         call mpas_dmpar_bcast_int(dminfo, config_num_halos)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record nhyd_model'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        config_init_case = ', config_init_case
            write(stderrUnit,*) '        config_calendar_type = ', trim(config_calendar_type)
            write(stderrUnit,*) '        config_start_time = ', trim(config_start_time)
            write(stderrUnit,*) '        config_stop_time = ', trim(config_stop_time)
            write(stderrUnit,*) '        config_theta_adv_order = ', config_theta_adv_order
            write(stderrUnit,*) '        config_coef_3rd_order = ', config_coef_3rd_order
            write(stderrUnit,*) '        config_num_halos = ', config_num_halos
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record nhyd_model.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'config_init_case', config_init_case)
      call mpas_pool_add_config(configPool, 'config_calendar_type', config_calendar_type)
      call mpas_pool_add_config(configPool, 'config_start_time', config_start_time)
      call mpas_pool_add_config(configPool, 'config_stop_time', config_stop_time)
      call mpas_pool_add_config(configPool, 'config_theta_adv_order', config_theta_adv_order)
      call mpas_pool_add_config(configPool, 'config_coef_3rd_order', config_coef_3rd_order)
      call mpas_pool_add_config(configPool, 'config_num_halos', config_num_halos)

   end subroutine init_atm_setup_nmlrec_nhyd_model


   subroutine init_atm_setup_nmlrec_dimensions(configPool, unitNumber, dminfo)
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

      integer :: config_nvertlevels = 41
      integer :: config_nsoillevels = 4
      integer :: config_nfglevels = 38
      integer :: config_nfgsoillevels = 4
      integer :: config_months = 12
      integer :: noznlev = 59

      namelist /dimensions/ &
         config_nvertlevels, &
         config_nsoillevels, &
         config_nfglevels, &
         config_nfgsoillevels, &
         config_months, &
         noznlev
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, dimensions, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_int(dminfo, config_nvertlevels)
         call mpas_dmpar_bcast_int(dminfo, config_nsoillevels)
         call mpas_dmpar_bcast_int(dminfo, config_nfglevels)
         call mpas_dmpar_bcast_int(dminfo, config_nfgsoillevels)
         call mpas_dmpar_bcast_int(dminfo, config_months)
         call mpas_dmpar_bcast_int(dminfo, noznlev)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record dimensions'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        config_nvertlevels = ', config_nvertlevels
            write(stderrUnit,*) '        config_nsoillevels = ', config_nsoillevels
            write(stderrUnit,*) '        config_nfglevels = ', config_nfglevels
            write(stderrUnit,*) '        config_nfgsoillevels = ', config_nfgsoillevels
            write(stderrUnit,*) '        config_months = ', config_months
            write(stderrUnit,*) '        noznlev = ', noznlev
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record dimensions.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'config_nvertlevels', config_nvertlevels)
      call mpas_pool_add_config(configPool, 'config_nsoillevels', config_nsoillevels)
      call mpas_pool_add_config(configPool, 'config_nfglevels', config_nfglevels)
      call mpas_pool_add_config(configPool, 'config_nfgsoillevels', config_nfgsoillevels)
      call mpas_pool_add_config(configPool, 'config_months', config_months)
      call mpas_pool_add_config(configPool, 'noznlev', noznlev)

   end subroutine init_atm_setup_nmlrec_dimensions


   subroutine init_atm_setup_nmlrec_data_sources(configPool, unitNumber, dminfo)
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

      character (len=StrKIND) :: config_geog_data_path = '/glade/p/work/wrfhelp/WPS_GEOG/'
      character (len=StrKIND) :: config_met_prefix = 'CFSR'
      character (len=StrKIND) :: config_sfc_prefix = 'SST'
      integer :: config_fg_interval = 86400
      character (len=StrKIND) :: config_landuse_data = 'USGS'
      logical :: config_use_spechumd = .false.

      namelist /data_sources/ &
         config_geog_data_path, &
         config_met_prefix, &
         config_sfc_prefix, &
         config_fg_interval, &
         config_landuse_data, &
         config_use_spechumd
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, data_sources, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_char(dminfo, config_geog_data_path)
         call mpas_dmpar_bcast_char(dminfo, config_met_prefix)
         call mpas_dmpar_bcast_char(dminfo, config_sfc_prefix)
         call mpas_dmpar_bcast_int(dminfo, config_fg_interval)
         call mpas_dmpar_bcast_char(dminfo, config_landuse_data)
         call mpas_dmpar_bcast_logical(dminfo, config_use_spechumd)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record data_sources'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        config_geog_data_path = ', trim(config_geog_data_path)
            write(stderrUnit,*) '        config_met_prefix = ', trim(config_met_prefix)
            write(stderrUnit,*) '        config_sfc_prefix = ', trim(config_sfc_prefix)
            write(stderrUnit,*) '        config_fg_interval = ', config_fg_interval
            write(stderrUnit,*) '        config_landuse_data = ', trim(config_landuse_data)
            write(stderrUnit,*) '        config_use_spechumd = ', config_use_spechumd
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record data_sources.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'config_geog_data_path', config_geog_data_path)
      call mpas_pool_add_config(configPool, 'config_met_prefix', config_met_prefix)
      call mpas_pool_add_config(configPool, 'config_sfc_prefix', config_sfc_prefix)
      call mpas_pool_add_config(configPool, 'config_fg_interval', config_fg_interval)
      call mpas_pool_add_config(configPool, 'config_landuse_data', config_landuse_data)
      call mpas_pool_add_config(configPool, 'config_use_spechumd', config_use_spechumd)

   end subroutine init_atm_setup_nmlrec_data_sources


   subroutine init_atm_setup_nmlrec_vertical_grid(configPool, unitNumber, dminfo)
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

      real (kind=RKIND) :: config_ztop = 30000.000000
      integer :: config_nsmterrain = 1
      logical :: config_smooth_surfaces = .true.
      real (kind=RKIND) :: config_dzmin = 0.300000
      integer :: config_nsm = 30
      logical :: config_tc_vertical_grid = .true.

      namelist /vertical_grid/ &
         config_ztop, &
         config_nsmterrain, &
         config_smooth_surfaces, &
         config_dzmin, &
         config_nsm, &
         config_tc_vertical_grid
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, vertical_grid, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_real(dminfo, config_ztop)
         call mpas_dmpar_bcast_int(dminfo, config_nsmterrain)
         call mpas_dmpar_bcast_logical(dminfo, config_smooth_surfaces)
         call mpas_dmpar_bcast_real(dminfo, config_dzmin)
         call mpas_dmpar_bcast_int(dminfo, config_nsm)
         call mpas_dmpar_bcast_logical(dminfo, config_tc_vertical_grid)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record vertical_grid'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        config_ztop = ', config_ztop
            write(stderrUnit,*) '        config_nsmterrain = ', config_nsmterrain
            write(stderrUnit,*) '        config_smooth_surfaces = ', config_smooth_surfaces
            write(stderrUnit,*) '        config_dzmin = ', config_dzmin
            write(stderrUnit,*) '        config_nsm = ', config_nsm
            write(stderrUnit,*) '        config_tc_vertical_grid = ', config_tc_vertical_grid
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record vertical_grid.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'config_ztop', config_ztop)
      call mpas_pool_add_config(configPool, 'config_nsmterrain', config_nsmterrain)
      call mpas_pool_add_config(configPool, 'config_smooth_surfaces', config_smooth_surfaces)
      call mpas_pool_add_config(configPool, 'config_dzmin', config_dzmin)
      call mpas_pool_add_config(configPool, 'config_nsm', config_nsm)
      call mpas_pool_add_config(configPool, 'config_tc_vertical_grid', config_tc_vertical_grid)

   end subroutine init_atm_setup_nmlrec_vertical_grid


   subroutine init_atm_setup_nmlrec_interpolation_control(configPool, unitNumber, dminfo)
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

      character (len=StrKIND) :: config_extrap_airtemp = 'linear'

      namelist /interpolation_control/ &
         config_extrap_airtemp
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, interpolation_control, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_char(dminfo, config_extrap_airtemp)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record interpolation_control'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        config_extrap_airtemp = ', trim(config_extrap_airtemp)
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record interpolation_control.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'config_extrap_airtemp', config_extrap_airtemp)

   end subroutine init_atm_setup_nmlrec_interpolation_control


   subroutine init_atm_setup_nmlrec_preproc_stages(configPool, unitNumber, dminfo)
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

      logical :: config_static_interp = .true.
      logical :: config_native_gwd_static = .true.
      real (kind=RKIND) :: config_gwd_cell_scaling = 1.000000
      logical :: config_vertical_grid = .true.
      logical :: config_met_interp = .true.
      logical :: config_input_sst = .false.
      logical :: config_frac_seaice = .true.

      namelist /preproc_stages/ &
         config_static_interp, &
         config_native_gwd_static, &
         config_gwd_cell_scaling, &
         config_vertical_grid, &
         config_met_interp, &
         config_input_sst, &
         config_frac_seaice
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, preproc_stages, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_logical(dminfo, config_static_interp)
         call mpas_dmpar_bcast_logical(dminfo, config_native_gwd_static)
         call mpas_dmpar_bcast_real(dminfo, config_gwd_cell_scaling)
         call mpas_dmpar_bcast_logical(dminfo, config_vertical_grid)
         call mpas_dmpar_bcast_logical(dminfo, config_met_interp)
         call mpas_dmpar_bcast_logical(dminfo, config_input_sst)
         call mpas_dmpar_bcast_logical(dminfo, config_frac_seaice)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record preproc_stages'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        config_static_interp = ', config_static_interp
            write(stderrUnit,*) '        config_native_gwd_static = ', config_native_gwd_static
            write(stderrUnit,*) '        config_gwd_cell_scaling = ', config_gwd_cell_scaling
            write(stderrUnit,*) '        config_vertical_grid = ', config_vertical_grid
            write(stderrUnit,*) '        config_met_interp = ', config_met_interp
            write(stderrUnit,*) '        config_input_sst = ', config_input_sst
            write(stderrUnit,*) '        config_frac_seaice = ', config_frac_seaice
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record preproc_stages.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'config_static_interp', config_static_interp)
      call mpas_pool_add_config(configPool, 'config_native_gwd_static', config_native_gwd_static)
      call mpas_pool_add_config(configPool, 'config_gwd_cell_scaling', config_gwd_cell_scaling)
      call mpas_pool_add_config(configPool, 'config_vertical_grid', config_vertical_grid)
      call mpas_pool_add_config(configPool, 'config_met_interp', config_met_interp)
      call mpas_pool_add_config(configPool, 'config_input_sst', config_input_sst)
      call mpas_pool_add_config(configPool, 'config_frac_seaice', config_frac_seaice)

   end subroutine init_atm_setup_nmlrec_preproc_stages


   subroutine init_atm_setup_nmlrec_io(configPool, unitNumber, dminfo)
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

      integer :: config_pio_num_iotasks = 0
      integer :: config_pio_stride = 1

      namelist /io/ &
         config_pio_num_iotasks, &
         config_pio_stride
      if (dminfo % my_proc_id == IO_NODE) then
         rewind(unitNumber)
         read(unitNumber, io, iostat=ierr)
      end if
      call mpas_dmpar_bcast_int(dminfo, ierr)

      if (ierr <= 0) then
         call mpas_dmpar_bcast_int(dminfo, config_pio_num_iotasks)
         call mpas_dmpar_bcast_int(dminfo, config_pio_stride)
         if (ierr < 0) then
            write(stderrUnit,*) '*** Encountered an issue while attempting to read namelist record io'
            write(stderrUnit,*) '    The following values will be used for variables in this record:'
            write(stderrUnit,*) ' '
            write(stderrUnit,*) '        config_pio_num_iotasks = ', config_pio_num_iotasks
            write(stderrUnit,*) '        config_pio_stride = ', config_pio_stride
            write(stderrUnit,*) ' '
         end if
      else if (ierr > 0) then
         write(stderrUnit, *) 'Error while reading namelist record io.'
         call mpas_dmpar_abort(dminfo)
      end if

      call mpas_pool_add_config(configPool, 'config_pio_num_iotasks', config_pio_num_iotasks)
      call mpas_pool_add_config(configPool, 'config_pio_stride', config_pio_stride)

   end subroutine init_atm_setup_nmlrec_io


   subroutine init_atm_setup_nmlrec_decomposition(configPool, unitNumber, dminfo)
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

   end subroutine init_atm_setup_nmlrec_decomposition


