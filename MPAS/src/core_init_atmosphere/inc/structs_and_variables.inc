   subroutine init_atm_generate_pool_mesh(block, structPool, dimensionPool, packagePool)
      use mpas_derived_types
      use mpas_pool_routines
      use mpas_io_units
      use mpas_io, only : MPAS_REAL_FILLVAL, MPAS_INT_FILLVAL, MPAS_CHAR_FILLVAL
      implicit none
      type (block_type), intent(inout), pointer :: block
      type (mpas_pool_type), intent(inout) :: structPool
      type (mpas_pool_type), intent(inout) :: dimensionPool
      type (mpas_pool_type), intent(in) :: packagePool

      type (field0DReal), dimension(:), pointer :: r0Ptr
      type (field1DReal), dimension(:), pointer :: r1Ptr
      type (field2DReal), dimension(:), pointer :: r2Ptr
      type (field3DReal), dimension(:), pointer :: r3Ptr
      type (field4DReal), dimension(:), pointer :: r4Ptr
      type (field5DReal), dimension(:), pointer :: r5Ptr
      type (field0DInteger), dimension(:), pointer :: i0Ptr
      type (field1DInteger), dimension(:), pointer :: i1Ptr
      type (field2DInteger), dimension(:), pointer :: i2Ptr
      type (field3DInteger), dimension(:), pointer :: i3Ptr
      type (field0DChar), dimension(:), pointer :: c0Ptr
      type (field1DChar), dimension(:), pointer :: c1Ptr

      type (mpas_pool_type), pointer :: newSubPool
      integer :: group_counter
      logical :: group_started
      integer :: group_start
      integer :: index_counter
      integer, pointer :: const_index

      logical, pointer :: initial_condsActive
      logical, pointer :: sfc_updateActive
      logical, pointer :: gwd_stage_inActive
      logical, pointer :: vertical_stage_inActive
      logical, pointer :: vertical_stage_outActive
      logical, pointer :: met_stage_inActive
      logical, pointer :: met_stage_outActive


      integer :: numConstituents

      nullify(newSubPool)
      group_counter = -1
      group_started = .false.
      group_start = -1
      call mpas_pool_get_package(packagePool, 'initial_condsActive', initial_condsActive)
      call mpas_pool_get_package(packagePool, 'sfc_updateActive', sfc_updateActive)
      call mpas_pool_get_package(packagePool, 'gwd_stage_inActive', gwd_stage_inActive)
      call mpas_pool_get_package(packagePool, 'vertical_stage_inActive', vertical_stage_inActive)
      call mpas_pool_get_package(packagePool, 'vertical_stage_outActive', vertical_stage_outActive)
      call mpas_pool_get_package(packagePool, 'met_stage_inActive', met_stage_inActive)
      call mpas_pool_get_package(packagePool, 'met_stage_outActive', met_stage_outActive)

      allocate(newSubPool)
      call mpas_pool_create_pool(newSubPool)
      call mpas_pool_add_subpool(structPool, 'mesh', newSubPool)
      call mpas_pool_add_subpool(block % allStructs, 'mesh', newSubPool)

! Define variable latCell
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'latCell'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'latCell', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'latCell', r1Ptr)

! Define variable lonCell
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'lonCell'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'lonCell', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'lonCell', r1Ptr)

! Define variable xCell
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'xCell'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'xCell', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'xCell', r1Ptr)

! Define variable yCell
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'yCell'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'yCell', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'yCell', r1Ptr)

! Define variable zCell
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'zCell'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'zCell', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'zCell', r1Ptr)

! Define variable indexToCellID
      allocate(i1Ptr(1))

! Setting up time level 1
      i1Ptr(1) % fieldName = 'indexToCellID'
      i1Ptr(1) % isVarArray = .false.
      i1Ptr(1) % isDecomposed = .true.
      i1Ptr(1) % hasTimeDimension = .false.
      i1Ptr(1) % isPersistent = .true.
      i1Ptr(1) % isActive = .false.
! Setting up dimensions
      i1Ptr(1) % dimNames(1) = 'nCells'
     i1Ptr(1) % defaultValue = 0
     i1Ptr(1) % defaultValue = 0
     nullify(i1Ptr(1) % array)
      nullify(i1Ptr(1) % next)
      nullify(i1Ptr(1) % prev)
      nullify(i1Ptr(1) % sendList)
      nullify(i1Ptr(1) % recvList)
      nullify(i1Ptr(1) % copyList)
      allocate(i1Ptr(1) % attLists(1))
      allocate(i1Ptr(1) % attLists(1) % attList)
      i1Ptr(1) % attLists(1) % attList % attName = ''
      i1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i1Ptr(1) % attLists(1) % attList % next)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueRealA)
      i1Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i1Ptr(1) % block => block

      i1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'indexToCellID', i1Ptr)
      call mpas_pool_add_field(block % allFields, 'indexToCellID', i1Ptr)

! Define variable latEdge
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'latEdge'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nEdges'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'latEdge', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'latEdge', r1Ptr)

! Define variable lonEdge
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'lonEdge'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nEdges'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'lonEdge', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'lonEdge', r1Ptr)

! Define variable xEdge
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'xEdge'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nEdges'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'xEdge', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'xEdge', r1Ptr)

! Define variable yEdge
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'yEdge'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nEdges'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'yEdge', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'yEdge', r1Ptr)

! Define variable zEdge
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'zEdge'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nEdges'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'zEdge', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'zEdge', r1Ptr)

! Define variable indexToEdgeID
      allocate(i1Ptr(1))

! Setting up time level 1
      i1Ptr(1) % fieldName = 'indexToEdgeID'
      i1Ptr(1) % isVarArray = .false.
      i1Ptr(1) % isDecomposed = .true.
      i1Ptr(1) % hasTimeDimension = .false.
      i1Ptr(1) % isPersistent = .true.
      i1Ptr(1) % isActive = .false.
! Setting up dimensions
      i1Ptr(1) % dimNames(1) = 'nEdges'
     i1Ptr(1) % defaultValue = 0
     i1Ptr(1) % defaultValue = 0
     nullify(i1Ptr(1) % array)
      nullify(i1Ptr(1) % next)
      nullify(i1Ptr(1) % prev)
      nullify(i1Ptr(1) % sendList)
      nullify(i1Ptr(1) % recvList)
      nullify(i1Ptr(1) % copyList)
      allocate(i1Ptr(1) % attLists(1))
      allocate(i1Ptr(1) % attLists(1) % attList)
      i1Ptr(1) % attLists(1) % attList % attName = ''
      i1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i1Ptr(1) % attLists(1) % attList % next)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueRealA)
      i1Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i1Ptr(1) % block => block

      i1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'indexToEdgeID', i1Ptr)
      call mpas_pool_add_field(block % allFields, 'indexToEdgeID', i1Ptr)

! Define variable latVertex
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'latVertex'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nVertices'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'latVertex', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'latVertex', r1Ptr)

! Define variable lonVertex
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'lonVertex'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nVertices'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'lonVertex', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'lonVertex', r1Ptr)

! Define variable xVertex
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'xVertex'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nVertices'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'xVertex', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'xVertex', r1Ptr)

! Define variable yVertex
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'yVertex'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nVertices'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'yVertex', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'yVertex', r1Ptr)

! Define variable zVertex
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'zVertex'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nVertices'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'zVertex', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'zVertex', r1Ptr)

! Define variable indexToVertexID
      allocate(i1Ptr(1))

! Setting up time level 1
      i1Ptr(1) % fieldName = 'indexToVertexID'
      i1Ptr(1) % isVarArray = .false.
      i1Ptr(1) % isDecomposed = .true.
      i1Ptr(1) % hasTimeDimension = .false.
      i1Ptr(1) % isPersistent = .true.
      i1Ptr(1) % isActive = .false.
! Setting up dimensions
      i1Ptr(1) % dimNames(1) = 'nVertices'
     i1Ptr(1) % defaultValue = 0
     i1Ptr(1) % defaultValue = 0
     nullify(i1Ptr(1) % array)
      nullify(i1Ptr(1) % next)
      nullify(i1Ptr(1) % prev)
      nullify(i1Ptr(1) % sendList)
      nullify(i1Ptr(1) % recvList)
      nullify(i1Ptr(1) % copyList)
      allocate(i1Ptr(1) % attLists(1))
      allocate(i1Ptr(1) % attLists(1) % attList)
      i1Ptr(1) % attLists(1) % attList % attName = ''
      i1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i1Ptr(1) % attLists(1) % attList % next)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueRealA)
      i1Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i1Ptr(1) % block => block

      i1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'indexToVertexID', i1Ptr)
      call mpas_pool_add_field(block % allFields, 'indexToVertexID', i1Ptr)

! Define variable cellsOnEdge
      allocate(i2Ptr(1))

! Setting up time level 1
      i2Ptr(1) % fieldName = 'cellsOnEdge'
      i2Ptr(1) % isVarArray = .false.
      i2Ptr(1) % isDecomposed = .true.
      i2Ptr(1) % hasTimeDimension = .false.
      i2Ptr(1) % isPersistent = .true.
      i2Ptr(1) % isActive = .false.
! Setting up dimensions
      i2Ptr(1) % dimNames(1) = 'TWO'
      i2Ptr(1) % dimNames(2) = 'nEdges'
     i2Ptr(1) % defaultValue = 0
     i2Ptr(1) % defaultValue = 0
     nullify(i2Ptr(1) % array)
      nullify(i2Ptr(1) % next)
      nullify(i2Ptr(1) % prev)
      nullify(i2Ptr(1) % sendList)
      nullify(i2Ptr(1) % recvList)
      nullify(i2Ptr(1) % copyList)
      allocate(i2Ptr(1) % attLists(1))
      allocate(i2Ptr(1) % attLists(1) % attList)
      i2Ptr(1) % attLists(1) % attList % attName = ''
      i2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i2Ptr(1) % attLists(1) % attList % next)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueRealA)
      i2Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i2Ptr(1) % block => block

      i2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'cellsOnEdge', i2Ptr)
      call mpas_pool_add_field(block % allFields, 'cellsOnEdge', i2Ptr)

! Define variable nEdgesOnCell
      allocate(i1Ptr(1))

! Setting up time level 1
      i1Ptr(1) % fieldName = 'nEdgesOnCell'
      i1Ptr(1) % isVarArray = .false.
      i1Ptr(1) % isDecomposed = .true.
      i1Ptr(1) % hasTimeDimension = .false.
      i1Ptr(1) % isPersistent = .true.
      i1Ptr(1) % isActive = .false.
! Setting up dimensions
      i1Ptr(1) % dimNames(1) = 'nCells'
     i1Ptr(1) % defaultValue = 0
     i1Ptr(1) % defaultValue = 0
     nullify(i1Ptr(1) % array)
      nullify(i1Ptr(1) % next)
      nullify(i1Ptr(1) % prev)
      nullify(i1Ptr(1) % sendList)
      nullify(i1Ptr(1) % recvList)
      nullify(i1Ptr(1) % copyList)
      allocate(i1Ptr(1) % attLists(1))
      allocate(i1Ptr(1) % attLists(1) % attList)
      i1Ptr(1) % attLists(1) % attList % attName = ''
      i1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i1Ptr(1) % attLists(1) % attList % next)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueRealA)
      i1Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i1Ptr(1) % block => block

      i1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'nEdgesOnCell', i1Ptr)
      call mpas_pool_add_field(block % allFields, 'nEdgesOnCell', i1Ptr)

! Define variable nEdgesOnEdge
      allocate(i1Ptr(1))

! Setting up time level 1
      i1Ptr(1) % fieldName = 'nEdgesOnEdge'
      i1Ptr(1) % isVarArray = .false.
      i1Ptr(1) % isDecomposed = .true.
      i1Ptr(1) % hasTimeDimension = .false.
      i1Ptr(1) % isPersistent = .true.
      i1Ptr(1) % isActive = .false.
! Setting up dimensions
      i1Ptr(1) % dimNames(1) = 'nEdges'
     i1Ptr(1) % defaultValue = 0
     i1Ptr(1) % defaultValue = 0
     nullify(i1Ptr(1) % array)
      nullify(i1Ptr(1) % next)
      nullify(i1Ptr(1) % prev)
      nullify(i1Ptr(1) % sendList)
      nullify(i1Ptr(1) % recvList)
      nullify(i1Ptr(1) % copyList)
      allocate(i1Ptr(1) % attLists(1))
      allocate(i1Ptr(1) % attLists(1) % attList)
      i1Ptr(1) % attLists(1) % attList % attName = ''
      i1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i1Ptr(1) % attLists(1) % attList % next)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueRealA)
      i1Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i1Ptr(1) % block => block

      i1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'nEdgesOnEdge', i1Ptr)
      call mpas_pool_add_field(block % allFields, 'nEdgesOnEdge', i1Ptr)

! Define variable edgesOnCell
      allocate(i2Ptr(1))

! Setting up time level 1
      i2Ptr(1) % fieldName = 'edgesOnCell'
      i2Ptr(1) % isVarArray = .false.
      i2Ptr(1) % isDecomposed = .true.
      i2Ptr(1) % hasTimeDimension = .false.
      i2Ptr(1) % isPersistent = .true.
      i2Ptr(1) % isActive = .false.
! Setting up dimensions
      i2Ptr(1) % dimNames(1) = 'maxEdges'
      i2Ptr(1) % dimNames(2) = 'nCells'
     i2Ptr(1) % defaultValue = 0
     i2Ptr(1) % defaultValue = 0
     nullify(i2Ptr(1) % array)
      nullify(i2Ptr(1) % next)
      nullify(i2Ptr(1) % prev)
      nullify(i2Ptr(1) % sendList)
      nullify(i2Ptr(1) % recvList)
      nullify(i2Ptr(1) % copyList)
      allocate(i2Ptr(1) % attLists(1))
      allocate(i2Ptr(1) % attLists(1) % attList)
      i2Ptr(1) % attLists(1) % attList % attName = ''
      i2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i2Ptr(1) % attLists(1) % attList % next)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueRealA)
      i2Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i2Ptr(1) % block => block

      i2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'edgesOnCell', i2Ptr)
      call mpas_pool_add_field(block % allFields, 'edgesOnCell', i2Ptr)

! Define variable edgesOnEdge
      allocate(i2Ptr(1))

! Setting up time level 1
      i2Ptr(1) % fieldName = 'edgesOnEdge'
      i2Ptr(1) % isVarArray = .false.
      i2Ptr(1) % isDecomposed = .true.
      i2Ptr(1) % hasTimeDimension = .false.
      i2Ptr(1) % isPersistent = .true.
      i2Ptr(1) % isActive = .false.
! Setting up dimensions
      i2Ptr(1) % dimNames(1) = 'maxEdges2'
      i2Ptr(1) % dimNames(2) = 'nEdges'
     i2Ptr(1) % defaultValue = 0
     i2Ptr(1) % defaultValue = 0
     nullify(i2Ptr(1) % array)
      nullify(i2Ptr(1) % next)
      nullify(i2Ptr(1) % prev)
      nullify(i2Ptr(1) % sendList)
      nullify(i2Ptr(1) % recvList)
      nullify(i2Ptr(1) % copyList)
      allocate(i2Ptr(1) % attLists(1))
      allocate(i2Ptr(1) % attLists(1) % attList)
      i2Ptr(1) % attLists(1) % attList % attName = ''
      i2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i2Ptr(1) % attLists(1) % attList % next)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueRealA)
      i2Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i2Ptr(1) % block => block

      i2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'edgesOnEdge', i2Ptr)
      call mpas_pool_add_field(block % allFields, 'edgesOnEdge', i2Ptr)

! Define variable weightsOnEdge
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'weightsOnEdge'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .false.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'maxEdges2'
      r2Ptr(1) % dimNames(2) = 'nEdges'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'weightsOnEdge', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'weightsOnEdge', r2Ptr)

! Define variable dvEdge
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'dvEdge'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nEdges'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'dvEdge', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'dvEdge', r1Ptr)

! Define variable dcEdge
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'dcEdge'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nEdges'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'dcEdge', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'dcEdge', r1Ptr)

! Define variable angleEdge
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'angleEdge'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nEdges'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'angleEdge', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'angleEdge', r1Ptr)

! Define variable areaCell
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'areaCell'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'areaCell', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'areaCell', r1Ptr)

! Define variable areaTriangle
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'areaTriangle'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nVertices'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'areaTriangle', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'areaTriangle', r1Ptr)

! Define variable edgeNormalVectors
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'edgeNormalVectors'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .false.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'R3'
      r2Ptr(1) % dimNames(2) = 'nEdges'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'edgeNormalVectors', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'edgeNormalVectors', r2Ptr)

! Define variable localVerticalUnitVectors
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'localVerticalUnitVectors'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .false.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'R3'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'localVerticalUnitVectors', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'localVerticalUnitVectors', r2Ptr)

! Define variable cellTangentPlane
      allocate(r3Ptr(1))

! Setting up time level 1
      r3Ptr(1) % fieldName = 'cellTangentPlane'
      r3Ptr(1) % isVarArray = .false.
      r3Ptr(1) % isDecomposed = .true.
      r3Ptr(1) % hasTimeDimension = .false.
      r3Ptr(1) % isPersistent = .true.
      r3Ptr(1) % isActive = .false.
! Setting up dimensions
      r3Ptr(1) % dimNames(1) = 'R3'
      r3Ptr(1) % dimNames(2) = 'TWO'
      r3Ptr(1) % dimNames(3) = 'nCells'
     r3Ptr(1) % defaultValue = 0.0
     r3Ptr(1) % defaultValue = 0.0
     nullify(r3Ptr(1) % array)
      nullify(r3Ptr(1) % next)
      nullify(r3Ptr(1) % prev)
      nullify(r3Ptr(1) % sendList)
      nullify(r3Ptr(1) % recvList)
      nullify(r3Ptr(1) % copyList)
      allocate(r3Ptr(1) % attLists(1))
      allocate(r3Ptr(1) % attLists(1) % attList)
      r3Ptr(1) % attLists(1) % attList % attName = ''
      r3Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r3Ptr(1) % attLists(1) % attList % next)
      nullify(r3Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r3Ptr(1) % attLists(1) % attList % attValueRealA)
      r3Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r3Ptr(1) % block => block

      r3Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'cellTangentPlane', r3Ptr)
      call mpas_pool_add_field(block % allFields, 'cellTangentPlane', r3Ptr)

! Define variable cellsOnCell
      allocate(i2Ptr(1))

! Setting up time level 1
      i2Ptr(1) % fieldName = 'cellsOnCell'
      i2Ptr(1) % isVarArray = .false.
      i2Ptr(1) % isDecomposed = .true.
      i2Ptr(1) % hasTimeDimension = .false.
      i2Ptr(1) % isPersistent = .true.
      i2Ptr(1) % isActive = .false.
! Setting up dimensions
      i2Ptr(1) % dimNames(1) = 'maxEdges'
      i2Ptr(1) % dimNames(2) = 'nCells'
     i2Ptr(1) % defaultValue = 0
     i2Ptr(1) % defaultValue = 0
     nullify(i2Ptr(1) % array)
      nullify(i2Ptr(1) % next)
      nullify(i2Ptr(1) % prev)
      nullify(i2Ptr(1) % sendList)
      nullify(i2Ptr(1) % recvList)
      nullify(i2Ptr(1) % copyList)
      allocate(i2Ptr(1) % attLists(1))
      allocate(i2Ptr(1) % attLists(1) % attList)
      i2Ptr(1) % attLists(1) % attList % attName = ''
      i2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i2Ptr(1) % attLists(1) % attList % next)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueRealA)
      i2Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i2Ptr(1) % block => block

      i2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'cellsOnCell', i2Ptr)
      call mpas_pool_add_field(block % allFields, 'cellsOnCell', i2Ptr)

! Define variable verticesOnCell
      allocate(i2Ptr(1))

! Setting up time level 1
      i2Ptr(1) % fieldName = 'verticesOnCell'
      i2Ptr(1) % isVarArray = .false.
      i2Ptr(1) % isDecomposed = .true.
      i2Ptr(1) % hasTimeDimension = .false.
      i2Ptr(1) % isPersistent = .true.
      i2Ptr(1) % isActive = .false.
! Setting up dimensions
      i2Ptr(1) % dimNames(1) = 'maxEdges'
      i2Ptr(1) % dimNames(2) = 'nCells'
     i2Ptr(1) % defaultValue = 0
     i2Ptr(1) % defaultValue = 0
     nullify(i2Ptr(1) % array)
      nullify(i2Ptr(1) % next)
      nullify(i2Ptr(1) % prev)
      nullify(i2Ptr(1) % sendList)
      nullify(i2Ptr(1) % recvList)
      nullify(i2Ptr(1) % copyList)
      allocate(i2Ptr(1) % attLists(1))
      allocate(i2Ptr(1) % attLists(1) % attList)
      i2Ptr(1) % attLists(1) % attList % attName = ''
      i2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i2Ptr(1) % attLists(1) % attList % next)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueRealA)
      i2Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i2Ptr(1) % block => block

      i2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'verticesOnCell', i2Ptr)
      call mpas_pool_add_field(block % allFields, 'verticesOnCell', i2Ptr)

! Define variable verticesOnEdge
      allocate(i2Ptr(1))

! Setting up time level 1
      i2Ptr(1) % fieldName = 'verticesOnEdge'
      i2Ptr(1) % isVarArray = .false.
      i2Ptr(1) % isDecomposed = .true.
      i2Ptr(1) % hasTimeDimension = .false.
      i2Ptr(1) % isPersistent = .true.
      i2Ptr(1) % isActive = .false.
! Setting up dimensions
      i2Ptr(1) % dimNames(1) = 'TWO'
      i2Ptr(1) % dimNames(2) = 'nEdges'
     i2Ptr(1) % defaultValue = 0
     i2Ptr(1) % defaultValue = 0
     nullify(i2Ptr(1) % array)
      nullify(i2Ptr(1) % next)
      nullify(i2Ptr(1) % prev)
      nullify(i2Ptr(1) % sendList)
      nullify(i2Ptr(1) % recvList)
      nullify(i2Ptr(1) % copyList)
      allocate(i2Ptr(1) % attLists(1))
      allocate(i2Ptr(1) % attLists(1) % attList)
      i2Ptr(1) % attLists(1) % attList % attName = ''
      i2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i2Ptr(1) % attLists(1) % attList % next)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueRealA)
      i2Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i2Ptr(1) % block => block

      i2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'verticesOnEdge', i2Ptr)
      call mpas_pool_add_field(block % allFields, 'verticesOnEdge', i2Ptr)

! Define variable edgesOnVertex
      allocate(i2Ptr(1))

! Setting up time level 1
      i2Ptr(1) % fieldName = 'edgesOnVertex'
      i2Ptr(1) % isVarArray = .false.
      i2Ptr(1) % isDecomposed = .true.
      i2Ptr(1) % hasTimeDimension = .false.
      i2Ptr(1) % isPersistent = .true.
      i2Ptr(1) % isActive = .false.
! Setting up dimensions
      i2Ptr(1) % dimNames(1) = 'vertexDegree'
      i2Ptr(1) % dimNames(2) = 'nVertices'
     i2Ptr(1) % defaultValue = 0
     i2Ptr(1) % defaultValue = 0
     nullify(i2Ptr(1) % array)
      nullify(i2Ptr(1) % next)
      nullify(i2Ptr(1) % prev)
      nullify(i2Ptr(1) % sendList)
      nullify(i2Ptr(1) % recvList)
      nullify(i2Ptr(1) % copyList)
      allocate(i2Ptr(1) % attLists(1))
      allocate(i2Ptr(1) % attLists(1) % attList)
      i2Ptr(1) % attLists(1) % attList % attName = ''
      i2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i2Ptr(1) % attLists(1) % attList % next)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueRealA)
      i2Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i2Ptr(1) % block => block

      i2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'edgesOnVertex', i2Ptr)
      call mpas_pool_add_field(block % allFields, 'edgesOnVertex', i2Ptr)

! Define variable cellsOnVertex
      allocate(i2Ptr(1))

! Setting up time level 1
      i2Ptr(1) % fieldName = 'cellsOnVertex'
      i2Ptr(1) % isVarArray = .false.
      i2Ptr(1) % isDecomposed = .true.
      i2Ptr(1) % hasTimeDimension = .false.
      i2Ptr(1) % isPersistent = .true.
      i2Ptr(1) % isActive = .false.
! Setting up dimensions
      i2Ptr(1) % dimNames(1) = 'vertexDegree'
      i2Ptr(1) % dimNames(2) = 'nVertices'
     i2Ptr(1) % defaultValue = 0
     i2Ptr(1) % defaultValue = 0
     nullify(i2Ptr(1) % array)
      nullify(i2Ptr(1) % next)
      nullify(i2Ptr(1) % prev)
      nullify(i2Ptr(1) % sendList)
      nullify(i2Ptr(1) % recvList)
      nullify(i2Ptr(1) % copyList)
      allocate(i2Ptr(1) % attLists(1))
      allocate(i2Ptr(1) % attLists(1) % attList)
      i2Ptr(1) % attLists(1) % attList % attName = ''
      i2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i2Ptr(1) % attLists(1) % attList % next)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueRealA)
      i2Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i2Ptr(1) % block => block

      i2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'cellsOnVertex', i2Ptr)
      call mpas_pool_add_field(block % allFields, 'cellsOnVertex', i2Ptr)

! Define variable kiteAreasOnVertex
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'kiteAreasOnVertex'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .false.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'vertexDegree'
      r2Ptr(1) % dimNames(2) = 'nVertices'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'kiteAreasOnVertex', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'kiteAreasOnVertex', r2Ptr)

! Define variable fEdge
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'fEdge'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nEdges'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'fEdge', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'fEdge', r1Ptr)

! Define variable fVertex
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'fVertex'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nVertices'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'fVertex', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'fVertex', r1Ptr)

! Define variable meshDensity
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'meshDensity'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'meshDensity', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'meshDensity', r1Ptr)

! Define variable cf1
      allocate(r0Ptr(1))

! Setting up time level 1
      r0Ptr(1) % fieldName = 'cf1'
      r0Ptr(1) % isVarArray = .false.
      r0Ptr(1) % isDecomposed = .false.
      r0Ptr(1) % hasTimeDimension = .false.
     r0Ptr(1) % defaultValue = 0.0
     r0Ptr(1) % defaultValue = 0.0
     r0Ptr(1) % scalar = 0.0
      nullify(r0Ptr(1) % next)
      nullify(r0Ptr(1) % prev)
      nullify(r0Ptr(1) % sendList)
      nullify(r0Ptr(1) % recvList)
      nullify(r0Ptr(1) % copyList)
      allocate(r0Ptr(1) % attLists(1))
      allocate(r0Ptr(1) % attLists(1) % attList)
      r0Ptr(1) % attLists(1) % attList % attName = ''
      r0Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r0Ptr(1) % attLists(1) % attList % next)
      nullify(r0Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r0Ptr(1) % attLists(1) % attList % attValueRealA)
      r0Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r0Ptr(1) % block => block

      r0Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'cf1', r0Ptr)
      call mpas_pool_add_field(block % allFields, 'cf1', r0Ptr)

! Define variable cf2
      allocate(r0Ptr(1))

! Setting up time level 1
      r0Ptr(1) % fieldName = 'cf2'
      r0Ptr(1) % isVarArray = .false.
      r0Ptr(1) % isDecomposed = .false.
      r0Ptr(1) % hasTimeDimension = .false.
     r0Ptr(1) % defaultValue = 0.0
     r0Ptr(1) % defaultValue = 0.0
     r0Ptr(1) % scalar = 0.0
      nullify(r0Ptr(1) % next)
      nullify(r0Ptr(1) % prev)
      nullify(r0Ptr(1) % sendList)
      nullify(r0Ptr(1) % recvList)
      nullify(r0Ptr(1) % copyList)
      allocate(r0Ptr(1) % attLists(1))
      allocate(r0Ptr(1) % attLists(1) % attList)
      r0Ptr(1) % attLists(1) % attList % attName = ''
      r0Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r0Ptr(1) % attLists(1) % attList % next)
      nullify(r0Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r0Ptr(1) % attLists(1) % attList % attValueRealA)
      r0Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r0Ptr(1) % block => block

      r0Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'cf2', r0Ptr)
      call mpas_pool_add_field(block % allFields, 'cf2', r0Ptr)

! Define variable cf3
      allocate(r0Ptr(1))

! Setting up time level 1
      r0Ptr(1) % fieldName = 'cf3'
      r0Ptr(1) % isVarArray = .false.
      r0Ptr(1) % isDecomposed = .false.
      r0Ptr(1) % hasTimeDimension = .false.
     r0Ptr(1) % defaultValue = 0.0
     r0Ptr(1) % defaultValue = 0.0
     r0Ptr(1) % scalar = 0.0
      nullify(r0Ptr(1) % next)
      nullify(r0Ptr(1) % prev)
      nullify(r0Ptr(1) % sendList)
      nullify(r0Ptr(1) % recvList)
      nullify(r0Ptr(1) % copyList)
      allocate(r0Ptr(1) % attLists(1))
      allocate(r0Ptr(1) % attLists(1) % attList)
      r0Ptr(1) % attLists(1) % attList % attName = ''
      r0Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r0Ptr(1) % attLists(1) % attList % next)
      nullify(r0Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r0Ptr(1) % attLists(1) % attList % attValueRealA)
      r0Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r0Ptr(1) % block => block

      r0Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'cf3', r0Ptr)
      call mpas_pool_add_field(block % allFields, 'cf3', r0Ptr)

! Define variable ter
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'ter'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'ter', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'ter', r1Ptr)

! Define variable landmask
      allocate(i1Ptr(1))

! Setting up time level 1
      i1Ptr(1) % fieldName = 'landmask'
      i1Ptr(1) % isVarArray = .false.
      i1Ptr(1) % isDecomposed = .true.
      i1Ptr(1) % hasTimeDimension = .false.
      i1Ptr(1) % isPersistent = .true.
      i1Ptr(1) % isActive = .false.
! Setting up dimensions
      i1Ptr(1) % dimNames(1) = 'nCells'
     i1Ptr(1) % defaultValue = 0
     i1Ptr(1) % defaultValue = 0
     nullify(i1Ptr(1) % array)
      nullify(i1Ptr(1) % next)
      nullify(i1Ptr(1) % prev)
      nullify(i1Ptr(1) % sendList)
      nullify(i1Ptr(1) % recvList)
      nullify(i1Ptr(1) % copyList)
      allocate(i1Ptr(1) % attLists(1))
      allocate(i1Ptr(1) % attLists(1) % attList)
      i1Ptr(1) % attLists(1) % attList % attName = ''
      i1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i1Ptr(1) % attLists(1) % attList % next)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueRealA)
      i1Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i1Ptr(1) % block => block

      i1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'landmask', i1Ptr)
      call mpas_pool_add_field(block % allFields, 'landmask', i1Ptr)

! Define variable ivgtyp
      allocate(i1Ptr(1))

! Setting up time level 1
      i1Ptr(1) % fieldName = 'ivgtyp'
      i1Ptr(1) % isVarArray = .false.
      i1Ptr(1) % isDecomposed = .true.
      i1Ptr(1) % hasTimeDimension = .false.
      i1Ptr(1) % isPersistent = .true.
      i1Ptr(1) % isActive = .false.
! Setting up dimensions
      i1Ptr(1) % dimNames(1) = 'nCells'
     i1Ptr(1) % defaultValue = 0
     i1Ptr(1) % defaultValue = 0
     nullify(i1Ptr(1) % array)
      nullify(i1Ptr(1) % next)
      nullify(i1Ptr(1) % prev)
      nullify(i1Ptr(1) % sendList)
      nullify(i1Ptr(1) % recvList)
      nullify(i1Ptr(1) % copyList)
      allocate(i1Ptr(1) % attLists(1))
      allocate(i1Ptr(1) % attLists(1) % attList)
      i1Ptr(1) % attLists(1) % attList % attName = ''
      i1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i1Ptr(1) % attLists(1) % attList % next)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueRealA)
      i1Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i1Ptr(1) % block => block

      i1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'lu_index', i1Ptr)
      call mpas_pool_add_field(block % allFields, 'ivgtyp', i1Ptr)

! Define variable mminlu
      allocate(c0Ptr(1))

! Setting up time level 1
      c0Ptr(1) % fieldName = 'mminlu'
      c0Ptr(1) % isVarArray = .false.
      c0Ptr(1) % isDecomposed = .false.
      c0Ptr(1) % hasTimeDimension = .false.
     c0Ptr(1) % defaultValue = ''
     c0Ptr(1) % defaultValue = ''
     c0Ptr(1) % scalar = ''
      nullify(c0Ptr(1) % next)
      nullify(c0Ptr(1) % prev)
      nullify(c0Ptr(1) % sendList)
      nullify(c0Ptr(1) % recvList)
      nullify(c0Ptr(1) % copyList)
      allocate(c0Ptr(1) % attLists(1))
      allocate(c0Ptr(1) % attLists(1) % attList)
      c0Ptr(1) % attLists(1) % attList % attName = ''
      c0Ptr(1) % attLists(1) % attList % attType = -1
      nullify(c0Ptr(1) % attLists(1) % attList % next)
      nullify(c0Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(c0Ptr(1) % attLists(1) % attList % attValueRealA)
      c0Ptr(1) % missingValue = MPAS_CHAR_FILLVAL
      c0Ptr(1) % block => block

      c0Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'mminlu', c0Ptr)
      call mpas_pool_add_field(block % allFields, 'mminlu', c0Ptr)

! Define variable isltyp
      allocate(i1Ptr(1))

! Setting up time level 1
      i1Ptr(1) % fieldName = 'isltyp'
      i1Ptr(1) % isVarArray = .false.
      i1Ptr(1) % isDecomposed = .true.
      i1Ptr(1) % hasTimeDimension = .false.
      i1Ptr(1) % isPersistent = .true.
      i1Ptr(1) % isActive = .false.
! Setting up dimensions
      i1Ptr(1) % dimNames(1) = 'nCells'
     i1Ptr(1) % defaultValue = 0
     i1Ptr(1) % defaultValue = 0
     nullify(i1Ptr(1) % array)
      nullify(i1Ptr(1) % next)
      nullify(i1Ptr(1) % prev)
      nullify(i1Ptr(1) % sendList)
      nullify(i1Ptr(1) % recvList)
      nullify(i1Ptr(1) % copyList)
      allocate(i1Ptr(1) % attLists(1))
      allocate(i1Ptr(1) % attLists(1) % attList)
      i1Ptr(1) % attLists(1) % attList % attName = ''
      i1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i1Ptr(1) % attLists(1) % attList % next)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueRealA)
      i1Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i1Ptr(1) % block => block

      i1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'soilcat_top', i1Ptr)
      call mpas_pool_add_field(block % allFields, 'isltyp', i1Ptr)

! Define variable soilcat_bot
      allocate(i1Ptr(1))

! Setting up time level 1
      i1Ptr(1) % fieldName = 'soilcat_bot'
      i1Ptr(1) % isVarArray = .false.
      i1Ptr(1) % isDecomposed = .true.
      i1Ptr(1) % hasTimeDimension = .false.
      i1Ptr(1) % isPersistent = .true.
      i1Ptr(1) % isActive = .false.
! Setting up dimensions
      i1Ptr(1) % dimNames(1) = 'nCells'
     i1Ptr(1) % defaultValue = 0
     i1Ptr(1) % defaultValue = 0
     nullify(i1Ptr(1) % array)
      nullify(i1Ptr(1) % next)
      nullify(i1Ptr(1) % prev)
      nullify(i1Ptr(1) % sendList)
      nullify(i1Ptr(1) % recvList)
      nullify(i1Ptr(1) % copyList)
      allocate(i1Ptr(1) % attLists(1))
      allocate(i1Ptr(1) % attLists(1) % attList)
      i1Ptr(1) % attLists(1) % attList % attName = ''
      i1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i1Ptr(1) % attLists(1) % attList % next)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i1Ptr(1) % attLists(1) % attList % attValueRealA)
      i1Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i1Ptr(1) % block => block

      i1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'soilcat_bot', i1Ptr)
      call mpas_pool_add_field(block % allFields, 'soilcat_bot', i1Ptr)

! Define variable snoalb
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'snoalb'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'snoalb', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'snoalb', r1Ptr)

! Define variable soiltemp
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'soiltemp'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'soiltemp', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'soiltemp', r1Ptr)

! Define variable greenfrac
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'greenfrac'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .false.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nMonths'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'greenfrac', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'greenfrac', r2Ptr)

! Define variable shdmin
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'shdmin'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'shdmin', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'shdmin', r1Ptr)

! Define variable shdmax
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'shdmax'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'shdmax', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'shdmax', r1Ptr)

! Define variable albedo12m
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'albedo12m'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .false.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nMonths'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'albedo12m', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'albedo12m', r2Ptr)

! Define variable varsso
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'varsso'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'varsso', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'varsso', r1Ptr)

! Define variable var2d
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'var2d'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'var2d', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'var2d', r1Ptr)

! Define variable con
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'con'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'con', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'con', r1Ptr)

! Define variable oa1
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'oa1'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'oa1', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'oa1', r1Ptr)

! Define variable oa2
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'oa2'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'oa2', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'oa2', r1Ptr)

! Define variable oa3
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'oa3'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'oa3', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'oa3', r1Ptr)

! Define variable oa4
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'oa4'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'oa4', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'oa4', r1Ptr)

! Define variable ol1
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'ol1'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'ol1', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'ol1', r1Ptr)

! Define variable ol2
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'ol2'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'ol2', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'ol2', r1Ptr)

! Define variable ol3
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'ol3'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'ol3', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'ol3', r1Ptr)

! Define variable ol4
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'ol4'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'ol4', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'ol4', r1Ptr)

! Define variable hx
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'hx'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .false.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevelsP1'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'hx', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'hx', r2Ptr)

! Define variable zgrid
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'zgrid'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .false.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevelsP1'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'zgrid', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'zgrid', r2Ptr)

! Define variable rdzw
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'rdzw'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .false.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nVertLevels'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'rdzw', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'rdzw', r1Ptr)

! Define variable dzu
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'dzu'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .false.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nVertLevels'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'dzu', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'dzu', r1Ptr)

! Define variable rdzu
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'rdzu'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .false.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nVertLevels'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'rdzu', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'rdzu', r1Ptr)

! Define variable fzm
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'fzm'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .false.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nVertLevels'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'fzm', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'fzm', r1Ptr)

! Define variable fzp
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'fzp'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .false.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nVertLevels'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'fzp', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'fzp', r1Ptr)

! Define variable zxu
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'zxu'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .false.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nEdges'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'zxu', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'zxu', r2Ptr)

! Define variable zz
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'zz'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .false.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'zz', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'zz', r2Ptr)

! Define variable zb
      allocate(r3Ptr(1))

! Setting up time level 1
      r3Ptr(1) % fieldName = 'zb'
      r3Ptr(1) % isVarArray = .false.
      r3Ptr(1) % isDecomposed = .true.
      r3Ptr(1) % hasTimeDimension = .false.
      r3Ptr(1) % isPersistent = .true.
      r3Ptr(1) % isActive = .false.
! Setting up dimensions
      r3Ptr(1) % dimNames(1) = 'nVertLevelsP1'
      r3Ptr(1) % dimNames(2) = 'TWO'
      r3Ptr(1) % dimNames(3) = 'nEdges'
     r3Ptr(1) % defaultValue = 0.0
     r3Ptr(1) % defaultValue = 0.0
     nullify(r3Ptr(1) % array)
      nullify(r3Ptr(1) % next)
      nullify(r3Ptr(1) % prev)
      nullify(r3Ptr(1) % sendList)
      nullify(r3Ptr(1) % recvList)
      nullify(r3Ptr(1) % copyList)
      allocate(r3Ptr(1) % attLists(1))
      allocate(r3Ptr(1) % attLists(1) % attList)
      r3Ptr(1) % attLists(1) % attList % attName = ''
      r3Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r3Ptr(1) % attLists(1) % attList % next)
      nullify(r3Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r3Ptr(1) % attLists(1) % attList % attValueRealA)
      r3Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r3Ptr(1) % block => block

      r3Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'zb', r3Ptr)
      call mpas_pool_add_field(block % allFields, 'zb', r3Ptr)

! Define variable zb3
      allocate(r3Ptr(1))

! Setting up time level 1
      r3Ptr(1) % fieldName = 'zb3'
      r3Ptr(1) % isVarArray = .false.
      r3Ptr(1) % isDecomposed = .true.
      r3Ptr(1) % hasTimeDimension = .false.
      r3Ptr(1) % isPersistent = .true.
      r3Ptr(1) % isActive = .false.
! Setting up dimensions
      r3Ptr(1) % dimNames(1) = 'nVertLevelsP1'
      r3Ptr(1) % dimNames(2) = 'TWO'
      r3Ptr(1) % dimNames(3) = 'nEdges'
     r3Ptr(1) % defaultValue = 0.0
     r3Ptr(1) % defaultValue = 0.0
     nullify(r3Ptr(1) % array)
      nullify(r3Ptr(1) % next)
      nullify(r3Ptr(1) % prev)
      nullify(r3Ptr(1) % sendList)
      nullify(r3Ptr(1) % recvList)
      nullify(r3Ptr(1) % copyList)
      allocate(r3Ptr(1) % attLists(1))
      allocate(r3Ptr(1) % attLists(1) % attList)
      r3Ptr(1) % attLists(1) % attList % attName = ''
      r3Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r3Ptr(1) % attLists(1) % attList % next)
      nullify(r3Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r3Ptr(1) % attLists(1) % attList % attValueRealA)
      r3Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r3Ptr(1) % block => block

      r3Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'zb3', r3Ptr)
      call mpas_pool_add_field(block % allFields, 'zb3', r3Ptr)

! Define variable dss
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'dss'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .false.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'dss', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'dss', r2Ptr)

! Define variable u_init
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'u_init'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .false.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nVertLevels'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'u_init', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'u_init', r1Ptr)

! Define variable t_init
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 't_init'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .false.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 't_init', r2Ptr)
      call mpas_pool_add_field(block % allFields, 't_init', r2Ptr)

! Define variable qv_init
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'qv_init'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .false.
      r1Ptr(1) % hasTimeDimension = .false.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nVertLevels'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'qv_init', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'qv_init', r1Ptr)

! Define variable deriv_two
      allocate(r3Ptr(1))

! Setting up time level 1
      r3Ptr(1) % fieldName = 'deriv_two'
      r3Ptr(1) % isVarArray = .false.
      r3Ptr(1) % isDecomposed = .true.
      r3Ptr(1) % hasTimeDimension = .false.
      r3Ptr(1) % isPersistent = .true.
      r3Ptr(1) % isActive = .false.
! Setting up dimensions
      r3Ptr(1) % dimNames(1) = 'FIFTEEN'
      r3Ptr(1) % dimNames(2) = 'TWO'
      r3Ptr(1) % dimNames(3) = 'nEdges'
     r3Ptr(1) % defaultValue = 0.0
     r3Ptr(1) % defaultValue = 0.0
     nullify(r3Ptr(1) % array)
      nullify(r3Ptr(1) % next)
      nullify(r3Ptr(1) % prev)
      nullify(r3Ptr(1) % sendList)
      nullify(r3Ptr(1) % recvList)
      nullify(r3Ptr(1) % copyList)
      allocate(r3Ptr(1) % attLists(1))
      allocate(r3Ptr(1) % attLists(1) % attList)
      r3Ptr(1) % attLists(1) % attList % attName = ''
      r3Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r3Ptr(1) % attLists(1) % attList % next)
      nullify(r3Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r3Ptr(1) % attLists(1) % attList % attValueRealA)
      r3Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r3Ptr(1) % block => block

      r3Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'deriv_two', r3Ptr)
      call mpas_pool_add_field(block % allFields, 'deriv_two', r3Ptr)

! Define variable advCells
      allocate(i2Ptr(1))

! Setting up time level 1
      i2Ptr(1) % fieldName = 'advCells'
      i2Ptr(1) % isVarArray = .false.
      i2Ptr(1) % isDecomposed = .true.
      i2Ptr(1) % hasTimeDimension = .false.
      i2Ptr(1) % isPersistent = .true.
      i2Ptr(1) % isActive = .false.
! Setting up dimensions
      i2Ptr(1) % dimNames(1) = 'TWENTYONE'
      i2Ptr(1) % dimNames(2) = 'nCells'
     i2Ptr(1) % defaultValue = 0
     i2Ptr(1) % defaultValue = 0
     nullify(i2Ptr(1) % array)
      nullify(i2Ptr(1) % next)
      nullify(i2Ptr(1) % prev)
      nullify(i2Ptr(1) % sendList)
      nullify(i2Ptr(1) % recvList)
      nullify(i2Ptr(1) % copyList)
      allocate(i2Ptr(1) % attLists(1))
      allocate(i2Ptr(1) % attLists(1) % attList)
      i2Ptr(1) % attLists(1) % attList % attName = ''
      i2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(i2Ptr(1) % attLists(1) % attList % next)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(i2Ptr(1) % attLists(1) % attList % attValueRealA)
      i2Ptr(1) % missingValue = MPAS_INT_FILLVAL
      i2Ptr(1) % block => block

      i2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'advCells', i2Ptr)
      call mpas_pool_add_field(block % allFields, 'advCells', i2Ptr)

! Define variable defc_a
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'defc_a'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .false.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'maxEdges'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'defc_a', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'defc_a', r2Ptr)

! Define variable defc_b
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'defc_b'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .false.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'maxEdges'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'defc_b', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'defc_b', r2Ptr)

! Define variable coeffs_reconstruct
      allocate(r3Ptr(1))

! Setting up time level 1
      r3Ptr(1) % fieldName = 'coeffs_reconstruct'
      r3Ptr(1) % isVarArray = .false.
      r3Ptr(1) % isDecomposed = .true.
      r3Ptr(1) % hasTimeDimension = .false.
      r3Ptr(1) % isPersistent = .true.
      r3Ptr(1) % isActive = .false.
! Setting up dimensions
      r3Ptr(1) % dimNames(1) = 'R3'
      r3Ptr(1) % dimNames(2) = 'maxEdges'
      r3Ptr(1) % dimNames(3) = 'nCells'
     r3Ptr(1) % defaultValue = 0.0
     r3Ptr(1) % defaultValue = 0.0
     nullify(r3Ptr(1) % array)
      nullify(r3Ptr(1) % next)
      nullify(r3Ptr(1) % prev)
      nullify(r3Ptr(1) % sendList)
      nullify(r3Ptr(1) % recvList)
      nullify(r3Ptr(1) % copyList)
      allocate(r3Ptr(1) % attLists(1))
      allocate(r3Ptr(1) % attLists(1) % attList)
      r3Ptr(1) % attLists(1) % attList % attName = ''
      r3Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r3Ptr(1) % attLists(1) % attList % next)
      nullify(r3Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r3Ptr(1) % attLists(1) % attList % attValueRealA)
      r3Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r3Ptr(1) % block => block

      r3Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'coeffs_reconstruct', r3Ptr)
      call mpas_pool_add_field(block % allFields, 'coeffs_reconstruct', r3Ptr)



      if (associated(newSubPool)) then
         call mpas_pool_add_config(newSubPool, 'on_a_sphere', block % domain % on_a_sphere)
         call mpas_pool_add_config(newSubPool, 'sphere_radius', block % domain % sphere_radius)
         call mpas_pool_add_config(newSubPool, 'is_periodic', block % domain % is_periodic)
         call mpas_pool_add_config(newSubPool, 'x_period', block % domain % x_period)
         call mpas_pool_add_config(newSubPool, 'y_period', block % domain % y_period)
      end if

   end subroutine init_atm_generate_pool_mesh


   subroutine init_atm_generate_pool_state(block, structPool, dimensionPool, packagePool)
      use mpas_derived_types
      use mpas_pool_routines
      use mpas_io_units
      use mpas_io, only : MPAS_REAL_FILLVAL, MPAS_INT_FILLVAL, MPAS_CHAR_FILLVAL
      implicit none
      type (block_type), intent(inout), pointer :: block
      type (mpas_pool_type), intent(inout) :: structPool
      type (mpas_pool_type), intent(inout) :: dimensionPool
      type (mpas_pool_type), intent(in) :: packagePool

      type (field0DReal), dimension(:), pointer :: r0Ptr
      type (field1DReal), dimension(:), pointer :: r1Ptr
      type (field2DReal), dimension(:), pointer :: r2Ptr
      type (field3DReal), dimension(:), pointer :: r3Ptr
      type (field4DReal), dimension(:), pointer :: r4Ptr
      type (field5DReal), dimension(:), pointer :: r5Ptr
      type (field0DInteger), dimension(:), pointer :: i0Ptr
      type (field1DInteger), dimension(:), pointer :: i1Ptr
      type (field2DInteger), dimension(:), pointer :: i2Ptr
      type (field3DInteger), dimension(:), pointer :: i3Ptr
      type (field0DChar), dimension(:), pointer :: c0Ptr
      type (field1DChar), dimension(:), pointer :: c1Ptr

      type (mpas_pool_type), pointer :: newSubPool
      integer :: group_counter
      logical :: group_started
      integer :: group_start
      integer :: index_counter
      integer, pointer :: const_index

      logical, pointer :: initial_condsActive
      logical, pointer :: sfc_updateActive
      logical, pointer :: gwd_stage_inActive
      logical, pointer :: vertical_stage_inActive
      logical, pointer :: vertical_stage_outActive
      logical, pointer :: met_stage_inActive
      logical, pointer :: met_stage_outActive


      integer :: numConstituents

      nullify(newSubPool)
      group_counter = -1
      group_started = .false.
      group_start = -1
      call mpas_pool_get_package(packagePool, 'initial_condsActive', initial_condsActive)
      call mpas_pool_get_package(packagePool, 'sfc_updateActive', sfc_updateActive)
      call mpas_pool_get_package(packagePool, 'gwd_stage_inActive', gwd_stage_inActive)
      call mpas_pool_get_package(packagePool, 'vertical_stage_inActive', vertical_stage_inActive)
      call mpas_pool_get_package(packagePool, 'vertical_stage_outActive', vertical_stage_outActive)
      call mpas_pool_get_package(packagePool, 'met_stage_inActive', met_stage_inActive)
      call mpas_pool_get_package(packagePool, 'met_stage_outActive', met_stage_outActive)

      allocate(newSubPool)
      call mpas_pool_create_pool(newSubPool)
      call mpas_pool_add_subpool(structPool, 'state', newSubPool)
      call mpas_pool_add_subpool(block % allStructs, 'state', newSubPool)

! Define var array scalars
      allocate(r3Ptr(1))
      index_counter = 0
      group_counter = -1
      group_start = -1
      group_started = .false.

! Starting group moist
! Define constituent var qv
! My Packages are (null)
      index_counter = index_counter + 1
      if (associated(newSubPool)) then
         call mpas_pool_add_dimension(newSubPool, 'index_qv', index_counter)
      end if
      group_counter = group_counter + 1
      if (.not. group_started) then
         group_start = index_counter
         if (associated(newSubPool)) then
            call mpas_pool_add_dimension(newSubPool, 'moist_start', group_start)
         end if
         group_started = .true.
      end if
! Define constituent var qc
! My packages are (null)
      index_counter = index_counter + 1
      if (associated(newSubPool)) then
         call mpas_pool_add_dimension(newSubPool, 'index_qc', index_counter)
      end if
      group_counter = group_counter + 1
      if (.not. group_started) then
         group_start = index_counter
         if (associated(newSubPool)) then
            call mpas_pool_add_dimension(newSubPool, 'moist_start', group_start)
         end if
         group_started = .true.
      end if
! Define constituent var qr
! My packages are (null)
      index_counter = index_counter + 1
      if (associated(newSubPool)) then
         call mpas_pool_add_dimension(newSubPool, 'index_qr', index_counter)
      end if
      group_counter = group_counter + 1
      if (.not. group_started) then
         group_start = index_counter
         if (associated(newSubPool)) then
            call mpas_pool_add_dimension(newSubPool, 'moist_start', group_start)
         end if
         group_started = .true.
      end if
      if (.not. group_started) then
         if (associated(newSubPool)) then
            call mpas_pool_add_dimension(newSubPool, 'moist_start', -1)
            call mpas_pool_add_dimension(newSubPool, 'moist_end', -1)
         end if
      else
         group_started = .false.
         if (associated(newSubPool)) then
            call mpas_pool_add_dimension(newSubPool, 'moist_end', index_counter)
         end if
      end if
! End of group       

      numConstituents = index_counter
      if (associated(newSubPool)) then
         call mpas_pool_add_dimension(newSubPool, 'num_scalars', numConstituents)
      end if
! Defining time level 1
      allocate( r3Ptr(1) % constituentNames(numConstituents) )
      r3Ptr(1) % fieldName = 'scalars'
      r3Ptr(1) % isDecomposed = .true.
      r3Ptr(1) % hasTimeDimension = .true.
      r3Ptr(1) % isVarArray = .true.
      r3Ptr(1) % isPersistent = .true.
      r3Ptr(1) % isActive = .false.

      if (associated(newSubPool)) then
         call mpas_pool_get_dimension(newSubPool, 'index_qv', const_index)
      end if
      if (const_index > 0) then
         r3Ptr(1) % constituentNames(const_index) = 'qv'
      end if
      if (associated(newSubPool)) then
         call mpas_pool_get_dimension(newSubPool, 'index_qc', const_index)
      end if
      if (const_index > 0) then
         r3Ptr(1) % constituentNames(const_index) = 'qc'
      end if
      if (associated(newSubPool)) then
         call mpas_pool_get_dimension(newSubPool, 'index_qr', const_index)
      end if
      if (const_index > 0) then
         r3Ptr(1) % constituentNames(const_index) = 'qr'
      end if

! Setup dimensions for       
      r3Ptr(1) % dimNames(1) = 'num_scalars'
      r3Ptr(1) % dimNames(2) = 'nVertLevels'
      r3Ptr(1) % dimNames(3) = 'nCells'

      nullify(r3Ptr(1) % array)
      r3Ptr(1) % defaultValue = 0.0
      nullify(r3Ptr(1) % next)
      nullify(r3Ptr(1) % prev)
      nullify(r3Ptr(1) % sendList)
      nullify(r3Ptr(1) % recvList)
      nullify(r3Ptr(1) % copyList)
      allocate(r3Ptr(1) % attLists(size(r3Ptr(1) % constituentNames, dim=1)))
      do index_counter = 1, size(r3Ptr(1) % constituentNames, dim=1)
         allocate(r3Ptr(1) % attLists(index_counter) % attList)
         r3Ptr(1) % attLists(index_counter) % attList % attName = ''
         r3Ptr(1) % attLists(index_counter) % attList % attType = -1
         nullify(r3Ptr(1) % attLists(index_counter) % attList % next)
         nullify(r3Ptr(1) % attLists(index_counter) % attList % attValueIntA)
         nullify(r3Ptr(1) % attLists(index_counter) % attList % attValueRealA)
      end do
      if (associated(newSubPool)) then
         call mpas_pool_get_dimension(newSubPool, 'index_qv', const_index)
      end if
      if (const_index > 0) then
         r3Ptr(1) % missingValue = MPAS_REAL_FILLVAL
         r3Ptr(1) % constituentNames(const_index) = 'qv'
      end if
      if (associated(newSubPool)) then
         call mpas_pool_get_dimension(newSubPool, 'index_qc', const_index)
      end if
      if (const_index > 0) then
         r3Ptr(1) % missingValue = MPAS_REAL_FILLVAL
         r3Ptr(1) % constituentNames(const_index) = 'qc'
      end if
      if (associated(newSubPool)) then
         call mpas_pool_get_dimension(newSubPool, 'index_qr', const_index)
      end if
      if (const_index > 0) then
         r3Ptr(1) % missingValue = MPAS_REAL_FILLVAL
         r3Ptr(1) % constituentNames(const_index) = 'qr'
      end if
      r3Ptr(1) % block => block

            r3Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'scalars', r3Ptr)
      call mpas_pool_add_field(block % allFields, 'scalars', r3Ptr)

! Define variable xtime
      allocate(c0Ptr(1))

! Setting up time level 1
      c0Ptr(1) % fieldName = 'xtime'
      c0Ptr(1) % isVarArray = .false.
      c0Ptr(1) % isDecomposed = .false.
      c0Ptr(1) % hasTimeDimension = .true.
     c0Ptr(1) % defaultValue = ''
     c0Ptr(1) % defaultValue = ''
     c0Ptr(1) % scalar = ''
      nullify(c0Ptr(1) % next)
      nullify(c0Ptr(1) % prev)
      nullify(c0Ptr(1) % sendList)
      nullify(c0Ptr(1) % recvList)
      nullify(c0Ptr(1) % copyList)
      allocate(c0Ptr(1) % attLists(1))
      allocate(c0Ptr(1) % attLists(1) % attList)
      c0Ptr(1) % attLists(1) % attList % attName = ''
      c0Ptr(1) % attLists(1) % attList % attType = -1
      nullify(c0Ptr(1) % attLists(1) % attList % next)
      nullify(c0Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(c0Ptr(1) % attLists(1) % attList % attValueRealA)
      c0Ptr(1) % missingValue = MPAS_CHAR_FILLVAL
      c0Ptr(1) % block => block

      c0Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'xtime', c0Ptr)
      call mpas_pool_add_field(block % allFields, 'xtime', c0Ptr)

! Define variable u
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'u'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nEdges'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'u', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'u', r2Ptr)

! Define variable w
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'w'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevelsP1'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'w', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'w', r2Ptr)

! Define variable rho_zz
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'rho_zz'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'rho_zz', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'rho_zz', r2Ptr)

! Define variable theta_m
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'theta_m'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'theta_m', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'theta_m', r2Ptr)

! Define variable h_oml_initial
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'h_oml_initial'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'h_oml_initial', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'h_oml_initial', r1Ptr)



      if (associated(newSubPool)) then
         call mpas_pool_add_config(newSubPool, 'on_a_sphere', block % domain % on_a_sphere)
         call mpas_pool_add_config(newSubPool, 'sphere_radius', block % domain % sphere_radius)
         call mpas_pool_add_config(newSubPool, 'is_periodic', block % domain % is_periodic)
         call mpas_pool_add_config(newSubPool, 'x_period', block % domain % x_period)
         call mpas_pool_add_config(newSubPool, 'y_period', block % domain % y_period)
      end if

   end subroutine init_atm_generate_pool_state


   subroutine init_atm_generate_pool_fg(block, structPool, dimensionPool, packagePool)
      use mpas_derived_types
      use mpas_pool_routines
      use mpas_io_units
      use mpas_io, only : MPAS_REAL_FILLVAL, MPAS_INT_FILLVAL, MPAS_CHAR_FILLVAL
      implicit none
      type (block_type), intent(inout), pointer :: block
      type (mpas_pool_type), intent(inout) :: structPool
      type (mpas_pool_type), intent(inout) :: dimensionPool
      type (mpas_pool_type), intent(in) :: packagePool

      type (field0DReal), dimension(:), pointer :: r0Ptr
      type (field1DReal), dimension(:), pointer :: r1Ptr
      type (field2DReal), dimension(:), pointer :: r2Ptr
      type (field3DReal), dimension(:), pointer :: r3Ptr
      type (field4DReal), dimension(:), pointer :: r4Ptr
      type (field5DReal), dimension(:), pointer :: r5Ptr
      type (field0DInteger), dimension(:), pointer :: i0Ptr
      type (field1DInteger), dimension(:), pointer :: i1Ptr
      type (field2DInteger), dimension(:), pointer :: i2Ptr
      type (field3DInteger), dimension(:), pointer :: i3Ptr
      type (field0DChar), dimension(:), pointer :: c0Ptr
      type (field1DChar), dimension(:), pointer :: c1Ptr

      type (mpas_pool_type), pointer :: newSubPool
      integer :: group_counter
      logical :: group_started
      integer :: group_start
      integer :: index_counter
      integer, pointer :: const_index

      logical, pointer :: initial_condsActive
      logical, pointer :: sfc_updateActive
      logical, pointer :: gwd_stage_inActive
      logical, pointer :: vertical_stage_inActive
      logical, pointer :: vertical_stage_outActive
      logical, pointer :: met_stage_inActive
      logical, pointer :: met_stage_outActive


      integer :: numConstituents

      nullify(newSubPool)
      group_counter = -1
      group_started = .false.
      group_start = -1
      call mpas_pool_get_package(packagePool, 'initial_condsActive', initial_condsActive)
      call mpas_pool_get_package(packagePool, 'sfc_updateActive', sfc_updateActive)
      call mpas_pool_get_package(packagePool, 'gwd_stage_inActive', gwd_stage_inActive)
      call mpas_pool_get_package(packagePool, 'vertical_stage_inActive', vertical_stage_inActive)
      call mpas_pool_get_package(packagePool, 'vertical_stage_outActive', vertical_stage_outActive)
      call mpas_pool_get_package(packagePool, 'met_stage_inActive', met_stage_inActive)
      call mpas_pool_get_package(packagePool, 'met_stage_outActive', met_stage_outActive)

      allocate(newSubPool)
      call mpas_pool_create_pool(newSubPool)
      call mpas_pool_add_subpool(structPool, 'fg', newSubPool)
      call mpas_pool_add_subpool(block % allStructs, 'fg', newSubPool)

! Define variable u_fg
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'u_fg'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nFGLevels'
      r2Ptr(1) % dimNames(2) = 'nEdges'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'u', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'u_fg', r2Ptr)

! Define variable v_fg
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'v_fg'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nFGLevels'
      r2Ptr(1) % dimNames(2) = 'nEdges'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'v', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'v_fg', r2Ptr)

! Define variable t_fg
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 't_fg'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nFGLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 't', r2Ptr)
      call mpas_pool_add_field(block % allFields, 't_fg', r2Ptr)

! Define variable p_fg
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'p_fg'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nFGLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'p', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'p_fg', r2Ptr)

! Define variable z_fg
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'z_fg'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nFGLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'z', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'z_fg', r2Ptr)

! Define variable rh_fg
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'rh_fg'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nFGLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'rh', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'rh_fg', r2Ptr)

! Define variable sh_fg
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'sh_fg'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nFGLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'sh', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'sh_fg', r2Ptr)

! Define variable soilz_fg
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'soilz_fg'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'soilz', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'soilz_fg', r1Ptr)

! Define variable psfc_fg
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'psfc_fg'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'psfc', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'psfc_fg', r1Ptr)

! Define variable pmsl_fg
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'pmsl_fg'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'pmsl', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'pmsl_fg', r1Ptr)

! Define variable dz_fg
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'dz_fg'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nFGSoilLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'dz_fg', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'dz_fg', r2Ptr)

! Define variable dzs_fg
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'dzs_fg'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nFGSoilLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'dzs_fg', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'dzs_fg', r2Ptr)

! Define variable zs_fg
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'zs_fg'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nFGSoilLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'zs_fg', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'zs_fg', r2Ptr)

! Define variable st_fg
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'st_fg'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nFGSoilLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'st_fg', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'st_fg', r2Ptr)

! Define variable sm_fg
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'sm_fg'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nFGSoilLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'sm_fg', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'sm_fg', r2Ptr)

! Define variable dz
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'dz'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nSoilLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'dz', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'dz', r2Ptr)

! Define variable dzs
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'dzs'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nSoilLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'dzs', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'dzs', r2Ptr)

! Define variable zs
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'zs'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nSoilLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'zs', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'zs', r2Ptr)

! Define variable sh2o
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'sh2o'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nSoilLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'sh2o', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'sh2o', r2Ptr)

! Define variable smois
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'smois'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nSoilLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'smois', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'smois', r2Ptr)

! Define variable tslb
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'tslb'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nSoilLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'tslb', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'tslb', r2Ptr)

! Define variable smcrel
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'smcrel'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nSoilLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'smcrel', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'smcrel', r2Ptr)

! Define variable tmn
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'tmn'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'tmn', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'tmn', r1Ptr)

! Define variable skintemp
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'skintemp'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'skintemp', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'skintemp', r1Ptr)

! Define variable sst
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'sst'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'sst', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'sst', r1Ptr)

! Define variable snow
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'snow'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'snow', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'snow', r1Ptr)

! Define variable snowc
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'snowc'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'snowc', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'snowc', r1Ptr)

! Define variable snowh
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'snowh'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'snowh', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'snowh', r1Ptr)

! Define variable xice
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'xice'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'xice', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'xice', r1Ptr)

! Define variable seaice
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'seaice'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'seaice', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'seaice', r1Ptr)

! Define variable gfs_z
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'gfs_z'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'gfs_z', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'gfs_z', r2Ptr)

! Define variable vegfra
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'vegfra'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'vegfra', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'vegfra', r1Ptr)

! Define variable sfc_albbck
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'sfc_albbck'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'sfc_albbck', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'sfc_albbck', r1Ptr)

! Define variable xland
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'xland'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'xland', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'xland', r1Ptr)

! Define variable u10
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'u10'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'u10', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'u10', r1Ptr)

! Define variable v10
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'v10'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'v10', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'v10', r1Ptr)

! Define variable q2
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'q2'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'q2', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'q2', r1Ptr)

! Define variable rh2
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'rh2'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'rh2', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'rh2', r1Ptr)

! Define variable t2m
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 't2m'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 't2m', r1Ptr)
      call mpas_pool_add_field(block % allFields, 't2m', r1Ptr)



      if (associated(newSubPool)) then
         call mpas_pool_add_config(newSubPool, 'on_a_sphere', block % domain % on_a_sphere)
         call mpas_pool_add_config(newSubPool, 'sphere_radius', block % domain % sphere_radius)
         call mpas_pool_add_config(newSubPool, 'is_periodic', block % domain % is_periodic)
         call mpas_pool_add_config(newSubPool, 'x_period', block % domain % x_period)
         call mpas_pool_add_config(newSubPool, 'y_period', block % domain % y_period)
      end if

   end subroutine init_atm_generate_pool_fg


   subroutine init_atm_generate_pool_diag(block, structPool, dimensionPool, packagePool)
      use mpas_derived_types
      use mpas_pool_routines
      use mpas_io_units
      use mpas_io, only : MPAS_REAL_FILLVAL, MPAS_INT_FILLVAL, MPAS_CHAR_FILLVAL
      implicit none
      type (block_type), intent(inout), pointer :: block
      type (mpas_pool_type), intent(inout) :: structPool
      type (mpas_pool_type), intent(inout) :: dimensionPool
      type (mpas_pool_type), intent(in) :: packagePool

      type (field0DReal), dimension(:), pointer :: r0Ptr
      type (field1DReal), dimension(:), pointer :: r1Ptr
      type (field2DReal), dimension(:), pointer :: r2Ptr
      type (field3DReal), dimension(:), pointer :: r3Ptr
      type (field4DReal), dimension(:), pointer :: r4Ptr
      type (field5DReal), dimension(:), pointer :: r5Ptr
      type (field0DInteger), dimension(:), pointer :: i0Ptr
      type (field1DInteger), dimension(:), pointer :: i1Ptr
      type (field2DInteger), dimension(:), pointer :: i2Ptr
      type (field3DInteger), dimension(:), pointer :: i3Ptr
      type (field0DChar), dimension(:), pointer :: c0Ptr
      type (field1DChar), dimension(:), pointer :: c1Ptr

      type (mpas_pool_type), pointer :: newSubPool
      integer :: group_counter
      logical :: group_started
      integer :: group_start
      integer :: index_counter
      integer, pointer :: const_index

      logical, pointer :: initial_condsActive
      logical, pointer :: sfc_updateActive
      logical, pointer :: gwd_stage_inActive
      logical, pointer :: vertical_stage_inActive
      logical, pointer :: vertical_stage_outActive
      logical, pointer :: met_stage_inActive
      logical, pointer :: met_stage_outActive


      integer :: numConstituents

      nullify(newSubPool)
      group_counter = -1
      group_started = .false.
      group_start = -1
      call mpas_pool_get_package(packagePool, 'initial_condsActive', initial_condsActive)
      call mpas_pool_get_package(packagePool, 'sfc_updateActive', sfc_updateActive)
      call mpas_pool_get_package(packagePool, 'gwd_stage_inActive', gwd_stage_inActive)
      call mpas_pool_get_package(packagePool, 'vertical_stage_inActive', vertical_stage_inActive)
      call mpas_pool_get_package(packagePool, 'vertical_stage_outActive', vertical_stage_outActive)
      call mpas_pool_get_package(packagePool, 'met_stage_inActive', met_stage_inActive)
      call mpas_pool_get_package(packagePool, 'met_stage_outActive', met_stage_outActive)

      allocate(newSubPool)
      call mpas_pool_create_pool(newSubPool)
      call mpas_pool_add_subpool(structPool, 'diag', newSubPool)
      call mpas_pool_add_subpool(block % allStructs, 'diag', newSubPool)

! Define variable pressure_p
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'pressure_p'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'pressure_p', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'pressure_p', r2Ptr)

! Define variable rho
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'rho'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'rho', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'rho', r2Ptr)

! Define variable theta
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'theta'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'theta', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'theta', r2Ptr)

! Define variable v
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'v'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nEdges'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'v', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'v', r2Ptr)

! Define variable relhum
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'relhum'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'relhum', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'relhum', r2Ptr)

! Define variable spechum
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'spechum'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'spechum', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'spechum', r2Ptr)

! Define variable uReconstructX
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'uReconstructX'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'uReconstructX', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'uReconstructX', r2Ptr)

! Define variable uReconstructY
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'uReconstructY'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'uReconstructY', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'uReconstructY', r2Ptr)

! Define variable uReconstructZ
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'uReconstructZ'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'uReconstructZ', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'uReconstructZ', r2Ptr)

! Define variable uReconstructZonal
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'uReconstructZonal'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'uReconstructZonal', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'uReconstructZonal', r2Ptr)

! Define variable uReconstructMeridional
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'uReconstructMeridional'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'uReconstructMeridional', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'uReconstructMeridional', r2Ptr)

! Define variable exner
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'exner'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'exner', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'exner', r2Ptr)

! Define variable exner_base
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'exner_base'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'exner_base', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'exner_base', r2Ptr)

! Define variable rtheta_base
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'rtheta_base'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'rtheta_base', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'rtheta_base', r2Ptr)

! Define variable pressure
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'pressure'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'pressure', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'pressure', r2Ptr)

! Define variable pressure_base
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'pressure_base'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'pressure_base', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'pressure_base', r2Ptr)

! Define variable rho_base
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'rho_base'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'rho_base', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'rho_base', r2Ptr)

! Define variable theta_base
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'theta_base'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'theta_base', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'theta_base', r2Ptr)

! Define variable cqw
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'cqw'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'cqw', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'cqw', r2Ptr)

! Define variable surface_pressure
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'surface_pressure'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'surface_pressure', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'surface_pressure', r1Ptr)

! Define variable ru
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'ru'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nEdges'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'ru', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'ru', r2Ptr)

! Define variable rw
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'rw'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevelsP1'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'rw', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'rw', r2Ptr)

! Define variable rtheta_p
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'rtheta_p'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'rtheta_p', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'rtheta_p', r2Ptr)

! Define variable rho_p
      allocate(r2Ptr(1))

! Setting up time level 1
      r2Ptr(1) % fieldName = 'rho_p'
      r2Ptr(1) % isVarArray = .false.
      r2Ptr(1) % isDecomposed = .true.
      r2Ptr(1) % hasTimeDimension = .true.
      r2Ptr(1) % isPersistent = .true.
      r2Ptr(1) % isActive = .false.
! Setting up dimensions
      r2Ptr(1) % dimNames(1) = 'nVertLevels'
      r2Ptr(1) % dimNames(2) = 'nCells'
     r2Ptr(1) % defaultValue = 0.0
     r2Ptr(1) % defaultValue = 0.0
     nullify(r2Ptr(1) % array)
      nullify(r2Ptr(1) % next)
      nullify(r2Ptr(1) % prev)
      nullify(r2Ptr(1) % sendList)
      nullify(r2Ptr(1) % recvList)
      nullify(r2Ptr(1) % copyList)
      allocate(r2Ptr(1) % attLists(1))
      allocate(r2Ptr(1) % attLists(1) % attList)
      r2Ptr(1) % attLists(1) % attList % attName = ''
      r2Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r2Ptr(1) % attLists(1) % attList % next)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r2Ptr(1) % attLists(1) % attList % attValueRealA)
      r2Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r2Ptr(1) % block => block

      r2Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'rho_p', r2Ptr)
      call mpas_pool_add_field(block % allFields, 'rho_p', r2Ptr)



      if (associated(newSubPool)) then
         call mpas_pool_add_config(newSubPool, 'on_a_sphere', block % domain % on_a_sphere)
         call mpas_pool_add_config(newSubPool, 'sphere_radius', block % domain % sphere_radius)
         call mpas_pool_add_config(newSubPool, 'is_periodic', block % domain % is_periodic)
         call mpas_pool_add_config(newSubPool, 'x_period', block % domain % x_period)
         call mpas_pool_add_config(newSubPool, 'y_period', block % domain % y_period)
      end if

   end subroutine init_atm_generate_pool_diag


   subroutine init_atm_generate_pool_diag_physics(block, structPool, dimensionPool, packagePool)
      use mpas_derived_types
      use mpas_pool_routines
      use mpas_io_units
      use mpas_io, only : MPAS_REAL_FILLVAL, MPAS_INT_FILLVAL, MPAS_CHAR_FILLVAL
      implicit none
      type (block_type), intent(inout), pointer :: block
      type (mpas_pool_type), intent(inout) :: structPool
      type (mpas_pool_type), intent(inout) :: dimensionPool
      type (mpas_pool_type), intent(in) :: packagePool

      type (field0DReal), dimension(:), pointer :: r0Ptr
      type (field1DReal), dimension(:), pointer :: r1Ptr
      type (field2DReal), dimension(:), pointer :: r2Ptr
      type (field3DReal), dimension(:), pointer :: r3Ptr
      type (field4DReal), dimension(:), pointer :: r4Ptr
      type (field5DReal), dimension(:), pointer :: r5Ptr
      type (field0DInteger), dimension(:), pointer :: i0Ptr
      type (field1DInteger), dimension(:), pointer :: i1Ptr
      type (field2DInteger), dimension(:), pointer :: i2Ptr
      type (field3DInteger), dimension(:), pointer :: i3Ptr
      type (field0DChar), dimension(:), pointer :: c0Ptr
      type (field1DChar), dimension(:), pointer :: c1Ptr

      type (mpas_pool_type), pointer :: newSubPool
      integer :: group_counter
      logical :: group_started
      integer :: group_start
      integer :: index_counter
      integer, pointer :: const_index

      logical, pointer :: initial_condsActive
      logical, pointer :: sfc_updateActive
      logical, pointer :: gwd_stage_inActive
      logical, pointer :: vertical_stage_inActive
      logical, pointer :: vertical_stage_outActive
      logical, pointer :: met_stage_inActive
      logical, pointer :: met_stage_outActive


      integer :: numConstituents

      nullify(newSubPool)
      group_counter = -1
      group_started = .false.
      group_start = -1
      call mpas_pool_get_package(packagePool, 'initial_condsActive', initial_condsActive)
      call mpas_pool_get_package(packagePool, 'sfc_updateActive', sfc_updateActive)
      call mpas_pool_get_package(packagePool, 'gwd_stage_inActive', gwd_stage_inActive)
      call mpas_pool_get_package(packagePool, 'vertical_stage_inActive', vertical_stage_inActive)
      call mpas_pool_get_package(packagePool, 'vertical_stage_outActive', vertical_stage_outActive)
      call mpas_pool_get_package(packagePool, 'met_stage_inActive', met_stage_inActive)
      call mpas_pool_get_package(packagePool, 'met_stage_outActive', met_stage_outActive)

      allocate(newSubPool)
      call mpas_pool_create_pool(newSubPool)
      call mpas_pool_add_subpool(structPool, 'diag_physics', newSubPool)
      call mpas_pool_add_subpool(block % allStructs, 'diag_physics', newSubPool)

! Define variable precipw
      allocate(r1Ptr(1))

! Setting up time level 1
      r1Ptr(1) % fieldName = 'precipw'
      r1Ptr(1) % isVarArray = .false.
      r1Ptr(1) % isDecomposed = .true.
      r1Ptr(1) % hasTimeDimension = .true.
      r1Ptr(1) % isPersistent = .true.
      r1Ptr(1) % isActive = .false.
! Setting up dimensions
      r1Ptr(1) % dimNames(1) = 'nCells'
     r1Ptr(1) % defaultValue = 0.0
     r1Ptr(1) % defaultValue = 0.0
     nullify(r1Ptr(1) % array)
      nullify(r1Ptr(1) % next)
      nullify(r1Ptr(1) % prev)
      nullify(r1Ptr(1) % sendList)
      nullify(r1Ptr(1) % recvList)
      nullify(r1Ptr(1) % copyList)
      allocate(r1Ptr(1) % attLists(1))
      allocate(r1Ptr(1) % attLists(1) % attList)
      r1Ptr(1) % attLists(1) % attList % attName = ''
      r1Ptr(1) % attLists(1) % attList % attType = -1
      nullify(r1Ptr(1) % attLists(1) % attList % next)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueIntA)
      nullify(r1Ptr(1) % attLists(1) % attList % attValueRealA)
      r1Ptr(1) % missingValue = MPAS_REAL_FILLVAL
      r1Ptr(1) % block => block

      r1Ptr(1) % isActive = .true.
      call mpas_pool_add_field(newSubPool, 'precipw', r1Ptr)
      call mpas_pool_add_field(block % allFields, 'precipw', r1Ptr)



      if (associated(newSubPool)) then
         call mpas_pool_add_config(newSubPool, 'on_a_sphere', block % domain % on_a_sphere)
         call mpas_pool_add_config(newSubPool, 'sphere_radius', block % domain % sphere_radius)
         call mpas_pool_add_config(newSubPool, 'is_periodic', block % domain % is_periodic)
         call mpas_pool_add_config(newSubPool, 'x_period', block % domain % x_period)
         call mpas_pool_add_config(newSubPool, 'y_period', block % domain % y_period)
      end if

   end subroutine init_atm_generate_pool_diag_physics


   subroutine init_atm_generate_structs(block, structPool, dimensionPool, packagePool)
      use mpas_derived_types
      use mpas_io_units
      implicit none
      type (block_type), pointer, intent(inout) :: block
      type (mpas_pool_type), intent(inout) :: structPool
      type (mpas_pool_type), intent(inout) :: dimensionPool
      type (mpas_pool_type), intent(in) :: packagePool

      call init_atm_generate_pool_mesh(block, structPool, dimensionPool, packagePool)

      call init_atm_generate_pool_state(block, structPool, dimensionPool, packagePool)

      call init_atm_generate_pool_fg(block, structPool, dimensionPool, packagePool)

      call init_atm_generate_pool_diag(block, structPool, dimensionPool, packagePool)

      call init_atm_generate_pool_diag_physics(block, structPool, dimensionPool, packagePool)

   end subroutine init_atm_generate_structs
