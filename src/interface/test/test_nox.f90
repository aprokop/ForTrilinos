! Copyright 2017-2018, UT-Battelle, LLC
!
! SPDX-License-Identifier: BSD-3-Clause
! License-Filename: LICENSE
! ---------------------------------------------------------------------------- !

program main
  ! -------------------------------------------------------------------------- !
#include "ForTrilinosInterface_config.hpp"
#include "ForTrilinos.h"
  use ISO_FORTRAN_ENV
  use, intrinsic :: ISO_C_BINDING
  use fortrilinos
  use forteuchos
#ifdef HAVE_MPI
  use mpi
#endif
  implicit none
  integer :: ierr
  type(TeuchosComm) :: comm
  type(ParameterList) :: params
  ! -------------------------------------------------------------------------- !

#ifdef HAVE_MPI
  ! Initialize MPI subsystem
  call MPI_INIT(ierr)
  if (ierr /= 0) then
    write(*,*) "MPI failed to init"
    stop 1
  endif
  comm = TeuchosComm(MPI_COMM_WORLD)
#else
  comm = TeuchosComm()
#endif

  params = ParameterList("TpetraModelEvaluator1DFEM")
  call load_from_xml(params, 'nox_params.xml')
  call main2(comm, params, ierr)

  if (ierr /= 0) then
    write(*,*) "TEST FAILED!"
    stop 1
  else
    write(*,*) "TEST PASSED!"
  endif

contains

  ! -------------------------------------------------------------------------- !

  subroutine main2(comm, params, ierr)
    ! ------------------------------------------------------------------------ !
    use fortpetra
    use fortrilinos
    use TpetraModelEvaluator1DFEM_module
    implicit none
    integer :: ierr
    type(TeuchosComm) :: comm
    type(ParameterList) :: params
    type(TpetraModelEvaluator1DFEM) :: evaluator
    type(NOXSolver) :: nox_solver
    integer(global_size_type) :: num_global_elems
    real(scalar_type) :: z_min, z_max
    integer(kind(NOXStatusType)) :: status
    ! ------------------------------------------------------------------------ !

    ierr = 0

    ! Create the model evaluator object
    num_global_elems = 100
    z_min = 0.0
    z_max = 1.0
    print*, 'TJF_DBG: HERE I AM X.0'
    evaluator = TpetraModelEvaluator1DFEM(comm, num_global_elems, z_min, z_max)
    print*, 'TJF_DBG: HERE I AM X.1'
    call evaluator%setup(params)
    print*, 'TJF_DBG: HERE I AM X.2'

    nox_solver = NOXSolver(evaluator)
    print*, 'TJF_DBG: HERE I AM X.3'
    call nox_solver%setup(params)
    print*, 'TJF_DBG: HERE I AM X.4'
    status = nox_solver%solve();
    print*, 'TJF_DBG: HERE I AM X.5'

    if (status /= 0) ierr = 1

  end subroutine main2

end program
