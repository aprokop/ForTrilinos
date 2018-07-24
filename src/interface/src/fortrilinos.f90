! This file was automatically generated by SWIG (http://www.swig.org).
! Version 4.0.0
!
! Do not make changes to this file unless you know what you are doing--modify
! the SWIG interface file instead.

! Copyright 2017-2018, UT-Battelle, LLC
!
! SPDX-License-Identifier: BSD-3-Clause
! License-Filename: LICENSE

module fortrilinos
 use, intrinsic :: ISO_C_BINDING
 use forerror
 use forteuchos
 use fortpetra
 implicit none
 private

 ! PUBLIC METHODS AND TYPES
 public :: TrilinosSolver

 enum, bind(c)
  enumerator :: SwigMemState = -1
  enumerator :: SWIG_NULL = 0
  enumerator :: SWIG_OWN
  enumerator :: SWIG_MOVE
  enumerator :: SWIG_REF
  enumerator :: SWIG_CREF
 end enum


type, bind(C) :: SwigClassWrapper
  type(C_PTR), public :: ptr = C_NULL_PTR
  integer(C_INT), public :: mem = SWIG_NULL
end type

 public :: TrilinosEigenSolver

type, bind(C) :: SwigArrayWrapper
  type(C_PTR), public :: data = C_NULL_PTR
  integer(C_SIZE_T), public :: size = 0
end type

 public :: ForModelEvaluator

public :: init_ForModelEvaluator


 ! TYPES
 type :: TrilinosSolver
  ! These should be treated as PROTECTED data
  type(SwigClassWrapper), public :: swigdata
 contains
  procedure, private :: init__SWIG_0 => swigf_TrilinosSolver_init__SWIG_0
  procedure, private :: init__SWIG_1 => swigf_TrilinosSolver_init__SWIG_1
  procedure :: setup_matrix => swigf_TrilinosSolver_setup_matrix
  procedure :: setup_operator => swigf_TrilinosSolver_setup_operator
  procedure :: setup_solver => swigf_TrilinosSolver_setup_solver
  procedure :: solve => swigf_TrilinosSolver_solve
  procedure :: finalize => swigf_TrilinosSolver_finalize
  procedure :: release => delete_TrilinosSolver
  procedure, private :: swigf_assignment_TrilinosSolver
  generic :: init => init__SWIG_0, init__SWIG_1
  generic :: assignment(=) => swigf_assignment_TrilinosSolver
 end type TrilinosSolver
 interface TrilinosSolver
  procedure new_TrilinosSolver
 end interface
 type :: TrilinosEigenSolver
  ! These should be treated as PROTECTED data
  type(SwigClassWrapper), public :: swigdata
 contains
  procedure, private :: init__SWIG_0 => swigf_TrilinosEigenSolver_init__SWIG_0
  procedure, private :: init__SWIG_1 => swigf_TrilinosEigenSolver_init__SWIG_1
  procedure :: setup_matrix => swigf_TrilinosEigenSolver_setup_matrix
  procedure :: setup_matrix_rhs => swigf_TrilinosEigenSolver_setup_matrix_rhs
  procedure :: setup_operator => swigf_TrilinosEigenSolver_setup_operator
  procedure :: setup_operator_rhs => swigf_TrilinosEigenSolver_setup_operator_rhs
  procedure :: setup_solver => swigf_TrilinosEigenSolver_setup_solver
  procedure :: solve => swigf_TrilinosEigenSolver_solve
  procedure :: finalize => swigf_TrilinosEigenSolver_finalize
  procedure :: release => delete_TrilinosEigenSolver
  procedure, private :: swigf_assignment_TrilinosEigenSolver
  generic :: init => init__SWIG_0, init__SWIG_1
  generic :: assignment(=) => swigf_assignment_TrilinosEigenSolver
 end type TrilinosEigenSolver
 interface TrilinosEigenSolver
  procedure new_TrilinosEigenSolver
 end interface
 type :: ForModelEvaluator
  ! These should be treated as PROTECTED data
  type(SwigClassWrapper), public :: swigdata
 contains
  procedure :: fhandle => swigf_ForModelEvaluator_fhandle
  procedure :: init => swigf_ForModelEvaluator_init
  procedure :: setup => swigf_ForModelEvaluator_setup
  procedure :: evaluate_residual => swigf_ForModelEvaluator_evaluate_residual
  procedure :: evaluate_jacobian => swigf_ForModelEvaluator_evaluate_jacobian
  procedure :: evaluate_preconditioner => swigf_ForModelEvaluator_evaluate_preconditioner
  procedure :: update_x => swigf_ForModelEvaluator_update_x
  procedure :: create_operator => swigf_ForModelEvaluator_create_operator
  procedure :: release => delete_ForModelEvaluator
  procedure, private :: swigf_assignment_ForModelEvaluator
  generic :: assignment(=) => swigf_assignment_ForModelEvaluator
 end type ForModelEvaluator
 interface ForModelEvaluator
  procedure new_ForModelEvaluator
 end interface

  type :: ForModelEvaluatorHandle
    class(ForModelEvaluator), pointer :: data
  end type



 ! WRAPPER DECLARATIONS
 interface
function swigc_new_TrilinosSolver() &
bind(C, name="_wrap_new_TrilinosSolver") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: fresult
end function

subroutine swigc_TrilinosSolver_init__SWIG_0(farg1) &
bind(C, name="_wrap_TrilinosSolver_init__SWIG_0")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
end subroutine

subroutine swigc_TrilinosSolver_init__SWIG_1(farg1, farg2) &
bind(C, name="_wrap_TrilinosSolver_init__SWIG_1")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
end subroutine

subroutine swigc_TrilinosSolver_setup_matrix(farg1, farg2) &
bind(C, name="_wrap_TrilinosSolver_setup_matrix")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
end subroutine

subroutine swigc_TrilinosSolver_setup_operator(farg1, farg2) &
bind(C, name="_wrap_TrilinosSolver_setup_operator")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
end subroutine

subroutine swigc_TrilinosSolver_setup_solver(farg1, farg2) &
bind(C, name="_wrap_TrilinosSolver_setup_solver")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
end subroutine

subroutine swigc_TrilinosSolver_solve(farg1, farg2, farg3) &
bind(C, name="_wrap_TrilinosSolver_solve")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
type(SwigClassWrapper) :: farg3
end subroutine

subroutine swigc_TrilinosSolver_finalize(farg1) &
bind(C, name="_wrap_TrilinosSolver_finalize")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
end subroutine

subroutine swigc_delete_TrilinosSolver(farg1) &
bind(C, name="_wrap_delete_TrilinosSolver")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
end subroutine

  subroutine swigc_assignment_TrilinosSolver(self, other) &
     bind(C, name="_wrap_assign_TrilinosSolver")
   use, intrinsic :: ISO_C_BINDING
   import :: SwigClassWrapper
   type(SwigClassWrapper), intent(inout) :: self
   type(SwigClassWrapper), intent(in) :: other
  end subroutine
function swigc_new_TrilinosEigenSolver() &
bind(C, name="_wrap_new_TrilinosEigenSolver") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: fresult
end function

subroutine swigc_TrilinosEigenSolver_init__SWIG_0(farg1) &
bind(C, name="_wrap_TrilinosEigenSolver_init__SWIG_0")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
end subroutine

subroutine swigc_TrilinosEigenSolver_init__SWIG_1(farg1, farg2) &
bind(C, name="_wrap_TrilinosEigenSolver_init__SWIG_1")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
end subroutine

subroutine swigc_TrilinosEigenSolver_setup_matrix(farg1, farg2) &
bind(C, name="_wrap_TrilinosEigenSolver_setup_matrix")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
end subroutine

subroutine swigc_TrilinosEigenSolver_setup_matrix_rhs(farg1, farg2) &
bind(C, name="_wrap_TrilinosEigenSolver_setup_matrix_rhs")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
end subroutine

subroutine swigc_TrilinosEigenSolver_setup_operator(farg1, farg2) &
bind(C, name="_wrap_TrilinosEigenSolver_setup_operator")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
end subroutine

subroutine swigc_TrilinosEigenSolver_setup_operator_rhs(farg1, farg2) &
bind(C, name="_wrap_TrilinosEigenSolver_setup_operator_rhs")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
end subroutine

subroutine swigc_TrilinosEigenSolver_setup_solver(farg1, farg2) &
bind(C, name="_wrap_TrilinosEigenSolver_setup_solver")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
end subroutine

function swigc_TrilinosEigenSolver_solve(farg1, farg2, farg3, farg4) &
bind(C, name="_wrap_TrilinosEigenSolver_solve") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
import :: SwigArrayWrapper
type(SwigClassWrapper) :: farg1
type(SwigArrayWrapper) :: farg2
type(SwigClassWrapper) :: farg3
type(SwigArrayWrapper) :: farg4
integer(C_SIZE_T) :: fresult
end function

subroutine swigc_TrilinosEigenSolver_finalize(farg1) &
bind(C, name="_wrap_TrilinosEigenSolver_finalize")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
end subroutine

subroutine swigc_delete_TrilinosEigenSolver(farg1) &
bind(C, name="_wrap_delete_TrilinosEigenSolver")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
end subroutine

  subroutine swigc_assignment_TrilinosEigenSolver(self, other) &
     bind(C, name="_wrap_assign_TrilinosEigenSolver")
   use, intrinsic :: ISO_C_BINDING
   import :: SwigClassWrapper
   type(SwigClassWrapper), intent(inout) :: self
   type(SwigClassWrapper), intent(in) :: other
  end subroutine
function swigc_ForModelEvaluator_fhandle(farg1) &
bind(C, name="_wrap_ForModelEvaluator_fhandle") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(C_PTR) :: fresult
end function

subroutine swigc_ForModelEvaluator_init(farg1, farg2) &
bind(C, name="_wrap_ForModelEvaluator_init")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(C_PTR), value :: farg2
end subroutine

subroutine swigc_ForModelEvaluator_setup(farg1, farg2, farg3) &
bind(C, name="_wrap_ForModelEvaluator_setup")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
type(SwigClassWrapper) :: farg3
end subroutine

subroutine swigc_ForModelEvaluator_evaluate_residual(farg1, farg2) &
bind(C, name="_wrap_ForModelEvaluator_evaluate_residual")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
end subroutine

subroutine swigc_ForModelEvaluator_evaluate_jacobian(farg1, farg2) &
bind(C, name="_wrap_ForModelEvaluator_evaluate_jacobian")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
end subroutine

subroutine swigc_ForModelEvaluator_evaluate_preconditioner(farg1, farg2) &
bind(C, name="_wrap_ForModelEvaluator_evaluate_preconditioner")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
end subroutine

subroutine swigc_ForModelEvaluator_update_x(farg1, farg2) &
bind(C, name="_wrap_ForModelEvaluator_update_x")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
end subroutine

function swigc_ForModelEvaluator_create_operator(farg1) &
bind(C, name="_wrap_ForModelEvaluator_create_operator") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: fresult
end function

function swigc_new_ForModelEvaluator() &
bind(C, name="_wrap_new_ForModelEvaluator") &
result(fresult)
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: fresult
end function

subroutine swigc_delete_ForModelEvaluator(farg1) &
bind(C, name="_wrap_delete_ForModelEvaluator")
use, intrinsic :: ISO_C_BINDING
import :: SwigClassWrapper
type(SwigClassWrapper) :: farg1
end subroutine

  subroutine swigc_assignment_ForModelEvaluator(self, other) &
     bind(C, name="_wrap_assign_ForModelEvaluator")
   use, intrinsic :: ISO_C_BINDING
   import :: SwigClassWrapper
   type(SwigClassWrapper), intent(inout) :: self
   type(SwigClassWrapper), intent(in) :: other
  end subroutine
 end interface


contains
 ! FORTRAN PROXY CODE
function new_TrilinosSolver() &
result(self)
use, intrinsic :: ISO_C_BINDING
type(TrilinosSolver) :: self
type(SwigClassWrapper) :: fresult

fresult = swigc_new_TrilinosSolver()
self%swigdata = fresult
end function

subroutine swigf_TrilinosSolver_init__SWIG_0(self)
use, intrinsic :: ISO_C_BINDING
class(TrilinosSolver), intent(inout) :: self
type(SwigClassWrapper) :: farg1

farg1 = self%swigdata
call swigc_TrilinosSolver_init__SWIG_0(farg1)
end subroutine

subroutine swigf_TrilinosSolver_init__SWIG_1(self, comm)
use, intrinsic :: ISO_C_BINDING
class(TrilinosSolver), intent(inout) :: self
class(TeuchosComm), intent(in) :: comm
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2

farg1 = self%swigdata
farg2 = comm%swigdata
call swigc_TrilinosSolver_init__SWIG_1(farg1, farg2)
end subroutine

subroutine swigf_TrilinosSolver_setup_matrix(self, a)
use, intrinsic :: ISO_C_BINDING
class(TrilinosSolver), intent(inout) :: self
class(TpetraCrsMatrix), intent(in) :: a
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2

farg1 = self%swigdata
farg2 = a%swigdata
call swigc_TrilinosSolver_setup_matrix(farg1, farg2)
end subroutine

subroutine swigf_TrilinosSolver_setup_operator(self, a)
use, intrinsic :: ISO_C_BINDING
class(TrilinosSolver), intent(inout) :: self
class(TpetraOperator), intent(in) :: a
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2

farg1 = self%swigdata
farg2 = a%swigdata
call swigc_TrilinosSolver_setup_operator(farg1, farg2)
end subroutine

subroutine swigf_TrilinosSolver_setup_solver(self, paramlist)
use, intrinsic :: ISO_C_BINDING
class(TrilinosSolver), intent(inout) :: self
class(ParameterList), intent(inout) :: paramlist
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2

farg1 = self%swigdata
farg2 = paramlist%swigdata
call swigc_TrilinosSolver_setup_solver(farg1, farg2)
end subroutine

subroutine swigf_TrilinosSolver_solve(self, rhs, lhs)
use, intrinsic :: ISO_C_BINDING
class(TrilinosSolver), intent(in) :: self
class(TpetraMultiVector), intent(in) :: rhs
class(TpetraMultiVector), intent(inout) :: lhs
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
type(SwigClassWrapper) :: farg3

farg1 = self%swigdata
farg2 = rhs%swigdata
farg3 = lhs%swigdata
call swigc_TrilinosSolver_solve(farg1, farg2, farg3)
end subroutine

subroutine swigf_TrilinosSolver_finalize(self)
use, intrinsic :: ISO_C_BINDING
class(TrilinosSolver), intent(inout) :: self
type(SwigClassWrapper) :: farg1

farg1 = self%swigdata
call swigc_TrilinosSolver_finalize(farg1)
end subroutine

subroutine delete_TrilinosSolver(self)
use, intrinsic :: ISO_C_BINDING
class(TrilinosSolver), intent(inout) :: self
type(SwigClassWrapper) :: farg1

farg1 = self%swigdata
if (self%swigdata%mem == SWIG_OWN) then
call swigc_delete_TrilinosSolver(farg1)
end if
self%swigdata%ptr = C_NULL_PTR
self%swigdata%mem = SWIG_NULL
end subroutine

  subroutine swigf_assignment_TrilinosSolver(self, other)
   use, intrinsic :: ISO_C_BINDING
   class(TrilinosSolver), intent(inout) :: self
   type(TrilinosSolver), intent(in) :: other
   call swigc_assignment_TrilinosSolver(self%swigdata, other%swigdata)
  end subroutine
function new_TrilinosEigenSolver() &
result(self)
use, intrinsic :: ISO_C_BINDING
type(TrilinosEigenSolver) :: self
type(SwigClassWrapper) :: fresult

fresult = swigc_new_TrilinosEigenSolver()
self%swigdata = fresult
end function

subroutine swigf_TrilinosEigenSolver_init__SWIG_0(self)
use, intrinsic :: ISO_C_BINDING
class(TrilinosEigenSolver), intent(inout) :: self
type(SwigClassWrapper) :: farg1

farg1 = self%swigdata
call swigc_TrilinosEigenSolver_init__SWIG_0(farg1)
end subroutine

subroutine swigf_TrilinosEigenSolver_init__SWIG_1(self, comm)
use, intrinsic :: ISO_C_BINDING
class(TrilinosEigenSolver), intent(inout) :: self
class(TeuchosComm), intent(in) :: comm
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2

farg1 = self%swigdata
farg2 = comm%swigdata
call swigc_TrilinosEigenSolver_init__SWIG_1(farg1, farg2)
end subroutine

subroutine swigf_TrilinosEigenSolver_setup_matrix(self, a)
use, intrinsic :: ISO_C_BINDING
class(TrilinosEigenSolver), intent(inout) :: self
class(TpetraCrsMatrix), intent(in) :: a
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2

farg1 = self%swigdata
farg2 = a%swigdata
call swigc_TrilinosEigenSolver_setup_matrix(farg1, farg2)
end subroutine

subroutine swigf_TrilinosEigenSolver_setup_matrix_rhs(self, m)
use, intrinsic :: ISO_C_BINDING
class(TrilinosEigenSolver), intent(inout) :: self
class(TpetraCrsMatrix), intent(in) :: m
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2

farg1 = self%swigdata
farg2 = m%swigdata
call swigc_TrilinosEigenSolver_setup_matrix_rhs(farg1, farg2)
end subroutine

subroutine swigf_TrilinosEigenSolver_setup_operator(self, a)
use, intrinsic :: ISO_C_BINDING
class(TrilinosEigenSolver), intent(inout) :: self
class(TpetraOperator), intent(in) :: a
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2

farg1 = self%swigdata
farg2 = a%swigdata
call swigc_TrilinosEigenSolver_setup_operator(farg1, farg2)
end subroutine

subroutine swigf_TrilinosEigenSolver_setup_operator_rhs(self, m)
use, intrinsic :: ISO_C_BINDING
class(TrilinosEigenSolver), intent(inout) :: self
class(TpetraOperator), intent(in) :: m
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2

farg1 = self%swigdata
farg2 = m%swigdata
call swigc_TrilinosEigenSolver_setup_operator_rhs(farg1, farg2)
end subroutine

subroutine swigf_TrilinosEigenSolver_setup_solver(self, paramlist)
use, intrinsic :: ISO_C_BINDING
class(TrilinosEigenSolver), intent(inout) :: self
class(ParameterList), intent(in) :: paramlist
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2

farg1 = self%swigdata
farg2 = paramlist%swigdata
call swigc_TrilinosEigenSolver_setup_solver(farg1, farg2)
end subroutine

function swigf_TrilinosEigenSolver_solve(self, eigenvalues, eigenvectors, eigenindex) &
result(swig_result)
use, intrinsic :: ISO_C_BINDING
integer(C_SIZE_T) :: swig_result
class(TrilinosEigenSolver), intent(in) :: self
real(C_DOUBLE), dimension(:), target :: eigenvalues
real(C_DOUBLE), pointer :: farg2_view
class(TpetraMultiVector), intent(inout) :: eigenvectors
integer(C_INT), dimension(:), target :: eigenindex
integer(C_INT), pointer :: farg4_view
integer(C_SIZE_T) :: fresult 
type(SwigClassWrapper) :: farg1 
type(SwigArrayWrapper) :: farg2 
type(SwigClassWrapper) :: farg3 
type(SwigArrayWrapper) :: farg4 

farg1 = self%swigdata
farg2_view => eigenvalues(1)
farg2%data = c_loc(farg2_view)
farg2%size = size(eigenvalues)
farg3 = eigenvectors%swigdata
farg4_view => eigenindex(1)
farg4%data = c_loc(farg4_view)
farg4%size = size(eigenindex)
fresult = swigc_TrilinosEigenSolver_solve(farg1, farg2, farg3, farg4)
swig_result = fresult
end function

subroutine swigf_TrilinosEigenSolver_finalize(self)
use, intrinsic :: ISO_C_BINDING
class(TrilinosEigenSolver), intent(inout) :: self
type(SwigClassWrapper) :: farg1

farg1 = self%swigdata
call swigc_TrilinosEigenSolver_finalize(farg1)
end subroutine

subroutine delete_TrilinosEigenSolver(self)
use, intrinsic :: ISO_C_BINDING
class(TrilinosEigenSolver), intent(inout) :: self
type(SwigClassWrapper) :: farg1

farg1 = self%swigdata
if (self%swigdata%mem == SWIG_OWN) then
call swigc_delete_TrilinosEigenSolver(farg1)
end if
self%swigdata%ptr = C_NULL_PTR
self%swigdata%mem = SWIG_NULL
end subroutine

  subroutine swigf_assignment_TrilinosEigenSolver(self, other)
   use, intrinsic :: ISO_C_BINDING
   class(TrilinosEigenSolver), intent(inout) :: self
   type(TrilinosEigenSolver), intent(in) :: other
   call swigc_assignment_TrilinosEigenSolver(self%swigdata, other%swigdata)
  end subroutine
function swigf_ForModelEvaluator_fhandle(self) &
result(swig_result)
use, intrinsic :: ISO_C_BINDING
type(C_PTR) :: swig_result
class(ForModelEvaluator), intent(in) :: self
type(C_PTR) :: fresult
type(SwigClassWrapper) :: farg1

farg1 = self%swigdata
fresult = swigc_ForModelEvaluator_fhandle(farg1)
swig_result = fresult
end function

subroutine swigf_ForModelEvaluator_init(self, fh)
use, intrinsic :: ISO_C_BINDING
class(ForModelEvaluator), intent(inout) :: self
type(C_PTR) :: fh
type(SwigClassWrapper) :: farg1
type(C_PTR) :: farg2

farg1 = self%swigdata
farg2 = fh
call swigc_ForModelEvaluator_init(farg1, farg2)
end subroutine

subroutine swigf_ForModelEvaluator_setup(self, x_map, f_map)
use, intrinsic :: ISO_C_BINDING
class(ForModelEvaluator), intent(inout) :: self
class(TpetraMap), intent(in) :: x_map
class(TpetraMap), intent(in) :: f_map
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2
type(SwigClassWrapper) :: farg3

farg1 = self%swigdata
farg2 = x_map%swigdata
farg3 = f_map%swigdata
call swigc_ForModelEvaluator_setup(farg1, farg2, farg3)
end subroutine

subroutine swigf_ForModelEvaluator_evaluate_residual(self, f)
use, intrinsic :: ISO_C_BINDING
class(ForModelEvaluator), intent(in) :: self
class(TpetraMultiVector), intent(inout) :: f
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2

farg1 = self%swigdata
farg2 = f%swigdata
call swigc_ForModelEvaluator_evaluate_residual(farg1, farg2)
end subroutine

subroutine swigf_ForModelEvaluator_evaluate_jacobian(self, j)
use, intrinsic :: ISO_C_BINDING
class(ForModelEvaluator), intent(in) :: self
class(TpetraOperator), intent(inout) :: j
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2

farg1 = self%swigdata
farg2 = j%swigdata
call swigc_ForModelEvaluator_evaluate_jacobian(farg1, farg2)
end subroutine

subroutine swigf_ForModelEvaluator_evaluate_preconditioner(self, m)
use, intrinsic :: ISO_C_BINDING
class(ForModelEvaluator), intent(in) :: self
class(TpetraOperator), intent(inout) :: m
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2

farg1 = self%swigdata
farg2 = m%swigdata
call swigc_ForModelEvaluator_evaluate_preconditioner(farg1, farg2)
end subroutine

subroutine swigf_ForModelEvaluator_update_x(self, x)
use, intrinsic :: ISO_C_BINDING
class(ForModelEvaluator), intent(in) :: self
class(TpetraMultiVector), intent(in) :: x
type(SwigClassWrapper) :: farg1
type(SwigClassWrapper) :: farg2

farg1 = self%swigdata
farg2 = x%swigdata
call swigc_ForModelEvaluator_update_x(farg1, farg2)
end subroutine

function swigf_ForModelEvaluator_create_operator(self) &
result(swig_result)
use, intrinsic :: ISO_C_BINDING
type(TpetraOperator) :: swig_result
class(ForModelEvaluator), intent(in) :: self
type(SwigClassWrapper) :: fresult
type(SwigClassWrapper) :: farg1

farg1 = self%swigdata
fresult = swigc_ForModelEvaluator_create_operator(farg1)
swig_result%swigdata = fresult
end function

function new_ForModelEvaluator() &
result(self)
use, intrinsic :: ISO_C_BINDING
type(ForModelEvaluator) :: self
type(SwigClassWrapper) :: fresult

fresult = swigc_new_ForModelEvaluator()
self%swigdata = fresult
end function

subroutine delete_ForModelEvaluator(self)
use, intrinsic :: ISO_C_BINDING
class(ForModelEvaluator), intent(inout) :: self
type(SwigClassWrapper) :: farg1


type(C_PTR) :: fself_ptr
type(ForModelEvaluatorHandle), pointer :: handle
fself_ptr = swigc_ForModelEvaluator_fhandle(self%swigdata)
call c_f_pointer(cptr=fself_ptr, fptr=handle)
farg1 = self%swigdata
if (self%swigdata%mem == SWIG_OWN) then
call swigc_delete_ForModelEvaluator(farg1)
end if
self%swigdata%ptr = C_NULL_PTR
self%swigdata%mem = SWIG_NULL

! Release the allocated handle
deallocate(handle)
end subroutine

  subroutine swigf_assignment_ForModelEvaluator(self, other)
   use, intrinsic :: ISO_C_BINDING
   class(ForModelEvaluator), intent(inout) :: self
   type(ForModelEvaluator), intent(in) :: other
   call swigc_assignment_ForModelEvaluator(self%swigdata, other%swigdata)
  end subroutine

! Convert a ISO-C class pointer struct into a user Fortran native pointer
subroutine c_f_pointer_ForModelEvaluator(clswrap, fptr)
  type(SwigClassWrapper), intent(in) :: clswrap
  class(ForModelEvaluator), pointer, intent(out) :: fptr
  type(ForModelEvaluatorHandle), pointer :: handle
  type(C_PTR) :: fself_ptr
  ! Convert C handle to fortran pointer
  fself_ptr = swigc_ForModelEvaluator_fhandle(clswrap)
  ! *** NOTE *** : gfortran 5 through 7 falsely claim the next line is not standards compliant. Since 'handle' is a scalar and
  ! not an array it should be OK, but TS29113 explicitly removes the interoperability requirement for fptr.
  ! Error: TS 29113/TS 18508: Noninteroperable array FPTR at (1) to C_F_POINTER: Expression is a noninteroperable derived type
  ! see https://gcc.gnu.org/bugzilla/show_bug.cgi?id=84924
  call c_f_pointer(cptr=fself_ptr, fptr=handle)
  if (.not. associated(handle)) stop 1
  ! Access the pointer inside that
  fptr => handle%data
  if (.not. associated(fptr)) stop 2
end subroutine

subroutine swigd_ForModelEvaluator_setup(fself, farg1, farg2) &
    bind(C, name="swigd_ForModelEvaluator_setup")
  use, intrinsic :: ISO_C_BINDING
  implicit none
  type(SwigClassWrapper), intent(in) :: fself
  type(SwigClassWrapper), intent(inout) :: farg1
  type(SwigClassWrapper), intent(inout) :: farg2

  class(ForModelEvaluator), pointer :: self
  type(TpetraMap) :: x_map
  type(TpetraMap) :: f_map

  ! Get pointer to Fortran object from class wrapper
  call c_f_pointer_ForModelEvaluator(fself, self)
  if (.not. associated(self)) stop 3

  ! Convert class references to fortran proxy references
  x_map%swigdata = farg1
  f_map%swigdata = farg2

  ! Call fortran function pointer with native fortran input/output
  call self%setup(x_map, f_map)
end subroutine

! This function must have input/output arguments compatible with ISO C, and it must be marked with "bind(C)"
subroutine swigd_ForModelEvaluator_evaluate_residual(fself, farg1) &
    bind(C, name="swigd_ForModelEvaluator_evaluate_residual")
  use, intrinsic :: ISO_C_BINDING
  implicit none
  type(SwigClassWrapper), intent(in) :: fself
  type(SwigClassWrapper), intent(inout) :: farg1

  class(ForModelEvaluator), pointer :: self
  type(TpetraMultiVector) :: f

  ! Get pointer to Fortran object from class wrapper
  call c_f_pointer_ForModelEvaluator(fself, self)
  if (.not. associated(self)) stop 3

  ! Convert class references to fortran proxy references
  f%swigdata = farg1

  ! Call fortran function pointer with native fortran input/output
  call self%evaluate_residual(f)
end subroutine

! This function must have input/output arguments compatible with ISO C, and it must be marked with "bind(C)"
subroutine swigd_ForModelEvaluator_evaluate_jacobian(fself, farg1) &
    bind(C, name="swigd_ForModelEvaluator_evaluate_jacobian")
  use, intrinsic :: ISO_C_BINDING
  implicit none
  type(SwigClassWrapper), intent(in) :: fself
  type(SwigClassWrapper), intent(in) :: farg1

  class(ForModelEvaluator), pointer :: self
  type(ForTpetraOperator) :: J

  ! Get pointer to Fortran object from class wrapper
  call c_f_pointer_ForModelEvaluator(fself, self)
  if (.not. associated(self)) stop 3

  ! Convert class references to fortran proxy references
  J%swigdata = farg1

  ! Call fortran function pointer with native fortran input/output
  call self%evaluate_jacobian(J)
end subroutine

! This function must have input/output arguments compatible with ISO C, and it must be marked with "bind(C)"
subroutine swigd_ForModelEvaluator_evaluate_preconditioner(fself, farg1) &
    bind(C, name="swigd_ForModelEvaluator_evaluate_preconditioner")
  use, intrinsic :: ISO_C_BINDING
  implicit none
  type(SwigClassWrapper), intent(in) :: fself
  type(SwigClassWrapper), intent(in) :: farg1

  class(ForModelEvaluator), pointer :: self
  type(ForTpetraOperator) :: M

  ! Get pointer to Fortran object from class wrapper
  call c_f_pointer_ForModelEvaluator(fself, self)
  if (.not. associated(self)) stop 3

  ! Convert class references to fortran proxy references
  M%swigdata = farg1

  ! Call fortran function pointer with native fortran input/output
  call self%evaluate_preconditioner(M)
end subroutine

! This function must have input/output arguments compatible with ISO C, and it must be marked with "bind(C)"
subroutine swigd_ForModelEvaluator_update_x(fself, farg1) &
    bind(C, name="swigd_ForModelEvaluator_update_x")
  use, intrinsic :: ISO_C_BINDING
  implicit none
  type(SwigClassWrapper), intent(in) :: fself
  type(SwigClassWrapper), intent(in) :: farg1

  class(ForModelEvaluator), pointer :: self
  type(TpetraMultiVector) :: x

  ! Get pointer to Fortran object from class wrapper
  call c_f_pointer_ForModelEvaluator(fself, self)
  if (.not. associated(self)) stop 3

  ! Convert class references to fortran proxy references
  x%swigdata = farg1

  ! Call fortran function pointer with native fortran input/output
  call self%update_x(x)
end subroutine

function swigd_ForModelEvaluator_create_operator(fself) &
    bind(C, name="swigd_ForModelEvaluator_create_operator") &
    result(fresult)
  use, intrinsic :: ISO_C_BINDING
  implicit none
  type(SwigClassWrapper), intent(in) :: fself
  type(SwigClassWrapper) :: fresult

  class(ForModelEvaluator), pointer :: self
  type(ForTpetraOperator) :: result

  ! Get pointer to Fortran object from class Handle
  call c_f_pointer_ForModelEvaluator(fself, self)
  if (.not. associated(self)) stop 3

  result = self%create_operator()

  fresult = result%swigdata
end function

subroutine init_ForModelEvaluator(self)
  class(ForModelEvaluator), target :: self
  type(ForModelEvaluatorHandle), pointer :: handle
  allocate(handle)
  handle%data => self
  self%swigdata = swigc_new_ForModelEvaluator()
  call swigc_ForModelEvaluator_init(self%swigdata, c_loc(handle))
end subroutine


end module
