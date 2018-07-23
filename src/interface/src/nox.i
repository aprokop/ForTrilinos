
/*
 * Copyright 2017-2018, UT-Battelle, LLC
 *
 * SPDX-License-Identifier: BSD-3-Clause
 * License-Filename: LICENSE
 */
%include <boost_shared_ptr.i>

%{
#include "model_evaluator.hpp"
%}

%include "model_evaluator.hpp"

%teuchos_rcp(ForTrilinos::ModelEvaluator<SC,LO,GO,NO>)
%template() ForTrilinos::ModelEvaluator<SC,LO,GO,NO>;

// =======================================================================
// Add the subclass
// =======================================================================
%teuchos_rcp(ForModelEvaluator)

%insert("header") %{
extern "C" {
/* Fortran BIND(C) function */
void swigd_ForModelEvaluator_evaluate_residual(
        SwigClassWrapper const *fself,
        SwigClassWrapper const *farg1
        );
}
%}

%fortranprepend ForModelEvaluator::~ForModelEvaluator() %{
  type(C_PTR) :: fself_ptr
  type(ForModelEvaluatorHandle), pointer :: handle
  fself_ptr = swigc_ForModelEvaluator_fhandle(self%swigdata)
  call c_f_pointer(cptr=fself_ptr, fptr=handle)
%}

%fortranappend ForModelEvaluator::~ForModelEvaluator() %{
  ! Release the allocated handle
  deallocate(handle)
%}

%inline %{
  class ForModelEvaluator : public ForTrilinos::ModelEvaluator<SC,LO,GO,NO> {
    // Pointer to polymorphic fortran pointer
    void* fhandle_;
   public:
    /* DIRECTOR FUNCTIONS */
    const void* fhandle() const { assert(fhandle_); return this->fhandle_; }
    void init(void* fh) { fhandle_ = fh; }

    /* ModelEvaluator */
    typedef Tpetra::Map<LO,GO,NO> map_type;
    typedef Tpetra::MultiVector<SC,LO,GO,NO> multivector_type;
    typedef Tpetra::Operator<SC,LO,GO,NO> operator_type;

    void setup(const Teuchos::RCP<const map_type>& x_map,
               const Teuchos::RCP<const map_type>& f_map) {}

    virtual void evaluate_residual(Teuchos::RCP<multivector_type>& f) const override {
      /* construct "this" pointer */
      // FIXME for some reason SWIG_NO_NULL_DELETER is included *after* this class definition
      /* Teuchos::RCP<ForModelEvaluator> tempthis( */
             /* const_cast<ForModelEvaluator*>(this) SWIG_NO_NULL_DELETER_0); */
      Teuchos::RCP<ForModelEvaluator> tempthis(
             const_cast<ForModelEvaluator*>(this), Teuchos::RCP_WEAK_NO_DEALLOC);
      SwigClassWrapper self;
      self.ptr = &tempthis;
      self.mem = SWIG_CREF; // since this function is const

      /* convert X -> class wrapper */
      SwigClassWrapper farg1;
      farg1.ptr = &f;
      farg1.mem = SWIG_REF; // f is mutable

      swigd_ForModelEvaluator_evaluate_residual(&self, &farg1);
    }

    virtual void evaluate_jacobian(Teuchos::RCP<operator_type>& J) const override
    { }

    virtual void evaluate_preconditioner(Teuchos::RCP<operator_type>& J) const override
    {
    }

    virtual void update_x(const Teuchos::RCP<const multivector_type>& x) const override
    { }

    virtual Teuchos::RCP<operator_type> create_operator() const override
    { return Teuchos::null; }
  };
%}

%insert("ftypes") %{
  type :: ForModelEvaluatorHandle
    class(ForModelEvaluator), pointer :: data
  end type
%}

%insert("fpublic") %{
public :: init_ForModelEvaluator
%}

%insert("fwrapper") %{
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

! This function must have input/output arguments compatible with ISO C, and it must be marked with "bind(C)"
subroutine swigd_ForModelEvaluator_evaluate_residual(fself, farg1) &
    bind(C, name="swigd_ForModelEvaluator_evaluate_residual")
  use, intrinsic :: ISO_C_BINDING
  implicit none
  type(SwigClassWrapper), intent(in) :: fself
  type(SwigClassWrapper), intent(in) :: farg1

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

subroutine init_ForModelEvaluator(self)
  class(ForModelEvaluator), target :: self
  type(ForModelEvaluatorHandle), pointer :: handle
  allocate(handle)
  handle%data => self
  self%swigdata = swigc_new_ForModelEvaluator()
  call swigc_ForModelEvaluator_init(self%swigdata, c_loc(handle))
end subroutine
%}
