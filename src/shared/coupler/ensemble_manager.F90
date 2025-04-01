!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                   !!
!!                   GNU General Public License                      !!
!!                                                                   !!
!! This file is part of the Flexible Modeling System (FMS).          !!
!!                                                                   !!
!! FMS is free software; you can redistribute it and/or modify       !!
!! it and are expected to follow the terms of the GNU General Public !!
!! License as published by the Free Software Foundation.             !!
!!                                                                   !!
!! FMS is distributed in the hope that it will be useful,            !!
!! but WITHOUT ANY WARRANTY; without even the implied warranty of    !!
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     !!
!! GNU General Public License for more details.                      !!
!!                                                                   !!
!! You should have received a copy of the GNU General Public License !!
!! along with FMS; if not, write to:                                 !!
!!          Free Software Foundation, Inc.                           !!
!!          59 Temple Place, Suite 330                               !!
!!          Boston, MA  02111-1307  USA                              !!
!! or see:                                                           !!
!!          http://www.gnu.org/licenses/gpl.txt                      !!
!!                                                                   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module ensemble_manager_mod


  use fms_mod, only : open_namelist_file,close_file,check_nml_error
  use mpp_mod, only : mpp_npes, stdout, stdlog, mpp_error, FATAL
  use mpp_mod, only : mpp_pe, mpp_declare_pelist
  use fms_io_mod, only       : set_filename_appendix 
  use diag_manager_mod, only : set_diag_filename_appendix

  IMPLICIT NONE

  private

  integer, parameter :: MAX_ENSEMBLE_SIZE = 100


  integer, allocatable, dimension(:,:) :: ensemble_pelist
  integer, allocatable, dimension(:,:) :: ensemble_pelist_ocean
  integer, allocatable, dimension(:,:) :: ensemble_pelist_atmos
  integer, allocatable, dimension(:)   :: ensemble_pelist_ocean_filter
  integer, allocatable, dimension(:)   :: ensemble_pelist_atmos_filter

  integer :: ensemble_size = 1
  integer :: ensemble_id = 1
  integer :: pe, total_npes_pm=0,ocean_npes_pm=0,atmos_npes_pm=0

  public :: ensemble_manager_init, get_ensemble_id, get_ensemble_size, get_ensemble_pelist
  public :: ensemble_pelist_setup
  public :: get_ensemble_filter_pelist
contains  

  subroutine ensemble_manager_init()


    integer :: i, io_status, ioun, npes

    namelist /ensemble_nml/ ensemble_size

    ioun = open_namelist_file()
    read(ioun,nml=ensemble_nml,iostat = io_status)
    call close_file(ioun)

    if(ensemble_size < 1) call mpp_error(FATAL, &
       'ensemble_manager_mod: ensemble_nml variable ensemble_size must be a positive integer')
    if(ensemble_size > max_ensemble_size)  call mpp_error(FATAL, &
       'ensemble_manager_mod: ensemble_nml variable ensemble_size should be no larger than MAX_ENSEMBLE_SIZE, '// &
       'change ensemble_size or increase MAX_ENSEMBLE_SIZE')

    pe = mpp_pe()
    npes = mpp_npes()
    total_npes_pm = npes/ensemble_size
    if (mod(npes, total_npes_pm) /= 0) call mpp_error(FATAL,'ensemble_size must be divis by npes')

    call mpp_declare_pelist((/(i,i=0,npes-1)/),'_ens0') ! for ensemble driver

  end subroutine ensemble_manager_init

  function get_ensemble_id()
    integer :: get_ensemble_id
    get_ensemble_id = ensemble_id
  end function get_ensemble_id

  function get_ensemble_size()

    integer, dimension(4) :: get_ensemble_size

    get_ensemble_size(1) = ensemble_size
    get_ensemble_size(2) = total_npes_pm
    get_ensemble_size(3) = ocean_npes_pm
    get_ensemble_size(4) = atmos_npes_pm

  end function get_ensemble_size


  subroutine get_ensemble_pelist(pelist, name)

    integer, intent(inout) :: pelist(:,:)
    character(len=*), intent(in), optional  :: name

    if (size(pelist,1) < ensemble_size) &
         call mpp_error(FATAL,'get_ensemble_pelist: size of pelist 1st index < ensemble_size')

    if(present(name)) then
       select case(name)
       case('ocean')
          if (size(pelist,2) < ocean_npes_pm)&
               call mpp_error(FATAL,'get_ensemble_pelist: size of pelist 2nd index < ocean_npes_pm') 
          pelist = 0       
          pelist(1:ensemble_size,1:ocean_npes_pm) = &
               ensemble_pelist_ocean(1:ensemble_size,1:ocean_npes_pm)

       case('atmos')
          if (size(pelist,2) < atmos_npes_pm)&
               call mpp_error(FATAL,'get_ensemble_pelist: size of pelist 2nd index < atmos_npes_pm') 
          pelist = 0       
          pelist(1:ensemble_size,1:atmos_npes_pm) = &
               ensemble_pelist_atmos(1:ensemble_size,1:atmos_npes_pm)

       case default
          call mpp_error(FATAL,'get_ensemble_pelist: unknown argument name='//name)
       end select
    else
       if (size(pelist,2) < total_npes_pm)&
            call mpp_error(FATAL,'get_ensemble_pelist: size of pelist 2nd index < total_npes_pm') 
       pelist = 0       
       pelist(1:ensemble_size,1:total_npes_pm) = &
            ensemble_pelist(1:ensemble_size,1:total_npes_pm)
    endif

    return
  end subroutine get_ensemble_pelist

  subroutine get_ensemble_filter_pelist(pelist, name)

    integer, intent(inout) :: pelist(:)
    character(len=*), intent(in)  :: name

    select case(name)
    case('ocean')
       if (size(pelist) < ensemble_size * ocean_npes_pm)&
            call mpp_error(FATAL,'get_ensemble_filter_pelist: size of pelist argument < ensemble_size * ocean_npes_pm') 
       pelist = 0       
       pelist(1:ensemble_size*ocean_npes_pm) = &
            ensemble_pelist_ocean_filter(1:ensemble_size*ocean_npes_pm)

    case('atmos')
       if (size(pelist) < ensemble_size * atmos_npes_pm)&
            call mpp_error(FATAL,'get_ensemble_filter_pelist: size of pelist argument < ensemble_size * atmos_npes_pm') 
       pelist = 0       
       pelist(1:ensemble_size*atmos_npes_pm) = &
            ensemble_pelist_atmos_filter(1:ensemble_size*atmos_npes_pm)

    case default
       call mpp_error(FATAL,'get_ensemble_filter_pelist: unknown argument name='//name)
    end select


    return
  end subroutine get_ensemble_filter_pelist

!nnz: I think the following block of code should be contained in a subroutine
!     to consolidate and ensure the consistency of declaring the various pelists.

  subroutine ensemble_pelist_setup(concurrent, atmos_npes, Atm_pelist)    
    logical, intent(in)                  :: concurrent
    integer, intent(in)                  :: atmos_npes
    integer, dimension(:), intent(inout) :: Atm_pelist
    integer           :: atmos_pe_start, atmos_pe_end
    character(len=10) :: pelist_name, text
    integer           :: npes, n, m ,i

    npes = total_npes_pm

    allocate(ensemble_pelist(ensemble_size,total_npes_pm))
    allocate(ensemble_pelist_atmos(1:ensemble_size, 1:atmos_npes) )
    atmos_pe_start = 0
    do n=1,ensemble_size
       atmos_pe_end = atmos_pe_start + atmos_npes
       ensemble_pelist_atmos(n, 1:atmos_npes) = (/(i,i=atmos_pe_start,atmos_pe_end)/)
       ensemble_pelist(n, 1:atmos_npes)       = ensemble_pelist_atmos(n, 1:atmos_npes)
       if(ANY(ensemble_pelist(n,:) == pe)) ensemble_id = n
       write(pelist_name,'(a,i2.2)')  '_ens',n
       call mpp_declare_pelist(ensemble_pelist(n,:), trim(pelist_name))
       atmos_pe_start = atmos_pe_end + 1
    enddo

    Atm_pelist(:)   = ensemble_pelist_atmos(ensemble_id,:)


    if( concurrent )then
       do n=1,ensemble_size
          write(pelist_name,'(a,i2.2)')  'atm_ens',n
          call mpp_declare_pelist(ensemble_pelist_atmos(n,:) , trim(pelist_name) )
       enddo
    else
       write(pelist_name,'(a,i2.2)')  'atm_ens',ensemble_id
       call mpp_declare_pelist(Atm_pelist , trim(pelist_name) )
    endif

    atmos_npes_pm = atmos_npes  

    !Declare pelist of all Ocean and Atmos pes across all ensembles ( filters )
    allocate(ensemble_pelist_atmos_filter(ensemble_size * atmos_npes_pm))
    do n=1,ensemble_size
       do m=1,atmos_npes_pm
          i=(n-1)*atmos_npes_pm + m
          ensemble_pelist_atmos_filter(i) = ensemble_pelist_atmos(n,m)
       enddo
    enddo
    
    write(pelist_name,'(a)')  'atm_filter'
    call mpp_declare_pelist(ensemble_pelist_atmos_filter, trim(pelist_name) )

    !
    !Rename output files to identify the ensemble
    !If ensemble_size=1 do not rename files so that the same coupler
    !can be used for non-ensemble experiments
    !
    if (ensemble_size > 1) then       
       write( text,'(a,i2.2)' ) 'ens_', ensemble_id    
       !Append ensemble_id to the restart filenames
       call set_filename_appendix(trim(text)) 
       !Append ensemble_id to the diag_out filenames
       write( text,'(a,i2.2)' ) '.ens_', ensemble_id    
       call set_diag_filename_appendix(trim(text)) 
    endif   
    
  end subroutine ensemble_pelist_setup


end module ensemble_manager_mod
