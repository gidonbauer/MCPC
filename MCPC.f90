! TODO: Add chemical potential

program MCPC
   use iso_c_binding
   use iso_fortran_env, only: error_unit
   use sdl2
   implicit none

   integer, dimension(:, :), allocatable :: grid
   integer, parameter :: NX=160, NY=120
   real :: T
   real, parameter :: dT = 0.01
   integer :: i, j, ierr
   real :: r

   ! type(SDL_Window), pointer :: window
   type(c_ptr)        :: window, renderer
   type(sdl_event)    :: event
   type(sdl_rect)     :: rect
   integer            :: flags
   integer, parameter :: WINDOW_WIDTH = 800, WINDOW_HEIGTH = 600
   integer, parameter :: RECT_WIDTH = WINDOW_WIDTH / NX, RECT_HEIGTH = WINDOW_HEIGTH / NY

   read_command_line: block
      character(len=256) :: arg
      if (command_argument_count().lt.1) then
         T = 0.1
      else
         call get_command_argument(1, arg)
         read (arg, *) T
      end if
   end block read_command_line

   call random_seed()

   allocate(grid(0:NX+1, 0:NY+1))
   grid = 0
   do i = 1,NX
      do j = 1,NY
         call random_number(r)
         grid(i,j) = merge(1, 0, r.le.0.25)
      end do
   end do

   ierr = SDL_Init(SDL_INIT_VIDEO)
   if (ierr.ne.0) then
      write (error_unit, "('SDL Error: ',A)") SDL_Get_Error()
      error stop "Could not initialize SDL"
   end if

   flags = ior(SDL_WINDOW_RESIZABLE, SDL_WINDOW_OPENGL)
   window = SDL_Create_Window("MCPC" // achar(0), 0, 0, WINDOW_WIDTH, WINDOW_HEIGTH, flags)
   if (.not.c_associated(window)) then
      error stop "Could not create window"
   end if

   renderer = SDL_Create_Renderer(window, -1, SDL_RENDERER_ACCELERATED)
   if (.not.c_associated(renderer)) then
      write (error_unit, "('SDL Error: ',A)") SDL_Get_Error()
      error stop "Could not create renderer"
   end if

   event_loop: do
      ! Poll events.
      do while (SDL_Poll_Event(event) > 0)
         select case (event%type)
            case (SDL_QUITEVENT)
               exit event_loop
            case (SDL_KEYDOWN)
               if (event%key%key_sym%sym.eq.SDLK_q) then
                  exit event_loop
               elseif (event%key%key_sym%sym.eq.SDLK_h) then
                  T = max(0.01, T - dT)
                  print "('T = ',ES13.6)", T
               elseif (event%key%key_sym%sym.eq.SDLK_j) then
                  T = min(2.0, T + dT)
                  print "('T = ',ES13.6)", T
               end if
         end select
      end do

      ierr = SDL_Set_Render_Draw_Color(renderer, uint8(255), uint8(255), uint8(255), uint8(255))
      ierr = SDL_Render_Clear(renderer)

      call render_grid()

      call SDL_Render_Present(renderer)
      ! call SDL_Delay(20)
      do i = 1,100
         ! call periodic_bconds()
         call dirichlet_bconds(0)
         call next_grid()
      end do
   end do event_loop

   call SDL_Destroy_Renderer(renderer)
   call SDL_Destroy_Window(window)
   call SDL_Quit()
   deallocate(grid)

contains
   subroutine render_grid()
      ierr = SDL_Set_Render_Draw_Color(renderer, uint8(0), uint8(136), uint8(204), uint8(255))
      do i = 1,NX
         do j = 1,NY
            if (grid(i,j).eq.1) then
               rect = SDL_Rect((i-1)*RECT_HEIGTH, (j-1)*RECT_WIDTH, RECT_WIDTH, RECT_HEIGTH)
               ierr = SDL_Render_Fill_Rect(renderer, rect)
            end if
         end do
      end do
   end subroutine render_grid

   subroutine next_grid()
      integer :: i1, j1, i2, j2, o
      integer :: E_curr, E_swap
      real    :: q, p, r
      integer, dimension(2, 4), parameter :: offset = reshape([[-1, 0], [1, 0], [0, -1], [0, 1]], shape(offset))
      integer, dimension(2), parameter :: s = shape(offset)

      i1 = 1; j1 = 1; i2 = 1; j2 = 1;
      do while (grid(i1, j1).eq.grid(i2,j2))
         call random_index(i1, j1)
         call random_index(i2, j2)
      end do

      E_curr = 0
      E_swap = 0

      do o = 1,s(2)
         E_curr = E_curr + merge(-1, 0, grid(i1,j1).eq.grid(i1+offset(1,o), j1+offset(2,o)).and.grid(i1,j1).eq.1)
         E_swap = E_swap + merge(-1, 0, grid(i2,j2).eq.grid(i1+offset(1,o), j1+offset(2,o)).and.grid(i2,j2).eq.1)

         E_curr = E_curr + merge(-1, 0, grid(i2,j2).eq.grid(i2+offset(1,o), j2+offset(2,o)).and.grid(i2,j2).eq.1)
         E_swap = E_swap + merge(-1, 0, grid(i1,j1).eq.grid(i2+offset(1,o), j2+offset(2,o)).and.grid(i1,j1).eq.1)
      end do

      q = exp(real(E_curr - E_swap, 4) / T)
      p = q / (1.0 + q)
      call random_number(r)
      if (r.le.p) then
         o            = grid(i1, j1)
         grid(i1, j1) = grid(i2, j2)
         grid(i2, j2) = o
      end if
   end subroutine next_grid

   subroutine random_index(ri, rj)
      integer, intent(out) :: ri, rj
      real :: r
      call random_number(r); ri = int(r * NX + 1)
      call random_number(r); rj = int(r * NY + 1)
   end subroutine random_index

   subroutine dirichlet_bconds(val)
      integer, intent(in) :: val
      grid(0,    1:NY) = val
      grid(NX+1, 1:NY) = val
      grid(1:NX, 0   ) = val
      grid(1:NX, NY+1) = val
   end subroutine dirichlet_bconds

   subroutine periodic_bconds()
      grid(0,    1:NY) = grid(NX, 1:NY)
      grid(NX+1, 1:NY) = grid(1,  1:NY)
      grid(1:NX, 0   ) = grid(1:NX, NY)
      grid(1:NX, NY+1) = grid(1:NX, 1 )
   end subroutine periodic_bconds

   subroutine clamp(val, lo, hi)
      integer, intent(inout) :: val
      integer, intent(in) :: lo, hi

      if (val.lt.lo) then
         val = lo
      elseif (val.gt.hi) then
         val = hi
      end if
   end subroutine clamp
end program MCPC
