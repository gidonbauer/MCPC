program MCPC
   use iso_c_binding
   use iso_fortran_env, only: error_unit
   use sdl2
   implicit none

   real, parameter :: T = 0.1
   integer, dimension(:, :), allocatable :: grid
   integer, parameter :: NX=80, NY=60
   integer :: i, j, ierr
   real :: r

   ! type(SDL_Window), pointer :: window
   type(c_ptr)        :: window, renderer
   type(sdl_event)    :: event
   type(sdl_rect)     :: rect
   integer            :: flags
   integer, parameter :: WINDOW_WIDTH = 800, WINDOW_HEIGTH = 600
   integer, parameter :: RECT_WIDTH = WINDOW_WIDTH / NX, RECT_HEIGTH = WINDOW_HEIGTH / NY

   call random_seed()

   allocate(grid(NX, NY))
   grid = 0
   do i = 2,NX-1
      do j = 2,NY-1
         call random_number(r)
         grid(i,j) = merge(0, 1, r.le.0.75)
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
               end if
         end select
      end do

      ierr = SDL_Set_Render_Draw_Color(renderer, uint8(255), uint8(255), uint8(255), uint8(255))
      ierr = SDL_Render_Clear(renderer)

      call render_grid()

      call SDL_Render_Present(renderer)
      ! call SDL_Delay(20)
      do i = 1,100
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

      call random_index(i1, j1); call clamp(i1, 2, NX-1); call clamp(j1, 2, NY-1)
      call random_index(i2, j2); call clamp(i2, 2, NX-1); call clamp(j2, 2, NY-1)
      if (grid(i1, j1).eq.grid(i2,j2)) then
         return
      end if

      E_curr = 0
      E_swap = 0

      do o = 1,s(1)
         E_curr = E_curr + merge(-1, 0, grid(i1,j1).eq.grid(i1+offset(1,o), j1+offset(2,o)).and.grid(i1,j1).eq.1)
         E_swap = E_swap + merge(-1, 0, grid(i2,j2).eq.grid(i1+offset(1,o), j1+offset(2,o)).and.grid(i2,j2).eq.1)

         E_curr = E_curr + merge(-1, 0, grid(i2,j2).eq.grid(i2+offset(1,o), j2+offset(2,o)).and.grid(i2,j2).eq.1)
         E_swap = E_swap + merge(-1, 0, grid(i1,j1).eq.grid(i2+offset(1,o), j2+offset(2,o)).and.grid(i1,j1).eq.1)
      end do

      q = exp(real(E_curr - E_swap, 4) / T)
      if (q.ne.q) then
         write (error_unit, "('E_curr = ',I0)") E_curr
         write (error_unit, "('E_swap = ',I0)") E_swap
         error stop "q is NaN"
      end if

      if (q.ge.huge(q)) then
         write (error_unit, "('E_curr = ',I0)") E_curr
         write (error_unit, "('E_swap = ',I0)") E_swap
         error stop "q is inf"
      end if
      
      p = q / (1.0 + q)
      if (p.ne.p) then
         write (error_unit, "('E_curr = ',I0)") E_curr
         write (error_unit, "('E_swap = ',I0)") E_swap
         error stop "p is NaN"
      end if
      if (p.lt.0.0.or.p.gt.1.0) then
         print "('q = ',ES13.6)", q
         print "('p = ',ES13.6)", p
         error stop "Probability is not in [0,1]"
      end if

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
