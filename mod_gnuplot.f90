module class_gnuplot

  implicit none
  
  private
  
  public :: dp, gp_plot, gp_image

  integer, parameter :: dp = selected_real_kind(p=15, r=307)

  character(len=10), parameter :: default_terminal   = 'x11'
  character(len=10), parameter :: default_plot_type  = 'lines'
  character(len=10), parameter :: default_line_color = '1'
  character(len=10), parameter :: default_line_type  = '1'
  character(len=10), parameter :: default_line_width = '1'
  character(len=10), parameter :: default_point_type = '1'
  character(len=10), parameter :: default_point_size = '1'
  
  character(len=100), parameter :: MATLAB_JET='defined '// &
       & '(1 "#00008f", 8 "#0000ff", 24 "#00ffff", '// &
       & '40 "#ffff00", 56 "#ff0000", 64 "#800000")'

  type, abstract :: gnuplot
     character(len=:), allocatable :: str
     character(len=:), allocatable :: dat_file_name
     logical  :: save_file = .false.
     real(dp) :: timeout = 0.d0
   contains
     procedure, non_overridable, pass :: add_str
     procedure, non_overridable, pass :: init
     procedure, non_overridable, pass :: set_term
     procedure, non_overridable, pass :: set_range
     procedure, non_overridable, pass :: set_label
     procedure, non_overridable, pass :: set_palet
     procedure, non_overridable, pass :: set_cntr
     procedure, non_overridable, pass :: set_pm3d
     procedure, non_overridable, pass :: set_pause
     procedure, non_overridable, pass :: excute
     procedure, non_overridable, pass :: free
  end type gnuplot

  type, extends(gnuplot) :: gp_plot
     logical :: first_plot = .true.
   contains
     procedure, pass :: set_config => set_config_plot
     procedure, pass :: add_plot => add_plot_2d
     final :: free_gp_plot
  end type gp_plot
  
  type, extends(gnuplot) :: gp_image
   contains
     procedure, pass :: set_config => set_config_image
     procedure, pass :: add_plot => add_plot_3d
     final :: free_gp_image
  end type gp_image

contains

! --- subroutines for base class, gnuplot
  subroutine add_str(self, str)
    class(gnuplot), intent(inout) :: self
    character(len=*),  intent(in) :: str

    if (allocated(self%str)) then
       self%str = self%str//str//'; '
    end if
  end subroutine add_str

  subroutine init(self, dat_file_name, save_file, timeout)
    class(gnuplot), intent(inout) :: self
    character(len=*),  intent(in) :: dat_file_name
    logical,  intent(in) :: save_file
    real(dp), intent(in) :: timeout

    self%str = ''
    self%dat_file_name = trim(dat_file_name)
    self%save_file = save_file
    self%timeout = timeout
  end subroutine init

  subroutine set_term(self, terminal, persist, title, filename)
    class(gnuplot), intent(inout) :: self
    character(len=*), intent(in) :: terminal, persist, title, filename

    if (trim(terminal) /= default_terminal) then
       call self%add_str('set term '//trim(term_type(terminal)))
       call self%add_str('set title "'//trim(title)//'"')
       call self%add_str('set output "'//trim(filename)//'"')
    else
       call self%add_str('set term '//trim(default_terminal)//' '// &
            & trim(persist)//' title "'//trim(title)//'"')
    end if
  end subroutine set_term

  subroutine set_range(self, xrange1, xrange2, yrange1, yrange2)
    class(gnuplot), intent(inout) :: self
    real(dp), intent(in) :: xrange1, xrange2, yrange1, yrange2
    character(len=20) :: x1, x2, y1, y2

    write(x1, '(e15.7)') xrange1
    write(x2, '(e15.7)') xrange2
    write(y1, '(e15.7)') yrange1
    write(y2, '(e15.7)') yrange2

    call self%add_str('set xrange ['//trim(x1)//':'//trim(x2)//']')
    call self%add_str('set yrange ['//trim(y1)//':'//trim(y2)//']')
  end subroutine set_range

  subroutine set_label(self, xlabel, ylabel, zlabel)
    class(gnuplot), intent(inout) :: self
    character(len=*),  intent(in), optional :: xlabel, ylabel, zlabel

    if (present(xlabel)) call self%add_str('set xlabel "'//trim(xlabel)//'"')
    if (present(ylabel)) call self%add_str('set ylabel "'//trim(ylabel)//'"')
    if (present(zlabel)) call self%add_str('set zlabel "'//trim(zlabel)//'"')
  end subroutine set_label

  subroutine set_palet(self, palette)
    class(gnuplot), intent(inout) :: self
    character(len=*),  intent(in) :: palette

    if ((trim(palette).eq.'RGB') &
         & .or. (trim(palette).eq.'HSV') &
         & .or. (trim(palette).eq.'CMY') &
         & .or. (trim(palette).eq.'YIQ') &
         & .or. (trim(palette).eq.'XYZ')) then
       call self%add_str('set palette model '//trim(palette))
    else     
       if (trim(palette).eq.'JET') then
          call self%add_str('set palette '//trim(MATLAB_JET))
       else
          call self%add_str('set palette '//trim(palette))
       end if
    end if
  end subroutine set_palet
  
  subroutine set_cntr(self, contour)
    class(gnuplot), intent(inout) :: self
    character(len=*),  intent(in) :: contour
    
    select case(trim(contour))
    case('base')
       call self%add_str('set contour base')
    case('surface')
       call self%add_str('set contour surface')
    case('both')
       call self%add_str('set contour both')
    case default
       stop 'unrecognized contour option !'
    end select
  end subroutine set_cntr
  
  subroutine set_pm3d(self, pm3d)
    class(gnuplot), intent(inout) :: self
    character(len=*),  intent(in) :: pm3d
    
    select case(trim(pm3d))
    case('t')
       call self%add_str('set pm3d at t')
    case('s')
       call self%add_str('set pm3d at s')
    case('b')
       call self%add_str('set pm3d at b')
    case default
       stop 'unrecognized pm3d option !'
    end select
  end subroutine set_pm3d

  subroutine set_pause(self)
    class(gnuplot), intent(inout) :: self
    character(len=10) :: f_str

    if (self%timeout < 0.d0) then
       call self%add_str('pause -1 "press RETURN to continue"')
    else
       write(f_str, '(F10.3)') self%timeout
       call self%add_str('pause '//trim(f_str))
    end if
  end subroutine set_pause

  subroutine excute(self)
    class(gnuplot), intent(inout) :: self
    integer :: status, system

    call self%set_pause()
    call self%add_str('q')

    status = system('gnuplot -e '' '//self%str//' '' ')
    if (status /= 0) stop ' Fatal error in running gnuplot !'

    if (.not. self%save_file) then
       status = system('rm '//trim(self%dat_file_name))
       if (status /= 0) stop 'Fatal error in running rm !'
    end if
  end subroutine excute

  subroutine free(self)
    class(gnuplot), intent(inout) :: self

    deallocate(self%str)
    deallocate(self%dat_file_name)
  end subroutine free

! --- subroutines for derived class, gp_plot
  subroutine set_config_plot(self, terminal, persist, title, filename, &
       & polar, xrange1, xrange2, yrange1, yrange2)
    class(gp_plot), intent(inout) :: self
    character(len=*),  intent(in) :: terminal, persist, title, filename
    logical,  intent(in) :: polar
    real(dp), intent(in) :: xrange1, xrange2, yrange1, yrange2
    character(len=20) :: x1, x2, y1, y2

    self%first_plot = .true.

    call self%set_term(terminal, persist, title, filename)
    
    call self%set_range(xrange1, xrange2, yrange1, yrange2)

    call self%add_str('unset key')

    if (polar) then
       call self%add_str('set size square')
       call self%add_str('set polar')
       call self%add_str('set grid polar')
    else
       call self%add_str('set grid')
    end if
  end subroutine set_config_plot

  subroutine add_plot_2d(self, index, plot_type, line_color, &
       & line_type, line_width, point_type, point_size)
    class(gp_plot), intent(inout) :: self
    character(len=*),  intent(in) :: index
    character(len=*),  intent(in), optional :: plot_type, &
         & line_color, line_type, line_width, &
         & point_type, point_size
    character(len=20) :: my_plt, my_l_c, my_l_t, my_l_w, &
         & my_p_t, my_p_s
    character(len=10) :: plot_handle

    plot_handle = 'replot'
    if (self%first_plot) then
       plot_handle = 'plot'
       self%first_plot = .false.
    end if

    my_plt = default_plot_type
    my_l_c = default_line_color
    my_l_t = default_line_type
    my_l_w = default_line_width
    my_p_t = default_point_type
    my_p_s = default_point_size

    if (present(plot_type))  my_plt = plot_type
    if (present(line_color)) my_l_c = line_color
    if (present(line_type))  my_l_t = line_type
    if (present(line_width)) my_l_w = line_width
    if (present(point_type)) my_p_t = point_type
    if (present(point_size)) my_p_s = point_size

    call self%add_str(plot_handle//' "'//trim(self%dat_file_name)// &
         & '" u '//trim(index)//' w '//trim(my_plt)// &
         & ' lt '//trim(my_l_t)//' lw '//trim(my_l_w)// &
         & ' pt '//trim(my_p_t)//' ps '//trim(my_p_s)// &
         & ' lc rgb '//trim(my_l_c))

  end subroutine add_plot_2d

  subroutine free_gp_plot(self)
    type(gp_plot), intent(inout) :: self

    call self%free
  end subroutine free_gp_plot

! --- subroutines for derived class, gp_image
  subroutine set_config_image(self, terminal, persist, title, filename, &
       & xlabel, ylabel, palette, xrange1, xrange2, yrange1, yrange2)
    class(gp_image), intent(inout) :: self
    character(len=*) :: terminal, persist, title, filename, &
         & xlabel, ylabel, palette
    real(dp) :: xrange1, xrange2, yrange1, yrange2
    
    call self%set_term(terminal, persist, title, filename)
    
    call self%set_range(xrange1, xrange2, yrange1, yrange2)
    
    call self%set_label(xlabel, ylabel)
    
    call self%set_palet(palette)
    
    call self%add_str('set pm3d map')
    call self%add_str('unset key')
    call self%add_str('set colorbox')
    call self%add_str('set tmargin at screen 0.92')
    call self%add_str('set bmargin at screen 0.10')
  end subroutine set_config_image
  
  subroutine add_plot_3d(self, handle, index, style)
    class(gp_image), intent(inout) :: self
    character(len=*),   intent(in) :: handle, index, style
    
    call self%add_str(trim(handle)//' "'//trim(self%dat_file_name)// &
         & '" using '//trim(index)//' with '//trim(style))
  end subroutine add_plot_3d
  
  subroutine free_gp_image(self)
    type(gp_image), intent(inout) :: self

    call self%free
  end subroutine free_gp_image

! --- miscellaneous routines and functions
  function term_type(terminal) result(f_result)
    implicit none
    character(len=*), intent(in) :: terminal
    character(len=35)            :: f_result

    select case(trim(terminal))
    case('ps')
       f_result = 'postscript eps enhanced color'
    case default
       f_result = terminal//' enhanced'
    end select
  end function term_type

end module class_gnuplot
