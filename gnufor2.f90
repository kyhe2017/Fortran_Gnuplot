! Author: Alexey Kuznetsov
! Modified: 28/12/2008
! this Fortran90 module contains a collection of subroutines for plotting data, 
! including 2D, 3D plots, surfaces, polar coordinates, histograms
! it is a modification of the GNUFOR interface written by John Burkardt:
! http://orion.math.iastate.edu/burkardt/g_src/gnufor/gnufor.html 
! ------------------------------------------------------------------------------ 
!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, version 3 of the License.

!    This program is distributed in the hope that it will be useful, 
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!    <http://www.gnu.org/licenses/>.
! ------------------------------------------------------------------------------ 
! Modified by Kaiyang He, 08/07/2017.
! Email: kyhe@ipp.ac.cn
! ------------------------------------------------------------------------------
module gnufor2

  use class_gnuplot

  implicit none

  private

  character(len=3),   parameter :: default_linewidth='1'
  character(len=100), parameter :: default_color1='blue'
  character(len=100), parameter :: default_color2='dark-green'
  character(len=100), parameter :: default_color3='orange-red'
  character(len=100), parameter :: default_color4='dark-salmon'
  character(len=100), parameter :: default_terminal='x11'
  character(len=100), parameter :: default_palette='CMY'

  public :: plot, surf, image, hist, plot3d

  interface plot
     module procedure plot_1
     module procedure plot_2
     module procedure plot_3
     module procedure plot_4
  end interface plot

  interface surf
     module procedure surf_1
     module procedure surf_2
     module procedure surf_3
  end interface surf

  interface image
     module procedure image_1
     module procedure image_2
     module procedure image_3
     module procedure image_4
     module procedure image_5
  end interface image

contains

  function my_date_and_time() result(f_result)
    implicit none
    character(len=8)  :: date
    character(len=10) :: time
    character(len=33) :: f_result

    call date_and_time(date, time)
    f_result = 'date_' &
         & //date(7:8)//'-'//date(5:6)//'-'//date(1:4) &
         & //'_time_' &
         & //time(1:2)//':'//time(3:4)//':'//time(5:10)
  end function my_date_and_time

  subroutine image_5(x, y, rgb, pause, terminal, filename, persist, input, title)
    implicit none
    real(dp), intent(in) :: x(:), y(:)
    integer,  intent(in) :: rgb(:,:,:)
    real(dp), optional   :: pause
    character(len=*), optional :: terminal, filename, persist, input, title
    integer :: nx, ny, i, j, dat_unit
    character(len=100) :: dat_file_name, cmd_file_name, my_pause, my_terminal, &
         & my_persist, my_title, my_filename
    logical :: save_file
    real(dp) :: timeout, xrange1, xrange2, yrange1, yrange2
    type(gp_image), pointer :: gg

    nx = size(rgb(1,:,1))
    ny = size(rgb(1,1,:))
    if ((size(x)/=nx) .or. (size(y)/=ny)) then
       print*, 'image ERROR: sizes of x(:), y(:) and gray(:,:) are not compatible'
       stop
    end if
    
    do j = 1, ny
       do i = 1, nx
          if ((maxval(rgb(:,i,j))>255) .or. (minval(rgb(:,i,j))<0)) then
             print*, 'image ERROR: a value of rgb(:,:,:) array is outside [0,255]'
             stop
          end if
       end do
    end do

    if (present(input)) then
       dat_file_name = 'dat_file_'//trim(input)//'.dat'
       cmd_file_name = 'cmd_file_'//trim(input)//'.plt'
    else
       dat_file_name = 'dat_file.dat'
       cmd_file_name = 'cmd_file.plt'
    end if

    call chk_unit(dat_file_name, 'image5_dat', dat_unit)

    write(dat_unit, '(2E12.4, 3I5)') ((x(i), y(j), rgb(1,i,j), rgb(2,i,j), rgb(3,i,j), &
         & i = 1, nx), j = 1, ny)

    close(unit=dat_unit, status='keep')

    save_file = .false.
    if (present(input)) save_file = .true.
    timeout = 0.d0
    if (present(pause)) timeout = pause
    my_terminal = default_terminal
    if (present(terminal)) my_terminal = terminal
    my_persist = 'persist'
    if (present(persist) .and. (persist=='no')) my_persist = ''
    my_title = 'Gnuplot'
    if (present(title) .and. (title/='')) my_title = title
    my_filename = my_date_and_time()
    if (present(filename)) my_filename = filename
    
    xrange1 = minval(x)
    xrange2 = maxval(x)
    yrange1 = minval(y)
    yrange2 = maxval(y)
    
    allocate(gg)
    
    call gg%init(dat_file_name, cmd_file_name, save_file, timeout)
    
    call gg%set_term(my_terminal, my_persist, my_title, my_filename)
    
    call gg%set_range(xrange1, xrange2, yrange1, yrange2)

    call gg%add_str('unset key')
    call gg%add_str('unset colorbox')
    call gg%add_str('plot "'//trim(gg%dat_file_name)//'" with rgbimage')

    call gg%excute()
    
    deallocate(gg)

  end subroutine image_5

  subroutine image_4(rgb, pause, terminal, filename, persist, input, title) 
    implicit none
    integer, intent(in) :: rgb(:,:,:)
    real(dp), optional  :: pause
    character(len=*), optional :: terminal, filename, persist, input, title
    integer :: nx, ny, i, j, dat_unit
    character(len=100) :: dat_file_name, cmd_file_name, my_terminal, &
         & my_persist, my_title, my_filename
    logical  :: save_file
    real(dp) :: timeout
    type(gp_image), pointer :: gg

    nx = size(rgb(1,:,1))
    ny = size(rgb(1,1,:))
    do j = 1, ny
       do i = 1, nx
          if ((maxval(rgb(:,i,j))>255) .or. (minval(rgb(:,i,j))<0)) then
             stop 'image ERROR: a value of rgb(:,:,:) array is outside [0,255]'
          end if
       end do
    end do

    if (present(input)) then
       dat_file_name = 'dat_file_'//trim(input)//'.dat'
       cmd_file_name = 'cmd_file_'//trim(input)//'.plt'
    else
       dat_file_name = 'dat_file.dat'
       cmd_file_name = 'cmd_file.plt'
    end if

    call chk_unit(dat_file_name, 'image4_dat', dat_unit)

    write(dat_unit, '(5I5)') ((i, j, rgb(1,i,j), rgb(2,i,j), rgb(3,i,j), &
         & i = 1, nx), j = 1, ny)

    close (unit=dat_unit, status='keep')
    
    save_file = .false.
    if (present(input)) save_file = .true.
    timeout = 0.d0
    if (present(pause)) timeout = pause
    my_terminal = default_terminal
    if (present(terminal)) my_terminal = terminal
    my_persist = 'persist'
    if (present(persist) .and. (persist=='no')) my_persist = ''
    my_title = 'Gnuplot'
    if (present(title) .and. (title/='')) my_title = title
    my_filename = my_date_and_time()
    if (present(filename)) my_filename = filename
    
    allocate(gg)
    
    call gg%init(dat_file_name, cmd_file_name, save_file, timeout)
    
    call gg%set_term(my_terminal, my_persist, my_title, my_filename)

    call gg%add_str('unset key')
    call gg%add_str('unset border')
    call gg%add_str('unset xtics')
    call gg%add_str('unset ytics')
    call gg%add_str('unset colorbox')
    call gg%add_str('plot "'//trim(gg%dat_file_name)//'" with rgbimage')
    
    call gg%excute()
    
    deallocate(gg)

  end subroutine image_4

  subroutine image_3(x, y, gray, pause, palette, terminal, filename, persist, input, &
       & title, xlabel, ylabel)
    implicit none
    real(dp), intent(in) :: x(:,:), y(:,:), gray(:,:)
    real(dp), optional   :: pause
    character(len=*), optional :: palette, terminal, filename, persist, input, &
         & title, xlabel, ylabel
    integer :: nx, ny, i, j, dat_unit
    character(len=100) :: dat_file_name, cmd_file_name, my_terminal, my_persist, &
         & my_title, my_filename, my_xlabel, my_ylabel, my_palette
    logical  :: save_file
    real(dp) :: timeout, xrange1, xrange2, yrange1, yrange2
    type(gp_image), pointer :: gg
    
    nx = size(gray(:,1))
    ny = size(gray(1,:))
    if ((size(x(:,1))/=nx) .or. (size(y(:,1))/=nx) .or. &
         & (size(x(1,:))/=ny) .or. (size(y(1,:))/=ny)) then
       stop 'image ERROR: sizes of x(:,:), y(:,:) and gray(:,:) are not compatible'
    end if

    if (present(input)) then
       dat_file_name = 'dat_file_'//trim(input)//'.dat'
       cmd_file_name = 'cmd_file_'//trim(input)//'.plt'
    else
       dat_file_name = 'dat_file.dat'
       cmd_file_name = 'cmd_file.plt'
    end if

    call chk_unit(dat_file_name, 'image3_dat', dat_unit)

    do j = 1, ny
       write(dat_unit, '(3E12.4)') (x(i,j), y(i,j), gray(i,j), i = 1, nx)
       write(dat_unit, '(a)')
    end do

    close(unit=dat_unit, status='keep')

    save_file = .false.
    if (present(input)) save_file = .true.
    timeout = 0.d0
    if (present(pause)) timeout = pause
    my_terminal = default_terminal
    if (present(terminal)) my_terminal = terminal
    my_persist = 'persist'
    if (present(persist) .and. (persist=='no')) my_persist = ''
    my_title = 'Gnuplot'
    if (present(title) .and. (title/='')) my_title = title
    my_filename = my_date_and_time()
    if (present(filename)) my_filename = filename
    my_xlabel  = 'x'
    if (present(xlabel) .and. (xlabel/='')) my_xlabel = xlabel
    my_ylabel  = 'y'
    if (present(ylabel) .and. (ylabel/='')) my_ylabel = ylabel
    my_palette = default_palette
    if (present(palette)) my_palette = palette
    
    xrange1 = minval(x)
    xrange2 = maxval(x)
    yrange1 = minval(y)
    yrange2 = maxval(y)
    
    allocate(gg)
    
    call gg%init(dat_file_name, cmd_file_name, save_file, timeout)
    
    call gg%set_config(my_terminal, my_persist, my_title, my_filename, &
       & my_xlabel, my_ylabel, my_palette, &
       & xrange1, xrange2, yrange1, yrange2)
    
    call gg%add_plot('splot', '1:2:3', 'pm3d')

    call gg%excute()
    
    deallocate(gg)

  end subroutine image_3

  subroutine image_2(x, y, gray, pause, palette, terminal, filename, persist, input, &
       & title, xlabel, ylabel)
    implicit none
    real(dp), intent(in) :: x(:), y(:), gray(:,:)
    real(dp), optional   :: pause
    character(len=*), optional :: palette, terminal, filename, persist, input, &
         & title, xlabel, ylabel
    integer :: nx, ny, i, j, dat_unit
    character(len=100) :: dat_file_name, cmd_file_name, my_terminal, my_persist, &
         & my_title, my_filename, my_xlabel, my_ylabel, my_palette
    logical  :: save_file
    real(dp) :: timeout, xrange1, xrange2, yrange1, yrange2
    type(gp_image), pointer :: gg
    
    nx = size(gray(:,1))
    ny = size(gray(1,:))
    if ((size(x)/=nx) .or. (size(y)/=ny)) &
        & stop 'image ERROR: sizes of x(:), y(:) and gray(:,:) are not compatible'

    if (present(input)) then
       dat_file_name = 'dat_file_'//trim(input)//'.dat'
       cmd_file_name = 'cmd_file_'//trim(input)//'.plt'
    else
       dat_file_name = 'dat_file.dat'
       cmd_file_name = 'cmd_file.plt'
    end if

    call chk_unit(dat_file_name, 'image2_dat', dat_unit)

    do j = 1, ny
       write(dat_unit, '(3E12.4)') (x(i), y(j), gray(i,j), i = 1, nx)
       write(dat_unit, '(a)')
    end do

    close(unit=dat_unit, status='keep')
    
    save_file = .false.
    if (present(input)) save_file = .true.
    timeout = 0.d0
    if (present(pause)) timeout = pause
    my_terminal = default_terminal
    if (present(terminal)) my_terminal = terminal
    my_persist = 'persist'
    if (present(persist) .and. (persist=='no')) my_persist = ''
    my_title = 'Gnuplot'
    if (present(title) .and. (title/='')) my_title = title
    my_filename = my_date_and_time()
    if (present(filename)) my_filename = filename
    my_xlabel  = 'x'
    if (present(xlabel) .and. (xlabel/='')) my_xlabel = xlabel
    my_ylabel  = 'y'
    if (present(ylabel) .and. (ylabel/='')) my_ylabel = ylabel
    my_palette = default_palette
    if (present(palette)) my_palette = palette
    
    xrange1 = minval(x)
    xrange2 = maxval(x)
    yrange1 = minval(y)
    yrange2 = maxval(y)
    
    allocate(gg)
    
    call gg%init(dat_file_name, cmd_file_name, save_file, timeout)
    
    call gg%set_config(my_terminal, my_persist, my_title, my_filename, &
       & my_xlabel, my_ylabel, my_palette, &
       & xrange1, xrange2, yrange1, yrange2)
    
    call gg%add_plot('splot', '1:2:3', 'pm3d')
    
    call gg%excute()
    
    deallocate(gg)

  end subroutine image_2

  subroutine image_1(gray, pause, palette, terminal, filename, persist, input, &
       & title)
    implicit none
    real(dp), intent(in) :: gray(:,:)
    real(dp), optional   :: pause
    character(len=*), optional :: palette, terminal, filename, persist, input, &
         & title
    integer  :: nx, ny, i, j, dat_unit
    character(len=100) :: dat_file_name, cmd_file_name, my_terminal, my_persist, &
         & my_title, my_filename, my_palette
    logical  :: save_file
    real(dp) :: timeout
    type(gp_image), pointer :: gg

    nx = size(gray(:,1))
    ny = size(gray(1,:))

    if (present(input)) then
       dat_file_name = 'dat_file_'//trim(input)//'.dat'
       cmd_file_name = 'cmd_file_'//trim(input)//'.plt'
    else
       dat_file_name = 'dat_file.dat'
       cmd_file_name = 'cmd_file.plt'
    end if

    call chk_unit(dat_file_name, 'image1_dat', dat_unit)

    write(dat_unit, '(I5, I5, E15.7)') ((i, j, gray(i,j), i = 1, nx), j = 1, ny)

    close(unit=dat_unit, status='keep')

    save_file = .false.
    if (present(input)) save_file = .true.
    timeout = 0.d0
    if (present(pause)) timeout = pause
    my_terminal = default_terminal
    if (present(terminal)) my_terminal = terminal
    my_persist = 'persist'
    if (present(persist) .and. (persist=='no')) my_persist = ''
    my_title = 'Gnuplot'
    if (present(title) .and. (title/='')) my_title = title
    my_filename = my_date_and_time()
    if (present(filename)) my_filename = filename
    my_palette = default_palette
    if (present(palette)) my_palette = palette
    
    allocate(gg)
    
    call gg%init(dat_file_name, cmd_file_name, save_file, timeout)
    
    call gg%set_term(my_terminal, my_persist, my_title, my_filename)

    call gg%add_str('unset key')
    call gg%add_str('unset border')
    call gg%add_str('unset xtics')
    call gg%add_str('unset ytics')
    call gg%add_str('unset colorbox')

    call gg%set_palet(my_palette)
    
    call gg%add_str('plot "'//trim(gg%dat_file_name)//'" with image')

    call gg%excute()
    
    deallocate(gg)

  end subroutine image_1

  subroutine plot3d(x, y, z, pause, color, terminal, filename, persist, input, title, linewidth)
    implicit none
    real(dp), intent(in) :: x(:), y(:), z(:)
    real(dp), optional   :: pause, linewidth
    character(len=*), optional :: color, terminal, filename, persist, input, title
    integer :: i, nx, dat_unit
    character(len=100) :: dat_file_name, cmd_file_name, my_terminal, &
         & my_persist, my_title, my_filename, my_linewidth, my_color
    logical :: save_file
    real(dp) :: timeout
    type(gp_image), pointer :: gg
    
    nx = size(x)
    if ((size(x)/=size(y)) .or. (size(x)/=size(z))) &
         & stop 'subroutine plot3d ERROR: incompatible sizes of x(:), y(:) and z(:)'

    if (present(input)) then
       dat_file_name = 'dat_file_'//trim(input)//'.dat'
       cmd_file_name = 'cmd_file_'//trim(input)//'.plt'
    else
       dat_file_name = 'dat_file.dat'
       cmd_file_name = 'cmd_file.plt'
    end if

    call chk_unit(dat_file_name, 'plot3d_dat', dat_unit)

    write(dat_unit, '(3E15.7)') (x(i), y(i), z(i), i = 1, nx)

    close(unit=dat_unit, status='keep')

    save_file = .false.
    if (present(input)) save_file = .true.
    timeout = 0.d0
    if (present(pause)) timeout = pause
    my_terminal = default_terminal
    if (present(terminal)) my_terminal = terminal
    my_persist = 'persist'
    if (present(persist) .and. (persist=='no')) my_persist = ''
    my_title = 'Gnuplot'
    if (present(title) .and. (title/='')) my_title = title
    my_filename = my_date_and_time()
    if (present(filename)) my_filename = filename

    if (present(linewidth)) then
       write(my_linewidth, '(e10.3)') linewidth
    else
       my_linewidth = trim(default_linewidth)
    end if
    if (present(color)) then
       my_color = '"'//trim(color)//'"'
    else
       my_color = '"'//trim(default_color1)//'"'
    end if
    
    allocate(gg)
    
    call gg%init(dat_file_name, cmd_file_name, save_file, timeout)
    
    call gg%set_term(my_terminal, my_persist, my_title, my_filename)
    
    call gg%add_str('unset key')
    call gg%add_str('splot "'//trim(gg%dat_file_name)// &
         & '" u 1:2:3 w l lc rgb '//trim(my_color)// &
         & ' lw '//trim(my_linewidth))
    
    call gg%excute()
    
    deallocate(gg)

  end subroutine plot3d

  subroutine hist(x, n, pause, color, terminal, filename, persist, input, title)
    implicit none
    real(dp), intent(in) :: x(:) !the data to plot
    integer,  intent(in) :: n    !the number of intervals
    real(dp), optional   :: pause
    character(len=*), optional :: color, terminal, filename, persist, input, title
    integer :: i, j, dat_unit, nx
    character(len=100) :: dat_file_name, cmd_file_name, xtic_start, dxtic, xtic_end, &
         & my_terminal, my_persist, my_title, my_filename, my_color
    logical  :: save_file
    real(dp) :: xmin, xmax, xhist(0:n), yhist(n+1), dx, yrange, xrange1, xrange2, timeout
    type(gp_plot), pointer :: gg

    nx   = size(x)
    xmin = minval(x)
    xmax = maxval(x)
    dx   = (xmax-xmin)/n
    do i = 0, n
       xhist(i) = xmin+i*dx
    end do
    yhist = 0.0d0
    do i = 1, nx
       j = floor((x(i)-xmin)/dx)+1
       yhist(j) = yhist(j)+1
    end do

    write(dxtic, '(e15.7)') dx
    yrange = maxval(yhist)*1.2
    xrange1 = xmin-(n/10.0)*dx
    xrange2 = xmax+(n/10.0)*dx
    write(xtic_start, '(e15.7)') xrange1
    write(xtic_end,   '(e15.7)') xrange2

    if (present(input)) then
       dat_file_name = 'dat_file_'//trim(input)//'.dat'
       cmd_file_name = 'cmd_file_'//trim(input)//'.plt'
    else
       dat_file_name = 'dat_file.dat'
       cmd_file_name = 'cmd_file.plt'
    end if

    call chk_unit(dat_file_name, 'hist_dat', dat_unit)

    write(dat_unit, '(2E15.7)') ((xhist(i-1)+0.5*dx), yhist(i), i = 1, n)

    close(unit=dat_unit, status='keep')

    save_file = .false.
    if (present(input)) save_file = .true.
    timeout = 0.d0
    if (present(pause)) timeout = pause
    my_terminal = default_terminal
    if (present(terminal)) my_terminal = terminal
    my_persist = 'persist'
    if (present(persist) .and. (persist=='no')) my_persist = ''
    my_title = 'Gnuplot'
    if (present(title) .and. (title/='')) my_title = title
    my_filename = my_date_and_time()
    if (present(filename)) my_filename = filename
    
    if (present(color)) then
       my_color = '"'//color//'"'
    else
       my_color = '"'//trim(default_color1)//'"'
    end if
    
    allocate(gg)
    
    call gg%init(dat_file_name, cmd_file_name, save_file, timeout)
    
    call gg%set_term(my_terminal, my_persist, my_title, my_filename)
    
    call gg%set_range(xrange1, xrange2, 0.d0, yrange)

    call gg%add_str('unset key')
    call gg%add_str('set xtic nomirror rotate by -45 ')
    call gg%add_str('set xtics '//trim(xtic_start)//', '//trim(dxtic)//', '//trim(xtic_end))
    call gg%add_str('set style data histograms')
    call gg%add_str('set style fill solid border -1')
    
    call gg%add_str('plot "'//trim( dat_file_name ) &
         & //'" using 1:2 with boxes linecolor rgb '//trim(my_color))

    call gg%excute()
    
    deallocate(gg)

  end subroutine hist

  subroutine surf_3(x, y, z, pause, palette, terminal, filename, pm3d, contour, persist, &
       & input, title)
    implicit none
    real(dp), intent(in) :: x(:), y(:), z(:,:)
    real(dp), optional   :: pause
    real(dp)             :: xyz(3, size(z(:,1)), size(z(1,:)))
    character(len=*), optional :: palette, terminal, filename, pm3d, contour, persist, &
         & input, title
    integer :: nx, ny
    integer :: i, j

    nx = size(z(:,1))
    ny = size(z(1,:))
    if ((size(x)/=nx) .or. (size(y)/=ny)) then
       print*, 'subroutine surf_3 ERROR: sizes of x(:), y(:), and z(:,:) are incompatible'
       stop
    end if

    do i = 1, nx
       do j = 1, ny
          xyz(1,i,j) = x(i)
          xyz(2,i,j) = y(j)
          xyz(3,i,j) = z(i,j)
       end do
    end do
    call surf_1(xyz, pause, palette, terminal, filename, pm3d, contour, persist, input, title)

  end subroutine surf_3

  subroutine surf_2(z, pause, palette, terminal, filename, pm3d, contour, persist, &
       & input, title)
    implicit none
    real(dp), intent(in) :: z(:,:)
    real(dp), optional   :: pause
    real(dp)             :: xyz(3,size(z(:,1)),size(z(1,:)))
    character(len=*), optional :: palette, terminal, filename, pm3d, contour, persist, &
         & input, title
    integer :: nx, ny
    integer :: i, j

    nx = size(z(:,1))
    ny = size(z(1,:))
    do i = 1, nx
       do j = 1, ny
          xyz(1,i,j) = dble(i)
          xyz(2,i,j) = dble(j)
          xyz(3,i,j) = z(i,j)
       end do
    end do
    call surf_1(xyz, pause, palette, terminal, filename, pm3d, contour, persist, input, title)

  end subroutine surf_2

  subroutine surf_1(xyz, pause, palette, terminal, filename, pm3d, contour, persist, &
       & input, title)
    implicit none
    real(dp), intent(in) :: xyz(:,:,:)
    real(dp), optional   :: pause
    character(len=*), optional :: palette, terminal, filename, pm3d, contour, persist, &
         & input, title
    integer            :: nx, ny, nrow, i, j, dat_unit
    character(len=100) :: dat_file_name, cmd_file_name,my_terminal, my_persist, &
         & my_title, my_filename, my_palette
    logical  :: save_file
    real(dp) :: timeout
    type(gp_image), pointer :: gg

    nx = size(xyz(1,:,1))
    ny = size(xyz(1,1,:))
    nrow = nx*ny

    if (present(input)) then
       dat_file_name = 'dat_file_'//trim(input)//'.dat'
       cmd_file_name = 'cmd_file_'//trim(input)//'.plt'
    else
       dat_file_name = 'dat_file.dat'
       cmd_file_name = 'cmd_file.plt'
    end if

    call chk_unit(dat_file_name, 'surf_dat', dat_unit)

    do j = 1, ny
       write(dat_unit, '(3E15.7)') (xyz(1:3,i,j), i = 1, nx)
       write(dat_unit, '(a)')
    end do

    close(unit=dat_unit, status='keep')

    save_file = .false.
    if (present(input)) save_file = .true.
    timeout = 0.d0
    if (present(pause)) timeout = pause
    my_terminal = default_terminal
    if (present(terminal)) my_terminal = terminal
    my_persist = 'persist'
    if (present(persist) .and. (persist=='no')) my_persist = ''
    my_title = 'Gnuplot'
    if (present(title) .and. (title/='')) my_title = title
    my_filename = my_date_and_time()
    if (present(filename)) my_filename = filename
    my_palette = default_palette
    if (present(palette)) my_palette = palette
    
    allocate(gg)
    
    call gg%init(dat_file_name, cmd_file_name, save_file, timeout)
    
    call gg%set_term(my_terminal, my_persist, my_title, my_filename)

    call gg%set_palet(my_palette)
    
    if (present(pm3d)) then
       call gg%set_pm3d(pm3d)
    else
       call gg%add_str('set surface')
       if (present(contour)) then
          call gg%set_cntr(contour)
       end if
    end if
    call gg%add_str('set hidden3d')
    call gg%add_str('set parametric')
    call gg%add_str('set ticslevel 0')
    call gg%add_str('unset key')
    
    call gg%add_plot('splot', '1:2:3', 'lines palette')

    call gg%excute()
    
    deallocate(gg)

  end subroutine surf_1

  subroutine plot_4(x1, y1, x2, y2, x3, y3, x4, y4, style, pause, &
       & color1, color2, color3, color4, terminal, filename, polar, &
       & persist, input, title, linewidth)
    implicit none
    real(dp), intent(in) :: x1(:), y1(:), x2(:), y2(:), x3(:), y3(:), x4(:), y4(:)
    real(dp), optional   :: pause, linewidth
    character(len=*), optional :: style, color1, color2, color3, color4, &
         & terminal, filename, polar, persist, input, title
    integer :: i, dat_unit, Nx1, Nx2, Nx3, Nx4, Nmax
    character(len=100) :: dat_file_name, cmd_file_name, my_linewidth
    character(len=20)  :: my_line_type1, my_line_type2, my_line_type3, my_line_type4, &
         & my_color1, my_color2, my_color3, my_color4, &
         & my_terminal, my_persist, my_title, my_filename
    logical :: save_file, my_polar
    real(dp) :: timeout, xrange1, xrange2, yrange1, yrange2
    type(gp_plot), pointer :: gg

    Nx1 = size(x1)
    Nx2 = size(x2)
    Nx3 = size(x3)
    Nx4 = size(x4)
    if ((Nx1/=size(y1)) .or. (Nx2/=size(y2)) .or. (Nx3/=size(y3)) .or. (Nx4/=size(y4))) &
         & stop 'subroutine plot ERROR: size(x) is not equal to size(y)'
    if (present(style) .and. (len(style)/=12)) &
         & stop 'subroutine plot ERROR: argument "style" has wrong number of characters'

    if (present(input)) then
       dat_file_name = 'dat_file_'//trim(input)//'.dat'
       cmd_file_name = 'cmd_file_'//trim(input)//'.plt'
    else
       dat_file_name = 'dat_file.dat'
       cmd_file_name = 'cmd_file.plt'
    end if

    call chk_unit(dat_file_name, 'plot4_dat', dat_unit)

    Nmax = max(Nx1, Nx2, Nx3, Nx4)	
    do i = 1, Nmax
       write(dat_unit, '(8E15.7)') x1(min(i, Nx1)), y1(min(i, Nx1)), &
            & x2(min(i, Nx2)), y2(min(i, Nx2)), x3(min(i, Nx3)), y3(min(i, Nx3)), &
            & x4(min(i, Nx4)), y4(min(i, Nx4))
    end do

    close(unit=dat_unit, status='keep')

    save_file = .false.
    if (present(input)) save_file = .true.
    timeout = 0.d0
    if (present(pause)) timeout = pause
    my_terminal = default_terminal
    if (present(terminal)) my_terminal = terminal
    my_persist = 'persist'
    if (present(persist) .and. (persist=='no')) my_persist = ''
    my_title = 'Gnuplot'
    if (present(title) .and. (title/='')) my_title = title
    my_filename = my_date_and_time()
    if (present(filename)) my_filename = filename
    my_polar = .false.
    if (present(polar).and. (polar=='yes')) my_polar = .true.

    xrange1 = min(minval(x1), minval(x2), minval(x3), minval(x4))
    xrange2 = max(maxval(x1), maxval(x2), maxval(x3), maxval(x4))
    yrange1 = min(minval(y1), minval(y2), minval(y3), minval(y4))
    yrange2 = max(maxval(y1), maxval(y2), maxval(y3), maxval(y4))

    if (my_polar) then
       xrange1 = -max(maxval(abs(y1)), maxval(abs(y2)), maxval(abs(y3)), maxval(abs(y4)))
       xrange2 = -xrange1
       yrange1 =  xrange1
       yrange2 = -xrange1
    end if

    my_line_type1 = 'lines'
    if (present(style)) then
       if ((style(3:3) == '-')) then
          my_line_type1 = 'linespoints'
       else
          my_line_type1 = 'points'
       end if
    end if
    my_line_type2 = 'lines'
    if (present(style)) then
       if ((style(6:6) == '-')) then
          my_line_type2 = 'linespoints'
       else
          my_line_type2 = 'points'
       end if
    end if
    my_line_type3 = 'lines'
    if (present(style)) then
       if ((style(9:9) == '-')) then
          my_line_type3 = 'linespoints'
       else
          my_line_type3 = 'points'
       end if
    end if
    my_line_type4 = 'lines'
    if (present(style)) then
       if ((style(12:12) == '-')) then
          my_line_type4 = 'linespoints'
       else
          my_line_type4 = 'points'
       end if
    end if
    if (present(linewidth)) then
       write(my_linewidth, '(e10.3)') linewidth
    else
       my_linewidth = trim(default_linewidth)
    end if
    if (present(color1)) then
       my_color1 = '"'//trim(color1)//'"'
    else
       my_color1 = '"'//trim(default_color1)//'"'
    end if
    if (present(color2)) then
       my_color2 = '"'//trim(color2)//'"'
    else
       my_color2 = '"'//trim(default_color2)//'"'
    end if
    if (present(color3)) then
       my_color3 = '"'//trim(color3)//'"'
    else
       my_color3 = '"'//trim(default_color3)//'"'
    end if
    if (present(color4)) then
       my_color4 = '"'//trim(color4)//'"'
    else
       my_color4 = '"'//trim(default_color4)//'"'
    end if

    allocate(gg)
    
    call gg%init(dat_file_name, cmd_file_name, save_file, timeout)
    
    call gg%set_config(my_terminal, my_persist, my_title, my_filename, my_polar, &
       & xrange1, xrange2, yrange1, yrange2)
    
    call gg%add_plot(index='1:2', plot_type=my_line_type1, line_color=my_color1, &
       & line_width=my_linewidth, point_type=style(1:2))
    call gg%add_plot(index='3:4', plot_type=my_line_type2, line_color=my_color2, &
       & line_width=my_linewidth, point_type=style(4:5))
    call gg%add_plot(index='5:6', plot_type=my_line_type3, line_color=my_color3, &
       & line_width=my_linewidth, point_type=style(7:8))
    call gg%add_plot(index='7:8', plot_type=my_line_type4, line_color=my_color4, &
       & line_width=my_linewidth, point_type=style(10:11))
       
    call gg%excute()
    
    deallocate(gg)

  end subroutine plot_4

  subroutine plot_3(x1, y1, x2, y2, x3, y3, style, pause, &
       & color1, color2, color3, terminal, filename, &
       & polar, persist, input, title, linewidth)
    implicit none
    real(dp), intent(in) :: x1(:), y1(:), x2(:), y2(:), x3(:), y3(:)
    real(dp), optional   :: pause, linewidth
    character(len=*), optional :: style, color1, color2, color3, &
         & terminal, filename, polar, persist, input, title
    integer :: i, dat_unit, Nx1, Nx2, Nx3, Nmax
    character(len=100) :: dat_file_name, cmd_file_name, my_linewidth
    character(len=20)  :: my_line_type1, my_line_type2, my_line_type3, &
         & my_color1, my_color2, my_color3, &
         & my_terminal, my_persist, my_title, my_filename
    logical :: save_file, my_polar
    real(dp) :: timeout, xrange1, xrange2, yrange1, yrange2
    type(gp_plot), pointer :: gg
    
    Nx1 = size(x1)
    Nx2 = size(x2)
    Nx3 = size(x3)
    if ((Nx1/=size(y1)) .or. (Nx2/=size(y2)) .or. (Nx3/=size(y3))) &
         & stop 'subroutine plot ERROR: size(x) is not equal to size(y)'
    if (present(style) .and. (len(style)/=9)) &
         & stop 'subroutine plot ERROR: argument "style" has wrong number of characters'

    if (present(input)) then
       dat_file_name = 'dat_file_'//trim(input)//'.dat'
       cmd_file_name = 'cmd_file_'//trim(input)//'.plt'
    else
       dat_file_name = 'dat_file.dat'
       cmd_file_name = 'cmd_file.plt'
    end if

    call chk_unit(dat_file_name, 'plot3_dat', dat_unit)

    Nmax = max(Nx1, Nx2, Nx3)	
    do i = 1, Nmax
       write(dat_unit, '(6E15.7)') x1(min(i, Nx1)), y1(min(i, Nx1)), &
            & x2(min(i, Nx2)), y2(min(i, Nx2)), x3(min(i, Nx3)), y3(min(i, Nx3))
    end do

    close(unit=dat_unit, status='keep')
    
    save_file = .false.
    if (present(input)) save_file = .true.
    timeout = 0.d0
    if (present(pause)) timeout = pause
    my_terminal = default_terminal
    if (present(terminal)) my_terminal = terminal
    my_persist = 'persist'
    if (present(persist) .and. (persist=='no')) my_persist = ''
    my_title = 'Gnuplot'
    if (present(title) .and. (title/='')) my_title = title
    my_filename = my_date_and_time()
    if (present(filename)) my_filename = filename
    my_polar = .false.
    if (present(polar).and. (polar=='yes')) my_polar = .true.
    
    xrange1 = min(minval(x1), minval(x2), minval(x3))
    xrange2 = max(maxval(x1), maxval(x2), maxval(x3))
    yrange1 = min(minval(y1), minval(y2), minval(y3))
    yrange2 = max(maxval(y1), maxval(y2), maxval(y3))
    
    if (my_polar) then
       xrange1 = -max(maxval(abs(y1)), maxval(abs(y2)), maxval(abs(y3)))
       xrange2 = -xrange1
       yrange1 =  xrange1
       yrange2 = -xrange1
    end if

    my_line_type1 = 'lines'
    if (present(style)) then
       if ((style(3:3) == '-')) then
          my_line_type1 = 'linespoints'
       else
          my_line_type1 = 'points'
       end if
    end if
    my_line_type2 = 'lines'
    if (present(style)) then
       if ((style(6:6) == '-')) then
          my_line_type2 = 'linespoints'
       else
          my_line_type2 = 'points'
       end if
    end if
    my_line_type3 = 'lines'
    if (present(style)) then
       if ((style(9:9) == '-')) then
          my_line_type3 = 'linespoints'
       else
          my_line_type3 = 'points'
       end if
    end if
    if (present(linewidth)) then
       write(my_linewidth, '(e10.3)') linewidth
    else
       my_linewidth = trim(default_linewidth)
    end if
    if (present(color1)) then
       my_color1 = '"'//trim(color1)//'"'
    else
       my_color1 = '"'//trim(default_color1)//'"'
    end if
    if (present(color2)) then
       my_color2 = '"'//trim(color2)//'"'
    else
       my_color2 = '"'//trim(default_color2)//'"'
    end if
    if (present(color3)) then
       my_color3 = '"'//trim(color3)//'"'
    else
       my_color3 = '"'//trim(default_color3)//'"'
    end if
    
    allocate(gg)
    
    call gg%init(dat_file_name, cmd_file_name, save_file, timeout)
    
    call gg%set_config(my_terminal, my_persist, my_title, my_filename, my_polar, &
       & xrange1, xrange2, yrange1, yrange2)
    
    call gg%add_plot(index='1:2', plot_type=my_line_type1, line_color=my_color1, &
       & line_width=my_linewidth, point_type=style(1:2))
    call gg%add_plot(index='3:4', plot_type=my_line_type2, line_color=my_color2, &
       & line_width=my_linewidth, point_type=style(4:5))
    call gg%add_plot(index='5:6', plot_type=my_line_type3, line_color=my_color3, &
       & line_width=my_linewidth, point_type=style(7:8))
       
    call gg%excute()
    
    deallocate(gg)

  end subroutine plot_3

  subroutine plot_2(x1, y1, x2, y2, style, pause, color1, color2, &
       & terminal, filename, polar, persist, input, title, linewidth)
    implicit none
    real(dp), intent(in) :: x1(:), y1(:), x2(:), y2(:)
    real(dp), optional   :: pause, linewidth
    character(len=*), optional :: style, color1, color2, terminal, &
         & filename, polar, persist, input, title
    integer :: i, dat_unit, Nx1, Nx2, Nmax
    character(len=100) :: dat_file_name, cmd_file_name, my_linewidth
    character(len=20)  :: my_line_type1, my_line_type2, my_color1, my_color2, &
         & my_terminal, my_persist, my_title, my_filename
    logical :: save_file, my_polar
    real(dp) :: timeout, xrange1, xrange2, yrange1, yrange2
    type(gp_plot), pointer :: gg

    Nx1 = size(x1)
    Nx2 = size(x2)
    if ((Nx1/=size(y1)) .or. (Nx2/=size(y2))) &
         & stop 'subroutine plot ERROR: size(x) is not equal to size(y)'
    
    if (present(style) .and. (len(style)/=6)) &
         & stop 'subroutine plot ERROR: argument "style" has wrong number of characters'

    if (present(input)) then
       dat_file_name = 'dat_file_'//trim(input)//'.dat'
       cmd_file_name = 'cmd_file_'//trim(input)//'.plt'
    else
       dat_file_name = 'dat_file.dat'
       cmd_file_name = 'cmd_file.plt'
    end if

    call chk_unit(dat_file_name, 'plot2_dat', dat_unit)

    Nmax = max(Nx1, Nx2)	
    do i = 1, Nmax
       write(dat_unit, '(4E15.7)') x1(min(i, Nx1)), y1(min(i, Nx1)), &
            & x2(min(i, Nx2)), y2(min(i, Nx2))
    end do

    close(unit=dat_unit, status='keep')

    save_file = .false.
    if (present(input)) save_file = .true.
    timeout = 0.d0
    if (present(pause)) timeout = pause
    my_terminal = default_terminal
    if (present(terminal)) my_terminal = terminal
    my_persist = 'persist'
    if (present(persist) .and. (persist=='no')) my_persist = ''
    my_title = 'Gnuplot'
    if (present(title) .and. (title/='')) my_title = title
    my_filename = my_date_and_time()
    if (present(filename)) my_filename = filename
    my_polar = .false.
    if (present(polar).and. (polar=='yes')) my_polar = .true.

    xrange1 = min(minval(x1), minval(x2))
    xrange2 = max(maxval(x1), maxval(x2))
    yrange1 = min(minval(y1), minval(y2))
    yrange2 = max(maxval(y1), maxval(y2))

    if (my_polar) then
       xrange1 = -max(maxval(abs(y1)), maxval(abs(y2)))
       xrange2 = -xrange1
       yrange1 =  xrange1
       yrange2 = -xrange1
    end if

    my_line_type1 = 'lines'
    if (present(style)) then
       if ((style(3:3) == '-')) then
          my_line_type1 = 'linespoints'
       else
          my_line_type1 = 'points'
       end if
    end if
    my_line_type2 = 'lines'
    if (present(style)) then
       if ((style(6:6) == '-')) then
          my_line_type2 = 'linespoints'
       else
          my_line_type2 = 'points'
       end if
    end if
    if (present(linewidth)) then
       write(my_linewidth, '(e10.3)') linewidth
    else
       my_linewidth = trim(default_linewidth)
    end if
    if (present(color1)) then
       my_color1 = '"'//trim(color1)//'"'
    else
       my_color1 = '"'//trim(default_color1)//'"'
    end if
    if (present(color2)) then
       my_color2 = '"'//trim(color2)//'"'
    else
       my_color2 = '"'//trim(default_color2)//'"'
    end if

    allocate(gg)
    
    call gg%init(dat_file_name, cmd_file_name, save_file, timeout)
    
    call gg%set_config(my_terminal, my_persist, my_title, my_filename, my_polar, &
       & xrange1, xrange2, yrange1, yrange2)
    
    call gg%add_plot(index='1:2', plot_type=my_line_type1, line_color=my_color1, &
       & line_width=my_linewidth, point_type=style(1:2))
    call gg%add_plot(index='3:4', plot_type=my_line_type2, line_color=my_color2, &
       & line_width=my_linewidth, point_type=style(4:5))
       
    call gg%excute()
    
    deallocate(gg)

  end subroutine plot_2

  subroutine plot_1(x1, y1, style, pause, color1, terminal, filename, polar, &
       & persist, input, title, linewidth)
    implicit none
    real(dp), intent(in) :: x1(:), y1(:)
    real(dp), optional   :: pause, linewidth
    character(len=*), optional :: style, color1, terminal, filename, polar, &
         & persist, input, title
    integer :: i, dat_unit, Nx1
    character(len=100) :: dat_file_name, cmd_file_name, my_linewidth
    character(len=20)  :: my_line_type1, my_color1, &
         & my_terminal, my_persist, my_title, my_filename
    logical :: save_file, my_polar
    real(dp) :: timeout, xrange1, xrange2, yrange1, yrange2
    type(gp_plot), pointer :: gg

    Nx1 = size(x1)
    if ((size(x1) /= size(y1))) &
         & stop 'subroutine plot ERROR: size(x) is not equal to size(y)'
    if (present(style) .and. (len(style)/=3)) &
         & stop 'subroutine plot ERROR: argument "style" has wrong number of characters'

    if (present(input)) then
       dat_file_name = 'dat_file_'//trim(input)//'.dat'
       cmd_file_name = 'cmd_file_'//trim(input)//'.plt'
    else
       dat_file_name = 'dat_file.dat'
       cmd_file_name = 'cmd_file.plt'
    end if

    call chk_unit(dat_file_name, 'plot1_dat', dat_unit)

    write(dat_unit, '(2E15.7)') (x1(i), y1(i), i = 1, Nx1)

    close(unit=dat_unit, status='keep')

    save_file = .false.
    if (present(input)) save_file = .true.
    timeout = 0.d0
    if (present(pause)) timeout = pause
    my_terminal = default_terminal
    if (present(terminal)) my_terminal = terminal
    my_persist = 'persist'
    if (present(persist) .and. (persist=='no')) my_persist = ''
    my_title = 'Gnuplot'
    if (present(title) .and. (title/='')) my_title = title
    my_filename = my_date_and_time()
    if (present(filename)) my_filename = filename
    my_polar = .false.
    if (present(polar).and. (polar=='yes')) my_polar = .true.

    xrange1 = minval(x1)
    xrange2 = maxval(x1)
    yrange1 = minval(y1)
    yrange2 = maxval(y1)

    if (my_polar) then
       xrange1 = -maxval(abs(y1))
       xrange2 = -xrange1
       yrange1 =  xrange1
       yrange2 = -xrange1
    end if

    my_line_type1 = 'lines'
    if (present(style)) then
       if ((style(3:3) == '-')) then
          my_line_type1 = 'linespoints'
       else
          my_line_type1 = 'points'
       end if
    end if
    if (present(linewidth)) then
       write(my_linewidth, '(e10.3)') linewidth
    else
       my_linewidth = trim(default_linewidth)
    end if
    if (present(color1)) then
       my_color1 = '"'//trim(color1)//'"'
    else
       my_color1 = '"'//trim(default_color1)//'"'
    end if

    allocate(gg)
    
    call gg%init(dat_file_name, cmd_file_name, save_file, timeout)
    
    call gg%set_config(my_terminal, my_persist, my_title, my_filename, my_polar, &
       & xrange1, xrange2, yrange1, yrange2)
    
    call gg%add_plot(index='1:2', plot_type=my_line_type1, line_color=my_color1, &
       & line_width=my_linewidth, point_type=style(1:2))
       
    call gg%excute()
    
    deallocate(gg)

  end subroutine plot_1

  subroutine get_unit(iunit)
    implicit none
    integer :: iunit
    integer :: i, ios
    logical :: lopen

    iunit = 0
    do i = 1, 99
       if (i/=5 .and. i/=6) then	
          inquire (unit=i, opened=lopen, iostat=ios)
          if (ios == 0) then
             if (.not. lopen) then
                iunit = i
                return
             end if
          end if
       end if
    end do
    return
  end subroutine get_unit

  subroutine chk_unit(file_name, string, file_unit)
    implicit none
    character(len=*), intent(in) :: file_name, string
    integer,         intent(out) :: file_unit
    integer                      :: ios

    call get_unit(file_unit)
    if (file_unit == 0) then
       print*, trim(string), ': fatal error! Could not get a free FORTRAN unit.'
       stop
    end if

    open (unit=file_unit, file=file_name, status='replace', iostat=ios)
    if (ios /= 0) then
       print*, trim(string), ': fatal error! Could not open the terminal command file.'
       stop
    end if
    return
  end subroutine chk_unit

end module gnufor2
