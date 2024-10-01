      !read point source data for constituents !rtb cs
      subroutine recall_read_pest

      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use maximum_data_module
      use time_module
      use exco_module
      
      implicit none      
 
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character(len=16) :: ob_name
      character(len=8) :: ob_typ
      character(len=16) :: pest_name
	  character(len=16) :: pest_cur
	  real :: pest_mass
      integer :: imax                 !none       |end of loop
      integer :: iyr                  !           |
      integer :: jday                 !           |
      integer :: mo                   !           |
      integer :: day_mo               !           |
      integer :: eof                  !           |end of file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: nbyr                 !none       !number of years the land use occurred 
      integer :: k                    !           |
      integer :: iyrs                 !           | 
      integer :: iyr_prev             !none       |previous year
      integer :: iyr_beg              !           |
	  integer :: istep                !           | 
      integer :: ipc                  !none       |counter
      integer :: ii                   !none       |counter
      integer :: i                    !           |
      integer :: iexco_om
      integer :: ifirst               !           |
      integer :: iexo_allo = 0
      integer :: ics,jj,kk,pr,pc,ipest
      
      eof = 0
      imax = 0
      
	  !read all rec_pest files
	  inquire (file="pest_recall.rec", exist=i_exist)
	  if (i_exist ) then
      do
	  open (107,file="pest_recall.rec")
	  read (107,*,iostat=eof) titldum
	  if (eof < 0) exit
	  read (107,*,iostat=eof) header
	  if (eof < 0) exit
	  imax = 0
	  do while (eof == 0)
	  read (107,*,iostat=eof) i
	  if (eof < 0) exit
	  imax = Max(imax,i) 
      end do
      end do
	  
	  if (imax > 0) then
	  allocate (rec_pest(0:imax))
	  !point sources originating from outside the watershed
        if (.not. allocated(recoutcsb_d)) allocate (recoutcsb_d(imax))
        if (.not. allocated(recoutcsb_m)) allocate (recoutcsb_m(imax))
        if (.not. allocated(recoutcsb_y)) allocate (recoutcsb_y(imax))
        if (.not. allocated(recoutcsb_a)) allocate (recoutcsb_a(imax))
        do ii=1,imax
          allocate (recoutcsb_d(ii)%pest(cs_db%num_pests))
          allocate (recoutcsb_m(ii)%pest(cs_db%num_pests))
          allocate (recoutcsb_y(ii)%pest(cs_db%num_pests))
          allocate (recoutcsb_a(ii)%pest(cs_db%num_pests))
          do ipest=1,cs_db%num_pests
            recoutcsb_d(ii)%pest(ipest) = 0.
            recoutcsb_m(ii)%pest(ipest) = 0.
            recoutcsb_y(ii)%pest(ipest) = 0.
            recoutcsb_a(ii)%pest(ipest) = 0.
          enddo
        enddo
	  
	  rewind (107)
	  read (107,*,iostat=eof) titldum
	  read (107,*,iostat=eof) header
	  do ii = 1, imax
		  read (107,*,iostat = eof) k, rec_pest(ii)%name, rec_pest(ii)%typ, rec_pest(ii)%filename
		  open (108,file = rec_pest(ii)%filename)
		  read (108,*,iostat=eof) titldum
		  read (108,*,iostat=eof) nbyr
		  read (108,*,iostat=eof) header
		  
	  select case (rec_pest(ii)%typ)
		  case (1) !! daily
		  allocate (rec_pest(ii)%hd_pest(366,nbyr))
		  do pr = 1, 366
		    do pc = 1, nbyr
			  allocate (rec_pest(ii)%hd_pest(pr,pc)%pest(cs_db%num_pests))
			  rec_pest(ii)%hd_pest(pr,pc)%pest = 0.
			end do
		  end do
		  
		  case (2) !! monthly
		  allocate (rec_pest(ii)%hd_pest(12,nbyr))
		  do pr = 1, 12
		    do pc = 1, nbyr
			  allocate (rec_pest(ii)%hd_pest(pr,pc)%pest(cs_db%num_pests))
			  rec_pest(ii)%hd_pest(pr,pc)%pest = 0.
			end do
		  end do
		  
		  case (3) !! annual
		  allocate (rec_pest(ii)%hd_pest(1,nbyr))
		  do pr = 1, 1
		    do pc = 1, nbyr
			  allocate (rec_pest(ii)%hd_pest(pr,pc)%pest(cs_db%num_pests))
			  rec_pest(ii)%hd_pest(pr,pc)%pest = 0.
			end do
		  end do
	  end select
	  
	  read (108,*,iostat=eof) jday, mo, day_mo, iyr, ob_typ, ob_name, pest_name, pest_mass
	  iyr_beg = iyr
	  backspace (108)
	  
	  iyrs = 1
	  pest_cur = "xyz"
	  do
	    if (eof < 0) exit
		read (108,*,iostat=eof) jday, mo, day_mo, iyr, ob_typ, ob_name, pest_name, pest_mass
		if(trim(pest_name) /= trim(pest_cur)) then
		  do kk = 1, cs_db%num_pests
		    if (trim(cs_db%pests(kk)) == trim(pest_name)) then
			  ipest = cs_db%pest_num(kk)
			  pest_cur = cs_db%pests(kk)
			end if
		  end do
		end if
		
		iyrs = iyr - iyr_beg + 1		
		rec_pest(k)%hd_pest(jday,iyrs)%pest(ipest) = pest_mass
	  end do
	  
	  close (108)
	  end do 
	  close (107)
	  end if    
	  end if
	  
      return
      end subroutine recall_read_pest