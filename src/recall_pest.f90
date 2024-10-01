      !rtb cs
      !include constituent mass from point sources
      subroutine recall_pest(irec)
      
      
      use basin_module
      use hydrograph_module
      use time_module
      use constituent_mass_module
      use ch_cs_module
      
      implicit none 

      !incoming variables
      integer :: irec
      
      !local variables
      integer :: ipest					!            |constituent counter
      integer :: ichan        !            |id of source channel
      real :: pest_conc         !g/m3        |concentration of constituent in source channel
      real :: div_mass        !kg          |mass of constituent in diversion water
      
      
      !depending on the point source type, add/remove constituent mass to object
      obcs(icmd)%hd(1)%pest = 0.
      if (cs_db%num_pests > 0) then
        select case (rec_pest(irec)%typ)  
          case (1)    !daily
            if (time%yrc >= recall(irec)%start_yr .and. time%yrc <= recall(irec)%end_yr) then 
              do ipest=1,cs_db%num_pests
                if(recall(irec)%hd(time%day,time%yrs)%flo < 0) then
                  !diversion: remove mass from channel directly
                  ichan = ob(icmd)%obtypno_out(1) !channel from which water is diverted
                  if(ch_stor(ichan)%flo > 10.) then !only proceed if channel has water
                    pest_conc = (ch_water(ichan)%pest(ipest)*1000.) / ch_stor(ichan)%flo !g/m3 in channel water
                    div_mass = (pest_conc * recall(irec)%hd(time%day,time%yrs)%flo) / 1000. !kg/day 
                    if((div_mass*(-1)) > ch_water(ichan)%pest(ipest)) then
                      div_mass = ch_water(ichan)%pest(ipest) * (-1) !only take what is there
                    endif
                    ch_water(ichan)%pest(ipest) = ch_water(ichan)%pest(ipest) + div_mass
                  endif
                else
                  !source: add mass
                  obcs(icmd)%hd(1)%pest(ipest) = rec_pest(irec)%hd_pest(time%day,time%yrs)%pest(ipest)
				endif
              enddo
              recoutcsb_d(irec)%pest = obcs(icmd)%hd(1)%pest
            else
              obcs(icmd)%hd(1) = hin_csz
            endif
          case (2)    !monthly
            if (time%yrc >= recall(irec)%start_yr .and. time%yrc <= recall(irec)%end_yr) then 
              do ipest=1,cs_db%num_pests
                obcs(icmd)%hd(1)%pest(ipest) = rec_pest(irec)%hd_pest(time%mo,time%yrs)%pest(ipest)
              enddo
                recoutcsb_d(irec)%pest = rec_pest(irec)%hd_pest(time%mo,time%yrs)%pest
            else
              obcs(icmd)%hd(1) = hin_csz
            endif
          case (3)    !annual
            if (time%yrc >= rec_cs(irec)%start_yr .or. time%yrc <= rec_cs(irec)%end_yr) then
              do ipest=1,cs_db%num_pests
                obcs(icmd)%hd(1)%pest(ipest) = rec_pest(irec)%hd_pest(1,time%yrs)%pest(ipest)
              enddo
                recoutcsb_d(irec)%pest = rec_pest(irec)%hd_pest(1,time%yrs)%pest
            else
              obcs(icmd)%hd(1) = hin_csz
            endif
          case (4)    !average annual
            if (time%yrc >= recall(irec)%start_yr .and. time%yrc <= recall(irec)%end_yr) then 
              do ipest=1,cs_db%num_pests
                obcs(icmd)%hd(1)%pest(ipest) = rec_pest(irec)%hd_pest(1,1)%pest(ipest)
              enddo
                recoutcsb_d(irec)%pest = rec_pest(irec)%hd_pest(1,1)%pest
            endif  
          end select
      endif

      
      return
      end subroutine recall_pest