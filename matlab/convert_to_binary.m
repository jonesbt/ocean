clear all
close all

%Chose precision
prec = 4; %single precision
%prec = 8; %double precision

%Setup time  for constructing output file data and output filenames
% 01 Jan 2011 to 30 Apr 2011
% 01 Oct 2010 to 30 Apr 2011
st_yr = 2010;
st_mnth = 10;
st_day = 01;
end_yr = 2011;
end_mnth = 05;
end_day = 01;

%Choose filenames for output files
basedir = '/delmar/data1/bjones/InputFilesProject';
ofname_hfx = sprintf('%s/%d0%d0%d_%d0%d0%d_hfx.dat', basedir, st_yr, st_mnth, st_day, end_yr, end_mnth, end_day);
ofname_wnd = sprintf('%s/%d0%d0%d_%d0%d0%d_wnd.dat', basedir, st_yr, st_mnth, st_day, end_yr, end_mnth, end_day);
ofname_evp = sprintf('%s/%d0%d0%d_%d0%d0%d_evp.dat', basedir, st_yr, st_mnth, st_day, end_yr, end_mnth, end_day);
ofname_tau = sprintf('%s/%d0%d0%d_%d0%d0%d_tau.dat', basedir, st_yr, st_mnth, st_day, end_yr, end_mnth, end_day);

%Load data, this was computed in a previous version of ConvertToBinary
load 'fvcom_input_workspace';
disp('Data loaded');

%% Write heat flux, wind, and evap/precip files

st_time = datenum(st_yr, st_mnth, st_day);
end_time = datenum(end_yr, end_mnth, end_day);
fid_hfx = fopen(ofname_hfx, 'wb');
fid_wnd = fopen(ofname_wnd, 'wb');
fid_tau = fopen(ofname_tau, 'wb');
fid_evp = fopen(ofname_evp, 'wb');
hfx_interlaced = zeros(2*length(Mobj.lat), 1);
evp_interlaced = zeros(2*length(Mobj.lat), 1);
wnd_interlaced = zeros(2*length(Mobj.xc), 1);
tau_interlaced = zeros(2*length(Mobj.xc), 1);
if(fid_hfx == -1 || fid_evp == -1 || fid_wnd == -1 || fid_tau == -1)
  disp('Not all output files were opened properly, unopened files will be ignored. Press any key to continue or C-c to abort.');
  pause;
end
for t = 1:length(time)
    if (time(t) >= st_time) && (time(t) < end_time)
        %Interlace data
        for i=1:length(Mobj.lat)
            hfx_interlaced(2*i - 1) = net_hfx_fitted(t,i);
            hfx_interlaced(2*i) = sw_hfx_fitted(t,i);
            evp_interlaced(2*i - 1) = evap_fitted(t,i);
            evp_interlaced(2*i) = precip_fitted(t,i);
        end
        for i=1:length(Mobj.xc)
            wnd_interlaced(2*i - 1) = wnd_spd(t,i);
            wnd_interlaced(2*i) = wnd_dir(t,i);
            tau_interlaced(2*i - 1) = wnd_tau_x(t,i);
            tau_interlaced(2*i) = wnd_tau_y(t,i);
        end
        %Write files (but only if open)
        cur_time = time(t) - st_time; %This converts to number of days since st_time
        if(fid_hfx ~= -1)
            write_fortran(cur_time, prec, 1, fid_hfx);
            write_fortran(hfx_interlaced, prec, 2*length(Mobj.lat), fid_hfx);
        end
        if(fid_evp ~= -1)
            write_fortran(cur_time, prec, 1, fid_evp);
            write_fortran(evp_interlaced, prec, 2*length(Mobj.lat), fid_evp);
        end
        if(fid_wnd ~= -1)
            write_fortran(cur_time, prec, 1, fid_wnd);
            write_fortran(wnd_interlaced, prec, 2*length(Mobj.xc), fid_wnd);
        end
        if(fid_tau ~= -1)
            write_fortran(cur_time, prec, 1, fid_tau);
            write_fortran(tau_interlaced, prec, 2*length(Mobj.xc), fid_tau);
        end

    end
end
fclose(fid_hfx);
fclose(fid_wnd);
fclose(fid_evp);
fclose(fid_tau);
