%
% Function: write_fortran
% -----------------------
% Write fortran unformatted binary files. The function write_fortran()
% writes nitems objects, each size bytes long, to the stream
% pointed to by stream, obtaining them from the location given by
% ptr and pads each end with a 4 byte offset specifying the number
% of bytes written.
%
function write_fortran(ptr, size, nitems, stream)
    if(size == 4)
        prec = 'float';
    elseif(size == 8)
        prec = 'double';
    else
        disp('Only 4 and 8 byte values are supported\n');
        return
    end
    total_size = size * nitems;
fwrite(stream, total_size, 'integer*4');
fwrite(stream, ptr(1:nitems), prec);
fwrite(stream, total_size, 'integer*4');
end
