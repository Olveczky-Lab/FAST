function convertToAmp(dat, fpath)
% convertToAmp pads your data with zeros and converts to a 64 channel * nsamp AMP file.
% INPUTS:
% dat: nCh * nsamp recording data @ double precision in units of pre-amplitifcation voltage.
% fpath: desired path and file name of the AMP file you want to generate. e.g. 'D:\Data\data_file_1'.
%
% In the AMP format, a value of 32768 corresponds to 0 V and steps of 1 correspond to 1.95e-7 V.

nCh = size(dat,1);
nsamp = size(dat,2);

[pathstr, fname, ~] = fileparts(fpath);

if nCh <= 64
    % Pad data with zeros to 64 channels
    dat = cat(1, double(dat), zeros(64-nCh, nsamp));

    % Convert to uint16 precision
    dat = uint16(dat/1.95e-7 + 32768);
    
    % Flatten data array
    chipInd = [1:32; 33:64];
    dat = dat(chipInd(:),:);
    dat = dat(:);

    % Write to disk
    if exist(pathstr, 'dir') ~= 7
        mkdir(pathstr);
    end
    fid = fopen([pathstr '\' fname '.amp'], 'w', 'l');
    fwrite(fid, dat, 'uint16');
    fclose(fid);
else
    disp('Input data must have <= 64 channels');
end
