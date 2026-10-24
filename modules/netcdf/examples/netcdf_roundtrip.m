%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Store a short time series in NetCDF and read it back.
filename = [tempdir(), 'nelson-example.nc'];
if isfile(filename), rmfile(filename); end
nccreate(filename, 'temperature', 'Dimensions', {'time', 5});
ncwrite(filename, 'temperature', [18.1; 19.4; 20.0; 19.7; 18.9]);
restored = ncread(filename, 'temperature');
disp(restored);
