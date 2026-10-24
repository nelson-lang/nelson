%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
filename = [tempname(), '.nc'];
nccreate(filename, 'temperature', 'Dimensions', {'time', 3}, 'Datatype', 'double');
ncwrite(filename, 'temperature', [10 11 12]);
data = ncread(filename, 'temperature');
asserts.isequal(data(:)', [10 11 12]);
ncwriteatt(filename, 'temperature', 'units', 'degC');
asserts.isequal(ncreadatt(filename, 'temperature', 'units'), 'degC');
info = ncinfo(filename);
asserts.istrue(isstruct(info));
asserts.isequal(netcdf.getConstant('NC_GLOBAL'), -1);
%=============================================================================




