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
asserts.isequal(nargin('ncread'), -1);
asserts.isequal(nargout('ncread'), -1);
ok = false; try; ncread(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
nccreate(filename, 'x', 'Dimensions', {'n', 5}, 'Datatype', 'double');
ncwrite(filename, 'x', 1:5);
data = ncread(filename, 'x');
asserts.isequal(data(:)', 1:5);
data = ncread(filename, 'x', 2, 3);
asserts.isequal(data(:)', [2 3 4]);
ok = false; try; ncread(filename, 'missing'); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



