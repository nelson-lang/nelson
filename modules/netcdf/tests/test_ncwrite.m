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
asserts.isequal(nargin('ncwrite'), -1);
asserts.isequal(nargout('ncwrite'), -1);
ok = false; try; ncwrite(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
nccreate(filename, 'x', 'Dimensions', {'n', 4}, 'Datatype', 'double');
ncwrite(filename, 'x', [1 2 3 4]);
data = ncread(filename, 'x');
asserts.isequal(data(:)', [1 2 3 4]);
ncwrite(filename, 'x', [8 9], 2);
data = ncread(filename, 'x');
asserts.isequal(data(:)', [1 8 9 4]);
ok = false; try; ncwrite([tempname(), '.nc'], 'x', 1); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



