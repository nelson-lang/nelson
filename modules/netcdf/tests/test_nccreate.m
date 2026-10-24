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
asserts.isequal(nargin('nccreate'), -1);
asserts.isequal(nargout('nccreate'), -1);
ok = false; try; nccreate(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
nccreate(filename, 'x', 'Dimensions', {'n', 3}, 'Datatype', 'double');
info = ncinfo(filename);
asserts.isequal(info.NumVariables, 1);
nccreate(filename, 'y', 'Dimensions', {'m', 0}, 'Datatype', 'single');
ok = false; try; nccreate(filename, 'bad', 'Dimensions', [1 2]); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



