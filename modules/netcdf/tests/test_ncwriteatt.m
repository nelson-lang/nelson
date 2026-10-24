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
asserts.isequal(nargin('ncwriteatt'), -1);
asserts.isequal(nargout('ncwriteatt'), -1);
ok = false; try; ncwriteatt(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
nccreate(filename, 'x', 'Dimensions', {'n', 2});
ncwriteatt(filename, 'x', 'units', 'meter');
asserts.isequal(ncreadatt(filename, 'x', 'units'), 'meter');
ncwriteatt(filename, '/', 'title', 'sample');
asserts.isequal(ncreadatt(filename, '/', 'title'), 'sample');
ok = false; try; ncwriteatt(filename, 'missing', 'a', 1); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



