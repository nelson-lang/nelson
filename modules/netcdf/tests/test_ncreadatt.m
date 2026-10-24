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
asserts.isequal(nargin('ncreadatt'), -1);
asserts.isequal(nargout('ncreadatt'), -1);
ok = false; try; ncreadatt(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
nccreate(filename, 'x', 'Dimensions', {'n', 2});
ncwriteatt(filename, 'x', 'scale', [1 2]);
asserts.isequal(ncreadatt(filename, 'x', 'scale'), [1 2]);
ncwriteatt(filename, '/', 'title', 'root');
asserts.isequal(ncreadatt(filename, '/', 'title'), 'root');
ok = false; try; ncreadatt(filename, 'x', 'missing'); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



