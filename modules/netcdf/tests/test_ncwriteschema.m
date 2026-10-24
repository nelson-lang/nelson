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
asserts.isequal(nargin('ncwriteschema'), -1);
asserts.isequal(nargout('ncwriteschema'), -1);
ok = false; try; ncwriteschema(); catch; ok = true; end; asserts.istrue(ok);
source = [tempname(), '.nc'];
target = [tempname(), '.nc'];
nccreate(source, 'x', 'Dimensions', {'n', 2});
ncwriteatt(source, '/', 'title', 'schema source');
ncwriteatt(source, 'x', 'units', 'meter');
schema = ncinfo(source);
ncwriteschema(target, schema);
info = ncinfo(target);
asserts.isequal(info.NumVariables, schema.NumVariables);
asserts.isequal(ncreadatt(target, '/', 'title'), 'schema source');
asserts.isequal(ncreadatt(target, 'x', 'units'), 'meter');
ok = false; try; ncwriteschema(target, 3); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



