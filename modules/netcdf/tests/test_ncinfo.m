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
asserts.isequal(nargin('ncinfo'), -1);
asserts.isequal(nargout('ncinfo'), -1);
ok = false; try; ncinfo(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
nccreate(filename, 'x', 'Dimensions', {'n', 2});
ncwriteatt(filename, '/', 'title', 'metadata test');
ncwriteatt(filename, 'x', 'units', 'meter');
info = ncinfo(filename);
asserts.istrue(isstruct(info));
asserts.isequal(info.Name, '/');
asserts.istrue(isfield(info, 'Format'));
asserts.isequal(info.NumVariables, 1);
asserts.isequal(info.NumDimensions, 1);
asserts.isequal(info.Attributes(1).Name, 'title');
asserts.isequal(info.Attributes(1).Value, 'metadata test');
asserts.isequal(info.Variables(1).Attributes(1).Name, 'units');
asserts.isequal(info.Variables(1).Attributes(1).Value, 'meter');
asserts.isequal(info.Variables(1).Dimensions(1).Name, 'n');
asserts.isequal(info.Variables(1).Size, 2);
ok = false; try; ncinfo([tempname(), '.nc']); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



