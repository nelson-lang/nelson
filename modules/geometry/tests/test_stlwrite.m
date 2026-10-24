%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
P = [0 0 0; 1 0 0; 0 1 0];
T = [1 2 3];
TR = triangulation(T, P);
filename = [tempname(), '.stl'];
stlwrite(TR, filename, 'text');
[TRr, fmt, attr, solid] = stlread(filename);
asserts.isequal(fmt, 'text');
asserts.isequal(TRr.Points, P);
asserts.isequal(TRr.ConnectivityList, T);
asserts.isequal(size(attr), [1 0]);
asserts.isequal(solid, 1);
rmfile(filename);
%=============================================================================
