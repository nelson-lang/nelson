%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
x = [0; 1; 0; 1; 0.5];
y = [0; 0; 1; 1; 0.5];
v = [1; 2; 3; 4; 5];
%=============================================================================
% Matrix grid inputs: result has the same size as the query grid.
F = scatteredInterpolant(x, y, v, 'natural', 'none');
[xg, yg] = meshgrid(linspace(0, 1, 4), linspace(0, 1, 3));
Z = F(xg, yg);
assert_isequal(size(Z), [3 4]);
assert_isequal(size(Z), size(xg));
%=============================================================================
% Row vector inputs keep the row orientation.
Zr = F([0 0.5 1], [0 0.5 1]);
assert_isequal(size(Zr), [1 3]);
%=============================================================================
% Column vector inputs keep the column orientation.
Zc = F([0; 0.5; 1], [0; 0.5; 1]);
assert_isequal(size(Zc), [3 1]);
%=============================================================================
% Single point-matrix form F(P) returns an Npts-by-1 column.
P = [0 0; 0.5 0.5; 1 1];
Zp = F(P);
assert_isequal(size(Zp), [3 1]);
%=============================================================================
% 'nearest' method also honors the query shape.
Fn = scatteredInterpolant(x, y, v, 'nearest');
Zn = Fn(xg, yg);
assert_isequal(size(Zn), [3 4]);
assert_isequal(size(Zn), size(xg));
%=============================================================================
% Mismatched coordinate sizes raise a clear error.
assert_checkerror('F([0 1], [0; 1])', _('The query coordinate arrays must all have the same size.'));
%=============================================================================
