%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
[x, y, z] = meshgrid(1:4, 1:5, 1:6);
V = 2 .* x + 3 .* y + 5 .* z;
vertices = [2 3 4; 1.5 2.5 3.5];
n = isonormals(x, y, z, V, vertices);
asserts.isapprox(n, repmat([-2 -3 -5], 2, 1), 1e-12);
%=============================================================================
nDefault = isonormals(V, vertices);
asserts.isapprox(nDefault, n, 1e-12);
nNegate = isonormals(x, y, z, V, vertices, 'negate');
asserts.isapprox(nNegate, -n, 1e-12);
%=============================================================================
xv = 10:10:40;
yv = 20:20:100;
zv = 30:30:180;
[xg, yg, zg] = meshgrid(xv, yv, zv);
Vg = 2 .* xg + 3 .* yg + 5 .* zg;
ng = isonormals(xv, yv, zv, Vg, [20 60 120]);
asserts.isapprox(ng, [-2 -3 -5], 1e-12);
xn = [1 2 4 8];
yn = [1 3 6 10 15];
zn = [1 2 5 9 14 20];
[xng, yng, zng] = meshgrid(xn, yn, zn);
Vn = 2 .* xng + 3 .* yng + 5 .* zng;
nn = isonormals(xn, yn, zn, Vn, [4 6 9; 2.5 4.5 7]);
asserts.isapprox(nn, repmat([-2 -3 -5], 2, 1), 1e-12);
outsideNormals = isonormals(xn, yn, zn, Vn, [100 6 9; 4 -100 9; 4 6 100]);
asserts.istrue(all(isnan(outsideNormals(:))));
%=============================================================================
f = figure('Visible', 'off');
pVertices = [2 3 4; 2 3 4; 2 3 4];
p = patch('Faces', [1 2 3], 'Vertices', pVertices);
isonormals(x, y, z, V, p);
asserts.isapprox(p.VertexNormals, repmat([-2 -3 -5], 3, 1), 1e-12);
asserts.isequal(p.VertexNormalsMode, 'manual');
close(f);
%=============================================================================
asserts.checkerror('isonormals(ones(3,3,3), ones(2,2));', getString(message('Nelson:validateIsonormalsVertices:invalidVertices')));
asserts.checkerror('isonormals(ones(3,3,3), vertices, ''bad'');', getString(message('Nelson:parseIsonormalsOption:invalidIsonormalsOption')));
asserts.checkerror('isonormals(ones(3,3,3), vertices);', getString(message('Nelson:isonormals:patchHandleExpectedWhen')));
%=============================================================================
