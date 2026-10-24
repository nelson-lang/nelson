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
[x, y, z] = meshgrid(-2:1:2, -2:1:2, -2:1:2);
V = x .^ 2 + y .^ 2 + z .^ 2;
[faces, vertices] = isosurface(V, 4);
asserts.isequal(size(faces, 2), 3);
asserts.isequal(size(vertices, 2), 3);
asserts.istrue(size(faces, 1) > 0);
asserts.istrue(size(vertices, 1) > 0);
asserts.istrue(all(faces(:) >= 1));
asserts.istrue(all(faces(:) <= size(vertices, 1)));
%=============================================================================
[faces2, vertices2] = isosurface(-2:1:2, -2:1:2, -2:1:2, V, 4);
asserts.isequal(size(faces2, 2), 3);
asserts.isequal(size(vertices2, 2), 3);
asserts.istrue(min(vertices2(:, 1)) >= -2);
asserts.istrue(max(vertices2(:, 1)) <= 2);
asserts.istrue(min(vertices2(:, 2)) >= -2);
asserts.istrue(max(vertices2(:, 2)) <= 2);
asserts.istrue(min(vertices2(:, 3)) >= -2);
asserts.istrue(max(vertices2(:, 3)) <= 2);
%=============================================================================
xNonUniform = [-2 -1.5 -0.25 0.5 2];
yNonUniform = [-3 -1 0.25 1.5 4];
zNonUniform = [-5 -2 -0.5 1 3];
[facesNU, verticesNU] = isosurface(xNonUniform, yNonUniform, zNonUniform, V, 4);
asserts.isequal(size(facesNU, 2), 3);
asserts.isequal(size(verticesNU, 2), 3);
asserts.istrue(min(verticesNU(:, 1)) >= min(xNonUniform));
asserts.istrue(max(verticesNU(:, 1)) <= max(xNonUniform));
asserts.istrue(min(verticesNU(:, 2)) >= min(yNonUniform));
asserts.istrue(max(verticesNU(:, 2)) <= max(yNonUniform));
asserts.istrue(min(verticesNU(:, 3)) >= min(zNonUniform));
asserts.istrue(max(verticesNU(:, 3)) <= max(zNonUniform));
%=============================================================================
s = isosurface(x, y, z, V, 4);
asserts.istrue(isstruct(s));
asserts.istrue(isfield(s, 'vertices'));
asserts.istrue(isfield(s, 'faces'));
asserts.isequal(size(s.faces, 2), 3);
asserts.isequal(size(s.vertices, 2), 3);
%=============================================================================
sAuto = isosurface(x, y, z, V);
asserts.istrue(size(sAuto.faces, 1) > 0);
asserts.istrue(size(sAuto.vertices, 1) > 0);
sEmpty = isosurface(ones(3, 3, 3));
asserts.isequal(sEmpty.faces, zeros(0, 3));
asserts.isequal(sEmpty.vertices, zeros(0, 3));
VWithNaN = V;
VWithNaN(3, 3, 3) = NaN;
sNaN = isosurface(x, y, z, VWithNaN, 4);
asserts.istrue(size(sNaN.faces, 1) > 0);
asserts.istrue(all(isfinite(sNaN.vertices(:))));
%=============================================================================
C = x + 2 .* y + 3 .* z;
sColor = isosurface(x, y, z, V, 4, C);
asserts.istrue(isfield(sColor, 'facevertexcdata'));
asserts.isequal(size(sColor.facevertexcdata, 1), size(sColor.vertices, 1));
[faces3, vertices3, cdata3] = isosurface(x, y, z, V, 4, C);
asserts.isequal(size(cdata3, 1), size(vertices3, 1));
asserts.isequal(size(faces3, 2), 3);
%=============================================================================
sColorAuto = isosurface(x, y, z, V, C);
asserts.istrue(isfield(sColorAuto, 'facevertexcdata'));
asserts.isequal(size(sColorAuto.facevertexcdata, 1), size(sColorAuto.vertices, 1));
%=============================================================================
[facesShared, verticesShared] = isosurface(x, y, z, V, 4);
[facesNoShare, verticesNoShare] = isosurface(x, y, z, V, 4, 'noshare');
asserts.isequal(size(facesNoShare, 2), 3);
asserts.istrue(size(verticesNoShare, 1) >= size(verticesShared, 1));
asserts.isequal(size(facesShared, 2), 3);
%=============================================================================
[facesVerbose, verticesVerbose] = isosurface(x, y, z, V, 4, 'verbose');
asserts.isequal(size(facesVerbose, 2), 3);
asserts.isequal(size(verticesVerbose, 2), 3);
%=============================================================================
[xFine, yFine, zFine] = meshgrid(-2:0.25:2, -2:0.25:2, -2:0.25:2);
VFine = xFine .^ 2 + yFine .^ 2 + zFine .^ 2;
[facesFine, verticesFine] = isosurface(xFine, yFine, zFine, VFine, 1);
asserts.isequal(size(facesFine), [536 3]);
asserts.isequal(size(verticesFine), [270 3]);
%=============================================================================
f = figure('Visible', 'off');
isosurface(x, y, z, V, 4);
ax = gca();
h = findobj(ax, 'Type', 'patch');
l = findobj(ax, 'Type', 'light');
asserts.isequal(numel(h), 1);
asserts.isequal(numel(l), 1);
asserts.isequal(h.FaceColor, 'flat');
asserts.isequal(h.FaceLighting, 'gouraud');
asserts.isequal(h.EdgeColor, 'none');
asserts.isequal(h.UserData, 4);
asserts.isequal(h.VertexNormalsMode, 'manual');
asserts.isequal(ax.View, [-37.5 30]);
asserts.isequal(char(ax.XGrid), 'off');
close(f);
%=============================================================================
asserts.checkerror('isosurface(ones(2), 1);', getString(message('Nelson:isosurface:invalidVolumeType')));
asserts.checkerror('isosurface(ones(3,3,3), [1 2]);', getString(message('Nelson:validateIsosurfaceLevel:invalidIsovalue')));
asserts.checkerror('isosurface(ones(3,3,3), 1, ones(2,2,2));', getString(message('Nelson:isosurface:colorMismatch')));
asserts.checkerror('isosurface(1:2, 1:3, 1:3, ones(3,3,3), 1);', getString(message('Nelson:isosurface:gridMismatch')));
asserts.checkerror('isosurface(ones(3,3,3), 1, ''bad'');', getString(message('Nelson:parseIsosurfaceOptions:invalidIsosurfaceOption')));
%=============================================================================
