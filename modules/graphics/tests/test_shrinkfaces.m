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
faces = [1 2 3 4];
vertices = [0 0 0; 1 0 0; 1 1 0; 0 1 0];
[newFaces, newVertices] = shrinkfaces(faces, vertices, 0.25);
asserts.isequal(newFaces, [1 2 3 4]);
asserts.isapprox(newVertices, [0.25 0.25 0; 0.75 0.25 0; 0.75 0.75 0; 0.25 0.75 0], 1e-12);
%=============================================================================
fv = struct('faces', faces, 'vertices', vertices);
nfv = shrinkfaces(fv);
asserts.istrue(isfield(nfv, 'faces'));
asserts.istrue(isfield(nfv, 'vertices'));
asserts.isequal(nfv.faces, [1 2 3 4]);
asserts.isapprox(mean(nfv.vertices, 1), [0.5 0.5 0], 1e-12);
%=============================================================================
faces2 = [1 2 3; 1 3 4];
[newFaces2, newVertices2] = shrinkfaces(faces2, vertices, 1);
asserts.isequal(newFaces2, [1 2 3; 4 5 6]);
asserts.isequal(size(newVertices2), [6 3]);
%=============================================================================
facesNaN = [1 2 3 NaN; 1 3 4 NaN];
[newFacesNaN, newVerticesNaN] = shrinkfaces(facesNaN, vertices, 0.25);
asserts.isequal(newFacesNaN, [1 2 3 NaN; 4 5 6 NaN]);
asserts.isequal(size(newVerticesNaN), [6 3]);
asserts.isapprox(newVerticesNaN(1:3, :), ...
  [0.333333333333333 0.166666666666667 0; ...
   0.833333333333333 0.166666666666667 0; ...
   0.833333333333333 0.666666666666667 0], 1e-12);
%=============================================================================
f = figure('Visible', 'off');
p = patch('Faces', faces, 'Vertices', vertices);
shrinkfaces(p, 0.25);
asserts.isequal(p.Faces, [1 2 3 4]);
asserts.isapprox(p.Vertices, newVertices, 1e-12);
close(f);
%=============================================================================
asserts.checkerror('shrinkfaces(faces, vertices, -1);', getString(message('Nelson:validateShrinkFactor:invalidShrinkFactor')));
asserts.checkerror('shrinkfaces([1 5], vertices, 1);', getString(message('Nelson:validateShrinkfacesData:facesContainInvalidVertex')));
asserts.checkerror('shrinkfaces(struct(''faces'', faces), 1);', getString(message('Nelson:shrinkfacesDataFromStruct:invalidStructure')));
%=============================================================================
