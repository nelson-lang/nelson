%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--INTERACTIVE TEST-->
% <--WEB GUI REQUIRED--> run with: nelson-webview --file runtests_web.m
%=============================================================================
% Large wireframe scenes use retained WebGL geometry. A software image remains
% available only when neither browser WebGL backend can be initialized.
teapot_directory = [modulepath('graphics', 'root'), '/examples/utah-teapot/'];
load([teapot_directory, 'teapot.nh5']);
f = figure('Visible', 'off');
axis off
axis([-3 3 -3 3 -3 5]);
axis('square');
patch('Faces', teapotFaces, 'Vertices', teapotVertices, 'FaceColor', 'none');
view(720, 360);
dl = jsondecode(__web_display_list__(f));
assert_istrue(isfield(dl, 'webglMeshes'));
assert_isequal(numel(dl.webglMeshes), 1);
assert_istrue(dl.webglMeshes(1).positionsLength > 1000);
assert_istrue(dl.webglMeshes(1).lineIndicesLength > 1000);
assert_istrue(length(dl.webglMeshes(1).positionsBase64) > 1000);
assert_istrue(length(dl.webglMeshes(1).lineIndicesBase64) > 1000);
assert_istrue(length(dl.softwareFallbackImage) > 1000);
assert_istrue(numel(dl.primitives) < 100);
close(f);
%=============================================================================
% Standard colored surfaces use the retained face-and-edge mesh as well.
f = figure('Visible', 'off');
s = surf(peaks);
dl = jsondecode(__web_display_list__(f));
assert_istrue(isfield(dl, 'webglMeshes'));
assert_istrue(dl.webglMeshes(1).drawFaces);
assert_istrue(dl.webglMeshes(1).positionsLength > 1000);
assert_istrue(dl.webglMeshes(1).triangleIndicesLength > 1000);
assert_istrue(dl.webglMeshes(1).lineIndicesLength > 1000);
assert_istrue(length(dl.webglMeshes(1).positionsBase64) > 1000);
assert_istrue(length(dl.webglMeshes(1).vertexColorsBase64) > 1000);
assert_istrue(length(dl.webglMeshes(1).triangleIndicesBase64) > 1000);
assert_istrue(length(dl.webglMeshes(1).lineIndicesBase64) > 1000);
dl = jsondecode(__web_display_list__(f));
assert_istrue(length(dl.webglMeshes(1).positionsBase64) > 1000);
assert_istrue(length(dl.webglMeshes(1).triangleIndicesBase64) > 1000);
assert_istrue(numel(dl.primitives) < 500);
sameRevision = dl.webglMeshes(1).revision;
sameLineIndicesLength = dl.webglMeshes(1).lineIndicesLength;
set(s, 'MeshStyle', 'row');
dl = jsondecode(__web_display_list__(f));
assert_istrue(~strcmp(dl.webglMeshes(1).revision, sameRevision));
assert_istrue(dl.webglMeshes(1).lineIndicesLength < sameLineIndicesLength);
set(s, 'LineStyle', 'none');
dl = jsondecode(__web_display_list__(f));
assert_istrue(dl.webglMeshes(1).lineIndicesLength == 0);
close(f);
%=============================================================================
% A surface quad arrives in grid order. Its triangles must share the same
% diagonal instead of crossing through the cell.
f = figure('Visible', 'off');
surf([0 1; 2 3], 'EdgeColor', 'none');
dl = jsondecode(__web_display_list__(f));
assert_isequal(dl.webglMeshes(1).triangleIndices(:)', [0 1 3 0 3 2]);
close(f);
%=============================================================================
% Filled patches retain interpolated vertex colors in the same WebGL mesh.
mask_directory = [modulepath('graphics', 'root'), '/examples/nefertiti-mask/'];
load([mask_directory, 'nefertiti-mask.nh5']);
f = figure('Visible', 'off');
patch('Faces', Faces, 'Vertices', Vertices, 'FaceVertexCData', Colors, ...
  'EdgeColor', 'white', 'FaceColor', 'interp', 'FaceAlpha', 1);
axis equal
axis off
view([0, 0, 1]);
dl = jsondecode(__web_display_list__(f));
assert_istrue(isfield(dl, 'webglMeshes'));
assert_istrue(dl.webglMeshes(1).drawFaces);
assert_isequal(dl.webglMeshes(1).vertexColorsLength, ...
  4 * dl.webglMeshes(1).positionsLength / 3);
assert_istrue(length(dl.webglMeshes(1).vertexColorsBase64) > 1000);
assert_istrue(numel(dl.primitives) < 100);
close(f);
%=============================================================================
