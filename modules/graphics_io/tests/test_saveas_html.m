%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
destination = [tempname(), '/'];
mkdir(destination);
%=============================================================================
f = figure('Visible', 'off');
surf(peaks);
htmlFile = [destination, 'figure.html'];
saveas(f, htmlFile);
asserts.istrue(isfile(htmlFile));
txt = fileread(htmlFile);
% The replayer export is neutralized to a global and the scene is inlined.
asserts.istrue(contains(txt, 'globalThis.__nelsonActivate='));
asserts.istrue(contains(txt, 'nelson-figure'));
% A 3D surface carries webgl meshes in the inlined scene.
asserts.istrue(contains(txt, 'webglMeshes'));
% No stray ES export survives, so the inlined module loads without imports.
asserts.istrue(~contains(txt, 'export {'));
close(f);
%=============================================================================
g = figure('Visible', 'off');
plot(1:5, [1, 4, 2, 5, 3]);
forcedFile = [destination, 'figure_forced'];
saveas(g, forcedFile, 'html');
asserts.istrue(isfile([forcedFile, '.html']));
close(g);
%=============================================================================
