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
close all
f = figure('Visible', 'off');
plot(1:10, (1:10) .^ 2);
scene_2d = [tempdir(), 'test_saveFigureScene_2d.json'];
nelson.graphics.internal.saveFigureScene(f, scene_2d);
asserts.istrue(isfile(scene_2d));
s = jsondecode(fileread(scene_2d));
asserts.isequal(s.format, 'nelson-figure');
asserts.isequal(s.version, 1);
asserts.istrue(s.width > 0);
asserts.istrue(s.height > 0);
asserts.istrue(~isempty(s.primitives));
asserts.istrue(ischar(s.background));
close(f);
%=============================================================================
g = figure('Visible', 'off');
surf(peaks);
scene_3d = [tempdir(), 'test_saveFigureScene_3d.json'];
nelson.graphics.internal.saveFigureScene(g, scene_3d);
s3 = jsondecode(fileread(scene_3d));
asserts.istrue(s3.is3d);
asserts.istrue(isfield(s3, 'webglMeshes'));
asserts.istrue(~isempty(s3.webglMeshes));
close(g);
%=============================================================================
h = figure('Visible', 'off');
caught = '';
try
  nelson.graphics.internal.saveFigureScene(h);
catch e
  caught = e.message;
end
asserts.istrue(~isempty(caught));
close(h);
%=============================================================================
