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
h5CloudFigureVisible = 'off';
h5CloudAnimationEnabled = true;
h5CloudAnimationFrames = 4;
h5CloudFramePause = 0;
h5CloudAnimationContinuous = false;
h5CloudPointCount = 800;
run([modulepath('hdf5', 'root'), '/examples/point_cloud_spin.m']);

assert_istrue(isgraphics(h5CloudFigure));
% The cloud was written to and read back from an HDF5 file.
assert_istrue(isfile(h5CloudFile));
assert_isequal(numel(h5CloudX), h5CloudPointCount);
assert_isequal(numel(h5CloudC), h5CloudPointCount);
% The round trip preserved the coordinates.
assert_isapprox(h5CloudX(2), sin(h5CloudC(2)) + 2 * sin(2 * h5CloudC(2)), ...
  1e-6);
delete(h5CloudFigure);
drawnow();
%=============================================================================
