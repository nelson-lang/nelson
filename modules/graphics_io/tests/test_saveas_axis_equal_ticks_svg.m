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
addpath([modulepath('graphics_io'), '/tests/helpers']);
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
out = [tempdir(), 'saveas_axis_equal_ticks_svg'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420], 'Color', [1 1 1]);
plot([0 2], [0 1]);
axis equal;
svgFile = [out, '/axis_equal_ticks.svg'];
saveas(f, svgFile);
close(f);
%=============================================================================
svgInfo = assert_valid_svg(svgFile, [560 420], {'polygon', 'line', 'text'});
asserts.istrue(numel(regexp(svgInfo.text, '<line ', 'match')) >= 4);
%=============================================================================
