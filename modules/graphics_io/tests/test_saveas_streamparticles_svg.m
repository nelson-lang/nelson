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
addpath([modulepath('graphics_io'), '/tests/helpers']);
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
addpath([modulepath('graphics', 'tests')]);
out = [tempdir(), 'saveas_streamparticles_svg'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
vertices = {[0 0; 0.5 0.2; 1 0.5; 1.5 0.8]};
cases = {'streamparticles_1', 'streamparticles_2'};
for k = 1:numel(cases)
  f = figure('Visible', 'off', 'Position', [100 100 560 420], 'Color', [1 1 1]);
  if k == 1
    streamparticles(vertices);
  else
    streamparticles(vertices, 4, 'MarkerFaceColor', 'red');
  end
  drawnow();
  svgFile = [out, '/', cases{k}, '.svg'];
  saveas(f, svgFile);
  svgInfo = assert_valid_svg(svgFile, [560 420], 'circle');
  svgText = svgInfo.text;
  % One <circle> per particle; the radius follows the export resolution.
  markerElements = regexp(svgText, '<circle [^>]*r="[0-9.]+"', 'match');
  asserts.isequal(numel(markerElements), 4);
  if k == 2
    asserts.isequal(numel(regexp(svgText, '<circle [^>]*fill="rgba\(255,0,0,1\.000000\)"', 'match')), 4);
  end
  close(f);
end
%=============================================================================
