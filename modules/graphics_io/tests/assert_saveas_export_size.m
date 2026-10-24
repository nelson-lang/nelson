function assert_saveas_export_size(width, height)
  %=============================================================================
  % Copyright (c) 2016-present Allan CORNET (Nelson)
  %=============================================================================
  % This file is part of Nelson.
  %=============================================================================
  % LICENCE_BLOCK_BEGIN
  % SPDX-License-Identifier: LGPL-3.0-or-later
  % LICENCE_BLOCK_END
  %=============================================================================
  out = [tempdir(), 'saveas_export_size'];
  if ~isfolder(out)
    mkdir(out);
  end
  expectedWidth = ceil(width * 150 / 96);
  expectedHeight = ceil(height * 150 / 96);

  pngFile = [out, '/figure_', int2str(width), 'x', int2str(height), '.png'];
  svgFile = [out, '/figure_', int2str(width), 'x', int2str(height), '.svg'];

  f = figure('Visible', 'off', 'Position', [100 100 width height], 'Color', [1 1 1]);
  surf(peaks);
  drawnow();
  saveas(f, pngFile);
  saveas(f, svgFile);
  close(f);

  pngImage = imread(pngFile);
  asserts.isequal(size(pngImage, 1), expectedHeight);
  asserts.isequal(size(pngImage, 2), expectedWidth);

  % The SVG declares the logical figure size (so a viewer renders it at its
  % intended on-screen dimensions) while the viewBox keeps the export DPI
  % resolution used for every primitive coordinate. Both declared dimensions are
  % the export resolution scaled by the inverse export ratio, so the declared
  % aspect ratio stays equal to the viewBox.
  txt = fileread(svgFile);
  pattern = '<svg width="([0-9.]+)" height="([0-9.]+)" viewBox="0 0 ([0-9]+) ([0-9]+)"';
  tokens = regexp(txt, pattern, 'tokens', 'once');
  asserts.isfalse(isempty(tokens));
  asserts.istrue(abs(str2double(tokens{1}) - expectedWidth * 96 / 150) < 1e-3);
  asserts.istrue(abs(str2double(tokens{2}) - expectedHeight * 96 / 150) < 1e-3);
  asserts.isequal(str2double(tokens{3}), expectedWidth);
  asserts.isequal(str2double(tokens{4}), expectedHeight);
  %=============================================================================
