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
%=============================================================================
out = [tempdir(), 'saveas_unicode_title'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
titleText = 'Unicode é €';
f = figure('Visible', 'off', 'Position', [100 100 420 315], 'Color', [1 1 1]);
plot(1:3, [1 4 2], 'LineWidth', 2);
title(titleText, 'Interpreter', 'none');
drawnow();
%=============================================================================
pngFile = [out, '/unicode_title.png'];
svgFile = [out, '/unicode_title.svg'];
pdfFile = [out, '/unicode_title.pdf'];
saveas(f, pngFile);
saveas(f, svgFile);
saveas(f, pdfFile);
close(f);
%=============================================================================
pngImage = imread(pngFile);
assert_title_band_has_text(pngImage);
svgInfo = assert_valid_svg(svgFile, [420 315], {'polyline', 'text'});
asserts.istrue(contains(svgInfo.text, titleText));
asserts.istrue(isfile(pdfFile));
asserts.istrue(file_size(pdfFile) > 1024);
%=============================================================================
function assert_title_band_has_text(img)
  gray = mean(double(img(:, :, 1:3)), 3);
  h = size(gray, 1);
  w = size(gray, 2);
  rows = 1:max(12, round(h * 0.12));
  cols = max(1, round(w * 0.22)):min(w, round(w * 0.78));
  band = gray(rows, cols);
  inkRatio = nnz(band < 180) / numel(band);
  asserts.istrue(inkRatio > 0.004);
end
%=============================================================================
function sz = file_size(filename)
  fid = fopen(filename, 'r');
  asserts.istrue(fid > 0);
  sz = fsize(fid);
  fclose(fid);
end
%=============================================================================
