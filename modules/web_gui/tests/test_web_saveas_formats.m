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
% Every raster export in WEB_GUI mode passes through the graphics_io codec
% registry. SVG and PDF retain their dedicated vector exporters.
out = tempname();
if ~isfolder(out)
  mkdir(out);
end
cleanupOut = onCleanup(@() rmdir(out, 's')); %#ok<NASGU>
f = figure();
plot(1:10, (1:10) .^ 2, 'r-o');
title('formats');
%=============================================================================
rasterFormats = {'png', 'jpg', 'gif', 'webp', 'tiff', 'bmp', 'tga', ...
                 'pbm', 'pgm', 'ppm', 'pnm', 'pcx'};
referenceSize = [];
for k = 1:length(rasterFormats)
  format = rasterFormats{k};
  filename = [out, '/fig.', format];
  saveas(f, filename);
  assert_istrue(isfile(filename));
  decoded = imread(filename);
  currentSize = [size(decoded, 1), size(decoded, 2)];
  if isempty(referenceSize)
    referenceSize = currentSize;
  end
  assert_isequal(currentSize, referenceSize);
end
%=============================================================================
svgFile = [out, '/fig.svg'];
saveas(f, svgFile);
assert_istrue(isfile(svgFile));
svgText = fileread(svgFile);
assert_istrue(contains(svgText, '<svg'));
assert_istrue(contains(svgText, '<polyline'));
assert_istrue(contains(svgText, '<text'));
%=============================================================================
pdfFile = [out, '/fig.pdf'];
saveas(f, pdfFile);
assert_istrue(isfile(pdfFile));
fid = fopen(pdfFile, 'rb');
magic = fread(fid, 5, 'uint8');
fclose(fid);
assert_isequal(char(magic'), '%PDF-');
%=============================================================================
defaultFile = [out, '/fig_default'];
saveas(f, defaultFile);
assert_istrue(isfile([defaultFile, '.png']));
forcedFile = [out, '/fig_forced'];
saveas(f, forcedFile, 'webp');
assert_istrue(isfile([forcedFile, '.webp']));
% Exercise the backend half of the clipboard.image channel. Browser-side
% permission, denial and gesture-retry behavior is covered by the frontend test.
copygraphics(f);
close(f);
%=============================================================================
