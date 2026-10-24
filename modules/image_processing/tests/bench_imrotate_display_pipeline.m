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
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
addpath([fileparts(nfilename('fullpathext'), 'path'), '/private']);
close all;
framesCount = 120;
RGB64 = uint8(reshape(mod(1:(64 * 64 * 3), 256), [64 64 3]));
alpha64 = uint8(reshape(mod(1:(64 * 64), 256), [64 64 1]));
RGBA64 = uint8(zeros(64, 64, 4));
RGBA64(:, :, 1:3) = RGB64;
RGBA64(:, :, 4) = alpha64;
asserts.isequal(class(RGBA64), 'uint8');
frames = cell(1, framesCount);
framesRgba = cell(1, framesCount);
%=============================================================================
timerStart = tic();
for k = 1:framesCount
  angle = mod(k - 1, 360);
  frames{k} = imrotate(RGB64, angle, 'bilinear', 'crop');
end
rotateTime = toc(timerStart);
ip_bench_report('imrotate_display_rotate_only', rotateTime, 1.0);
asserts.isequal(size(frames{end}), [64 64 3]);
asserts.isequal(class(frames{end}), 'uint8');
%=============================================================================
timerStart = tic();
for k = 1:framesCount
  angle = mod(k - 1, 360);
  framesRgba{k} = imrotate(RGBA64, angle, 'bilinear', 'crop');
end
rotateRgbaTime = toc(timerStart);
ip_bench_report('imrotate_display_rgba_rotate_only', rotateRgbaTime, 1.5);
asserts.isequal(size(framesRgba{end}), [64 64 4]);
asserts.isequal(class(framesRgba{end}), 'uint8');
%=============================================================================
f = figure('Visible', 'off');
ii = imshow(RGB64);
asserts.istrue(isgraphics(ii, 'image'));
drawnow();
%=============================================================================
timerStart = tic();
for k = 1:framesCount
  set(ii, 'CData', frames{k});
end
setTime = toc(timerStart);
ip_bench_report('imrotate_display_set_cdata_only', setTime, 1.0);
asserts.isequal(size(ii.CData), size(frames{end}));
asserts.isequal(double(ii.CData), double(frames{end}));
%=============================================================================
timerStart = tic();
for k = 1:framesCount
  set(ii, 'CData', frames{k});
  refresh(f);
end
refreshTime = toc(timerStart);
ip_bench_report('imrotate_display_set_cdata_refresh', refreshTime, 5.0);
asserts.istrue(isgraphics(f, 'figure'));
close(f);
drawnow();
%=============================================================================
f = figure('Visible', 'off');
ii = imshow(RGB64);
drawnow();
timerStart = tic();
for k = 1:framesCount
  angle = mod(k - 1, 360);
  set(ii, 'CData', imrotate(RGB64, angle, 'bilinear', 'crop'));
end
rotateSetTime = toc(timerStart);
ip_bench_report('imrotate_display_rotate_set_cdata', rotateSetTime, 2.0);
asserts.isequal(size(ii.CData), size(frames{end}));
asserts.isequal(double(ii.CData), double(frames{end}));
%=============================================================================
timerStart = tic();
for k = 1:framesCount
  angle = mod(k - 1, 360);
  set(ii, 'CData', imrotate(RGB64, angle, 'bilinear', 'crop'));
  refresh(f);
end
rotateRefreshTime = toc(timerStart);
ip_bench_report('imrotate_display_rotate_set_cdata_refresh', rotateRefreshTime, 7.0);
close(f);
drawnow();
%=============================================================================
f = figure('Visible', 'off');
ii = image('CData', RGBA64);
drawnow();
timerStart = tic();
for k = 1:framesCount
  angle = mod(k - 1, 360);
  set(ii, 'CData', imrotate(RGBA64, angle, 'bilinear', 'crop'));
  refresh(f);
end
rgbaPipelineTime = toc(timerStart);
ip_bench_report('imrotate_display_rgba_rotate_set_cdata_refresh', rgbaPipelineTime, 9.0);
asserts.isequal(size(ii.CData), size(framesRgba{end}));
asserts.isequal(double(ii.CData), double(framesRgba{end}));
close(f);
drawnow();
close all;
%=============================================================================
