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
% getframe in WEB_GUI mode goes through the Qt-free software rasterizer
% (display list -> RGB). The frame must be a plausible rendering, not a
% uniform buffer.
f = figure();
plot(1:10, sin(1:10), 'b-o');
title('web getframe');
F = getframe(gcf);
assert_istrue(isstruct(F));
sz = size(F.cdata);
assert_isequal(class(F.cdata), 'uint8');
assert_isequal(length(sz), 3);
assert_isequal(sz(3), 3);
assert_istrue(sz(1) > 100 && sz(2) > 100);
% the image must contain ink (not uniform) and a white plot background
assert_istrue(double(min(F.cdata(:))) < 128);
assert_istrue(double(max(F.cdata(:))) == 255);
close(f);
%=============================================================================
% 3D: surf must rasterize with real content (colored polygons)
g = figure();
surf(peaks);
F2 = getframe(gcf);
sz2 = size(F2.cdata);
assert_isequal(sz2(3), 3);
% a surf frame has strongly colored pixels (parula blues/yellows): the min
% over channels somewhere must be far from grayscale background
r = double(F2.cdata(:, :, 1));
b = double(F2.cdata(:, :, 3));
assert_istrue(max(max(abs(r - b))) > 60);
close(g);
%=============================================================================
