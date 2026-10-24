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
% image/imagesc go through RenderInterface::drawImage, which carries an
% RGBAImageView (not a QImage): RenderWeb PNG-encodes it
% with stb and inlines a data URI. The frame must show the colored raster.
f = figure();
imagesc(magic(8));
F = getframe(gcf);
assert_isequal(class(F.cdata), 'uint8');
assert_isequal(size(F.cdata, 3), 3);
% a parula-mapped magic square has strongly colored cells: red and blue
% channels diverge a lot somewhere.
r = double(F.cdata(:, :, 1));
b = double(F.cdata(:, :, 3));
assert_istrue(max(max(abs(r - b))) > 100);
% and the image also exports as a valid PNG in web mode.
out = [tempname(), '.png'];
saveas(f, out);
assert_istrue(isfile(out));
info = dir(out);
assert_istrue(info.bytes > 2000);
close(f);
rmfile(out);
%=============================================================================
