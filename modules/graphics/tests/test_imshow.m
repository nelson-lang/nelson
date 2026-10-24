%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
images_path = [modulepath('graphics', 'tests'), '/images/'];
%=============================================================================
f = figure();
h = imshow([images_path, 'winter-fox-64x64-indexed8.png']);
asserts.istrue(isgraphics(h, 'image'));
%=============================================================================
f = figure();
h = imshow([images_path, 'winter-fox-64x64-rgb32.jpg']);
asserts.istrue(isgraphics(h, 'image'));
%=============================================================================
f = figure();
h = imshow([images_path, 'winter-fox-64x64-rgb32-gray.jpg']);
asserts.istrue(isgraphics(h, 'image'));
%=============================================================================
f = figure();
h = imshow([images_path, 'winter-fox-64x64-rgba32.png']);
asserts.istrue(isgraphics(h, 'image'));
%=============================================================================
f = figure();
h = imshow([images_path, 'winter-fox-64x64-rgba32-gray.png']);
asserts.istrue(isgraphics(h, 'image'));
close all;
%=============================================================================
