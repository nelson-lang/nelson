%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
images_path = [modulepath('graphics', 'tests'), '/images/'];
%=============================================================================
f = figure();
[img, map] = imread([images_path, 'winter-fox-64x64-rgba32-gray.png']);
h = imshow(img);
asserts.istrue(isgraphics(h, 'image'));
%=============================================================================
f = figure();
[img, map] = imread([images_path, 'winter-fox-64x64-indexed8.png']);
h = imshow(img, map);
asserts.istrue(isgraphics(h, 'image'));
h = imshow(img, map, 'Colormap', summer);
asserts.istrue(isgraphics(h, 'image'));
close all;
%=============================================================================
