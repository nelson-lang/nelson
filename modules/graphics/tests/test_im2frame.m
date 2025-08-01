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
%=============================================================================
images_path = [modulepath('graphics', 'tests'), '/images/'];
%=============================================================================
[img, map, alpha] = imread([images_path, 'winter-fox-64x64-indexed8.png']);
F = im2frame(img, map);

REF_colormap = [0.6314    0.7922    0.8627;
0.6196    0.3216    0.2235;
1.0000    1.0000    1.0000;
0.5294    0.6941    0.7725;
0.4784    0.2235    0.1843;
0.7922    0.6627    0.6196;
0.5922    0.3137    0.2235;
     0         0         0;
0.8902    0.7686    0.7255;
0.6471    0.6471    0.6471;
0.5804    0.5804    0.5804;
0.3373    0.3608    0.3961;
0.1451    0.1490    0.1569;
0.2353    0.2471    0.2667;
0.5922    0.7490    0.8275;
0.6314    0.4784    0.4314;
0.6235    0.4824    0.4353];
assert_isapprox(F.colormap, REF_colormap, 1e-4);
assert_isapprox(F.colormap, map, 1e-4);
assert_isequal(size(F.cdata), [64, 64]);
assert_isequal(class(F.cdata), 'uint8');
%=============================================================================
[img, map, alpha] = imread([images_path,'winter-fox-64x64-rgb32.jpg']);
F = im2frame(img, map);
assert_isequal(size(F.cdata), [64, 64, 3]);
assert_isequal(size(F.colormap), [0, 0]);
assert_isequal(F.cdata, img);
%=============================================================================
