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
f = figure();
[img, map, alpha] = imread([images_path, 'winter-fox-64x64-rgba32-gray.png']);
asserts.isequal(size(img), [64, 64, 3]);
asserts.isequal(size(alpha), [64, 64]);
asserts.isequal(min(alpha, [], 'all'), max(alpha, [], 'all'))
asserts.isequal(min(alpha, [], 'all'), uint8(255));
imshow(img, map);
%=============================================================================
