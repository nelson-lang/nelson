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
path_dest = [tempdir(), 'imwrite_tests/'];
[res, msg] = rmdir(path_dest, 's');
mkdir(path_dest);
images_path = [modulepath('graphics', 'tests'), '/images/'];
%=============================================================================
[img_REF, map_REF, alpha_REF] = imread([images_path, 'winter-fox-64x64-indexed8.png']);
imwrite(img_REF, map_REF, [path_dest, 'winter-fox-64x64-indexed8.png']);
[img, map, alpha] = imread([path_dest, 'winter-fox-64x64-indexed8.png']);
assert_isequal(img, img_REF);
assert_isequal(map, map_REF);
%=============================================================================
[img_REF, map_REF, alpha_REF] = imread([images_path,'winter-fox-64x64-rgba32.png']);
imwrite(img_REF, [path_dest, 'winter-fox-64x64-rgba32.png']);
[img, map, alpha] = imread([path_dest, 'winter-fox-64x64-rgba32.png']);
assert_isequal(img, img_REF);
assert_isequal(map, map_REF);
%=============================================================================
[img_REF, map_REF, alpha_REF] = imread([images_path,'winter-fox-64x64-rgba32-gray.png']);
imwrite(img_REF, [path_dest, 'winter-fox-64x64-rgba32-gray.png']);
[img, map, alpha] = imread([path_dest, 'winter-fox-64x64-rgba32-gray.png']);
assert_isequal(img, img_REF);
assert_isequal(map, map_REF);
%=============================================================================
[img_REF, map_REF, alpha_REF] = imread([images_path, 'winter-fox-64x64-indexed8.png']);
imwrite(img_REF, map_REF, [path_dest, 'winter-fox-64x64-indexed8.pcx'])
[img, map, alpha] = imread([path_dest, 'winter-fox-64x64-indexed8.pcx']);
figure();
imshow(img, map)
%=============================================================================
[img_REF, map_REF, alpha_REF] = imread([images_path,'winter-fox-64x64-rgba32.png']);
imwrite(img_REF, [path_dest, 'winter-fox-64x64-rgba32.pcx']);
[img, map, alpha] = imread([path_dest, 'winter-fox-64x64-rgba32.pcx']);
figure();
imshow(img, map);
assert_isequal(img,img_REF)
%=============================================================================
[img_REF, map_REF, alpha_REF] = imread([images_path,'winter-fox-64x64-rgba32-gray.png']);
imwrite(img_REF, [path_dest, 'winter-fox-64x64-rgba32-gray.pcx']);
[img, map, alpha] = imread([path_dest, 'winter-fox-64x64-rgba32-gray.pcx']);
figure();
imshow(img, map);
assert_isequal(img,img_REF)
%=============================================================================
