%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
images_path = [modulepath('graphics', 'tests'), '/images/'];
f = figure();
[img, map, alpha] = imread([images_path, 'winter-fox-64x64-indexed8.png']);
assert_isequal(size(img), [64, 64]);
assert_istrue(isa(img, 'uint8'));
assert_isequal(size(map), [17, 3]);
assert_istrue(isdouble(map));
assert_isequal(size(alpha), [64, 64]);
assert_isequal(min(alpha,[],'all'), max(alpha,[],'all'))
imshow(img, map);
%=============================================================================
f = figure();
[img, map, alpha] = imread([images_path,'winter-fox-64x64-rgb32.jpg']);
assert_isequal(size(img), [64, 64, 3]);
assert_istrue(isa(img, 'uint8'));
assert_isequal(size(map), [0, 0]);
assert_isequal(size(alpha), [0, 0]);
imshow(img, map);
%=============================================================================
f = figure();
[img, map, alpha] = imread([images_path,'winter-fox-64x64-rgb32-gray.jpg']);
assert_isequal(size(img), [64, 64, 3]);
assert_isequal(size(map), [0, 0]);
assert_isequal(size(alpha), [0, 0]);
imshow(img, map);
%=============================================================================
f = figure();
[img, map, alpha] = imread([images_path,'winter-fox-64x64-rgba32.png']);
assert_isequal(size(img), [64, 64, 3]);
assert_isequal(size(alpha), [64, 64]);
assert_isequal(min(alpha,[],'all'), max(alpha,[],'all'))
assert_isequal(min(alpha,[],'all'), uint8(255));
imshow(img, map);
%=============================================================================
f = figure();
[img, map, alpha] = imread([images_path,'winter-fox-64x64-rgba32-gray.png']);
assert_isequal(size(img), [64, 64, 3]);
assert_isequal(size(alpha), [64, 64]);
assert_isequal(min(alpha,[],'all'), max(alpha,[],'all'))
assert_isequal(min(alpha,[],'all'), uint8(255));
imshow(img, map);
%=============================================================================
f = figure();
subplot(2, 1, 1);
img1 =imread([images_path, 'cube.jpg']);
imagesc(img1);
subplot(2, 1, 2);
img2 =imread([images_path, 'teaset.png']);
imagesc(img2);
%=============================================================================
f = figure();
images = {'input.pcx', 'abydos.pcx', 'flag-76.PCX', 'BUGLE.PCX', 'FEDEAGL1.PCX'};
for i = 1:length(images)
    img = imread([images_path, images{i}]);
    subplot(3, 2, i);
    imagesc(img);
end
%=============================================================================
