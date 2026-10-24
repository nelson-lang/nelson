%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
% <--ADV-CLI MODE-->
%=============================================================================
A = ones(6, 8);
R = imresize(A, 0.5);
REF = ones(3, 4);
asserts.isapprox(R, REF, 1e-15);
%=============================================================================
A = ones(6, 8);
R = imresize(A, 0.5, 'bicubic');
REF = ones(3, 4);
asserts.isapprox(R, REF, 1e-15);
%=============================================================================
A = ones(6, 8);
R = imresize(A, 0.5, "bicubic");
REF = ones(3, 4);
asserts.isapprox(R, REF, 1e-15);
%=============================================================================
A = ones(6, 8);
R = imresize(A, [6, 8]);
asserts.isequal(A, R);
%=============================================================================
A = ones(6, 8);
R = imresize(A, [3, 4]);
REF = ones(3, 4);
asserts.isapprox(R, REF, 1e-15);
%=============================================================================
A = ones(6, 8);
R = imresize(A, [3, 4], 'box');
REF = ones(3, 4);
asserts.isapprox(R, REF, 1e-15);
%=============================================================================
A = ones(6, 8);
R = imresize(A, [3, NaN], 'box');
REF = ones(3, 4);
asserts.isapprox(R, REF, 1e-15);
%=============================================================================
A = ones(6, 8);
R = imresize(A, [NaN, 4], 'box');
REF = ones(3, 4);
asserts.isapprox(R, REF, 1e-15);
%=============================================================================
A = ones(6, 8);
methods = {'nearest', 'bilinear', 'bicubic', 'box', 'lanczos2', 'lanczos3'};
for k = 1:numel(methods)
  R = imresize(A, [3, 4], methods{k});
  REF = ones(3, 4);
  asserts.isapprox(R, REF, 1e-15);
end
%=============================================================================
images_path = [modulepath('graphics', 'tests'), '/images/'];
%=============================================================================
[img, map, alpha] = imread([images_path, 'winter-fox-64x64-indexed8.png']);
[img2, map2] = imresize(img, map, 30);
figure;
image(img2);
colormap(map2);
%=============================================================================
A = rand(20, 20);
R1 = imresize(A, 0.4, 'Antialiasing', true, 'bilinear');
R2 = imresize(A, 0.4, 'Antialiasing', false, 'bilinear');
assert(size(R1, 1) == 8 && size(R1, 2) == 8);
assert(size(R2, 1) == 8 && size(R2, 2) == 8);
assert(~isequal(R1, R2));
%=============================================================================
[img, map] = imread([images_path, 'winter-fox-64x64-indexed8.png']);
[img_opt, map_opt] = imresize(img, map, 32, 'Colormap', 'optimized');
[img_orig, map_orig] = imresize(img, map, 32, 'Colormap', 'original');
assert(size(map_opt, 2) == 3);
assert(size(map_orig, 2) == 3);
assert(size(map_opt, 1) <= size(map_orig, 1));
%=============================================================================
[img, map] = imread([images_path, 'winter-fox-64x64-indexed8.png']);
[img_dither, map_dither] = imresize(img, map, 32, 'Dither', true);
[img_nodither, map_nodither] = imresize(img, map, 32, 'Dither', false);
asserts.isequal(size(img_dither), size(img_nodither));
%=============================================================================
% 'Method' name-value pair is equivalent to the positional method.
A = reshape(single(1:48), 6, 8);
R_positional = imresize(A, [3, 4], 'bilinear');
R_namevalue = imresize(A, [3, 4], 'method', 'bilinear');
asserts.isequal(R_positional, R_namevalue);
%=============================================================================
% 'Method' is case-insensitive and accepts the supported methods.
A = ones(6, 8);
R1 = imresize(A, [3, 4], 'Method', 'nearest');
asserts.isequal(size(R1), [3, 4]);
R2 = imresize(A, [3, 4], 'method', 'bicubic');
asserts.isequal(size(R2), [3, 4]);
%=============================================================================
% An unknown method value is rejected.
A = ones(6, 8);
cmd = 'imresize(A, [3, 4], ''method'', ''nope'')';
asserts.checkerror(cmd, _('Unsupported "nope" option.'));
%=============================================================================
% A positional method may precede a name-value pair:
% imresize(A, scale, 'bicubic', 'Antialiasing', true).
A = ones(40, 40);
R = imresize(A, 0.5, 'bicubic', 'Antialiasing', true);
asserts.isequal(size(R), [20, 20]);
R = imresize(A, 0.5, "bicubic", "Antialiasing", true);
asserts.isequal(size(R), [20, 20]);
R = imresize(A, 0.5, 'bilinear', 'Antialiasing', false);
asserts.isequal(size(R), [20, 20]);
%=============================================================================
