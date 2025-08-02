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
path_dest = [tempdir(), 'imwrite_tests'];
[res, msg] = rmdir(path_dest, 's');
mkdir(path_dest);
images_path = [modulepath('graphics', 'tests'), '/images/'];
[img, map, alpha] = imread([images_path, 'winter-fox-64x64-indexed8.png']);
%=============================================================================
imwrite(img, map, [path_dest, 'indexed8_1.png']);
assert_istrue(isfile([path_dest, 'indexed8_1.png']));
%=============================================================================
imwrite(img, map, [path_dest, 'indexed8_2.png'], 'png');
assert_istrue(isfile([path_dest, 'indexed8_2.png']));
%=============================================================================
imwrite(img, map, [path_dest, 'indexed8_3.png'], 'Comments', 'Nelson');
assert_istrue(isfile([path_dest, 'indexed8_3.png']));
%=============================================================================
imwrite(img, map, [path_dest, 'indexed8_4.png'], 'png', 'Comments', 'Nelson');
assert_istrue(isfile([path_dest, 'indexed8_4.png']));
%=============================================================================
