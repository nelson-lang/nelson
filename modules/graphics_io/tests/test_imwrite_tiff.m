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
imfmt = imformats('tiff');
skip_testsuite(isempty(imfmt), _('No TIFF format supported.'))
%=============================================================================
path_dest = [tempdir(), 'imwrite_tests_tiff'];
[res, msg] = rmdir(path_dest, 's');
mkdir(path_dest);
images_path = [modulepath('graphics', 'tests'), '/images/'];
[img, map, alpha] = imread([images_path, 'winter-fox-64x64-indexed8.png']);
%=============================================================================
imwrite(img, map, [path_dest, 'image-tiff.tiff'], 'tiff');
assert_istrue(isfile([path_dest, 'image-tiff.tiff']));
%=============================================================================
