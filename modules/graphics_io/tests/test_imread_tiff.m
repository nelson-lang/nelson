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
imfmt = imformats('tiff');
skip_testsuite(isempty(imfmt), _('No TIFF format supported.'))
%=============================================================================
images_path = [modulepath('graphics', 'tests'), '/images/'];
%=============================================================================
f = figure();
[img, map, alpha] = imread([images_path,'f14.tiff']);
assert_isequal(size(img), [   480   640     3]);
assert_isequal(size(alpha), [0, 0]);
imshow(img, map);
%=============================================================================
