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
f = figure();
subplot(2, 1, 1);
img1 = imread([images_path, 'cube.jpg']);
imagesc(img1);
subplot(2, 1, 2);
img2 = imread([images_path, 'teaset.png']);
imagesc(img2);
%=============================================================================
