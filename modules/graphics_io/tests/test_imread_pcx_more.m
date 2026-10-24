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
images = {'BUGLE.PCX', 'FEDEAGL1.PCX'};
for i = 1:length(images)
  img = imread([images_path, images{i}]);
  subplot(2, 1, i);
  imagesc(img);
end
%=============================================================================
