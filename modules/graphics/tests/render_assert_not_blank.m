%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function render_assert_not_blank(img)
  rgb = img(:, :, 1:3);
  values = double(rgb(:));
  dynamicRange = max(values) - min(values);
  nonWhiteRatio = sum(values < 248) / numel(values);
  asserts.istrue(dynamicRange > 8);
  asserts.istrue(nonWhiteRatio > 0.002);
end
%=============================================================================
