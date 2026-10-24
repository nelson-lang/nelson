%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = imerode(img, se)
  narginchk(2, 2);
  nargoutchk(0, 1);
  ip_assert_morphology_image(img);
  nhood = ip_nhood(se);
  out = builtin('imerode', img, nhood);
end
%=============================================================================
