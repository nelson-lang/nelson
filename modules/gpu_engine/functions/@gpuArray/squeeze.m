%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = squeeze(x)
  % remove singleton dimensions; 2-D inputs are returned unchanged. Composed
  % from reshape so the data stays resident on the device.
  sz = size(x);
  if numel(sz) <= 2
    r = x;
    return
  end
  newsz = sz(sz ~= 1);
  if numel(newsz) < 2
    newsz = [newsz, ones(1, 2 - numel(newsz))];
  end
  r = reshape(x, newsz);
end
%=============================================================================
