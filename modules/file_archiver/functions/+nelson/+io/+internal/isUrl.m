%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isUrl(source)
  % True for text starting with http:// or https://.
  tf = (ischar(source) || (isstring(source) && isscalar(source))) && ...
    (startsWith(source, 'http://', 'IgnoreCase', true) || startsWith(source, 'https://', 'IgnoreCase', true));
end
%=============================================================================
