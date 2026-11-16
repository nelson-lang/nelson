%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Decode a base64 character vector / string to a uint8 row vector (RFC 4648).
function out = base64decode(str)
  narginchk(1, 1);
  nargoutchk(0, 1);
  out = __base64decode__(str);
end
%=============================================================================
