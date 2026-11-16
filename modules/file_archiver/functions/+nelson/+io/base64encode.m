%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Encode bytes to a base64 character vector (RFC 4648, with padding). A numeric
% or logical array is taken as raw bytes; a char/string is encoded as its UTF-8
% bytes.
function out = base64encode(data)
  narginchk(1, 1);
  nargoutchk(0, 1);
  out = __base64encode__(data);
end
%=============================================================================
