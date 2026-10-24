%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sos = tf2sos(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  [z, p, k] = tf2zp(varargin{1}, varargin{2});
  sos = zp2sos(z, p, k);
end
%=============================================================================
