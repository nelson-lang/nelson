%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% isstr is a deprecated alias for ischar, kept for compatibility with
% legacy toolboxes (e.g. the Bayes Net Toolbox).
function tf = isstr(x)
  narginchk(1, 1);
  nargoutchk(0, 1);
  tf = ischar(x);
end
%=============================================================================
