%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function codes = single(A)
  codes = single(A.codes);
  % <undefined> elements (code 0) convert to NaN
  codes(codes == 0) = NaN;
end
%=============================================================================
