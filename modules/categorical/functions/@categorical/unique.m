%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [C, ia, ic] = unique(A, varargin)
  % <undefined> (code 0) is a missing value: each one is distinct and sorted
  % last, unless 'TreatMissingAsDistinct' is false.
  codes = A.codes(:);
  undefinedMask = (codes == 0);
  codes(undefinedMask) = NaN;
  [codes, ia, ic] = unique(codes, varargin{:});
  codes(isnan(codes)) = 0;
  C = categorical.fromCodes(codes, A.categoryNames, A.isOrdinal, A.isProtected);
end
%=============================================================================
