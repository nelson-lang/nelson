%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isequal(A, varargin)
  tf = true;
  for k = 1:length(varargin)
    B = varargin{k};
    if ~isa(B, 'categorical') || ~isequal(size(A.codes), size(B.codes)) ...
        || A.isOrdinal ~= B.isOrdinal || A.isProtected ~= B.isProtected ...
        || ~isequal(A.categoryNames, B.categoryNames) || ~isequal(A.codes, B.codes)
      tf = false;
      return
    end
  end
end
%=============================================================================
