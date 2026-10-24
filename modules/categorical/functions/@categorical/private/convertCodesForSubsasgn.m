%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [codes, names] = convertCodesForSubsasgn(codes, sourceNames, targetNames, isProtected)
  names = targetNames(:);
  map = zeros(length(sourceNames), 1);
  for k = 1:length(sourceNames)
    idx = find(strcmp(names, sourceNames{k}), 1);
    if isempty(idx)
      if isProtected
        error('Nelson:categorical:cannotAddToProtected', 'Cannot add a category to a protected categorical array.');
      end
      names{end + 1, 1} = sourceNames{k};
      idx = length(names);
    end
    map(k) = idx;
  end
  for k = 1:numel(codes)
    code = codes(k);
    if code > 0
      codes(k) = map(code);
    end
  end
end
%=============================================================================
