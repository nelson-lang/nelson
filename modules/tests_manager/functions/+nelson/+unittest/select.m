%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function suite = select(suite, varargin)
  options = select_parseSelectOptions(varargin);
  initialCount = length(suite.cases);
  selected = struct([]);
  for k = 1:length(suite.cases)
    if select_isSelected(suite.cases(k), options)
      selected = [selected; suite.cases(k)];
    end
  end
  suite.cases = selected;
  suite.count = length(suite.cases);
  suite.filtered = initialCount - suite.count;
end
