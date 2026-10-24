%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = empty(varargin)
  % table.empty, table.empty(0, n), table.empty(m, 0) or table.empty([0 n]):
  % an empty table of that size, with n double variables Var1 ... Varn.
  sz = size(double.empty(varargin{:}));
  if length(sz) > 2
    error('Nelson:table:emptyMustBeTwoDims', _('A table must be two-dimensional.'));
  end
  if sz(2) > 0
    columns = repmat({zeros(0, 0)}, 1, sz(2));
    obj = table(columns{:});
  elseif sz(1) > 0
    obj = table(zeros(sz(1), 1));
    obj(:, 1) = [];
  else
    obj = table();
  end
end
%=============================================================================
