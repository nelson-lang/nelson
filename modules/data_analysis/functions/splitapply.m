%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = splitapply(func, varargin)
  narginchk(2, inf);
  if ~isa(func, 'function_handle')
    error('Nelson:data_analysis:firstInputFunctionHandle', '%s must be a function handle.', 'First input');
  end
  G = varargin{end};
  data = varargin(1:end - 1);
  Gv = G(:);
  finite = Gv(~isnan(Gv));
  if isempty(finite)
    ngroups = 0;
  else
    ngroups = max(finite);
  end
  nout = max(nargout, 1);
  varargout = cell(1, nout);
  collected = cell(ngroups, nout);
  % Group the rows in a single pass instead of scanning Gv == k for every group
  % (which is O(ngroups * nrows)). Sorting by group id then original row index
  % keeps each group's rows in their original order; cumulative counts give
  % each group's contiguous slice.
  rowOrder = zeros(0, 1);
  edges = zeros(ngroups + 1, 1);
  if ngroups > 0
    validIdx = find(~isnan(Gv));
    gValid = Gv(validIdx);
    [~, sp] = sortrows([gValid, validIdx]);
    rowOrder = validIdx(sp);
    edges = [0; cumsum(accumarray(gValid, 1, [ngroups, 1]))];
  end
  for k = 1:ngroups
    rows = rowOrder(edges(k) + 1:edges(k + 1));
    args = cell(1, numel(data));
    for d = 1:numel(data)
      col = data{d};
      args{d} = col(rows, :);
    end
    outs = cell(1, nout);
    [outs{1:nout}] = func(args{:});
    for o = 1:nout
      collected{k, o} = outs{o};
    end
  end
  for o = 1:nout
    if ngroups == 0
      varargout{o} = [];
    else
      varargout{o} = vertcat(collected{:, o});
    end
  end
end
%=============================================================================
