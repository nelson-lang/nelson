%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [C, ia, ic] = tableUnique(T, varargin)
  % Each variable column is replaced by its sorted-order group codes, so the
  % unique rows of the code matrix follow sortrows semantics across variables
  % while 'sorted'/'stable'/'first'/'last' are handled by the numeric unique.
  distinctMissing = parseTableUniqueOptions(varargin);
  st = struct(T);
  nRows = height(T);
  names = st.Properties.VariableNames;
  codes = zeros(nRows, 0);
  for k = 1:length(names)
    codes = [codes, tableVariableCodes(st.data.(names{k}), nRows, distinctMissing)];
  end
  if nRows == 0
    % still goes through unique so the options are validated
    [~, ia, ic] = unique(zeros(0, 1), varargin{:});
  elseif isempty(codes)
    % no variables: every row is the same
    [~, ia, ic] = unique(ones(nRows, 1), varargin{:});
  else
    [~, ia, ic] = unique(codes, 'rows', varargin{:});
  end
  ia = ia(:);
  ic = ic(:);
  C = T(ia, :);
end
%=============================================================================
function distinctMissing = parseTableUniqueOptions(args)
  distinctMissing = true;
  k = 1;
  while k <= length(args)
    arg = args{k};
    if (ischar(arg) || (isstring(arg) && isscalar(arg))) && strcmpi(arg, 'TreatMissingAsDistinct') && k < length(args)
      distinctMissing = logical(args{k + 1});
      k = k + 2;
      continue;
    end
    if (ischar(arg) || (isstring(arg) && isscalar(arg))) && any(strcmpi(arg, {'legacy', 'R2012a'}))
      error('Nelson:table:unique:unsupportedOption', 'The ''R2012a'' and ''legacy'' options are not supported.');
    end
    k = k + 1;
  end
end
%=============================================================================
