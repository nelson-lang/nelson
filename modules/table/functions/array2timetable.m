%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function TT = array2timetable(A, varargin)
  narginchk(1, Inf);
  opts = parseOptions(varargin{:});
  if isempty(opts.RowTimes)
    error('Nelson:table:invalidSyntax', 'RowTimes must be specified.');
  end
  T = array2table(A);
  if ~isempty(opts.VariableNames)
    T.Properties.VariableNames = opts.VariableNames;
  end
  TT = timetable.fromTableAndTimes(T, opts.RowTimes, []);
end
%=============================================================================
function opts = parseOptions(varargin)
  opts.RowTimes = [];
  opts.VariableNames = {};
  k = 1;
  while k <= nargin
    name = varargin{k};
    if isstring(name)
      name = char(name);
    end
    if ~ischar(name) || k == nargin
      error('Nelson:table:nameValuePair', 'Name-value argument expected.');
    end
    switch lower(name)
      case 'rowtimes'
        opts.RowTimes = varargin{k + 1};
      case 'variablenames'
        opts.VariableNames = varargin{k + 1};
      otherwise
        error('Nelson:table:unsupportedOption', 'Unsupported "%s" option.', 'array2timetable');
    end
    k = k + 2;
  end
end
%=============================================================================
