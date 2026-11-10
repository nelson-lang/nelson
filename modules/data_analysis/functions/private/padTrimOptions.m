%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [lengths, dims, opts] = padTrimOptions(A, m, allowPadding, args)
  % parses the length and the name-value options shared by paddata and trimdata
  if ~(isnumeric(m) && isvector(m) && isreal(m) && all(m >= 0) && all(m == fix(m)))
    error('Nelson:padtrim:invalidLength', 'Length must be a vector of nonnegative integers.');
  end
  opts.Side = 'trailing';
  opts.Pattern = 'constant';
  opts.FillValue = [];
  opts.HasFill = false;
  dimension = 'auto';
  if mod(numel(args), 2) ~= 0
    error('Nelson:padtrim:invalidOption', 'Options must be name-value pairs.');
  end
  names = {'Side', 'Dimension'};
  if allowPadding
    names = [names, {'Pattern', 'FillValue'}];
  end
  for k = 1:2:numel(args)
    name = args{k};
    value = args{k + 1};
    if ~(ischar(name) || isstring(name)) || ~any(strcmpi(char(name), names))
      error('Nelson:padtrim:invalidOption', 'Options must be name-value pairs.');
    end
    switch lower(char(name))
      case 'side'
        if ~((ischar(value) || isstring(value)) && any(strcmpi(char(value), {'leading', 'trailing', 'both'})))
          error('Nelson:padtrim:invalidSide', 'Side must be ''leading'', ''trailing'' or ''both''.');
        end
        opts.Side = lower(char(value));
      case 'pattern'
        patterns = {'constant', 'edge', 'circular', 'flip', 'reflect'};
        if ~((ischar(value) || isstring(value)) && any(strcmpi(char(value), patterns)))
          message = 'Pattern must be ''constant'', ''edge'', ''circular'', ''flip'' or ''reflect''.';
          error('Nelson:padtrim:invalidPattern', message);
        end
        opts.Pattern = lower(char(value));
      case 'fillvalue'
        opts.FillValue = value;
        opts.HasFill = true;
      otherwise
        dimension = value;
    end
  end
  m = double(m(:).');
  if (ischar(dimension) || isstring(dimension)) && strcmpi(char(dimension), 'auto')
    if isscalar(m)
      dims = find(size(A) ~= 1, 1);
      if isempty(dims)
        dims = 1;
      end
    else
      dims = 1:numel(m);
    end
  elseif isnumeric(dimension) && isvector(dimension) && all(dimension >= 1) && all(dimension == fix(dimension))
    dims = double(dimension(:).');
    if ~isscalar(m) && numel(m) ~= numel(dims)
      error('Nelson:padtrim:invalidDimension', 'Dimension must be ''auto'' or positive integers matching the lengths.');
    end
  else
    error('Nelson:padtrim:invalidDimension', 'Dimension must be ''auto'' or positive integers matching the lengths.');
  end
  lengths = m;
  if isscalar(lengths)
    lengths = repmat(lengths, 1, numel(dims));
  end
end
%=============================================================================
