%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function td = tdigest(varargin)
  % Default values
  x = [];
  compression = 100;
  narginchk(0, 2);
  if (nargin() == 1)
    arg = varargin{1};
    if (isscalar(arg) && isnumeric(arg))
      compression = arg;
    elseif (isnumeric(arg))
      x = arg;
    else
      error(_('Invalid input argument. Expected a numeric array or a scalar compression value.'));
    end
  elseif (nargin() == 2)
    compression = varargin{1};
    x = varargin{2};
    if ~isscalar(compression) || ~isnumeric(compression) || compression <= 0
      error(_('Compression parameter must be a positive scalar number.'));
    end
    if ~isnumeric(x)
      error(_('Data must be numeric.'));
    end
  end
  
  % Create t-digest
  if ~isempty(x)
    % Remove NaN values
    valid_x = x(~isnan(x));
    if ~isempty(valid_x)
      td = feval('@tdigest/create_tdigest', compression, numel(valid_x), valid_x(:), ones(numel(valid_x), 1));
    else
      td = feval('@tdigest/create_tdigest', compression, 0, [], []);
    end
  else
    td = feval('@tdigest/create_tdigest', compression, 0, [], []);
  end
  
  % Compress if needed
  if feval('@tdigest/shouldcompress', td)
    td = feval('@tdigest/compress', td);
  end
end
%=============================================================================
