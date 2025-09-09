%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function td_out = plus(td1, td2)
  if isa(td1, 'tdigest') && isnumeric(td2)
    td_out = add(td1, td2(:)');
    return;
  end
  if isa(td2, 'tdigest') && isnumeric(td1)
    td_out = add(td2, td1(:)');
    return;
  end
  if isa(td1, 'tdigest') && isa(td2, 'tdigest')
    st2 = struct(td2);
    td_out = add(td1, st2.means, st2.weights);
    return;
  end
  error(_("Combining 'tdigest' objects is not allowed as they are designed to be scalar."));
end
%=============================================================================
function td = add(varargin)
  td = varargin{1};
  x = varargin{2};
  if (nargin < 3)
    w = 1;
  else
    w = varargin{3};
  end
  
  % Remove NaN values from x
  valid_idx = ~isnan(x(:));
  x_valid = x(valid_idx);
  
  % If no valid values, return original td
  if isempty(x_valid)
    return;
  end
  
  st = struct(td);
  % append all new valid values as individual centroids
  means   = [st.means;   x_valid(:)]; % column vector
  weights = [st.weights; w * ones(numel(x_valid),1)];
  
  totalWeight = st.totalWeight + numel(x_valid) * w;
  % compress if too many centroids
  if feval('@tdigest/shouldcompress',td)
    td = feval('@tdigest/create_tdigest', st.compression, totalWeight, means, weights);
    td = feval('@tdigest/compress', td);
  else
    td = feval('@tdigest/create_tdigest', st.compression, totalWeight, means, weights);
  end
end
%=============================================================================
