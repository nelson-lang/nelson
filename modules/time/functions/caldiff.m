%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = caldiff(t, components, dim)
  narginchk(1, 3);
  if nargin < 2
    components = 'months';
  end
  serial = datenum(t);
  if nargin < 3
    dim = find(size(serial) ~= 1, 1);
    if isempty(dim)
      dim = 1;
    end
  end
  idx1 = repmat({':'}, 1, ndims(serial));
  idx2 = idx1;
  idx1{dim} = 1:(size(serial, dim) - 1);
  idx2{dim} = 2:size(serial, dim);
  c = between(datetime(serial(idx1{:}), 'ConvertFrom', 'datenum'), ...
    datetime(serial(idx2{:}), 'ConvertFrom', 'datenum'), components);
end
%=============================================================================
