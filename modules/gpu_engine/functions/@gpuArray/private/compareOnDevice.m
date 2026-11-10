%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = compareOnDevice(args, nanEqual)
  % [] when an input needs the host comparison (text, cell, sparse, complex...)
  tf = [];
  if numel(args) < 2 || ~all(cellfun(@isDeviceComparable, args))
    return;
  end
  tf = true;
  ref = args{1};
  for k = 2:numel(args)
    other = args{k};
    if ~isequal(size(ref), size(other))
      tf = false;
      return;
    end
    same = ref(:) == other(:);
    if nanEqual
      same = same | (isnan(ref(:)) & isnan(other(:)));
    end
    if ~gather(all(same))
      tf = false;
      return;
    end
  end
end
%=============================================================================
function tf = isDeviceComparable(x)
  % real dense floating-point or logical data, on the device or on the host
  if isa(x, 'gpuArray')
    tf = ~any(strcmp(underlyingType(x), {'int64', 'uint64'}));
  else
    tf = (isfloat(x) || islogical(x)) && ~issparse(x) && isreal(x);
  end
end
%=============================================================================
