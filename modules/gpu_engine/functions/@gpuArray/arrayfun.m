%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = arrayfun(fun, varargin)
  % Fused GPU path: compile the anonymous body into a single WGSL kernel.
  % Falls back to a host arrayfun when the body is not translatable, when
  % options (UniformOutput/ErrorHandler) are used, or for multiple outputs.
  % The builtin picks the kernel from the input classes: float-like inputs use
  % the f32 kernel, the small integer classes (int8/uint8/int16) use a
  % saturating i32 kernel, and anything else returns ok = false for the host
  % fallback below.
  if nargout <= 1
    [ok, r] = __gpuArray_arrayfun__(fun, varargin{:});
    if ok
      varargout{1} = r;
      return
    end
  end
  % CPU fallback
  args = varargin;
  for k = 1:numel(args)
    if isgpuarray(args{k})
      args{k} = gather(args{k});
    end
  end
  n = max(nargout, 1);
  outs = cell(1, n);
  [outs{1:n}] = arrayfun(fun, args{:});
  if n == 1 && (isnumeric(outs{1}) || islogical(outs{1})) && isreal(outs{1})
    outs{1} = gpuArray(outs{1});
  end
  varargout = outs;
end
%=============================================================================
