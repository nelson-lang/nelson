%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = issorted(varargin)
  % ordering query: evaluated on the host, returns a host logical.
  args = varargin;
  for k = 1:numel(args)
    if isgpuarray(args{k})
      args{k} = gather(args{k});
    end
  end
  tf = issorted(args{:});
end
%=============================================================================
