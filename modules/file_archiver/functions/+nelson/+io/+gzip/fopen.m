%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function fid = fopen(source, mode, varargin)
  % File identifier on gzip data: a .gz file, uint8 bytes, or [] to write in memory.
  if nargin < 2
    mode = 'r';
  end
  fid = nelson.io.internal.openStream('gzip', struct('Level', -1, 'MaxSize', Inf), source, mode, varargin{:});
end
%=============================================================================
