%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bytes = tobytes(fid)
  % Finishes an in-memory writer from nelson.io.zstd.fopen([], 'w'), closes it, returns its bytes.
  bytes = __stream_tobytes__(fid);
end
%=============================================================================
