%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isavailable()
  % True when this build can compress and decompress Zstandard data.
  tf = strlength(__stream_version__('zstd')) > 0;
end
%=============================================================================
