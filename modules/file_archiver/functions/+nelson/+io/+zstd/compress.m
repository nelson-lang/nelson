%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bytes = compress(data, varargin)
  % Compresses uint8 data or text (UTF-8) into Zstandard data.
  bytes = nelson.io.internal.compressData('zstd', struct('Level', 0, 'Workers', 'auto'), data, varargin{:});
end
%=============================================================================
