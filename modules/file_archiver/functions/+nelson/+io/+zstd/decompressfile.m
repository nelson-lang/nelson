%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function destination = decompressfile(source, varargin)
  % Decompresses a .zst file (source name without '.zst' by default).
  destination = nelson.io.internal.decompressFile('zstd', '.zst', source, varargin{:});
end
%=============================================================================
