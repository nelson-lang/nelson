%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function destination = compressfile(source, varargin)
  % Compresses a file into a .zst file (source name + '.zst' by default).
  defaults = struct('Level', 0, 'Workers', 'auto');
  destination = nelson.io.internal.compressFile('zstd', '.zst', defaults, source, varargin{:});
end
%=============================================================================
