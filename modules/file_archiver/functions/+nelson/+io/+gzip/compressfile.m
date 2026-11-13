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
  % Compresses a file into a .gz file (source name + '.gz' by default).
  destination = nelson.io.internal.compressFile('gzip', '.gz', struct('Level', -1), source, varargin{:});
end
%=============================================================================
