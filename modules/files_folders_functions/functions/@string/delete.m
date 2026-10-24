%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function delete(varargin)
  % delete(filename1, ..., filenameN) removes files from disk.
  % Each argument can be a character vector, a string array or a cell
  % array of character vectors. Filenames can include wildcards (*).
  __delete_files__(varargin{:});
end
%=============================================================================
