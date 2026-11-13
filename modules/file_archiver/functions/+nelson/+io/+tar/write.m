%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function write(t, name, data, varargin)
  % Adds uint8 data or text (UTF-8) as a new entry.
  t.write(name, data, varargin{:});
end
%=============================================================================
