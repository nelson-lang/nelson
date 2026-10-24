%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function cleaned = cleanup_processes(varargin)
  cleaned = __nelson_unittest_cleanup_processes__(varargin{:});
end
%=============================================================================
