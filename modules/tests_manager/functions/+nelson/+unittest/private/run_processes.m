%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = run_processes(varargin)
  [varargout{1:nargout}] = __nelson_unittest_run_processes__(varargin{:}); %#ok<NLS0004>
end
%=============================================================================
