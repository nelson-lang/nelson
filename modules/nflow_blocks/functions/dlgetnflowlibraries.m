%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = dlgetnflowlibraries()
  % Libraries a native nflow block plugin must link against.
  %
  % The simulation helpers a plugin calls (getInput, getState, setOutput,
  % updated, ...) are compiled into nlsNflow_blocks, so linking that single
  % library is enough; its own Nelson dependencies resolve at load time.
  %
  % Each entry is a library path WITHOUT extension: dlgeneratemake appends the
  % platform-specific suffix (.lib on Windows, .so/.dylib elsewhere).
  %
  % See also: dlgetnflowincludes, dlgetnelsonlibraries, dlgeneratemake, dlmake.
  nelsonLibrariesPath = modulepath('nelson', 'builtin');
  c = {[nelsonLibrariesPath, '/libnlsNflow_blocks']};
end
%=============================================================================
