%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = dlgetnflowincludes()
  % Include directories needed to build a native nflow block plugin.
  %
  % Returns the nflow_blocks SDK headers (BlockPlugin.h, SimEngineTypes.hpp,
  % ...) followed by the Nelson SDK include directories.  SimEngineTypes.hpp
  % pulls in <nlohmann/json.hpp>, which ships with the commons module exposed
  % by dlgetnelsonincludes(), so both sets are required.
  %
  % The paths resolve transparently whether Nelson runs from its source tree or
  % from an installation (see dlgetnelsonincludes for the same two-layout rule).
  %
  % See also: dlgetnflowlibraries, dlgetnelsonincludes, dlgeneratemake, dlmake.
  c = {};
  nflowBlocksInclude = getNflowBlocksInclude();
  if ~isempty(nflowBlocksInclude)
    c = [c; nflowBlocksInclude];
  end
  c = [c; dlgetnelsonincludes()];
end
%=============================================================================
function p = getNflowBlocksInclude()
  % Source-tree layout, and the Windows installer layout, both keep the public
  % headers under <modules>/nflow_blocks/src/include.
  p = '';
  srcInclude = [modulepath('nflow_blocks'), '/src/include'];
  if isdir(srcInclude)
    p = fullpath(srcInclude);
    return
  end
  % CMake-based installations (macOS/Linux) ship them under
  % <prefix>/include/Nelson/nflow_blocks instead.
  installedInclude = [modulepath('nelson', 'builtin'), '/../../include/Nelson/nflow_blocks'];
  if isdir(installedInclude)
    p = fullpath(installedInclude);
  end
end
%=============================================================================
