%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
cmaps = colormaplist();
asserts.istrue(isstring(cmaps));
asserts.istrue(iscolumn(cmaps));
expected = ["parula";
  "turbo";
  "hsv";
  "hot";
  "cool";
  "spring";
  "summer";
  "autumn";
  "winter";
  "gray";
  "bone";
  "copper";
  "pink";
  "sky";
  "abyss";
  "nebula";
  "jet";
  "lines";
  "colorcube";
  "prism";
  "flag";
  "white";
  "viridis"];
asserts.isequal(cmaps, expected);
%=============================================================================
