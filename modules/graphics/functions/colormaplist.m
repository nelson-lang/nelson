%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = colormaplist(varargin)
  % List available colormaps
  narginchk(0, 0);
  nargoutchk(0, 1);
  orderedMaps = ["parula";
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
    "white"];
  mapFiles = dir([modulepath('graphics', 'functions'), '/colormaps/*.m']);
  availableMaps = string(char(mapFiles.name));
  availableMaps = replace(availableMaps, ".m", "");
  availableMaps = deblank(availableMaps);
  maps = orderedMaps;
  for k = 1:numel(availableMaps)
    if ~any(availableMaps(k) == orderedMaps)
      maps = [maps; availableMaps(k)];
    end
  end
  varargout{1} = maps;
end
%=============================================================================
