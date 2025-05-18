%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = colormaplist(varargin)
  % List available colormaps
  narginchk(0, 0);
  nargoutchk(0, 1);
  mapFiles =  dir([modulepath('graphics', 'functions'), '/colormaps/*.m']);
  maps = string(char(mapFiles.name));
  maps = replace(maps,".m", "");
  maps = deblank(maps);
  varargout{1} = sort(maps);
end
%=============================================================================
