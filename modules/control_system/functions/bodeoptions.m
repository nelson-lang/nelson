%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function opt = bodeoptions(varargin)
  % Options for a Bode plot.
  %
  % opt = bodeoptions()
  %
  % Returns a structure holding the Bode plot options with their default
  % values. The returned structure can be passed to bodeplot to customize
  % the rendering.

  narginchk(0, 1);
  nargoutchk(0, 1);

  if (nargin == 1)
    style = varargin{1};
    if (~ischar(style) && ~isStringScalar(style))
      error('Nelson:control_system:invalidType', '%s expected.', 'character vector');
    end
  end

  opt = struct();
  opt.FreqUnits = 'rad/s';
  opt.MagUnits = 'dB';
  opt.PhaseUnits = 'deg';
  opt.Grid = 'off';
  opt.Title = struct('String', '');
  opt.XLabel = struct('String', '');
  opt.YLabel = struct('String', '');
end
%=============================================================================
