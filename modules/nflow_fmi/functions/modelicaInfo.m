%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% info = modelicaInfo()
%
% Report the OpenModelica installation nflow will use for the Modelica bridge
% (a `modelica` block compiles a Modelica model to an FMU with OpenModelica,
% then imports it through the FMI path). Returns a struct:
%
%   .available (logical) : an omc executable was found and runs
%   .capable   (logical) : available AND able to export an FMU (FMI runtime present)
%   .omc       (char)    : resolved path to omc, '' if none
%   .home      (char)    : OpenModelica home directory, '' if unknown
%   .version   (char)    : version string reported by omc
%   .reason    (char)    : human-readable status / why it is not usable
%
% A `modelica` block simulates only when .capable is true; otherwise the
% simulation is blocked with .reason. The location is configurable with
% modelicaConfigure.
%
% See also: modelicaConfigure.
%=============================================================================
function info = modelicaInfo()
  narginchk(0, 0);
  info = modelica_resolve(modelica_override());
end
%=============================================================================
