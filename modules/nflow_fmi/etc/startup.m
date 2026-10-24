%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nflow_fmi imports FMI 3.0 Co-Simulation components (fmi4c, vendored in-tree).
addpath(modulepath('nflow_fmi', 'functions'), '-begin', '-frozen');
addgateway(modulepath('nflow_fmi', 'builtin'), 'nflow_fmi');
% Force the builtin library to load now so its FMU component factory registers
% with the nflow_blocks registry, making the "fmu" block available to the NFlow
% engine. The gateway DLL is otherwise loaded lazily (on first builtin call).
try
  __nflow_fmi_available__();
catch
end
%=============================================================================
