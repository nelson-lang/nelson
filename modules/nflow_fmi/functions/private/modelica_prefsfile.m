%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Path of the JSON file that persists the nflow Modelica / OpenModelica
% configuration (the user-configured OpenModelica location). Stored in the
% Nelson preferences directory, like the other Nelson JSON preference files.
%=============================================================================
function p = modelica_prefsfile()
  p = [prefdir(), '/nflow_modelica.json'];
end
%=============================================================================
