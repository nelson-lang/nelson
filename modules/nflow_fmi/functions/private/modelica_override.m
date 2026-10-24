%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Read the persisted OpenModelica override path ('' if none configured). This
% is the user-set location that takes precedence over OPENMODELICAHOME / the
% standard install directories / PATH when resolving omc.
%=============================================================================
function p = modelica_override()
  p = '';
  f = modelica_prefsfile();
  if ~isfile(f)
    return
  end
  try
    cfg = jsondecode(fileread(f));
  catch
    return
  end
  if isstruct(cfg) && isfield(cfg, 'omcPath') && (ischar(cfg.omcPath) || isstring(cfg.omcPath))
    p = char(cfg.omcPath);
  end
end
%=============================================================================
