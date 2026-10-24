%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% modelicaConfigure(path)   set the OpenModelica location nflow should use
% p = modelicaConfigure()   return the currently resolved omc path ('' if none)
% modelicaConfigure('')     clear the configured override (fall back to auto)
%
% The Modelica bridge (a `modelica` block) needs an OpenModelica compiler (omc)
% to turn a Modelica model into an FMU. modelicaConfigure lets you point nflow
% at a specific OpenModelica when auto-detection is wrong or when several are
% installed. The setting is persisted in the Nelson preferences directory and
% takes precedence over OPENMODELICAHOME, the standard install directories and
% PATH.
%
% `path` may be an OpenModelica home directory (e.g.
% 'C:/Program Files/OpenModelica1.27.0-64bit') or the omc executable itself.
% Setting a path that does not resolve to a runnable omc raises a warning but is
% still stored, so you can pre-configure a machine.
%
% The nflow GUI settings write the same preference through this function, so the
% GUI and the command line share one source of truth.
%
% See also: modelicaInfo.
%=============================================================================
function varargout = modelicaConfigure(path)
  narginchk(0, 1);
  nargoutchk(0, 1);
  f = modelica_prefsfile();

  if nargin == 0
    % Query: return the resolved omc path for the current configuration.
    info = modelica_resolve(modelica_override());
    varargout{1} = info.omc;
    return
  end

  if ~(ischar(path) || (isstring(path) && isscalar(path)))
    error('Nelson:nflow_fmi:modelicaConfigureArg', 'path must be a string.');
  end
  path = char(path);

  if isempty(path)
    % Clear the override.
    if isfile(f)
      delete(f);
    end
  else
    % Store the override, then validate and report.
    path = strrep(path, '\', '/');
    cfg = struct('omcPath', path);
    fid = fopen(f, 'w');
    if fid < 0
      error('Nelson:nflow_fmi:modelicaConfigureWrite', ...
        'Could not write the Modelica configuration to ''%s''.', f);
    end
    fwrite(fid, jsonencode(cfg));
    fclose(fid);
  end

  info = modelica_resolve(modelica_override());
  if ~isempty(path) && ~info.available
    warning('Nelson:nflow_fmi:modelicaConfigure', ...
      sprintf('OpenModelica was not found at the configured path: %s', info.reason));
  elseif ~isempty(path) && ~info.capable
    warning('Nelson:nflow_fmi:modelicaConfigure', ...
      sprintf('OpenModelica was found but cannot export FMUs: %s', info.reason));
  end
  if nargout >= 1
    varargout{1} = info.omc;
  end
end
%=============================================================================
