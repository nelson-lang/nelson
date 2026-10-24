%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% info = modelica_resolve(overridePath)
%
% Locate a usable OpenModelica compiler (omc) and report whether it is not only
% present but *capable* of exporting an FMU. Returns a struct:
%   .available (logical) : an omc executable was found and runs (--version)
%   .capable   (logical) : available AND the FMI export runtime is installed
%   .omc       (char)    : resolved path to omc[.exe], '' if none
%   .home      (char)    : OpenModelica home (parent of bin), '' if unknown
%   .version   (char)    : version string reported by omc, '' if none
%   .reason    (char)    : human-readable explanation when not capable
%
% Resolution precedence: overridePath -> OPENMODELICAHOME -> well-known install
% dirs -> PATH. `overridePath` may be an OpenModelica home directory or the omc
% executable itself; '' means no configured override.
%
% Capability matters because presence != capability: an omc.exe can run yet be
% unable to build an FMU when the install is missing share/omc (the FMI export
% runtime templates) -- observed on an incomplete OpenModelica 1.27.0 install.
%=============================================================================
function info = modelica_resolve(overridePath)
  if nargin < 1
    overridePath = '';
  end
  info = struct('available', false, 'capable', false, 'omc', '', ...
    'home', '', 'version', '', 'reason', '');

  omc = nflow_modelica_find_omc(overridePath);
  if isempty(omc)
    if ~isempty(overridePath)
      info.reason = ['OpenModelica (omc) was not found at the configured location ''', ...
        overridePath, '''. Fix it or clear it with modelicaConfigure(''''); nflow will ', ...
        'then auto-detect OpenModelica.'];
    else
      info.reason = ['OpenModelica (omc) was not found. Install OpenModelica, or ', ...
        'set its location with modelicaConfigure(path). Looked at: ', ...
        'NELSON_OPENMODELICA_HOME, OPENMODELICAHOME, the standard install ', ...
        'directories and PATH.'];
    end
    return
  end
  info.omc = omc;
  info.home = nflow_modelica_home_of(omc);

  [ok, ver] = nflow_modelica_version(omc);
  if ~ok
    info.reason = ['Found omc at ''', omc, ''' but it did not run (--version failed). ', ...
      'The OpenModelica installation may be broken.'];
    return
  end
  info.available = true;
  info.version = ver;

  % Capability probe: FMU export needs the FMI runtime build templates under
  % share/omc. Their absence is exactly what makes buildModelFMU fail.
  [capable, why] = nflow_modelica_capability(info.home);
  info.capable = capable;
  if capable
    info.reason = ['OpenModelica ', ver, ' at ''', info.home, ''' is ready to compile Modelica to FMU.'];
  else
    info.reason = why;
  end
end
%=============================================================================
function omc = nflow_modelica_find_omc(overridePath)
  omc = '';
  candidates = {};
  % An explicit override is AUTHORITATIVE: when configured, only that location is
  % tried. Pointing nflow at a specific OpenModelica must not silently resolve to
  % a different one; a bad override reports "not found there" so the user knows.
  if ~isempty(overridePath)
    candidates = nflow_modelica_expand(candidates, overridePath);
    omc = nflow_modelica_first_existing(candidates);
    return
  end
  % Auto-detect chain (no override configured).
  % 2) environment variables: a Nelson-scoped one first (lets CI / headless
  %    setups point nflow without touching the global OpenModelica variable or
  %    writing a preference file), then the standard OpenModelica variable.
  for envName = {'NELSON_OPENMODELICA_HOME', 'OPENMODELICAHOME'}
    ev = getenv(envName{1});
    if ~isempty(ev)
      candidates = nflow_modelica_expand(candidates, ev);
    end
  end
  % 3) well-known install locations
  if ispc()
    roots = {'C:/Program Files', 'C:/Program Files (x86)', 'C:/'};
    for r = 1:numel(roots)
      d = dir([roots{r}, '/OpenModelica*']);
      names = {};
      for k = 1:numel(d)
        if d(k).isdir
          names{end + 1} = [roots{r}, '/', d(k).name]; %#ok<AGROW>
        end
      end
      names = sort(names); % newest version sorts last
      for k = numel(names):-1:1
        candidates = nflow_modelica_expand(candidates, names{k});
      end
    end
  else
    unixHomes = {'/usr', '/usr/local', '/opt/openmodelica', '/Applications/OpenModelica.app/Contents/Resources'};
    for k = 1:numel(unixHomes)
      candidates = nflow_modelica_expand(candidates, unixHomes{k});
    end
  end
  % 4) PATH (bare name, resolved to a full path if it runs)
  if ispc()
    candidates{end + 1} = 'omc.exe';
  else
    candidates{end + 1} = 'omc';
  end
  omc = nflow_modelica_first_existing(candidates);
end
%=============================================================================
function omc = nflow_modelica_first_existing(candidates)
  % First candidate that exists (or, for the bare PATH name, that runs).
  omc = '';
  for k = 1:numel(candidates)
    c = candidates{k};
    if isfile(c)
      omc = c;
      return
    end
    if ~nflow_modelica_haspath(c) % a bare 'omc'/'omc.exe' -> try PATH
      resolved = nflow_modelica_which(c);
      if ~isempty(resolved)
        omc = resolved;
        return
      end
    end
  end
end
%=============================================================================
function candidates = nflow_modelica_expand(candidates, p)
  % Turn a home dir or exe path into concrete omc candidate paths.
  p = strrep(p, '\', '/');
  if ispc()
    exe = 'omc.exe';
  else
    exe = 'omc';
  end
  low = lower(p);
  if endsWith(low, '/omc') || endsWith(low, '/omc.exe') || strcmp(low, 'omc') || strcmp(low, 'omc.exe')
    candidates{end + 1} = p; % already an exe path
  else
    candidates{end + 1} = [p, '/bin/', exe]; % home -> home/bin/omc
    candidates{end + 1} = [p, '/', exe]; % home -> home/omc
  end
end
%=============================================================================
function tf = nflow_modelica_haspath(p)
  tf = ~isempty(strfind(strrep(p, '\', '/'), '/'));
end
%=============================================================================
function full = nflow_modelica_which(name)
  full = '';
  if ispc()
    cmd = ['where ', name];
  else
    cmd = ['command -v ', name];
  end
  [st, out] = system(cmd);
  if st == 0
    lines = strsplit(strtrim(out), sprintf('\n'));
    if ~isempty(lines)
      full = strrep(strtrim(lines{1}), '\', '/');
    end
  end
end
%=============================================================================
function home = nflow_modelica_home_of(omc)
  % <home>/bin/omc[.exe] -> <home>
  omc = strrep(omc, '\', '/');
  binDir = fileparts(omc);
  home = fileparts(binDir);
  if isempty(home)
    home = binDir;
  end
end
%=============================================================================
function [ok, ver] = nflow_modelica_version(omc)
  ok = false;
  ver = '';
  [st, out] = system(['"', omc, '" --version']);
  if st == 0 && ~isempty(out)
    ver = strtrim(out);
    ok = true;
  end
end
%=============================================================================
function [capable, why] = nflow_modelica_capability(home)
  capable = false;
  why = '';
  if isempty(home)
    why = 'OpenModelica home could not be determined, so FMU-export capability cannot be verified.';
    return
  end
  fmiRuntime = [home, '/share/omc/runtime/c/fmi'];
  if ~isfolder(fmiRuntime)
    why = ['OpenModelica is present but cannot export FMUs: the FMI export runtime ', ...
      '''', fmiRuntime, ''' is missing (share/omc not installed). Install a complete ', ...
      'OpenModelica (with the C runtime / FMI export support), or point nflow at one ', ...
      'via modelicaConfigure(path).'];
    return
  end
  capable = true;
end
%=============================================================================
